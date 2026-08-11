module VersionControlService.LakeFs.LakeFsStateStore

open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions

module LakeFsProviderOptions = VersionControlService.LakeFs.LakeFsProviderOptions
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodePath = VersionControlService.Runtime.Node.Path

type ResolvedState = {
    StateId: string
    StateDirectory: string
    TransactionsDirectory: string
    RecoveryDirectory: string
    TemporaryDirectory: string
    WorkspaceRoot: string
    ProvisioningMode: string
    IsReady: bool
}

[<Literal>]
let RecoverySchemaVersion = 1

type RecoveryReplacement = {
    Path: string
    TemporaryPath: string
    TargetPath: string
    Sha256: string
    ExpectedTargetHash: string option
}

type RecoveryRemoval = {
    Path: string
    TargetPath: string
}

type RecoveryRecord = {
    SchemaVersion: int
    TransactionId: string
    TransactionDirectory: string
    ExpectedWorkspace: string option
    ObservedMaterialization: string
    AffectedPaths: string[]
    Replacements: RecoveryReplacement[]
    Removals: RecoveryRemoval[]
    CurrentIndex: obj
    NextIndex: obj
}

type LoadedRecoveryRecord = {
    Path: string
    Record: RecoveryRecord
}

[<Literal>]
let private ManifestSchemaVersion = 1

[<Literal>]
let private ManifestFileName = "state.json"

[<Literal>]
let InitializeProvisioning = "initialize"

[<Literal>]
let CloneProvisioning = "clone"

[<Literal>]
let BindProvisioning = "bind"

type private StateManifest = {
    SchemaVersion: int
    StateId: string
    WorkspaceRoot: string
    ProvisioningMode: string
    IsReady: bool
}

[<Emit("JSON.stringify($0, null, 2)")>]
let private jsonStringify (_value: obj) : string = jsNative

[<Emit("JSON.parse($0)")>]
let private jsonParse (_text: string) : obj = jsNative

[<Emit("require('node:fs').rmSync($0, { recursive: true, force: true })")>]
let private removeTreeSync (_path: string) : unit = jsNative

let private pathComparison =
    if NodeInterop.processPlatform () = "win32" then
        StringComparison.OrdinalIgnoreCase
    else
        StringComparison.Ordinal

let private normalize path = NodePath.resolve [| path |]

let private isWithin root candidate =
    let normalizedRoot = normalize root
    let normalizedCandidate = normalize candidate
    let relative = NodePath.relative normalizedRoot normalizedCandidate

    String.Equals(normalizedRoot, normalizedCandidate, pathComparison)
    || (not (NodePath.isAbsolute relative)
        && relative <> ".."
        && not (relative.StartsWith("../", StringComparison.Ordinal))
        && not (relative.StartsWith("..\\", StringComparison.Ordinal)))

let private failure code message =
    OperationFailure.create ProviderError code message

let reconcileRecoveryAction (recordPaths: string[]) =
    let locations = String.concat ", " recordPaths
    let instructions =
        $"Inspect the retained recovery metadata at {locations}, reconcile diverged "
        + "workspace paths manually, then run RestorePaths to complete materialization."

    {
        Code = "reconcile_materialization"
        Instructions = Some instructions
    }

let private errorCode (error: exn) =
    try
        error?code |> Option.ofObj |> Option.map unbox<string>
    with _ ->
        None

let private tryLstat path =
    try
        Ok(Some(NodeFileSystem.lstatSync path))
    with error ->
        let code: string = error?code |> unbox

        if code = "ENOENT" then
            Ok None
        else
            Error(
                OperationFailure.createRedacted
                    ProviderError
                    "provider_state_inspection_failed"
                    $"Inspecting an external lakeFS state path failed: {error.Message}"
            )

let private pathChain path =
    let rec collect current paths =
        let parent = NodePath.dirname current

        if String.Equals(parent, current, pathComparison) then
            current :: paths
        else
            collect parent (current :: paths)

    collect (normalize path) []

let private rejectLinksInChain path =
    pathChain path
    |> List.fold
        (fun result current ->
            match result with
            | Error _ -> result
            | Ok() ->
                match tryLstat current with
                | Error inspectionFailure -> Error inspectionFailure
                | Ok(Some stats) when stats.isSymbolicLink () ->
                    Error(
                        failure
                            "provider_state_link_not_supported"
                            "Symbolic links, junctions, and reparse-point paths are not supported for lakeFS provider state."
                    )
                | Ok _ -> Ok())
        (Ok())

let private stateIdPattern = Regex("^state-[a-f0-9]{32}$")

let private resolvedState stateRoot stateId manifest =
    let stateDirectory = NodePath.resolve [| stateRoot; stateId |]

    {
        StateId = stateId
        StateDirectory = stateDirectory
        TransactionsDirectory = NodePath.join [| stateDirectory; "transactions" |]
        RecoveryDirectory = NodePath.join [| stateDirectory; "recovery" |]
        TemporaryDirectory = NodePath.join [| stateDirectory; "temporary" |]
        WorkspaceRoot = manifest.WorkspaceRoot
        ProvisioningMode = manifest.ProvisioningMode
        IsReady = manifest.IsReady
    }

let private stateDirectory stateRoot stateId =
    NodePath.resolve [| stateRoot; stateId |]

let private manifestPath stateDirectory =
    NodePath.join [| stateDirectory; ManifestFileName |]

let private indexPath stateDirectory =
    NodePath.join [| stateDirectory; "index.json" |]

let private validateOwnedLayout (state: ResolvedState) =
    [|
        manifestPath state.StateDirectory
        indexPath state.StateDirectory
        state.TransactionsDirectory
        state.RecoveryDirectory
        state.TemporaryDirectory
    |]
    |> Array.fold
        (fun result path ->
            match result with
            | Error _ -> result
            | Ok() -> rejectLinksInChain path)
        (Ok())

let private recoveryFailure path message =
    {
        OperationFailure.createRedacted
            ProviderError
            "materialization_recovery_corrupt"
            message with
            RecoveryAction = Some(reconcileRecoveryAction [| path |])
    }

let private loadRecoveryRecord path =
    try
        let parsed = jsonParse (NodeFileSystem.readFileSync path NodeFileSystem.TextEncoding.Utf8)

        let schemaVersion =
            parsed?SchemaVersion
            |> Option.ofObj
            |> Option.map unbox<int>
            |> Option.defaultValue 0

        if schemaVersion <> RecoverySchemaVersion then
            Error(
                recoveryFailure
                    path
                    $"Unsupported lakeFS materialization recovery schema version {schemaVersion} in '{path}'."
            )
        elif
            isNull parsed?TransactionId
            || isNull parsed?TransactionDirectory
            || isNull parsed?ObservedMaterialization
            || isNull parsed?AffectedPaths
            || isNull parsed?Replacements
            || isNull parsed?Removals
            || isNull parsed?CurrentIndex
            || isNull parsed?NextIndex
        then
            Error(recoveryFailure path $"The lakeFS materialization recovery record '{path}' is incomplete.")
        else
            Ok {
                Path = path
                Record = unbox<RecoveryRecord> parsed
            }
    with error ->
        Error(
            recoveryFailure
                path
                $"Reading lakeFS materialization recovery metadata at '{path}' failed: {error.Message}"
        )

let loadRecoveryRecords recoveryDirectory : Result<LoadedRecoveryRecord[], OperationFailure> =
    try
        NodeFileSystem.readdirSync recoveryDirectory
        |> Array.filter (fun name -> name.EndsWith(".json", StringComparison.Ordinal))
        |> Array.sort
        |> Array.fold
            (fun loaded name ->
                loaded
                |> Result.bind (fun records ->
                    let path = NodePath.join [| recoveryDirectory; name |]

                    loadRecoveryRecord path
                    |> Result.map (fun record -> Array.append records [| record |])))
            (Ok [||])
    with
    | error when errorCode error = Some "ENOENT" -> Ok [||]
    | error ->
        Error(
            {
                OperationFailure.createRedacted
                    ProviderError
                    "materialization_recovery_inspection_failed"
                    $"Inspecting lakeFS materialization recovery metadata failed: {error.Message}" with
                    RecoveryAction = Some(reconcileRecoveryAction [| recoveryDirectory |])
            }
        )

let sweepTransientEntries (state: ResolvedState) =
    let sweepDirectory directory keep =
        try
            for name in NodeFileSystem.readdirSync directory do
                let path = NodePath.join [| directory; name |]

                if not (keep path name) then
                    try
                        removeTreeSync path
                    with _ ->
                        ()
        with _ ->
            ()

    match loadRecoveryRecords state.RecoveryDirectory with
    | Error _ -> ()
    | Ok records ->
        let transactionIds =
            records
            |> Array.map _.Record.TransactionId
            |> Set.ofArray

        sweepDirectory
            state.TransactionsDirectory
            (fun _ name -> transactionIds.Contains name)

        sweepDirectory state.TemporaryDirectory (fun _ _ -> false)

let private writeManifest path manifest =
    NodeFileSystem.writeUtf8FileExclusiveAndFlushSync path (jsonStringify manifest)

let private loadManifest path =
    if not (NodeFileSystem.existsSync path) then
        Error(failure "provider_state_corrupt" "The external lakeFS state manifest is missing.")
    else
        try
            let parsed = jsonParse (NodeFileSystem.readFileSync path NodeFileSystem.TextEncoding.Utf8)

            let schemaVersion =
                parsed?SchemaVersion
                |> Option.ofObj
                |> Option.map unbox<int>
                |> Option.defaultValue 0

            if schemaVersion <> ManifestSchemaVersion then
                Error(
                    failure
                        "provider_state_corrupt"
                        $"Unsupported lakeFS state manifest schema version {schemaVersion}."
                )
            elif
                isNull parsed?StateId
                || isNull parsed?WorkspaceRoot
                || isNull parsed?ProvisioningMode
                || isNull parsed?IsReady
            then
                Error(failure "provider_state_corrupt" "The external lakeFS state manifest is incomplete.")
            else
                Ok(unbox<StateManifest> parsed)
        with error ->
            Error(
                OperationFailure.createRedacted
                    ProviderError
                    "provider_state_corrupt"
                    $"The external lakeFS state manifest could not be parsed: {error.Message}"
            )

let private validateLocation
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (workspaceRoot: string)
    (state: ResolvedState)
    =
    let stateRoot = normalize options.StateRoot

    if not (isWithin stateRoot state.StateDirectory) then
        Error(failure "provider_state_ref_invalid" "The provider state reference escapes the configured state root.")
    elif isWithin workspaceRoot state.StateDirectory then
        Error(
            failure
                "provider_state_inside_workspace"
                "lakeFS provider state must be stored outside the managed workspace."
        )
    else
        match rejectLinksInChain stateRoot with
        | Error linkFailure -> Error linkFailure
        | Ok() ->
            match rejectLinksInChain state.StateDirectory with
            | Error linkFailure -> Error linkFailure
            | Ok() -> Ok state

let createForProvisioning
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (workspaceRoot: string)
    (provisioningMode: string)
    : Result<ResolvedState, OperationFailure> =
    try
        if String.IsNullOrWhiteSpace options.StateRoot then
            Error(failure "provider_state_root_invalid" "The lakeFS provider state root is required.")
        else
            let stateRoot = normalize options.StateRoot

            if isWithin workspaceRoot stateRoot then
                Error(
                    failure
                        "provider_state_inside_workspace"
                        "lakeFS provider state must be stored outside the managed workspace."
                )
            else
                match rejectLinksInChain stateRoot with
                | Error linkFailure -> Error linkFailure
                | Ok() ->
                    NodeFileSystem.mkdirSync stateRoot (NodeFileSystem.MkdirOptions(recursive = true))

                    match rejectLinksInChain stateRoot with
                    | Error linkFailure -> Error linkFailure
                    | Ok() ->
                        let stateId = "state-" + NodeInterop.randomUuid().Replace("-", "").ToLowerInvariant()
                        let manifest = {
                            SchemaVersion = ManifestSchemaVersion
                            StateId = stateId
                            WorkspaceRoot = normalize workspaceRoot
                            ProvisioningMode = provisioningMode
                            IsReady = false
                        }
                        let state = resolvedState stateRoot stateId manifest

                        match validateLocation options workspaceRoot state with
                        | Error invalid -> Error invalid
                        | Ok valid ->
                            NodeFileSystem.mkdirSync valid.StateDirectory (NodeFileSystem.MkdirOptions(recursive = false))

                            for child in [|
                                valid.TransactionsDirectory
                                valid.RecoveryDirectory
                                valid.TemporaryDirectory
                            |] do
                                NodeFileSystem.mkdirSync child (NodeFileSystem.MkdirOptions(recursive = false))

                            writeManifest (manifestPath valid.StateDirectory) manifest

                            validateOwnedLayout valid |> Result.map (fun () -> valid)
    with error ->
        Error(
            OperationFailure.createRedacted
                ProviderError
                "provider_state_create_failed"
                $"Creating external lakeFS provider state failed: {error.Message}"
        )

let create
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (workspaceRoot: string)
    : Result<ResolvedState, OperationFailure> =
    createForProvisioning options workspaceRoot BindProvisioning

let resolve
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (workspaceRoot: string)
    (providerStateRef: string option)
    : Result<ResolvedState, OperationFailure> =
    match providerStateRef with
    | None ->
        Error(
            failure
                "provider_state_ref_missing"
                "The workspace binding does not contain a lakeFS provider state reference."
        )
    | Some stateId when not (stateIdPattern.IsMatch stateId) ->
        Error(
            failure
                "provider_state_ref_invalid"
                "The workspace binding contains an invalid lakeFS provider state reference."
        )
    | Some stateId ->
        let directory = stateDirectory (normalize options.StateRoot) stateId

        match rejectLinksInChain directory with
        | Error linkFailure -> Error linkFailure
        | Ok() when not (NodeFileSystem.existsSync directory) ->
            Error(failure "provider_state_missing" "The external lakeFS provider state directory is missing.")
        | Ok() ->
            match rejectLinksInChain (manifestPath directory) with
            | Error linkFailure -> Error linkFailure
            | Ok() ->
                match loadManifest (manifestPath directory) with
                | Error invalid -> Error invalid
                | Ok manifest when manifest.StateId <> stateId ->
                    Error(failure "provider_state_corrupt" "The external lakeFS state manifest identity is invalid.")
                | Ok manifest when not (String.Equals(normalize workspaceRoot, normalize manifest.WorkspaceRoot, pathComparison)) ->
                    Error(
                        failure
                            "provider_state_mismatch"
                            "The external lakeFS state belongs to a different workspace."
                    )
                | Ok manifest ->
                    let state = resolvedState (normalize options.StateRoot) stateId manifest

                    match validateLocation options workspaceRoot state with
                    | Error invalid -> Error invalid
                    | Ok valid ->
                        match validateOwnedLayout valid with
                        | Error linkFailure -> Error linkFailure
                        | Ok() ->
                            let missingLayout =
                                [|
                                    valid.TransactionsDirectory
                                    valid.RecoveryDirectory
                                    valid.TemporaryDirectory
                                |]
                                |> Array.exists (NodeFileSystem.existsSync >> not)

                            if missingLayout then
                                Error(
                                    failure
                                        "provider_state_corrupt"
                                        "The external lakeFS provider state directory is incomplete."
                                )
                            else
                                Ok valid

let markReady (state: ResolvedState) : Result<ResolvedState, OperationFailure> =
    let temporaryPath =
        NodePath.join [| state.TemporaryDirectory; $"state-{NodeInterop.randomUuid()}.tmp" |]

    let nextManifest = {
        SchemaVersion = ManifestSchemaVersion
        StateId = state.StateId
        WorkspaceRoot = state.WorkspaceRoot
        ProvisioningMode = state.ProvisioningMode
        IsReady = true
    }

    try
        writeManifest temporaryPath nextManifest
        NodeFileSystem.renameSync temporaryPath (manifestPath state.StateDirectory)
        Ok { state with IsReady = true }
    with error ->
        try
            NodeFileSystem.unlinkSync temporaryPath
        with _ ->
            ()

        Error(
            OperationFailure.createRedacted
                ProviderError
                "provider_state_write_failed"
                $"Updating the external lakeFS state manifest failed: {error.Message}"
        )
