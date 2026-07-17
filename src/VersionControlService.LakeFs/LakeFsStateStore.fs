module VersionControlService.LakeFs.LakeFsStateStore

open System
open System.Text.RegularExpressions
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
}

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

let private stateIdPattern = Regex("^state-[a-f0-9]{32}$")

let private resolvedState stateRoot stateId =
    let stateDirectory = NodePath.resolve [| stateRoot; stateId |]

    {
        StateId = stateId
        StateDirectory = stateDirectory
        TransactionsDirectory = NodePath.join [| stateDirectory; "transactions" |]
        RecoveryDirectory = NodePath.join [| stateDirectory; "recovery" |]
        TemporaryDirectory = NodePath.join [| stateDirectory; "temporary" |]
    }

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
        Ok state

let create
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (workspaceRoot: string)
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
                NodeFileSystem.mkdirSync stateRoot (NodeFileSystem.MkdirOptions(recursive = true))
                let stateId = "state-" + NodeInterop.randomUuid().Replace("-", "").ToLowerInvariant()
                let state = resolvedState stateRoot stateId

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

                    Ok valid
    with error ->
        Error(
            OperationFailure.createRedacted
                ProviderError
                "provider_state_create_failed"
                $"Creating external lakeFS provider state failed: {error.Message}"
        )

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
        let state = resolvedState (normalize options.StateRoot) stateId

        match validateLocation options workspaceRoot state with
        | Error invalid -> Error invalid
        | Ok valid when not (NodeFileSystem.existsSync valid.StateDirectory) ->
            Error(failure "provider_state_missing" "The external lakeFS provider state directory is missing.")
        | Ok valid ->
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
