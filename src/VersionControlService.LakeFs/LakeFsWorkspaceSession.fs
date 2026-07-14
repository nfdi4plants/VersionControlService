/// lakeFS workspace sessions: a provider-owned, server-visible workspace branch,
/// a local object index, and client-side check-then-act-then-verify for every
/// commit and merge (lakeFS has no server-side expected-head precondition).
module VersionControlService.LakeFs.LakeFsWorkspaceSession

open System
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes

module LakeFsApi = VersionControlService.LakeFs.LakeFsApi
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path

let private workspaceBranchName (workspaceRoot: string) (ownershipToken: string) =
    let sanitizedWorkspaceId =
        NodePath.basename workspaceRoot
        |> _.ToLowerInvariant()
        |> Seq.map (fun character -> if Char.IsLetterOrDigit character || character = '-' then character else '-')
        |> Seq.toArray
        |> String
        |> _.Trim('-')
        |> function
            | value when String.IsNullOrWhiteSpace value -> "workspace"
            | value when value.Length > 40 -> value.Substring(0, 40).TrimEnd('-')
            | value -> value

    $"vcs-workspace-{sanitizedWorkspaceId}-{ownershipToken.Substring(0, 12)}"

/// Test seams, mirroring the Git provider: named barrier points let harnesses
/// advance destinations inside check-then-act-then-verify windows.
type LakeFsSessionHooks = {
    Barrier: (string -> string -> OperationContext -> Async<unit>) option
}

module LakeFsSessionHooks =

    let none: LakeFsSessionHooks = { Barrier = None }

let private lakeFsProviderId =
    match ProviderId.tryCreate "lakefs" with
    | Ok providerId -> providerId
    | Error message -> failwith message

let private mkRevisionId (value: string) =
    match RevisionId.tryCreate value with
    | Ok revisionId -> revisionId
    | Error message -> failwith message

let private mkProviderRef (value: string) =
    match ProviderRef.tryCreate value with
    | Ok reference -> reference
    | Error message -> failwith message

type private ConflictItemState = {
    ItemPath: string
    BaseContent: string option
    WorkspaceContent: string option
    TargetContent: string option
    mutable ResolvedContent: string option
}

type private ConflictState = {
    SessionId: string
    mutable HandleVersion: int
    Items: ConflictItemState list
    TargetRevisionAtOpen: string
}

type private SessionState = {
    Binding: WorkspaceBinding
    Location: LakeFsLocation
    Credentials: LakeFsCredentials.LakeFsCredentialStrategy
    Hooks: LakeFsSessionHooks
    mutable Index: LakeFsIndex.WorkspaceIndex
    mutable Conflict: ConflictState option
    mutable ConflictGeneration: int
    mutable Busy: bool
}

let private barrier (state: SessionState) (point: string) (context: OperationContext) =
    match state.Hooks.Barrier with
    | Some hook -> hook state.Binding.WorkspaceRoot point context
    | None -> async.Return()

let private connect (state: SessionState) : Async<Result<LakeFsConnection, OperationFailure>> =
    async {
        let! connection = state.Credentials.ResolveConnection state.Binding.ConnectionProfileId

        match connection with
        | Ok resolved -> return Ok resolved
        | Error message ->
            return
                Error(OperationFailure.createRedacted Authentication "connection_profile_unresolved" message)
    }

/// Repository object key for a repo-relative path (prefix applied).
let private objectKey (state: SessionState) (path: string) =
    if state.Index.Prefix = "" then
        path
    else
        $"{state.Index.Prefix}/{path}"

let private relativePathOfKey (state: SessionState) (key: string) =
    if state.Index.Prefix = "" then
        Some key
    elif key.StartsWith(state.Index.Prefix + "/") then
        Some(key.Substring(state.Index.Prefix.Length + 1))
    else
        None

let private localPath (state: SessionState) (path: string) =
    NodePath.join [| state.Binding.WorkspaceRoot; path |]

let private tryReadLocal (state: SessionState) (path: string) : string option =
    let absolute = localPath state path

    if NodeFileSystem.existsSync absolute then
        Some(NodeFileSystem.readFileSync absolute NodeFileSystem.TextEncoding.Utf8)
    else
        None

let private writeLocal (state: SessionState) (path: string) (content: string) =
    let absolute = localPath state path
    let parent = NodePath.dirname absolute

    if not (NodeFileSystem.existsSync parent) then
        NodeFileSystem.mkdirSync parent (NodeFileSystem.MkdirOptions(recursive = true))

    NodeFileSystem.writeFileSync absolute content NodeFileSystem.TextEncoding.Utf8

let private removeLocal (state: SessionState) (path: string) =
    let absolute = localPath state path

    if NodeFileSystem.existsSync absolute then
        NodeFileSystem.unlinkSync absolute

/// All repo-relative files currently in the workspace directory.
let rec private walkLocalFiles (root: string) (relative: string) : string list =
    let current = if relative = "" then root else NodePath.join [| root; relative |]

    if not (NodeFileSystem.existsSync current) then
        []
    else
        NodeFileSystem.readdirSync current
        |> Array.toList
        |> List.collect (fun name ->
            if name = LakeFsIndex.IndexFileName || name.EndsWith ".tmp" then
                []
            else
                let childRelative = if relative = "" then name else $"{relative}/{name}"
                let childAbsolute = NodePath.join [| root; childRelative |]

                try
                    if (NodeFileSystem.statSync childAbsolute).isDirectory () then
                        walkLocalFiles root childRelative
                    else
                        [ childRelative ]
                with _ ->
                    [])

type private LocalChange = {
    ChangePath: string
    State: LakeFsIndex.LocalObjectState
}

/// Classifies every local file and indexed entry (content-hash authority).
let private classifyWorkspace (state: SessionState) : LocalChange list =
    let entriesByPath =
        state.Index.Entries
        |> Array.map (fun entry -> entry.Path, entry)
        |> Map.ofArray

    let localFiles = walkLocalFiles state.Binding.WorkspaceRoot ""

    let localChanges =
        localFiles
        |> List.map (fun path ->
            let entry = entriesByPath.TryFind path
            let content = tryReadLocal state path

            {
                ChangePath = path
                State = LakeFsIndex.classifyLocalObject entry content
            })

    let deletions =
        state.Index.Entries
        |> Array.toList
        |> List.filter (fun entry -> not (localFiles |> List.contains entry.Path))
        |> List.map (fun entry -> {
            ChangePath = entry.Path
            State = LakeFsIndex.DeletedObject
        })

    (localChanges @ deletions)
    |> List.filter (fun change -> change.State <> LakeFsIndex.UnchangedObject)

let private workspaceVersion (state: SessionState) =
    let changesIdentity =
        classifyWorkspace state
        |> List.map (fun change -> $"{change.ChangePath}:{change.State}")
        |> String.concat ";"

    let conflictPart =
        match state.Conflict with
        | Some conflict -> $"c{conflict.HandleVersion}"
        | None -> "none"

    let identityHash = LakeFsIndex.hashContent changesIdentity
    let workspaceRevision = state.Index.WorkspaceRevision |> Option.defaultValue "none"

    $"lakefs:{state.Index.Generation}:{workspaceRevision}:{identityHash}:{conflictPart}"

let private staleFailure () =
    OperationFailure.create
        Concurrency
        "precondition_failed"
        "The expected workspace version is stale; refresh status and retry."

let private withValidatedMutation
    (state: SessionState)
    (expectedVersion: string)
    (body: unit -> Async<OperationResult<'T>>)
    : Async<OperationResult<'T>> =
    async {
        while state.Busy do
            do! Async.Sleep 5

        state.Busy <- true

        try
            if workspaceVersion state <> expectedVersion then
                return Failed(staleFailure ())
            else
                return! body ()
        finally
            state.Busy <- false
    }

let private saveIndex (state: SessionState) =
    match LakeFsIndex.save state.Binding.WorkspaceRoot state.Index with
    | Ok saved ->
        state.Index <- saved
        Ok()
    | Error message -> Error(OperationFailure.createRedacted ProviderError "index_write_failed" message)

let private conflictSummary (state: SessionState) : ConflictSessionSummary option =
    state.Conflict
    |> Option.map (fun conflict -> {
        Handle = {
            SessionId = conflict.SessionId
            Version = string conflict.HandleVersion
        }
        Items =
            conflict.Items
            |> List.filter (fun item -> item.ResolvedContent.IsNone)
            |> List.choose (fun item ->
                match RepositoryPath.tryCreate item.ItemPath with
                | Error _ -> None
                | Ok path ->
                    Some {
                        Path = path
                        Candidates = [|
                            {
                                CandidateId = "workspace"
                                Label = "Workspace version"
                                Revision = state.Index.WorkspaceRevision |> Option.map mkRevisionId
                                Preview = item.WorkspaceContent |> Option.map TextPreview
                            }
                            {
                                CandidateId = "target"
                                Label = "Target version"
                                Revision = Some(mkRevisionId conflict.TargetRevisionAtOpen)
                                Preview = item.TargetContent |> Option.map TextPreview
                            }
                            yield!
                                match item.BaseContent with
                                | Some baseContent ->
                                    [|
                                        {
                                            CandidateId = "base"
                                            Label = "Base version"
                                            Revision = state.Index.BaseRevision |> Option.map mkRevisionId
                                            Preview = Some(TextPreview baseContent)
                                        }
                                    |]
                                | None -> [||]
                        |]
                        SupportsResolvedContent = true
                    })
            |> List.toArray
    })

let private synchronizationState (state: SessionState) (targetHead: string option) : SynchronizationState =
    let baseRevision = state.Index.BaseRevision
    let workspaceRevision = state.Index.WorkspaceRevision

    let relationship =
        match targetHead, baseRevision, workspaceRevision with
        | None, _, _ -> NoTarget
        | Some target, _, Some workspace when target = workspace -> UpToDate
        | Some target, Some baseRev, Some workspace when target = baseRev && workspace = baseRev -> UpToDate
        | Some target, Some baseRev, Some workspace when target = baseRev && workspace <> baseRev -> LocalAhead
        | Some target, Some baseRev, Some workspace when target <> baseRev && workspace = baseRev -> TargetAhead
        | Some _, Some _, Some _ -> Diverged
        | _ -> UnknownRelationship

    {
        BaseRevision = baseRevision |> Option.map mkRevisionId
        WorkspaceRevision = workspaceRevision |> Option.map mkRevisionId
        TargetRevision = targetHead |> Option.map mkRevisionId
        LocalRevisionCount = None
        TargetRevisionCount = None
        RemoteChangedPaths = None
        Relationship = relationship
    }

let private getTargetHead (state: SessionState) (context: OperationContext) =
    async {
        let! connection = connect state

        match connection with
        | Error failure -> return Error failure
        | Ok resolved ->
            let! branch = LakeFsApi.getBranch resolved state.Index.Repository state.Index.TargetRef context

            match branch with
            | Ok value -> return Ok value.CommitId
            | Error failure -> return Error failure
    }

/// Target-vs-base changed keys under the prefix, as repo-relative paths.
let private targetChangedPaths (state: SessionState) (context: OperationContext) =
    async {
        match state.Index.BaseRevision with
        | None -> return Ok []
        | Some baseRevision ->
            let! connection = connect state

            match connection with
            | Error failure -> return Error failure
            | Ok resolved ->
                let! diff =
                    LakeFsApi.diffRefs resolved state.Index.Repository baseRevision state.Index.TargetRef context

                match diff with
                | Error failure -> return Error failure
                | Ok entries ->
                    return
                        Ok(
                            entries
                            |> Array.toList
                            |> List.choose (fun entry -> relativePathOfKey state entry.Path)
                        )
    }

// ---------------------------------------------------------------------------
// Core operations
// ---------------------------------------------------------------------------

let private getStatus (state: SessionState) (context: OperationContext) =
    async {
        let changes =
            classifyWorkspace state
            |> List.choose (fun change ->
                match RepositoryPath.tryCreate change.ChangePath with
                | Error _ -> None
                | Ok path ->
                    let kind =
                        match change.State with
                        | LakeFsIndex.AddedObject -> AddedChange
                        | LakeFsIndex.DeletedObject -> DeletedChange
                        | _ -> ModifiedChange

                    Some {
                        Path = path
                        OldPath = None
                        Kind = kind
                    })
            |> List.toArray

        // Target state is advisory here; a refresh reads it live.
        return
            OperationResult.succeeded {
                CurrentRef =
                    Some {
                        Name = state.Index.TargetRef
                        ProviderRef = mkProviderRef $"lakefs:{state.Index.TargetRef}"
                        Kind = LocalRef
                        IsCurrent = true
                    }
                WorkspaceVersion = workspaceVersion state
                Changes = changes
                ActiveConflictSession = conflictSummary state
                Synchronization = Some(synchronizationState state state.Index.BaseRevision)
            }
    }

/// Uploads/deletes exactly the selected paths on the workspace branch, commits,
/// and verifies the returned commit against the expected parent and the branch
/// head (client-side check-then-act-then-verify; never a server precondition).
let private createRevision (state: SessionState) (request: CreateRevisionRequest) (context: OperationContext) =
    async {
        if request.Paths.Length = 0 then
            return OperationResult.validationFailed "no_paths_selected" "Select at least one path."
        else
            let! connection = connect state

            match connection with
            | Error failure -> return Failed failure
            | Ok resolved ->
                do! barrier state "transfer-start" context

                if context.Cancellation.IsCancellationRequested() then
                    return OperationResult.canceled "The selected revision was canceled before any transfer."
                else
                    let entriesByPath =
                        state.Index.Entries |> Array.map (fun entry -> entry.Path, entry) |> Map.ofArray

                    let selections =
                        request.Paths
                        |> Array.map (fun path ->
                            let pathValue = RepositoryPath.value path
                            let content = tryReadLocal state pathValue

                            pathValue,
                            content,
                            LakeFsIndex.classifyLocalObject (entriesByPath.TryFind pathValue) content)

                    let missing =
                        selections
                        |> Array.filter (fun (pathValue, content, _) ->
                            content.IsNone && not (entriesByPath.ContainsKey pathValue))
                        |> Array.map (fun (pathValue, _, _) -> pathValue)

                    if missing.Length > 0 then
                        return
                            Failed {
                                OperationFailure.create
                                    NotFound
                                    "path_not_found"
                                    "A selected path exists neither locally nor in the workspace branch." with
                                    AffectedPaths = missing
                            }
                    else
                        let changed =
                            selections
                            |> Array.filter (fun (_, _, objectState) ->
                                objectState <> LakeFsIndex.UnchangedObject)

                        if changed.Length = 0 then
                            let currentRevision =
                                state.Index.WorkspaceRevision |> Option.defaultValue "none"

                            return
                                OperationResult.noOp
                                    (Some "The selected paths are unchanged.")
                                    (mkRevisionId currentRevision)
                        else
                            // Act: stream only the selected changes to the workspace branch.
                            let mutable transferFailure: OperationFailure option = None

                            for pathValue, content, objectState in changed do
                                if transferFailure.IsNone then
                                    match objectState, content with
                                    | LakeFsIndex.DeletedObject, _ ->
                                        let! deletion =
                                            LakeFsApi.deleteObject
                                                resolved
                                                state.Index.Repository
                                                state.Index.WorkspaceBranch
                                                (objectKey state pathValue)
                                                context

                                        match deletion with
                                        | Error failure -> transferFailure <- Some failure
                                        | Ok() -> ()
                                    | _, Some fileContent ->
                                        let! upload =
                                            LakeFsApi.uploadObject
                                                resolved
                                                state.Index.Repository
                                                state.Index.WorkspaceBranch
                                                (objectKey state pathValue)
                                                fileContent
                                                context

                                        match upload with
                                        | Error failure -> transferFailure <- Some failure
                                        | Ok() -> ()
                                    | _, None -> ()

                            match transferFailure with
                            | Some failure -> return Failed failure
                            | None ->
                                let expectedParent = state.Index.WorkspaceRevision

                                let! committed =
                                    LakeFsApi.commit
                                        resolved
                                        state.Index.Repository
                                        state.Index.WorkspaceBranch
                                        request.Message
                                        context

                                match committed with
                                | Error failure ->
                                    return
                                        Failed {
                                            failure with
                                                StateChanged = true
                                                RecoveryAction =
                                                    Some {
                                                        Code = "review_workspace_branch"
                                                        Instructions =
                                                            Some
                                                                "Uploaded objects remain on the workspace branch; review and retry the commit."
                                                    }
                                        }
                                | Ok commit ->
                                    // Verify: branch head is the returned commit with the expected parent.
                                    let! branchAfter =
                                        LakeFsApi.getBranch
                                            resolved
                                            state.Index.Repository
                                            state.Index.WorkspaceBranch
                                            context

                                    let verified =
                                        match branchAfter with
                                        | Ok branch ->
                                            branch.CommitId = commit.Id
                                            && (match expectedParent with
                                                | Some parent -> commit.Parents |> Array.contains parent
                                                | None -> true)
                                        | Error _ -> false

                                    // Update index entries for the selected paths only.
                                    let updatedEntries =
                                        let withoutSelected =
                                            state.Index.Entries
                                            |> Array.filter (fun entry ->
                                                not (
                                                    changed
                                                    |> Array.exists (fun (pathValue, _, _) ->
                                                        pathValue = entry.Path)
                                                ))

                                        let newEntries =
                                            changed
                                            |> Array.choose (fun (pathValue, content, objectState) ->
                                                match objectState, content with
                                                | LakeFsIndex.DeletedObject, _ -> None
                                                | _, Some fileContent ->
                                                    Some {
                                                        LakeFsIndex.Path = pathValue
                                                        LakeFsIndex.BaseChecksum = ""
                                                        LakeFsIndex.LocalHash =
                                                            LakeFsIndex.hashContent fileContent
                                                        LakeFsIndex.LocalSize = float fileContent.Length
                                                        LakeFsIndex.LocalMtimeMs = 0.0
                                                    }
                                                | _, None -> None)

                                        Array.append withoutSelected newEntries

                                    state.Index <- {
                                        state.Index with
                                            Entries = updatedEntries
                                            WorkspaceRevision = Some commit.Id
                                    }

                                    match saveIndex state with
                                    | Error failure -> return Failed { failure with StateChanged = true }
                                    | Ok() ->
                                        let outcome = {
                                            OperationOutcome.performed (mkRevisionId commit.Id) with
                                                AffectedPaths =
                                                    changed |> Array.map (fun (pathValue, _, _) -> pathValue)
                                                ResultingRevision = Some(mkRevisionId commit.Id)
                                                ResultingWorkspaceVersion = Some(workspaceVersion state)
                                                Publication = LocalOnly
                                        }

                                        if verified then
                                            return Succeeded outcome
                                        else
                                            let! observedHead =
                                                LakeFsApi.getBranch
                                                    resolved
                                                    state.Index.Repository
                                                    state.Index.WorkspaceBranch
                                                    context

                                            return
                                                OperationResult.partiallySucceeded
                                                    outcome
                                                    {
                                                        OperationFailure.create
                                                            Concurrency
                                                            "precondition_failed"
                                                            "The workspace branch advanced during commit verification." with
                                                            RevisionEvidence = [|
                                                                yield!
                                                                    expectedParent
                                                                    |> Option.map (fun parent ->
                                                                        "expected_parent", mkRevisionId parent)
                                                                    |> Option.toList
                                                                yield!
                                                                    (match observedHead with
                                                                     | Ok branch ->
                                                                         [|
                                                                             "observed_head",
                                                                             mkRevisionId branch.CommitId
                                                                         |]
                                                                     | Error _ -> [||])
                                                            |]
                                                    }
                                                    {
                                                        Code = "review_and_retry"
                                                        Instructions =
                                                            Some
                                                                "Review the observed workspace-branch head, refresh, and retry."
                                                    }
    }

let private restorePaths (state: SessionState) (request: RestoreRequest) (context: OperationContext) =
    async {
        if request.Paths.Length = 0 then
            return OperationResult.validationFailed "no_paths_selected" "Select at least one path."
        else
            let! connection = connect state

            match connection with
            | Error failure -> return Failed failure
            | Ok resolved ->
                let workspaceRef =
                    state.Index.WorkspaceRevision |> Option.defaultValue state.Index.WorkspaceBranch

                let mutable failure: OperationFailure option = None

                for path in request.Paths do
                    if failure.IsNone then
                        let pathValue = RepositoryPath.value path

                        let! content =
                            LakeFsApi.getObjectContent
                                resolved
                                state.Index.Repository
                                workspaceRef
                                (objectKey state pathValue)
                                context

                        match content with
                        | Ok objectContent -> writeLocal state pathValue objectContent
                        | Error notFound when notFound.Category = NotFound -> removeLocal state pathValue
                        | Error other -> failure <- Some other

                match failure with
                | Some value -> return Failed value
                | None -> return OperationResult.succeeded ()
    }

let private listRefs (state: SessionState) (context: OperationContext) =
    async {
        let! connection = connect state

        match connection with
        | Error failure -> return Failed failure
        | Ok resolved ->
            let! branches = LakeFsApi.listBranches resolved state.Index.Repository context

            match branches with
            | Error failure -> return Failed failure
            | Ok all ->
                // Provider workspace branches are filtered from the LOGICAL listing
                // only; they stay ordinary, server-visible lakeFS branches.
                return
                    all
                    |> Array.filter (fun branch -> not (branch.Id.StartsWith "vcs-workspace-"))
                    |> Array.filter (fun branch -> not (branch.Id.StartsWith "vcs-access-probe-"))
                    |> Array.map (fun branch -> {
                        Name = branch.Id
                        ProviderRef = mkProviderRef $"lakefs:{branch.Id}"
                        Kind = LocalRef
                        IsCurrent = branch.Id = state.Index.TargetRef
                    })
                    |> OperationResult.succeeded
    }

[<Emit("process.platform")>]
let private nodePlatform: string = jsNative

[<Emit("$0.normalize('NFC')")>]
let private normalizeNfc (_text: string) : string = jsNative

let private windowsReservedBaseNames =
    set [ "con"; "prn"; "aux"; "nul"; "com1"; "com2"; "com3"; "lpt1"; "lpt2"; "lpt3" ]

let private isWindowsInvalidName (path: string) =
    path.Split '/'
    |> Array.exists (fun segment ->
        let baseName = (segment.Split '.').[0].ToLowerInvariant()

        windowsReservedBaseNames.Contains baseName
        || segment.EndsWith "."
        || segment.EndsWith " "
        || segment |> Seq.exists (fun character -> int character < 32 || "<>:\"|?*".Contains(string character)))

/// Byte-exact keys may alias on the local filesystem: detect collisions and
/// unrepresentable names before materializing.
let private checkMaterializationSafety (paths: string list) : Result<unit, OperationFailure> =
    let aliasKey (path: string) =
        match nodePlatform with
        | "win32" -> path.ToLowerInvariant()
        | "darwin" -> (normalizeNfc path).ToLowerInvariant()
        | _ -> path

    let collisions =
        paths
        |> List.groupBy aliasKey
        |> List.filter (fun (_, group) -> group.Length > 1)
        |> List.collect snd

    if collisions.Length > 0 then
        Error {
            OperationFailure.create
                Validation
                "path_collision"
                "Distinct repository paths alias to one local file on this filesystem." with
                AffectedPaths = List.toArray collisions
        }
    elif nodePlatform = "win32" then
        let invalid = paths |> List.filter isWindowsInvalidName

        if invalid.Length > 0 then
            Error {
                OperationFailure.create
                    Validation
                    "unrepresentable_path"
                    "A repository path cannot be represented on the local filesystem." with
                    AffectedPaths = List.toArray invalid
            }
        else
            Ok()
    else
        Ok()

/// Downloads the given ref's objects (under the prefix) into the workspace and
/// rebuilds the index entries. Used by open, switch, and update.
let private materializeRef
    (state: SessionState)
    (resolved: LakeFsConnection)
    (reference: string)
    (context: OperationContext)
    : Async<Result<unit, OperationFailure>> =
    async {
        let! objects =
            LakeFsApi.listObjects resolved state.Index.Repository reference state.Index.Prefix context

        match objects with
        | Error failure -> return Error failure
        | Ok stats ->
            let keyedPaths =
                stats
                |> Array.toList
                |> List.choose (fun stat -> relativePathOfKey state stat.Path |> Option.map (fun p -> p, stat))

            match checkMaterializationSafety (keyedPaths |> List.map fst) with
            | Error failure -> return Error failure
            | Ok() ->
                let mutable failure: OperationFailure option = None
                let entries = ResizeArray<LakeFsIndex.IndexEntry>()

                for path, stat in keyedPaths do
                    if failure.IsNone then
                        let! content =
                            LakeFsApi.getObjectContent
                                resolved
                                state.Index.Repository
                                reference
                                stat.Path
                                context

                        match content with
                        | Error downloadFailure -> failure <- Some downloadFailure
                        | Ok objectContent ->
                            writeLocal state path objectContent

                            entries.Add {
                                Path = path
                                BaseChecksum = stat.Checksum
                                LocalHash = LakeFsIndex.hashContent objectContent
                                LocalSize = stat.SizeBytes
                                LocalMtimeMs = stat.Mtime
                            }

                match failure with
                | Some value -> return Error value
                | None ->
                    // Remove previously indexed files that no longer exist on the ref.
                    for entry in state.Index.Entries do
                        if not (keyedPaths |> List.exists (fun (path, _) -> path = entry.Path)) then
                            removeLocal state entry.Path

                    state.Index <- {
                        state.Index with
                            Entries = entries.ToArray()
                    }

                    return Ok()
    }

// ---------------------------------------------------------------------------
// Refs
// ---------------------------------------------------------------------------

let private refNameOfProviderRef (reference: ProviderRef) =
    let value = ProviderRef.value reference

    if value.StartsWith "lakefs:" then
        value.Substring "lakefs:".Length
    else
        value

let private createRef (state: SessionState) (request: CreateRefRequest) (context: OperationContext) =
    async {
        let! connection = connect state

        match connection with
        | Error failure -> return Failed failure
        | Ok resolved ->
            let source =
                request.BaseRef
                |> Option.map refNameOfProviderRef
                |> Option.defaultValue state.Index.TargetRef

            let! created =
                LakeFsApi.createBranch resolved state.Index.Repository request.Name source context

            match created with
            | Error failure -> return Failed failure
            | Ok() ->
                let saveResult =
                    if request.SwitchTo then
                        state.Index <- { state.Index with TargetRef = request.Name }
                        saveIndex state
                    else
                        Ok()

                match saveResult with
                | Error failure -> return Failed failure
                | Ok() ->
                    return
                        OperationResult.succeeded {
                            Name = request.Name
                            ProviderRef = mkProviderRef $"lakefs:{request.Name}"
                            Kind = LocalRef
                            IsCurrent = request.SwitchTo
                        }
    }

let private preflightSwitchRef (state: SessionState) (request: SwitchRefRequest) (context: OperationContext) =
    async {
        let! connection = connect state

        match connection with
        | Error failure -> return Failed failure
        | Ok resolved ->
            let targetName = refNameOfProviderRef request.TargetRef

            let! branch = LakeFsApi.getBranch resolved state.Index.Repository targetName context

            match branch with
            | Error failure -> return Failed failure
            | Ok _ ->
                let dirtyPaths = classifyWorkspace state |> List.map _.ChangePath |> Set.ofList

                let! objects =
                    LakeFsApi.listObjects resolved state.Index.Repository targetName state.Index.Prefix context

                match objects with
                | Error failure -> return Failed failure
                | Ok stats ->
                    let targetContent =
                        stats
                        |> Array.choose (fun stat ->
                            relativePathOfKey state stat.Path |> Option.map (fun p -> p, stat.Checksum))
                        |> Map.ofArray

                    let entriesByPath =
                        state.Index.Entries |> Array.map (fun entry -> entry.Path, entry) |> Map.ofArray

                    let atRisk =
                        dirtyPaths
                        |> Set.filter (fun path ->
                            let currentChecksum =
                                entriesByPath.TryFind path |> Option.map _.BaseChecksum

                            targetContent.TryFind path <> currentChecksum)
                        |> Set.toArray
                        |> Array.choose (RepositoryPath.tryCreate >> Result.toOption)

                    return
                        OperationResult.succeeded {
                            PathsAtRisk = atRisk
                            IsSafe = atRisk.Length = 0
                        }
    }

let private switchRef (state: SessionState) (request: SwitchRefRequest) (context: OperationContext) =
    async {
        let! connection = connect state

        match connection with
        | Error failure -> return Failed failure
        | Ok resolved ->
            let targetName = refNameOfProviderRef request.TargetRef

            let! branch = LakeFsApi.getBranch resolved state.Index.Repository targetName context

            match branch with
            | Error failure -> return Failed failure
            | Ok targetBranch ->
                let! materialized = materializeRef state resolved targetName context

                match materialized with
                | Error failure -> return Failed failure
                | Ok() ->
                    state.Index <- {
                        state.Index with
                            TargetRef = targetName
                            BaseRevision = Some targetBranch.CommitId
                            WorkspaceRevision = Some targetBranch.CommitId
                    }

                    match saveIndex state with
                    | Error failure -> return Failed failure
                    | Ok() -> return! getStatus state context
    }

let private getDiffSummary (state: SessionState) (context: OperationContext) =
    async {
        let entries =
            classifyWorkspace state
            |> List.choose (fun change ->
                match RepositoryPath.tryCreate change.ChangePath with
                | Error _ -> None
                | Ok path ->
                    let kind =
                        match change.State with
                        | LakeFsIndex.AddedObject -> AddedChange
                        | LakeFsIndex.DeletedObject -> DeletedChange
                        | _ -> ModifiedChange

                    Some {
                        Path = path
                        OldPath = None
                        Kind = kind
                        LineInsertions = None
                        LineDeletions = None
                    })
            |> List.toArray

        return OperationResult.succeeded { Entries = entries }
    }

// ---------------------------------------------------------------------------
// Synchronization: refresh / preview / update / publish
// ---------------------------------------------------------------------------

let private refresh (state: SessionState) (context: OperationContext) =
    async {
        do! barrier state "transfer-start" context

        if context.Cancellation.IsCancellationRequested() then
            return OperationResult.canceled "The refresh was canceled."
        else
            let! targetHead = getTargetHead state context

            match targetHead with
            | Error failure -> return Failed { failure with Retryable = true }
            | Ok head ->
                let! changed = targetChangedPaths state context

                let remoteChanged =
                    match changed with
                    | Ok paths ->
                        Some(paths |> List.choose (RepositoryPath.tryCreate >> Result.toOption) |> List.toArray)
                    | Error _ -> None

                return
                    OperationResult.succeeded {
                        synchronizationState state (Some head) with
                            RemoteChangedPaths = remoteChanged
                    }
    }

let private previewUpdate (state: SessionState) (context: OperationContext) =
    async {
        let! changed = targetChangedPaths state context

        match changed with
        | Error failure -> return Failed failure
        | Ok changedPaths ->
            let dirtyPaths = classifyWorkspace state |> List.map _.ChangePath |> Set.ofList

            // Locally committed changes since base: workspace branch vs base diff.
            let! connection = connect state

            match connection with
            | Error failure -> return Failed failure
            | Ok resolved ->
                let! localCommitted =
                    match state.Index.BaseRevision, state.Index.WorkspaceRevision with
                    | Some baseRevision, Some workspaceRevision when baseRevision <> workspaceRevision ->
                        async {
                            let! diff =
                                LakeFsApi.diffRefs resolved state.Index.Repository baseRevision workspaceRevision context

                            match diff with
                            | Ok entries ->
                                return
                                    entries
                                    |> Array.toList
                                    |> List.choose (fun entry -> relativePathOfKey state entry.Path)
                                    |> Set.ofList
                            | Error _ -> return Set.empty
                        }
                    | _ -> async { return Set.empty }

                let changedSet = Set.ofList changedPaths
                let overlapping = Set.intersect changedSet (Set.union dirtyPaths localCommitted)

                return
                    OperationResult.succeeded {
                        ChangedPaths =
                            changedPaths
                            |> List.choose (RepositoryPath.tryCreate >> Result.toOption)
                            |> List.toArray
                        OverlappingPaths =
                            overlapping
                            |> Set.toList
                            |> List.choose (RepositoryPath.tryCreate >> Result.toOption)
                            |> List.toArray
                        HasDataLossRisk = not (Set.isEmpty (Set.intersect changedSet dirtyPaths))
                        WouldCreateConflictSession = not (Set.isEmpty overlapping)
                    }
    }

/// Builds the conflict item contents for overlapping paths from base, workspace
/// branch, and target ref object content.
let private buildConflictItems
    (state: SessionState)
    (resolved: LakeFsConnection)
    (overlapping: string list)
    (targetHead: string)
    (context: OperationContext)
    : Async<ConflictItemState list> =
    async {
        let items = ResizeArray<ConflictItemState>()

        for path in overlapping do
            let key = objectKey state path

            let readRef reference =
                async {
                    let! content = LakeFsApi.getObjectContent resolved state.Index.Repository reference key context

                    match content with
                    | Ok value -> return Some value
                    | Error _ -> return None
                }

            let! baseContent =
                match state.Index.BaseRevision with
                | Some baseRevision -> readRef baseRevision
                | None -> async { return None }

            let! targetContent = readRef targetHead
            let workspaceContent = tryReadLocal state path

            items.Add {
                ItemPath = path
                BaseContent = baseContent
                WorkspaceContent = workspaceContent
                TargetContent = targetContent
                ResolvedContent = None
            }

        return List.ofSeq items
    }

let private update (state: SessionState) (request: UpdateRequest) (context: OperationContext) =
    async {
        if state.Conflict.IsSome then
            return
                Failed(
                    OperationFailure.create
                        Conflict
                        "conflict_session_active"
                        "Resolve or cancel the active conflict session first."
                )
        else
            do! barrier state "transfer-start" context

            if context.Cancellation.IsCancellationRequested() then
                return OperationResult.canceled "The update was canceled."
            else
                // Pre-check: read the target head.
                let! targetHead = getTargetHead state context

                match targetHead with
                | Error failure -> return Failed failure
                | Ok head when Some head = state.Index.BaseRevision ->
                    return
                        OperationResult.noOp
                            (Some "The workspace is already up to date.")
                            (synchronizationState state (Some head))
                | Ok head ->
                    let! previewResult = previewUpdate state context

                    match previewResult with
                    | Failed failure -> return Failed failure
                    | PartiallySucceeded(_, failure) -> return Failed failure
                    | Succeeded previewOutcome ->
                        let overlapping =
                            previewOutcome.Value.OverlappingPaths
                            |> Array.toList
                            |> List.map RepositoryPath.value

                        let! connection = connect state

                        match connection with
                        | Error failure -> return Failed failure
                        | Ok resolved ->
                            if not overlapping.IsEmpty then
                                // Conflicts: leave the logical target and workspace branch
                                // unchanged; open a provider-managed session.
                                let! items = buildConflictItems state resolved overlapping head context

                                state.ConflictGeneration <- state.ConflictGeneration + 1

                                state.Conflict <-
                                    Some {
                                        SessionId = $"lakefs-conflict-{head}-{state.ConflictGeneration}"
                                        HandleVersion = 1
                                        Items = items
                                        TargetRevisionAtOpen = head
                                    }

                                return
                                    OperationResult.partiallySucceeded
                                        (OperationOutcome.performed (synchronizationState state (Some head)))
                                        (OperationFailure.create
                                            Conflict
                                            "conflicts_detected"
                                            "The update produced conflicts that need resolution.")
                                        {
                                            Code = "resolve_conflict_session"
                                            Instructions = Some "Resolve every conflict item, then finalize."
                                        }
                            else
                                // Act: merge the target into the workspace branch, then
                                // verify the resulting head (client-side race window).
                                do! barrier state "update-precheck-done" context

                                let! merged =
                                    LakeFsApi.merge
                                        resolved
                                        state.Index.Repository
                                        state.Index.TargetRef
                                        state.Index.WorkspaceBranch
                                        "update: incorporate target"
                                        context

                                match merged with
                                | Error failure when failure.Category = Conflict ->
                                    return Failed failure
                                | Error failure -> return Failed failure
                                | Ok mergeResult ->
                                    let! branchAfter =
                                        LakeFsApi.getBranch
                                            resolved
                                            state.Index.Repository
                                            state.Index.WorkspaceBranch
                                            context

                                    let verified =
                                        match branchAfter with
                                        | Ok branch -> branch.CommitId = mergeResult.Reference
                                        | Error _ -> false

                                    let! materialized =
                                        materializeRef state resolved state.Index.WorkspaceBranch context

                                    match materialized with
                                    | Error failure -> return Failed { failure with StateChanged = true }
                                    | Ok() ->
                                        state.Index <- {
                                            state.Index with
                                                BaseRevision = Some head
                                                WorkspaceRevision = Some mergeResult.Reference
                                        }

                                        match saveIndex state with
                                        | Error failure -> return Failed { failure with StateChanged = true }
                                        | Ok() ->
                                            let syncState = synchronizationState state (Some head)

                                            if verified then
                                                return OperationResult.succeeded syncState
                                            else
                                                return
                                                    OperationResult.partiallySucceeded
                                                        (OperationOutcome.performed syncState)
                                                        {
                                                            OperationFailure.create
                                                                Concurrency
                                                                "precondition_failed"
                                                                "The workspace branch advanced during update verification." with
                                                                RevisionEvidence = [|
                                                                    "expected_head", mkRevisionId mergeResult.Reference
                                                                |]
                                                        }
                                                        {
                                                            Code = "review_and_retry"
                                                            Instructions =
                                                                Some "Review the workspace branch, refresh, and retry."
                                                        }
    }

let private publish (state: SessionState) (request: PublishRequest) (context: OperationContext) =
    async {
        let! connection = connect state

        match connection with
        | Error failure -> return Failed { failure with Retryable = true }
        | Ok resolved ->
            // Pre-check: the consumer-observed expected target vs the live head.
            let! targetHead = getTargetHead state context

            match targetHead with
            | Error failure -> return Failed { failure with Retryable = true }
            | Ok head ->
                let expectedMatches =
                    match request.ExpectedTargetRevision with
                    | None -> true
                    | Some expected -> RevisionId.value expected = head

                if not expectedMatches then
                    return
                        Failed {
                            staleFailure () with
                                RevisionEvidence = [|
                                    yield!
                                        request.ExpectedTargetRevision
                                        |> Option.map (fun expected -> "expected_target", expected)
                                        |> Option.toList
                                    "observed_target", mkRevisionId head
                                |]
                        }
                elif state.Index.WorkspaceRevision = Some head then
                    return
                        OperationResult.noOp
                            (Some "The target already has every local revision.")
                            (synchronizationState state (Some head))
                else
                    do! barrier state "publish-precheck-done" context
                    do! barrier state "transfer-start" context

                    if context.Cancellation.IsCancellationRequested() then
                        return OperationResult.canceled "The publish was canceled."
                    else
                        // Act: merge the workspace branch into the logical target.
                        let! merged =
                            LakeFsApi.merge
                                resolved
                                state.Index.Repository
                                state.Index.WorkspaceBranch
                                state.Index.TargetRef
                                "publish: workspace revisions"
                                context

                        match merged with
                        | Error failure ->
                            // A failed publish leaves the workspace branch intact for retry.
                            return
                                Failed {
                                    failure with
                                        Retryable = true
                                        RecoveryAction =
                                            Some {
                                                Code = "retry_publish"
                                                Instructions =
                                                    Some
                                                        "The workspace branch still holds every revision; retry once the target is reachable."
                                            }
                                }
                        | Ok mergeResult ->
                            // Verify: re-read the target head and check parentage.
                            let! branchAfter =
                                LakeFsApi.getBranch resolved state.Index.Repository state.Index.TargetRef context

                            let! mergeCommit =
                                LakeFsApi.getCommit resolved state.Index.Repository mergeResult.Reference context

                            let verified =
                                match branchAfter, mergeCommit with
                                | Ok branch, Ok commit ->
                                    branch.CommitId = mergeResult.Reference
                                    && (commit.Parents |> Array.contains head
                                        || commit.Parents.Length = 0
                                        || mergeResult.Reference = head)
                                | _ -> false

                            state.Index <- {
                                state.Index with
                                    BaseRevision = Some mergeResult.Reference
                                    WorkspaceRevision = Some mergeResult.Reference
                            }

                            match saveIndex state with
                            | Error failure -> return Failed { failure with StateChanged = true }
                            | Ok() ->
                                let outcome = {
                                    OperationOutcome.performed (synchronizationState state (Some mergeResult.Reference)) with
                                        Publication = Published
                                        ResultingRevision = Some(mkRevisionId mergeResult.Reference)
                                        ResultingWorkspaceVersion = Some(workspaceVersion state)
                                }

                                if verified then
                                    return Succeeded outcome
                                else
                                    let observedHead =
                                        match branchAfter with
                                        | Ok branch -> Some branch.CommitId
                                        | Error _ -> None

                                    return
                                        OperationResult.partiallySucceeded
                                            outcome
                                            {
                                                OperationFailure.create
                                                    Concurrency
                                                    "precondition_failed"
                                                    "The target advanced during publish verification." with
                                                    RevisionEvidence = [|
                                                        "expected_target", mkRevisionId head
                                                        yield!
                                                            observedHead
                                                            |> Option.map (fun observed ->
                                                                "observed_target", mkRevisionId observed)
                                                            |> Option.toList
                                                    |]
                                            }
                                            {
                                                Code = "review_and_retry"
                                                Instructions =
                                                    Some "Review the observed target revision, refresh, and retry."
                                            }
    }

// ---------------------------------------------------------------------------
// Conflict sessions
// ---------------------------------------------------------------------------

let private handleRejection () =
    {
        OperationFailure.create
            Concurrency
            "precondition_failed"
            "The conflict-session handle is stale, foreign, or closed." with
            RecoveryAction =
                Some {
                    Code = ConflictRecovery.RefreshConflictSession
                    Instructions = Some "Refresh the conflict session and deliberately retry with the live handle."
                }
    }

let private validateHandle (state: SessionState) (handle: ConflictSessionHandle) (expectedVersion: string) =
    match state.Conflict with
    | Some conflict when
        conflict.SessionId = handle.SessionId
        && string conflict.HandleVersion = handle.Version
        && workspaceVersion state = expectedVersion
        ->
        Ok conflict
    | _ -> Error(handleRejection ())

let private createConflictService (state: SessionState) : ConflictResolutionService = {
    GetActiveSession = fun _ -> async { return OperationResult.succeeded (conflictSummary state) }
    Resolve =
        fun request context -> async {
            match validateHandle state request.Handle request.ExpectedWorkspaceVersion with
            | Error failure -> return Failed failure
            | Ok conflict ->
                let pathValue = RepositoryPath.value request.Path

                match
                    conflict.Items
                    |> List.tryFind (fun item -> item.ItemPath = pathValue && item.ResolvedContent.IsNone)
                with
                | None ->
                    return
                        Failed(
                            OperationFailure.create
                                NotFound
                                "conflict_item_not_found"
                                "No unresolved conflict exists for the selected path."
                        )
                | Some item ->
                    let resolvedContent =
                        match request.Resolution with
                        | SupplyResolvedContent content -> Some content
                        | PickCandidate "workspace" -> item.WorkspaceContent
                        | PickCandidate "target" -> item.TargetContent
                        | PickCandidate "base" -> item.BaseContent
                        | PickCandidate _ -> None

                    match resolvedContent with
                    | None ->
                        return
                            Failed(
                                OperationFailure.create
                                    Validation
                                    "unknown_candidate"
                                    "The candidate ID is not part of this conflict item."
                            )
                    | Some content ->
                        item.ResolvedContent <- Some content
                        conflict.HandleVersion <- conflict.HandleVersion + 1

                        return
                            OperationResult.succeeded {
                                RefreshedHandle = {
                                    SessionId = conflict.SessionId
                                    Version = string conflict.HandleVersion
                                }
                                RemainingItems =
                                    conflictSummary state |> Option.map _.Items |> Option.defaultValue [||]
                            }
        }
    Finalize =
        fun request context -> async {
            match validateHandle state request.Handle request.ExpectedWorkspaceVersion with
            | Error failure -> return Failed failure
            | Ok conflict ->
                if conflict.Items |> List.exists (fun item -> item.ResolvedContent.IsNone) then
                    return
                        Failed(
                            OperationFailure.create
                                Validation
                                "conflicts_unresolved"
                                "Every conflict item must be resolved before finalizing."
                        )
                else
                    let! connection = connect state

                    match connection with
                    | Error failure -> return Failed failure
                    | Ok resolved ->
                        // Pre-check the finalize destination (the target head).
                        let! targetHead = getTargetHead state context

                        match targetHead with
                        | Error failure -> return Failed failure
                        | Ok observedTarget when observedTarget <> conflict.TargetRevisionAtOpen ->
                            return
                                Failed {
                                    handleRejection () with
                                        RevisionEvidence = [|
                                            "expected_destination", mkRevisionId conflict.TargetRevisionAtOpen
                                            "observed_destination", mkRevisionId observedTarget
                                        |]
                                }
                        | Ok observedTarget ->
                            do! barrier state "finalize-precheck-done" context

                            // Act: merge the target into the workspace branch with the
                            // destination-wins whole-merge strategy as the mechanical
                            // step, then upload each per-path resolution on top. This
                            // is custom orchestration, not native per-path merging.
                            let! merged =
                                LakeFsApi.merge
                                    resolved
                                    state.Index.Repository
                                    state.Index.TargetRef
                                    state.Index.WorkspaceBranch
                                    "merge: finalize conflict session"
                                    context

                            match merged with
                            | Error failure -> return Failed failure
                            | Ok _ ->
                                let mutable uploadFailure: OperationFailure option = None

                                for item in conflict.Items do
                                    if uploadFailure.IsNone then
                                        match item.ResolvedContent with
                                        | Some content ->
                                            let! upload =
                                                LakeFsApi.uploadObject
                                                    resolved
                                                    state.Index.Repository
                                                    state.Index.WorkspaceBranch
                                                    (objectKey state item.ItemPath)
                                                    content
                                                    context

                                            match upload with
                                            | Error failure -> uploadFailure <- Some failure
                                            | Ok() -> ()
                                        | None -> ()

                                match uploadFailure with
                                | Some failure -> return Failed { failure with StateChanged = true }
                                | None ->
                                    let! committed =
                                        LakeFsApi.commit
                                            resolved
                                            state.Index.Repository
                                            state.Index.WorkspaceBranch
                                            (request.Message |> Option.defaultValue "merge: finalize conflict session")
                                            context

                                    match committed with
                                    | Error failure -> return Failed { failure with StateChanged = true }
                                    | Ok commit ->
                                        // Verify the workspace branch head, then verify the
                                        // pre-checked target one more time.
                                        let! verifyTarget = getTargetHead state context

                                        match verifyTarget with
                                        | Ok verifiedTarget when verifiedTarget <> observedTarget ->
                                            return
                                                Failed {
                                                    handleRejection () with
                                                        RevisionEvidence = [|
                                                            "expected_destination", mkRevisionId observedTarget
                                                            "observed_destination", mkRevisionId verifiedTarget
                                                        |]
                                                }
                                        | _ ->
                                            // Materialize resolved content locally.
                                            for item in conflict.Items do
                                                match item.ResolvedContent with
                                                | Some content -> writeLocal state item.ItemPath content
                                                | None -> ()

                                            let! materialized =
                                                materializeRef state resolved state.Index.WorkspaceBranch context

                                            match materialized with
                                            | Error failure -> return Failed { failure with StateChanged = true }
                                            | Ok() ->
                                                state.Index <- {
                                                    state.Index with
                                                        BaseRevision = Some observedTarget
                                                        WorkspaceRevision = Some commit.Id
                                                }

                                                match saveIndex state with
                                                | Error failure ->
                                                    return Failed { failure with StateChanged = true }
                                                | Ok() ->
                                                    state.Conflict <- None
                                                    return OperationResult.succeeded (Some(mkRevisionId commit.Id))
        }
    Cancel =
        fun request context -> async {
            match validateHandle state request.Handle request.ExpectedWorkspaceVersion with
            | Error failure -> return Failed failure
            | Ok _ ->
                // The update never touched the workspace branch or local files for
                // conflicting paths, so canceling only closes the session.
                state.Conflict <- None
                return OperationResult.succeeded ()
        }
}

// ---------------------------------------------------------------------------
// Session assembly and open
// ---------------------------------------------------------------------------

let private createSessionFromState (state: SessionState) : WorkspaceSession =
    let descriptor = {
        ProviderId = lakeFsProviderId
        WorkspaceRoot = state.Binding.WorkspaceRoot
        Location = Some state.Binding.Location
    }

    let core: CoreVersionControl = {
        GetStatus = fun context -> getStatus state context
        ListRefs = fun context -> listRefs state context
        CreateRef =
            fun _ _ ->
                async.Return(
                    OperationResult.failed (
                        OperationFailure.create ProviderError "lakefs_core_pending" "lakeFS ref creation is not implemented yet."
                    )
                )
        PreflightSwitchRef =
            fun _ _ ->
                async.Return(
                    OperationResult.failed (
                        OperationFailure.create ProviderError "lakefs_core_pending" "lakeFS ref switching is not implemented yet."
                    )
                )
        SwitchRef =
            fun _ _ ->
                async.Return(
                    OperationResult.failed (
                        OperationFailure.create ProviderError "lakefs_core_pending" "lakeFS ref switching is not implemented yet."
                    )
                )
        CreateRevision =
            fun _ _ ->
                async.Return(
                    OperationResult.failed (
                        OperationFailure.create
                            ProviderError
                            "lakefs_selected_revision_pending"
                            "lakeFS selected revisions are not implemented yet."
                    )
                )
        RestorePaths =
            fun _ _ ->
                async.Return(
                    OperationResult.failed (
                        OperationFailure.create ProviderError "lakefs_core_pending" "lakeFS restore is not implemented yet."
                    )
                )
        GetDiffSummary =
            fun _ ->
                async.Return(
                    OperationResult.failed (
                        OperationFailure.create ProviderError "lakefs_core_pending" "lakeFS object diff is not implemented yet."
                    )
                )
    }

    {
        WorkspaceSession.createCoreOnly descriptor core with
            Synchronization =
                Some {
                    Refresh = fun context -> refresh state context
                    PreviewUpdate = fun context -> previewUpdate state context
                    Update =
                        fun request context ->
                            withValidatedMutation state request.ExpectedWorkspaceVersion (fun () ->
                                update state request context)
                    Publish =
                        fun request context ->
                            withValidatedMutation state request.ExpectedWorkspaceVersion (fun () ->
                                publish state request context)
                }
    }

/// Opens a session: loads the index or creates the provider-owned workspace
/// branch (server-visible; filtered only from logical listings) and materializes
/// the target into the workspace directory.
let openSession
    (hooks: LakeFsSessionHooks)
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    (binding: WorkspaceBinding)
    (context: OperationContext)
    : Async<OperationResult<WorkspaceSession>> =
    async {
        match LakeFsLocation.tryParse binding.Location.ProviderLocation with
        | Error message -> return Failed(OperationFailure.create Validation "invalid_location" message)
        | Ok location ->
            let! connection = credentials.ResolveConnection binding.ConnectionProfileId

            match connection with
            | Error message ->
                return
                    Failed(OperationFailure.createRedacted Authentication "connection_profile_unresolved" message)
            | Ok resolved ->
                let createOwnedState (previousEntries: LakeFsIndex.IndexEntry[]) = async {
                    let! targetBranch =
                        LakeFsApi.getBranch resolved location.Repository location.TargetRef context

                    match targetBranch with
                    | Error failure -> return Error failure
                    | Ok target ->
                        let ownershipToken = LakeFsIndex.createOwnershipToken ()
                        let workspaceBranch = workspaceBranchName binding.WorkspaceRoot ownershipToken

                        let! created =
                            LakeFsApi.createBranch
                                resolved
                                location.Repository
                                workspaceBranch
                                location.TargetRef
                                context

                        match created with
                        | Error failure -> return Error failure
                        | Ok() ->
                            if not (NodeFileSystem.existsSync binding.WorkspaceRoot) then
                                NodeFileSystem.mkdirSync
                                    binding.WorkspaceRoot
                                    (NodeFileSystem.MkdirOptions(recursive = true))

                            let state = {
                                Binding = binding
                                Location = location
                                Credentials = credentials
                                Hooks = hooks
                                Index = {
                                    SchemaVersion = LakeFsIndex.CurrentSchemaVersion
                                    Repository = location.Repository
                                    TargetRef = location.TargetRef
                                    Prefix = location.Prefix
                                    WorkspaceBranch = workspaceBranch
                                    OwnershipToken = ownershipToken
                                    BaseRevision = Some target.CommitId
                                    WorkspaceRevision = Some target.CommitId
                                    Generation = 0
                                    Entries = previousEntries
                                }
                                Conflict = None
                                ConflictGeneration = 0
                                Busy = false
                            }

                            let! materialized = materializeRef state resolved workspaceBranch context

                            match materialized with
                            | Error failure -> return Error failure
                            | Ok() ->
                                match saveIndex state with
                                | Error failure -> return Error failure
                                | Ok() -> return Ok state
                }

                match LakeFsIndex.load binding.WorkspaceRoot with
                | LakeFsIndex.Corrupt message ->
                    return Failed(OperationFailure.createRedacted ProviderError "index_corrupt" message)
                | LakeFsIndex.Loaded index ->
                    if
                        index.Repository = location.Repository
                        && index.TargetRef = location.TargetRef
                        && index.Prefix = location.Prefix
                    then
                        let state = {
                            Binding = binding
                            Location = location
                            Credentials = credentials
                            Hooks = hooks
                            Index = index
                            Conflict = None
                            ConflictGeneration = 0
                            Busy = false
                        }

                        return OperationResult.succeeded (createSessionFromState state)
                    else
                        let! retargeted = createOwnedState index.Entries

                        match retargeted with
                        | Error failure -> return Failed failure
                        | Ok state -> return OperationResult.succeeded (createSessionFromState state)
                | LakeFsIndex.Missing ->
                    let! created = createOwnedState [||]

                    match created with
                    | Error failure -> return Failed failure
                    | Ok state -> return OperationResult.succeeded (createSessionFromState state)
    }

/// Factory whose Open builds real lakeFS sessions.
let createFactory
    (hooks: LakeFsSessionHooks)
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    : ProviderFactory =
    let baseFactory = LakeFsProviderFactory.createFactory credentials

    {
        baseFactory with
            Open = fun binding context -> openSession hooks credentials binding context
    }
