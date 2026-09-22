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
module LakeFsMaterialization = VersionControlService.LakeFs.LakeFsMaterialization
module LakeFsProviderOptions = VersionControlService.LakeFs.LakeFsProviderOptions
module LakeFsStateStore = VersionControlService.LakeFs.LakeFsStateStore
module LakeFsPathSafety = VersionControlService.LakeFs.LakeFsPathSafety
module LakeFsObjectTransfer = VersionControlService.LakeFs.LakeFsObjectTransfer
module LakeFsSelectedRevision = VersionControlService.LakeFs.LakeFsSelectedRevision
module LakeFsSynchronization = VersionControlService.LakeFs.LakeFsSynchronization
module LakeFsConflictSession = VersionControlService.LakeFs.LakeFsConflictSession
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
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

type private SessionState = {
    Binding: WorkspaceBinding
    StateDirectory: string
    RecoveryDirectory: string
    Location: LakeFsLocation
    Credentials: LakeFsCredentials.LakeFsCredentialStrategy
    Hooks: LakeFsSessionHooks
    mutable Index: LakeFsIndex.WorkspaceIndex
    mutable Conflict: LakeFsConflictSession.State option
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

let private repositoryPathOfKey (state: SessionState) (key: string) =
    LakeFsPathSafety.resolveRemotePath
        state.Binding.WorkspaceRoot
        state.Index.Prefix
        key
    |> LakeFsPathSafety.orRaise

let private repositoryPath path =
    match RepositoryPath.tryCreate path with
    | Ok value -> value
    | Error message ->
        raise (
            LakeFsPathSafety.WorkspacePathFailure {
                OperationFailure.create Validation "unsafe_repository_path" message with
                    AffectedPaths = [| path |]
            }
        )

let private inspectLocalFile (state: SessionState) (path: string) =
    LakeFsPathSafety.inspectFile state.Binding.WorkspaceRoot (repositoryPath path)
    |> LakeFsPathSafety.orRaise

let private temporaryPath (state: SessionState) label =
    let directory = NodePath.join [| state.StateDirectory; "temporary" |]

    if not (NodeFileSystem.existsSync directory) then
        NodeFileSystem.mkdirSync directory (NodeFileSystem.MkdirOptions(recursive = false))

    NodePath.join [| directory; $"{label}-{NodeInterop.randomUuid()}.tmp" |]

let private cleanupConflictCandidates (state: SessionState) =
    let directory = NodePath.join [| state.StateDirectory; "temporary" |]

    if NodeFileSystem.existsSync directory then
        try
            for name in NodeFileSystem.readdirSync directory do
                if
                    name.StartsWith("conflict-candidate-", StringComparison.Ordinal)
                    || name.StartsWith("conflict-resolution-", StringComparison.Ordinal)
                then
                    try
                        NodeFileSystem.unlinkSync (NodePath.join [| directory; name |])
                    with _ ->
                        ()
        with _ ->
            ()

let private writeLocal (state: SessionState) (path: string) (content: string) =
    LakeFsPathSafety.writeUtf8File state.Binding.WorkspaceRoot (repositoryPath path) content
    |> LakeFsPathSafety.orRaise

let private removeLocal (state: SessionState) (path: string) =
    LakeFsPathSafety.removeFile state.Binding.WorkspaceRoot (repositoryPath path)
    |> LakeFsPathSafety.orRaise

/// All repo-relative files currently in the workspace directory.
let private walkLocalFiles (state: SessionState) : string list =
    LakeFsPathSafety.walkFiles state.Binding.WorkspaceRoot
    |> LakeFsPathSafety.orRaise
    |> Array.map RepositoryPath.value
    |> Array.toList

type private LocalChange = {
    ChangePath: string
    State: LakeFsIndex.LocalObjectState
    ContentHash: string option
}

/// Classifies every local file and indexed entry (content-hash authority).
let private classifyWorkspace (state: SessionState) : LocalChange list =
    let entriesByPath =
        state.Index.Entries
        |> Array.map (fun entry -> entry.Path, entry)
        |> Map.ofArray

    let localFiles = walkLocalFiles state

    let localChanges =
        localFiles
        |> List.map (fun path ->
            let entry = entriesByPath.TryFind path
            let localHash =
                inspectLocalFile state path
                |> Option.map _.Sha256

            {
                ChangePath = path
                State = LakeFsIndex.classifyLocalObject entry localHash
                ContentHash = localHash
            })

    let deletions =
        state.Index.Entries
        |> Array.toList
        |> List.filter (fun entry -> not (localFiles |> List.contains entry.Path))
        |> List.map (fun entry -> {
            ChangePath = entry.Path
            State = LakeFsIndex.DeletedObject
            ContentHash = None
        })

    (localChanges @ deletions)
    |> List.filter (fun change -> change.State <> LakeFsIndex.UnchangedObject)

let private workspaceVersion (state: SessionState) =
    let changesIdentity =
        classifyWorkspace state
        |> List.map (fun change ->
            let contentHash = change.ContentHash |> Option.defaultValue "none"
            $"{change.ChangePath}:{change.State}:{contentHash}")
        |> String.concat ";"

    let conflictPart =
        match state.Conflict with
        | Some conflict -> $"c{conflict.HandleVersion}"
        | None -> "none"

    let identityHash = LakeFsIndex.hashMetadata changesIdentity
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
    (allowRecovery: bool)
    (body: unit -> Async<OperationResult<'T>>)
    : Async<OperationResult<'T>> =
    async {
        while state.Busy do
            do! Async.Sleep 5

        state.Busy <- true

        try
            match LakeFsIndex.load state.StateDirectory with
            | LakeFsIndex.Loaded persisted
                when persisted.Repository = state.Index.Repository
                     && persisted.WorkspaceBranch = state.Index.WorkspaceBranch
                     && persisted.OwnershipToken = state.Index.OwnershipToken ->
                state.Index <- persisted

                if not allowRecovery then
                    match LakeFsMaterialization.loadPendingRecoveries state.RecoveryDirectory with
                    | Error failure -> return Failed failure
                    | Ok pending when pending.Length > 0 ->
                        return Failed(LakeFsMaterialization.pendingFailure pending)
                    | Ok _ ->
                        if workspaceVersion state <> expectedVersion then
                            return Failed(staleFailure ())
                        else
                            return! body ()
                elif workspaceVersion state <> expectedVersion then
                    return Failed(staleFailure ())
                else
                    return! body ()
            | LakeFsIndex.Loaded _ ->
                return
                    Failed(
                        OperationFailure.create
                            Concurrency
                            "workspace_binding_changed"
                            "The persisted lakeFS workspace ownership changed; reopen the session."
                    )
            | LakeFsIndex.Missing ->
                return
                    Failed(
                        OperationFailure.create
                            ProviderError
                            "workspace_index_missing"
                            "The persisted lakeFS workspace index is missing."
                    )
            | LakeFsIndex.Corrupt message ->
                return
                    Failed(
                        OperationFailure.createRedacted
                            ProviderError
                            "workspace_index_corrupt"
                            message
                    )
        finally
            state.Busy <- false
    }

let private pendingRecoveryGate (state: SessionState) =
    match LakeFsMaterialization.loadPendingRecoveries state.RecoveryDirectory with
    | Error failure -> Error failure
    | Ok pending when pending.Length > 0 ->
        Error(LakeFsMaterialization.pendingFailure pending)
    | Ok _ -> Ok()

let private guardConflictMutation state operation =
    async {
        match pendingRecoveryGate state with
        | Error failure -> return Failed failure
        | Ok() -> return! operation
    }

let private saveIndex (state: SessionState) =
    match LakeFsIndex.save state.StateDirectory state.Index with
    | Ok saved ->
        state.Index <- saved
        Ok()
    | Error message -> Error(OperationFailure.createRedacted ProviderError "index_write_failed" message)

let private reloadIndex (state: SessionState) =
    match LakeFsIndex.load state.StateDirectory with
    | LakeFsIndex.Loaded persisted
        when persisted.Repository = state.Index.Repository
             && persisted.WorkspaceBranch = state.Index.WorkspaceBranch
             && persisted.OwnershipToken = state.Index.OwnershipToken ->
        state.Index <- persisted
        Ok()
    | LakeFsIndex.Loaded _ ->
        Error(
            OperationFailure.create
                Concurrency
                "workspace_binding_changed"
                "The persisted lakeFS workspace ownership changed; reopen the session."
        )
    | LakeFsIndex.Missing ->
        Error(
            OperationFailure.create
                ProviderError
                "workspace_index_missing"
                "The persisted lakeFS workspace index is missing."
        )
    | LakeFsIndex.Corrupt message ->
        Error(OperationFailure.createRedacted ProviderError "workspace_index_corrupt" message)

let private conflictSummary (state: SessionState) : ConflictSessionSummary option =
    LakeFsConflictSession.summary
        (state.Index.BaseRevision |> Option.map mkRevisionId)
        (state.Index.WorkspaceRevision |> Option.map mkRevisionId)
        state.Conflict

let private mapOutcomeValue (value: 'T) (outcome: OperationOutcome<'U>) : OperationOutcome<'T> = {
    Value = value
    Effect = outcome.Effect
    Warnings = outcome.Warnings
    AffectedPaths = outcome.AffectedPaths
    ResultingRevision = outcome.ResultingRevision
    ResultingWorkspaceVersion = outcome.ResultingWorkspaceVersion
    Publication = outcome.Publication
}

let private localWorkspaceStatus (state: SessionState) : WorkspaceStatus =
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

    {
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
        Synchronization = None
    }

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
        TargetRef =
            targetHead
            |> Option.map (fun _ -> {
                Name = state.Index.TargetRef
                ProviderRef = mkProviderRef $"lakefs:{state.Index.TargetRef}"
                Kind = LocalRef
                IsCurrent = false
            })
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
let private targetChangedPathsAgainst
    (state: SessionState)
    (head: string)
    (context: OperationContext)
    =
    async {
        match state.Index.BaseRevision with
        | None -> return Ok []
        | Some baseRevision ->
            let! connection = connect state

            match connection with
            | Error failure -> return Error failure
            | Ok resolved ->
                let! diff =
                    LakeFsApi.diffRefs resolved state.Index.Repository baseRevision head context

                match diff with
                | Error failure -> return Error failure
                | Ok entries ->
                    return
                        Ok(
                            entries
                            |> Array.toList
                            |> List.choose (fun entry -> repositoryPathOfKey state entry.Path)
                            |> List.map RepositoryPath.value
                        )
    }

let private targetChangedPaths (state: SessionState) (context: OperationContext) =
    async {
        let! targetHead = getTargetHead state context

        match targetHead with
        | Error failure -> return Error failure
        | Ok head -> return! targetChangedPathsAgainst state head context
    }

// ---------------------------------------------------------------------------
// Core operations
// ---------------------------------------------------------------------------

let private getStatus (state: SessionState) (context: OperationContext) =
    async {
        match reloadIndex state with
        | Error failure -> return Failed failure
        | Ok() ->
            let! connection = connect state

            match connection with
            | Error failure -> return Failed failure
            | Ok resolved ->
                let! targetBranch =
                    LakeFsApi.getBranch resolved state.Index.Repository state.Index.TargetRef context

                let! workspaceBranch =
                    LakeFsApi.getBranch resolved state.Index.Repository state.Index.WorkspaceBranch context

                match targetBranch, workspaceBranch with
                | Error failure, _
                | _, Error failure -> return Failed failure
                | Ok target, Ok workspace ->
                    let! remoteChanges =
                        match state.Index.BaseRevision with
                        | None -> async.Return(Ok [||])
                        | Some baseRevision ->
                            LakeFsApi.diffRefs
                                resolved
                                state.Index.Repository
                                baseRevision
                                state.Index.TargetRef
                                context

                    match remoteChanges with
                    | Error failure -> return Failed failure
                    | Ok changedObjects ->
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

                        let observedState = {
                            state with
                                Index = {
                                    state.Index with
                                        WorkspaceRevision = Some workspace.CommitId
                                }
                        }

                        let remotePaths =
                            changedObjects
                            |> Array.choose (fun entry -> repositoryPathOfKey state entry.Path)

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
                                Synchronization =
                                    Some {
                                        synchronizationState observedState (Some target.CommitId) with
                                            RemoteChangedPaths = Some remotePaths
                                    }
                            }
    }

let private interruptedSelectedRevisionFailure
    (stateChanged: bool)
    (expectedHead: string option)
    (observedHead: string option)
    =
    {
        OperationFailure.create
            Canceled
            "operation_canceled"
            (if stateChanged then
                 "The selected revision was canceled after the remote workspace may have changed."
             else
                 "The selected revision was canceled and its remote transient state was cleaned.") with
            StateChanged = stateChanged
            Retryable = not stateChanged
            RecoveryAction =
                if stateChanged then
                    Some {
                        Code = "review_workspace_branch"
                        Instructions =
                            Some
                                "Review the owned workspace branch, refresh its observed head, and retry deliberately."
                    }
                else
                    None
            RevisionEvidence = [|
                yield!
                    expectedHead
                    |> Option.map (fun revision -> "expected_head", mkRevisionId revision)
                    |> Option.toList
                yield!
                    observedHead
                    |> Option.map (fun revision -> "observed_head", mkRevisionId revision)
                    |> Option.toList
            |]
    }

/// Cancellation recovery uses a detached context because the caller's signal is
/// already set. It only resets the provider-owned branch when both the local
/// ownership proof and the live expected head still match.
let private recoverInterruptedSelectedRevision
    (state: SessionState)
    (resolved: LakeFsConnection)
    (expectedHead: string option)
    (operationId: string)
    =
    async {
        let recoveryContext = OperationContext.detached $"{operationId}-recovery"
        let ownsBranch =
            state.Index.OwnershipToken <> ""
            && workspaceBranchName state.Binding.WorkspaceRoot state.Index.OwnershipToken
               = state.Index.WorkspaceBranch

        match ownsBranch, expectedHead with
        | false, _
        | _, None ->
            return interruptedSelectedRevisionFailure true expectedHead None
        | true, Some expected ->
            let! observed =
                LakeFsApi.getBranch
                    resolved
                    state.Index.Repository
                    state.Index.WorkspaceBranch
                    recoveryContext

            match observed with
            | Error _ ->
                return interruptedSelectedRevisionFailure true expectedHead None
            | Ok branch when branch.CommitId <> expected ->
                return
                    interruptedSelectedRevisionFailure
                        true
                        expectedHead
                        (Some branch.CommitId)
            | Ok _ ->
                let! deleted =
                    LakeFsApi.deleteBranch
                        resolved
                        state.Index.Repository
                        state.Index.WorkspaceBranch
                        recoveryContext

                match deleted with
                | Error _ ->
                    return interruptedSelectedRevisionFailure true expectedHead (Some expected)
                | Ok() ->
                    let! recreated =
                        LakeFsApi.createBranch
                            resolved
                            state.Index.Repository
                            state.Index.WorkspaceBranch
                            expected
                            recoveryContext

                    match recreated with
                    | Error _ ->
                        return interruptedSelectedRevisionFailure true expectedHead None
                    | Ok() ->
                        let! verified =
                            LakeFsApi.getBranch
                                resolved
                                state.Index.Repository
                                state.Index.WorkspaceBranch
                                recoveryContext

                        match verified with
                        | Ok branch when branch.CommitId = expected ->
                            return interruptedSelectedRevisionFailure false expectedHead (Some expected)
                        | Ok branch ->
                            return
                                interruptedSelectedRevisionFailure
                                    true
                                    expectedHead
                                    (Some branch.CommitId)
                        | Error _ ->
                            return interruptedSelectedRevisionFailure true expectedHead None
    }

let private finishSelectedRevision
    (state: SessionState)
    (changed: (string * LakeFsPathSafety.InspectedFile option * LakeFsIndex.LocalObjectState)[])
    (completed: LakeFsObjectTransfer.CompletedObjectTransfer[])
    (expectedParent: string option)
    (commit: LakeFsCommit)
    (branchAfter: Result<LakeFsBranch, OperationFailure>)
    (commitAfter: Result<LakeFsCommit, OperationFailure>)
    =
    async {
        let verified =
            match branchAfter, commitAfter with
            | Ok branch, Ok observedCommit ->
                LakeFsSelectedRevision.verifiesExpectedParentAndHead
                    expectedParent
                    observedCommit
                    branch
            | _ -> false

        let affectedPaths =
            changed |> Array.map (fun (pathValue, _, _) -> pathValue)

        if verified then
            let updatedEntries =
                let uploadedByPath =
                    completed
                    |> Array.choose (fun transferred ->
                        transferred.Uploaded
                        |> Option.map (fun uploaded -> transferred.Path, uploaded))
                    |> Map.ofArray

                let withoutSelected =
                    state.Index.Entries
                    |> Array.filter (fun entry ->
                        not (
                            changed
                            |> Array.exists (fun (pathValue, _, _) -> pathValue = entry.Path)
                        ))

                let newEntries =
                    changed
                    |> Array.choose (fun (pathValue, inspected, objectState) ->
                        match objectState, inspected with
                        | LakeFsIndex.DeletedObject, _ -> None
                        | _, Some _ ->
                            let uploaded = uploadedByPath.[pathValue]
                            Some {
                                LakeFsIndex.Path = pathValue
                                LakeFsIndex.BaseChecksum = ""
                                LakeFsIndex.LocalHash = uploaded.Sha256
                                LakeFsIndex.LocalSize = uploaded.BytesCopied
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
                return
                    Succeeded {
                        OperationOutcome.performed (mkRevisionId commit.Id) with
                            AffectedPaths = affectedPaths
                            ResultingRevision = Some(mkRevisionId commit.Id)
                            ResultingWorkspaceVersion = Some(workspaceVersion state)
                            Publication = LocalOnly
                    }
        else
            let observedHead =
                match branchAfter with
                | Ok branch -> Some branch.CommitId
                | Error _ -> None

            let outcome = {
                OperationOutcome.performed (mkRevisionId commit.Id) with
                    AffectedPaths = affectedPaths
                    ResultingRevision = Some(mkRevisionId commit.Id)
                    ResultingWorkspaceVersion = Some(workspaceVersion state)
                    Publication = LocalOnly
            }

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
                                    |> Option.map (fun parent -> "expected_parent", mkRevisionId parent)
                                    |> Option.toList
                                yield!
                                    observedHead
                                    |> Option.map (fun head -> "observed_head", mkRevisionId head)
                                    |> Option.toList
                            |]
                    }
                    {
                        Code = "review_and_retry"
                        Instructions =
                            Some "Review the observed workspace-branch head, refresh, and retry."
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
                            let inspected = inspectLocalFile state pathValue

                            pathValue,
                            inspected,
                            LakeFsIndex.classifyLocalObject
                                (entriesByPath.TryFind pathValue)
                                (inspected |> Option.map _.Sha256))

                    let missing =
                        selections
                        |> Array.filter (fun (pathValue, inspected, _) ->
                            inspected.IsNone && not (entriesByPath.ContainsKey pathValue))
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
                            do! barrier state "selected-revision-sources-classified" context

                            // Act: stream only the selected changes to the workspace branch.
                            let expectedParent = state.Index.WorkspaceRevision
                            let selectedTransfers: LakeFsObjectTransfer.SelectedObjectTransfer[] =
                                changed
                                |> Array.map (fun (pathValue, inspected, objectState) -> {
                                    Path = pathValue
                                    ObjectKey = objectKey state pathValue
                                    SourcePath = inspected |> Option.map _.AbsolutePath
                                    ValidateSource =
                                        inspected
                                        |> Option.map (fun source ->
                                            LakeFsPathSafety.validateOpenedFile
                                                state.Binding.WorkspaceRoot
                                                (repositoryPath pathValue)
                                                source.Identity)
                                    IsDeletion = objectState = LakeFsIndex.DeletedObject
                                })

                            let! transferResult =
                                LakeFsObjectTransfer.transferSelected
                                    resolved
                                    state.Index.Repository
                                    state.Index.WorkspaceBranch
                                    selectedTransfers
                                    context

                            match transferResult with
                            | LakeFsObjectTransfer.TransferFailed(failure, completed)
                                when failure.Category = Canceled ->
                                let! interrupted =
                                    recoverInterruptedSelectedRevision
                                        state
                                        resolved
                                        expectedParent
                                        context.OperationId

                                return Failed interrupted
                            | LakeFsObjectTransfer.TransferFailed(failure, completed) ->
                                let completedPaths = completed |> Array.map _.Path
                                let affectedPaths =
                                    Array.append completedPaths failure.AffectedPaths
                                    |> Array.distinct

                                return
                                    Failed {
                                        failure with
                                            StateChanged = completedPaths.Length > 0
                                            AffectedPaths = affectedPaths
                                            RecoveryAction =
                                                if completedPaths.Length > 0 then
                                                    Some {
                                                        Code = "review_workspace_branch"
                                                        Instructions =
                                                            Some
                                                                "Some selected objects may remain uncommitted on the owned workspace branch."
                                                    }
                                                else
                                                    failure.RecoveryAction
                                    }
                            | LakeFsObjectTransfer.TransferCompleted completed ->
                                context.ReportProgress {
                                    PhaseCode = "selected-upload-complete"
                                    Item = None
                                    Completed = Some(float completed.Length)
                                    Total = Some(float selectedTransfers.Length)
                                    DisplayMessage = Some "Selected lakeFS objects uploaded"
                                }

                                do! barrier state "selected-revision-upload-done" context

                                if context.Cancellation.IsCancellationRequested() then
                                    let! interrupted =
                                        recoverInterruptedSelectedRevision
                                            state
                                            resolved
                                            expectedParent
                                            context.OperationId

                                    return Failed interrupted
                                else

                                    let! committed =
                                        LakeFsApi.commit
                                            resolved
                                            state.Index.Repository
                                            state.Index.WorkspaceBranch
                                            request.Message
                                            context

                                    match committed with
                                    | Error failure when failure.Category = Canceled ->
                                        let! interrupted =
                                            recoverInterruptedSelectedRevision
                                                state
                                                resolved
                                                expectedParent
                                                context.OperationId

                                        return Failed interrupted
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
                                        do! barrier state "selected-revision-commit-done" context

                                        if context.Cancellation.IsCancellationRequested() then
                                            let! interrupted =
                                                recoverInterruptedSelectedRevision
                                                    state
                                                    resolved
                                                    expectedParent
                                                    context.OperationId

                                            return Failed interrupted
                                        else

                                            // Verify only after the commit is visible to concurrent writers.
                                            let! branchAfter =
                                                LakeFsApi.getBranch
                                                    resolved
                                                    state.Index.Repository
                                                    state.Index.WorkspaceBranch
                                                    context

                                            let! commitAfter =
                                                LakeFsApi.getCommit
                                                    resolved
                                                    state.Index.Repository
                                                    commit.Id
                                                    context

                                            return!
                                                finishSelectedRevision
                                                    state
                                                    changed
                                                    completed
                                                    expectedParent
                                                    commit
                                                    branchAfter
                                                    commitAfter
    }

let private materializationResultToOperation
    (state: SessionState)
    (result: LakeFsMaterialization.MaterializationResult)
    : OperationResult<unit> =
    match result with
    | LakeFsMaterialization.Materialized(saved, affectedPaths, warnings) ->
        state.Index <- saved

        Succeeded {
            OperationOutcome.performed () with
                Warnings = warnings
                AffectedPaths = affectedPaths
                ResultingWorkspaceVersion = Some(workspaceVersion state)
        }
    | LakeFsMaterialization.MaterializationFailed failure -> Failed failure
    | LakeFsMaterialization.MaterializationPartiallyApplied(saved, failure) ->
        saved |> Option.iter (fun index -> state.Index <- index)

        PartiallySucceeded(
            {
                OperationOutcome.performed () with
                    AffectedPaths = failure.AffectedPaths
                    ResultingWorkspaceVersion = Some(workspaceVersion state)
            },
            failure
        )

let private validateRestoreRequest (request: RestoreRequest) =
    if request.Paths.Length = 0 then
        Error(
            OperationFailure.create
                Validation
                "no_paths_selected"
                "Select at least one path."
        )
    else
        LakeFsPathSafety.validateMaterializationPaths request.Paths

let private restorePathsWithoutRecovery (state: SessionState) (request: RestoreRequest) (context: OperationContext) =
    async {
        match validateRestoreRequest request with
        | Error failure -> return Failed failure
        | Ok() ->
                let! connection = connect state

                match connection with
                | Error failure -> return Failed failure
                | Ok resolved ->
                    let workspaceRef =
                        state.Index.WorkspaceRevision |> Option.defaultValue state.Index.WorkspaceBranch

                    let! listed =
                        LakeFsApi.listObjects
                            resolved
                            state.Index.Repository
                            workspaceRef
                            state.Index.Prefix
                            context

                    match listed with
                    | Error failure -> return Failed failure
                    | Ok stats ->
                        let statsByPath =
                            stats
                            |> Array.choose (fun stat ->
                                repositoryPathOfKey state stat.Path
                                |> Option.map (fun path -> RepositoryPath.value path, stat))
                            |> Map.ofArray

                        let materializationObjects =
                            ResizeArray<LakeFsMaterialization.MaterializationObject>()

                        let removals = ResizeArray<RepositoryPath * string>()
                        let mutable validationFailure: OperationFailure option = None

                        for path in request.Paths do
                            if validationFailure.IsNone then
                                match
                                    LakeFsPathSafety.resolveWorkspacePath
                                        state.Binding.WorkspaceRoot
                                        path
                                with
                                | Error failure -> validationFailure <- Some failure
                                | Ok targetPath ->
                                    match statsByPath.TryFind(RepositoryPath.value path) with
                                    | Some stat ->
                                        materializationObjects.Add {
                                            Path = path
                                            ObjectKey = stat.Path
                                            TargetPath = targetPath
                                            BaseChecksum = stat.Checksum
                                            Mtime = stat.Mtime
                                        }
                                    | None -> removals.Add(path, targetPath)

                        match validationFailure with
                        | Some failure -> return Failed failure
                        | None ->
                            let transactionsDirectory =
                                NodePath.join [| state.StateDirectory; "transactions" |]

                            let! prepared =
                                LakeFsMaterialization.prepareSelected
                                    transactionsDirectory
                                    state.Index
                                    false
                                    (materializationObjects.ToArray())
                                    (removals.ToArray())
                                    (fun preparedObject downloadedPath downloadContext ->
                                        LakeFsApi.downloadObjectToFile
                                            resolved
                                            state.Index.Repository
                                            workspaceRef
                                            preparedObject.ObjectKey
                                            downloadedPath
                                            downloadContext)
                                    (fun point barrierContext -> barrier state point barrierContext)
                                    context

                            match prepared with
                            | Error failure -> return Failed failure
                            | Ok plan ->
                                let! applied =
                                    LakeFsMaterialization.apply
                                        state.Binding.WorkspaceRoot
                                        state.StateDirectory
                                        state.RecoveryDirectory
                                        state.Index.WorkspaceRevision
                                        workspaceRef
                                        plan
                                        (fun point barrierContext -> barrier state point barrierContext)
                                        context

                                return materializationResultToOperation state applied
    }

let private restorePaths (state: SessionState) (request: RestoreRequest) (context: OperationContext) =
    async {
        match validateRestoreRequest request with
        | Error failure -> return Failed failure
        | Ok() ->
            let! recovery =
                LakeFsMaterialization.reapplyPending
                    state.Binding.WorkspaceRoot
                    state.StateDirectory
                    state.RecoveryDirectory
                    (fun point barrierContext -> barrier state point barrierContext)
                    context

            match recovery with
            | Error failure -> return Failed failure
            | Ok(Some result) ->
                match materializationResultToOperation state result with
                | Succeeded _ -> return! restorePathsWithoutRecovery state request context
                | Failed failure -> return Failed failure
                | PartiallySucceeded(outcome, failure) ->
                    return PartiallySucceeded(outcome, failure)
            | Ok None -> return! restorePathsWithoutRecovery state request context
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

let private materializationRecoveryFailure
    (recoveryDirectory: string)
    (expectedRevision: string option)
    (observedRevision: string)
    (affectedPaths: string[])
    (source: OperationFailure)
    =
    {
        source with
            StateChanged = true
            Retryable = true
            AffectedPaths = affectedPaths
            RecoveryAction =
                Some(LakeFsStateStore.reconcileRecoveryAction [| recoveryDirectory |])
            RevisionEvidence = [|
                yield! source.RevisionEvidence
                yield!
                    expectedRevision
                    |> Option.map (fun revision -> "expected_workspace", mkRevisionId revision)
                    |> Option.toList
                "observed_materialization", mkRevisionId observedRevision
            |]
    }

/// Downloads the objects of a ref under the prefix into the workspace and rebuilds the
/// index entries. Open and switch call it without a target diff. Update passes a set
/// after a merge, so objects outside that set are skipped when the index already tracks
/// their paths. This preserves local deletions and keeps their index entries available
/// to workspace classification.
let private materializeRef
    (state: SessionState)
    (resolved: LakeFsConnection)
    (reference: string)
    (preserveExistingFiles: bool)
    (targetChangedPaths: Set<string> option)
    (finalizeIndex: LakeFsIndex.WorkspaceIndex -> LakeFsIndex.WorkspaceIndex)
    (context: OperationContext)
    : Async<OperationResult<unit>> =
    async {
        let! objects =
            LakeFsApi.listObjects resolved state.Index.Repository reference state.Index.Prefix context

        match objects with
        | Error failure -> return Failed failure
        | Ok stats ->
            let keyedPaths =
                stats
                |> Array.toList
                |> List.choose (fun stat -> repositoryPathOfKey state stat.Path |> Option.map (fun p -> p, stat))

            match
                keyedPaths
                |> List.map fst
                |> List.toArray
                |> LakeFsPathSafety.validateMaterializationPaths
            with
            | Error failure -> return Failed failure
            | Ok() ->
                let mutable validationFailure: OperationFailure option = None
                let materializationObjects = ResizeArray<LakeFsMaterialization.MaterializationObject>()
                let removals = ResizeArray<RepositoryPath * string>()

                for repositoryPath, stat in keyedPaths do
                    if validationFailure.IsNone then
                        match
                            LakeFsPathSafety.resolveWorkspacePath
                                state.Binding.WorkspaceRoot
                                repositoryPath
                        with
                        | Error pathFailure -> validationFailure <- Some pathFailure
                        | Ok targetPath ->
                            let hasIndexEntry =
                                state.Index.Entries
                                |> Array.exists (fun entry ->
                                    entry.Path = RepositoryPath.value repositoryPath)

                            let skipObject =
                                match targetChangedPaths with
                                | Some changed ->
                                    not (Set.contains (RepositoryPath.value repositoryPath) changed)
                                    && hasIndexEntry
                                | None -> false

                            if not skipObject then
                                materializationObjects.Add {
                                    Path = repositoryPath
                                    ObjectKey = stat.Path
                                    TargetPath = targetPath
                                    BaseChecksum = stat.Checksum
                                    Mtime = stat.Mtime
                                }

                for entry in state.Index.Entries do
                    if
                        validationFailure.IsNone
                        && not (
                            keyedPaths
                            |> List.exists (fun (path, _) -> RepositoryPath.value path = entry.Path)
                        )
                    then
                        match RepositoryPath.tryCreate entry.Path with
                        | Error message ->
                            validationFailure <-
                                Some {
                                    OperationFailure.create
                                        Validation
                                        "unsafe_repository_path"
                                        message with
                                        AffectedPaths = [| entry.Path |]
                                }
                        | Ok removalPath ->
                            match
                                LakeFsPathSafety.resolveWorkspacePath
                                    state.Binding.WorkspaceRoot
                                    removalPath
                            with
                            | Error pathFailure -> validationFailure <- Some pathFailure
                            | Ok targetPath -> removals.Add(removalPath, targetPath)

                match validationFailure with
                | Some failure -> return Failed failure
                | None ->
                    let transactionsDirectory =
                        NodePath.join [| state.StateDirectory; "transactions" |]

                    let prepare =
                        match targetChangedPaths with
                        | Some _ -> LakeFsMaterialization.prepareSelected
                        | None -> LakeFsMaterialization.prepare

                    let! prepared =
                        prepare
                            transactionsDirectory
                            state.Index
                            preserveExistingFiles
                            (materializationObjects.ToArray())
                            (removals.ToArray())
                            (fun preparedObject downloadedPath downloadContext ->
                                LakeFsApi.downloadObjectToFile
                                    resolved
                                    state.Index.Repository
                                    reference
                                    preparedObject.ObjectKey
                                    downloadedPath
                                    downloadContext)
                            (fun point barrierContext -> barrier state point barrierContext)
                            context

                    match prepared with
                    | Error failure -> return Failed failure
                    | Ok plan ->
                        let plan = {
                            plan with
                                NextIndex = finalizeIndex plan.NextIndex
                        }

                        let! applied =
                            LakeFsMaterialization.apply
                                state.Binding.WorkspaceRoot
                                state.StateDirectory
                                state.RecoveryDirectory
                                state.Index.WorkspaceRevision
                                reference
                                plan
                                (fun point barrierContext -> barrier state point barrierContext)
                                context

                        return materializationResultToOperation state applied
    }

// ---------------------------------------------------------------------------
// Refs
// ---------------------------------------------------------------------------

let private tryRefNameOfProviderRef (reference: ProviderRef) =
    let value = ProviderRef.value reference

    if value.StartsWith "lakefs:" && value.Length > "lakefs:".Length then
        Ok(value.Substring "lakefs:".Length)
    else
        Error(
            OperationFailure.create
                Validation
                "invalid_provider_ref"
                "The provider ref is not a lakeFS ref."
        )

let private resetOwnedWorkspaceBranch
    (state: SessionState)
    (resolved: LakeFsConnection)
    (expectedCurrentHead: string option)
    (sourceRevision: string)
    (context: OperationContext)
    =
    async {
        let! current =
            LakeFsApi.getBranch
                resolved
                state.Index.Repository
                state.Index.WorkspaceBranch
                context

        match current with
        | Error failure -> return Error failure
        | Ok observed when expectedCurrentHead <> Some observed.CommitId ->
            return
                Error {
                    OperationFailure.create
                        Concurrency
                        "precondition_failed"
                        "The owned workspace branch moved before it could be reset." with
                        RevisionEvidence = [|
                            yield!
                                expectedCurrentHead
                                |> Option.map (fun revision -> "expected_head", mkRevisionId revision)
                                |> Option.toList
                            "observed_head", mkRevisionId observed.CommitId
                        |]
                }
        | Ok _ ->
            let! deleted =
                LakeFsApi.deleteBranch
                    resolved
                    state.Index.Repository
                    state.Index.WorkspaceBranch
                    context

            match deleted with
            | Error failure -> return Error failure
            | Ok() ->
                let! created =
                    LakeFsApi.createBranch
                        resolved
                        state.Index.Repository
                        state.Index.WorkspaceBranch
                        sourceRevision
                        context

                match created with
                | Error failure -> return Error { failure with StateChanged = true }
                | Ok() ->
                    let! recreated =
                        LakeFsApi.getBranch
                            resolved
                            state.Index.Repository
                            state.Index.WorkspaceBranch
                            context

                    match recreated with
                    | Error failure -> return Error { failure with StateChanged = true }
                    | Ok branch when branch.CommitId = sourceRevision -> return Ok()
                    | Ok branch ->
                        return
                            Error {
                                OperationFailure.create
                                    Concurrency
                                    "precondition_failed"
                                    "The recreated workspace branch does not point at the selected revision." with
                                    StateChanged = true
                                    RevisionEvidence = [|
                                        "expected_head", mkRevisionId sourceRevision
                                        "observed_head", mkRevisionId branch.CommitId
                                    |]
                            }
    }

let private switchWorkspaceToRef
    (state: SessionState)
    (resolved: LakeFsConnection)
    (targetName: string)
    (targetRevision: string)
    (context: OperationContext)
    =
    async {
        let expectedWorkspace = state.Index.WorkspaceRevision
        let finalizeIndex (index: LakeFsIndex.WorkspaceIndex) = {
            index with
                TargetRef = targetName
                BaseRevision = Some targetRevision
                WorkspaceRevision = Some targetRevision
        }

        let! materialized =
            materializeRef state resolved targetRevision false None finalizeIndex context

        match materialized with
        | Failed failure -> return Failed failure
        | PartiallySucceeded(outcome, failure) ->
            return PartiallySucceeded(outcome, failure)
        | Succeeded materializationOutcome ->
            let! reset =
                resetOwnedWorkspaceBranch
                    state
                    resolved
                    expectedWorkspace
                    targetRevision
                    context

            match reset with
            | Error failure ->
                return
                    PartiallySucceeded(
                        materializationOutcome,
                        materializationRecoveryFailure
                            state.RecoveryDirectory
                            expectedWorkspace
                            targetRevision
                            materializationOutcome.AffectedPaths
                            failure
                    )
            | Ok() ->
                return
                    Succeeded {
                        materializationOutcome with
                            ResultingWorkspaceVersion = Some(workspaceVersion state)
                    }
    }

let private createRef (state: SessionState) (request: CreateRefRequest) (context: OperationContext) =
    async {
        let! connection = connect state

        match connection with
        | Error failure -> return Failed failure
        | Ok resolved ->
            let sourceResult =
                match request.BaseRef with
                | None -> Ok state.Index.TargetRef
                | Some reference -> tryRefNameOfProviderRef reference

            match sourceResult with
            | Error failure -> return Failed failure
            | Ok source ->
                let! created =
                    LakeFsApi.createBranch resolved state.Index.Repository request.Name source context

                match created with
                | Error failure -> return Failed failure
                | Ok() ->
                    if not request.SwitchTo then
                        return
                            OperationResult.succeeded {
                                Name = request.Name
                                ProviderRef = mkProviderRef $"lakefs:{request.Name}"
                                Kind = LocalRef
                                IsCurrent = false
                            }
                    else
                        let! createdBranch =
                            LakeFsApi.getBranch resolved state.Index.Repository request.Name context

                        match createdBranch with
                        | Error failure -> return Failed { failure with StateChanged = true }
                        | Ok branch ->
                            let! switched =
                                switchWorkspaceToRef state resolved request.Name branch.CommitId context

                            match switched with
                            | Failed failure -> return Failed { failure with StateChanged = true }
                            | PartiallySucceeded(outcome, failure) ->
                                let logicalRef = {
                                    Name = request.Name
                                    ProviderRef = mkProviderRef $"lakefs:{request.Name}"
                                    Kind = LocalRef
                                    IsCurrent = false
                                }

                                return
                                    PartiallySucceeded(
                                        mapOutcomeValue logicalRef outcome,
                                        { failure with StateChanged = true }
                                    )
                            | Succeeded _ ->
                                return
                                    OperationResult.succeeded {
                                        Name = request.Name
                                        ProviderRef = mkProviderRef $"lakefs:{request.Name}"
                                        Kind = LocalRef
                                        IsCurrent = true
                                    }
    }

let private preflightSwitchRef (state: SessionState) (request: SwitchRefRequest) (context: OperationContext) =
    async {
        let! connection = connect state

        match connection with
        | Error failure -> return Failed failure
        | Ok resolved ->
            match tryRefNameOfProviderRef request.TargetRef with
            | Error failure -> return Failed failure
            | Ok targetName ->
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
                                repositoryPathOfKey state stat.Path
                                |> Option.map (fun p -> RepositoryPath.value p, stat.Checksum))
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
            match tryRefNameOfProviderRef request.TargetRef with
            | Error failure -> return Failed failure
            | Ok targetName ->
                let! branch = LakeFsApi.getBranch resolved state.Index.Repository targetName context

                match branch with
                | Error failure -> return Failed failure
                | Ok targetBranch ->
                    let! switched =
                        switchWorkspaceToRef state resolved targetName targetBranch.CommitId context

                    match switched with
                    | Failed failure -> return Failed failure
                    | PartiallySucceeded(outcome, failure) ->
                        return
                            PartiallySucceeded(
                                mapOutcomeValue (localWorkspaceStatus state) outcome,
                                failure
                            )
                    | Succeeded _ -> return! getStatus state context
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
                let! changed = targetChangedPathsAgainst state head context

                match changed with
                | Error failure -> return Failed failure
                | Ok paths ->
                    return
                        synchronizationState state (Some head)
                        |> LakeFsSynchronization.withRemoteChangedPaths paths
                        |> OperationResult.succeeded
    }

let private previewAgainst (state: SessionState) (head: string) (context: OperationContext) =
    async {
        let noSynchronizedBase = state.Index.BaseRevision.IsNone

        let! changed =
            match state.Index.BaseRevision with
            | None -> async { return Error(LakeFsSynchronization.missingBaseRevisionFailure ()) }
            | Some _ -> targetChangedPathsAgainst state head context

        match changed with
        | Error failure when noSynchronizedBase -> return Failed failure
        | Error failure ->
            return
                Failed(
                    LakeFsSynchronization.previewIndeterminate
                        "target changed paths"
                        failure
                )
        | Ok changedPaths ->
            let dirtyPaths = classifyWorkspace state |> List.map _.ChangePath |> Set.ofList

            // Locally committed changes since base: workspace branch vs base diff.
            let! connection = connect state

            match connection with
            | Error failure ->
                return
                    Failed(
                        LakeFsSynchronization.previewIndeterminate
                            "locally committed objects"
                            failure
                    )
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
                                    Ok(
                                        entries
                                        |> Array.toList
                                        |> List.choose (fun entry -> repositoryPathOfKey state entry.Path)
                                        |> List.map RepositoryPath.value
                                        |> Set.ofList
                                    )
                            | Error failure ->
                                return Error(LakeFsSynchronization.previewIndeterminate "locally committed objects" failure)
                        }
                    | _ -> async { return Ok Set.empty }

                match localCommitted with
                | Error failure -> return Failed failure
                | Ok localCommittedPaths ->
                    return
                        LakeFsSynchronization.createPreview changedPaths dirtyPaths localCommittedPaths
                        |> OperationResult.succeeded
    }

let private previewUpdate (state: SessionState) (context: OperationContext) =
    async {
        let! targetHead = getTargetHead state context

        match targetHead with
        | Error failure -> return Failed(LakeFsSynchronization.previewIndeterminate "target head" failure)
        | Ok head -> return! previewAgainst state head context
    }

/// Builds the conflict item contents for overlapping paths from base, workspace
/// branch, and target ref object content.
let private buildConflictItems
    (state: SessionState)
    (resolved: LakeFsConnection)
    (overlapping: string list)
    (targetHead: string)
    (context: OperationContext)
    : Async<LakeFsConflictSession.ItemState list> =
    async {
        let items = ResizeArray<LakeFsConflictSession.ItemState>()

        let candidateFromBuffer sourcePath buffer : LakeFsConflictSession.CandidateContent =
            let preview =
                if float (NodeInterop.bufferLength buffer) > float (1024 * 1024) then
                    UnsupportedPreview(Some "lakeFS object exceeds the text conflict preview limit.")
                else
                    if NodeInterop.bufferContainsNul buffer || not (NodeInterop.bufferIsValidUtf8 buffer) then
                        UnsupportedPreview(Some "Binary lakeFS object; select an original candidate or resolve it manually.")
                    else
                        TextPreview(NodeInterop.bufferToUtf8String buffer)

            {
                SourcePath = sourcePath
                Preview = preview
            }

        let candidateFromFile sourcePath =
            let buffer, _ = NodeFileSystem.readBufferNoFollowSync sourcePath
            candidateFromBuffer sourcePath buffer

        for path in overlapping do
            let key = objectKey state path

            let readRef reference =
                async {
                    let downloadedPath = temporaryPath state "conflict-candidate"

                    let! content =
                        LakeFsApi.downloadObjectToFile
                            resolved
                            state.Index.Repository
                            reference
                            key
                            downloadedPath
                            context

                    match content with
                    | Ok _ -> return Some(candidateFromFile downloadedPath)
                    | Error _ -> return None
                }

            let! baseContent =
                match state.Index.BaseRevision with
                | Some baseRevision -> readRef baseRevision
                | None -> async { return None }

            let! targetContent = readRef targetHead

            let workspaceContent =
                match inspectLocalFile state path with
                | None -> None
                | Some inspected ->
                    LakeFsPathSafety.readBuffer state.Binding.WorkspaceRoot (repositoryPath path)
                    |> LakeFsPathSafety.orRaise
                    |> Option.map (candidateFromBuffer inspected.AbsolutePath)

            items.Add {
                ItemPath = path
                BaseContent = baseContent
                WorkspaceContent = workspaceContent
                TargetContent = targetContent
                ResolvedContent = None
            }

        return List.ofSeq items
    }

let private updateAgainstCore (state: SessionState) (head: string) (context: OperationContext) =
    async {
        if Some head = state.Index.BaseRevision then
            return
                OperationResult.noOp
                    (Some "The workspace is already up to date.")
                    (synchronizationState state (Some head))
        else
            let! previewResult = previewAgainst state head context

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
                        let workspaceRevision =
                            state.Index.WorkspaceRevision |> Option.defaultValue head

                        state.Conflict <-
                            Some(LakeFsConflictSession.create head workspaceRevision items)

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
                        let expectedWorkspace = state.Index.WorkspaceRevision
                        let canFastForwardToTarget =
                            expectedWorkspace = state.Index.BaseRevision
                            && (classifyWorkspace state |> List.isEmpty)

                        do! barrier state "update-precheck-done" context

                        let! workspaceBeforeMerge =
                            LakeFsApi.getBranch
                                resolved
                                state.Index.Repository
                                state.Index.WorkspaceBranch
                                context

                        match workspaceBeforeMerge with
                        | Error failure -> return Failed failure
                        | Ok observedWorkspace ->
                            let! merged =
                                LakeFsApi.merge
                                    resolved
                                    state.Index.Repository
                                    head
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

                                let! mergeCommit =
                                    LakeFsApi.getCommit
                                        resolved
                                        state.Index.Repository
                                        mergeResult.Reference
                                        context

                                let verified =
                                    match branchAfter, mergeCommit with
                                    | Ok resultingBranch, Ok resultingCommit ->
                                        let sourceVerified =
                                            mergeResult.Reference = head
                                            || resultingCommit.Parents |> Array.contains head

                                        let destinationVerified =
                                            mergeResult.Reference = observedWorkspace.CommitId
                                            || resultingCommit.Parents
                                               |> Array.contains observedWorkspace.CommitId

                                        expectedWorkspace = Some observedWorkspace.CommitId
                                        && resultingBranch.CommitId = mergeResult.Reference
                                        && sourceVerified
                                        && destinationVerified
                                    | _ -> false

                                if not verified then
                                    let observedResult =
                                        match branchAfter with
                                        | Ok branch -> Some branch.CommitId
                                        | Error _ -> None

                                    let observedState = {
                                        state with
                                            Index = {
                                                state.Index with
                                                    WorkspaceRevision =
                                                        observedResult
                                                        |> Option.orElse (Some mergeResult.Reference)
                                            }
                                    }

                                    let outcome = {
                                        OperationOutcome.performed (
                                            synchronizationState observedState (Some head)
                                        ) with
                                            ResultingRevision =
                                                Some(mkRevisionId mergeResult.Reference)
                                            Publication = LocalOnly
                                    }

                                    return
                                        OperationResult.partiallySucceeded
                                            outcome
                                            {
                                                OperationFailure.create
                                                    Concurrency
                                                    "precondition_failed"
                                                    "The workspace branch advanced during update verification." with
                                                    RevisionEvidence = [|
                                                        yield!
                                                            expectedWorkspace
                                                            |> Option.map (fun revision ->
                                                                "expected_workspace",
                                                                mkRevisionId revision)
                                                            |> Option.toList
                                                        "observed_workspace",
                                                        mkRevisionId observedWorkspace.CommitId
                                                        "expected_target", mkRevisionId head
                                                        yield!
                                                            observedResult
                                                            |> Option.map (fun revision ->
                                                                "observed_result",
                                                                mkRevisionId revision)
                                                            |> Option.toList
                                                    |]
                                            }
                                            {
                                                Code = "review_and_retry"
                                                Instructions =
                                                    Some
                                                        "Review the observed workspace-branch merge, refresh, and retry deliberately."
                                            }
                                else
                                    let! preparedWorkspace =
                                        if canFastForwardToTarget then
                                            resetOwnedWorkspaceBranch
                                                state
                                                resolved
                                                (Some mergeResult.Reference)
                                                head
                                                context
                                        else
                                            async.Return(Ok())

                                    match preparedWorkspace with
                                    | Error failure ->
                                        return Failed { failure with StateChanged = true }
                                    | Ok() ->
                                        let materializationRef =
                                            if canFastForwardToTarget then
                                                head
                                            else
                                                mergeResult.Reference

                                        let resultingWorkspace =
                                            if canFastForwardToTarget then
                                                head
                                            else
                                                mergeResult.Reference

                                        let finalizeIndex (index: LakeFsIndex.WorkspaceIndex) = {
                                            index with
                                                BaseRevision = Some head
                                                WorkspaceRevision = Some resultingWorkspace
                                        }

                                        // updateAgainst refuses an index without a base, so the preview's
                                        // target diff is always the exact write set here.
                                        let targetChangedPaths =
                                            Some(
                                                previewOutcome.Value.ChangedPaths
                                                |> Array.map RepositoryPath.value
                                                |> Set.ofArray
                                            )

                                        let! materialized =
                                            materializeRef
                                                state
                                                resolved
                                                materializationRef
                                                false
                                                targetChangedPaths
                                                finalizeIndex
                                                context

                                        match materialized with
                                        | Failed failure ->
                                            let outcome =
                                                OperationOutcome.performed (
                                                    synchronizationState
                                                        { state with Index = finalizeIndex state.Index }
                                                        (Some head)
                                                )

                                            return
                                                PartiallySucceeded(
                                                    outcome,
                                                    materializationRecoveryFailure
                                                        state.RecoveryDirectory
                                                        expectedWorkspace
                                                        materializationRef
                                                        [||]
                                                        failure
                                                )
                                        | PartiallySucceeded(outcome, failure) ->
                                            return
                                                PartiallySucceeded(
                                                    mapOutcomeValue
                                                        (synchronizationState
                                                            { state with Index = finalizeIndex state.Index }
                                                            (Some head))
                                                        outcome,
                                                    failure
                                                )
                                        | Succeeded _ ->
                                            return
                                                synchronizationState state (Some head)
                                                |> OperationResult.succeeded
    }

let private updateAgainst (state: SessionState) (head: string) (context: OperationContext) =
    async {
        match state.Index.BaseRevision with
        | None -> return Failed(LakeFsSynchronization.missingBaseRevisionFailure ())
        | Some _ -> return! updateAgainstCore state head context
    }

let private update (state: SessionState) (_request: UpdateRequest) (context: OperationContext) =
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
                // Read the target once, then pass its commit to the update body.
                let! targetHead = getTargetHead state context

                match targetHead with
                | Error failure -> return Failed failure
                | Ok head -> return! updateAgainst state head context
    }

let private targetRevisionMissing () =
    OperationFailure.create
        ProviderError
        "target_revision_missing"
        "The refreshed lakeFS state did not contain a target revision."

let private publish (state: SessionState) (expectedTarget: RevisionId option) (context: OperationContext) =
    async {
        do! barrier state "publish-connect" context
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
                    match expectedTarget with
                    | None -> true
                    | Some expected -> RevisionId.value expected = head

                if not expectedMatches then
                    return
                        Failed {
                            staleFailure () with
                                RevisionEvidence = [|
                                    yield!
                                        expectedTarget
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
                    let expectedWorkspace = state.Index.WorkspaceRevision
                    do! barrier state "publish-precheck-done" context
                    do! barrier state "transfer-start" context

                    if context.Cancellation.IsCancellationRequested() then
                        return OperationResult.canceled "The publish was canceled."
                    else
                        let! targetBeforeMerge =
                            LakeFsApi.getBranch
                                resolved
                                state.Index.Repository
                                state.Index.TargetRef
                                context

                        match targetBeforeMerge with
                        | Error failure -> return Failed { failure with Retryable = true }
                        | Ok observedTarget ->
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
                                // Verify the destination and both sides of the merge before
                                // advancing the persisted workspace state.
                                let! branchAfter =
                                    LakeFsApi.getBranch
                                        resolved
                                        state.Index.Repository
                                        state.Index.TargetRef
                                        context

                                let! mergeCommit =
                                    LakeFsApi.getCommit
                                        resolved
                                        state.Index.Repository
                                        mergeResult.Reference
                                        context

                                let verified =
                                    match branchAfter, mergeCommit, expectedWorkspace with
                                    | Ok branch, Ok commit, Some workspaceRevision ->
                                        let sourceVerified =
                                            mergeResult.Reference = workspaceRevision
                                            || commit.Parents |> Array.contains workspaceRevision

                                        let destinationVerified =
                                            mergeResult.Reference = observedTarget.CommitId
                                            || commit.Parents |> Array.contains observedTarget.CommitId

                                        head = observedTarget.CommitId
                                        && branch.CommitId = mergeResult.Reference
                                        && sourceVerified
                                        && destinationVerified
                                    | _ -> false

                                if not verified then
                                    let resultingTarget =
                                        match branchAfter with
                                        | Ok branch -> Some branch.CommitId
                                        | Error _ -> None

                                    let observedState = {
                                        state with
                                            Index = {
                                                state.Index with
                                                    BaseRevision = Some mergeResult.Reference
                                            }
                                    }

                                    let outcome = {
                                        OperationOutcome.performed (
                                            synchronizationState
                                                observedState
                                                (resultingTarget |> Option.orElse (Some mergeResult.Reference))
                                        ) with
                                            Publication = Published
                                            ResultingRevision = Some(mkRevisionId mergeResult.Reference)
                                    }

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
                                                        "observed_target", mkRevisionId observedTarget.CommitId
                                                        yield!
                                                            expectedWorkspace
                                                            |> Option.map (fun revision ->
                                                                "expected_workspace", mkRevisionId revision)
                                                            |> Option.toList
                                                        yield!
                                                            resultingTarget
                                                            |> Option.map (fun revision ->
                                                                "observed_result", mkRevisionId revision)
                                                            |> Option.toList
                                                    |]
                                            }
                                            {
                                                Code = "review_and_retry"
                                                Instructions =
                                                    Some
                                                        "The owned workspace branch is preserved. Review the observed target revision, refresh, and retry deliberately."
                                            }
                                else
                                    let! preparedWorkspace =
                                        if expectedWorkspace = Some mergeResult.Reference then
                                            async.Return(Ok())
                                        else
                                            resetOwnedWorkspaceBranch
                                                state
                                                resolved
                                                expectedWorkspace
                                                mergeResult.Reference
                                                context

                                    match preparedWorkspace with
                                    | Error failure ->
                                        return Failed { failure with StateChanged = true }
                                    | Ok() ->
                                        state.Index <- {
                                            state.Index with
                                                BaseRevision = Some mergeResult.Reference
                                                WorkspaceRevision = Some mergeResult.Reference
                                        }

                                        match saveIndex state with
                                        | Error failure -> return Failed { failure with StateChanged = true }
                                        | Ok() ->
                                            return
                                                Succeeded {
                                                    OperationOutcome.performed (
                                                        synchronizationState state (Some mergeResult.Reference)
                                                    ) with
                                                        Publication = Published
                                                        ResultingRevision = Some(mkRevisionId mergeResult.Reference)
                                                        ResultingWorkspaceVersion = Some(workspaceVersion state)
                                                }
    }

// ---------------------------------------------------------------------------
// Conflict sessions
// ---------------------------------------------------------------------------

let private handleRejection () =
    LakeFsConflictSession.rejection ()

let private validateHandle (state: SessionState) (handle: ConflictSessionHandle) (expectedVersion: string) =
    LakeFsConflictSession.validate state.Conflict handle (workspaceVersion state) expectedVersion

let private completeConflictFinalize
    (state: SessionState)
    (resolved: LakeFsConnection)
    (targetRevision: string)
    (resultingRevision: string)
    (context: OperationContext)
    =
    async {
        let targetHead =
            state.Conflict
            |> Option.map _.TargetRevisionAtOpen
            |> Option.defaultValue targetRevision

        let! targetChangedResult = targetChangedPathsAgainst state targetHead context
        let mutable selectedPaths = Set.empty
        let mutable targetChangedFailure = None

        match targetChangedResult with
        | Error failure -> targetChangedFailure <- Some failure
        | Ok paths -> selectedPaths <- Set.ofList paths

        match state.Conflict with
        | Some conflict ->
            selectedPaths <-
                Set.union
                    selectedPaths
                    (conflict.Items |> List.map _.ItemPath |> Set.ofList)
        | None -> ()

        let finalizeIndex (index: LakeFsIndex.WorkspaceIndex) = {
            index with
                BaseRevision = Some targetRevision
                WorkspaceRevision = Some resultingRevision
        }

        let! materialized =
            match targetChangedFailure with
            | Some failure -> async { return Failed failure }
            | None ->
                materializeRef
                    state
                    resolved
                    state.Index.WorkspaceBranch
                    false
                    (Some selectedPaths)
                    finalizeIndex
                    context

        match materialized with
        | Failed failure ->
            return
                PartiallySucceeded(
                    OperationOutcome.performed (Some(mkRevisionId resultingRevision)),
                    materializationRecoveryFailure
                        state.RecoveryDirectory
                        state.Index.WorkspaceRevision
                        resultingRevision
                        [||]
                        failure
                )
        | PartiallySucceeded(outcome, failure) ->
            return
                PartiallySucceeded(
                    mapOutcomeValue (Some(mkRevisionId resultingRevision)) outcome,
                    failure
                )
        | Succeeded outcome ->
            cleanupConflictCandidates state
            state.Conflict <- None
            return Succeeded(mapOutcomeValue (Some(mkRevisionId resultingRevision)) outcome)
    }

let private partialConflictFinalize
    (state: SessionState)
    (conflict: LakeFsConflictSession.State)
    (expectedDestination: string)
    (observedDestination: string)
    (resultingRevision: string)
    (observedTarget: string option)
    (canConfirmOnRetry: bool)
    =
    if canConfirmOnRetry then
        conflict.PendingFinalizeRevision <- Some resultingRevision
        conflict.HandleVersion <- conflict.HandleVersion + 1

    let revision = mkRevisionId resultingRevision
    let outcome = {
        OperationOutcome.performed (Some revision) with
            ResultingRevision = Some revision
            Publication = LocalOnly
    }

    OperationResult.partiallySucceeded
        outcome
        {
            OperationFailure.create
                Concurrency
                "precondition_failed"
                "The conflict-finalize destination advanced during verification." with
                RevisionEvidence = [|
                    "expected_destination", mkRevisionId expectedDestination
                    "observed_destination", mkRevisionId observedDestination
                    "observed_result", revision
                    "expected_target", mkRevisionId conflict.TargetRevisionAtOpen
                    yield!
                        observedTarget
                        |> Option.map (fun target -> "observed_target", mkRevisionId target)
                        |> Option.toList
                |]
        }
        {
            Code =
                if canConfirmOnRetry then
                    ConflictRecovery.RefreshConflictSession
                else
                    "review_and_retry"
            Instructions =
                Some(
                    if canConfirmOnRetry then
                        "Refresh the rotated live session and deliberately confirm the verified resolution revision."
                    else
                        "Review the observed workspace branch and retry deliberately."
                )
        }

let private createConflictService (state: SessionState) : ConflictResolutionService = {
    GetActiveSession = fun _ -> async { return OperationResult.succeeded (conflictSummary state) }
    Resolve =
        fun request context -> guardConflictMutation state (async {
            match validateHandle state request.Handle request.ExpectedWorkspaceVersion with
            | Error failure -> return Failed failure
            | Ok conflict ->
                match LakeFsConflictSession.resolve conflict request.Path request.Resolution with
                | Error failure -> return Failed failure
                | Ok() ->
                    return
                        OperationResult.succeeded {
                            RefreshedHandle = {
                                SessionId = conflict.SessionId
                                Version = string conflict.HandleVersion
                            }
                            RemainingItems =
                                conflictSummary state |> Option.map _.Items |> Option.defaultValue [||]
                        }
        })
    Finalize =
        fun request context -> guardConflictMutation state (async {
            match validateHandle state request.Handle request.ExpectedWorkspaceVersion with
            | Error failure -> return Failed failure
            | Ok conflict ->
                if LakeFsConflictSession.hasUnresolvedItems conflict then
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
                        let! targetHead = getTargetHead state context

                        match targetHead with
                        | Error failure -> return Failed failure
                        | Ok observedTarget when observedTarget <> conflict.TargetRevisionAtOpen ->
                            return
                                Failed {
                                    handleRejection () with
                                        RevisionEvidence = [|
                                            "expected_target", mkRevisionId conflict.TargetRevisionAtOpen
                                            "observed_target", mkRevisionId observedTarget
                                        |]
                                }
                        | Ok observedTarget ->
                            match conflict.PendingFinalizeRevision with
                            | Some pendingRevision ->
                                let! pendingBranch =
                                    LakeFsApi.getBranch
                                        resolved
                                        state.Index.Repository
                                        state.Index.WorkspaceBranch
                                        context

                                match pendingBranch with
                                | Error failure -> return Failed failure
                                | Ok branch when branch.CommitId <> pendingRevision ->
                                    return
                                        Failed {
                                            handleRejection () with
                                                RevisionEvidence = [|
                                                    "expected_destination", mkRevisionId pendingRevision
                                                    "observed_destination", mkRevisionId branch.CommitId
                                                |]
                                        }
                                | Ok _ ->
                                    return!
                                        completeConflictFinalize
                                            state
                                            resolved
                                            observedTarget
                                            pendingRevision
                                            context
                            | None ->
                                let expectedDestination = conflict.WorkspaceRevisionAtOpen
                                do! barrier state "finalize-precheck-done" context

                                let! destinationBeforeMerge =
                                    LakeFsApi.getBranch
                                        resolved
                                        state.Index.Repository
                                        state.Index.WorkspaceBranch
                                        context

                                match destinationBeforeMerge with
                                | Error failure -> return Failed failure
                                | Ok observedDestination ->
                                    // Use destination-wins only to establish the merge base;
                                    // each selected resolution is applied explicitly below.
                                    let! merged =
                                        LakeFsApi.mergeWithStrategy
                                            resolved
                                            state.Index.Repository
                                            state.Index.TargetRef
                                            state.Index.WorkspaceBranch
                                            "merge: finalize conflict session"
                                            (Some "dest-wins")
                                            context

                                    match merged with
                                    | Error failure -> return Failed failure
                                    | Ok mergeResult ->
                                        let! mergeBranch =
                                            LakeFsApi.getBranch
                                                resolved
                                                state.Index.Repository
                                                state.Index.WorkspaceBranch
                                                context

                                        let! mergeCommit =
                                            LakeFsApi.getCommit
                                                resolved
                                                state.Index.Repository
                                                mergeResult.Reference
                                                context

                                        let mergeVerified =
                                            match mergeBranch, mergeCommit with
                                            | Ok branch, Ok commit ->
                                                let sourceVerified =
                                                    mergeResult.Reference = observedTarget
                                                    || commit.Parents |> Array.contains observedTarget

                                                let destinationVerified =
                                                    mergeResult.Reference = observedDestination.CommitId
                                                    || commit.Parents
                                                       |> Array.contains observedDestination.CommitId

                                                branch.CommitId = mergeResult.Reference
                                                && sourceVerified
                                                && destinationVerified
                                            | _ -> false

                                        if not mergeVerified then
                                            return
                                                partialConflictFinalize
                                                    state
                                                    conflict
                                                    expectedDestination
                                                    observedDestination.CommitId
                                                    mergeResult.Reference
                                                    (Some observedTarget)
                                                    false
                                        else
                                            let mutable resolutionFailure: OperationFailure option = None

                                            for item in conflict.Items do
                                                if resolutionFailure.IsNone then
                                                    match item.ResolvedContent with
                                                    | Some(Some content) ->
                                                        let sourceAndValidation =
                                                            match content with
                                                            | LakeFsConflictSession.WorkspaceFile path ->
                                                                match
                                                                    LakeFsPathSafety.resolveWorkspacePath
                                                                        state.Binding.WorkspaceRoot
                                                                        path
                                                                with
                                                                | Error failure -> Error failure
                                                                | Ok _ ->
                                                                    match
                                                                        LakeFsPathSafety.inspectFile
                                                                            state.Binding.WorkspaceRoot
                                                                            path
                                                                    with
                                                                    | Error failure -> Error failure
                                                                    | Ok None ->
                                                                        Error {
                                                                            OperationFailure.create
                                                                                NotFound
                                                                                "workspace_file_missing"
                                                                                "The selected workspace conflict candidate is no longer present." with
                                                                                AffectedPaths = [| RepositoryPath.value path |]
                                                                        }
                                                                    | Ok(Some inspected) ->
                                                                        Ok(
                                                                            inspected.AbsolutePath,
                                                                            LakeFsPathSafety.validateOpenedFile
                                                                                state.Binding.WorkspaceRoot
                                                                                path
                                                                                inspected.Identity
                                                                        )
                                                            | LakeFsConflictSession.ExistingFile path ->
                                                                Ok(path, fun stats ->
                                                                    if stats.isSymbolicLink() || not (stats.isFile()) then
                                                                        Error(
                                                                            OperationFailure.create
                                                                                Validation
                                                                                "symlink_not_supported"
                                                                                "The conflict candidate is not a regular file."
                                                                        )
                                                                    else
                                                                        Ok())
                                                            | LakeFsConflictSession.SuppliedText text ->
                                                                let path = temporaryPath state "conflict-resolution"
                                                                NodeFileSystem.writeUtf8FileExclusiveAndFlushSync path text
                                                                Ok(path, fun stats ->
                                                                    if stats.isSymbolicLink() || not (stats.isFile()) then
                                                                        Error(
                                                                            OperationFailure.create
                                                                                Validation
                                                                                "symlink_not_supported"
                                                                                "The conflict candidate is not a regular file."
                                                                        )
                                                                    else
                                                                        Ok())

                                                        match sourceAndValidation with
                                                        | Error failure -> resolutionFailure <- Some failure
                                                        | Ok(sourcePath, validateSource) ->
                                                            let! upload =
                                                                LakeFsApi.uploadObjectFromFileChecked
                                                                    resolved
                                                                    state.Index.Repository
                                                                    state.Index.WorkspaceBranch
                                                                    (objectKey state item.ItemPath)
                                                                    sourcePath
                                                                    validateSource
                                                                    context

                                                            match upload with
                                                            | Error failure -> resolutionFailure <- Some failure
                                                            | Ok _ -> ()
                                                    | Some None ->
                                                        let! deletion =
                                                            LakeFsApi.deleteObject
                                                                resolved
                                                                state.Index.Repository
                                                                state.Index.WorkspaceBranch
                                                                (objectKey state item.ItemPath)
                                                                context

                                                        match deletion with
                                                        | Error failure when failure.Category = NotFound -> ()
                                                        | Error failure -> resolutionFailure <- Some failure
                                                        | Ok() -> ()
                                                    | None -> ()

                                            match resolutionFailure with
                                            | Some failure ->
                                                return Failed { failure with StateChanged = true }
                                            | None ->
                                                let! committed =
                                                    LakeFsApi.commit
                                                        resolved
                                                        state.Index.Repository
                                                        state.Index.WorkspaceBranch
                                                        (request.Message
                                                         |> Option.defaultValue
                                                             "merge: finalize conflict session")
                                                        context

                                                match committed with
                                                | Error failure ->
                                                    return Failed { failure with StateChanged = true }
                                                | Ok commit ->
                                                    let! branchAfter =
                                                        LakeFsApi.getBranch
                                                            resolved
                                                            state.Index.Repository
                                                            state.Index.WorkspaceBranch
                                                            context

                                                    let! commitAfter =
                                                        LakeFsApi.getCommit
                                                            resolved
                                                            state.Index.Repository
                                                            commit.Id
                                                            context

                                                    let! targetAfter = getTargetHead state context

                                                    let resolutionVerified =
                                                        match branchAfter, commitAfter with
                                                        | Ok branch, Ok resultingCommit ->
                                                            branch.CommitId = commit.Id
                                                            && (commit.Id = mergeResult.Reference
                                                                || resultingCommit.Parents
                                                                   |> Array.contains mergeResult.Reference)
                                                        | _ -> false

                                                    let targetVerified =
                                                        match targetAfter with
                                                        | Ok target -> target = observedTarget
                                                        | Error _ -> false

                                                    let canConfirmOnRetry =
                                                        resolutionVerified && targetVerified

                                                    if
                                                        expectedDestination = observedDestination.CommitId
                                                        && canConfirmOnRetry
                                                    then
                                                        return!
                                                            completeConflictFinalize
                                                                state
                                                                resolved
                                                                observedTarget
                                                                commit.Id
                                                                context
                                                    else
                                                        let observedTargetAfter =
                                                            match targetAfter with
                                                            | Ok target -> Some target
                                                            | Error _ -> None

                                                        return
                                                            partialConflictFinalize
                                                                state
                                                                conflict
                                                                expectedDestination
                                                                observedDestination.CommitId
                                                                commit.Id
                                                                observedTargetAfter
                                                                canConfirmOnRetry
        })
    Cancel =
        fun request context -> guardConflictMutation state (async {
            match validateHandle state request.Handle request.ExpectedWorkspaceVersion with
            | Error failure -> return Failed failure
            | Ok _ ->
                // The update never touched the workspace branch or local files for
                // conflicting paths, so canceling only closes the session.
                cleanupConflictCandidates state
                state.Conflict <- None
                return OperationResult.succeeded ()
        })
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
        GetStatus = fun context -> getStatus state context |> LakeFsPathSafety.guard
        ListRefs = fun context -> listRefs state context |> LakeFsPathSafety.guard
        CreateRef =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion false (fun () ->
                    createRef state request context)
                |> LakeFsPathSafety.guard
        PreflightSwitchRef =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion false (fun () ->
                    preflightSwitchRef state request context)
                |> LakeFsPathSafety.guard
        SwitchRef =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion false (fun () ->
                    switchRef state request context)
                |> LakeFsPathSafety.guard
        CreateRevision =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion false (fun () ->
                    createRevision state request context)
                |> LakeFsPathSafety.guard
        RestorePaths =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion true (fun () ->
                    restorePaths state request context)
                |> LakeFsPathSafety.guard
        GetDiffSummary = fun context -> getDiffSummary state context |> LakeFsPathSafety.guard
    }

    let conflictResolution = createConflictService state

    let guardedConflictResolution = {
        GetActiveSession =
            fun context -> conflictResolution.GetActiveSession context |> LakeFsPathSafety.guard
        Resolve =
            fun request context -> conflictResolution.Resolve request context |> LakeFsPathSafety.guard
        Finalize =
            fun request context -> conflictResolution.Finalize request context |> LakeFsPathSafety.guard
        Cancel =
            fun request context -> conflictResolution.Cancel request context |> LakeFsPathSafety.guard
    }

    {
        WorkspaceSession.createCoreOnly descriptor core with
            Synchronization =
                Some {
                    Refresh = fun context -> refresh state context |> LakeFsPathSafety.guard
                    PreviewUpdate = fun context -> previewUpdate state context |> LakeFsPathSafety.guard
                    Update =
                        fun request context ->
                            withValidatedMutation state request.ExpectedWorkspaceVersion false (fun () ->
                                update state request context)
                            |> LakeFsPathSafety.guard
                    Publish =
                        fun request context ->
                            withValidatedMutation state request.ExpectedWorkspaceVersion false (fun () ->
                                publish state request.ExpectedTargetRevision context)
                            |> LakeFsPathSafety.guard
                    Synchronize =
                        fun request context ->
                            withValidatedMutation state request.ExpectedWorkspaceVersion false (fun () ->
                                Synchronization.compose
                                    {
                                        HasActiveConflictSession =
                                            fun _ -> async { return OperationResult.succeeded state.Conflict.IsSome }
                                        Refresh = fun context -> refresh state context
                                        PreviewUpdate =
                                            fun syncState context ->
                                                async {
                                                    match syncState.TargetRevision with
                                                    | Some head -> return! previewAgainst state (RevisionId.value head) context
                                                    | None -> return Failed(targetRevisionMissing ())
                                                }
                                        Update =
                                            fun syncState context ->
                                                async {
                                                    match syncState.TargetRevision with
                                                    | Some head -> return! updateAgainst state (RevisionId.value head) context
                                                    | None -> return Failed(targetRevisionMissing ())
                                                }
                                        Publish =
                                            fun syncState context ->
                                                publish state syncState.TargetRevision context
                                    }
                                    request
                                    context)
                            |> LakeFsPathSafety.guard
                }
            ConflictResolution = Some guardedConflictResolution
    }

/// Opens a session: loads the index or creates the provider-owned workspace
/// branch (server-visible; filtered only from logical listings) and materializes
/// the target into the workspace directory.
let private openSessionFromStateDirectory
    (providerState: LakeFsStateStore.ResolvedState)
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
                    | Error failure -> return Failed failure
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
                        | Error failure -> return Failed failure
                        | Ok() ->
                            if not (NodeFileSystem.existsSync binding.WorkspaceRoot) then
                                NodeFileSystem.mkdirSync
                                    binding.WorkspaceRoot
                                    (NodeFileSystem.MkdirOptions(recursive = true))

                            let state = {
                                Binding = binding
                                StateDirectory = providerState.StateDirectory
                                RecoveryDirectory = providerState.RecoveryDirectory
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

                            let preserveExistingFiles =
                                providerState.ProvisioningMode <> LakeFsStateStore.CloneProvisioning

                            let! materialized =
                                materializeRef
                                    state
                                    resolved
                                    workspaceBranch
                                    preserveExistingFiles
                                    None
                                    id
                                    context

                            match materialized with
                            | Failed failure ->
                                let! deleted =
                                    LakeFsApi.deleteBranch
                                        resolved
                                        state.Index.Repository
                                        workspaceBranch
                                        (OperationContext.detached $"{context.OperationId}-cleanup")

                                match deleted with
                                | Ok() -> return Failed failure
                                | Error cleanupFailure ->
                                    return
                                        Failed {
                                            cleanupFailure with
                                                StateChanged = true
                                                Retryable = true
                                                Details =
                                                    Array.append
                                                        cleanupFailure.Details
                                                        [| failure.Code; failure.Message |]
                                                RevisionEvidence = [|
                                                    "observed_materialization",
                                                    mkRevisionId target.CommitId
                                                |]
                                                RecoveryAction =
                                                    Some {
                                                        Code = "review_workspace_branch"
                                                        Instructions =
                                                            Some
                                                                "Review and remove the provider-owned workspace branch before retrying."
                                                    }
                                        }
                            | PartiallySucceeded(outcome, failure) ->
                                let indexPreparation =
                                    match LakeFsIndex.load state.StateDirectory with
                                    | LakeFsIndex.Loaded saved ->
                                        state.Index <- saved
                                        Ok()
                                    | LakeFsIndex.Missing ->
                                        match LakeFsIndex.save state.StateDirectory state.Index with
                                        | Ok saved ->
                                            state.Index <- saved
                                            Ok()
                                        | Error message -> Error message
                                    | LakeFsIndex.Corrupt message -> Error message

                                let readiness =
                                    match indexPreparation with
                                    | Ok() -> LakeFsStateStore.markReady providerState
                                    | Error message ->
                                        Error(
                                            OperationFailure.createRedacted
                                                ProviderError
                                                "index_write_failed"
                                                message
                                        )
                                let extraDetails = [|
                                    match indexPreparation with
                                    | Error message -> yield Redaction.redact message
                                    | Ok() -> ()

                                    match readiness with
                                    | Error readyFailure -> yield readyFailure.Code
                                    | Ok _ -> ()
                                |]

                                return
                                    PartiallySucceeded(
                                        mapOutcomeValue state outcome,
                                        {
                                            failure with
                                                Details = Array.append failure.Details extraDetails
                                        }
                                    )
                            | Succeeded outcome ->
                                match LakeFsStateStore.markReady providerState with
                                | Error failure ->
                                    return
                                        PartiallySucceeded(
                                            mapOutcomeValue state outcome,
                                            materializationRecoveryFailure
                                                state.RecoveryDirectory
                                                state.Index.WorkspaceRevision
                                                target.CommitId
                                                outcome.AffectedPaths
                                                failure
                                        )
                                | Ok _ -> return OperationResult.succeeded state
                }

                match LakeFsIndex.load providerState.StateDirectory with
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
                            StateDirectory = providerState.StateDirectory
                            RecoveryDirectory = providerState.RecoveryDirectory
                            Location = location
                            Credentials = credentials
                            Hooks = hooks
                            Index = index
                            Conflict = None
                            ConflictGeneration = 0
                            Busy = false
                        }

                        let openLoadedState () = async {
                            if providerState.IsReady then
                                return OperationResult.succeeded (createSessionFromState state)
                            else
                                match LakeFsStateStore.markReady providerState with
                                | Error failure -> return Failed failure
                                | Ok _ -> return OperationResult.succeeded (createSessionFromState state)
                        }

                        match
                            LakeFsMaterialization.loadPendingRecoveries state.RecoveryDirectory
                        with
                        | Error failure -> return Failed failure
                        | Ok _ ->
                            LakeFsStateStore.sweepTransientEntries providerState
                            return! openLoadedState ()
                    else
                        return
                            Failed(
                                OperationFailure.create
                                    Concurrency
                                    "provider_state_mismatch"
                                    "The external lakeFS state does not match the workspace binding location."
                            )
                | LakeFsIndex.Missing ->
                    if providerState.IsReady then
                        return
                            Failed(
                                OperationFailure.create
                                    ProviderError
                                    "provider_state_index_missing"
                                    "The ready external lakeFS provider state is missing its workspace index."
                            )
                    else
                        let! created = createOwnedState [||]

                        match created with
                        | Failed failure -> return Failed failure
                        | PartiallySucceeded(outcome, failure) ->
                            return
                                PartiallySucceeded(
                                    mapOutcomeValue (createSessionFromState outcome.Value) outcome,
                                    failure
                                )
                        | Succeeded outcome ->
                            return Succeeded(mapOutcomeValue (createSessionFromState outcome.Value) outcome)
    }

let openSession
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (hooks: LakeFsSessionHooks)
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    (binding: WorkspaceBinding)
    (context: OperationContext)
    : Async<OperationResult<WorkspaceSession>> =
    async {
        match LakeFsStateStore.resolve options binding.WorkspaceRoot binding.ProviderStateRef with
        | Error failure -> return Failed failure
        | Ok state ->
            return!
                openSessionFromStateDirectory
                    state
                    hooks
                    credentials
                    binding
                    context
                |> LakeFsPathSafety.guard
    }

let private cleanupPreconditionFailure message expectedHead observedHead =
    {
        OperationFailure.create Concurrency "precondition_failed" message with
            RevisionEvidence = [|
                yield!
                    expectedHead
                    |> Option.map (fun revision -> "expected_head", mkRevisionId revision)
                    |> Option.toList
                yield!
                    observedHead
                    |> Option.map (fun revision -> "observed_head", mkRevisionId revision)
                    |> Option.toList
            |]
            RecoveryAction =
                Some {
                    Code = "review_workspace_branch"
                    Instructions =
                        Some "Refresh the owned workspace branch, verify its ownership and head, then retry deliberately."
                }
    }

/// Deletes an expired binding's provider-owned workspace branch. Session Close is
/// intentionally non-destructive; hosts call this lifecycle operation only when a
/// binding is being discarded. lakeFS has no conditional branch delete, so the
/// implementation checks the head immediately before deletion and verifies absence
/// afterwards, reporting any observed race instead of claiming clean success.
let private cleanupOwnedWorkspaceBranchFromStateDirectory
    (stateDirectory: string)
    (hooks: LakeFsSessionHooks)
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    (binding: WorkspaceBinding)
    (ownershipToken: string)
    (expectedHead: string option)
    (context: OperationContext)
    : Async<OperationResult<unit>> =
    async {
        match LakeFsLocation.tryParse binding.Location.ProviderLocation with
        | Error message ->
            return Failed(OperationFailure.create Validation "invalid_location" message)
        | Ok location ->
            match LakeFsIndex.load stateDirectory with
            | LakeFsIndex.Missing ->
                return OperationResult.noOp (Some "No lakeFS workspace index remains to clean up.") ()
            | LakeFsIndex.Corrupt message ->
                return Failed(OperationFailure.createRedacted ProviderError "workspace_index_corrupt" message)
            | LakeFsIndex.Loaded index ->
                let derivedBranch =
                    if String.IsNullOrWhiteSpace ownershipToken then
                        ""
                    else
                        workspaceBranchName binding.WorkspaceRoot ownershipToken

                let ownsBranch =
                    ownershipToken <> ""
                    && ownershipToken = index.OwnershipToken
                    && derivedBranch = index.WorkspaceBranch
                    && location.Repository = index.Repository
                    && location.TargetRef = index.TargetRef
                    && location.Prefix = index.Prefix

                if not ownsBranch then
                    return
                        Failed(
                            cleanupPreconditionFailure
                                "The persisted binding does not prove ownership of this lakeFS workspace branch."
                                expectedHead
                                None
                        )
                else
                    match expectedHead with
                    | None ->
                        return
                            Failed(
                                cleanupPreconditionFailure
                                    "Cleanup requires the caller-observed workspace-branch head."
                                    None
                                    index.WorkspaceRevision
                            )
                    | Some expected ->
                        let! connection = credentials.ResolveConnection binding.ConnectionProfileId

                        match connection with
                        | Error message ->
                            return
                                Failed(
                                    OperationFailure.createRedacted
                                        Authentication
                                        "connection_profile_unresolved"
                                        message
                                )
                        | Ok resolved ->
                            let! initialHead =
                                LakeFsApi.getBranch
                                    resolved
                                    index.Repository
                                    index.WorkspaceBranch
                                    context

                            match initialHead with
                            | Error failure when failure.Category = NotFound ->
                                return
                                    OperationResult.noOp
                                        (Some "The owned lakeFS workspace branch is already absent.")
                                        ()
                            | Error failure -> return Failed failure
                            | Ok branch when branch.CommitId <> expected ->
                                return
                                    Failed(
                                        cleanupPreconditionFailure
                                            "The owned workspace branch moved before cleanup."
                                            (Some expected)
                                            (Some branch.CommitId)
                                    )
                            | Ok _ ->
                                match hooks.Barrier with
                                | Some hook ->
                                    do! hook binding.WorkspaceRoot "cleanup-precheck-done" context
                                | None -> ()

                                let! finalHead =
                                    LakeFsApi.getBranch
                                        resolved
                                        index.Repository
                                        index.WorkspaceBranch
                                        context

                                match finalHead with
                                | Error failure when failure.Category = NotFound ->
                                    return
                                        OperationResult.noOp
                                            (Some "The owned lakeFS workspace branch is already absent.")
                                            ()
                                | Error failure -> return Failed failure
                                | Ok branch when branch.CommitId <> expected ->
                                    return
                                        Failed(
                                            cleanupPreconditionFailure
                                                "The owned workspace branch advanced during cleanup verification."
                                                (Some expected)
                                                (Some branch.CommitId)
                                        )
                                | Ok _ ->
                                    let! deleted =
                                        LakeFsApi.deleteBranch
                                            resolved
                                            index.Repository
                                            index.WorkspaceBranch
                                            context

                                    match deleted with
                                    | Error failure -> return Failed failure
                                    | Ok() ->
                                        let! verified =
                                            LakeFsApi.getBranch
                                                resolved
                                                index.Repository
                                                index.WorkspaceBranch
                                                context

                                        match verified with
                                        | Error failure when failure.Category = NotFound ->
                                            return OperationResult.succeeded ()
                                        | Error failure ->
                                            return Failed { failure with StateChanged = true }
                                        | Ok branch ->
                                            return
                                                OperationResult.partiallySucceeded
                                                    (OperationOutcome.performed ())
                                                    {
                                                        cleanupPreconditionFailure
                                                            "The workspace branch was recreated during cleanup verification."
                                                            (Some expected)
                                                            (Some branch.CommitId) with
                                                            StateChanged = true
                                                    }
                                                    {
                                                        Code = "review_workspace_branch"
                                                        Instructions =
                                                            Some
                                                                "Review the recreated branch before retrying cleanup."
                                                    }
    }

let cleanupOwnedWorkspaceBranch
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (hooks: LakeFsSessionHooks)
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    (binding: WorkspaceBinding)
    (ownershipToken: string)
    (expectedHead: string option)
    (context: OperationContext)
    : Async<OperationResult<unit>> =
    async {
        match LakeFsStateStore.resolve options binding.WorkspaceRoot binding.ProviderStateRef with
        | Error failure -> return Failed failure
        | Ok state ->
            return!
                cleanupOwnedWorkspaceBranchFromStateDirectory
                    state.StateDirectory
                    hooks
                    credentials
                    binding
                    ownershipToken
                    expectedHead
                    context
    }

/// Factory whose Open builds real lakeFS sessions.
let createFactoryWithHooks
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (hooks: LakeFsSessionHooks)
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    : ProviderFactory =
    let baseFactory = LakeFsProviderFactory.createFactory options credentials

    {
        baseFactory with
            Open = fun binding context -> openSession options hooks credentials binding context
    }

/// Accepts a provider-neutral revision policy for factory composition. lakeFS has no
/// large-object representation, so it ignores the strategy and never invokes it.
let createFactoryWithHooksAndPolicy
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (hooks: LakeFsSessionHooks)
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    (revisionPolicy: RevisionPolicyStrategy)
    : ProviderFactory =
    createFactoryWithHooks options hooks credentials

let createFactory
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    : ProviderFactory =
    createFactoryWithHooks options LakeFsSessionHooks.none credentials

/// Accepts the strategy and delegates to the existing lakeFS factory. lakeFS never
/// invokes the strategy because it has no large-object representation.
let createFactoryWithPolicy
    (options: LakeFsProviderOptions.LakeFsProviderOptions)
    (credentials: LakeFsCredentials.LakeFsCredentialStrategy)
    (revisionPolicy: RevisionPolicyStrategy)
    : ProviderFactory =
    createFactory options credentials
