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
module LakeFsObjectTransfer = VersionControlService.LakeFs.LakeFsObjectTransfer
module LakeFsSelectedRevision = VersionControlService.LakeFs.LakeFsSelectedRevision
module LakeFsSynchronization = VersionControlService.LakeFs.LakeFsSynchronization
module LakeFsConflictSession = VersionControlService.LakeFs.LakeFsConflictSession
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

type private SessionState = {
    Binding: WorkspaceBinding
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
            match LakeFsIndex.load state.Binding.WorkspaceRoot with
            | LakeFsIndex.Loaded persisted
                when persisted.Repository = state.Index.Repository
                     && persisted.WorkspaceBranch = state.Index.WorkspaceBranch
                     && persisted.OwnershipToken = state.Index.OwnershipToken ->
                state.Index <- persisted

                if workspaceVersion state <> expectedVersion then
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

let private saveIndex (state: SessionState) =
    match LakeFsIndex.save state.Binding.WorkspaceRoot state.Index with
    | Ok saved ->
        state.Index <- saved
        Ok()
    | Error message -> Error(OperationFailure.createRedacted ProviderError "index_write_failed" message)

let private reloadIndex (state: SessionState) =
    match LakeFsIndex.load state.Binding.WorkspaceRoot with
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
                            |> Array.choose (fun entry ->
                                relativePathOfKey state entry.Path
                                |> Option.bind (RepositoryPath.tryCreate >> Result.toOption))

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
    (changed: (string * string option * LakeFsIndex.LocalObjectState)[])
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
                let withoutSelected =
                    state.Index.Entries
                    |> Array.filter (fun entry ->
                        not (
                            changed
                            |> Array.exists (fun (pathValue, _, _) -> pathValue = entry.Path)
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
                                LakeFsIndex.LocalHash = LakeFsIndex.hashContent fileContent
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
                            let expectedParent = state.Index.WorkspaceRevision
                            let selectedTransfers: LakeFsObjectTransfer.SelectedObjectTransfer[] =
                                changed
                                |> Array.map (fun (pathValue, content, objectState) -> {
                                    Path = pathValue
                                    ObjectKey = objectKey state pathValue
                                    Content = content
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
                            | LakeFsObjectTransfer.TransferFailed(failure, completedPaths)
                                when failure.Category = Canceled ->
                                let! interrupted =
                                    recoverInterruptedSelectedRevision
                                        state
                                        resolved
                                        expectedParent
                                        context.OperationId

                                return Failed interrupted
                            | LakeFsObjectTransfer.TransferFailed(failure, completedPaths) ->
                                return
                                    Failed {
                                        failure with
                                            StateChanged = completedPaths.Length > 0
                                            AffectedPaths = completedPaths
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
                            | LakeFsObjectTransfer.TransferCompleted completedPaths ->
                                context.ReportProgress {
                                    PhaseCode = "selected-upload-complete"
                                    Item = None
                                    Completed = Some completedPaths.Length
                                    Total = Some selectedTransfers.Length
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
                                                    expectedParent
                                                    commit
                                                    branchAfter
                                                    commitAfter
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
        let! materialized = materializeRef state resolved targetRevision context

        match materialized with
        | Error failure -> return Error failure
        | Ok() ->
            let! reset =
                resetOwnedWorkspaceBranch
                    state
                    resolved
                    state.Index.WorkspaceRevision
                    targetRevision
                    context

            match reset with
            | Error failure -> return Error { failure with StateChanged = true }
            | Ok() ->
                state.Index <- {
                    state.Index with
                        TargetRef = targetName
                        BaseRevision = Some targetRevision
                        WorkspaceRevision = Some targetRevision
                }

                match saveIndex state with
                | Error failure -> return Error { failure with StateChanged = true }
                | Ok() -> return Ok()
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
                            | Error failure -> return Failed { failure with StateChanged = true }
                            | Ok() ->
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

                match changed with
                | Error failure -> return Failed failure
                | Ok paths ->
                    return
                        synchronizationState state (Some head)
                        |> LakeFsSynchronization.withRemoteChangedPaths paths
                        |> OperationResult.succeeded
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

                return
                    LakeFsSynchronization.createPreview changedPaths dirtyPaths localCommitted
                    |> OperationResult.succeeded
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
                                                        state.Index.WorkspaceBranch

                                                let! materialized =
                                                    materializeRef state resolved materializationRef context

                                                match materialized with
                                                | Error failure ->
                                                    return Failed { failure with StateChanged = true }
                                                | Ok() ->
                                                    let resultingWorkspace =
                                                        if canFastForwardToTarget then
                                                            head
                                                        else
                                                            mergeResult.Reference

                                                    state.Index <- {
                                                        state.Index with
                                                            BaseRevision = Some head
                                                            WorkspaceRevision = Some resultingWorkspace
                                                    }

                                                    match saveIndex state with
                                                    | Error failure ->
                                                        return Failed { failure with StateChanged = true }
                                                    | Ok() ->
                                                        return
                                                            synchronizationState state (Some head)
                                                            |> OperationResult.succeeded
    }

let private publish (state: SessionState) (request: PublishRequest) (context: OperationContext) =
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
        let! materialized =
            materializeRef state resolved state.Index.WorkspaceBranch context

        match materialized with
        | Error failure -> return Failed { failure with StateChanged = true }
        | Ok() ->
            state.Index <- {
                state.Index with
                    BaseRevision = Some targetRevision
                    WorkspaceRevision = Some resultingRevision
            }

            match saveIndex state with
            | Error failure -> return Failed { failure with StateChanged = true }
            | Ok() ->
                state.Conflict <- None
                return OperationResult.succeeded (Some(mkRevisionId resultingRevision))
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
        fun request context -> async {
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
        }
    Finalize =
        fun request context -> async {
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
                                                        let! upload =
                                                            LakeFsApi.uploadObject
                                                                resolved
                                                                state.Index.Repository
                                                                state.Index.WorkspaceBranch
                                                                (objectKey state item.ItemPath)
                                                                content
                                                                context

                                                        match upload with
                                                        | Error failure -> resolutionFailure <- Some failure
                                                        | Ok() -> ()
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
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion (fun () ->
                    createRef state request context)
        PreflightSwitchRef =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion (fun () ->
                    preflightSwitchRef state request context)
        SwitchRef =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion (fun () ->
                    switchRef state request context)
        CreateRevision =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion (fun () ->
                    createRevision state request context)
        RestorePaths =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion (fun () ->
                    restorePaths state request context)
        GetDiffSummary = fun context -> getDiffSummary state context
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
            ConflictResolution = Some(createConflictService state)
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
let cleanupOwnedWorkspaceBranch
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
            match LakeFsIndex.load binding.WorkspaceRoot with
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
