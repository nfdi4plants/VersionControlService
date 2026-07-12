/// v2 Git provider: factory and per-workspace sessions over the extracted Git
/// internals. The initial shell deliberately wraps the current v1 behaviors so
/// the recorded defects stay observable; the Task 8/9/10 behavior cycles replace
/// them one row at a time.
module VersionControlService.Git.GitWorkspaceSession

open System
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Contracts.Git

module GitService = VersionControlService.Git.GitService
module GitProvisioningService = VersionControlService.Git.GitProvisioningService
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path

/// Test seams. `RunProcess` lets tests spy on or replace child-process execution;
/// `Barrier` lets tests pause/act at named transaction points (root, point, context).
type GitSessionHooks = {
    RunProcess: (NodeProcess.ProcessRequest -> OperationContext -> Async<OperationResult<NodeProcess.ProcessOutput>>) option
    Barrier: (string -> string -> OperationContext -> Async<unit>) option
}

module GitSessionHooks =

    let none: GitSessionHooks = {
        RunProcess = None
        Barrier = None
    }

[<Emit("Date.now()")>]
let private nowMilliseconds () : float = jsNative

let private gitProviderId =
    match ProviderId.tryCreate "git" with
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

let private categoryOfKind (kind: GitFailureKind) =
    match kind with
    | GitFailureKind.Unauthorized -> Authentication
    | GitFailureKind.Forbidden -> Authorization
    | GitFailureKind.Network -> Network
    | GitFailureKind.Timeout -> Timeout
    | GitFailureKind.Canceled -> Canceled
    | GitFailureKind.LfsInstallRequired -> DependencyMissing
    | GitFailureKind.RemoteProjectAlreadyExists -> ProviderError
    | GitFailureKind.Unknown -> ProviderError

let private codeOfKind (kind: GitFailureKind) =
    match kind with
    | GitFailureKind.Unauthorized -> "unauthorized"
    | GitFailureKind.Forbidden -> "forbidden"
    | GitFailureKind.Network -> "network_failure"
    | GitFailureKind.Timeout -> "timeout"
    | GitFailureKind.Canceled -> "operation_canceled"
    | GitFailureKind.LfsInstallRequired -> "lfs_install_required"
    | GitFailureKind.RemoteProjectAlreadyExists -> "remote_project_exists"
    | GitFailureKind.Unknown -> "git_failure"

let private toOperationFailure (failure: GitService.GitFailure) : OperationFailure =
    OperationFailure.createRedacted (categoryOfKind failure.Kind) (codeOfKind failure.Kind) failure.Message

let private awaitGit (operation: JS.Promise<GitService.GitResult<'T>>) : Async<Result<'T, OperationFailure>> =
    async {
        let! result = Async.AwaitPromise operation

        match result with
        | Ok value -> return Ok value
        | Error failure -> return Error(toOperationFailure failure)
    }

/// Direct git process invocation honoring the RunProcess hook.
let private runGit
    (hooks: GitSessionHooks)
    (repoPath: string)
    (arguments: string[])
    (stdinData: string option)
    (context: OperationContext)
    : Async<Result<NodeProcess.ProcessOutput, OperationFailure>> =
    async {
        let request = {
            NodeProcess.ProcessRequest.create "git" arguments with
                WorkingDirectory = Some repoPath
                StdinData = stdinData
                ProgressPhase = "git"
        }

        let runner = hooks.RunProcess |> Option.defaultValue NodeProcess.run
        let! result = runner request context

        match result with
        | Succeeded outcome -> return Ok outcome.Value
        | PartiallySucceeded(_, failure)
        | Failed failure -> return Error failure
    }

/// runGit that fails when git exits nonzero.
let private runGitChecked hooks repoPath arguments stdinData context =
    async {
        let! result = runGit hooks repoPath arguments stdinData context

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            let commandSummary = String.concat " " (Array.truncate 3 arguments)

            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "git_failure"
                        $"git {commandSummary} failed: {output.StdErr}"
                )
        | Ok output -> return Ok output
    }

let private barrier (hooks: GitSessionHooks) (repoPath: string) (point: string) (context: OperationContext) =
    match hooks.Barrier with
    | Some barrierHook -> barrierHook repoPath point context
    | None -> async.Return()

let private tryCreateRepositoryPath (value: string) = RepositoryPath.tryCreate value

let private toFileChange (isConflicted: bool) (file: GitFileStatusDto) : FileChange option =
    match tryCreateRepositoryPath file.Path with
    | Error _ -> None
    | Ok path ->
        let kind =
            if isConflicted then ConflictedChange
            elif file.Index = "A" || file.WorkingDir = "?" || file.Index = "?" then AddedChange
            elif file.Index = "D" || file.WorkingDir = "D" then DeletedChange
            elif file.Index = "R" then RenamedChange
            else ModifiedChange

        Some {
            Path = path
            OldPath =
                file.OriginalPath
                |> Option.bind (tryCreateRepositoryPath >> Result.toOption)
            Kind = kind
        }

/// Session-internal mutable state shared by all operations of one open session.
type private SessionState = {
    RepoPath: string
    Hooks: GitSessionHooks
}

// ---------------------------------------------------------------------------
// Status and workspace version
// ---------------------------------------------------------------------------

/// Shell workspace version: deliberately unstable (Date-based) until the
/// workspace-version cycle (8.3) derives a stable token and adds the session lock.
let private workspaceVersion (_state: SessionState) (_status: GitStatusDto) : Async<string> =
    async { return $"unstable-{nowMilliseconds ()}" }

let private toWorkspaceStatus (state: SessionState) (status: GitStatusDto) (context: OperationContext) =
    async {
        let conflictedSet = Set.ofArray status.Conflicted

        let changes =
            status.Files
            |> Array.choose (fun file -> toFileChange (conflictedSet.Contains file.Path) file)

        let! version = workspaceVersion state status

        // Synchronization revisions: HEAD and the current branch's origin counterpart.
        let! headRevision =
            runGit state.Hooks state.RepoPath [| "rev-parse"; "HEAD" |] None context

        let workspaceRevision =
            match headRevision with
            | Ok output when output.ExitCode = 0 -> Some(mkRevisionId (output.StdOut.Trim()))
            | _ -> None

        let! targetRevisionOutput =
            match status.Current with
            | Some current ->
                runGit state.Hooks state.RepoPath [| "rev-parse"; $"refs/remotes/origin/{current}" |] None context
            | None -> async { return Ok { ExitCode = 1; StdOut = ""; StdErr = "" } }

        let targetRevision =
            match targetRevisionOutput with
            | Ok output when output.ExitCode = 0 -> Some(mkRevisionId (output.StdOut.Trim()))
            | _ -> None

        let relationship =
            match workspaceRevision, targetRevision with
            | _, None -> NoTarget
            | Some workspace, Some target when workspace = target -> UpToDate
            | _ when status.Ahead > 0 && status.Behind > 0 -> Diverged
            | _ when status.Behind > 0 -> TargetAhead
            | _ when status.Ahead > 0 -> LocalAhead
            | _ -> UnknownRelationship

        return {
            CurrentRef =
                status.Current
                |> Option.map (fun current -> {
                    Name = current
                    ProviderRef = mkProviderRef $"git-local:{current}"
                    Kind = LocalRef
                    IsCurrent = true
                })
            WorkspaceVersion = version
            Changes = changes
            // Conflict sessions arrive with the Task 9 conflict-session cycle.
            ActiveConflictSession = None
            Synchronization =
                Some {
                    BaseRevision = None
                    WorkspaceRevision = workspaceRevision
                    TargetRevision = targetRevision
                    LocalRevisionCount = Some status.Ahead
                    TargetRevisionCount = Some status.Behind
                    RemoteChangedPaths = None
                    Relationship = relationship
                }
        }
    }

let private getWorkspaceStatus (state: SessionState) (context: OperationContext) =
    async {
        let! statusResult = awaitGit (GitService.getStatus state.RepoPath)

        match statusResult with
        | Error failure -> return Failed failure
        | Ok status ->
            let! workspaceStatus = toWorkspaceStatus state status context
            return OperationResult.succeeded workspaceStatus
    }

// ---------------------------------------------------------------------------
// Core operations (shell: wraps the recorded v1 behaviors)
// ---------------------------------------------------------------------------

let private refNameOfProviderRef (reference: ProviderRef) =
    let value = ProviderRef.value reference

    if value.StartsWith "git-remote:" then
        Choice2Of2(value.Substring "git-remote:".Length)
    elif value.StartsWith "git-local:" then
        Choice1Of2(value.Substring "git-local:".Length)
    else
        Choice1Of2 value

let private createRevision (state: SessionState) (request: CreateRevisionRequest) (context: OperationContext) =
    async {
        if request.Paths.Length = 0 then
            return OperationResult.validationFailed "no_paths_selected" "Select at least one path."
        else
            // v1 flow (defect record): unstage everything staged, stage the selected
            // paths as raw pathspecs, then commit. The Task 8 cycles replace this with
            // literal transport and an isolated transaction.
            let pathValues = request.Paths |> Array.map RepositoryPath.value

            let! statusResult = awaitGit (GitService.getStatus state.RepoPath)

            match statusResult with
            | Error failure -> return Failed failure
            | Ok status ->
                let stagedPaths =
                    status.Files
                    |> Array.filter (fun file ->
                        not (String.IsNullOrWhiteSpace file.Index)
                        && file.Index <> " "
                        && file.Index <> "?")
                    |> Array.map _.Path
                    |> Array.distinct

                let! unstageResult =
                    if stagedPaths.Length = 0 then
                        async { return Ok() }
                    else
                        awaitGit (GitService.unstagePaths state.RepoPath stagedPaths)

                match unstageResult with
                | Error failure -> return Failed failure
                | Ok() ->
                    // Selected paths are exact literal file names: :(literal) pathspec
                    // magic disables wildcard/bracket expansion.
                    let literalSpecs = request.Paths |> Array.map GitPathTransport.literalPathspec

                    let! stageOutput =
                        runGit state.Hooks state.RepoPath [| "add"; "--"; yield! literalSpecs |] None context

                    let stageResult =
                        match stageOutput with
                        | Error failure -> Error failure
                        | Ok output when output.ExitCode = 0 -> Ok()
                        | Ok output when output.StdErr.Contains "did not match any files" ->
                            Error {
                                OperationFailure.create
                                    NotFound
                                    "path_not_found"
                                    "A selected path does not exist in the workspace." with
                                    AffectedPaths = pathValues
                            }
                        | Ok output ->
                            Error(
                                OperationFailure.createRedacted
                                    ProviderError
                                    "git_failure"
                                    $"Staging the selected paths failed: {output.StdErr}"
                            )

                    match stageResult with
                    | Error failure -> return Failed failure
                    | Ok() ->
                        let! commitResult = awaitGit (GitService.commit state.RepoPath request.Message)

                        match commitResult with
                        | Error failure -> return Failed failure
                        | Ok commitHash ->
                            return
                                Succeeded {
                                    OperationOutcome.performed (mkRevisionId commitHash) with
                                        AffectedPaths = pathValues
                                        ResultingRevision = Some(mkRevisionId commitHash)
                                        Publication = LocalOnly
                                }
    }

let private restorePaths (state: SessionState) (request: RestoreRequest) (context: OperationContext) =
    async {
        if request.Paths.Length = 0 then
            return OperationResult.validationFailed "no_paths_selected" "Select at least one path."
        else
            let! result =
                awaitGit (GitService.discardPaths state.RepoPath (request.Paths |> Array.map RepositoryPath.value))

            match result with
            | Error failure -> return Failed failure
            | Ok() -> return OperationResult.succeeded ()
    }

let private listRefs (state: SessionState) (context: OperationContext) =
    async {
        let! result = awaitGit (GitService.getBranches state.RepoPath)

        match result with
        | Error failure -> return Failed failure
        | Ok branches ->
            return
                branches
                |> Array.map (fun branch -> {
                    Name = branch.RefName
                    ProviderRef =
                        match branch.Kind with
                        | GitBranchRefKind.Local -> mkProviderRef $"git-local:{branch.RefName}"
                        | GitBranchRefKind.Remote -> mkProviderRef $"git-remote:{branch.RefName}"
                    Kind =
                        match branch.Kind with
                        | GitBranchRefKind.Local -> LocalRef
                        | GitBranchRefKind.Remote -> RemoteRef
                    IsCurrent = branch.IsCurrent
                })
                |> OperationResult.succeeded
    }

let private createRef (state: SessionState) (request: CreateRefRequest) (context: OperationContext) =
    async {
        let baseRef =
            request.BaseRef
            |> Option.map (fun reference ->
                match refNameOfProviderRef reference with
                | Choice1Of2 name -> name
                | Choice2Of2 name -> name)

        if request.SwitchTo then
            let! result = awaitGit (GitService.createBranch state.RepoPath request.Name baseRef)

            match result with
            | Error failure -> return Failed failure
            | Ok() ->
                return
                    OperationResult.succeeded {
                        Name = request.Name
                        ProviderRef = mkProviderRef $"git-local:{request.Name}"
                        Kind = LocalRef
                        IsCurrent = true
                    }
        else
            // Create-only: the branch is created without switching.
            let arguments =
                match baseRef with
                | Some basePoint -> [| "branch"; "--"; request.Name; basePoint |]
                | None -> [| "branch"; "--"; request.Name |]

            let! result = runGitChecked state.Hooks state.RepoPath arguments None context

            match result with
            | Error failure -> return Failed failure
            | Ok _ ->
                return
                    OperationResult.succeeded {
                        Name = request.Name
                        ProviderRef = mkProviderRef $"git-local:{request.Name}"
                        Kind = LocalRef
                        IsCurrent = false
                    }
    }

let private preflightSwitchRef (state: SessionState) (request: SwitchRefRequest) (context: OperationContext) =
    async {
        let targetName =
            match refNameOfProviderRef request.TargetRef with
            | Choice1Of2 name -> name
            | Choice2Of2 name -> name

        let! verify = runGit state.Hooks state.RepoPath [| "rev-parse"; "--verify"; "--quiet"; targetName |] None context

        match verify with
        | Ok output when output.ExitCode <> 0 ->
            return
                Failed(OperationFailure.create NotFound "ref_not_found" $"Ref '{targetName}' does not exist.")
        | Error failure -> return Failed failure
        | Ok _ ->
            let! changedBetween =
                runGitChecked state.Hooks state.RepoPath [| "diff"; "--name-only"; "HEAD"; targetName |] None context

            match changedBetween with
            | Error failure -> return Failed failure
            | Ok diffOutput ->
                let differingPaths =
                    diffOutput.StdOut.Replace("\r\n", "\n").Split('\n')
                    |> Array.filter (fun line -> line.Trim() <> "")
                    |> Set.ofArray

                let! statusResult = awaitGit (GitService.getStatus state.RepoPath)

                match statusResult with
                | Error failure -> return Failed failure
                | Ok status ->
                    let dirtyPaths = status.Files |> Array.map _.Path |> Set.ofArray

                    let atRisk =
                        Set.intersect dirtyPaths differingPaths
                        |> Set.toArray
                        |> Array.choose (tryCreateRepositoryPath >> Result.toOption)

                    return
                        OperationResult.succeeded {
                            PathsAtRisk = atRisk
                            IsSafe = atRisk.Length = 0
                        }
    }

let private switchRef (state: SessionState) (request: SwitchRefRequest) (context: OperationContext) =
    async {
        let checkoutRequest: GitCheckoutBranchRequest =
            match refNameOfProviderRef request.TargetRef with
            | Choice2Of2 remoteRef ->
                let localName =
                    match remoteRef.Split '/' with
                    | segments when segments.Length >= 2 -> String.Join("/", segments |> Array.skip 1)
                    | _ -> remoteRef

                {
                    Name = localName
                    StartPoint = Some remoteRef
                }
            | Choice1Of2 localName ->
                {
                    Name = localName
                    StartPoint = None
                }

        let! result = awaitGit (GitService.checkoutBranch state.RepoPath checkoutRequest)

        match result with
        | Error failure -> return Failed failure
        | Ok() -> return! getWorkspaceStatus state context
    }

let private getDiffSummary (state: SessionState) (context: OperationContext) =
    async {
        // Shell: unstaged-only diff (recorded GIT-005 behavior); the Task 9 diff
        // cycle unions staged and unstaged changes.
        let! result = runGitChecked state.Hooks state.RepoPath [| "diff"; "--name-status" |] None context

        match result with
        | Error failure -> return Failed failure
        | Ok output ->
            let entries =
                output.StdOut.Replace("\r\n", "\n").Split('\n')
                |> Array.filter (fun line -> line.Trim() <> "")
                |> Array.choose (fun line ->
                    let parts = line.Split '\t'

                    if parts.Length < 2 then
                        None
                    else
                        match tryCreateRepositoryPath parts[1] with
                        | Error _ -> None
                        | Ok path ->
                            let kind =
                                match parts[0].Trim() with
                                | "A" -> AddedChange
                                | "D" -> DeletedChange
                                | status when status.StartsWith "R" -> RenamedChange
                                | _ -> ModifiedChange

                            Some {
                                Path = path
                                OldPath = None
                                Kind = kind
                                LineInsertions = None
                                LineDeletions = None
                            })

            return OperationResult.succeeded { Entries = entries }
    }

// ---------------------------------------------------------------------------
// Synchronization (shell: direct git commands over the configured origin)
// ---------------------------------------------------------------------------

let private currentBranchName (state: SessionState) (context: OperationContext) =
    async {
        let! result =
            runGitChecked state.Hooks state.RepoPath [| "branch"; "--show-current" |] None context

        match result with
        | Error failure -> return Error failure
        | Ok output ->
            let name = output.StdOut.Trim()

            if name = "" then
                return
                    Error(OperationFailure.create Validation "detached_head" "The workspace has no current branch.")
            else
                return Ok name
    }

let private revParse (state: SessionState) (reference: string) (context: OperationContext) =
    async {
        let! result =
            runGit state.Hooks state.RepoPath [| "rev-parse"; "--verify"; "--quiet"; reference |] None context

        match result with
        | Ok output when output.ExitCode = 0 -> return Some(output.StdOut.Trim())
        | _ -> return None
    }

let private synchronizationState (state: SessionState) (context: OperationContext) =
    async {
        let! branchResult = currentBranchName state context

        match branchResult with
        | Error failure -> return Error failure
        | Ok branch ->
            let! workspaceRevision = revParse state "HEAD" context
            let! targetRevision = revParse state $"refs/remotes/origin/{branch}" context

            let! baseRevision =
                match targetRevision with
                | Some target ->
                    async {
                        let! mergeBase =
                            runGit state.Hooks state.RepoPath [| "merge-base"; "HEAD"; target |] None context

                        match mergeBase with
                        | Ok output when output.ExitCode = 0 -> return Some(output.StdOut.Trim())
                        | _ -> return None
                    }
                | None -> async { return None }

            let relationship =
                match workspaceRevision, targetRevision, baseRevision with
                | _, None, _ -> NoTarget
                | Some workspace, Some target, _ when workspace = target -> UpToDate
                | Some workspace, Some _, Some mergeBase when workspace = mergeBase -> TargetAhead
                | Some _, Some target, Some mergeBase when target = mergeBase -> LocalAhead
                | Some _, Some _, Some _ -> Diverged
                | _ -> UnknownRelationship

            let! remoteChanged =
                match baseRevision, targetRevision with
                | Some mergeBase, Some target when mergeBase <> target ->
                    async {
                        let! diff =
                            runGit state.Hooks state.RepoPath [| "diff"; "--name-only"; mergeBase; target |] None context

                        match diff with
                        | Ok output when output.ExitCode = 0 ->
                            return
                                Some(
                                    output.StdOut.Replace("\r\n", "\n").Split('\n')
                                    |> Array.filter (fun line -> line.Trim() <> "")
                                    |> Array.choose (tryCreateRepositoryPath >> Result.toOption)
                                )
                        | _ -> return None
                    }
                | _ -> async { return None }

            return
                Ok {
                    BaseRevision = baseRevision |> Option.map mkRevisionId
                    WorkspaceRevision = workspaceRevision |> Option.map mkRevisionId
                    TargetRevision = targetRevision |> Option.map mkRevisionId
                    LocalRevisionCount = None
                    TargetRevisionCount = None
                    RemoteChangedPaths = remoteChanged
                    Relationship = relationship
                }
    }

let private hasOrigin (state: SessionState) (context: OperationContext) =
    async {
        let! result = runGit state.Hooks state.RepoPath [| "remote" |] None context

        match result with
        | Ok output -> return output.StdOut.Contains "origin"
        | Error _ -> return false
    }

let private refresh (state: SessionState) (context: OperationContext) =
    async {
        let! originExists = hasOrigin state context

        if originExists then
            do! barrier state.Hooks state.RepoPath "transfer-start" context

            let! fetchResult = runGitChecked state.Hooks state.RepoPath [| "fetch"; "origin" |] None context

            match fetchResult with
            | Error failure -> return Failed { failure with Retryable = true }
            | Ok _ ->
                let! stateResult = synchronizationState state context

                match stateResult with
                | Error failure -> return Failed failure
                | Ok syncState -> return OperationResult.succeeded syncState
        else
            let! stateResult = synchronizationState state context

            match stateResult with
            | Error failure -> return Failed failure
            | Ok syncState -> return OperationResult.noOp (Some "No target is configured.") syncState
    }

let private previewUpdate (state: SessionState) (context: OperationContext) =
    async {
        let! refreshResult = refresh state context

        match refreshResult with
        | Failed failure -> return Failed failure
        | PartiallySucceeded(_, failure) -> return Failed failure
        | Succeeded outcome ->
            let syncState = outcome.Value

            let changed = syncState.RemoteChangedPaths |> Option.defaultValue [||]

            let! statusResult = awaitGit (GitService.getStatus state.RepoPath)

            match statusResult with
            | Error failure -> return Failed failure
            | Ok status ->
                let dirtyPaths = status.Files |> Array.map _.Path |> Set.ofArray

                let overlapping =
                    changed
                    |> Array.filter (fun path -> dirtyPaths.Contains(RepositoryPath.value path))

                return
                    OperationResult.succeeded {
                        ChangedPaths = changed
                        OverlappingPaths = overlapping
                        HasDataLossRisk = overlapping.Length > 0
                        WouldCreateConflictSession = overlapping.Length > 0
                    }
    }

let private update (state: SessionState) (request: UpdateRequest) (context: OperationContext) =
    async {
        let! refreshResult = refresh state context

        match refreshResult with
        | Failed failure -> return Failed failure
        | PartiallySucceeded(_, failure) -> return Failed failure
        | Succeeded refreshOutcome ->
            let syncState = refreshOutcome.Value

            match syncState.Relationship with
            | UpToDate
            | LocalAhead
            | NoTarget -> return OperationResult.noOp (Some "The workspace is already up to date.") syncState
            | _ ->
                let targetReference =
                    syncState.TargetRevision |> Option.map RevisionId.value |> Option.get

                do! barrier state.Hooks state.RepoPath "update-merge" context

                let! mergeResult =
                    runGit state.Hooks state.RepoPath [| "merge"; "--no-edit"; targetReference |] None context

                match mergeResult with
                | Error failure -> return Failed failure
                | Ok output when output.ExitCode = 0 ->
                    let! updatedState = synchronizationState state context

                    match updatedState with
                    | Error failure -> return Failed failure
                    | Ok newState -> return OperationResult.succeeded newState
                | Ok _ ->
                    // Conflicting merge: the conflict-session cycle turns this into a
                    // provider-managed session; the shell reports the structured code.
                    let! updatedState = synchronizationState state context

                    let stateValue =
                        match updatedState with
                        | Ok value -> value
                        | Error _ -> syncState

                    return
                        OperationResult.partiallySucceeded
                            (OperationOutcome.performed stateValue)
                            (OperationFailure.create
                                Conflict
                                "conflicts_detected"
                                "The update produced conflicts that need resolution.")
                            {
                                Code = "resolve_conflict_session"
                                Instructions = Some "Resolve every conflict item, then finalize."
                            }
    }

let private publish (state: SessionState) (request: PublishRequest) (context: OperationContext) =
    async {
        let! branchResult = currentBranchName state context

        match branchResult with
        | Error failure -> return Failed failure
        | Ok branch ->
            // Client-side pre-check against the consumer-observed target revision.
            let! observedTarget = revParse state $"refs/remotes/origin/{branch}" context

            let expectedMatches =
                match request.ExpectedTargetRevision, observedTarget with
                | None, _ -> true
                | Some expected, Some observed -> RevisionId.value expected = observed
                | Some _, None -> false

            if not expectedMatches then
                return
                    Failed {
                        OperationFailure.create
                            Concurrency
                            "precondition_failed"
                            "The publication target advanced past the expected revision." with
                            RevisionEvidence = [|
                                yield!
                                    request.ExpectedTargetRevision
                                    |> Option.map (fun revision -> "expected_target", revision)
                                    |> Option.toList
                                yield!
                                    observedTarget
                                    |> Option.map (fun observed -> "observed_target", mkRevisionId observed)
                                    |> Option.toList
                            |]
                    }
            else
                let! workspaceRevision = revParse state "HEAD" context

                if workspaceRevision = observedTarget then
                    let! stateResult = synchronizationState state context

                    match stateResult with
                    | Error failure -> return Failed failure
                    | Ok syncState ->
                        return
                            OperationResult.noOp (Some "The target already has every local revision.") syncState
                else
                    do! barrier state.Hooks state.RepoPath "publish-precheck-done" context
                    do! barrier state.Hooks state.RepoPath "transfer-start" context

                    let! pushResult =
                        runGit state.Hooks state.RepoPath [| "push"; "origin"; branch |] None context

                    match pushResult with
                    | Error failure -> return Failed failure
                    | Ok output when output.ExitCode <> 0 ->
                        let combined = output.StdErr + output.StdOut

                        if combined.Contains "rejected" || combined.Contains "non-fast-forward" then
                            let! raced = revParse state $"refs/remotes/origin/{branch}" context

                            return
                                Failed {
                                    OperationFailure.createRedacted
                                        Concurrency
                                        "precondition_failed"
                                        "The publication target advanced during publish." with
                                        Retryable = true
                                        RevisionEvidence = [|
                                            yield!
                                                observedTarget
                                                |> Option.map (fun observed ->
                                                    "expected_target", mkRevisionId observed)
                                                |> Option.toList
                                            yield!
                                                raced
                                                |> Option.map (fun value -> "observed_target", mkRevisionId value)
                                                |> Option.toList
                                        |]
                                }
                        else
                            return
                                Failed {
                                    OperationFailure.createRedacted
                                        Network
                                        "target_unreachable"
                                        $"Publishing to origin failed: {combined}" with
                                        Retryable = true
                                }
                    | Ok _ ->
                        let! stateResult = synchronizationState state context

                        match stateResult with
                        | Error failure -> return Failed failure
                        | Ok syncState ->
                            return
                                Succeeded {
                                    OperationOutcome.performed syncState with
                                        Publication = Published
                                        ResultingRevision = syncState.WorkspaceRevision
                                }
    }

// ---------------------------------------------------------------------------
// Conflict sessions (shell: none active until the Task 9 conflict cycle)
// ---------------------------------------------------------------------------

let private noActiveSessionFailure () =
    OperationFailure.create NotFound "no_conflict_session" "No conflict session is active."

let private createConflictService (state: SessionState) : ConflictResolutionService = {
    GetActiveSession = fun _ -> async { return OperationResult.succeeded None }
    Resolve = fun _ _ -> async { return Failed(noActiveSessionFailure ()) }
    Finalize = fun _ _ -> async { return Failed(noActiveSessionFailure ()) }
    Cancel = fun _ _ -> async { return Failed(noActiveSessionFailure ()) }
}

// ---------------------------------------------------------------------------
// Optional extensions available from the shell
// ---------------------------------------------------------------------------

let private createTextDiff (state: SessionState) : TextDiffService =
    let mapDiff (operation: JS.Promise<GitService.GitResult<string>>) =
        async {
            let! result = awaitGit operation

            match result with
            | Ok text -> return OperationResult.succeeded (TextContent text)
            | Error failure -> return OperationResult.succeeded (UnsupportedContent(Some failure.Message))
        }

    {
        GetDiff = fun path _ -> mapDiff (GitService.getDiff state.RepoPath [| RepositoryPath.value path |])
        GetWordDiff = fun path _ -> mapDiff (GitService.getWordDiff state.RepoPath [| RepositoryPath.value path |])
    }

let private createBrowser (state: SessionState) : RepositoryBrowserService = {
    GetRepositoryWebUrl =
        fun _ -> async {
            let! result = Async.AwaitPromise(GitService.getOriginRepositoryWebUrl state.RepoPath)

            match result with
            | Ok url -> return OperationResult.succeeded url
            | Error failure -> return Failed(toOperationFailure failure)
        }
}

// ---------------------------------------------------------------------------
// Session and factory
// ---------------------------------------------------------------------------

let createSession (hooks: GitSessionHooks) (binding: WorkspaceBinding) : WorkspaceSession =
    let state = {
        RepoPath = binding.WorkspaceRoot
        Hooks = hooks
    }

    let core: CoreVersionControl = {
        GetStatus = fun context -> getWorkspaceStatus state context
        ListRefs = fun context -> listRefs state context
        CreateRef = fun request context -> createRef state request context
        PreflightSwitchRef = fun request context -> preflightSwitchRef state request context
        SwitchRef = fun request context -> switchRef state request context
        CreateRevision = fun request context -> createRevision state request context
        RestorePaths = fun request context -> restorePaths state request context
        GetDiffSummary = fun context -> getDiffSummary state context
    }

    let descriptor = {
        ProviderId = gitProviderId
        WorkspaceRoot = binding.WorkspaceRoot
        Location = Some binding.Location
    }

    {
        WorkspaceSession.createCoreOnly descriptor core with
            Synchronization =
                Some {
                    Refresh = fun context -> refresh state context
                    PreviewUpdate = fun context -> previewUpdate state context
                    Update = fun request context -> update state request context
                    Publish = fun request context -> publish state request context
                }
            ConflictResolution = Some(createConflictService state)
            TextDiff = Some(createTextDiff state)
            RepositoryBrowser = Some(createBrowser state)
    }

let private probe (workspacePath: string) : Async<ProbeResult> =
    async {
        try
            let markerPath = NodePath.join [| workspacePath; ".git" |]

            if NodeFileSystem.existsSync markerPath then
                return Detected(workspacePath, 100, None)
            else
                return NotDetected
        with error ->
            return ProbeFailed(OperationFailure.createRedacted ProviderError "probe_exception" error.Message)
    }

let private bindingFor (workspaceRoot: string) (location: RepositoryLocation) : WorkspaceBinding = {
    SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
    ProviderId = gitProviderId
    WorkspaceRoot = workspaceRoot
    ProviderStateRef = None
    Location = location
    ConnectionProfileId = location.ConnectionProfileId
}

let private localLocation (providerLocation: string) : RepositoryLocation = {
    ProviderId = gitProviderId
    DisplayName = None
    ProviderLocation = providerLocation
    ConnectionProfileId = None
}

let createFactory (hooks: GitSessionHooks) : ProviderFactory = {
    Id = gitProviderId
    Probe = probe
    VerifyLocation =
        fun request context -> async {
            // Shell: reachability only; per-intent verification arrives with the
            // credential/provisioning task.
            let! result =
                runGit hooks "." [| "ls-remote"; request.Location.ProviderLocation |] None context

            match result with
            | Ok output when output.ExitCode = 0 ->
                return
                    OperationResult.succeeded {
                        Location = request.Location
                        GrantedIntents = request.Intents
                        DeniedIntents = [||]
                    }
            | Ok output ->
                return
                    Failed(
                        OperationFailure.createRedacted
                            Network
                            "location_unreachable"
                            $"The repository location is not reachable: {output.StdErr}"
                    )
            | Error failure -> return Failed failure
        }
    Initialize =
        fun request context -> async {
            let! result = Async.AwaitPromise(GitProvisioningService.initRepository request.TargetPath)

            match result with
            | Error failure -> return Failed(toOperationFailure failure)
            | Ok normalizedPath ->
                let location =
                    request.Location |> Option.defaultValue (localLocation normalizedPath)

                return OperationResult.succeeded (bindingFor normalizedPath location)
        }
    Clone =
        fun request context -> async {
            let! result =
                runGit
                    hooks
                    "."
                    [|
                        "clone"
                        request.Location.ProviderLocation
                        request.TargetPath
                    |]
                    None
                    context

            match result with
            | Error failure -> return Failed failure
            | Ok output when output.ExitCode <> 0 ->
                let combined = output.StdErr + output.StdOut

                if combined.Contains "already exists and is not an empty directory" then
                    return
                        Failed(
                            OperationFailure.create
                                Validation
                                "target_not_empty"
                                "The clone target directory is not empty."
                        )
                else
                    return
                        Failed(
                            OperationFailure.createRedacted ProviderError "clone_failed" $"Clone failed: {combined}"
                        )
            | Ok _ -> return OperationResult.succeeded (bindingFor request.TargetPath request.Location)
        }
    Bind =
        fun request context -> async {
            // Attach or re-target: set the origin remote of the existing workspace.
            let! existing = runGit hooks request.WorkspaceRoot [| "remote" |] None context

            let! result =
                match existing with
                | Ok output when output.StdOut.Contains "origin" ->
                    runGitChecked
                        hooks
                        request.WorkspaceRoot
                        [|
                            "remote"
                            "set-url"
                            "origin"
                            request.Location.ProviderLocation
                        |]
                        None
                        context
                | _ ->
                    runGitChecked
                        hooks
                        request.WorkspaceRoot
                        [|
                            "remote"
                            "add"
                            "origin"
                            request.Location.ProviderLocation
                        |]
                        None
                        context

            match result with
            | Error failure -> return Failed failure
            | Ok _ ->
                let! _ = runGit hooks request.WorkspaceRoot [| "fetch"; "origin" |] None context
                return OperationResult.succeeded (bindingFor request.WorkspaceRoot request.Location)
        }
    Open = fun binding _ -> async { return OperationResult.succeeded (createSession hooks binding) }
    CheckDependencies =
        fun context -> async {
            let! gitVersion = runGit hooks "." [| "--version" |] None context

            match gitVersion with
            | Ok output when output.ExitCode = 0 ->
                return
                    OperationResult.succeeded [|
                        {
                            Component = "git"
                            Installed = true
                            Version = Some(output.StdOut.Trim())
                            Compatible = true
                            Remediation = None
                        }
                    |]
            | _ ->
                return
                    OperationResult.succeeded [|
                        {
                            Component = "git"
                            Installed = false
                            Version = None
                            Compatible = false
                            Remediation = Some "Install Git 2.38 or newer and ensure it is on PATH."
                        }
                    |]
        }
}
