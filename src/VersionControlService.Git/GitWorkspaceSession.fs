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
module GitRefs = VersionControlService.Git.GitRefs
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
let private runGitEnv
    (hooks: GitSessionHooks)
    (repoPath: string)
    (arguments: string[])
    (stdinData: string option)
    (environment: (string * string)[])
    (context: OperationContext)
    : Async<Result<NodeProcess.ProcessOutput, OperationFailure>> =
    async {
        let request = {
            NodeProcess.ProcessRequest.create "git" arguments with
                WorkingDirectory = Some repoPath
                StdinData = stdinData
                Environment = environment
                ProgressPhase = "git"
        }

        let runner = hooks.RunProcess |> Option.defaultValue NodeProcess.run
        let! result = runner request context

        match result with
        | Succeeded outcome -> return Ok outcome.Value
        | PartiallySucceeded(_, failure)
        | Failed failure -> return Error failure
    }

let private runGit hooks repoPath arguments stdinData context =
    runGitEnv hooks repoPath arguments stdinData [||] context

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

/// Cooperative in-process mutation lock: JS is single-threaded, so a busy flag
/// with async polling serializes session mutations without blocking reads.
type MutationLock() =
    let mutable busy = false

    member _.Acquire() : Async<unit> =
        async {
            while busy do
                do! Async.Sleep 5

            busy <- true
        }

    member _.Release() = busy <- false

/// Session-internal mutable state shared by all operations of one open session.
type private SessionState = {
    RepoPath: string
    Hooks: GitSessionHooks
    Lock: MutationLock
}

// ---------------------------------------------------------------------------
// Status and workspace version
// ---------------------------------------------------------------------------

let private stableHash (text: string) =
    let mutable hash = 5381

    for character in text do
        hash <- ((hash <<< 5) + hash + int character) &&& 0x7FFFFFFF

    hash

/// Stable opaque workspace-version token derived from HEAD, an identity over
/// index/worktree state, and the active merge/conflict state. Pure reads over an
/// unchanged workspace return the same token; the token guards in-process,
/// per-session races only — external processes can invalidate it at any time,
/// which is why mutations revalidate it under the session lock.
let private computeWorkspaceVersion (state: SessionState) (context: OperationContext) : Async<string> =
    async {
        let! headOutput =
            runGit state.Hooks state.RepoPath [| "rev-parse"; "--verify"; "--quiet"; "HEAD" |] None context

        let headPart =
            match headOutput with
            | Ok output when output.ExitCode = 0 -> output.StdOut.Trim()
            | _ -> "unborn"

        let! statusOutput =
            runGit
                state.Hooks
                state.RepoPath
                [|
                    "status"
                    "--porcelain=v2"
                    "-z"
                    "--untracked-files=all"
                |]
                None
                context

        let statusPart =
            match statusOutput with
            | Ok output when output.ExitCode = 0 -> stableHash output.StdOut
            | _ -> -1

        let! mergeHeadOutput =
            runGit state.Hooks state.RepoPath [| "rev-parse"; "--git-path"; "MERGE_HEAD" |] None context

        let mergePart =
            match mergeHeadOutput with
            | Ok output when output.ExitCode = 0 ->
                let mergeHeadPath = output.StdOut.Trim()

                let resolvedPath =
                    if
                        mergeHeadPath.StartsWith "/"
                        || (mergeHeadPath.Length >= 2 && mergeHeadPath[1] = ':')
                    then
                        mergeHeadPath
                    else
                        NodePath.join [| state.RepoPath; mergeHeadPath |]

                if NodeFileSystem.existsSync resolvedPath then "merge" else "none"
            | _ -> "none"

        return $"git:{headPart}:{statusPart}:{mergePart}"
    }

/// Serializes a mutation and revalidates the expected workspace version under the
/// lock, so a stale request never reaches provider state.
let private withValidatedMutation
    (state: SessionState)
    (expectedVersion: string)
    (context: OperationContext)
    (body: unit -> Async<OperationResult<'T>>)
    : Async<OperationResult<'T>> =
    async {
        do! state.Lock.Acquire()

        try
            let! currentVersion = computeWorkspaceVersion state context

            if currentVersion <> expectedVersion then
                return
                    Failed(
                        OperationFailure.create
                            Concurrency
                            "precondition_failed"
                            "The expected workspace version is stale; refresh status and retry."
                    )
            else
                return! body ()
        finally
            state.Lock.Release()
    }

let private workspaceVersion (state: SessionState) (_status: GitStatusDto) : Async<string> =
    computeWorkspaceVersion state (OperationContext.detached "workspace-version")

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

/// Resolves a repository state path (MERGE_HEAD, ...) through Git itself so
/// linked worktrees report the right location instead of `<root>/.git/<name>`.
let private resolveGitStatePath (state: SessionState) (name: string) (context: OperationContext) =
    async {
        let! output = runGit state.Hooks state.RepoPath [| "rev-parse"; "--git-path"; name |] None context

        match output with
        | Ok result when result.ExitCode = 0 ->
            let rawPath = result.StdOut.Trim()

            let absolutePath =
                if rawPath.StartsWith "/" || (rawPath.Length >= 2 && rawPath[1] = ':') then
                    rawPath
                else
                    NodePath.join [| state.RepoPath; rawPath |]

            return Some absolutePath
        | _ -> return None
    }

/// Active merge state as a provider-managed conflict-session summary. Present
/// whenever MERGE_HEAD exists — including after the last conflict was staged.
let private getMergeConflictSummary (state: SessionState) (context: OperationContext) =
    async {
        let! mergeHeadPath = resolveGitStatePath state "MERGE_HEAD" context

        match mergeHeadPath with
        | Some path when NodeFileSystem.existsSync path ->
            let mergeHead =
                (NodeFileSystem.readFileSync path NodeFileSystem.TextEncoding.Utf8).Trim()

            let! unmergedOutput =
                runGit
                    state.Hooks
                    state.RepoPath
                    [|
                        "diff"
                        "--name-only"
                        "--diff-filter=U"
                        "-z"
                    |]
                    None
                    context

            let unmergedPaths =
                match unmergedOutput with
                | Ok result when result.ExitCode = 0 ->
                    result.StdOut.Split '\000'
                    |> Array.filter (fun entry -> entry <> "")
                    |> Array.choose (tryCreateRepositoryPath >> Result.toOption)
                | _ -> [||]

            let items =
                unmergedPaths
                |> Array.map (fun conflictPath -> {
                    Path = conflictPath
                    Candidates = [|
                        {
                            CandidateId = "workspace"
                            Label = "Workspace version"
                            Revision = None
                            Preview = None
                        }
                        {
                            CandidateId = "target"
                            Label = "Target version"
                            Revision = Some(mkRevisionId mergeHead)
                            Preview = None
                        }
                        {
                            CandidateId = "base"
                            Label = "Base version"
                            Revision = None
                            Preview = None
                        }
                    |]
                    SupportsResolvedContent = true
                })

            return
                Some {
                    Handle = {
                        SessionId = $"merge-{mergeHead}"
                        Version = "1"
                    }
                    Items = items
                }
        | _ -> return None
    }

let private getWorkspaceStatus (state: SessionState) (context: OperationContext) =
    async {
        let! statusResult = awaitGit (GitService.getStatus state.RepoPath)

        match statusResult with
        | Error failure -> return Failed failure
        | Ok status ->
            let! workspaceStatus = toWorkspaceStatus state status context
            let! conflictSummary = getMergeConflictSummary state context

            // Submodule-internal changes are never workspace changes: entries at or
            // under a gitlink root are filtered from the reported change list.
            let selectedRevisionRunner: GitSelectedRevision.GitRunner =
                fun arguments stdinData environment ->
                    runGitEnv state.Hooks state.RepoPath arguments stdinData environment context

            let! gitlinkRootsResult = GitSelectedRevision.listGitlinkRoots selectedRevisionRunner

            let gitlinkRoots =
                match gitlinkRootsResult with
                | Ok roots -> roots
                | Error _ -> [||]

            let filteredChanges =
                workspaceStatus.Changes
                |> Array.filter (fun change ->
                    let pathValue = RepositoryPath.value change.Path

                    not (
                        gitlinkRoots
                        |> Array.exists (fun root -> pathValue = root || pathValue.StartsWith(root + "/"))
                    ))

            return
                OperationResult.succeeded {
                    workspaceStatus with
                        Changes = filteredChanges
                        ActiveConflictSession = conflictSummary
                }
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
            // Deterministic interruption point before the transaction starts; the
            // canceled state is then observed by the process adapter before any
            // git command runs.
            do! barrier state.Hooks state.RepoPath "transfer-start" context

            if context.Cancellation.IsCancellationRequested() then
                return OperationResult.canceled "The selected revision was canceled before the transaction started."
            else

            // Isolated literal transaction: temporary index, commit-tree, and a
            // compare-and-swap ref update. Selected paths are exact literal names
            // over NUL stdin (spike 3: direct process adapter).
            let runner: GitSelectedRevision.GitRunner =
                fun arguments stdinData environment ->
                    async {
                        let processRequest = {
                            NodeProcess.ProcessRequest.create "git" arguments with
                                WorkingDirectory = Some state.RepoPath
                                StdinData = stdinData
                                Environment = environment
                                ProgressPhase = "git"
                        }

                        let runProcess = state.Hooks.RunProcess |> Option.defaultValue NodeProcess.run
                        let! result = runProcess processRequest context

                        match result with
                        | Succeeded outcome -> return Ok outcome.Value
                        | PartiallySucceeded(_, failure)
                        | Failed failure -> return Error failure
                    }

            let transactionBarrier point = barrier state.Hooks state.RepoPath point context

            return!
                GitSelectedRevision.createRevision
                    runner
                    transactionBarrier
                    state.RepoPath
                    request.Message
                    request.Paths
                    context
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
        let refRunner: GitRefs.GitRunner =
            fun arguments stdinData -> runGit state.Hooks state.RepoPath arguments stdinData context

        let! nameValidation = GitRefs.validateBranchName refRunner request.Name

        match nameValidation with
        | Error failure -> return Failed failure
        | Ok _ ->

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
                runGitChecked
                    state.Hooks
                    state.RepoPath
                    [|
                        "diff"
                        "--name-only"
                        "-z"
                        "HEAD"
                        targetName
                    |]
                    None
                    context

            match changedBetween with
            | Error failure -> return Failed failure
            | Ok diffOutput ->
                let differingPaths =
                    diffOutput.StdOut.Split '\000'
                    |> Array.filter (fun entry -> entry <> "")
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

[<Emit("process.platform")>]
let private nodePlatform: string = jsNative

[<Emit("$0.normalize('NFC')")>]
let private normalizeNfc (_text: string) : string = jsNative

let private windowsReservedBaseNames =
    set [
        "con"
        "prn"
        "aux"
        "nul"
        "com1"
        "com2"
        "com3"
        "com4"
        "com5"
        "com6"
        "com7"
        "com8"
        "com9"
        "lpt1"
        "lpt2"
        "lpt3"
        "lpt4"
        "lpt5"
        "lpt6"
        "lpt7"
        "lpt8"
        "lpt9"
    ]

let private isWindowsInvalidName (path: string) =
    path.Split '/'
    |> Array.exists (fun segment ->
        let baseName = (segment.Split '.').[0].ToLowerInvariant()

        windowsReservedBaseNames.Contains baseName
        || segment.EndsWith "."
        || segment.EndsWith " "
        || segment |> Seq.exists (fun character -> int character < 32 || "<>:\"|?*".Contains(string character)))

/// Guards materialization: provider keys are byte-exact, but the local filesystem
/// may alias case-different (Windows, macOS) or normalization-equivalent (macOS)
/// names, and Windows rejects reserved/invalid names. Detected problems are
/// structured failures attributed to the affected paths — never silent overwrites
/// or thrown exceptions.
let private checkMaterializationSafety
    (state: SessionState)
    (targetName: string)
    (context: OperationContext)
    : Async<Result<unit, OperationFailure>> =
    async {
        let! treeOutput =
            runGit
                state.Hooks
                state.RepoPath
                [|
                    "ls-tree"
                    "-r"
                    "--name-only"
                    "-z"
                    targetName
                |]
                None
                context

        match treeOutput with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "git_failure"
                        $"Listing the target tree failed: {output.StdErr}"
                )
        | Ok output ->
            let paths = output.StdOut.Split '\000' |> Array.filter (fun entry -> entry <> "")

            let aliasKey (path: string) =
                match nodePlatform with
                | "win32" -> path.ToLowerInvariant()
                | "darwin" -> (normalizeNfc path).ToLowerInvariant()
                | _ -> path

            let collisions =
                paths
                |> Array.groupBy aliasKey
                |> Array.filter (fun (_, group) -> group.Length > 1)
                |> Array.collect snd

            if collisions.Length > 0 then
                return
                    Error {
                        OperationFailure.create
                            Validation
                            "path_collision"
                            "Distinct repository paths alias to one local file on this filesystem." with
                            AffectedPaths = collisions
                    }
            elif nodePlatform = "win32" then
                let invalidNames = paths |> Array.filter isWindowsInvalidName

                if invalidNames.Length > 0 then
                    return
                        Error {
                            OperationFailure.create
                                Validation
                                "unrepresentable_path"
                                "A repository path cannot be represented on the local filesystem." with
                                AffectedPaths = invalidNames
                        }
                else
                    return Ok()
            else
                return Ok()
    }

let private switchRef (state: SessionState) (request: SwitchRefRequest) (context: OperationContext) =
    async {
        let refRunner: GitRefs.GitRunner =
            fun arguments stdinData -> runGit state.Hooks state.RepoPath arguments stdinData context

        // Large objects stay as pointers during branch switches (Swate behavior).
        let checkoutEnvironment = [| "GIT_LFS_SKIP_SMUDGE", "1" |]

        let checkout (arguments: string[]) =
            async {
                let! output = runGitEnv state.Hooks state.RepoPath arguments None checkoutEnvironment context

                match output with
                | Error failure -> return Error failure
                | Ok result when result.ExitCode <> 0 ->
                    return
                        Error(
                            OperationFailure.createRedacted
                                ProviderError
                                "checkout_failed"
                                $"Switching refs failed: {result.StdErr}"
                        )
                | Ok _ -> return Ok()
            }

        let localBranchExists (name: string) =
            async {
                let! output =
                    runGit
                        state.Hooks
                        state.RepoPath
                        [|
                            "rev-parse"
                            "--verify"
                            "--quiet"
                            $"refs/heads/{name}"
                        |]
                        None
                        context

                match output with
                | Ok result -> return result.ExitCode = 0
                | Error _ -> return false
            }

        match refNameOfProviderRef request.TargetRef with
        | Choice2Of2 remoteRef ->
            // The exact selected remote ref is preserved: the local branch tracks
            // precisely the requested remote, never a silently substituted origin.
            let localName =
                match remoteRef.Split '/' with
                | segments when segments.Length >= 2 -> String.Join("/", segments |> Array.skip 1)
                | _ -> remoteRef

            let! nameValidation = GitRefs.validateBranchName refRunner localName

            match nameValidation with
            | Error failure -> return Failed failure
            | Ok _ ->
                let! materializationSafety = checkMaterializationSafety state remoteRef context

                match materializationSafety with
                | Error failure -> return Failed failure
                | Ok() ->
                    let! exists = localBranchExists localName

                    let! checkoutResult =
                        if exists then
                            checkout [| "checkout"; localName |]
                        else
                            checkout [|
                                "checkout"
                                "-b"
                                localName
                                "--track"
                                remoteRef
                            |]

                    match checkoutResult with
                    | Error failure -> return Failed failure
                    | Ok() -> return! getWorkspaceStatus state context
        | Choice1Of2 localName ->
            let! nameValidation = GitRefs.validateBranchName refRunner localName

            match nameValidation with
            | Error failure -> return Failed failure
            | Ok _ ->
                let! exists = localBranchExists localName

                if not exists then
                    return
                        Failed(
                            OperationFailure.create NotFound "ref_not_found" $"Ref '{localName}' does not exist."
                        )
                else
                    let! materializationSafety = checkMaterializationSafety state localName context

                    match materializationSafety with
                    | Error failure -> return Failed failure
                    | Ok() ->
                        let! checkoutResult = checkout [| "checkout"; localName |]

                        match checkoutResult with
                        | Error failure -> return Failed failure
                        | Ok() -> return! getWorkspaceStatus state context
    }

/// Parses `git diff --name-status -z` output: NUL-delimited tokens of
/// (status, path) pairs, with renames carrying (status, oldPath, newPath).
let private parseNameStatusZ (output: string) : DiffEntry[] =
    let tokens = output.Split '\000' |> Array.filter (fun token -> token <> "")
    let entries = ResizeArray<DiffEntry>()
    let mutable index = 0

    while index < tokens.Length - 1 do
        let status = tokens[index]

        if status.StartsWith "R" || status.StartsWith "C" then
            if index + 2 < tokens.Length then
                match tryCreateRepositoryPath tokens[index + 2], tryCreateRepositoryPath tokens[index + 1] with
                | Ok newPath, oldPath ->
                    entries.Add {
                        Path = newPath
                        OldPath = oldPath |> Result.toOption
                        Kind = RenamedChange
                        LineInsertions = None
                        LineDeletions = None
                    }
                | Error _, _ -> ()

            index <- index + 3
        else
            match tryCreateRepositoryPath tokens[index + 1] with
            | Ok path ->
                let kind =
                    match status with
                    | "A" -> AddedChange
                    | "D" -> DeletedChange
                    | "U" -> ConflictedChange
                    | _ -> ModifiedChange

                entries.Add {
                    Path = path
                    OldPath = None
                    Kind = kind
                    LineInsertions = None
                    LineDeletions = None
                }
            | Error _ -> ()

            index <- index + 2

    entries.ToArray()

let private getDiffSummary (state: SessionState) (context: OperationContext) =
    async {
        // The object-level summary is the union of staged (index vs HEAD) and
        // unstaged (worktree vs index) changes, each path counted once. Line
        // counts stay optional.
        let! unstagedResult =
            runGitChecked state.Hooks state.RepoPath [| "diff"; "--name-status"; "-z" |] None context

        match unstagedResult with
        | Error failure -> return Failed failure
        | Ok unstagedOutput ->
            let! stagedResult =
                runGit state.Hooks state.RepoPath [| "diff"; "--cached"; "--name-status"; "-z" |] None context

            let stagedEntries =
                match stagedResult with
                // `diff --cached` fails on an unborn branch; treat that as no staged diff.
                | Ok output when output.ExitCode = 0 -> parseNameStatusZ output.StdOut
                | _ -> [||]

            let unstagedEntries = parseNameStatusZ unstagedOutput.StdOut

            let entries =
                Array.append stagedEntries unstagedEntries
                |> Array.distinctBy (fun entry -> RepositoryPath.value entry.Path)

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
                            runGit
                                state.Hooks
                                state.RepoPath
                                [|
                                    "diff"
                                    "--name-only"
                                    "-z"
                                    mergeBase
                                    target
                                |]
                                None
                                context

                        match diff with
                        | Ok output when output.ExitCode = 0 ->
                            return
                                Some(
                                    output.StdOut.Split '\000'
                                    |> Array.filter (fun entry -> entry <> "")
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

                // Committed-side conflicts between diverged histories via
                // `merge-tree --write-tree` (Git 2.38+): exit code 1 = conflicts.
                let! committedConflicts =
                    match syncState.Relationship, syncState.TargetRevision with
                    | Diverged, Some target ->
                        async {
                            let! mergeTree =
                                runGit
                                    state.Hooks
                                    state.RepoPath
                                    [|
                                        "merge-tree"
                                        "--write-tree"
                                        "HEAD"
                                        RevisionId.value target
                                    |]
                                    None
                                    context

                            match mergeTree with
                            | Ok output -> return output.ExitCode <> 0
                            | Error _ -> return false
                        }
                    | _ -> async { return false }

                return
                    OperationResult.succeeded {
                        ChangedPaths = changed
                        OverlappingPaths = overlapping
                        HasDataLossRisk = overlapping.Length > 0
                        WouldCreateConflictSession = overlapping.Length > 0 || committedConflicts
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
        Lock = MutationLock()
    }

    let core: CoreVersionControl = {
        GetStatus = fun context -> getWorkspaceStatus state context
        ListRefs = fun context -> listRefs state context
        CreateRef =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion context (fun () ->
                    createRef state request context)
        PreflightSwitchRef = fun request context -> preflightSwitchRef state request context
        SwitchRef =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion context (fun () ->
                    switchRef state request context)
        CreateRevision =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion context (fun () ->
                    createRevision state request context)
        RestorePaths =
            fun request context ->
                withValidatedMutation state request.ExpectedWorkspaceVersion context (fun () ->
                    restorePaths state request context)
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
                    Update =
                        fun request context ->
                            withValidatedMutation state request.ExpectedWorkspaceVersion context (fun () ->
                                update state request context)
                    Publish =
                        fun request context ->
                            withValidatedMutation state request.ExpectedWorkspaceVersion context (fun () ->
                                publish state request context)
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
                let versionText = output.StdOut.Trim()

                // Synchronization preview uses `merge-tree --write-tree`,
                // documented from Git 2.38 — older versions are incompatible.
                let versionMatch =
                    System.Text.RegularExpressions.Regex.Match(versionText, @"(\d+)\.(\d+)")

                let compatible =
                    versionMatch.Success
                    && (let major = int versionMatch.Groups[1].Value
                        let minor = int versionMatch.Groups[2].Value
                        major > 2 || (major = 2 && minor >= 38))

                return
                    OperationResult.succeeded [|
                        {
                            Component = "git"
                            Installed = true
                            Version = Some versionText
                            Compatible = compatible
                            Remediation =
                                if compatible then
                                    None
                                else
                                    Some
                                        "Install Git 2.38 or newer: synchronization preview requires `git merge-tree --write-tree`."
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
