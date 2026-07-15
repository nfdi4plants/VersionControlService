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
module GitConflictSession = VersionControlService.Git.GitConflictSession
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module GitLfsExtensions = VersionControlService.Git.GitLfsExtensions
module GitProvisioningService = VersionControlService.Git.GitProvisioningService
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeInterop = VersionControlService.Runtime.Node.Interop

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
    /// The bound repository location and its connection profile.
    Location: RepositoryLocation
    ConnectionProfileId: string option
    /// Injected credential resolution; never global state.
    Credentials: GitCredentialStrategy.GitCredentialStrategy
    /// Active conflict-session identity and rotating handle version.
    mutable ConflictSession: (string * int) option
    /// Monotonic counter so re-opened merges never reuse a closed session ID.
    mutable ConflictGeneration: int
}

/// Scoped credential `-c` arguments for this session's remote host, resolved
/// through the injected strategy (empty for anonymous/SSH/local flows).
let private credentialArguments (state: SessionState) : Async<string[]> =
    GitCredentialStrategy.resolveAuthArguments
        state.Credentials
        state.Location.ProviderLocation
        state.ConnectionProfileId

// ---------------------------------------------------------------------------
// Status and workspace version
// ---------------------------------------------------------------------------

let private stableHash (text: string) =
    let mutable hash = 5381

    for character in text do
        hash <- ((hash <<< 5) + hash + int character) &&& 0x7FFFFFFF

    hash

let private unsafeWorkspacePathFailure (path: string) =
    {
        OperationFailure.create
            Validation
            "unsafe_workspace_path"
            $"The unmerged path '{path}' traverses a symbolic link or reparse point." with
            AffectedPaths = [| path |]
    }

let private workspaceEvidenceFailure (path: string) (message: string) =
    {
        OperationFailure.createRedacted ProviderError "workspace_evidence_unavailable" message with
            AffectedPaths = [| path |]
    }

let private validateContainedWorktreeFile (state: SessionState) (path: string) =
    async {
        let repositoryRoot = NodePath.resolve [| state.RepoPath |]
        let absolutePath = NodePath.resolve [| repositoryRoot; path |]
        let relativePath = NodePath.relative repositoryRoot absolutePath

        if
            String.IsNullOrEmpty relativePath
            || relativePath = "."
            || relativePath = ".."
            || relativePath.StartsWith("../", StringComparison.Ordinal)
            || relativePath.StartsWith("..\\", StringComparison.Ordinal)
            || NodePath.isAbsolute relativePath
        then
            return Error(unsafeWorkspacePathFailure path)
        else
            let segments = path.Replace("\\", "/").Split('/')
            let mutable currentPath = repositoryRoot
            let mutable validationFailure = None
            let mutable index = 0

            while validationFailure.IsNone && index < segments.Length do
                currentPath <- NodePath.join [| currentPath; segments[index] |]

                try
                    let! stats = NodeFileSystem.lstatAsync currentPath |> Async.AwaitPromise

                    if stats.isSymbolicLink () then
                        validationFailure <- Some(unsafeWorkspacePathFailure path)
                    elif index < segments.Length - 1 && not (stats.isDirectory ()) then
                        validationFailure <-
                            Some(
                                workspaceEvidenceFailure
                                    path
                                    $"A parent of the unmerged path '{path}' is not a directory."
                            )
                    elif index = segments.Length - 1 && not (stats.isFile ()) then
                        validationFailure <-
                            Some(
                                workspaceEvidenceFailure path $"The unmerged path '{path}' is not a regular file."
                            )
                with error ->
                    validationFailure <-
                        Some(
                            workspaceEvidenceFailure
                                path
                                $"The unmerged path '{path}' could not be inspected: {error.Message}"
                        )

                index <- index + 1

            match validationFailure with
            | Some failure -> return Error failure
            | None -> return Ok absolutePath
    }

let private hashUnmergedWorktreePath
    (state: SessionState)
    (path: string)
    (context: OperationContext)
    : Async<Result<string, OperationFailure>> =
    async {
        let! containment = validateContainedWorktreeFile state path

        match containment with
        | Error failure -> return Error failure
        | Ok _ ->
            let! hashResult =
                runGit
                    state.Hooks
                    state.RepoPath
                    [| "hash-object"; "--no-filters"; "--"; path |]
                    None
                    context

            match hashResult with
            | Error failure -> return Error failure
            | Ok output when output.ExitCode = 0 -> return Ok($"{path}:{output.StdOut.Trim()}")
            | Ok output ->
                return
                    Error(
                        workspaceEvidenceFailure
                            path
                            $"Hashing the unmerged path '{path}' failed: {output.StdErr}"
                    )
    }

let private computeUnmergedContentPart (state: SessionState) (context: OperationContext) =
    async {
        let! unmergedResult =
            runGit
                state.Hooks
                state.RepoPath
                [| "diff"; "--name-only"; "--diff-filter=U"; "-z" |]
                None
                context

        match unmergedResult with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "git_failure"
                        $"Listing unmerged paths for workspace evidence failed: {output.StdErr}"
                )
        | Ok output ->
            let paths =
                output.StdOut.Split '\000'
                |> Array.filter (fun path -> not (String.IsNullOrEmpty path))

            let rec collect index evidence =
                async {
                    if index >= paths.Length then
                        return Ok(stableHash (String.concat "\000" (List.rev evidence)))
                    else
                        let! hashed = hashUnmergedWorktreePath state paths[index] context

                        match hashed with
                        | Error failure -> return Error failure
                        | Ok item -> return! collect (index + 1) (item :: evidence)
                }

            return! collect 0 []
    }

/// Stable opaque workspace-version token derived from HEAD, an identity over
/// index/worktree state, and the active merge/conflict state. Pure reads over an
/// unchanged workspace return the same token; the token guards in-process,
/// per-session races only — external processes can invalidate it at any time,
/// which is why mutations revalidate it under the session lock.
let private computeWorkspaceVersion
    (state: SessionState)
    (context: OperationContext)
    : Async<Result<string, OperationFailure>> =
    async {
        let! headOutput =
            runGit state.Hooks state.RepoPath [| "rev-parse"; "--verify"; "--quiet"; "HEAD" |] None context

        match headOutput with
        | Error failure -> return Error failure
        | Ok headResult ->
            let headPart =
                if headResult.ExitCode = 0 then headResult.StdOut.Trim() else "unborn"

            let! statusOutput =
                runGit
                    state.Hooks
                    state.RepoPath
                    [| "status"; "--porcelain=v2"; "-z"; "--untracked-files=all" |]
                    None
                    context

            match statusOutput with
            | Error failure -> return Error failure
            | Ok statusResult when statusResult.ExitCode <> 0 ->
                return
                    Error(
                        OperationFailure.createRedacted
                            ProviderError
                            "git_failure"
                            $"Reading workspace status evidence failed: {statusResult.StdErr}"
                    )
            | Ok statusResult ->
                let statusPart = stableHash statusResult.StdOut
                let! unmergedContentResult = computeUnmergedContentPart state context

                match unmergedContentResult with
                | Error failure -> return Error failure
                | Ok unmergedContentPart ->
                    let! mergeHeadOutput =
                        runGit
                            state.Hooks
                            state.RepoPath
                            [| "rev-parse"; "--git-path"; "MERGE_HEAD" |]
                            None
                            context

                    match mergeHeadOutput with
                    | Error failure -> return Error failure
                    | Ok mergeHeadResult ->
                        let mergePart =
                            if mergeHeadResult.ExitCode = 0 then
                                let mergeHeadPath = mergeHeadResult.StdOut.Trim()

                                let resolvedPath =
                                    if
                                        mergeHeadPath.StartsWith "/"
                                        || (mergeHeadPath.Length >= 2 && mergeHeadPath[1] = ':')
                                    then
                                        mergeHeadPath
                                    else
                                        NodePath.join [| state.RepoPath; mergeHeadPath |]

                                if NodeFileSystem.existsSync resolvedPath then "merge" else "none"
                            else
                                "none"

                        return Ok($"git:{headPart}:{statusPart}:{unmergedContentPart}:{mergePart}")
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
            let! currentVersionResult = computeWorkspaceVersion state context

            match currentVersionResult with
            | Error failure -> return Failed failure
            | Ok currentVersion ->
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

let private toWorkspaceStatus (state: SessionState) (status: GitStatusDto) (context: OperationContext) =
    async {
        let conflictedSet = Set.ofArray status.Conflicted

        let changes =
            status.Files
            |> Array.choose (fun file -> toFileChange (conflictedSet.Contains file.Path) file)

        let! versionResult = computeWorkspaceVersion state context

        let versionFailure, version =
            match versionResult with
            | Error failure -> Some failure, String.Empty
            | Ok value -> None, value

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

        match versionFailure with
        | Some failure -> return Error failure
        | None ->
            return
                Ok {
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
                    ActiveConflictSession = None
                    Synchronization =
                        Some {
                            BaseRevision = None
                            WorkspaceRevision = workspaceRevision
                            TargetRevision = targetRevision
                            TargetRef = None
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

/// The active merge target revision, resolved through Git state paths.
let private tryGetMergeHead (state: SessionState) (context: OperationContext) =
    async {
        let! mergeHeadPath = resolveGitStatePath state "MERGE_HEAD" context

        match mergeHeadPath with
        | Some path when NodeFileSystem.existsSync path ->
            return Some((NodeFileSystem.readFileSync path NodeFileSystem.TextEncoding.Utf8).Trim())
        | _ -> return None
    }

let private conflictRunner (state: SessionState) (context: OperationContext) : GitConflictSession.GitRunner =
    fun arguments stdinData -> runGit state.Hooks state.RepoPath arguments stdinData context

let private readConflictCombinedPreview
    (state: SessionState)
    (path: string)
    : Async<Result<ConflictPreview option, OperationFailure>> =
    async {
        let! containment = validateContainedWorktreeFile state path

        match containment with
        | Error failure -> return Error failure
        | Ok absolutePath ->
            try
                let! buffer = NodeFileSystem.readFileBufferAsync absolutePath |> Async.AwaitPromise

                if GitService.isLikelyBinaryBuffer buffer then
                    return Ok(Some(UnsupportedPreview(Some $"Unsupported git content for '{path}'.")))
                else
                    return Ok(Some(TextPreview(NodeInterop.bufferToUtf8String buffer)))
            with error ->
                return
                    Error(
                        workspaceEvidenceFailure
                            path
                            $"The combined conflict content for '{path}' could not be read: {error.Message}"
                    )
    }

/// Active merge state as a provider-managed conflict session. Present whenever
/// MERGE_HEAD exists — including after the last conflict was staged. The handle
/// version rotates on every successful nonterminal resolution and the session ID
/// changes when a merge is reopened after finalize/cancel.
let private getMergeConflictSummary (state: SessionState) (context: OperationContext) =
    async {
        let! mergeHead = tryGetMergeHead state context

        match mergeHead with
        | None ->
            state.ConflictSession <- None
            return Ok None
        | Some mergeHeadValue ->
            let sessionId, version =
                match state.ConflictSession with
                | Some(sessionId, version) -> sessionId, version
                | None ->
                    state.ConflictGeneration <- state.ConflictGeneration + 1
                    let shortHead = mergeHeadValue.Substring(0, min 8 mergeHeadValue.Length)
                    let created = $"merge-{shortHead}-{state.ConflictGeneration}", 1
                    state.ConflictSession <- Some created
                    created

            let runner = conflictRunner state context
            let! unmergedResult = GitConflictSession.listUnmergedPaths runner

            match unmergedResult with
            | Error failure -> return Error failure
            | Ok unmergedPaths ->
                let! itemsResult =
                    GitConflictSession.buildConflictItems
                        runner
                        (readConflictCombinedPreview state)
                        (Some(mkRevisionId mergeHeadValue))
                        unmergedPaths

                match itemsResult with
                | Error failure -> return Error failure
                | Ok items ->
                    return
                        Ok(
                            Some {
                                Handle = {
                                    SessionId = sessionId
                                    Version = string version
                                }
                                Items = items
                            }
                        )
    }

let private getWorkspaceStatus (state: SessionState) (context: OperationContext) =
    async {
        let! statusResult = awaitGit (GitService.getStatus state.RepoPath)

        match statusResult with
        | Error failure -> return Failed failure
        | Ok status ->
            let! workspaceStatusResult = toWorkspaceStatus state status context

            match workspaceStatusResult with
            | Error failure -> return Failed failure
            | Ok workspaceStatus ->
                let! conflictSummaryResult = getMergeConflictSummary state context

                match conflictSummaryResult with
                | Error failure -> return Failed failure
                | Ok conflictSummary ->
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
                    TargetRef = None
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

            let! authArguments = credentialArguments state

            let! fetchResult =
                runGitChecked state.Hooks state.RepoPath [| yield! authArguments; "fetch"; "origin" |] None context

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

                    let! authArguments = credentialArguments state

                    let! pushResult =
                        runGit
                            state.Hooks
                            state.RepoPath
                            [| yield! authArguments; "push"; "origin"; branch |]
                            None
                            context

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
// Conflict sessions over the real Git merge state
// ---------------------------------------------------------------------------

/// Validates the opaque handle and the independently observed workspace version
/// BEFORE any provider state changes. Stale, foreign, or closed handles are
/// rejected with Concurrency/precondition_failed/refresh_conflict_session.
let private validateConflictHandle
    (state: SessionState)
    (handle: ConflictSessionHandle)
    (expectedWorkspaceVersion: string)
    (context: OperationContext)
    : Async<Result<string, OperationFailure>> =
    async {
        let! mergeHead = tryGetMergeHead state context

        match mergeHead, state.ConflictSession with
        | Some mergeHeadValue, Some(sessionId, version) when
            sessionId = handle.SessionId && string version = handle.Version
            ->
            let! currentVersionResult = computeWorkspaceVersion state context

            match currentVersionResult with
            | Error failure -> return Error failure
            | Ok currentVersion ->
                if currentVersion <> expectedWorkspaceVersion then
                    return Error(GitConflictSession.handleRejection ())
                else
                    return Ok mergeHeadValue
        | _ -> return Error(GitConflictSession.handleRejection ())
    }

let private rotateConflictHandle (state: SessionState) : ConflictSessionHandle =
    match state.ConflictSession with
    | Some(sessionId, version) ->
        state.ConflictSession <- Some(sessionId, version + 1)

        {
            SessionId = sessionId
            Version = string (version + 1)
        }
    | None -> failwith "No conflict session is active."

let private createConflictService (state: SessionState) : ConflictResolutionService =
    let stagePath (path: RepositoryPath) (context: OperationContext) =
        async {
            let payload = GitPathTransport.nulDelimitedLiteralPathspecs [| path |]

            let! result =
                runGit
                    state.Hooks
                    state.RepoPath
                    [| "add"; yield! GitPathTransport.pathspecFromStdinArguments |]
                    (Some payload)
                    context

            match result with
            | Ok output when output.ExitCode = 0 -> return Ok()
            | Ok output ->
                return
                    Error(
                        OperationFailure.createRedacted
                            ProviderError
                            "git_failure"
                            $"Staging the resolved path failed: {output.StdErr}"
                    )
            | Error failure -> return Error failure
        }

    {
        GetActiveSession =
            fun context -> async {
                let! summaryResult = getMergeConflictSummary state context

                match summaryResult with
                | Error failure -> return Failed failure
                | Ok summary -> return OperationResult.succeeded summary
            }
        Resolve =
            fun request context -> async {
                let! validation = validateConflictHandle state request.Handle request.ExpectedWorkspaceVersion context

                match validation with
                | Error failure -> return Failed failure
                | Ok _ ->
                    let runner = conflictRunner state context
                    let! unmergedResult = GitConflictSession.listUnmergedPaths runner

                    match unmergedResult with
                    | Error failure -> return Failed failure
                    | Ok unmergedPaths ->
                        let pathValue = RepositoryPath.value request.Path

                        if not (unmergedPaths |> Array.contains pathValue) then
                            return
                                Failed(
                                    OperationFailure.create
                                        NotFound
                                        "conflict_item_not_found"
                                        $"No unresolved conflict exists for the selected path."
                                )
                        else
                            let resolveContent () =
                                async {
                                    match request.Resolution with
                                    | SupplyResolvedContent content -> return Ok(Some content)
                                    | PickCandidate "workspace" ->
                                        let! content = GitConflictSession.readStageContent runner 2 pathValue
                                        return Ok content
                                    | PickCandidate "target" ->
                                        let! content = GitConflictSession.readStageContent runner 3 pathValue
                                        return Ok content
                                    | PickCandidate "base" ->
                                        let! content = GitConflictSession.readStageContent runner 1 pathValue
                                        return Ok content
                                    | PickCandidate _ ->
                                        return
                                            Error(
                                                OperationFailure.create
                                                    Validation
                                                    "unknown_candidate"
                                                    "The candidate ID is not part of this conflict item."
                                            )
                                }

                            let! contentResult = resolveContent ()

                            match contentResult with
                            | Error failure -> return Failed failure
                            | Ok None ->
                                return
                                    Failed(
                                        OperationFailure.create
                                            Validation
                                            "candidate_content_unavailable"
                                            "The selected candidate has no content for this path."
                                    )
                            | Ok(Some content) ->
                                let absolutePath = NodePath.join [| state.RepoPath; pathValue |]
                                NodeFileSystem.writeFileSync absolutePath content NodeFileSystem.TextEncoding.Utf8

                                let! staged = stagePath request.Path context

                                match staged with
                                | Error failure -> return Failed failure
                                | Ok() ->
                                    let refreshedHandle = rotateConflictHandle state
                                    let! summaryResult = getMergeConflictSummary state context

                                    match summaryResult with
                                    | Error failure -> return Failed failure
                                    | Ok summary ->
                                        return
                                            OperationResult.succeeded {
                                                RefreshedHandle = refreshedHandle
                                                RemainingItems =
                                                    summary
                                                    |> Option.map _.Items
                                                    |> Option.defaultValue [||]
                                            }
            }
        Finalize =
            fun request context -> async {
                let! validation = validateConflictHandle state request.Handle request.ExpectedWorkspaceVersion context

                match validation with
                | Error failure -> return Failed failure
                | Ok mergeHead ->
                    let runner = conflictRunner state context
                    let! unmergedResult = GitConflictSession.listUnmergedPaths runner

                    match unmergedResult with
                    | Error failure -> return Failed failure
                    | Ok unmergedPaths when unmergedPaths.Length > 0 ->
                        return
                            Failed {
                                OperationFailure.create
                                    Validation
                                    "conflicts_unresolved"
                                    "Every conflict item must be resolved before finalizing." with
                                    AffectedPaths = unmergedPaths
                            }
                    | Ok _ ->
                        // Pre-check the destination head, then commit under a
                        // compare-and-swap ref update.
                        let! branchOutput =
                            runGit state.Hooks state.RepoPath [| "symbolic-ref"; "-q"; "HEAD" |] None context

                        match branchOutput with
                        | Error failure -> return Failed failure
                        | Ok branchResult when branchResult.ExitCode <> 0 ->
                            return
                                Failed(
                                    OperationFailure.create
                                        Validation
                                        "detached_head"
                                        "Finalizing requires a current branch."
                                )
                        | Ok branchResult ->
                            let branchRef = branchResult.StdOut.Trim()
                            let! expectedHead = revParse state "HEAD" context

                            match expectedHead with
                            | None ->
                                return
                                    Failed(
                                        OperationFailure.create
                                            ProviderError
                                            "git_failure"
                                            "The current head could not be resolved."
                                    )
                            | Some expectedHeadValue ->
                                do! barrier state.Hooks state.RepoPath "finalize-precheck-done" context

                                let! treeResult =
                                    runGitChecked state.Hooks state.RepoPath [| "write-tree" |] None context

                                match treeResult with
                                | Error failure -> return Failed failure
                                | Ok treeOutput ->
                                    let message =
                                        request.Message |> Option.defaultValue "merge: finalize conflict session"

                                    let! commitResult =
                                        runGitChecked
                                            state.Hooks
                                            state.RepoPath
                                            [|
                                                "commit-tree"
                                                treeOutput.StdOut.Trim()
                                                "-p"
                                                expectedHeadValue
                                                "-p"
                                                mergeHead
                                                "-m"
                                                message
                                            |]
                                            None
                                            context

                                    match commitResult with
                                    | Error failure -> return Failed failure
                                    | Ok commitOutput ->
                                        let newCommit = commitOutput.StdOut.Trim()

                                        let! updateResult =
                                            runGit
                                                state.Hooks
                                                state.RepoPath
                                                [|
                                                    "update-ref"
                                                    branchRef
                                                    newCommit
                                                    expectedHeadValue
                                                |]
                                                None
                                                context

                                        match updateResult with
                                        | Error failure -> return Failed failure
                                        | Ok updateOutput when updateOutput.ExitCode <> 0 ->
                                            // The destination advanced between the
                                            // pre-check and the ref update: report the
                                            // race with revision evidence; the session
                                            // stays live for refresh-and-retry.
                                            let! observedHead = revParse state "HEAD" context

                                            return
                                                Failed {
                                                    OperationFailure.create
                                                        Concurrency
                                                        "precondition_failed"
                                                        "The destination advanced between the finalize pre-check and verification." with
                                                        RevisionEvidence = [|
                                                            "expected_destination", mkRevisionId expectedHeadValue
                                                            yield!
                                                                observedHead
                                                                |> Option.map (fun value ->
                                                                    "observed_destination", mkRevisionId value)
                                                                |> Option.toList
                                                        |]
                                                        RecoveryAction =
                                                            Some {
                                                                Code = ConflictRecovery.RefreshConflictSession
                                                                Instructions =
                                                                    Some
                                                                        "Refresh the conflict session and deliberately retry."
                                                            }
                                                }
                                        | Ok _ ->
                                            // Close the merge state and the session.
                                            for stateFile in [ "MERGE_HEAD"; "MERGE_MSG"; "MERGE_MODE" ] do
                                                let! statePath = resolveGitStatePath state stateFile context

                                                match statePath with
                                                | Some path when NodeFileSystem.existsSync path ->
                                                    try
                                                        NodeFileSystem.unlinkSync path
                                                    with _ ->
                                                        ()
                                                | _ -> ()

                                            state.ConflictSession <- None
                                            return OperationResult.succeeded (Some(mkRevisionId newCommit))
            }
        Cancel =
            fun request context -> async {
                let! validation = validateConflictHandle state request.Handle request.ExpectedWorkspaceVersion context

                match validation with
                | Error failure -> return Failed failure
                | Ok _ ->
                    let! abortResult =
                        runGitChecked state.Hooks state.RepoPath [| "merge"; "--abort" |] None context

                    match abortResult with
                    | Error failure -> return Failed failure
                    | Ok _ ->
                        state.ConflictSession <- None
                        return OperationResult.succeeded ()
            }
    }

// ---------------------------------------------------------------------------
// Optional extensions available from the shell
// ---------------------------------------------------------------------------

let private validateLiteralRepositoryPath (path: string) =
    let hasDrivePrefix = path.Length >= 2 && Char.IsLetter path[0] && path[1] = ':'

    if String.IsNullOrEmpty path then
        Error(OperationFailure.create Validation "invalid_path" "The repository path must not be empty.")
    elif path.StartsWith "/" || hasDrivePrefix then
        Error(OperationFailure.create Validation "invalid_path" "Absolute repository paths are not allowed.")
    elif path.Contains '\000' then
        Error(OperationFailure.create Validation "invalid_path" "Repository paths must not contain null bytes.")
    elif path.Split('/') |> Array.exists (fun segment -> segment = "." || segment = "..") then
        Error(OperationFailure.create Validation "invalid_path" "Repository paths must not contain traversal segments.")
    else
        Ok path

let private tryPorcelainV2RenamePath (record: string) =
    let mutable separatorCount = 0
    let mutable index = 0

    while separatorCount < 9 && index < record.Length do
        if record[index] = ' ' then
            separatorCount <- separatorCount + 1

        index <- index + 1

    if separatorCount = 9 then Some(record.Substring index) else None

let private findBasePathFromStatus (requestedPath: string) (statusText: string) =
    let records = statusText.Split '\000'
    let mutable index = 0
    let mutable basePath = requestedPath
    let mutable found = false

    while not found && index < records.Length do
        let record = records[index]

        if record.StartsWith("2 ", StringComparison.Ordinal) then
            match tryPorcelainV2RenamePath record with
            | Some renamedPath when
                index + 1 < records.Length
                && String.Equals(renamedPath, requestedPath, StringComparison.Ordinal)
                ->
                basePath <- records[index + 1]
                found <- true
            | _ -> index <- index + 2
        else
            index <- index + 1

    basePath

let private getBaseContent
    (state: SessionState)
    (path: RepositoryPath)
    (context: OperationContext)
    : Async<OperationResult<ContentView>> =
    async {
        let requestedPath = RepositoryPath.value path

        match validateLiteralRepositoryPath requestedPath with
        | Error failure -> return Failed failure
        | Ok literalPath ->
            let! statusResult =
                runGit
                    state.Hooks
                    state.RepoPath
                    [| "status"; "--porcelain=v2"; "-z"; "--untracked-files=all" |]
                    None
                    context

            match statusResult with
            | Error failure -> return Failed failure
            | Ok statusOutput when statusOutput.ExitCode <> 0 ->
                return
                    Failed(
                        OperationFailure.createRedacted
                            ProviderError
                            "git_failure"
                            $"Reading exact Git status for base content failed: {statusOutput.StdErr}"
                    )
            | Ok statusOutput ->
                let basePath = findBasePathFromStatus literalPath statusOutput.StdOut

                let comparedPaths = [|
                    yield literalPath

                    if not (String.Equals(basePath, literalPath, StringComparison.Ordinal)) then
                        yield basePath
                |]

                let! binaryResult =
                    runGit
                        state.Hooks
                        state.RepoPath
                        [|
                            "--literal-pathspecs"
                            "diff"
                            "--numstat"
                            "--no-ext-diff"
                            "--no-textconv"
                            "--find-renames"
                            "HEAD"
                            "--"
                            yield! comparedPaths
                        |]
                        None
                        context

                match binaryResult with
                | Error failure -> return Failed failure
                | Ok binaryOutput when binaryOutput.ExitCode <> 0 ->
                    return
                        Failed(
                            OperationFailure.createRedacted
                                ProviderError
                                "git_failure"
                                $"Classifying Git base content failed: {binaryOutput.StdErr}"
                        )
                | Ok binaryOutput when
                    binaryOutput.StdOut.Split '\n'
                    |> Array.exists (fun line -> line.StartsWith("-\t-\t", StringComparison.Ordinal))
                    ->
                    return
                        OperationResult.succeeded (
                            UnsupportedContent(Some $"Unsupported git content for '{literalPath}'.")
                        )
                | Ok _ ->
                    let! showResult =
                        runGit state.Hooks state.RepoPath [| "show"; $"HEAD:{basePath}" |] None context

                    match showResult with
                    | Error failure -> return Failed failure
                    | Ok showOutput when showOutput.ExitCode = 0 ->
                        return OperationResult.succeeded (TextContent showOutput.StdOut)
                    | Ok showOutput ->
                        let diagnostic = (showOutput.StdErr + showOutput.StdOut).ToLowerInvariant()

                        if
                            diagnostic.Contains("does not exist in 'head'")
                            || diagnostic.Contains("exists on disk, but not in 'head'")
                            || diagnostic.Contains("invalid object name 'head'")
                            || diagnostic.Contains("bad revision 'head'")
                        then
                            return
                                Failed(
                                    OperationFailure.create
                                        NotFound
                                        "base_content_not_found"
                                        $"The path '{literalPath}' is absent from the committed base."
                                )
                        else
                            return
                                Failed(
                                    OperationFailure.createRedacted
                                        ProviderError
                                        "git_failure"
                                        $"Reading Git base content failed: {showOutput.StdErr}"
                                )
    }

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
        GetBaseContent = fun path context -> getBaseContent state path context
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

let createSessionWithCredentials
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (binding: WorkspaceBinding)
    : WorkspaceSession =
    let state = {
        RepoPath = binding.WorkspaceRoot
        Hooks = hooks
        Lock = MutationLock()
        Location = binding.Location
        ConnectionProfileId = binding.ConnectionProfileId
        Credentials = credentials
        ConflictSession = None
        ConflictGeneration = 0
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
            // Git LFS is optional: the services exist because the Git provider
            // supports them; their operations report their own dependency status
            // when git-lfs is not installed. Core Git never requires LFS.
            ObjectMaterialization = Some(GitLfsExtensions.createObjectMaterialization state.RepoPath)
            StoragePolicy = Some(GitLfsExtensions.createStoragePolicy state.RepoPath)
            Maintenance = Some(GitLfsExtensions.createMaintenance state.RepoPath)
            RepositoryBrowser = Some(createBrowser state)
    }

/// Session with the anonymous credential strategy.
let createSession (hooks: GitSessionHooks) (binding: WorkspaceBinding) : WorkspaceSession =
    createSessionWithCredentials hooks GitCredentialStrategy.anonymous binding

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

let createFactoryWithCredentials
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    : ProviderFactory =
    let checkDependencies (context: OperationContext) : Async<OperationResult<DependencyStatus[]>> =
        async {
            let! gitVersion = runGit hooks "." [| "--version" |] None context

            match gitVersion with
            | Error failure when failure.Category = Canceled -> return Failed failure
            | _ ->
                let! lfsVersion = runGit hooks "." [| "lfs"; "version" |] None context

                match lfsVersion with
                | Error failure when failure.Category = Canceled -> return Failed failure
                | _ ->
                    let! lfsFilter =
                        runGit hooks "." [| "config"; "--global"; "--get"; "filter.lfs.process" |] None context

                    match lfsFilter with
                    | Error failure when failure.Category = Canceled -> return Failed failure
                    | _ ->
                        let gitStatus: DependencyStatus =
                            match gitVersion with
                            | Ok output when output.ExitCode = 0 ->
                                let versionText = output.StdOut.Trim()

                                let compatible =
                                    match GitService.tryParseVersion versionText with
                                    | Some(major, minor, _) -> major > 2 || (major = 2 && minor >= 38)
                                    | None -> false

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
                            | _ ->
                                {
                                    Component = "git"
                                    Installed = false
                                    Version = None
                                    Compatible = false
                                    Remediation = Some "Install Git 2.38 or newer and ensure it is on PATH."
                                }

                        let lfsStatus: DependencyStatus =
                            match lfsVersion with
                            | Ok output when output.ExitCode = 0 ->
                                let versionText = output.StdOut.Trim()

                                let compatible =
                                    match GitLfsService.tryParseVersion versionText with
                                    | Some(major, minor, _) -> major > 3 || (major = 3 && minor >= 7)
                                    | None -> false

                                {
                                    Component = "git-lfs"
                                    Installed = true
                                    Version = Some versionText
                                    Compatible = compatible
                                    Remediation =
                                        if compatible then
                                            None
                                        else
                                            Some "Install a supported Git LFS release and ensure `git lfs version` succeeds."
                                }
                            | _ ->
                                {
                                    Component = "git-lfs"
                                    Installed = false
                                    Version = None
                                    Compatible = false
                                    Remediation = Some "Install Git LFS and ensure `git lfs version` succeeds."
                                }

                        let filterInstalled =
                            match lfsFilter with
                            | Ok output when output.ExitCode = 0 ->
                                output.StdOut.Trim().Equals("git-lfs filter-process", StringComparison.Ordinal)
                            | _ -> false

                        let filterStatus: DependencyStatus =
                            {
                                Component = "git-lfs-configuration"
                                Installed = filterInstalled
                                Version = None
                                Compatible = filterInstalled && lfsStatus.Compatible
                                Remediation =
                                    if filterInstalled && lfsStatus.Compatible then
                                        None
                                    elif not lfsStatus.Installed then
                                        Some "Install Git LFS before configuring its global filter process."
                                    else
                                        Some "Run `git lfs install --skip-repo` to configure Git LFS globally."
                            }

                        return OperationResult.succeeded [| gitStatus; lfsStatus; filterStatus |]
        }

    let manualLfsInstallationRequired () =
        OperationFailure.create
            Unsupported
            "manual_install_required"
            "Install Git LFS 3.7 or newer manually, then rerun dependency checks."

    {
    Id = gitProviderId
    Probe = probe
    VerifyLocation =
        fun request context -> async {
            let! authArguments =
                GitCredentialStrategy.resolveAuthArguments
                    credentials
                    request.Location.ProviderLocation
                    request.Location.ConnectionProfileId

            let! result =
                runGit
                    hooks
                    "."
                    [|
                        yield! authArguments
                        "ls-remote"
                        request.Location.ProviderLocation
                    |]
                    None
                    context

            match result with
            | Ok output when output.ExitCode = 0 ->
                return
                    OperationResult.succeeded {
                        Location = request.Location
                        GrantedIntents = request.Intents
                        DeniedIntents = [||]
                    }
            | Ok output ->
                // Classify the failure so consumers can recover: authentication
                // and authorization problems keep their categories instead of
                // collapsing into a generic network error.
                let category =
                    match GitService.classifyFailureKind output.StdErr with
                    | GitFailureKind.Unauthorized -> Authentication
                    | GitFailureKind.Forbidden -> Authorization
                    | GitFailureKind.Network -> Network
                    | GitFailureKind.Timeout -> Timeout
                    | _ -> NotFound

                return
                    Failed(
                        OperationFailure.createRedacted
                            category
                            "location_unreachable"
                            $"The repository location is not reachable: {output.StdErr}"
                    )
            | Error failure -> return Failed failure
        }
    Initialize =
        fun request context -> async {
            let! result = Async.AwaitPromise(GitProvisioningService.initRepository request.TargetPath)

            match result with
            | Error failure when failure.Message.Contains("already a git repository", StringComparison.OrdinalIgnoreCase) ->
                return
                    Failed(
                        OperationFailure.create
                            Validation
                            "already_initialized"
                            "The target path is already initialized as a Git repository."
                    )
            | Error failure -> return Failed(toOperationFailure failure)
            | Ok normalizedPath ->
                let location =
                    request.Location |> Option.defaultValue (localLocation normalizedPath)

                return OperationResult.succeeded (bindingFor normalizedPath location)
        }
    Clone =
        fun request context -> async {
            let! authArguments =
                GitCredentialStrategy.resolveAuthArguments
                    credentials
                    request.Location.ProviderLocation
                    request.Location.ConnectionProfileId

            // Large objects stay as pointers during the Git transfer; hydration is a
            // separate step so its failure can be reported as partial success.
            let! result =
                runGitEnv
                    hooks
                    "."
                    [|
                        yield! authArguments
                        "clone"
                        request.Location.ProviderLocation
                        request.TargetPath
                    |]
                    None
                    [| "GIT_LFS_SKIP_SMUDGE", "1" |]
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
            | Ok _ ->
                let binding = bindingFor request.TargetPath request.Location

                if not request.MaterializeAllObjects then
                    return OperationResult.succeeded binding
                else
                    // Git transfer succeeded; object hydration failing afterwards is
                    // partial success with a retry action, never an overall error
                    // that hides the changed workspace.
                    let! hydration =
                        runGit
                            hooks
                            request.TargetPath
                            [| yield! authArguments; "lfs"; "pull" |]
                            None
                            context

                    match hydration with
                    | Ok hydrationOutput when hydrationOutput.ExitCode = 0 ->
                        return OperationResult.succeeded binding
                    | Ok hydrationOutput ->
                        return
                            OperationResult.partiallySucceeded
                                (OperationOutcome.performed binding)
                                (OperationFailure.createRedacted
                                    DependencyMissing
                                    "hydration_failed"
                                    $"Large-object hydration failed after a successful clone: {hydrationOutput.StdErr}")
                                {
                                    Code = "retry_materialization"
                                    Instructions =
                                        Some
                                            "Retry downloading large objects once the object store is reachable."
                                }
                    | Error hydrationFailure ->
                        return
                            OperationResult.partiallySucceeded
                                (OperationOutcome.performed binding)
                                hydrationFailure
                                {
                                    Code = "retry_materialization"
                                    Instructions =
                                        Some
                                            "Retry downloading large objects once the object store is reachable."
                                }
        }
    Adopt =
        fun request context ->
            async {
                let! rootResult =
                    runGitChecked hooks request.WorkspaceRoot [| "rev-parse"; "--show-toplevel" |] None context

                match rootResult with
                | Error failure -> return Failed failure
                | Ok rootOutput ->
                    let workspaceRoot = rootOutput.StdOut.Trim()
                    let! remoteResult =
                        runGit hooks workspaceRoot [| "config"; "--get"; "remote.origin.url" |] None context

                    let bind providerLocation =
                        let location = {
                            ProviderId = gitProviderId
                            DisplayName = None
                            ProviderLocation = providerLocation
                            ConnectionProfileId = request.ConnectionProfileId
                        }

                        OperationResult.succeeded (bindingFor workspaceRoot location)

                    match remoteResult with
                    | Error failure -> return Failed failure
                    | Ok output when output.ExitCode = 0 && not (String.IsNullOrWhiteSpace output.StdOut) ->
                        return bind (output.StdOut.Trim())
                    | Ok output
                        when output.ExitCode = 1
                             && String.IsNullOrWhiteSpace output.StdOut
                             && String.IsNullOrWhiteSpace output.StdErr ->
                        return bind workspaceRoot
                    | Ok output ->
                        return
                            Failed(
                                OperationFailure.createRedacted
                                    ProviderError
                                    "origin_lookup_failed"
                                    $"Git could not read remote.origin.url: {output.StdErr}"
                            )
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
    Open =
        fun binding _ -> async {
            return OperationResult.succeeded (createSessionWithCredentials hooks credentials binding)
        }
    CheckDependencies = checkDependencies
    InstallDependency =
        fun dependencyComponent context ->
            async {
                match dependencyComponent with
                | "git-lfs-configuration" ->
                    let! precheckResult = checkDependencies context

                    match precheckResult with
                    | Failed failure -> return Failed failure
                    | PartiallySucceeded(_, failure) -> return Failed failure
                    | Succeeded precheck ->
                        match
                            precheck.Value
                            |> Array.tryFind (fun status -> status.Component = "git-lfs")
                        with
                        | Some status when status.Installed && status.Compatible ->
                            let! installResult =
                                runGit hooks "." [| "lfs"; "install"; "--skip-repo" |] None context

                            match installResult with
                            | Error failure -> return Failed failure
                            | Ok output when output.ExitCode <> 0 ->
                                return
                                    Failed(
                                        OperationFailure.createRedacted
                                            DependencyMissing
                                            "lfs_configuration_install_failed"
                                            $"Git LFS configuration failed: {output.StdErr}"
                                    )
                            | Ok _ ->
                                let! dependenciesResult = checkDependencies context

                                match dependenciesResult with
                                | Failed failure -> return Failed failure
                                | PartiallySucceeded(_, failure) -> return Failed failure
                                | Succeeded outcome ->
                                    match
                                        outcome.Value
                                        |> Array.tryFind (fun status -> status.Component = "git-lfs-configuration")
                                    with
                                    | Some status when status.Installed && status.Compatible ->
                                        return OperationResult.succeeded status
                                    | _ ->
                                        return
                                            Failed(
                                                OperationFailure.create
                                                    DependencyMissing
                                                    "lfs_configuration_unhealthy"
                                                    "Git LFS reported successful installation, but filter.lfs.process is still not configured."
                                            )
                        | _ -> return Failed(manualLfsInstallationRequired ())
                | "git" ->
                    return
                        Failed(
                            OperationFailure.create
                                Unsupported
                                "manual_install_required"
                                "Install Git 2.38 or newer manually, then rerun dependency checks."
                        )
                | "git-lfs" ->
                    return Failed(manualLfsInstallationRequired ())
                | _ ->
                    return
                        Failed(
                            OperationFailure.create
                                Unsupported
                                "dependency_not_supported"
                                "Unsupported dependency component. Supported remediation: git-lfs-configuration."
                        )
            }
    }

/// Factory with the anonymous credential strategy.
let createFactory (hooks: GitSessionHooks) : ProviderFactory =
    createFactoryWithCredentials hooks GitCredentialStrategy.anonymous
