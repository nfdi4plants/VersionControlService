/// Git provider factory and per-workspace sessions over internal Git machinery.
module VersionControlService.Git.GitWorkspaceSession

open System
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.GitEngineTypes

module GitService = VersionControlService.Git.GitService
module GitRefs = VersionControlService.Git.GitRefs
module GitConflictSession = VersionControlService.Git.GitConflictSession
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module GitExecution = VersionControlService.Git.GitExecution
module GitLfsExtensions = VersionControlService.Git.GitLfsExtensions
module GitProvisioningService = VersionControlService.Git.GitProvisioningService
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeInterop = VersionControlService.Runtime.Node.Interop

/// Test seams. `RunProcess` lets tests spy on or replace child-process execution;
/// `Barrier` lets tests pause/act at named transaction points (root, point, context).
type GitSessionHooks = {
    RunBytesProcess: (NodeProcess.ProcessRequest -> OperationContext -> Async<OperationResult<NodeProcess.ByteProcessOutput>>) option
    RunProcess: (NodeProcess.ProcessRequest -> OperationContext -> Async<OperationResult<NodeProcess.ProcessOutput>>) option
    Barrier: (string -> string -> OperationContext -> Async<unit>) option
}

module GitSessionHooks =

    let none: GitSessionHooks = {
        RunBytesProcess = None
        RunProcess = None
        Barrier = None
    }

let private publicationVerificationTimeoutMilliseconds = 30_000
/// This is a cancellation deadline, not a wall-clock bound. The runner resolves after the process closes.
let private postMergeInspectionTimeoutMilliseconds = 30_000

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
    | GitFailureKind.InvalidLfsThreshold -> Validation
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
    | GitFailureKind.InvalidLfsThreshold -> "invalid_lfs_threshold"
    | GitFailureKind.RemoteProjectAlreadyExists -> "remote_project_exists"
    | GitFailureKind.Unknown -> "git_failure"

let private toOperationFailure (failure: GitService.GitFailure) : OperationFailure =
    OperationFailure.createRedacted (categoryOfKind failure.Kind) (codeOfKind failure.Kind) failure.Message

let private hydrationFailure (operation: string) (detail: string) =
    let kind = GitService.classifyFailureKind detail

    OperationFailure.createRedacted
        (categoryOfKind kind)
        "hydration_failed"
        $"Large-object hydration failed after a successful {operation}: {detail}"

/// Recovery for a large-object hydration that did not finish after the Git transfer
/// itself succeeded. A user cancellation gets its own wording so nobody goes looking
/// for an outage that did not happen.
let private materializationRecovery (failure: OperationFailure) : RecoveryAction = {
    Code = "retry_materialization"
    Instructions =
        Some(
            if failure.Category = Canceled then
                "The large-object download was canceled. Retry materialization to finish downloading."
            else
                "Retry downloading large objects once the object store is reachable."
        )
}

let private awaitGit (operation: JS.Promise<GitService.GitResult<'T>>) : Async<Result<'T, OperationFailure>> =
    async {
        let! result = Async.AwaitPromise operation

        match result with
        | Ok value -> return Ok value
        | Error failure -> return Error(toOperationFailure failure)
    }

/// Direct git process invocation honoring the RunProcess hook.
let private mergeGitEnvironment (requestEnvironment: (string * string)[]) =
    let requestNames = requestEnvironment |> Array.map fst |> Set.ofArray

    Array.append
        (GitExecution.environmentOverrides ()
         |> Array.filter (fun (name, _) -> not (Set.contains name requestNames)))
        requestEnvironment

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
                Environment = mergeGitEnvironment environment
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

let private runHookedBytesProcess
    (hooks: GitSessionHooks)
    (request: NodeProcess.ProcessRequest)
    (context: OperationContext)
    =
    async {
        let runner = hooks.RunBytesProcess |> Option.defaultValue NodeProcess.runBytes
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

/// Cooperative in-process mutation lock: JS is single-threaded, so a busy flag
/// with async polling serializes session mutations without blocking reads.
type private MutationLock() =
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
    RevisionIdentity: GitCredentialStrategy.GitIdentityStrategy
    RevisionPolicy: RevisionPolicyStrategy
    /// Active conflict-session identity and rotating handle version.
    mutable ConflictSession: (string * int) option
    /// Monotonic counter so re-opened merges never reuse a closed session ID.
    mutable ConflictGeneration: int
}

let private credentialAuthenticationForRemote
    (state: SessionState)
    (remoteName: string)
    (remoteUrl: string)
    =
    GitCredentialStrategy.resolveCommandAuthentication
        state.Credentials
        remoteUrl
        state.ConnectionProfileId
        remoteName

// ---------------------------------------------------------------------------
// Status and workspace version
// ---------------------------------------------------------------------------

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

let private comparisonReadCanceledFailure (path: string) =
    {
        OperationFailure.create Canceled "operation_canceled" "The Git comparison read was canceled." with
            AffectedPaths = [| path |]
    }

type private UnmergedWorktreePathKind =
    | RegularWorktreeFile of NodeFileSystem.Stats
    | MissingWorktreePath
    | WorktreeDirectory
    | WorktreeSymlink
    | OtherWorktreeEntry
    | NonDirectoryWorktreeParent

type private ClassifiedUnmergedWorktreePath = {
    AbsolutePath: string
    Kind: UnmergedWorktreePathKind
    InspectionError: exn option
}

type private ContainedWorktreeFile = {
    AbsolutePath: string
    Stats: NodeFileSystem.Stats
}

let private unmergedWorktreeKindMarker = function
    | MissingWorktreePath -> "missing"
    | WorktreeDirectory -> "directory"
    | WorktreeSymlink -> "symlink"
    | OtherWorktreeEntry
    | NonDirectoryWorktreeParent -> "other"
    | RegularWorktreeFile _ -> invalidOp "Regular files contribute content hashes, not kind markers."

[<Emit("$0?.code")>]
let private getNodeErrorCode (_error: exn) : string = jsNative

let private tryGetNodeErrorCode (error: exn) : string option =
    getNodeErrorCode error |> Option.ofObj

/// Classifies token-participating path kinds without following leaf symlinks; parent symlinks fail containment.
let private classifyUnmergedWorktreePath
    (state: SessionState)
    (path: string)
    (context: OperationContext)
    =
    async {
        let repositoryRoot = NodePath.resolve [| state.RepoPath |]
        let absolutePath = NodePath.resolve [| repositoryRoot; path |]
        let relativePath = NodePath.relative repositoryRoot absolutePath

        if context.Cancellation.IsCancellationRequested() then
            return Error(comparisonReadCanceledFailure path)
        elif
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

            let classified kind inspectionError =
                Ok {
                    AbsolutePath = absolutePath
                    Kind = kind
                    InspectionError = inspectionError
                }

            let rec inspect index currentPath =
                async {
                    if context.Cancellation.IsCancellationRequested() then
                        return Error(comparisonReadCanceledFailure path)
                    elif index >= segments.Length then
                        return
                            Error(
                                workspaceEvidenceFailure
                                    path
                                    $"The unmerged path '{path}' could not be identified."
                            )
                    else
                        let nextPath = NodePath.join [| currentPath; segments[index] |]

                        try
                            let! stats = NodeFileSystem.lstatAsync nextPath |> Async.AwaitPromise

                            if context.Cancellation.IsCancellationRequested() then
                                return Error(comparisonReadCanceledFailure path)
                            elif stats.isSymbolicLink () then
                                if index < segments.Length - 1 then
                                    return Error(unsafeWorkspacePathFailure path)
                                else
                                    return classified WorktreeSymlink None
                            elif index < segments.Length - 1 then
                                if stats.isDirectory () then
                                    return! inspect (index + 1) nextPath
                                else
                                    return classified NonDirectoryWorktreeParent None
                            elif stats.isFile () then
                                return classified (RegularWorktreeFile stats) None
                            elif stats.isDirectory () then
                                return classified WorktreeDirectory None
                            else
                                return classified OtherWorktreeEntry None
                        with error ->
                            if context.Cancellation.IsCancellationRequested() then
                                return Error(comparisonReadCanceledFailure path)
                            else
                                match tryGetNodeErrorCode error with
                                | Some "ENOENT" -> return classified MissingWorktreePath (Some error)
                                | Some "ENOTDIR" -> return classified NonDirectoryWorktreeParent (Some error)
                                | _ ->
                                    return
                                        Error(
                                            workspaceEvidenceFailure
                                                path
                                                $"The unmerged path '{path}' could not be inspected: {error.Message}"
                                        )
                }

            return! inspect 0 repositoryRoot
    }

let private validateContainedWorktreeFile
    (state: SessionState)
    (path: string)
    (context: OperationContext)
    =
    async {
        let! classification = classifyUnmergedWorktreePath state path context

        match classification with
        | Error failure -> return Error failure
        | Ok { AbsolutePath = absolutePath; Kind = RegularWorktreeFile stats } ->
            return
                Ok {
                    AbsolutePath = absolutePath
                    Stats = stats
                }
        | Ok { Kind = WorktreeSymlink } -> return Error(unsafeWorkspacePathFailure path)
        | Ok { InspectionError = Some error } ->
            return
                Error(
                    workspaceEvidenceFailure
                        path
                        $"The unmerged path '{path}' could not be inspected: {error.Message}"
                )
        | Ok { Kind = NonDirectoryWorktreeParent } ->
            return
                Error(
                    workspaceEvidenceFailure path $"A parent of the unmerged path '{path}' is not a directory."
                )
        | Ok _ ->
            return
                Error(workspaceEvidenceFailure path $"The unmerged path '{path}' is not a regular file.")
    }

let private sameFileIdentity (left: NodeFileSystem.Stats) (right: NodeFileSystem.Stats) =
    left.dev = right.dev && left.ino = right.ino

let private withValidatedWorktreeHandle
    (state: SessionState)
    (path: string)
    (context: OperationContext)
    (consume: NodeFileSystem.FileHandle -> Async<Result<'T, OperationFailure>>)
    : Async<Result<'T, OperationFailure>> =
    async {
        let! beforeOpen = validateContainedWorktreeFile state path context

        match beforeOpen with
        | Error failure -> return Error failure
        | Ok beforeFile ->
            if context.Cancellation.IsCancellationRequested() then
                return Error(comparisonReadCanceledFailure path)
            else
                do! barrier state.Hooks state.RepoPath "conflict-content-validated" context

                if context.Cancellation.IsCancellationRequested() then
                    return Error(comparisonReadCanceledFailure path)
                else
                    let! openResult =
                        async {
                            try
                                let! handle =
                                    NodeFileSystem.openReadNoFollowAsync beforeFile.AbsolutePath
                                    |> Async.AwaitPromise

                                return Ok handle
                            with error ->
                                return
                                    Error(
                                        workspaceEvidenceFailure
                                            path
                                            $"The unmerged path '{path}' could not be opened safely: {error.Message}"
                                    )
                        }

                    match openResult with
                    | Error failure -> return Error failure
                    | Ok handle ->
                        let! consumeResult =
                            async {
                                try
                                    let! afterOpen = validateContainedWorktreeFile state path context

                                    match afterOpen with
                                    | Error failure -> return Error failure
                                    | Ok afterFile ->
                                        let! handleStats = handle.stat () |> Async.AwaitPromise

                                        if
                                            not (sameFileIdentity beforeFile.Stats afterFile.Stats)
                                            || not (sameFileIdentity afterFile.Stats handleStats)
                                        then
                                            return Error(unsafeWorkspacePathFailure path)
                                        elif context.Cancellation.IsCancellationRequested() then
                                            return Error(comparisonReadCanceledFailure path)
                                        else
                                            return! consume handle
                                with error ->
                                    return
                                        Error(
                                            workspaceEvidenceFailure
                                                path
                                                $"The unmerged path '{path}' could not be read safely: {error.Message}"
                                        )
                            }

                        let! closeResult =
                            async {
                                try
                                    do! handle.close () |> Async.AwaitPromise
                                    return Ok()
                                with error ->
                                    return
                                        Error(
                                            workspaceEvidenceFailure
                                                path
                                                $"The unmerged path handle for '{path}' could not be closed: {error.Message}"
                                        )
                            }

                        match consumeResult, closeResult with
                        | Error failure, _ -> return Error failure
                        | Ok _, Error failure -> return Error failure
                        | Ok value, Ok() -> return Ok value
    }

let private hashHandleContent
    (state: SessionState)
    (path: string)
    (context: OperationContext)
    (handle: NodeFileSystem.FileHandle)
    =
    async {
        let chunkSize = 64 * 1024
        let buffer = NodeInterop.bufferAlloc chunkSize
        let mutable position = 0.0
        let hash = NodeInterop.createSha256Hash ()
        let mutable finished = false
        let mutable failure = None

        while not finished && failure.IsNone do
            if context.Cancellation.IsCancellationRequested() then
                failure <- Some(comparisonReadCanceledFailure path)
            else
                let! readResult = handle.read(buffer, 0, chunkSize, position) |> Async.AwaitPromise

                if readResult.bytesRead = 0 then
                    finished <- true
                else
                    NodeInterop.updateHash
                        hash
                        (NodeInterop.bufferSubarray readResult.buffer 0 readResult.bytesRead)
                    position <- position + float readResult.bytesRead
                    do! barrier state.Hooks state.RepoPath "conflict-content-chunk" context

                    if context.Cancellation.IsCancellationRequested() then
                        failure <- Some(comparisonReadCanceledFailure path)

        match failure with
        | Some readFailure -> return Error readFailure
        | None -> return Ok($"{path}\000{NodeInterop.digestHashHex hash}")
    }

type private BoundedContentRead =
    | CompleteContent of obj
    | ContentTooLarge

let private readHandleContentBuffer
    (state: SessionState)
    (path: string)
    (context: OperationContext)
    (handle: NodeFileSystem.FileHandle)
    =
    async {
        let chunkSize = 64 * 1024
        let maximumPreviewBytes = 1024 * 1024
        let chunks = ResizeArray<obj>()
        let mutable position = 0.0
        let mutable finished = false
        let mutable tooLarge = false
        let mutable failure = None

        while not finished && failure.IsNone do
            if context.Cancellation.IsCancellationRequested() then
                failure <- Some(comparisonReadCanceledFailure path)
            else
                let buffer = NodeInterop.bufferAlloc chunkSize
                let! readResult = handle.read(buffer, 0, chunkSize, position) |> Async.AwaitPromise

                if readResult.bytesRead = 0 then
                    finished <- true
                elif position + float readResult.bytesRead > float maximumPreviewBytes then
                    finished <- true
                    tooLarge <- true
                else
                    chunks.Add(NodeInterop.bufferSubarray readResult.buffer 0 readResult.bytesRead)
                    position <- position + float readResult.bytesRead

                if readResult.bytesRead > 0 then
                    do! barrier state.Hooks state.RepoPath "conflict-content-chunk" context

                    if context.Cancellation.IsCancellationRequested() then
                        failure <- Some(comparisonReadCanceledFailure path)

        match failure with
        | Some readFailure -> return Error readFailure
        | None when tooLarge -> return Ok ContentTooLarge
        | None -> return Ok(CompleteContent(NodeInterop.bufferConcat (chunks.ToArray())))
    }

let private hashUnmergedWorktreePath
    (state: SessionState)
    (path: string)
    (context: OperationContext)
    : Async<Result<string, OperationFailure>> =
    withValidatedWorktreeHandle state path context (hashHandleContent state path context)

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

            let markerEvidence path kind =
                $"{path}\000{unmergedWorktreeKindMarker kind}"

            let pathEvidence path classification =
                async {
                    match classification.Kind with
                    | RegularWorktreeFile _ ->
                        let! hashed = hashUnmergedWorktreePath state path context

                        match hashed with
                        | Ok item -> return Ok item
                        | Error failure when failure.Code = "workspace_evidence_unavailable" ->
                            let! reclassification = classifyUnmergedWorktreePath state path context

                            match reclassification with
                            | Error reclassificationFailure -> return Error reclassificationFailure
                            | Ok { Kind = RegularWorktreeFile _ } -> return Error failure
                            | Ok reclassified -> return Ok(markerEvidence path reclassified.Kind)
                        | Error failure -> return Error failure
                    | kind -> return Ok(markerEvidence path kind)
                }

            let rec collect index evidence =
                async {
                    if index >= paths.Length then
                        return Ok(NodeInterop.sha256Utf8 (String.concat "\000" (List.rev evidence)))
                    else
                        let path = paths[index]
                        let! classification = classifyUnmergedWorktreePath state path context

                        match classification with
                        | Error failure -> return Error failure
                        | Ok classified ->
                            let! itemResult = pathEvidence path classified

                            match itemResult with
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
                let statusPart = NodeInterop.sha256Utf8 statusResult.StdOut
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

type private ConfiguredUpstream = {
    RevisionRef: string
    LogicalRef: LogicalRef
}

let private tryConfiguredUpstream (state: SessionState) (context: OperationContext) =
    async {
        let! result =
            runGit
                state.Hooks
                state.RepoPath
                [| "rev-parse"; "--symbolic-full-name"; "@{upstream}" |]
                None
                context

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode = 0 ->
            let revisionRef = output.StdOut.Trim()

            let configured =
                if revisionRef.StartsWith("refs/remotes/", StringComparison.Ordinal) then
                    let name = revisionRef.Substring("refs/remotes/".Length)

                    Some {
                        RevisionRef = revisionRef
                        LogicalRef = {
                            Name = name
                            ProviderRef = mkProviderRef $"git-remote:{name}"
                            Kind = RemoteRef
                            IsCurrent = false
                        }
                    }
                elif revisionRef.StartsWith("refs/heads/", StringComparison.Ordinal) then
                    let name = revisionRef.Substring("refs/heads/".Length)

                    Some {
                        RevisionRef = revisionRef
                        LogicalRef = {
                            Name = name
                            ProviderRef = mkProviderRef $"git-local:{name}"
                            Kind = LocalRef
                            IsCurrent = false
                        }
                    }
                else
                    None

            return Ok configured
        | Ok _ -> return Ok None
    }

let private previewIndeterminate (classification: string) (detail: string) =
    {
        OperationFailure.createRedacted
            ProviderError
            "preview_indeterminate"
            $"The update preview could not classify {classification}: {detail}" with
            Retryable = true
    }

let private remoteNames (output: string) =
    output.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map _.Trim()
    |> Array.filter (String.IsNullOrWhiteSpace >> not)

let private configuredRemoteNames (state: SessionState) (context: OperationContext) =
    async {
        let! remotes = runGitChecked state.Hooks state.RepoPath [| "remote" |] None context

        match remotes with
        | Error failure -> return Error failure
        | Ok output -> return Ok(remoteNames output.StdOut)
    }

let private configuredUpstreamRemote
    (state: SessionState)
    (upstreamName: string)
    (context: OperationContext)
    =
    async {
        let! remotes = configuredRemoteNames state context

        match remotes with
        | Error failure -> return Error failure
        | Ok names ->
            let remote =
                names
                |> Array.filter (fun candidate -> upstreamName.StartsWith(candidate + "/", StringComparison.Ordinal))
                |> Array.sortByDescending _.Length
                |> Array.tryHead

            return Ok remote
    }

let private configuredRemoteExists (state: SessionState) (remoteName: string) (context: OperationContext) =
    async {
        let! remotes = configuredRemoteNames state context

        match remotes with
        | Error failure -> return Error failure
        | Ok names ->
            return
                Ok(
                    names
                    |> Array.exists (fun candidate -> String.Equals(candidate, remoteName, StringComparison.Ordinal))
                )
    }

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

type private PublishRemote = {
    Name: string
    Url: string
}

let private configuredTargetInvalidFailure () =
    OperationFailure.create
        Validation
        "configured_target_invalid"
        "The configured Git upstream does not identify a remote."

/// Non-empty lines of a `git remote get-url` query. Exit code 2 is "No such remote",
/// any other nonzero exit is git failing to answer.
let private readRemoteUrls
    (state: SessionState)
    (arguments: string[])
    (remoteName: string)
    (context: OperationContext)
    : Async<Result<string[], OperationFailure>> =
    async {
        let! result =
            runGit state.Hooks state.RepoPath [| "remote"; "get-url"; yield! arguments; remoteName |] None context

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode = 0 ->
            let urls =
                output.StdOut.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun url -> url.Trim())
                |> Array.filter (fun url -> url <> "")
                |> Array.distinct

            if urls.Length = 0 then
                return Error(configuredTargetInvalidFailure ())
            else
                return Ok urls
        | Ok output when output.ExitCode = 2 -> return Error(configuredTargetInvalidFailure ())
        | Ok output ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "git_failure"
                        $"git remote get-url {remoteName} failed: {output.StdErr + output.StdOut}"
                )
    }

/// The effective push URL: pushurl over url, with insteadOf and pushInsteadOf applied,
/// which is what `git push <name>` uses. Several distinct push URLs would publish to
/// several hosts, which one credential and one identity cannot serve, so that
/// configuration counts as an invalid target.
let private resolvePublishRemoteUrl
    (state: SessionState)
    (remoteName: string)
    (context: OperationContext)
    =
    async {
        let! urls = readRemoteUrls state [| "--push"; "--all" |] remoteName context

        match urls with
        | Error failure -> return Error failure
        | Ok [| url |] -> return Ok { Name = remoteName; Url = url }
        | Ok _ -> return Error(configuredTargetInvalidFailure ())
    }

/// The effective fetch URL, with insteadOf applied. Fetch, ls-remote, LFS downloads and
/// the first fetch after Initialize connect to this URL, so their credentials have to
/// be scoped to it and not to the raw configuration value or to the bound location.
let private resolveRemoteFetchUrl
    (state: SessionState)
    (remoteName: string)
    (context: OperationContext)
    : Async<Result<string, OperationFailure>> =
    async {
        let! urls = readRemoteUrls state [||] remoteName context
        return urls |> Result.map (fun urls -> urls.[0])
    }

/// Header-only credential arguments for a fetch-direction command against a remote.
let private fetchAuthArgumentsForRemote
    (state: SessionState)
    (remoteName: string)
    (context: OperationContext)
    : Async<Result<string[], OperationFailure>> =
    async {
        let! url = resolveRemoteFetchUrl state remoteName context

        match url with
        | Error failure -> return Error failure
        | Ok url ->
            let! arguments = GitCredentialStrategy.resolveAuthArguments state.Credentials url state.ConnectionProfileId
            return Ok arguments
    }

/// Command authentication (headers plus LFS URLs) for a fetch-direction command.
let private fetchAuthenticationForRemote
    (state: SessionState)
    (remoteName: string)
    (context: OperationContext)
    : Async<Result<VersionControlService.Git.GitAuthAdapter.GitCommandAuthentication, OperationFailure>> =
    async {
        let! url = resolveRemoteFetchUrl state remoteName context

        match url with
        | Error failure -> return Error failure
        | Ok url ->
            let! authentication = credentialAuthenticationForRemote state remoteName url
            return Ok authentication
    }

let private readOptionalConfigValue
    (state: SessionState)
    (key: string)
    (context: OperationContext)
    =
    async {
        let! result = runGit state.Hooks state.RepoPath [| "config"; "--get"; key |] None context

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode = 0 && not (String.IsNullOrWhiteSpace output.StdOut) ->
            return Ok(Some(output.StdOut.Trim()))
        | Ok output
            when output.ExitCode = 1
                 && String.IsNullOrWhiteSpace output.StdOut
                 && String.IsNullOrWhiteSpace output.StdErr ->
            return Ok None
        | Ok output ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "git_failure"
                        $"git config --get {key} failed: {output.StdErr + output.StdOut}"
                )
    }

let private configuredBranchRemote
    (state: SessionState)
    (branch: string)
    (context: OperationContext)
    =
    async {
        let! remoteResult = readOptionalConfigValue state $"branch.{branch}.remote" context

        match remoteResult with
        | Error failure -> return Error failure
        | Ok remote ->
            let! mergeResult = readOptionalConfigValue state $"branch.{branch}.merge" context

            match remote, mergeResult with
            | _, Error failure -> return Error failure
            | None, Ok None -> return Ok None
            | Some remoteName, Ok(Some _) -> return Ok(Some remoteName)
            | _ -> return Error(configuredTargetInvalidFailure ())
    }

let private resolvePublishRemote
    (state: SessionState)
    (branch: string)
    (context: OperationContext)
    =
    async {
        let! upstreamResult = tryConfiguredUpstream state context

        match upstreamResult with
        | Error failure -> return Error failure
        | Ok(Some upstream) when upstream.LogicalRef.Kind = RemoteRef ->
            let! remoteResult = configuredUpstreamRemote state upstream.LogicalRef.Name context

            match remoteResult with
            | Ok(Some remote) ->
                let! remoteUrlResult = resolvePublishRemoteUrl state remote context
                return remoteUrlResult |> Result.map Some
            | Ok None -> return Error(configuredTargetInvalidFailure ())
            | Error failure -> return Error failure
        | Ok(Some _) -> return Error(configuredTargetInvalidFailure ())
        | Ok None ->
            let! branchRemoteResult = configuredBranchRemote state branch context

            match branchRemoteResult with
            | Error failure -> return Error failure
            | Ok(Some ".") -> return Error(configuredTargetInvalidFailure ())
            | Ok(Some remoteName) ->
                let! existsResult = configuredRemoteExists state remoteName context

                match existsResult with
                | Error failure -> return Error failure
                | Ok false -> return Error(configuredTargetInvalidFailure ())
                | Ok true ->
                    let! remoteUrlResult = resolvePublishRemoteUrl state remoteName context
                    return remoteUrlResult |> Result.map Some
            | Ok None ->
                let! originResult = configuredRemoteExists state "origin" context

                match originResult with
                | Error failure -> return Error failure
                | Ok false -> return Ok None
                | Ok true ->
                    let! remoteUrlResult = resolvePublishRemoteUrl state "origin" context
                    return remoteUrlResult |> Result.map Some
    }

let private identityMissingFailure () =
    {
        OperationFailure.create
            Validation
            "identity_missing"
            "Git requires user.name and user.email to create a revision. Configure the repository identity or supply a Git identity strategy." with
            RecoveryAction =
                Some {
                    Code = "configure_git_identity"
                    Instructions =
                        Some "Configure git user.name and user.email, or supply a GitIdentityStrategy for the workspace."
                }
    }

let private readConfiguredIdentityValue
    (state: SessionState)
    (key: string)
    (context: OperationContext)
    : Async<Result<string option, OperationFailure>> =
    async {
        let! result = runGit state.Hooks state.RepoPath [| "config"; "--get"; key |] None context

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode = 0 ->
            let value = output.StdOut.Trim()
            return Ok(if String.IsNullOrWhiteSpace value then None else Some value)
        | Ok output when output.ExitCode = 1 -> return Ok None
        | Ok output ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "git_failure"
                        $"git config --get {key} failed: {output.StdErr}"
                )
    }

let private resolveRevisionIdentity
    (state: SessionState)
    (context: OperationContext)
    : Async<Result<string[], OperationFailure>> =
    async {
        // The identity should match the hub the revision will be published to, so the
        // host follows the configured publish remote the same way publish credentials
        // do. Without a publish remote, or on a detached HEAD, the bound location is an
        // advisory fallback. An invalid upstream configuration yields no host at all: a
        // local revision stays possible, and publish reports the configuration. Any other
        // lookup failure is a real error and fails the operation. Sessions built without
        // a strategy keep the default, which never reads the request, so they skip the
        // lookups entirely.
        let! resolvedIdentity =
            if LanguagePrimitives.PhysicalEquality state.RevisionIdentity GitCredentialStrategy.anonymousIdentity then
                async { return Ok None }
            else
                async {
                    let! branchResult = currentBranchName state context

                    let! targetUrl =
                        async {
                            match branchResult with
                            | Error failure when failure.Code = "detached_head" ->
                                return Ok(Some state.Location.ProviderLocation)
                            | Error failure -> return Error failure
                            | Ok branch ->
                                let! remoteResult = resolvePublishRemote state branch context

                                match remoteResult with
                                | Ok(Some remote) -> return Ok(Some remote.Url)
                                | Ok None -> return Ok(Some state.Location.ProviderLocation)
                                | Error failure when failure.Code = "configured_target_invalid" -> return Ok None
                                | Error failure -> return Error failure
                        }

                    match targetUrl with
                    | Error failure -> return Error failure
                    | Ok targetUrl ->
                        let identityRequest: GitCredentialStrategy.RevisionIdentityRequest = {
                            WorkspaceRoot = state.RepoPath
                            TargetHost = targetUrl |> Option.bind GitCredentialStrategy.tryIdentityHost
                            ConnectionProfileId = state.ConnectionProfileId
                        }

                        let! identity = state.RevisionIdentity.ResolveIdentity identityRequest
                        return Ok identity
                }

        match resolvedIdentity with
        | Error failure -> return Error failure
        | Ok(Some identity) when
            String.IsNullOrWhiteSpace identity.Name
            || String.IsNullOrWhiteSpace identity.Email ->
            return Error(identityMissingFailure ())
        | Ok(Some identity) ->
            return
                Ok [|
                    "-c"
                    $"user.name={identity.Name}"
                    "-c"
                    $"user.email={identity.Email}"
                |]
        | Ok None ->
            let! emailResult = readConfiguredIdentityValue state "user.email" context

            match emailResult with
            | Error failure -> return Error failure
            | Ok None -> return Error(identityMissingFailure ())
            | Ok(Some _) ->
                let! nameResult = readConfiguredIdentityValue state "user.name" context

                match nameResult with
                | Error failure -> return Error failure
                | Ok(Some _) -> return Ok [||]
                | Ok None -> return Error(identityMissingFailure ())
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

        // Synchronization revisions: HEAD and the configured upstream, if any.
        let! headRevision =
            runGit state.Hooks state.RepoPath [| "rev-parse"; "HEAD" |] None context

        let workspaceRevision =
            match headRevision with
            | Ok output when output.ExitCode = 0 -> Some(mkRevisionId (output.StdOut.Trim()))
            | _ -> None

        let! upstreamResult = tryConfiguredUpstream state context

        let upstreamFailure, upstream =
            match upstreamResult with
            | Error failure -> Some failure, None
            | Ok value -> None, value

        let! targetRevisionOutput =
            match upstream with
            | Some target ->
                runGit state.Hooks state.RepoPath [| "rev-parse"; target.RevisionRef |] None context
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

        match versionFailure, upstreamFailure with
        | Some failure, _ -> return Error failure
        | None, Some failure -> return Error failure
        | None, None ->
            return
                Ok {
                    CurrentRef =
                        status.Current
                        |> Option.filter (fun current -> current <> "HEAD")
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
                            TargetRef = upstream |> Option.map _.LogicalRef
                            LocalRevisionCount = upstream |> Option.map (fun _ -> status.Ahead)
                            TargetRevisionCount = upstream |> Option.map (fun _ -> status.Behind)
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
        | Error failure -> return Error failure
        | Ok result when result.ExitCode = 0 ->
            let rawPath = result.StdOut.Trim()

            if String.IsNullOrWhiteSpace rawPath then
                return Ok None
            else
                let absolutePath =
                    if rawPath.StartsWith "/" || (rawPath.Length >= 2 && rawPath[1] = ':') then
                        rawPath
                    else
                        NodePath.join [| state.RepoPath; rawPath |]

                return Ok(Some absolutePath)
        | Ok result ->
            let detail = result.StdErr + result.StdOut
            let missingRepository =
                detail.IndexOf("not a git repository", StringComparison.OrdinalIgnoreCase) >= 0

            if missingRepository then
                return Ok None
            else
                return
                    Error(
                        OperationFailure.createRedacted
                            ProviderError
                            "git_failure"
                            $"git rev-parse --git-path {name} failed: {detail}"
                    )
    }

let private resolveGitStatePaths
    (state: SessionState)
    (names: string[])
    (context: OperationContext)
    : Async<Result<string option[], OperationFailure>> =
    let rec resolve index resolved =
        async {
            if index = names.Length then
                return Ok(List.rev resolved |> List.toArray)
            else
                let! result = resolveGitStatePath state names[index] context

                match result with
                | Error failure -> return Error failure
                | Ok path -> return! resolve (index + 1) (path :: resolved)
        }

    resolve 0 []

/// The active merge target revision, resolved through Git state paths.
let private tryGetMergeHead (state: SessionState) (context: OperationContext) =
    async {
        let! mergeHeadPathResult = resolveGitStatePath state "MERGE_HEAD" context

        match mergeHeadPathResult with
        | Error failure -> return Error failure
        | Ok(Some path) when NodeFileSystem.existsSync path ->
            return Ok(Some((NodeFileSystem.readFileSync path NodeFileSystem.TextEncoding.Utf8).Trim()))
        | Ok _ -> return Ok None
    }

let private activeOperationGuard (state: SessionState) (context: OperationContext) =
    async {
        let! mergeHeadResult = tryGetMergeHead state context

        match mergeHeadResult with
        | Error failure -> return Failed failure
        | Ok(Some _) -> return OperationResult.succeeded true
        | Ok None ->
            let! statePathsResult =
                resolveGitStatePaths
                    state
                    [| "rebase-merge"; "rebase-apply"; "CHERRY_PICK_HEAD"; "REVERT_HEAD"; "sequencer" |]
                    context

            match statePathsResult with
            | Error failure -> return Failed failure
            | Ok statePaths ->
                let pathExists path = path |> Option.exists NodeFileSystem.existsSync

                let operationInProgress = statePaths |> Array.exists pathExists

                let inProgressFailure () =
                    OperationFailure.create
                        Conflict
                        "operation_in_progress"
                        "A Git operation is in progress (rebase, cherry-pick, revert or unmerged paths). Finish or abort it before synchronizing."

                if operationInProgress then
                    return Failed(inProgressFailure ())
                else
                    let! unmergedResult =
                        runGitChecked state.Hooks state.RepoPath [| "ls-files"; "-u" |] None context

                    match unmergedResult with
                    | Error failure -> return Failed failure
                    | Ok output when not (String.IsNullOrWhiteSpace output.StdOut) ->
                        return Failed(inProgressFailure ())
                    | Ok _ -> return OperationResult.succeeded false
    }

/// The synchronization target is `@{upstream}`, while publication targets the
/// current branch on the publish remote. A differently named tracked branch has
/// separate target revisions for those two operations.
let private publicationTargetRevision
    (state: SessionState)
    (syncState: SynchronizationState)
    (context: OperationContext)
    : Async<Result<RevisionId option, OperationFailure>> =
    async {
        let! branchResult = currentBranchName state context

        match branchResult with
        | Error failure -> return Error failure
        | Ok branch ->
            let! remoteResult = resolvePublishRemote state branch context

            match remoteResult with
            | Error failure -> return Error failure
            | Ok(Some remote) ->
                match syncState.TargetRef with
                | Some target when target.Kind = RemoteRef && target.Name = $"{remote.Name}/{branch}" ->
                    return Ok syncState.TargetRevision
                | _ -> return Ok None
            | Ok None -> return Ok None
    }

let private conflictRunner (state: SessionState) (context: OperationContext) : GitConflictSession.GitRunner =
    fun arguments stdinData -> runGit state.Hooks state.RepoPath arguments stdinData context

let private unsupportedConflictPreview (path: string) =
    UnsupportedPreview(Some $"Unsupported git content for '{path}'.")

let private maximumConflictPreviewBytes = int64 (1024 * 1024)

let private readConflictStagePreview
    (state: SessionState)
    (stage: int)
    (path: string)
    (context: OperationContext)
    : Async<Result<ConflictPreview option, OperationFailure>> =
    async {
        let stageExpression = $":{stage}:{path}"
        let! objectIdResult =
            runGit
                state.Hooks
                state.RepoPath
                [| "rev-parse"; "--verify"; "--quiet"; stageExpression |]
                None
                context

        match objectIdResult with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 -> return Ok None
        | Ok output ->
            let objectId = output.StdOut.Trim()

            if String.IsNullOrWhiteSpace objectId then
                return
                    Error(
                        OperationFailure.createRedacted
                            ProviderError
                            "invalid_git_output"
                            $"Git returned an empty object ID for conflict stage {stage} at '{path}'."
                    )
            else
                let! typeResult =
                    runGit state.Hooks state.RepoPath [| "cat-file"; "-t"; objectId |] None context

                match typeResult with
                | Error failure -> return Error failure
                | Ok typeOutput when typeOutput.ExitCode <> 0 ->
                    return
                        Error(
                            OperationFailure.createRedacted
                                ProviderError
                                "git_failure"
                                $"Git could not inspect conflict stage {stage} at '{path}'."
                        )
                | Ok typeOutput when
                    typeOutput.StdOut.Trim() <> "blob"
                    || GitService.isExplicitlyUnsupportedPath path
                    ->
                    return Ok(Some(unsupportedConflictPreview path))
                | Ok _ ->
                    let! sizeResult =
                        runGit state.Hooks state.RepoPath [| "cat-file"; "-s"; objectId |] None context

                    match sizeResult with
                    | Error failure -> return Error failure
                    | Ok sizeOutput when sizeOutput.ExitCode <> 0 ->
                        return
                            Error(
                                OperationFailure.createRedacted
                                    ProviderError
                                    "git_failure"
                                    $"Git could not size conflict stage {stage} at '{path}'."
                            )
                    | Ok sizeOutput ->
                        match Int64.TryParse(sizeOutput.StdOut.Trim()) with
                        | false, _ ->
                            return
                                Error(
                                    OperationFailure.createRedacted
                                        ProviderError
                                        "invalid_git_output"
                                        $"Git returned an invalid size for conflict stage {stage} at '{path}'."
                                )
                        | true, size when size > maximumConflictPreviewBytes ->
                            return Ok(Some(unsupportedConflictPreview path))
                        | true, _ ->
                            let request = {
                                NodeProcess.ProcessRequest.create "git" [| "cat-file"; "blob"; objectId |] with
                                    WorkingDirectory = Some state.RepoPath
                                    Environment = mergeGitEnvironment [||]
                                    ProgressPhase = "git"
                            }

                            let! processResult = runHookedBytesProcess state.Hooks request context

                            match processResult with
                            | Error failure -> return Error failure
                            | Ok processOutput when processOutput.ExitCode <> 0 ->
                                return
                                    Error(
                                        OperationFailure.createRedacted
                                            ProviderError
                                            "git_failure"
                                            $"Git could not read conflict stage {stage} at '{path}'."
                                    )
                            | Ok processOutput when GitService.isLikelyBinaryBuffer processOutput.StdOut ->
                                return Ok(Some(unsupportedConflictPreview path))
                            | Ok processOutput ->
                                return Ok(Some(TextPreview(NodeInterop.bufferToUtf8String processOutput.StdOut)))
    }

let private readConflictCombinedPreview
    (state: SessionState)
    (path: string)
    (context: OperationContext)
    : Async<Result<ConflictPreview option, OperationFailure>> =
    async {
        if GitService.isExplicitlyUnsupportedPath path then
            return Ok(Some(unsupportedConflictPreview path))
        else
            let! classification = classifyUnmergedWorktreePath state path context

            match classification with
            | Error failure -> return Error failure
            | Ok { Kind = MissingWorktreePath }
            | Ok { Kind = WorktreeDirectory }
            | Ok { Kind = OtherWorktreeEntry }
            | Ok { Kind = NonDirectoryWorktreeParent } -> return Ok None
            | Ok { Kind = WorktreeSymlink }
            | Ok { Kind = RegularWorktreeFile _ } ->
                let! bufferResult =
                    withValidatedWorktreeHandle state path context (readHandleContentBuffer state path context)

                match bufferResult with
                | Error failure -> return Error failure
                | Ok ContentTooLarge ->
                    return Ok(Some(UnsupportedPreview(Some $"Conflict preview for '{path}' exceeds the text preview limit.")))
                | Ok(CompleteContent buffer) ->
                    if GitService.isLikelyBinaryBuffer buffer then
                        return Ok(Some(unsupportedConflictPreview path))
                    elif not (NodeInterop.bufferIsValidUtf8 buffer) then
                        return Ok(Some(unsupportedConflictPreview path))
                    else
                        return Ok(Some(TextPreview(NodeInterop.bufferToUtf8String buffer)))
    }

/// Active merge state as a provider-managed conflict session. Present whenever
/// MERGE_HEAD exists — including after the last conflict was staged. The handle
/// version rotates on every successful nonterminal resolution and the session ID
/// changes when a merge is reopened after finalize/cancel.
let private getMergeConflictSummary (state: SessionState) (context: OperationContext) =
    async {
        let! mergeHeadResult = tryGetMergeHead state context

        match mergeHeadResult with
        | Error failure -> return Error failure
        | Ok None ->
            state.ConflictSession <- None
            return Ok None
        | Ok(Some mergeHeadValue) ->
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
                        (fun stage path -> readConflictStagePreview state stage path context)
                        (fun path -> readConflictCombinedPreview state path context)
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
// Core workspace operations
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
                                Environment = mergeGitEnvironment environment
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

            let! identityResult = resolveRevisionIdentity state context

            match identityResult with
            | Error failure -> return Failed failure
            | Ok identityArguments ->
                return!
                    GitSelectedRevision.createRevision
                        runner
                        transactionBarrier
                        state.RepoPath
                        request.Message
                        request.Paths
                        identityArguments
                        state.RevisionPolicy
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

        // Large objects stay as pointers during branch switches.
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

let private revParse (state: SessionState) (reference: string) (context: OperationContext) =
    async {
        let! result =
            runGit state.Hooks state.RepoPath [| "rev-parse"; "--verify"; "--quiet"; reference |] None context

        match result with
        | Ok output when output.ExitCode = 0 -> return Some(output.StdOut.Trim())
        | _ -> return None
    }

/// What update records right before it spawns git merge, so cleanup after a cancel
/// can tell this merge's residue apart from state that was already in the repository.
type private MergeStart = {
    WorkspaceVersion: string
    Head: string option
    MergeHeadPresent: bool
    /// True when index.lock already existed before the merge was spawned, so a lock
    /// found afterwards was not created by this update.
    IndexLockPresent: bool
    /// Paths that were changed or untracked before the merge. They may hold the
    /// user's work and are never treated as merge residue.
    PreexistingPaths: Set<string>
}

/// Paths named by `git status --porcelain -z`. Renames and copies carry the original
/// path as the next NUL-separated token, and both count.
let private statusPaths (output: string) =
    let tokens = output.Split '\000'
    let paths = ResizeArray<string>()
    let mutable index = 0

    while index < tokens.Length do
        let entry = tokens.[index]

        if entry.Length >= 4 then
            paths.Add(entry.Substring 3)
            let code = entry.Substring(0, 2)

            if (code.Contains "R" || code.Contains "C") && index + 1 < tokens.Length then
                index <- index + 1
                paths.Add tokens.[index]

        index <- index + 1

    Set.ofSeq paths

let private captureMergeStart
    (state: SessionState)
    (context: OperationContext)
    : Async<Result<MergeStart, OperationFailure>> =
    async {
        let! version = computeWorkspaceVersion state context

        match version with
        | Error failure -> return Error failure
        | Ok version ->
            let! head = revParse state "HEAD" context
            let! mergeHeadResult = tryGetMergeHead state context
            let! lockPathResult = resolveGitStatePath state "index.lock" context

            match mergeHeadResult, lockPathResult with
            | Error failure, _
            | _, Error failure -> return Error failure
            | Ok mergeHead, Ok lockPath ->
                let lockPresent =
                    match lockPath with
                    | Some path -> NodeFileSystem.existsSync path
                    | None -> false

                let! status =
                    runGit
                        state.Hooks
                        state.RepoPath
                        [| "status"; "--porcelain"; "-z"; "--untracked-files=all" |]
                        None
                        context

                match status with
                | Error failure -> return Error failure
                | Ok output when output.ExitCode <> 0 ->
                    return
                        Error(
                            OperationFailure.createRedacted
                                ProviderError
                                "git_failure"
                                $"git status failed before the merge: {output.StdErr}"
                        )
                | Ok output ->
                    return
                        Ok {
                            WorkspaceVersion = version
                            Head = head
                            MergeHeadPresent = mergeHead.IsSome
                            IndexLockPresent = lockPresent
                            PreexistingPaths = statusPaths output.StdOut
                        }
    }

let private gitOutcome (result: Result<NodeProcess.ProcessOutput, OperationFailure>) =
    match result with
    | Ok output when output.ExitCode = 0 -> Ok()
    | Ok output -> Error output.StdErr
    | Error failure -> Error failure.Message

/// Paths a killed fast-forward may have rewritten: those that differ between HEAD and
/// the target and are listed by status now but were not before the merge. This is a
/// report for the caller, never a list of paths to touch. Ignored files, case-only
/// name collisions, submodule and symlink transitions, sparse checkouts and an
/// implicit autostash all break any argument that such paths hold nothing of the
/// user's, so the provider does not restore them on its own.
let private rewrittenTargetPaths
    (state: SessionState)
    (start: MergeStart)
    (targetReference: string)
    (context: OperationContext)
    : Async<Result<string[], string>> =
    async {
        let! diff =
            runGit
                state.Hooks
                state.RepoPath
                [| "diff"; "--name-only"; "--no-renames"; "-z"; "HEAD"; targetReference |]
                None
                context

        match diff with
        | Error failure -> return Error failure.Message
        | Ok output when output.ExitCode <> 0 -> return Error output.StdErr
        | Ok output ->
            let candidates =
                output.StdOut.Split '\000'
                |> Array.filter (fun path -> path <> "" && not (start.PreexistingPaths.Contains path))

            if candidates.Length = 0 then
                return Ok [||]
            else
                // --ignored lists ignored files one by one under --untracked-files=all.
                // The overwrite check exempts them, so a killed fast-forward may have
                // written exactly those, and the report must not drop them.
                let! status =
                    runGit
                        state.Hooks
                        state.RepoPath
                        [| "status"; "--porcelain"; "-z"; "--untracked-files=all"; "--ignored" |]
                        None
                        context

                match status with
                | Error failure -> return Error failure.Message
                | Ok listed when listed.ExitCode <> 0 -> return Error listed.StdErr
                | Ok listed ->
                    let dirty = statusPaths listed.StdOut
                    return Ok(candidates |> Array.filter dirty.Contains)
    }

/// Cleans up after a canceled `git merge`. A true merge that was killed leaves
/// MERGE_HEAD with a half-applied index and worktree, and `git merge --abort` puts
/// that back. A fast-forward writes no MERGE_HEAD and may have rewritten part of the
/// worktree before the kill. That residue is reported, not undone, because no
/// argument available here proves the rewritten paths held nothing of the user's.
/// Either kind of kill can leave an index.lock behind, which is left for the caller
/// as well: nothing here can prove who owns a lock, and deleting a live writer's lock
/// can lose its index update. Cleanup runs on a copy of the context without the
/// cancellation, because the process runner refuses to start anything on a context
/// that is already canceled. Whatever cannot be undone or observed is reported with
/// StateChanged set.
let private recoverCanceledMerge
    (state: SessionState)
    (start: MergeStart)
    (targetReference: string)
    (failure: OperationFailure)
    (context: OperationContext)
    : Async<OperationFailure> =
    async {
        let cleanupContext = {
            context with
                Cancellation = OperationCancellation.none
        }

        let residue (code: string) (instructions: string) (affected: string[]) = {
            failure with
                StateChanged = true
                AffectedPaths = affected
                RecoveryAction =
                    Some {
                        Code = code
                        Instructions = Some(Redaction.redact instructions)
                    }
        }

        let inspect (detail: string) =
            residue
                "inspect_workspace"
                $"Git could not report the workspace state after the cancellation: {detail}. Refresh before retrying."
                [||]

        // A lock blocks git merge --abort and every later mutation. Its mtime says
        // nothing about ownership, and a live writer whose lock disappears can commit
        // its index over another process's lock, so the lock is reported, never removed.
        let! lockPathResult = resolveGitStatePath state "index.lock" cleanupContext

        match lockPathResult with
        | Error lookupFailure -> return inspect lookupFailure.Message
        | Ok None -> return inspect "the index.lock path could not be resolved"
        | Ok(Some lockPath) when NodeFileSystem.existsSync lockPath && start.IndexLockPresent ->
            // The lock predates this update. Usually git merge refused to start and the
            // repository is as it was, but the other process may have released its lock in
            // the window before the spawn, so the workspace version decides the report.
            let! versionAfter = computeWorkspaceVersion state cleanupContext

            match versionAfter with
            | Ok version when version = start.WorkspaceVersion ->
                return {
                    failure with
                        RecoveryAction =
                            Some {
                                Code = "remove_index_lock"
                                Instructions =
                                    Some
                                        "An index.lock was already present before the update started, so another process holds the index or left a stale lock. The update did not create it and the workspace matches its pre-merge state. Clear the lock once no git process is running, then retry."
                            }
                }
            | _ ->
                return
                    residue
                        "remove_index_lock"
                        "A stale .git/index.lock is present. Make sure no git process is still running on the repository, remove the lock, run git merge --abort if MERGE_HEAD exists, then refresh."
                        [||]
        | Ok(Some lockPath) when NodeFileSystem.existsSync lockPath ->
            return
                residue
                    "remove_index_lock"
                    "A stale .git/index.lock is present. Make sure no git process is still running on the repository, remove the lock, run git merge --abort if MERGE_HEAD exists, then refresh."
                    [||]
        | Ok(Some _) ->
            let! mergeHeadPathResult = resolveGitStatePath state "MERGE_HEAD" cleanupContext

            match mergeHeadPathResult with
            | Error lookupFailure -> return inspect lookupFailure.Message
            | Ok None -> return inspect "the MERGE_HEAD path could not be resolved"
            | Ok(Some mergeHeadPath) ->
                // Merge state that was already there before this merge started is
                // someone else's work in progress and is left alone.
                let abortRuns = NodeFileSystem.existsSync mergeHeadPath && not start.MergeHeadPresent

                let! aborted =
                    if abortRuns then
                        async {
                            let! result = runGit state.Hooks state.RepoPath [| "merge"; "--abort" |] None cleanupContext
                            return gitOutcome result
                        }
                    else
                        async { return Ok() }

                match aborted with
                | Error message ->
                    return
                        residue
                            "abort_merge"
                            $"Run git merge --abort in the workspace and refresh. Cleanup failed: {message}"
                            [||]
                | Ok() ->
                    let! versionAfter = computeWorkspaceVersion state cleanupContext

                    match versionAfter with
                    | Error observeFailure -> return inspect observeFailure.Message
                    | Ok version when version = start.WorkspaceVersion -> return failure
                    | Ok _ when abortRuns ->
                        return
                            residue
                                "inspect_workspace"
                                "git merge --abort completed, but the workspace differs from its pre-merge state. Review the status and refresh."
                                [||]
                    | Ok _ ->
                        let! headAfter = revParse state "HEAD" cleanupContext

                        if headAfter <> start.Head then
                            return
                                residue
                                    "refresh_workspace"
                                    "The merge finished before the cancellation took effect. Refresh to load the updated workspace."
                                    [||]
                        else
                            // HEAD did not move and no merge state remains, yet the workspace
                            // differs. Either a fast-forward was killed while rewriting or
                            // something outside the update changed files. Both are reported
                            // as a possible change with the paths the fast-forward could have
                            // touched, and the caller decides.
                            let! rewritten = rewrittenTargetPaths state start targetReference cleanupContext

                            match rewritten with
                            | Error message -> return inspect message
                            | Ok [||] ->
                                return
                                    residue
                                        "inspect_workspace"
                                        "The workspace differs from its pre-merge state, but no path the update could have rewritten has changed. Review the status and refresh."
                                        [||]
                            | Ok affected ->
                                return
                                    residue
                                        "restore_workspace"
                                        "The update was canceled while git may have been rewriting files. Review the listed paths, restore or keep them, then refresh."
                                        affected
    }

let private synchronizationState (state: SessionState) (context: OperationContext) =
    async {
        let! upstreamResult = tryConfiguredUpstream state context

        match upstreamResult with
        | Error failure -> return Error failure
        | Ok upstream ->
            let! workspaceRevision = revParse state "HEAD" context
            let! targetRevision =
                match upstream with
                | Some target -> revParse state target.RevisionRef context
                | None -> async { return None }

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

            let canceledInspectionFailure () =
                OperationFailure.create Canceled "operation_canceled" "The workspace state inspection was canceled."

            let! remoteChangedResult =
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
                            let entries =
                                output.StdOut.Split '\000'
                                |> Array.filter (fun entry -> entry <> "")

                            let mutable invalidPath = None

                            let paths =
                                entries
                                |> Array.choose (fun entry ->
                                    match tryCreateRepositoryPath entry with
                                    | Ok path -> Some path
                                    | Error message ->
                                        invalidPath <- Some message
                                        None)

                            match invalidPath with
                            | Some message ->
                                return Error(previewIndeterminate "target changed paths" message)
                            | None -> return Ok(Some paths)
                        | Ok output ->
                            let detail =
                                if String.IsNullOrWhiteSpace output.StdErr then
                                    output.StdOut
                                else
                                    output.StdErr

                            if context.Cancellation.IsCancellationRequested() then
                                return Error(canceledInspectionFailure ())
                            else
                                return Error(previewIndeterminate "target changed paths" detail)
                        | Error failure ->
                            if context.Cancellation.IsCancellationRequested() then
                                return Error(canceledInspectionFailure ())
                            else
                                return Error(previewIndeterminate "target changed paths" failure.Message)
                    }
                | _ -> async { return Ok None }

            match remoteChangedResult with
            | Error failure -> return Error failure
            | Ok remoteChanged ->
                if context.Cancellation.IsCancellationRequested() then
                    return Error(canceledInspectionFailure ())
                else
                    return
                        Ok {
                            BaseRevision = baseRevision |> Option.map mkRevisionId
                            WorkspaceRevision = workspaceRevision |> Option.map mkRevisionId
                            TargetRevision = targetRevision |> Option.map mkRevisionId
                            TargetRef = upstream |> Option.map _.LogicalRef
                            LocalRevisionCount = None
                            TargetRevisionCount = None
                            RemoteChangedPaths = remoteChanged
                            Relationship = relationship
                        }
    }

let private readRemoteBranchRevision
    (state: SessionState)
    (remoteName: string)
    (authentication: VersionControlService.Git.GitAuthAdapter.GitCommandAuthentication)
    (branch: string)
    (context: OperationContext)
    =
    async {
        let! result =
            runGitEnv
                state.Hooks
                state.RepoPath
                [|
                    yield! authentication.ConfigArgs
                    "ls-remote"
                    "--refs"
                    "--"
                    remoteName
                    $"refs/heads/{branch}"
                |]
                None
                [| "GIT_TERMINAL_PROMPT", "0" |]
                context

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            return
                Error {
                    OperationFailure.createRedacted
                        Network
                        "target_unreachable"
                        $"Reading the publication target from {remoteName} failed: {output.StdErr + output.StdOut}" with
                        Retryable = true
                }
        | Ok output ->
            return
                output.StdOut.Replace("\r\n", "\n").Split('\n', StringSplitOptions.RemoveEmptyEntries)
                |> Array.tryHead
                |> Option.bind (fun line ->
                    line.Split([| '\t'; ' ' |], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.tryHead)
                |> Ok
    }

let private publicationChangedPaths
    (state: SessionState)
    (previousRevision: string option)
    (publishedRevision: string)
    (context: OperationContext)
    =
    async {
        let arguments =
            match previousRevision with
            | Some previous ->
                [| "diff"; "--no-renames"; "--name-only"; "-z"; previous; publishedRevision |]
            | None ->
                [|
                    "ls-tree"
                    "-r"
                    "--name-only"
                    "-z"
                    publishedRevision
                |]

        let! result = runGit state.Hooks state.RepoPath arguments None context

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            return
                Error {
                    OperationFailure.createRedacted
                        ProviderError
                        "publish_evidence_failed"
                        $"Reading the published path evidence failed: {output.StdErr + output.StdOut}" with
                        Retryable = true
                }
        | Ok output ->
            let entries =
                output.StdOut.Split('\000', StringSplitOptions.RemoveEmptyEntries)

            let mutable invalidPath = None

            let paths =
                entries
                |> Array.choose (fun entry ->
                    match tryCreateRepositoryPath entry with
                    | Ok path -> Some(RepositoryPath.value path)
                    | Error message ->
                        invalidPath <- Some message
                        None)

            match invalidPath with
            | Some message ->
                return
                    Error {
                        OperationFailure.createRedacted
                            ProviderError
                            "publish_evidence_failed"
                            $"The published path evidence contained an unsafe path: {message}" with
                            Retryable = true
                    }
            | None -> return Ok paths
    }

let private refresh (state: SessionState) (context: OperationContext) =
    async {
        let! upstreamResult = tryConfiguredUpstream state context

        match upstreamResult with
        | Error failure -> return Failed failure
        | Ok(Some upstream) when upstream.LogicalRef.Kind = LocalRef ->
            let! stateResult = synchronizationState state context

            match stateResult with
            | Error failure -> return Failed failure
            | Ok syncState ->
                return
                    OperationResult.noOp
                        (Some "The configured target is a local ref and does not require fetching.")
                        syncState
        | Ok(Some upstream) ->
            let! remoteResult = configuredUpstreamRemote state upstream.LogicalRef.Name context

            match remoteResult with
            | Error failure -> return Failed failure
            | Ok None ->
                return
                    Failed(
                        OperationFailure.create
                            Validation
                            "configured_target_invalid"
                            "The configured Git upstream does not identify a remote."
                    )
            | Ok(Some remote) ->
                do! barrier state.Hooks state.RepoPath "transfer-start" context

                let! authArguments = fetchAuthArgumentsForRemote state remote context

                match authArguments with
                | Error failure -> return Failed failure
                | Ok authArguments ->

                let! fetchResult =
                    runGitChecked
                        state.Hooks
                        state.RepoPath
                        [| yield! authArguments; "fetch"; remote |]
                        None
                        context

                match fetchResult with
                | Error failure -> return Failed { failure with Retryable = true }
                | Ok _ ->
                    let! stateResult = synchronizationState state context

                    match stateResult with
                    | Error failure -> return Failed failure
                    | Ok syncState -> return OperationResult.succeeded syncState
        | Ok None ->
            let! stateResult = synchronizationState state context

            match stateResult with
            | Error failure -> return Failed failure
            | Ok syncState -> return OperationResult.noOp (Some "No target is configured.") syncState
    }

let private previewRefreshedState (state: SessionState) (context: OperationContext) (syncState: SynchronizationState) =
    async {
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
                        | Ok output when output.ExitCode = 0 -> return Ok false
                        | Ok output when output.ExitCode = 1 -> return Ok true
                        | Ok output ->
                            let detail =
                                if String.IsNullOrWhiteSpace output.StdErr then output.StdOut else output.StdErr

                            return Error(previewIndeterminate "committed conflicts" detail)
                        | Error failure ->
                            return Error(previewIndeterminate "committed conflicts" failure.Message)
                    }
                | _ -> async { return Ok false }

            match committedConflicts with
            | Error failure -> return Failed failure
            | Ok hasCommittedConflicts ->
                return
                    OperationResult.succeeded {
                        ChangedPaths = changed
                        OverlappingPaths = overlapping
                        HasDataLossRisk = overlapping.Length > 0
                        WouldCreateConflictSession = overlapping.Length > 0 || hasCommittedConflicts
                    }
    }

let private previewFromState
    (state: SessionState)
    (syncState: SynchronizationState)
    (context: OperationContext)
    =
    async {
        match syncState.Relationship, syncState.TargetRevision with
        | UnknownRelationship, Some _ ->
            return
                Failed(
                    previewIndeterminate
                        "synchronization state"
                        "The workspace and target histories do not share a merge base."
                )
        | _ -> return! previewRefreshedState state context syncState
    }

let private previewUpdate (state: SessionState) (context: OperationContext) =
    async {
        let! refreshResult = refresh state context

        match refreshResult with
        | Failed failure -> return Failed failure
        | PartiallySucceeded(_, failure) -> return Failed failure
        | Succeeded outcome -> return! previewFromState state outcome.Value context
    }

let private updateFromState
    (state: SessionState)
    (syncState: SynchronizationState)
    (context: OperationContext)
    =
    async {
        match syncState.Relationship with
        | UpToDate
        | LocalAhead
        | NoTarget -> return OperationResult.noOp (Some "The workspace is already up to date.") syncState
        | _ ->
            let targetReference =
                syncState.TargetRevision |> Option.map RevisionId.value |> Option.get

            let! identityResult = resolveRevisionIdentity state context
            let! startResult = captureMergeStart state context

            // The failure carries whether git merge was spawned. Recovery only makes
            // sense after a spawn attempt: an identity failure or a cancellation that
            // was already pending never touched the repository.
            let! mergeResult =
                match identityResult, startResult with
                | Error failure, _
                | _, Error failure -> async { return Error(failure, false) }
                | Ok identityArguments, Ok _ ->
                    async {
                        do! barrier state.Hooks state.RepoPath "update-merge" context

                        if context.Cancellation.IsCancellationRequested() then
                            return
                                Error(
                                    OperationFailure.create
                                        Canceled
                                        "operation_canceled"
                                        "The update was canceled before the merge started.",
                                    false
                                )
                        else
                            let! result =
                                runGitEnv
                                    state.Hooks
                                    state.RepoPath
                                    // Ignored files are not part of the overwrite check by
                                    // default, and an implicit autostash moves files outside
                                    // the target diff. Both would defeat a truthful report of
                                    // what a killed merge may have touched.
                                    [|
                                        yield! identityArguments
                                        "merge"
                                        "--no-edit"
                                        "--no-overwrite-ignore"
                                        "--no-autostash"
                                        targetReference
                                    |]
                                    None
                                    [| "GIT_LFS_SKIP_SMUDGE", "1" |]
                                    context

                            return result |> Result.mapError (fun failure -> failure, true)
                    }

            // The merge has already changed the repository. The follow-up reads state only, so
            // caller cancellation would fabricate a state instead of reporting the merge result.
            // The detached read has a deadline so a provider read cannot wait forever.
            let inspectionCancellation = OperationCancellation.Source()
            let mutable inspectionCompleted = false
            let mutable inspectionTimedOut = false

            Async.StartImmediate(
                async {
                    do! Async.Sleep postMergeInspectionTimeoutMilliseconds

                    if not inspectionCompleted then
                        inspectionTimedOut <- true
                        inspectionCancellation.Cancel()
                }
            )

            let inspectionContext = {
                context with
                    Cancellation = inspectionCancellation.Cancellation
            }

            let refreshWorkspaceRecovery = {
                Code = "refresh_workspace"
                Instructions = Some "The update was applied. Refresh the workspace to read its state."
            }

            let inspectionFailure (failure: OperationFailure) : OperationFailure =
                if inspectionTimedOut then
                    {
                        OperationFailure.create
                            Timeout
                            "inspection_timeout"
                            "Reading the workspace state after the update exceeded the inspection deadline." with
                            StateChanged = true
                            RecoveryAction = Some refreshWorkspaceRecovery
                    }
                else
                    let recoveryAction =
                        failure.RecoveryAction |> Option.defaultValue refreshWorkspaceRecovery

                    { failure with
                        StateChanged = true
                        RecoveryAction = Some recoveryAction }

            match mergeResult, startResult with
            | Error(failure, true), Ok start when failure.Category = Canceled ->
                let! recovered = recoverCanceledMerge state start targetReference failure context
                return Failed recovered
            | Error(failure, _), _ -> return Failed failure
            | Ok _, Error failure -> return Failed failure
            | Ok output, Ok start when output.ExitCode = 0 ->
                let! updatedState = synchronizationState state inspectionContext

                match updatedState with
                | Error failure ->
                    inspectionCompleted <- true
                    return Failed(inspectionFailure failure)
                | Ok newState ->
                    let! materializationSetting =
                        runGit
                            state.Hooks
                            state.RepoPath
                            [| "config"; "--get"; GitService.MaterializeLargeObjectsKey |]
                            None
                            inspectionContext
                    inspectionCompleted <- true

                    let materializationFailure, materializeLargeObjects =
                        match materializationSetting with
                        | Ok setting when setting.ExitCode = 0 ->
                            None,
                            match setting.StdOut.Trim().ToLowerInvariant() with
                            | "true"
                            | "1"
                            | "yes"
                            | "on" -> true
                            | _ -> false
                        | Ok _ -> None, false
                        | Error failure -> Some failure, false

                    if materializationFailure.IsSome then
                        return Failed(inspectionFailure materializationFailure.Value)
                    elif not materializeLargeObjects then
                        return OperationResult.succeeded newState
                    else
                        let! remoteResult =
                            match syncState.TargetRef with
                            | Some target -> configuredUpstreamRemote state target.Name context
                            | None -> async { return Ok None }

                        let! hydration =
                            match remoteResult with
                            | Error failure -> async { return Error failure }
                            | Ok None ->
                                async {
                                    return
                                        Error(
                                            OperationFailure.create
                                                Validation
                                                "configured_target_invalid"
                                                "The configured Git upstream does not identify a remote."
                                        )
                                }
                            | Ok(Some remote) ->
                                async {
                                    let! authentication = fetchAuthenticationForRemote state remote context

                                    let hydrationRef =
                                        syncState.TargetRef
                                        |> Option.bind (fun target ->
                                            let prefix = remote + "/"

                                            if target.Name.StartsWith(prefix, StringComparison.Ordinal) then
                                                Some(target.Name.Substring prefix.Length)
                                            else
                                                None)

                                    match authentication with
                                    | Error failure -> return Error failure
                                    | Ok authentication ->
                                        return!
                                            runGitEnv
                                                state.Hooks
                                                state.RepoPath
                                                [|
                                                    yield! authentication.ConfigArgs
                                                    "lfs"
                                                    "pull"
                                                    remote
                                                    yield! hydrationRef |> Option.toArray
                                                |]
                                                None
                                                [| "GIT_TERMINAL_PROMPT", "0" |]
                                                context
                                }

                        let affectedPaths =
                            syncState.RemoteChangedPaths
                            |> Option.defaultValue [||]
                            |> Array.map RepositoryPath.value

                        let outcome = {
                            OperationOutcome.performed newState with
                                AffectedPaths = affectedPaths
                                ResultingRevision = newState.WorkspaceRevision
                        }

                        let partial failure =
                            OperationResult.partiallySucceeded
                                outcome
                                {
                                    failure with
                                        AffectedPaths = affectedPaths
                                }
                                (materializationRecovery failure)

                        match hydration with
                        | Ok hydrationOutput when hydrationOutput.ExitCode = 0 -> return Succeeded outcome
                        | Ok hydrationOutput ->
                            let detail =
                                if String.IsNullOrWhiteSpace hydrationOutput.StdErr then
                                    hydrationOutput.StdOut
                                else
                                    hydrationOutput.StdErr

                            return
                                partial (hydrationFailure "update" detail)
                        | Error hydrationFailure ->
                            return
                                partial {
                                    hydrationFailure with
                                        Code = "hydration_failed"
                                }
            | Ok output, Ok start ->
                let! mergeHeadResult = tryGetMergeHead state inspectionContext

                match mergeHeadResult with
                | Error failure ->
                    inspectionCompleted <- true
                    return
                        Failed {
                            failure with
                                StateChanged = true
                                RecoveryAction =
                                    Some {
                                        Code = "inspect_workspace"
                                        Instructions =
                                            Some
                                                "The state after the update could not be read. Inspect the workspace before retrying."
                                    }
                        }
                | Ok(Some _) ->
                    // Conflicting merge: the conflict-session cycle turns this into a
                    // provider-managed session; the shell reports the structured code.
                    let! updatedState = synchronizationState state inspectionContext
                    inspectionCompleted <- true

                    let stateValue, warnings =
                        match updatedState with
                        | Ok value -> value, [||]
                        | Error failure ->
                            let reportedFailure = inspectionFailure failure
                            syncState,
                            [| {
                                   Code = "state_inspection_failed"
                                   Message = reportedFailure.Message
                               } |]

                    return
                        OperationResult.partiallySucceeded
                            ({ OperationOutcome.performed stateValue with Warnings = warnings })
                            (OperationFailure.create
                                Conflict
                                "conflicts_detected"
                                "The update produced conflicts that need resolution.")
                            {
                                Code = "resolve_conflict_session"
                                Instructions = Some "Resolve every conflict item, then finalize."
                            }
                | Ok None ->
                    let! workspaceVersionAfter = computeWorkspaceVersion state inspectionContext
                    inspectionCompleted <- true

                    let stateChanged, recoveryAction =
                        match workspaceVersionAfter with
                        | Ok version -> version <> start.WorkspaceVersion, None
                        | Error _ ->
                            true,
                            Some {
                                Code = "inspect_workspace"
                                Instructions = Some "The state after the rejected update could not be read. Inspect the workspace before retrying."
                            }

                    let details =
                        output.StdErr.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)

                    let rejected =
                        OperationFailure.createRedacted ProviderError "update_rejected" "Git rejected the update."
                        |> OperationFailure.withDetails details

                    return
                        Failed {
                            rejected with
                                StateChanged = stateChanged
                                Retryable = false
                                RecoveryAction = recoveryAction
                        }
    }

let private update (state: SessionState) (request: UpdateRequest) (context: OperationContext) =
    async {
        let! refreshResult = refresh state context

        match refreshResult with
        | Failed failure -> return Failed failure
        | PartiallySucceeded(_, failure) -> return Failed failure
        | Succeeded outcome -> return! updateFromState state outcome.Value context
    }

/// Whether the ref a publish just pushed is the ref the synchronization state tracks.
/// Only then may the state claim the target sits at the pushed revision. A branch that
/// tracks a differently named ref keeps the state its upstream describes.
let private publishedRefIsSynchronizationTarget
    (syncState: SynchronizationState)
    (remoteName: string)
    (branch: string)
    =
    match syncState.TargetRef with
    | Some target when target.Kind = RemoteRef && target.Name = $"{remoteName}/{branch}" -> true
    | _ -> false

let private publish (state: SessionState) (expectedTarget: RevisionId option) (context: OperationContext) =
    async {
        let! branchResult = currentBranchName state context

        match branchResult with
        | Error failure -> return Failed failure
        | Ok branch ->
            let! targetRemoteResult = resolvePublishRemote state branch context
            let mutable resolvedTarget = None

            // Validate against remote truth. The local remote-tracking ref can be
            // stale between refresh and publish, and explicit LFS planning must
            // never mask a target-revision precondition failure.
            let! observedTargetResult =
                match targetRemoteResult with
                | Error failure -> async { return Error failure }
                | Ok None ->
                    async {
                        return
                            Error(
                                OperationFailure.create
                                    Validation
                                    "publish_target_missing"
                                    "The workspace has no configured Git publication target."
                            )
                    }
                | Ok(Some remote) ->
                    async {
                        // ls-remote connects to the fetch URL and push to the push URL. With an
                        // https fetch URL and an ssh pushurl these differ, so each command gets
                        // credentials for the host it actually reaches.
                        let! fetchAuthentication = fetchAuthenticationForRemote state remote.Name context

                        match fetchAuthentication with
                        | Error failure -> return Error failure
                        | Ok fetchAuthentication ->
                            let! pushAuthentication =
                                credentialAuthenticationForRemote state remote.Name remote.Url

                            resolvedTarget <- Some(remote, fetchAuthentication, pushAuthentication)

                            return!
                                readRemoteBranchRevision state remote.Name fetchAuthentication branch context
                    }

            match observedTargetResult with
            | Error failure -> return Failed failure
            | Ok observedTarget ->
                let targetRemote, fetchAuthentication, authentication =
                    resolvedTarget
                    |> Option.defaultWith (fun () -> failwith "The publication target was not resolved.")

                let remoteName = targetRemote.Name

                let expectedMatches =
                    match expectedTarget, observedTarget with
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
                                        expectedTarget
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
                        let reportLfsProgress (progress: GitProgressDto) =
                            context.ReportProgress {
                                PhaseCode = "lfs-upload"
                                Item = None
                                Completed = progress.Processed
                                Total = progress.Total
                                DisplayMessage = progress.Output |> Option.map Redaction.redact
                            }

                        let! publicationPreparation =
                            async {
                                do! barrier state.Hooks state.RepoPath "publish-precheck-done" context
                                do! barrier state.Hooks state.RepoPath "transfer-start" context

                                let! pathResult =
                                    match workspaceRevision with
                                    | Some publishedRevision ->
                                        publicationChangedPaths
                                            state
                                            observedTarget
                                            publishedRevision
                                            context
                                    | None ->
                                        async {
                                            return
                                                Error {
                                                    OperationFailure.create
                                                        ProviderError
                                                        "publish_evidence_failed"
                                                        "The intended publication revision could not be read." with
                                                        Retryable = true
                                                }
                                        }

                                match pathResult with
                                | Error failure ->
                                    return
                                        Error {
                                            failure with
                                                StateChanged = false
                                        }
                                | Ok affectedPaths ->
                                    let! lfsPreparation =
                                        GitService.prepareExplicitLfsPush
                                            state.RepoPath
                                            remoteName
                                            branch
                                            fetchAuthentication
                                            authentication
                                            (Some reportLfsProgress)
                                            context.Cancellation.IsCancellationRequested
                                        |> Async.AwaitPromise

                                    match lfsPreparation with
                                    | Error failure ->
                                        return
                                            Error {
                                                toOperationFailure failure with
                                                    Retryable = true
                                            }
                                    | Ok explicitUploadCompleted ->
                                        return Ok(affectedPaths, explicitUploadCompleted)
                            }

                        match publicationPreparation with
                        | Error failure -> return Failed failure
                        | Ok(affectedPaths, explicitUploadCompleted) ->
                            let pushEnvironment = [|
                                "GIT_TERMINAL_PROMPT", "0"

                                if explicitUploadCompleted then
                                    "GIT_LFS_SKIP_PUSH", "1"
                            |]

                            let! pushResult =
                                runGitEnv
                                    state.Hooks
                                    state.RepoPath
                                    [|
                                        yield! authentication.ConfigArgs
                                        "push"
                                        remoteName
                                        branch
                                    |]
                                    None
                                    pushEnvironment
                                    context

                            let pushFailure =
                                match pushResult with
                                | Error failure -> Some failure
                                | Ok output when output.ExitCode <> 0 ->
                                    let combined = output.StdErr + output.StdOut

                                    let lines =
                                        combined.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
                                        |> Array.map (fun line -> line.Trim())

                                    let concurrencyRejection =
                                        lines
                                        |> Array.exists (fun line ->
                                            line.Contains "[rejected]"
                                            && (line.Contains "(fetch first)"
                                                || line.Contains "(non-fast-forward)"
                                                || line.Contains "(stale info)"
                                                || line.Contains "(cannot lock ref"))

                                    let remoteRejection =
                                        lines
                                        |> Array.exists (fun line ->
                                            line.Contains "[remote rejected]" || line.Contains "[rejected]")

                                    if concurrencyRejection then
                                        Some {
                                            OperationFailure.createRedacted
                                                Concurrency
                                                "precondition_failed"
                                                "The publication target advanced during publish." with
                                                Retryable = true
                                        }
                                    elif remoteRejection then
                                        let remoteReason =
                                            lines
                                            |> Array.choose (fun line ->
                                                if line.StartsWith("remote:", StringComparison.Ordinal) then
                                                    Some(line.Substring("remote:".Length).Trim())
                                                else
                                                    None)
                                            |> String.concat " "

                                        let message =
                                            if String.IsNullOrWhiteSpace remoteReason then
                                                "The remote rejected the publication."
                                            else
                                                $"The remote rejected the publication. {remoteReason}"

                                        Some(
                                            OperationFailure.createRedacted ProviderError "publish_rejected" message
                                            |> OperationFailure.withDetails lines
                                        )
                                    else
                                        Some {
                                            OperationFailure.createRedacted
                                                Network
                                                "target_unreachable"
                                                $"Publishing to {remoteName} failed: {combined}" with
                                                Retryable = true
                                        }
                                | Ok _ -> None

                            // A push response is ambiguous until the exact target ref is read.
                            // This follow-up is deliberately read-only and ignores caller
                            // cancellation so a cancellation cannot conceal an accepted ref.
                            let verificationCancellation = OperationCancellation.Source()
                            let mutable verificationCompleted = false
                            let mutable verificationTimedOut = false

                            Async.StartImmediate(
                                async {
                                    do! Async.Sleep publicationVerificationTimeoutMilliseconds

                                    if not verificationCompleted then
                                        verificationTimedOut <- true
                                        verificationCancellation.Cancel()
                                }
                            )

                            let verificationContext = {
                                context with
                                    Cancellation = verificationCancellation.Cancellation
                            }

                            let! verificationResult =
                                readRemoteBranchRevision
                                    state
                                    remoteName
                                    fetchAuthentication
                                    branch
                                    verificationContext

                            verificationCompleted <- true

                            let verification =
                                if verificationTimedOut then
                                    Error {
                                        OperationFailure.create
                                            Timeout
                                            "publish_verification_timeout"
                                            "Reading the exact remote ref exceeded the publication verification deadline." with
                                            Retryable = true
                                    }
                                else
                                    verificationResult

                            let reconcilePublished = {
                                Code = "retry_publish_verification"
                                Instructions =
                                    Some
                                        "The intended revision is published; refresh and reconcile synchronization state before deciding whether another publish is needed."
                            }

                            let verifyBeforeRetry = {
                                Code = "retry_publish_verification"
                                Instructions =
                                    Some
                                        "Read the exact remote ref and reconcile its revision before retrying publication."
                            }

                            let appendDetails (details: string[]) (failure: OperationFailure) = {
                                failure with
                                    Details =
                                        Array.append failure.Details (details |> Array.map Redaction.redact)
                            }

                            let combineEvidence existing current =
                                Array.append existing current |> Array.distinct

                            let targetEvidence verifiedRevision = [|
                                yield!
                                    observedTarget
                                    |> Option.map (fun revision -> "previous_target", mkRevisionId revision)
                                    |> Option.toList
                                yield!
                                    observedTarget
                                    |> Option.map (fun revision -> "expected_target", mkRevisionId revision)
                                    |> Option.toList
                                yield!
                                    verifiedRevision
                                    |> Option.map (fun revision -> "observed_target", mkRevisionId revision)
                                    |> Option.toList
                            |]

                            let publishedEvidence verifiedRevision = [|
                                yield! targetEvidence verifiedRevision
                                yield!
                                    workspaceRevision
                                    |> Option.map (fun revision -> "published_revision", mkRevisionId revision)
                                    |> Option.toList
                            |]

                            let fallbackState verifiedRevision relationship = {
                                BaseRevision = None
                                WorkspaceRevision = workspaceRevision |> Option.map mkRevisionId
                                TargetRevision = verifiedRevision |> Option.map mkRevisionId
                                TargetRef = None
                                LocalRevisionCount = None
                                TargetRevisionCount = None
                                RemoteChangedPaths = None
                                Relationship = relationship
                            }

                            match verification with
                            | Error verificationFailure ->
                                match pushFailure with
                                | Some originalFailure ->
                                    // Verification could not establish whether the ref changed.
                                    // `StateChanged = true` means "may have changed" here; it
                                    // prevents callers from treating a retry as automatically safe.
                                    let failureWithVerification =
                                        originalFailure
                                        |> appendDetails [|
                                            yield! verificationFailure.Details
                                            $"Remote verification failed ({verificationFailure.Code}): {verificationFailure.Message}"
                                        |]

                                    return
                                        Failed {
                                            failureWithVerification with
                                                StateChanged = true
                                                Retryable = true
                                                RecoveryAction = Some verifyBeforeRetry
                                                RevisionEvidence =
                                                    combineEvidence
                                                        originalFailure.RevisionEvidence
                                                        (targetEvidence None)
                                        }
                                | None ->
                                    return
                                        Failed {
                                            verificationFailure with
                                                StateChanged = true
                                                Retryable = true
                                                RecoveryAction = Some verifyBeforeRetry
                                                RevisionEvidence =
                                                    combineEvidence
                                                        verificationFailure.RevisionEvidence
                                                        (targetEvidence None)
                                        }
                            | Ok verifiedRevision ->
                                let publicationVerified =
                                    match workspaceRevision, verifiedRevision with
                                    | Some expected, Some observed -> expected = observed
                                    | _ -> false

                                match pushFailure with
                                | Some originalFailure when publicationVerified ->
                                    let! stateResult = synchronizationState state verificationContext

                                    let exactState, stateDetails =
                                        match stateResult with
                                        | Ok syncState when publishedRefIsSynchronizationTarget syncState remoteName branch ->
                                            {
                                                syncState with
                                                    BaseRevision = workspaceRevision |> Option.map mkRevisionId
                                                    WorkspaceRevision = workspaceRevision |> Option.map mkRevisionId
                                                    TargetRevision = verifiedRevision |> Option.map mkRevisionId
                                                    RemoteChangedPaths = None
                                                    Relationship = UpToDate
                                            },
                                            [||]
                                        | Ok syncState -> syncState, [||]
                                        | Error failure ->
                                            fallbackState verifiedRevision UpToDate,
                                            [| $"State inspection failed ({failure.Code}): {failure.Message}" |]

                                    let failureWithEvidence =
                                        originalFailure
                                        |> appendDetails stateDetails

                                    let failureAffectedPaths =
                                        Array.append originalFailure.AffectedPaths affectedPaths
                                        |> Array.distinct

                                    return
                                        OperationResult.partiallySucceeded
                                            {
                                                OperationOutcome.performed exactState with
                                                    AffectedPaths = affectedPaths
                                                    Publication = Published
                                                    ResultingRevision = workspaceRevision |> Option.map mkRevisionId
                                            }
                                            {
                                                failureWithEvidence with
                                                    AffectedPaths = failureAffectedPaths
                                                    RevisionEvidence =
                                                        combineEvidence
                                                            originalFailure.RevisionEvidence
                                                            (publishedEvidence verifiedRevision)
                                            }
                                            reconcilePublished
                                | Some originalFailure when verifiedRevision = observedTarget ->
                                    return
                                        Failed {
                                            originalFailure with
                                                StateChanged = false
                                                RevisionEvidence =
                                                    combineEvidence
                                                        originalFailure.RevisionEvidence
                                                        (targetEvidence verifiedRevision)
                                        }
                                | Some originalFailure ->
                                    let racedFailure =
                                        OperationFailure.createRedacted
                                            Concurrency
                                            "precondition_failed"
                                            "The publication target advanced during publish."
                                        |> appendDetails [|
                                            yield! originalFailure.Details
                                            $"Original push failure ({originalFailure.Code}): {originalFailure.Message}"
                                        |]

                                    return
                                        Failed {
                                            racedFailure with
                                                StateChanged = true
                                                Retryable = true
                                                AffectedPaths = originalFailure.AffectedPaths
                                                RevisionEvidence =
                                                    combineEvidence
                                                        originalFailure.RevisionEvidence
                                                        (targetEvidence verifiedRevision)
                                        }
                                | None when publicationVerified ->
                                    let! stateResult = synchronizationState state verificationContext

                                    let syncState, stateFailure =
                                        match stateResult with
                                        | Ok value -> value, None
                                        | Error failure ->
                                            fallbackState verifiedRevision UnknownRelationship, Some failure

                                    let resultingState =
                                        if publishedRefIsSynchronizationTarget syncState remoteName branch then
                                            {
                                                syncState with
                                                    BaseRevision = workspaceRevision |> Option.map mkRevisionId
                                                    WorkspaceRevision = workspaceRevision |> Option.map mkRevisionId
                                                    TargetRevision = verifiedRevision |> Option.map mkRevisionId
                                                    RemoteChangedPaths = None
                                                    Relationship = UpToDate
                                            }
                                        else
                                            syncState

                                    let outcome = {
                                        OperationOutcome.performed resultingState with
                                            AffectedPaths = affectedPaths
                                            Publication = Published
                                            ResultingRevision = workspaceRevision |> Option.map mkRevisionId
                                    }

                                    match stateFailure with
                                    | None -> return Succeeded outcome
                                    | Some failure ->
                                        return
                                            OperationResult.partiallySucceeded
                                                outcome
                                                {
                                                    failure with
                                                        AffectedPaths =
                                                            Array.append failure.AffectedPaths affectedPaths
                                                            |> Array.distinct
                                                        RevisionEvidence =
                                                            combineEvidence
                                                                failure.RevisionEvidence
                                                                (publishedEvidence verifiedRevision)
                                                }
                                                reconcilePublished
                                | None when verifiedRevision = observedTarget ->
                                    return
                                        Failed {
                                            OperationFailure.create
                                                ProviderError
                                                "publish_not_observed"
                                                "The ref push reported success, but the exact remote ref did not change." with
                                                StateChanged = false
                                                Retryable = true
                                                RecoveryAction = Some verifyBeforeRetry
                                                RevisionEvidence = targetEvidence verifiedRevision
                                        }
                                | None ->
                                    return
                                        Failed {
                                            OperationFailure.create
                                                Concurrency
                                                "precondition_failed"
                                                "The publication target advanced to a different revision during publish." with
                                                StateChanged = true
                                                Retryable = true
                                                RecoveryAction = Some verifyBeforeRetry
                                                RevisionEvidence = targetEvidence verifiedRevision
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
        let! mergeHeadResult = tryGetMergeHead state context

        match mergeHeadResult, state.ConflictSession with
        | Error failure, _ -> return Error failure
        | Ok(Some mergeHeadValue), Some(sessionId, version) when
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
        | Ok _, _ -> return Error(GitConflictSession.handleRejection ())
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

let private manualConflictResolutionFailure (path: RepositoryPath) =
    {
        OperationFailure.create
            Unsupported
            "manual_resolution_required"
            $"The conflict at '{RepositoryPath.value path}' contains binary or non-text content and must be resolved manually." with
            AffectedPaths = [| RepositoryPath.value path |]
    }

let private ensureConflictSupportsTextResolution
    (state: SessionState)
    (path: RepositoryPath)
    (context: OperationContext)
    =
    async {
        let! summaryResult = getMergeConflictSummary state context

        match summaryResult with
        | Error failure -> return Error failure
        | Ok None ->
            return
                Error(
                    OperationFailure.create NotFound "conflict_item_not_found" "No active conflict session exists."
                )
        | Ok(Some summary) ->
            match summary.Items |> Array.tryFind (fun item -> item.Path = path) with
            | None ->
                return
                    Error(
                        OperationFailure.create
                            NotFound
                            "conflict_item_not_found"
                            "No unresolved conflict exists for the selected path."
                    )
            | Some item when not item.SupportsResolvedContent ->
                return Error(manualConflictResolutionFailure path)
            | Some _ -> return Ok()
    }

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
                            let! supportResult =
                                ensureConflictSupportsTextResolution state request.Path context

                            match supportResult with
                            | Error failure -> return Failed failure
                            | Ok() ->
                                let readCandidate stage =
                                    async {
                                        let! previewResult =
                                            readConflictStagePreview state stage pathValue context

                                        match previewResult with
                                        | Error failure -> return Error failure
                                        | Ok None -> return Ok None
                                        | Ok(Some(TextPreview content)) -> return Ok(Some content)
                                        | Ok(Some(UnsupportedPreview _)) ->
                                            return Error(manualConflictResolutionFailure request.Path)
                                    }

                                let resolveContent () =
                                    async {
                                        match request.Resolution with
                                        | SupplyResolvedContent content -> return Ok(Some content)
                                        | PickCandidate "workspace" -> return! readCandidate 2
                                        | PickCandidate "target" -> return! readCandidate 3
                                        | PickCandidate "base" -> return! readCandidate 1
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
                            let! identityResult = resolveRevisionIdentity state context

                            match expectedHead, identityResult with
                            | None, _ ->
                                return
                                    Failed(
                                        OperationFailure.create
                                            ProviderError
                                            "git_failure"
                                            "The current head could not be resolved."
                                    )
                            | Some _, Error failure -> return Failed failure
                            | Some expectedHeadValue, Ok identityArguments ->
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
                                                yield! identityArguments
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
                                            let mutable cleanupFailure = None

                                            for stateFile in [ "MERGE_HEAD"; "MERGE_MSG"; "MERGE_MODE" ] do
                                                if cleanupFailure.IsNone then
                                                    let! statePathResult = resolveGitStatePath state stateFile context

                                                    match statePathResult with
                                                    | Error failure -> cleanupFailure <- Some failure
                                                    | Ok(Some path) when NodeFileSystem.existsSync path ->
                                                        try
                                                            NodeFileSystem.unlinkSync path
                                                        with _ ->
                                                            ()
                                                    | Ok _ -> ()

                                            match cleanupFailure with
                                            | Some failure -> return Failed failure
                                            | None ->
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

let private baseContentNotFoundFailure (literalPath: string) =
    OperationFailure.create
        NotFound
        "base_content_not_found"
        $"The path '{literalPath}' is absent from the committed base."

type private BaseBlobProbe = {
    ObjectId: string
    IsBlob: bool
}

let private probeBaseHead
    (state: SessionState)
    (literalPath: string)
    (context: OperationContext)
    : Async<Result<string, OperationFailure>> =
    async {
        let! headProbe =
            runGit
                state.Hooks
                state.RepoPath
                [| "rev-parse"; "--verify"; "--quiet"; "HEAD" |]
                None
                context

        match headProbe with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            return Error(baseContentNotFoundFailure literalPath)
        | Ok headOutput ->
            let headObjectId = headOutput.StdOut.Trim()

            if String.IsNullOrWhiteSpace headObjectId then
                return
                    Error(
                        OperationFailure.createRedacted
                            ProviderError
                            "invalid_git_output"
                            "Git returned an empty HEAD object ID while reading base content."
                    )
            else
                return Ok headObjectId
    }

let private probeBaseBlob
    (state: SessionState)
    (literalPath: string)
    (basePath: string)
    (headObjectId: string)
    (context: OperationContext)
    : Async<Result<BaseBlobProbe, OperationFailure>> =
    async {
        let objectExpression = $"{headObjectId}:{basePath}"
        let! objectProbe =
            runGit
                state.Hooks
                state.RepoPath
                [| "rev-parse"; "--verify"; "--quiet"; objectExpression |]
                None
                context

        match objectProbe with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode <> 0 ->
            return Error(baseContentNotFoundFailure literalPath)
        | Ok objectOutput ->
            let objectId = objectOutput.StdOut.Trim()

            if String.IsNullOrWhiteSpace objectId then
                return
                    Error(
                        OperationFailure.createRedacted
                            ProviderError
                            "invalid_git_output"
                            $"Git returned an empty object ID for base path '{literalPath}'."
                    )
            else
                let! typeProbe =
                    runGit state.Hooks state.RepoPath [| "cat-file"; "-t"; objectId |] None context

                match typeProbe with
                | Error failure -> return Error failure
                | Ok output when output.ExitCode <> 0 ->
                    return
                        Error(
                            OperationFailure.createRedacted
                                ProviderError
                                "git_failure"
                                $"Git could not inspect base content for '{literalPath}'."
                        )
                | Ok output ->
                    return
                        Ok {
                            ObjectId = objectId
                            IsBlob = output.StdOut.Trim() = "blob"
                        }
    }

let private readBaseBlob
    (state: SessionState)
    (literalPath: string)
    (objectName: string)
    (context: OperationContext)
    : Async<Result<obj, OperationFailure>> =
    async {
        let request = {
            NodeProcess.ProcessRequest.create "git" [| "cat-file"; "blob"; objectName |] with
                WorkingDirectory = Some state.RepoPath
                Environment = mergeGitEnvironment [||]
                ProgressPhase = "git"
        }

        let! processResult = runHookedBytesProcess state.Hooks request context

        match processResult with
        | Error failure -> return Error failure
        | Ok processOutput when processOutput.ExitCode <> 0 ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "git_failure"
                        $"Reading Git base content failed: {processOutput.StdErr}"
                )
        | Ok _ when context.Cancellation.IsCancellationRequested() ->
            return Error(comparisonReadCanceledFailure literalPath)
        | Ok processOutput -> return Ok processOutput.StdOut
    }

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
            let rec readStableBase remainingRetries =
                async {
                    let! headResult = probeBaseHead state literalPath context

                    match headResult with
                    | Error failure -> return Failed failure
                    | Ok headObjectId ->
                        let! statusResult =
                            runGit
                                state.Hooks
                                state.RepoPath
                                [|
                                    "-c"
                                    "status.renames=true"
                                    "status"
                                    "--find-renames"
                                    "--porcelain=v2"
                                    "-z"
                                    "--untracked-files=all"
                                |]
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
                            let! observedHeadResult = probeBaseHead state literalPath context

                            match observedHeadResult with
                            | Ok observedHead when
                                not (String.Equals(headObjectId, observedHead, StringComparison.Ordinal))
                                && remainingRetries > 0
                                ->
                                return! readStableBase (remainingRetries - 1)
                            | Ok observedHead when
                                not (String.Equals(headObjectId, observedHead, StringComparison.Ordinal))
                                ->
                                return
                                    Failed {
                                        OperationFailure.create
                                            Concurrency
                                            "precondition_failed"
                                            "HEAD changed repeatedly while reading base content." with
                                            Retryable = true
                                            RevisionEvidence = [|
                                                "expected_head", mkRevisionId headObjectId
                                                "observed_head", mkRevisionId observedHead
                                            |]
                                    }
                            | Error failure when
                                failure.Category = NotFound
                                && remainingRetries > 0
                                ->
                                return! readStableBase (remainingRetries - 1)
                            | Error failure -> return Failed failure
                            | Ok _ ->
                                let basePath = findBasePathFromStatus literalPath statusOutput.StdOut

                                let! objectProbeResult =
                                    probeBaseBlob state literalPath basePath headObjectId context

                                match objectProbeResult with
                                | Error failure -> return Failed failure
                                | Ok probe when
                                    not probe.IsBlob
                                    ||
                                    GitService.isExplicitlyUnsupportedPath literalPath
                                    || GitService.isExplicitlyUnsupportedPath basePath
                                    ->
                                    return
                                        OperationResult.succeeded (
                                            UnsupportedContent(Some $"Unsupported git content for '{literalPath}'.")
                                        )
                                | Ok probe ->
                                    let! baseBlob = readBaseBlob state literalPath probe.ObjectId context

                                    match baseBlob with
                                    | Error failure -> return Failed failure
                                    | Ok buffer when GitService.isLikelyBinaryBuffer buffer ->
                                        return
                                            OperationResult.succeeded (
                                                UnsupportedContent(Some $"Unsupported git content for '{literalPath}'.")
                                            )
                                    | Ok buffer ->
                                        return
                                            OperationResult.succeeded (
                                                TextContent(NodeInterop.bufferToUtf8String buffer)
                                            )
                }

            return! readStableBase 2
    }

let private createTextDiff (state: SessionState) : TextDiffService =
    let mapDiff (context: OperationContext) (operation: unit -> JS.Promise<GitService.GitResult<string>>) =
        async {
            try
                if context.Cancellation.IsCancellationRequested() then
                    return OperationResult.canceled "The Git text diff was canceled."
                else
                    let! result = awaitGit (operation ())

                    if context.Cancellation.IsCancellationRequested() then
                        return OperationResult.canceled "The Git text diff was canceled."
                    else
                        match result with
                        | Ok text -> return OperationResult.succeeded (TextContent text)
                        | Error failure -> return Failed failure
            with error ->
                return
                    Failed(
                        OperationFailure.createRedacted ProviderError "git_failure" error.Message
                    )
        }

    {
        GetDiff =
            fun path context ->
                mapDiff context (fun () -> GitService.getDiff state.RepoPath [| RepositoryPath.value path |])
        GetWordDiff =
            fun path context ->
                mapDiff context (fun () -> GitService.getWordDiff state.RepoPath [| RepositoryPath.value path |])
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

let createSessionWithCredentialsIdentityAndPolicy
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (revisionIdentity: GitCredentialStrategy.GitIdentityStrategy)
    (revisionPolicy: RevisionPolicyStrategy)
    (binding: WorkspaceBinding)
    : WorkspaceSession =
    let state = {
        RepoPath = binding.WorkspaceRoot
        Hooks = hooks
        Lock = MutationLock()
        Location = binding.Location
        ConnectionProfileId = binding.ConnectionProfileId
        Credentials = credentials
        RevisionIdentity = revisionIdentity
        RevisionPolicy = revisionPolicy
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

    let activeConflictFailure () =
        {
            OperationFailure.create
                Conflict
                "conflict_session_active"
                "A conflict session is active. Resolve or cancel it first." with
                StateChanged = false
                RecoveryAction =
                    Some {
                        Code = "resolve_conflict_session"
                        Instructions = Some "Resolve every conflict item, then finalize, or cancel the session."
                    }
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
                                async {
                                    let! guardResult = activeOperationGuard state context

                                    match guardResult with
                                    | Failed failure -> return Failed failure
                                    | Succeeded active when active.Value -> return Failed(activeConflictFailure ())
                                    | Succeeded _ -> return! update state request context
                                    | PartiallySucceeded(_, failure) -> return Failed failure
                                })
                    Publish =
                        fun request context ->
                            withValidatedMutation state request.ExpectedWorkspaceVersion context (fun () ->
                                async {
                                    let! guardResult = activeOperationGuard state context

                                    match guardResult with
                                    | Failed failure -> return Failed failure
                                    | Succeeded active when active.Value -> return Failed(activeConflictFailure ())
                                    | Succeeded _ ->
                                        return! publish state request.ExpectedTargetRevision context
                                    | PartiallySucceeded(_, failure) -> return Failed failure
                                })
                    Synchronize =
                        fun request context ->
                            withValidatedMutation state request.ExpectedWorkspaceVersion context (fun () ->
                                Synchronization.compose
                                    {
                                        HasActiveConflictSession = fun context -> activeOperationGuard state context
                                        Refresh = fun context -> refresh state context
                                        PreviewUpdate = fun syncState context -> previewFromState state syncState context
                                        Update = fun syncState context -> updateFromState state syncState context
                                        Publish =
                                            fun syncState context ->
                                                async {
                                                    let! expected = publicationTargetRevision state syncState context

                                                    match expected with
                                                    | Error failure -> return Failed failure
                                                    | Ok expected -> return! publish state expected context
                                                }
                                    }
                                    request
                                    context)
                }
            ConflictResolution = Some(createConflictService state)
            TextDiff = Some(createTextDiff state)
            // Git LFS is optional: the services exist because the Git provider
            // supports them; their operations report their own dependency status
            // when git-lfs is not installed. Core Git never requires LFS.
            ObjectMaterialization =
                Some(
                    GitLfsExtensions.createObjectMaterialization
                        state.RepoPath
                        state.Credentials
                        state.ConnectionProfileId
                )
            StoragePolicy = Some(GitLfsExtensions.createStoragePolicy state.RepoPath)
            Maintenance =
                Some(
                    GitLfsExtensions.createMaintenance
                        state.RepoPath
                        state.Credentials
                        state.ConnectionProfileId
                )
            RepositoryBrowser = Some(createBrowser state)
    }

let createSessionWithCredentialsAndIdentity
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (revisionIdentity: GitCredentialStrategy.GitIdentityStrategy)
    (binding: WorkspaceBinding)
    : WorkspaceSession =
    createSessionWithCredentialsIdentityAndPolicy
        hooks
        credentials
        revisionIdentity
        RevisionPolicyStrategy.automatic
        binding

let createSessionWithCredentials
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (binding: WorkspaceBinding)
    : WorkspaceSession =
    createSessionWithCredentialsAndIdentity hooks credentials GitCredentialStrategy.anonymousIdentity binding

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

let private validateFactoryLocation (location: RepositoryLocation) =
    match GitService.ensureAllowedRemoteUrl location.ProviderLocation with
    | Ok providerLocation -> Ok { location with ProviderLocation = providerLocation }
    | Error failure ->
        Error(OperationFailure.createRedacted Validation "location_not_allowed" failure.Message)

let private resolvesToSamePath (firstPath: string) (secondPath: string) =
    try
        String.Equals(
            NodePath.resolve [| firstPath |],
            NodePath.resolve [| secondPath |],
            StringComparison.OrdinalIgnoreCase
        )
    with _ ->
        false

let private hasGitEntry (workspaceRoot: string) =
    try
        NodeFileSystem.tryLstatSync (NodePath.join [| workspaceRoot; ".git" |]) |> Option.isSome
    with _ ->
        false

let private adoptionUnsupportedFailure (detail: string) =
    OperationFailure.createRedacted
        Unsupported
        "adoption_unsupported"
        $"The workspace cannot be adopted without guessing because Git cannot parse its repository metadata: {detail}"

/// The URL git will connect to for a location the caller supplied, with insteadOf
/// rewriting applied. The query runs in the process working directory, so global and
/// system configuration take part, plus the local configuration of a repository that
/// happens to enclose that directory. Location verification runs ls-remote from the
/// same directory and sees the same rewriting. Clone starts without a repository and
/// ignores an enclosing one, so a local insteadOf there can scope clone credentials to
/// a host clone never contacts. Hosts running from inside a repository should keep
/// that in mind. When git cannot answer, the location is used as written.
let private expandLocationUrl (hooks: GitSessionHooks) (location: string) (context: OperationContext) : Async<string> =
    async {
        let! result = runGit hooks "." [| "ls-remote"; "--get-url"; location |] None context

        match result with
        | Ok output when output.ExitCode = 0 && not (String.IsNullOrWhiteSpace output.StdOut) ->
            return output.StdOut.Trim()
        | _ -> return location
    }

let createFactoryWithCredentialsIdentityAndPolicy
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (revisionIdentity: GitCredentialStrategy.GitIdentityStrategy)
    (revisionPolicy: RevisionPolicyStrategy)
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
            match validateFactoryLocation request.Location with
            | Error failure -> return Failed failure
            | Ok location ->
                let! effectiveLocation = expandLocationUrl hooks location.ProviderLocation context

                let! authArguments =
                    GitCredentialStrategy.resolveAuthArguments
                        credentials
                        effectiveLocation
                        location.ConnectionProfileId

                let! result =
                    runGit
                        hooks
                        "."
                        [|
                            yield! authArguments
                            "ls-remote"
                            "--"
                            location.ProviderLocation
                        |]
                        None
                        context

                match result with
                | Ok output when output.ExitCode = 0 ->
                    return
                        OperationResult.succeeded {
                            Location = location
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
            let validatedLocationResult =
                match request.Location with
                | None -> Ok None
                | Some location -> validateFactoryLocation location |> Result.map Some

            match validatedLocationResult with
            | Error failure -> return Failed failure
            | Ok requestedLocation ->
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
                    match requestedLocation with
                    | None -> return OperationResult.succeeded (bindingFor normalizedPath (localLocation normalizedPath))
                    | Some location when resolvesToSamePath location.ProviderLocation normalizedPath ->
                        return OperationResult.succeeded (bindingFor normalizedPath location)
                    | Some location ->
                        let! remoteResult =
                            runGitChecked
                                hooks
                                normalizedPath
                                [|
                                    "remote"
                                    "add"
                                    "origin"
                                    "--"
                                    location.ProviderLocation
                                |]
                                None
                                context

                        match remoteResult with
                        | Error failure -> return Failed { failure with StateChanged = true }
                        | Ok _ -> return OperationResult.succeeded (bindingFor normalizedPath location)
        }
    Clone =
        fun request context -> async {
            match validateFactoryLocation request.Location with
            | Error failure -> return Failed failure
            | Ok location ->
                let! effectiveLocation = expandLocationUrl hooks location.ProviderLocation context

                let! authentication =
                    GitCredentialStrategy.resolveCommandAuthentication
                        credentials
                        effectiveLocation
                        location.ConnectionProfileId
                        "origin"

                // Remember what the target looked like so a killed or failed clone can be
                // undone without touching anything the caller already had there. When the
                // target cannot be inspected, nothing is cleaned up later.
                let targetExistedBefore, targetWasEmptyDirectory =
                    try
                        let existed = NodeFileSystem.existsSync request.TargetPath

                        let empty =
                            existed
                            && (let stats = NodeFileSystem.lstatSync request.TargetPath

                                stats.isDirectory ()
                                && not (stats.isSymbolicLink ())
                                && (NodeFileSystem.readdirSync request.TargetPath).Length = 0)

                        existed, empty
                    with _ ->
                        true, false

                let removeCloneResidue () =
                    async {
                        try
                            if not targetExistedBefore then
                                if NodeFileSystem.existsSync request.TargetPath then
                                    do!
                                        NodeFileSystem.rmAsync
                                            request.TargetPath
                                            (NodeFileSystem.RmOptions(
                                                recursive = true,
                                                force = true,
                                                maxRetries = 5,
                                                retryDelay = 100
                                            ))
                                        |> Async.AwaitPromise
                            elif targetWasEmptyDirectory then
                                for entry in NodeFileSystem.readdirSync request.TargetPath do
                                    do!
                                        NodeFileSystem.rmAsync
                                            (NodePath.join [| request.TargetPath; entry |])
                                            (NodeFileSystem.RmOptions(
                                                recursive = true,
                                                force = true,
                                                maxRetries = 5,
                                                retryDelay = 100
                                            ))
                                        |> Async.AwaitPromise

                            return None
                        with error ->
                            return Some error.Message
                    }

                // When the residue could not be removed the failure says so, with the
                // path the caller has to clear, so StateChanged stays truthful.
                let withResidue (failure: OperationFailure) (cleanupError: string option) =
                    match cleanupError with
                    | None -> failure
                    | Some message ->
                        {
                            failure with
                                StateChanged = true
                                RecoveryAction =
                                    Some {
                                        Code = "remove_clone_target"
                                        Instructions =
                                            Some(
                                                Redaction.redact
                                                    $"Remove '{request.TargetPath}' before retrying the clone. Cleanup failed: {message}"
                                            )
                                    }
                        }

                // Large objects stay as pointers during the Git transfer; hydration is a
                // separate step so its failure can be reported as partial success.
                let! result =
                    runGitEnv
                        hooks
                        "."
                        [|
                            yield! authentication.ConfigArgs
                            "clone"
                            "--"
                            location.ProviderLocation
                            request.TargetPath
                        |]
                        None
                        [|
                            "GIT_TERMINAL_PROMPT", "0"
                            "GIT_LFS_SKIP_SMUDGE", "1"
                        |]
                        context

                match result with
                | Error failure ->
                    let! cleanupError = removeCloneResidue ()
                    return Failed(withResidue failure cleanupError)
                | Ok output when output.ExitCode <> 0 ->
                    let combined = output.StdErr + output.StdOut

                    if combined.Contains "already exists and is not an empty directory" then
                        // git refused before writing anything. Whatever is in the target
                        // now belongs to someone else, so nothing is removed.
                        return
                            Failed(
                                OperationFailure.create
                                    Validation
                                    "target_not_empty"
                                    "The clone target directory is not empty."
                            )
                    else
                        let! cleanupError = removeCloneResidue ()

                        return
                            Failed(
                                withResidue
                                    (OperationFailure.createRedacted
                                        ProviderError
                                        "clone_failed"
                                        $"Clone failed: {combined}")
                                    cleanupError
                            )
                | Ok _ ->
                    let binding = bindingFor request.TargetPath location

                    if not request.MaterializeAllObjects then
                        return OperationResult.succeeded binding
                    else
                        // Git transfer succeeded; object hydration failing afterwards is
                        // partial success with a retry action, never an overall error
                        // that hides the changed workspace.
                        let! hydration =
                            runGitEnv
                                hooks
                                request.TargetPath
                                [|
                                    yield! authentication.ConfigArgs
                                    "lfs"
                                    "pull"
                                    "origin"
                                |]
                                None
                                [| "GIT_TERMINAL_PROMPT", "0" |]
                                context

                        match hydration with
                        | Ok hydrationOutput when hydrationOutput.ExitCode = 0 ->
                            return OperationResult.succeeded binding
                        | Ok hydrationOutput ->
                            let detail =
                                if String.IsNullOrWhiteSpace hydrationOutput.StdErr then
                                    hydrationOutput.StdOut
                                else
                                    hydrationOutput.StdErr

                            let failure = hydrationFailure "clone" detail

                            return
                                OperationResult.partiallySucceeded
                                    (OperationOutcome.performed binding)
                                    failure
                                    (materializationRecovery failure)
                        | Error hydrationFailure ->
                            return
                                OperationResult.partiallySucceeded
                                    (OperationOutcome.performed binding)
                                    hydrationFailure
                                    (materializationRecovery hydrationFailure)
        }
    Adopt =
        fun request context ->
            async {
                let adoptParsedRoot workspaceRoot = async {
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

                let! rootResult =
                    runGit hooks request.WorkspaceRoot [| "rev-parse"; "--show-toplevel" |] None context

                match rootResult with
                | Error failure -> return Failed failure
                | Ok rootOutput when rootOutput.ExitCode = 0 ->
                    return! adoptParsedRoot (rootOutput.StdOut.Trim())
                | Ok output when hasGitEntry request.WorkspaceRoot ->
                    let detail = output.StdErr + output.StdOut

                    if detail.Contains("dubious ownership", StringComparison.OrdinalIgnoreCase) then
                        return
                            Failed(
                                OperationFailure.createRedacted
                                    ProviderError
                                    "git_failure"
                                    $"git rev-parse --show-toplevel failed: {detail}"
                            )
                    else
                        let reportedDetail =
                            if String.IsNullOrWhiteSpace detail then
                                "Git exited without diagnostic output."
                            else
                                detail

                        return Failed(adoptionUnsupportedFailure reportedDetail)
                | Ok output ->
                    return
                        Failed(
                            OperationFailure.createRedacted
                                ProviderError
                                "git_failure"
                                $"git rev-parse --show-toplevel failed: {output.StdErr + output.StdOut}"
                        )
            }
    Bind =
        fun request context -> async {
            match validateFactoryLocation request.Location with
            | Error failure -> return Failed failure
            | Ok location ->
                // Attach or re-target: set the origin remote of the existing workspace.
                let! existing = runGit hooks request.WorkspaceRoot [| "remote" |] None context

                let hasOrigin =
                    match existing with
                    | Ok output ->
                        remoteNames output.StdOut
                        |> Array.exists (fun candidate ->
                            String.Equals(candidate, "origin", StringComparison.Ordinal))
                    | _ -> false

                let! result =
                    if hasOrigin then
                        runGitChecked
                            hooks
                            request.WorkspaceRoot
                            [|
                                "remote"
                                "set-url"
                                "origin"
                                "--"
                                location.ProviderLocation
                            |]
                            None
                            context
                    else
                        runGitChecked
                            hooks
                            request.WorkspaceRoot
                            [|
                                "remote"
                                "add"
                                "origin"
                                "--"
                                location.ProviderLocation
                            |]
                            None
                            context

                match result with
                | Error failure -> return Failed failure
                | Ok _ ->
                    // The remote exists in the repository now, so the repository's own answer
                    // is exactly what `git fetch origin` will use.
                    let! effectiveLocation =
                        async {
                            let! result = runGit hooks request.WorkspaceRoot [| "remote"; "get-url"; "origin" |] None context

                            match result with
                            | Ok output when output.ExitCode = 0 && not (String.IsNullOrWhiteSpace output.StdOut) ->
                                return output.StdOut.Trim()
                            | _ -> return location.ProviderLocation
                        }

                    let! authArguments =
                        GitCredentialStrategy.resolveAuthArguments
                            credentials
                            effectiveLocation
                            location.ConnectionProfileId

                    let! _ =
                        runGit hooks request.WorkspaceRoot [| yield! authArguments; "fetch"; "origin" |] None context

                    return OperationResult.succeeded (bindingFor request.WorkspaceRoot location)
        }
    Open =
        fun binding _ -> async {
            return
                OperationResult.succeeded
                    (createSessionWithCredentialsIdentityAndPolicy
                        hooks
                        credentials
                        revisionIdentity
                        revisionPolicy
                        binding)
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

let createFactoryWithCredentialsAndIdentity
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (revisionIdentity: GitCredentialStrategy.GitIdentityStrategy)
    : ProviderFactory =
    createFactoryWithCredentialsIdentityAndPolicy
        hooks
        credentials
        revisionIdentity
        RevisionPolicyStrategy.automatic

let createFactoryWithCredentials
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    : ProviderFactory =
    createFactoryWithCredentialsAndIdentity hooks credentials GitCredentialStrategy.anonymousIdentity

/// Factory with the anonymous credential strategy.
let createFactory (hooks: GitSessionHooks) : ProviderFactory =
    createFactoryWithCredentials hooks GitCredentialStrategy.anonymous
