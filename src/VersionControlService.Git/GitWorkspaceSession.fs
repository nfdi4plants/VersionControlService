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

[<Emit("Date.now()")>]
let private nowMilliseconds () : float = jsNative

let private publicationVerificationTimeoutMilliseconds = 30_000

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

let private credentialAuthentication (state: SessionState) (remoteName: string) =
    GitCredentialStrategy.resolveCommandAuthentication
        state.Credentials
        state.Location.ProviderLocation
        state.ConnectionProfileId
        remoteName

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
        // The host comes from the bound location, so identity and credentials resolve
        // against the same hub without the host reading git configuration itself.
        let identityRequest: GitCredentialStrategy.RevisionIdentityRequest = {
            WorkspaceRoot = state.RepoPath
            TargetHost =
                VersionControlService.Git.GitAuthAdapter.tryExtractHostFromRemoteUrl state.Location.ProviderLocation
                |> Result.toOption
        }

        let! resolvedIdentity = state.RevisionIdentity.ResolveIdentity identityRequest

        match resolvedIdentity with
        | Some identity when
            String.IsNullOrWhiteSpace identity.Name
            || String.IsNullOrWhiteSpace identity.Email ->
            return Error(identityMissingFailure ())
        | Some identity ->
            return
                Ok [|
                    "-c"
                    $"user.name={identity.Name}"
                    "-c"
                    $"user.email={identity.Email}"
                |]
        | None ->
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

let private resolvePublishRemoteUrl
    (state: SessionState)
    (remoteName: string)
    (context: OperationContext)
    =
    async {
        let! result =
            runGit
                state.Hooks
                state.RepoPath
                [| "config"; "--get"; $"remote.{remoteName}.url" |]
                None
                context

        match result with
        | Error failure -> return Error failure
        | Ok output when output.ExitCode = 0 && not (String.IsNullOrWhiteSpace output.StdOut) ->
            return
                Ok {
                    Name = remoteName
                    Url = output.StdOut.Trim()
                }
        | Ok _ -> return Error(configuredTargetInvalidFailure ())
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

let private revParse (state: SessionState) (reference: string) (context: OperationContext) =
    async {
        let! result =
            runGit state.Hooks state.RepoPath [| "rev-parse"; "--verify"; "--quiet"; reference |] None context

        match result with
        | Ok output when output.ExitCode = 0 -> return Some(output.StdOut.Trim())
        | _ -> return None
    }

/// A merge process killed part-way can leave MERGE_HEAD, a half-applied index and
/// worktree, and its index.lock behind. Cleanup runs on a copy of the context
/// without the cancellation, because the process runner refuses to start anything
/// on a context that is already canceled.
let private recoverCanceledMerge
    (state: SessionState)
    (headBefore: RevisionId option)
    (failure: OperationFailure)
    (context: OperationContext)
    : Async<OperationFailure> =
    async {
        let cleanupContext = {
            context with
                Cancellation = OperationCancellation.none
        }

        let residue (code: string) (instructions: string) = {
            failure with
                StateChanged = true
                RecoveryAction =
                    Some {
                        Code = code
                        Instructions = Some instructions
                    }
        }

        // The lock goes first. git merge --abort cannot run while a dead process's
        // lock is still in place.
        let! lockPath = resolveGitStatePath state "index.lock" cleanupContext

        let lockError =
            match lockPath with
            | Some path when NodeFileSystem.existsSync path ->
                try
                    NodeFileSystem.unlinkSync path
                    None
                with error ->
                    Some error.Message
            | _ -> None

        match lockError with
        | Some message ->
            return
                residue
                    "remove_index_lock"
                    $"Remove the stale index.lock from the repository state and refresh. Cleanup failed: {message}"
        | None ->
            let! mergeHead = tryGetMergeHead state cleanupContext

            match mergeHead with
            | Some _ ->
                let! abortResult = runGit state.Hooks state.RepoPath [| "merge"; "--abort" |] None cleanupContext

                match abortResult with
                | Ok output when output.ExitCode = 0 -> return failure
                | Ok output ->
                    return
                        residue
                            "abort_merge"
                            $"Run git merge --abort in the workspace and refresh. Cleanup failed: {output.StdErr}"
                | Error abortFailure ->
                    return
                        residue
                            "abort_merge"
                            $"Run git merge --abort in the workspace and refresh. Cleanup failed: {abortFailure.Message}"
            | None ->
                // No merge state means either nothing happened or the merge finished
                // before the kill. A moved HEAD tells the two apart.
                let! headAfter = revParse state "HEAD" cleanupContext

                match headBefore, headAfter with
                | Some before, Some after when RevisionId.value before <> after ->
                    return
                        residue
                            "refresh_workspace"
                            "The merge finished before the cancellation took effect. Refresh to load the updated workspace."
                | _ -> return failure
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

                            return Error(previewIndeterminate "target changed paths" detail)
                        | Error failure ->
                            return Error(previewIndeterminate "target changed paths" failure.Message)
                    }
                | _ -> async { return Ok None }

            match remoteChangedResult with
            | Error failure -> return Error failure
            | Ok remoteChanged ->
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

                let! authArguments = credentialArguments state

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

let private previewUpdate (state: SessionState) (context: OperationContext) =
    async {
        let! refreshResult = refresh state context

        match refreshResult with
        | Failed failure -> return Failed failure
        | PartiallySucceeded(_, failure) -> return Failed failure
        | Succeeded outcome ->
            let syncState = outcome.Value

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

let private updateWithIdentity
    (state: SessionState)
    (request: UpdateRequest)
    (context: OperationContext)
    =
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

                let! identityResult = resolveRevisionIdentity state context

                let! mergeResult =
                    match identityResult with
                    | Error failure -> async { return Error failure }
                    | Ok identityArguments ->
                        async {
                            do! barrier state.Hooks state.RepoPath "update-merge" context

                            return!
                                runGitEnv
                                    state.Hooks
                                    state.RepoPath
                                    [|
                                        yield! identityArguments
                                        "merge"
                                        "--no-edit"
                                        targetReference
                                    |]
                                    None
                                    [| "GIT_LFS_SKIP_SMUDGE", "1" |]
                                    context
                        }

                match mergeResult with
                | Error failure when failure.Category = Canceled ->
                    let! recovered = recoverCanceledMerge state syncState.WorkspaceRevision failure context
                    return Failed recovered
                | Error failure -> return Failed failure
                | Ok output when output.ExitCode = 0 ->
                    let! updatedState = synchronizationState state context

                    match updatedState with
                    | Error failure -> return Failed failure
                    | Ok newState ->
                        let! materializationSetting =
                            runGit
                                state.Hooks
                                state.RepoPath
                                [| "config"; "--get"; GitService.MaterializeLargeObjectsKey |]
                                None
                                context

                        let materializeLargeObjects =
                            match materializationSetting with
                            | Ok setting when setting.ExitCode = 0 ->
                                match setting.StdOut.Trim().ToLowerInvariant() with
                                | "true"
                                | "1"
                                | "yes"
                                | "on" -> true
                                | _ -> false
                            | _ -> false

                        if not materializeLargeObjects then
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
                                        let! authentication = credentialAuthentication state remote

                                        let hydrationRef =
                                            syncState.TargetRef
                                            |> Option.bind (fun target ->
                                                let prefix = remote + "/"

                                                if target.Name.StartsWith(prefix, StringComparison.Ordinal) then
                                                    Some(target.Name.Substring prefix.Length)
                                                else
                                                    None)

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
                                    {
                                        Code = "retry_materialization"
                                        Instructions =
                                            Some "Retry downloading large objects once the object store is reachable."
                                    }

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

let private update (state: SessionState) (request: UpdateRequest) (context: OperationContext) =
    updateWithIdentity state request context

let private publish (state: SessionState) (request: PublishRequest) (context: OperationContext) =
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
                        let! authentication =
                            credentialAuthenticationForRemote state remote.Name remote.Url

                        resolvedTarget <- Some(remote, authentication)

                        return!
                            readRemoteBranchRevision state remote.Name authentication branch context
                    }

            match observedTargetResult with
            | Error failure -> return Failed failure
            | Ok observedTarget ->
                let targetRemote, authentication =
                    resolvedTarget
                    |> Option.defaultWith (fun () -> failwith "The publication target was not resolved.")

                let remoteName = targetRemote.Name

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

                                    if combined.Contains "rejected" || combined.Contains "non-fast-forward" then
                                        Some {
                                            OperationFailure.createRedacted
                                                Concurrency
                                                "precondition_failed"
                                                "The publication target advanced during publish." with
                                                Retryable = true
                                        }
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
                                    authentication
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
                                        | Ok syncState ->
                                            {
                                                syncState with
                                                    BaseRevision = workspaceRevision |> Option.map mkRevisionId
                                                    WorkspaceRevision = workspaceRevision |> Option.map mkRevisionId
                                                    TargetRevision = verifiedRevision |> Option.map mkRevisionId
                                                    RemoteChangedPaths = None
                                                    Relationship = UpToDate
                                            },
                                            [||]
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

                                    let exactState = {
                                        syncState with
                                            BaseRevision = workspaceRevision |> Option.map mkRevisionId
                                            WorkspaceRevision = workspaceRevision |> Option.map mkRevisionId
                                            TargetRevision = verifiedRevision |> Option.map mkRevisionId
                                            RemoteChangedPaths = None
                                            Relationship = UpToDate
                                    }

                                    let outcome = {
                                        OperationOutcome.performed exactState with
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

let createSessionWithCredentialsAndIdentity
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (revisionIdentity: GitCredentialStrategy.GitIdentityStrategy)
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

let createFactoryWithCredentialsAndIdentity
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (revisionIdentity: GitCredentialStrategy.GitIdentityStrategy)
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
                let! authArguments =
                    GitCredentialStrategy.resolveAuthArguments
                        credentials
                        location.ProviderLocation
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
                let! authentication =
                    GitCredentialStrategy.resolveCommandAuthentication
                        credentials
                        location.ProviderLocation
                        location.ConnectionProfileId
                        "origin"

                // Remember what the target looked like so a killed or failed clone can be
                // undone without touching anything the caller already had there.
                let targetExistedBefore = NodeFileSystem.existsSync request.TargetPath

                let targetWasEmptyDirectory =
                    targetExistedBefore
                    && (let stats = NodeFileSystem.lstatSync request.TargetPath

                        stats.isDirectory ()
                        && not (stats.isSymbolicLink ())
                        && (NodeFileSystem.readdirSync request.TargetPath).Length = 0)

                let removeCloneResidue () =
                    async {
                        try
                            if not targetExistedBefore then
                                if NodeFileSystem.existsSync request.TargetPath then
                                    do!
                                        NodeFileSystem.rmAsync
                                            request.TargetPath
                                            (NodeFileSystem.RmOptions(recursive = true, force = true))
                                        |> Async.AwaitPromise
                            elif targetWasEmptyDirectory then
                                for entry in NodeFileSystem.readdirSync request.TargetPath do
                                    do!
                                        NodeFileSystem.rmAsync
                                            (NodePath.join [| request.TargetPath; entry |])
                                            (NodeFileSystem.RmOptions(recursive = true, force = true))
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
                                            Some
                                                $"Remove '{request.TargetPath}' before retrying the clone. Cleanup failed: {message}"
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
                    let! cleanupError = removeCloneResidue ()

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

                            return
                                OperationResult.partiallySucceeded
                                    (OperationOutcome.performed binding)
                                    (hydrationFailure "clone" detail)
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
                    let! _ = runGit hooks request.WorkspaceRoot [| "fetch"; "origin" |] None context
                    return OperationResult.succeeded (bindingFor request.WorkspaceRoot location)
        }
    Open =
        fun binding _ -> async {
            return
                OperationResult.succeeded
                    (createSessionWithCredentialsAndIdentity hooks credentials revisionIdentity binding)
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

let createFactoryWithCredentials
    (hooks: GitSessionHooks)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    : ProviderFactory =
    createFactoryWithCredentialsAndIdentity hooks credentials GitCredentialStrategy.anonymousIdentity

/// Factory with the anonymous credential strategy.
let createFactory (hooks: GitSessionHooks) : ProviderFactory =
    createFactoryWithCredentials hooks GitCredentialStrategy.anonymous
