/// Git LFS as optional services: object materialization, storage policy, and
/// local storage maintenance. Core Git never requires these; each operation
/// reports its own dependency status when git-lfs is unavailable.
module internal VersionControlService.Git.GitLfsExtensions

open System
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.GitEngineTypes

module GitService = VersionControlService.Git.GitService
module GitLfsService = VersionControlService.Git.GitLfsService
module GitLfsObjects = VersionControlService.Git.GitLfsObjects
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module GitInternals = VersionControlService.Git.GitInternals
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodePath = VersionControlService.Runtime.Node.Path
module NodeProcess = VersionControlService.Runtime.Node.Process

let private categoryOfKind (kind: GitFailureKind) =
    match kind with
    | GitFailureKind.Unauthorized -> Authentication
    | GitFailureKind.Forbidden -> Authorization
    | GitFailureKind.Network -> Network
    | GitFailureKind.Timeout -> Timeout
    | GitFailureKind.Canceled -> Canceled
    | GitFailureKind.LfsInstallRequired -> DependencyMissing
    | GitFailureKind.LfsBackupRetained -> ProviderError
    | GitFailureKind.InvalidLfsThreshold -> Validation
    | GitFailureKind.RemoteProjectAlreadyExists -> ProviderError
    | GitFailureKind.LocalChanges -> ProviderError
    | GitFailureKind.Unknown -> ProviderError

let private toOperationFailure (failure: GitService.GitFailure) : OperationFailure =
    let mapped =
        match GitInternals.tryIndexLockFailure failure.Message with
        | Some(path, ageSeconds) -> GitInternals.indexLockFailure path ageSeconds
        | None when failure.Kind = GitFailureKind.InvalidLfsThreshold ->
            OperationFailure.create Validation "invalid_lfs_threshold" failure.Message
        | None when failure.Kind = GitFailureKind.LfsBackupRetained ->
            {
                OperationFailure.createRedacted ProviderError "lfs_backup_retained" failure.Message with
                    StateChanged = true
            }
        | None when failure.Kind = GitFailureKind.LocalChanges ->
            OperationFailure.createRedacted
                (categoryOfKind failure.Kind)
                ObjectMaterializationCodes.LocalChanges
                failure.Message
        | None ->
            OperationFailure.createRedacted (categoryOfKind failure.Kind) "lfs_operation_failed" failure.Message

    // A checkout that reports an index error has written its files already.
    if GitLfsService.mentionsIndexUpdateFailure failure.Message then
        { mapped with StateChanged = true }
    else
        mapped

let private toOperationResult (failure: GitService.GitFailure) : OperationResult<unit> =
    if failure.Kind = GitFailureKind.Canceled then
        OperationResult.canceled failure.Message
    else
        Failed(toOperationFailure failure)

let private wrapUnit
    (context: OperationContext)
    (beginOperation: unit -> unit)
    (operation: unit -> JS.Promise<GitService.GitResult<unit>>)
    : Async<OperationResult<unit>> =
    async {
        if context.Cancellation.IsCancellationRequested() then
            return OperationResult.canceled "Git LFS operation canceled."
        else
            beginOperation ()

            if context.Cancellation.IsCancellationRequested() then
                return OperationResult.canceled "Git LFS operation canceled."
            else
                let! result = Async.AwaitPromise(operation ())

                match result with
                | Ok() -> return OperationResult.succeeded ()
                | Error failure -> return toOperationResult failure
    }

let private storagePolicyPaths (relativePath: string) = [| relativePath; ".gitattributes" |]

let private affectedPath (relativePath: string) (failure: OperationFailure) = {
    failure with
        StateChanged = false
        AffectedPaths = [| relativePath |]
}

/// affectedPath for a failure that follows a checkout which wrote the file.
let private changedAffectedPath (relativePath: string) (failure: OperationFailure) =
    {
        affectedPath relativePath failure with
            StateChanged = true
    }

let private bumpRegularFileMtime (repoPath: string) (relativePath: string) : Result<unit, string> =
    try
        let absolutePath = NodePath.resolve [| repoPath; relativePath |]

        match NodeFileSystem.tryLstatSync absolutePath with
        | Some stats when stats.isFile () ->
            let now = DateTime.Now
            NodeFileSystem.utimesSync absolutePath now now
            Ok()
        | _ -> Ok()
    with error ->
        Error(NodeInterop.errorMessage error)

let private completeStoragePolicyChange (repoPath: string) (relativePath: string) : OperationResult<unit> =
    let affectedPaths = storagePolicyPaths relativePath

    match bumpRegularFileMtime repoPath relativePath with
    | Ok() ->
        Succeeded {
            OperationOutcome.performed () with
                AffectedPaths = affectedPaths
        }
    | Error error ->
        Failed {
            OperationFailure.createRedacted
                ProviderError
                "mtime_update_failed"
                $"Git LFS changed the path policy, but could not refresh the file timestamp: {error}" with
                StateChanged = true
                AffectedPaths = affectedPaths
        }

/// The index lock failure when the repository has a lock file, found through the caller's git runner.
/// The lookup runs without the cancellation of the context, because it follows a checkout that ran to
/// its end. A lookup that fails finds nothing.
let private tryHeldIndexLockFailure
    (repoPath: string)
    (runGit: string[] -> OperationContext -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>)
    (context: OperationContext)
    : Async<OperationFailure option> =
    async {
        let lookupContext = {
            context with
                Cancellation = OperationCancellation.none
        }

        let! lockPath = GitLfsService.tryFindHeldIndexLockWith (fun arguments -> runGit arguments lookupContext) repoPath

        return
            lockPath
            |> Option.map (fun path -> GitInternals.indexLockFailure path (GitInternals.tryFileAgeSeconds path))
    }

let private prepareUnmarkAfterPreflight
    (repoPath: string)
    (runGit: string[] -> OperationContext -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>)
    (relativePath: string)
    (context: OperationContext)
    : Async<Result<bool, OperationFailure>> =
    async {
        let! pointerResult = GitLfsObjects.readWorktreePointer repoPath relativePath

        match pointerResult with
        | Error failure -> return Error(affectedPath relativePath failure)
        | Ok None -> return Ok false
        | Ok(Some pointer) ->
            let! mediaDirectoryResult =
                GitLfsObjects.resolveLocalMediaDirectory (fun arguments -> runGit arguments context) repoPath

            match mediaDirectoryResult with
            | Error failure -> return Error(affectedPath relativePath failure)
            | Ok mediaDirectory when
                not (GitLfsObjects.isObjectLocallyAvailable mediaDirectory pointer.Oid pointer.SizeInBytes) ->
                return
                    Error(
                        OperationFailure.create
                            Validation
                            "object_not_local"
                            "Download the file before storing it without Git LFS."
                        |> affectedPath relativePath
                    )
            | Ok _ ->
                // The checkout is local and holds the index lock, so a cancel must not kill it.
                let! checkoutResult =
                    runGit
                        [| "lfs"; "checkout"; "--"; GitLfsObjects.lfsCheckoutPattern relativePath |]
                        { context with
                            Cancellation = OperationCancellation.none }

                match checkoutResult with
                | Error failure ->
                    return
                        Error(
                            OperationFailure.createRedacted
                                ProviderError
                                "object_materialization_failed"
                                failure.Message
                            |> affectedPath relativePath
                        )
                | Ok output when output.ExitCode = 0 ->
                    match GitLfsService.lfsCheckoutIndexUpdateFailed output.StdOut output.StdErr with
                    | Some text ->
                        let! lockFailure = tryHeldIndexLockFailure repoPath runGit context

                        return
                            Error(
                                lockFailure
                                |> Option.defaultWith (fun () ->
                                    OperationFailure.createRedacted ProviderError "object_materialization_failed" text)
                                |> changedAffectedPath relativePath
                            )
                    | None ->
                        let! restoredPointerResult = GitLfsObjects.readWorktreePointer repoPath relativePath

                        match restoredPointerResult with
                        | Error failure -> return Error(affectedPath relativePath failure)
                        | Ok(Some _) ->
                            return
                                Error(
                                    OperationFailure.create
                                        ProviderError
                                        "object_materialization_failed"
                                        "Git LFS did not restore the file's content, so it stays in Git LFS."
                                    |> affectedPath relativePath
                                )
                        | Ok None -> return Ok true
                | Ok output ->
                    let detail =
                        if String.IsNullOrWhiteSpace output.StdErr then
                            $"Git exited with code {output.ExitCode}."
                        else
                            output.StdErr.Trim()

                    return
                        Error(
                            OperationFailure.createRedacted ProviderError "object_materialization_failed" detail
                            |> affectedPath relativePath
                        )
    }

let private prepareUnmark
    (repoPath: string)
    (preflight: OperationContext -> Async<Result<unit, OperationFailure>>)
    (runGit: string[] -> OperationContext -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>)
    (relativePath: string)
    (context: OperationContext)
    : Async<Result<bool, OperationFailure>> =
    async {
        let! preflightResult = preflight context

        match preflightResult with
        | Error failure -> return Error(affectedPath relativePath failure)
        | Ok() -> return! prepareUnmarkAfterPreflight repoPath runGit relativePath context
    }

let private wrapUnitWithPreflight
    (preflight: OperationContext -> Async<Result<unit, OperationFailure>>)
    (context: OperationContext)
    (beginOperation: unit -> unit)
    (operation: unit -> JS.Promise<GitService.GitResult<unit>>)
    : Async<OperationResult<unit>> =
    async {
        if context.Cancellation.IsCancellationRequested() then
            return OperationResult.canceled "Git LFS operation canceled."
        else
            let! preflightResult = preflight context

            match preflightResult with
            | Error failure when failure.Category = Canceled -> return OperationResult.canceled failure.Message
            | Error failure -> return Failed failure
            | Ok() -> return! wrapUnit context beginOperation operation
    }

/// Removes the cached objects of files that were just freed. An object stays when an unpushed commit
/// adds it, because git-lfs needs the local object to push that commit. The same holds for a custom
/// lfs.storage, which other clones can share and which prune refuses too. Every error is ignored,
/// because a cached object that stays only costs disk space.
let deleteCachedObjectsOfFreedPaths
    (repoPath: string)
    (runGit: string[] -> OperationContext -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>)
    (context: OperationContext)
    (relativePaths: string[])
    : Async<unit> =
    async {
        let detachedContext = { context with Cancellation = OperationCancellation.none }

        let hasNoOutput (result: Result<NodeProcess.ProcessOutput, OperationFailure>) =
            match result with
            | Ok output -> output.ExitCode = 0 && String.IsNullOrWhiteSpace output.StdOut
            | Error _ -> false

        try
            let pointers = ResizeArray<GitLfsObjects.LfsPointerInfo>()

            for relativePath in relativePaths do
                let! pointerResult = GitLfsObjects.readWorktreePointer repoPath relativePath

                match pointerResult with
                | Ok(Some pointer) -> pointers.Add pointer
                | Error _
                | Ok None -> ()

            if pointers.Count > 0 then
                let! mediaDirectoryResult =
                    GitLfsObjects.resolveLocalMediaDirectory (fun arguments -> runGit arguments detachedContext) repoPath

                match mediaDirectoryResult with
                | Error _ -> ()
                | Ok mediaDirectory ->
                    let! storageResult = runGit [| "config"; "--get"; "lfs.storage" |] detachedContext

                    match storageResult with
                    | Ok output when not (String.IsNullOrWhiteSpace(output.StdOut.Trim())) -> ()
                    | _ ->
                        // One question answers it for every object when no commit is unpushed.
                        // The search covers HEAD too, so a commit on a detached HEAD keeps its objects.
                        let! unpushedCommitsResult =
                            runGit [| "rev-list"; "-1"; "HEAD"; "--branches"; "--not"; "--remotes=origin" |] detachedContext

                        for pointer in pointers do
                            let! isUnreferenced =
                                async {
                                    if hasNoOutput unpushedCommitsResult then
                                        return true
                                    else
                                        let! unpushedResult =
                                            runGit
                                                [|
                                                    "-c"
                                                    "log.diffMerges=separate"
                                                    "log"
                                                    "-m"
                                                    "HEAD"
                                                    "--branches"
                                                    "--not"
                                                    "--remotes=origin"
                                                    "--format=%H"
                                                    "-1"
                                                    ("-Soid sha256:" + pointer.Oid)
                                                |]
                                                detachedContext

                                        return hasNoOutput unpushedResult
                                }

                            if isUnreferenced then
                                // One object that cannot be removed does not keep the others.
                                try
                                    let objectPath = GitLfsObjects.objectPath mediaDirectory pointer.Oid

                                    if NodeFileSystem.existsSync objectPath then
                                        NodeFileSystem.unlinkSync objectPath
                                with _ ->
                                    ()
        with _ ->
            ()
    }

let createObjectMaterialization
    (repoPath: string)
    (runGit: string[] -> OperationContext -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (preflight: OperationContext -> Async<Result<unit, OperationFailure>>)
    (serialize: (unit -> Async<OperationResult<unit>>) -> Async<OperationResult<unit>>)
    : ObjectMaterializationService = {
    ListObjects =
        fun context -> async {
            if context.Cancellation.IsCancellationRequested() then
                return OperationResult.canceled "Git LFS listing was canceled."
            else
                context.ReportProgress {
                    PhaseCode = "lfs-list"
                    Item = None
                    Completed = None
                    Total = None
                    DisplayMessage = None
                }

                let! listingResult =
                    Async.AwaitPromise(GitLfsService.readLsFilesByRelativePath repoPath context)

                match listingResult with
                | Error message when context.Cancellation.IsCancellationRequested() ->
                    return OperationResult.canceled "Git LFS listing was canceled."
                | Error message ->
                    let category = categoryOfKind (GitService.classifyFailureKind message)

                    return
                        Failed(
                            OperationFailure.createRedacted
                                category
                                "lfs_listing_failed"
                                $"Listing large objects failed: {message}"
                        )
                | Ok filesByPath ->
                    let objects =
                        filesByPath.Values
                        |> Seq.choose (fun file ->
                            match RepositoryPath.tryCreate file.name with
                            | Ok path ->
                                Some {
                                    Path = path
                                    IsMaterialized = file.checkout
                                    IsLocallyAvailable = file.downloaded
                                    SizeBytes = Some file.size
                                    ObjectId = Some file.oid
                                }
                            | Error _ -> None)
                        |> Seq.toArray

                    let totalBytes =
                        objects
                        |> Array.sumBy (fun item -> item.SizeBytes |> Option.defaultValue 0.0)

                    context.ReportProgress {
                        PhaseCode = "lfs-list"
                        Item = None
                        Completed = Some totalBytes
                        Total = Some totalBytes
                        DisplayMessage = None
                    }

                    if context.Cancellation.IsCancellationRequested() then
                        return OperationResult.canceled "Git LFS listing was canceled."
                    else
                        return OperationResult.succeeded objects
        }
    Materialize =
        fun path context ->
            serialize (fun () ->
                wrapUnitWithPreflight
                    preflight
                    context
                    (fun () ->
                        context.ReportProgress {
                            PhaseCode = "lfs-materialize"
                            Item = Some(RepositoryPath.value path)
                            Completed = None
                            Total = None
                            DisplayMessage = None
                        })
                    (fun () ->
                        GitService.downloadLfsFile
                            repoPath
                            (RepositoryPath.value path)
                            credentials
                            connectionProfileId
                            context))
    Dematerialize =
        fun path context ->
            serialize (fun () ->
                wrapUnitWithPreflight
                    preflight
                    context
                    (fun () ->
                        context.ReportProgress {
                            PhaseCode = "lfs-dematerialize"
                            Item = Some(RepositoryPath.value path)
                            Completed = None
                            Total = None
                            DisplayMessage = None
                        })
                    (fun () ->
                        promise {
                            let! result =
                                GitService.freeLocalLfsCopy
                                    repoPath
                                    (RepositoryPath.value path)
                                    credentials
                                    connectionProfileId
                                    context

                            match result with
                            | Ok true ->
                                do!
                                    deleteCachedObjectsOfFreedPaths repoPath runGit context [| RepositoryPath.value path |]
                                    |> Async.StartAsPromise

                                return Ok()
                            | Ok false -> return Ok()
                            | Error failure -> return Error failure
                        }))
}

/// How a batch operation reports an incomplete run.
type internal BatchAction = {
    /// Code of a failure when some paths fail.
    IncompleteCode: string
    /// Recovery of a partial success.
    Recovery: RecoveryAction
}

let internal batchMaterializeAction = {
    IncompleteCode = ObjectMaterializationCodes.BatchMaterializationIncomplete
    Recovery = {
        Code = ObjectMaterializationCodes.RetryMaterializationRecovery
        Instructions = Some "Materialize the path again to replace the pointer with the object's content."
    }
}

let private batchDematerializeAction = {
    IncompleteCode = ObjectMaterializationCodes.BatchDematerializationIncomplete
    Recovery = {
        Code = ObjectMaterializationCodes.RetryDematerializationRecovery
        Instructions = Some "Free the path again after the reported problem is fixed."
    }
}

let private batchNoOpReason = "Every path is already in the requested state."

/// Collapses the outcomes of the paths into one result. The failure of a run with failed paths names
/// them in AffectedPaths and Details. It takes its category and retry flag from the first failure that
/// is not a cancellation, and has the category Canceled only when every failure is a cancellation. The
/// code is operation_canceled when every failure is a cancellation and local_changes when every failure is
/// the refusal of a path with local changes. Any other mix has the code of the action.
let internal toBatchResult (action: BatchAction) (outcomes: GitService.LfsPathOutcome[]) : OperationResult<unit> =
    let changed =
        outcomes
        |> Array.choose (fun outcome ->
            match outcome.Result with
            | Ok GitService.Changed -> Some outcome.Path
            | _ -> None)

    let failed =
        outcomes
        |> Array.choose (fun outcome ->
            match outcome.Result with
            | Error failure -> Some(outcome.Path, failure)
            | Ok _ -> None)

    if failed.Length = 0 then
        if changed.Length = 0 then
            OperationResult.noOp (Some batchNoOpReason) ()
        else
            Succeeded {
                OperationOutcome.performed () with
                    AffectedPaths = changed
            }
    else
        let failures = failed |> Array.map (fun (path, failure) -> path, toOperationFailure failure)
        let allCanceled = failed |> Array.forall (fun (_, failure) -> failure.Kind = GitFailureKind.Canceled)

        let allLocalChanges =
            failed |> Array.forall (fun (_, failure) -> failure.Kind = GitFailureKind.LocalChanges)

        // A cancellation says little about why other paths failed. The first failure that is not a
        // cancellation therefore supplies the message and the category with its retry flag.
        let basis =
            match failed |> Array.tryFindIndex (fun (_, failure) -> failure.Kind <> GitFailureKind.Canceled) with
            | Some index -> snd failures[index]
            | None -> snd failures[0]

        let failure = {
            basis with
                Code =
                    if allCanceled then "operation_canceled"
                    elif allLocalChanges then ObjectMaterializationCodes.LocalChanges
                    else action.IncompleteCode
                Message = $"{failed.Length} of {outcomes.Length} paths failed: {basis.Message}"
                StateChanged = changed.Length > 0 || (failures |> Array.exists (fun (_, item) -> item.StateChanged))
                AffectedPaths = failures |> Array.map fst
                Details = failures |> Array.map (fun (path, item) -> $"{path}: {item.Message}")
        }

        // A lock blocks every later path, so a lock recovery of any failed path replaces the retry
        // and is also the recovery of a run in which every path failed.
        let lockRecovery =
            failures
            |> Array.tryPick (fun (_, item) ->
                item.RecoveryAction |> Option.filter (fun recovery -> recovery.Code = "remove_index_lock"))

        if changed.Length > 0 then
            OperationResult.partiallySucceeded
                {
                    OperationOutcome.performed () with
                        AffectedPaths = changed
                }
                failure
                (lockRecovery |> Option.defaultValue action.Recovery)
        else
            Failed(
                match lockRecovery with
                | Some recovery ->
                    { failure with
                        RecoveryAction = Some recovery }
                | None -> failure
            )

/// Runs a batch operation under the session lock, after the preflight. An empty request needs neither.
let private runBatch
    (preflight: OperationContext -> Async<Result<unit, OperationFailure>>)
    (serialize: (unit -> Async<OperationResult<unit>>) -> Async<OperationResult<unit>>)
    (phaseCode: string)
    (action: BatchAction)
    (operation: string[] -> OperationContext -> JS.Promise<GitService.GitResult<GitService.LfsPathOutcome[]>>)
    (paths: RepositoryPath[])
    (context: OperationContext)
    : Async<OperationResult<unit>> =
    if paths.Length = 0 then
        async { return OperationResult.noOp (Some batchNoOpReason) () }
    else
        serialize (fun () ->
            async {
                if context.Cancellation.IsCancellationRequested() then
                    return OperationResult.canceled "Git LFS operation canceled."
                else
                    let! preflightResult = preflight context

                    match preflightResult with
                    | Error failure when failure.Category = Canceled -> return OperationResult.canceled failure.Message
                    | Error failure -> return Failed failure
                    | Ok() ->
                        context.ReportProgress {
                            PhaseCode = phaseCode
                            Item = None
                            Completed = None
                            Total = None
                            DisplayMessage = None
                        }

                        if context.Cancellation.IsCancellationRequested() then
                            return OperationResult.canceled "Git LFS operation canceled."
                        else
                            let! result =
                                Async.AwaitPromise(operation (paths |> Array.map RepositoryPath.value) context)

                            match result with
                            | Error failure -> return toOperationResult failure
                            | Ok outcomes -> return toBatchResult action outcomes
            })

/// The batch variant of createObjectMaterialization. Both operations read the Git LFS listing once, and the
/// download fetches the objects of many paths together. A path that fails does not stop the others.
let createObjectBatchMaterialization
    (repoPath: string)
    (runGit: string[] -> OperationContext -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    (preflight: OperationContext -> Async<Result<unit, OperationFailure>>)
    (serialize: (unit -> Async<OperationResult<unit>>) -> Async<OperationResult<unit>>)
    : ObjectBatchMaterializationService = {
    Materialize =
        runBatch
            preflight
            serialize
            "lfs-materialize"
            batchMaterializeAction
            (fun requestedPaths context ->
                GitService.downloadLfsFiles
                    repoPath
                    requestedPaths
                    credentials
                    connectionProfileId
                    context.ReportProgress
                    context)
    Dematerialize =
        runBatch
            preflight
            serialize
            "lfs-dematerialize"
            batchDematerializeAction
            (fun requestedPaths context ->
                promise {
                    let! result =
                        GitService.freeLfsFiles
                            repoPath
                            requestedPaths
                            credentials
                            connectionProfileId
                            context.ReportProgress
                            context

                    match result with
                    | Error failure -> return Error failure
                    | Ok outcomes ->
                        // The cleanup also covers a run that stopped on cancellation, because the files
                        // that were already freed stay freed.
                        let freedPaths =
                            outcomes
                            |> Array.choose (fun outcome ->
                                match outcome.Result with
                                | Ok GitService.Changed -> Some outcome.Path
                                | _ -> None)

                        if freedPaths.Length > 0 then
                            do!
                                deleteCachedObjectsOfFreedPaths repoPath runGit context freedPaths
                                |> Async.StartAsPromise

                        return Ok outcomes
                })
}

let createStoragePolicy
    (repoPath: string)
    (runGit: string[] -> OperationContext -> Async<Result<NodeProcess.ProcessOutput, OperationFailure>>)
    (preflight: OperationContext -> Async<Result<unit, OperationFailure>>)
    : StoragePolicyService = {
    SetPathPolicy =
        fun path useLargeObjectStorage context -> async {
            let relativePath = RepositoryPath.value path

            if useLargeObjectStorage then
                let! result = GitLfsService.trackLiteral repoPath relativePath |> Async.AwaitPromise

                match result with
                | Ok() -> return completeStoragePolicyChange repoPath relativePath
                | Error error ->
                    let kind = GitService.classifyFailureKind error

                    return
                        Failed(
                            OperationFailure.createRedacted
                                (categoryOfKind kind)
                                "lfs_operation_failed"
                                error
                        )
            else
                let! preparation = prepareUnmark repoPath preflight runGit relativePath context

                match preparation with
                | Error failure -> return Failed failure
                | Ok wasMaterialized ->
                    let! result = GitLfsService.untrackLiteral repoPath relativePath |> Async.AwaitPromise

                    match result with
                    | Ok() -> return completeStoragePolicyChange repoPath relativePath
                    | Error error ->
                        let kind = GitService.classifyFailureKind error
                        let failure =
                            OperationFailure.createRedacted
                                (categoryOfKind kind)
                                "lfs_operation_failed"
                                error

                        return
                            Failed {
                                failure with
                                    StateChanged = wasMaterialized
                                    AffectedPaths = if wasMaterialized then [| relativePath |] else [||]
                            }
        }
    GetSettings =
        fun _ -> async {
            let! result = Async.AwaitPromise(GitService.getLfsSettings repoPath)

            match result with
            | Ok settings ->
                return
                    OperationResult.succeeded {
                        AutoPolicyThresholdMb = Some settings.AutoTrackThresholdMb
                        MaterializeLargeObjects = settings.DownloadLargeFiles
                    }
            | Error failure -> return Failed(toOperationFailure failure)
        }
    SetSettings =
        fun settings context -> async {
            if settings.AutoPolicyThresholdMb |> Option.exists (fun value -> value <= 0) then
                return
                    Failed(
                        OperationFailure.create
                            Validation
                            "invalid_lfs_threshold"
                            GitService.InvalidLfsThresholdMessage
                    )
            else
                let! currentResult = Async.AwaitPromise(GitService.getLfsSettings repoPath)

                match currentResult with
                | Error failure -> return Failed(toOperationFailure failure)
                | Ok current ->
                    let next: GitLfsSettingsDto = {
                        AutoTrackThresholdMb =
                            settings.AutoPolicyThresholdMb
                            |> Option.defaultValue current.AutoTrackThresholdMb
                        DownloadLargeFiles = settings.MaterializeLargeObjects
                    }

                    return! wrapUnit context ignore (fun () -> GitService.setLfsSettings repoPath next)
        }
}

let private reportMaintenanceProgress (phaseCode: string) (context: OperationContext) (progress: GitProgressDto) =
    context.ReportProgress {
        PhaseCode = phaseCode
        Item = None
        Completed = progress.Processed
        Total = progress.Total
        DisplayMessage = progress.Output |> Option.map Redaction.redact
    }

let private beginMaintenance (phaseCode: string) (context: OperationContext) =
    context.ReportProgress {
        PhaseCode = phaseCode
        Item = None
        Completed = None
        Total = None
        DisplayMessage = None
    }

let private runMaintenance
    phaseCode
    canceledMessage
    (operation:
        GitService.GitProgressCallback option
            -> (unit -> bool)
            -> (unit -> unit)
            -> JS.Promise<GitService.GitResult<string>>)
    (context: OperationContext)
    =
    async {
        if context.Cancellation.IsCancellationRequested() then
            return OperationResult.canceled canceledMessage
        else
            let progress = reportMaintenanceProgress phaseCode context

            let! (result: GitService.GitResult<string>) =
                operation
                    (Some progress)
                    context.Cancellation.IsCancellationRequested
                    (fun () -> beginMaintenance phaseCode context)
                |> Async.AwaitPromise

            match result with
            | Ok output -> return OperationResult.succeeded output
            | Error failure when failure.Kind = GitFailureKind.Canceled ->
                return OperationResult.canceled canceledMessage
            | Error failure -> return Failed(toOperationFailure failure)
    }

let createMaintenance
    (repoPath: string)
    (credentials: GitCredentialStrategy.GitCredentialStrategy)
    (connectionProfileId: string option)
    : StorageMaintenanceService = {
    Prune =
        runMaintenance
            "maintenance-prune"
            "Git LFS cache pruning was canceled."
            (GitService.pruneLfsCacheWithProgressAndCancellation
                repoPath
                credentials
                connectionProfileId)
    Deduplicate =
        runMaintenance
            "maintenance-deduplicate"
            "Git LFS storage deduplication was canceled."
            (GitService.dedupLfsStorageWithProgressAndCancellation repoPath)
}
