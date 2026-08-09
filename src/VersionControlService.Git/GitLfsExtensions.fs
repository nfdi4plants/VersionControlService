/// Git LFS as optional services: object materialization, storage policy, and
/// local storage maintenance. Core Git never requires these; each operation
/// reports its own dependency status when git-lfs is unavailable.
module internal VersionControlService.Git.GitLfsExtensions

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.GitEngineTypes

module GitService = VersionControlService.Git.GitService
module GitLfsService = VersionControlService.Git.GitLfsService

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

let private toOperationFailure (failure: GitService.GitFailure) : OperationFailure =
    OperationFailure.createRedacted (categoryOfKind failure.Kind) "lfs_operation_failed" failure.Message

let private wrapUnit (operation: JS.Promise<GitService.GitResult<unit>>) : Async<OperationResult<unit>> =
    async {
        let! result = Async.AwaitPromise operation

        match result with
        | Ok() -> return OperationResult.succeeded ()
        | Error failure -> return Failed(toOperationFailure failure)
    }

let createObjectMaterialization (repoPath: string) : ObjectMaterializationService = {
    ListObjects =
        fun _ -> async {
            // Operational failures stay classified failures — never an empty
            // successful list that hides them (GIT-014).
            let! listingResult = Async.AwaitPromise(GitLfsService.readLsFilesByRelativePath repoPath)

            match listingResult with
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

                return OperationResult.succeeded objects
        }
    Materialize = fun path _ -> wrapUnit (GitService.downloadLfsFile repoPath (RepositoryPath.value path))
    Dematerialize = fun path _ -> wrapUnit (GitService.freeLocalLfsCopy repoPath (RepositoryPath.value path))
}

let createStoragePolicy (repoPath: string) : StoragePolicyService = {
    SetPathPolicy =
        fun path useLargeObjectStorage _ -> async {
            let relativePath = RepositoryPath.value path

            let! (result: Result<unit, string>) =
                if useLargeObjectStorage then
                    GitLfsService.trackLiteral repoPath relativePath
                else
                    GitLfsService.untrackLiteral repoPath relativePath
                |> Async.AwaitPromise

            match result with
            | Ok() -> return OperationResult.succeeded ()
            | Error error ->
                return
                    Failed(OperationFailure.createRedacted DependencyMissing "lfs_operation_failed" (string error))
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
        fun settings _ -> async {
            if settings.AutoPolicyThresholdMb |> Option.exists (fun value -> value <= 0) then
                return
                    Failed(
                        OperationFailure.create
                            Validation
                            "invalid_lfs_threshold"
                            "The automatic LFS threshold must be a positive whole MiB value."
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

                    return! wrapUnit (GitService.setLfsSettings repoPath next)
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

let createMaintenance (repoPath: string) : StorageMaintenanceService = {
    Prune =
        runMaintenance
            "maintenance-prune"
            "Git LFS cache pruning was canceled."
            (GitService.pruneLfsCacheWithProgressAndCancellation repoPath)
    Deduplicate =
        runMaintenance
            "maintenance-deduplicate"
            "Git LFS storage deduplication was canceled."
            (GitService.dedupLfsStorageWithProgressAndCancellation repoPath)
}
