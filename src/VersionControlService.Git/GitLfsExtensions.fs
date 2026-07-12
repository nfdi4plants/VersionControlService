/// Git LFS as optional v2 services: object materialization, storage policy, and
/// local storage maintenance. Core Git never requires these; each operation
/// reports its own dependency status when git-lfs is unavailable.
module VersionControlService.Git.GitLfsExtensions

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Contracts.Git

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
            // v1 behavior (recorded GIT-014): listing errors currently surface as
            // an empty successful list; the listing-failures cycle preserves them.
            let! filesByPath = Async.AwaitPromise(GitLfsService.tryGetLsFilesByRelativePath repoPath)

            let objects =
                filesByPath.Values
                |> Seq.choose (fun file ->
                    match RepositoryPath.tryCreate file.name with
                    | Ok path ->
                        Some {
                            Path = path
                            IsMaterialized = file.downloaded
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
        fun pattern useLargeObjectStorage _ -> async {
            let command =
                if useLargeObjectStorage then
                    GitLfsCommand.Track
                else
                    GitLfsCommand.Untrack

            let request = GitLfsService.createRequest repoPath command (Some pattern) None
            let! result = Async.AwaitPromise(GitLfsService.runSilently request)

            match result with
            | Ok _ -> return OperationResult.succeeded ()
            | Error error ->
                return
                    Failed(OperationFailure.createRedacted DependencyMissing "lfs_operation_failed" error.Message)
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

let createMaintenance (repoPath: string) : StorageMaintenanceService = {
    Prune =
        fun _ -> async {
            let! result = Async.AwaitPromise(GitService.pruneLfsCache repoPath)

            match result with
            | Ok output -> return OperationResult.succeeded output
            | Error failure -> return Failed(toOperationFailure failure)
        }
    Deduplicate =
        fun _ -> async {
            let! result = Async.AwaitPromise(GitService.dedupLfsStorage repoPath)

            match result with
            | Ok output -> return OperationResult.succeeded output
            | Error failure -> return Failed(toOperationFailure failure)
        }
}
