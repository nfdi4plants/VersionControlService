module VersionControlService.Git.GitProvider

open System
open System.Text.RegularExpressions
open Fable.Core
open VersionControlService.Contracts.FileSystem
open VersionControlService.Contracts.Git
open VersionControlService.Contracts.VersionControl
open VersionControlService.Git

let private versionPattern = Regex(@"(\d+)\.(\d+)(?:\.(\d+))?")

let private isAtLeast (major, minor, patch) (minMajor, minMinor, minPatch) =
    major > minMajor
    || (major = minMajor && minor > minMinor)
    || (major = minMajor && minor = minMinor && patch >= minPatch)

let private hasMinVersion minimum (output: string option) =
    let m = versionPattern.Match(defaultArg output "")

    if m.Success then
        isAtLeast
            (Int32.Parse m.Groups.[1].Value,
             Int32.Parse m.Groups.[2].Value,
             if m.Groups.[3].Success then Int32.Parse m.Groups.[3].Value else 0)
            minimum
    else
        false

let capabilities = {
    SupportsInitializeWorkspace = true
    SupportsSelectedPathCommit = true
    SupportsPullPreflight = true
    SupportsContentMergeResolution = true
    SupportsVersionPickMergeResolution = false
    SupportsDiffLineCounts = true
    SupportsWordDiff = true
    SupportsLargeFilePolicySelection = true
    SupportsLargeFileThreshold = true
    SupportsDownloadLargeObjectsToggle = true
    SupportsDownloadLargeObject = true
    SupportsFreeLocalObjectCopy = true
    SupportsStoragePrune = true
    SupportsStorageDeduplication = true
}

let private toFailureKind =
    function
    | GitFailureKind.Unauthorized -> VersionControlFailureKind.Unauthorized
    | GitFailureKind.Forbidden -> VersionControlFailureKind.Forbidden
    | GitFailureKind.Network -> VersionControlFailureKind.Network
    | GitFailureKind.Timeout -> VersionControlFailureKind.Timeout
    | GitFailureKind.Canceled -> VersionControlFailureKind.Canceled
    | GitFailureKind.LfsInstallRequired -> VersionControlFailureKind.DependencyMissing
    | GitFailureKind.RemoteProjectAlreadyExists -> VersionControlFailureKind.RemoteProjectAlreadyExists
    | GitFailureKind.Unknown -> VersionControlFailureKind.Unknown

let private toFailure (failure: GitService.GitFailure) = {
    Kind = toFailureKind failure.Kind
    Message = failure.Message
}

let private toDeduplicateStorageFailure (failure: GitService.GitFailure) =
    if
        failure.Message.Contains(
            "does not support de-duplication",
            StringComparison.OrdinalIgnoreCase
        )
    then
        {
            Kind = VersionControlFailureKind.NotApplicable
            Message = failure.Message
        }
    else
        toFailure failure

let private toFailureFromException (error: exn) = {
    Kind = GitService.classifyFailureKind error.Message |> toFailureKind
    Message = error.Message
}

let private wrapGitResult (map: 'T -> 'U) (operation: JS.Promise<GitService.GitResult<'T>>) =
    promise {
        match! operation with
        | Ok value -> return VersionControlResult.performed (map value)
        | Error failure -> return Error(toFailure failure)
    }

let private wrapGitUnit operation = wrapGitResult id operation

let private normalizedStatusCodes (file: GitFileStatusDto) =
    [| file.Index; file.WorkingDir |]
    |> Array.choose (fun code ->
        let normalized =
            code
            |> Option.ofObj
            |> Option.map _.Trim()
            |> Option.defaultValue String.Empty

        if String.IsNullOrWhiteSpace normalized then
            None
        else
            Some normalized
    )

let private toChangeKind isConflicted (file: GitFileStatusDto) =
    let codes = normalizedStatusCodes file
    let has code = codes |> Array.exists ((=) code)

    if isConflicted then
        VersionControlChangeKind.Conflicted
    elif has "R" then
        VersionControlChangeKind.Renamed
    elif has "C" then
        VersionControlChangeKind.Copied
    elif has "A" then
        VersionControlChangeKind.Added
    elif has "D" then
        VersionControlChangeKind.Deleted
    elif has "T" then
        VersionControlChangeKind.TypeChanged
    elif has "?" then
        VersionControlChangeKind.Untracked
    elif has "M" then
        VersionControlChangeKind.Modified
    else
        VersionControlChangeKind.Unknown

let private toFileStatus (conflictedPaths: Set<string>) (file: GitFileStatusDto) : VersionControlFileStatusDto =
    let isConflicted = conflictedPaths |> Set.contains file.Path

    {
        Path = file.Path
        OriginalPath = file.OriginalPath
        ChangeKind = toChangeKind isConflicted file
        IsConflicted = isConflicted
        CanCommit = not isConflicted
        CanDiscard = true
    }

let private toStatus (status: GitStatusDto) : VersionControlStatusDto =
    let conflictedPaths = status.Conflicted |> Set.ofArray

    {
        Current = status.Current
        Tracking = status.Tracking
        Ahead = status.Ahead
        Behind = status.Behind
        IsClean = status.IsClean
        Conflicted = status.Conflicted
        IsMergeInProgress = status.IsMergeInProgress
        Files = status.Files |> Array.map (toFileStatus conflictedPaths)
    }

let private toBranchRef (branch: GitBranchRefDto) : VersionControlBranchRefDto = {
    RefName = branch.RefName
    DisplayLabel = branch.DisplayLabel
    IsCurrent = branch.IsCurrent
    CanCheckout = true
    ProviderRef =
        match branch.Kind with
        | GitBranchRefKind.Local -> $"git-local:{branch.RefName}"
        | GitBranchRefKind.Remote -> $"git-remote:{branch.RefName}"
}

let private toDiffSummary (summary: GitDiffSummaryDto) : VersionControlDiffSummaryDto = {
    Changed = summary.Changed
    Insertions = Some summary.Insertions
    Deletions = Some summary.Deletions
}

let private toProgress (progress: GitProgressDto) : VersionControlProgressDto = {
    Method = progress.Method
    Stage = progress.Stage
    Progress = progress.Progress
    Processed = progress.Processed
    Total = progress.Total
    Output = progress.Output
}

let private fromProgressCallback (progress: VersionControlProgressCallback option) : GitService.GitProgressCallback option =
    progress |> Option.map (fun callback -> toProgress >> callback)

let private toPullPreflightStatus =
    function
    | GitPullPreflightStatus.SafeToPull -> VersionControlPullPreflightStatus.SafeToPull
    | GitPullPreflightStatus.WouldRequireMergeResolution ->
        VersionControlPullPreflightStatus.WouldRequireMergeResolution
    | GitPullPreflightStatus.Indeterminate -> VersionControlPullPreflightStatus.Indeterminate

let private toPullPreflight (result: GitPullPreflightResult) : VersionControlPullPreflightResult = {
    Status = toPullPreflightStatus result.Status
    Message = result.Message
}

let private toPullResult (result: GitService.GitPullResult) : VersionControlPullResult = {
    Warning = result.Warning |> Option.map toFailure
}

let private toDiffViewData (data: GitDiffViewDataDto) : VersionControlDiffViewDataDto = {
    Path = data.Path
    PreviousContent = data.PreviousContent
    CurrentContent = data.CurrentContent
    WordDiffText = data.WordDiffText
}

let private toMergeConflictViewData (data: GitMergeConflictViewDataDto) : VersionControlMergeConflictViewDataDto = {
    Path = data.Path
    MergeConflictContent = data.MergeConflictContent
}

let private toUnsupportedContent (unsupported: GitUnsupportedContentDto) : VersionControlUnsupportedContentDto = {
    Path = unsupported.Path
    Reason = unsupported.Reason
}

let private wrapGitPageLoadResult
    (requestedPath: string)
    (map: 'T -> 'U)
    (operation: JS.Promise<GitService.GitResult<'T>>)
    : JS.Promise<VersionControlResult<VersionControlPageLoadResultDto<'U>>> =
    promise {
        match! operation with
        | Ok value ->
            return
                value
                |> map
                |> VersionControlPageLoadResultDto.Loaded
                |> VersionControlResult.performed
        | Error failure ->
            match GitService.tryGetUnsupportedGitContent requestedPath failure with
            | Some unsupported ->
                return
                    unsupported
                    |> toUnsupportedContent
                    |> VersionControlPageLoadResultDto.Unsupported
                    |> VersionControlResult.performed
            | None -> return Error(toFailure failure)
    }

let private toConfirmMergeResolutionResult
    (result: GitConfirmMergeResolutionResult)
    : VersionControlConfirmMergeResolutionResult = {
    UpdatedStatus = toStatus result.UpdatedStatus
    RemainingConflictedPaths = result.RemainingConflictedPaths
    NextConflictedPath = result.NextConflictedPath
}

let private tryTrimPrefix (prefix: string) (value: string) =
    if value.StartsWith(prefix, StringComparison.Ordinal) then
        Some(value.Substring(prefix.Length))
    else
        None

let private localBranchNameFromRemoteRef (remoteRef: string) =
    let slashIndex = remoteRef.IndexOf("/", StringComparison.Ordinal)

    if slashIndex >= 0 && slashIndex + 1 < remoteRef.Length then
        remoteRef.Substring(slashIndex + 1)
    else
        remoteRef

let private fromGitProviderRef (providerRef: string) =
    providerRef
    |> tryTrimPrefix "git-remote:"
    |> Option.orElseWith (fun () -> providerRef |> tryTrimPrefix "git-local:")
    |> Option.defaultValue providerRef

let private toCheckoutRequest (request: VersionControlCheckoutBranchRequest) : GitCheckoutBranchRequest =
    match request.ProviderRef |> tryTrimPrefix "git-remote:" with
    | Some remoteRef -> {
        Name = localBranchNameFromRemoteRef remoteRef
        StartPoint = Some remoteRef
      }
    | None -> {
        Name = fromGitProviderRef request.ProviderRef
        StartPoint = None
      }

let private toLargeObjectInfo (info: GitLfsLsFileInfo) : VersionControlLargeObjectInfo = {
    Path = info.name
    SizeBytes = info.size
    IsMaterialized = info.checkout
    IsDownloaded = info.downloaded
    ObjectId =
        if String.IsNullOrWhiteSpace info.oid then
            None
        else
            Some info.oid
    ObjectIdType =
        if String.IsNullOrWhiteSpace info.``oid_type`` then
            None
        else
            Some info.``oid_type``
    ProviderMetadataVersion =
        if String.IsNullOrWhiteSpace info.version then
            None
        else
            Some info.version
}

let private checkRequirements () = promise {
    let! gitVersion = GitLfsAdapter.tryExecGitText None 5000 [| "--version" |]
    let! gitLfsVersion = GitLfsAdapter.tryExecGitText None 5000 [| "lfs"; "--version" |]

    if hasMinVersion (2, 32, 0) gitVersion && hasMinVersion (3, 7, 0) gitLfsVersion then
        return VersionControlResult.performed ()
    else
        return
            Error {
                Kind = VersionControlFailureKind.DependencyMissing
                Message = "Git provider requires Git 2.32.0+ and Git LFS 3.7.0+ (`git lfs --version`)."
            }
}

let private installRequirements () = promise {
    match! GitLfsService.installSystem () with
    | Ok() -> return VersionControlResult.performed ()
    | Error message ->
        return
            Error {
                Kind = VersionControlFailureKind.DependencyMissing
                Message = message
            }
}

let private getLargeObjectSettings repoPath = promise {
    match! GitService.getLfsSettings repoPath with
    | Ok settings ->
        return
            VersionControlResult.performed {
                LargeFileThresholdMb = Some settings.AutoTrackThresholdMb
                DownloadLargeObjects = Some settings.DownloadLargeFiles
            }
    | Error failure -> return Error(toFailure failure)
}

let private setLargeObjectSettings repoPath settings = promise {
    match! GitService.getLfsSettings repoPath with
    | Error failure -> return Error(toFailure failure)
    | Ok current ->
        let next: GitLfsSettingsDto = {
            AutoTrackThresholdMb =
                settings.LargeFileThresholdMb
                |> Option.defaultValue current.AutoTrackThresholdMb
            DownloadLargeFiles =
                settings.DownloadLargeObjects
                |> Option.defaultValue current.DownloadLargeFiles
        }

        return! wrapGitUnit (GitService.setLfsSettings repoPath next)
}

let private initializeWorkspace request = promise {
    if String.IsNullOrWhiteSpace request.TargetPath then
        return
            Error {
                Kind = VersionControlFailureKind.Unknown
                Message = "Workspace target path must not be empty."
            }
    else
        return! wrapGitResult id (GitProvisioningService.initRepository request.TargetPath)
}

let private listLargeObjects repoPath = promise {
    let! filesByPath = GitLfsService.tryGetLsFilesByRelativePath repoPath

    return
        filesByPath.Values
        |> Seq.map toLargeObjectInfo
        |> Seq.toArray
        |> VersionControlResult.performed
}

let private setPathLargeFilePolicy repoPath request = promise {
    let command =
        if request.UseLargeObjectStorage then
            GitLfsCommand.Track
        else
            GitLfsCommand.Untrack

    let gitLfsRequest = GitLfsService.createRequest repoPath command (Some request.Path) None

    match! GitLfsService.runSilently gitLfsRequest with
    | Ok _ -> return VersionControlResult.performed ()
    | Error error -> return Error(toFailureFromException error)
}

let private isStagedIndexStatus (indexStatus: string) =
    let normalized =
        indexStatus
        |> Option.ofObj
        |> Option.map _.Trim()
        |> Option.defaultValue String.Empty

    not (String.IsNullOrWhiteSpace normalized) && normalized <> "?"

let private commit repoPath request = promise {
    let normalizedMessage =
        request.Message
        |> Option.ofObj
        |> Option.map _.Trim()
        |> Option.defaultValue String.Empty

    if String.IsNullOrWhiteSpace normalizedMessage then
        return
            Error {
                Kind = VersionControlFailureKind.Unknown
                Message = "Commit message must not be empty."
            }
    elif isNull request.Paths || request.Paths.Length = 0 then
        return
            Error {
                Kind = VersionControlFailureKind.Unknown
                Message = "At least one path is required for commit."
            }
    else
        match! GitService.getStatus repoPath with
        | Error failure -> return Error(toFailure failure)
        | Ok status ->
            let stagedPaths =
                status.Files
                |> Array.filter (fun file -> isStagedIndexStatus file.Index)
                |> Array.map _.Path
                |> Array.distinct

            let! unstageResult =
                if stagedPaths.Length = 0 then
                    promise { return Ok() }
                else
                    GitService.unstagePaths repoPath stagedPaths

            match unstageResult with
            | Error failure -> return Error(toFailure failure)
            | Ok() ->
                match! GitService.stagePaths repoPath request.Paths with
                | Error failure -> return Error(toFailure failure)
                | Ok() ->
                    return!
                        GitService.commit repoPath normalizedMessage
                        |> wrapGitResult id
}

let create () : VersionControlProvider = {
    Kind = VersionControlProviderKind.Git
    Capabilities = capabilities
    CheckRequirements = checkRequirements
    InstallRequirements = installRequirements
    GetStatus = fun repoPath -> wrapGitResult toStatus (GitService.getStatus repoPath)
    GetBranches =
        fun repoPath ->
            GitService.getBranches repoPath
            |> wrapGitResult (Array.map toBranchRef)
    GetRepositoryWebUrl =
        fun repoPath -> wrapGitResult id (GitService.getOriginRepositoryWebUrl repoPath)
    GetLargeObjectSettings = getLargeObjectSettings
    SetLargeObjectSettings = setLargeObjectSettings
    ListLargeObjects = listLargeObjects
    PreviewPull =
        fun repoPath request progress ->
            GitService.previewPull repoPath request.Remote request.Branch (fromProgressCallback progress)
            |> wrapGitResult toPullPreflight
    Fetch =
        fun repoPath request progress ->
            GitService.fetch repoPath request.Remote request.Branch (fromProgressCallback progress)
            |> wrapGitUnit
    Pull =
        fun repoPath request progress ->
            GitService.pull repoPath request.Remote request.Branch (fromProgressCallback progress)
            |> wrapGitResult toPullResult
    Push =
        fun repoPath request progress ->
            GitService.push repoPath request.Remote request.Branch (fromProgressCallback progress)
            |> wrapGitUnit
    CancelPush =
        fun repoPath ->
            match GitService.cancelPush repoPath with
            | Ok() -> VersionControlResult.performed ()
            | Error failure -> Error(toFailure failure)
    InitializeWorkspace = initializeWorkspace
    CloneRepository =
        fun request progress ->
            GitProvisioningService.cloneRepository
                request.RemoteUrl
                request.TargetPath
                request.Branch
                request.DownloadLargeObjects
                (fromProgressCallback progress)
            |> wrapGitResult id
    ConnectRemote =
        fun repoPath request ->
            GitService.addRemote repoPath request.RemoteName request.RemoteUrl
            |> wrapGitUnit
    Commit = commit
    Discard =
        fun repoPath request ->
            GitService.discardPaths repoPath request.Paths
            |> wrapGitUnit
    CreateBranch =
        fun repoPath request ->
            let baseRef = request.BaseProviderRef |> Option.map fromGitProviderRef
            GitService.createBranch repoPath request.Name baseRef
            |> wrapGitUnit
    CheckoutBranch =
        fun repoPath request ->
            GitService.checkoutBranch repoPath (toCheckoutRequest request)
            |> wrapGitUnit
    GetDiffSummary =
        fun repoPath -> wrapGitResult toDiffSummary (GitService.getDiffSummary repoPath)
    GetWordDiff =
        fun repoPath request -> wrapGitResult id (GitService.getWordDiff repoPath request.Paths)
    GetDiffViewData =
        fun repoPath requestedPath ->
            GitService.getDiffViewData repoPath requestedPath
            |> wrapGitPageLoadResult requestedPath toDiffViewData
    GetMergeConflictViewData =
        fun repoPath requestedPath ->
            GitService.getMergeConflictViewData repoPath requestedPath
            |> wrapGitPageLoadResult requestedPath toMergeConflictViewData
    ConfirmMergeResolution =
        fun repoPath request ->
            GitService.confirmMergeResolution
                repoPath
                request.Path
                request.ExpectedConflictContent
                request.ResolvedContent
                request.AutoCommit
            |> wrapGitResult toConfirmMergeResolutionResult
    SetPathLargeFilePolicy = setPathLargeFilePolicy
    DownloadLargeObject =
        fun repoPath request -> GitService.downloadLfsFile repoPath request.Path |> wrapGitUnit
    FreeLocalObjectCopy =
        fun repoPath request -> GitService.freeLocalLfsCopy repoPath request.Path |> wrapGitUnit
    PruneStorage =
        fun repoPath progress ->
            GitService.pruneLfsCacheWithProgress repoPath (fromProgressCallback progress)
            |> wrapGitResult id
    DeduplicateStorage =
        fun repoPath progress ->
            promise {
                match! GitService.dedupLfsStorageWithProgress repoPath (fromProgressCallback progress) with
                | Ok output -> return VersionControlResult.performed output
                | Error failure -> return Error(toDeduplicateStorageFailure failure)
            }
}
