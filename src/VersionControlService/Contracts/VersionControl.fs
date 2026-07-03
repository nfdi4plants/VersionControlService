module VersionControlService.Contracts.VersionControl

open Fable.Core

[<RequireQualifiedAccess>]
type VersionControlProviderKind =
    | Git
    | LakeFs

[<RequireQualifiedAccess>]
type VersionControlFailureKind =
    | Unauthorized
    | Forbidden
    | Network
    | Timeout
    | Canceled
    | DependencyMissing
    | RemoteProjectAlreadyExists
    | Conflict
    | Unsupported
    | NotApplicable
    | Unknown

type VersionControlFailure = {
    Kind: VersionControlFailureKind
    Message: string
}

[<RequireQualifiedAccess>]
type VersionControlEffect =
    | Performed of message: string option
    | NoOp of reason: string option

type VersionControlOutcome<'T> = {
    Value: 'T
    Effect: VersionControlEffect
}

type VersionControlResult<'T> = Result<VersionControlOutcome<'T>, VersionControlFailure>

module VersionControlOutcome =

    let performed (value: 'T) = {
        Value = value
        Effect = VersionControlEffect.Performed None
    }

    let performedWithMessage (message: string option) (value: 'T) = {
        Value = value
        Effect = VersionControlEffect.Performed message
    }

    let noOp (reason: string option) (value: 'T) = {
        Value = value
        Effect = VersionControlEffect.NoOp reason
    }

module VersionControlResult =

    let performed (value: 'T) : VersionControlResult<'T> =
        Ok(VersionControlOutcome.performed value)

    let performedWithMessage message (value: 'T) : VersionControlResult<'T> =
        Ok(VersionControlOutcome.performedWithMessage message value)

    let noOp reason (value: 'T) : VersionControlResult<'T> =
        Ok(VersionControlOutcome.noOp reason value)

    let unsupported (message: string) : VersionControlResult<'T> =
        Error {
            Kind = VersionControlFailureKind.Unsupported
            Message = message
        }

    let notApplicable (message: string) : VersionControlResult<'T> =
        Error {
            Kind = VersionControlFailureKind.NotApplicable
            Message = message
        }

[<RequireQualifiedAccess>]
type VersionControlChangeKind =
    | Added
    | Modified
    | Deleted
    | Renamed
    | Copied
    | TypeChanged
    | Untracked
    | Conflicted
    | Unknown

type VersionControlFileStatusDto = {
    Path: string
    OriginalPath: string option
    ChangeKind: VersionControlChangeKind
    IsConflicted: bool
    CanCommit: bool
    CanDiscard: bool
}

type VersionControlBranchRefDto = {
    RefName: string
    DisplayLabel: string
    IsCurrent: bool
    CanCheckout: bool
    ProviderRef: string
}

type VersionControlDiffViewDataDto = {
    Path: string
    PreviousContent: string
    CurrentContent: string
    WordDiffText: string
}

type VersionControlMergeConflictViewDataDto = {
    Path: string
    MergeConflictContent: string
}

type VersionControlUnsupportedContentDto = {
    Path: string
    Reason: string option
}

[<RequireQualifiedAccess>]
type VersionControlPageLoadResultDto<'T> =
    | Loaded of 'T
    | Unsupported of VersionControlUnsupportedContentDto

type VersionControlStatusDto = {
    Current: string option
    Tracking: string option
    Ahead: int
    Behind: int
    IsClean: bool
    Conflicted: string[]
    IsMergeInProgress: bool
    Files: VersionControlFileStatusDto[]
}

type VersionControlDiffSummaryDto = {
    Changed: int
    Insertions: int
    Deletions: int
}

type VersionControlProgressDto = {
    Method: string option
    Stage: string option
    Progress: float option
    Processed: float option
    Total: float option
    Output: string option
}

type VersionControlProgressCallback = VersionControlProgressDto -> unit

type VersionControlRemoteOperationRequest = {
    Remote: string option
    Branch: string option
}

[<RequireQualifiedAccess>]
type VersionControlPullPreflightStatus =
    | SafeToPull
    | WouldRequireMergeResolution
    | Indeterminate

type VersionControlPullPreflightResult = {
    Status: VersionControlPullPreflightStatus
    Message: string option
}

type VersionControlPullResult = { Warning: VersionControlFailure option }

type VersionControlRemoteConfigRequest = {
    RemoteName: string
    RemoteUrl: string
}

type VersionControlCloneRepositoryRequest = {
    RemoteUrl: string
    TargetPath: string
    Branch: string option
    DownloadLargeObjects: bool
}

type VersionControlProviderOption = {
    Key: string
    Value: string
}

type VersionControlInitializeWorkspaceRequest = {
    TargetPath: string
    ProviderRemoteUri: string option
    ProviderOptions: VersionControlProviderOption[]
}

type VersionControlPathsRequest = { Paths: string[] }

type VersionControlObjectRequest = { Path: string }

type VersionControlCommitRequest = {
    Message: string
    Paths: string[]
}

type VersionControlLargeObjectSettings = {
    LargeFileThresholdMb: int option
    DownloadLargeObjects: bool option
}

type VersionControlLargeObjectInfo = {
    Path: string
    SizeBytes: float
    IsMaterialized: bool
    IsDownloaded: bool
    ObjectId: string option
    ObjectIdType: string option
    ProviderMetadataVersion: string option
}

type VersionControlLargeFilePolicyRequest = {
    Path: string
    UseLargeObjectStorage: bool
}

type VersionControlCapabilities = {
    SupportsInitializeWorkspace: bool
    SupportsSelectedPathCommit: bool
    SupportsPullPreflight: bool
    SupportsMergeConflictResolution: bool
    SupportsLargeFilePolicySelection: bool
    SupportsLargeFileThreshold: bool
    SupportsDownloadLargeObjectsToggle: bool
    SupportsDownloadLargeObject: bool
    SupportsFreeLocalObjectCopy: bool
    SupportsStoragePrune: bool
    SupportsStorageDeduplication: bool
}

type VersionControlCreateBranchRequest = {
    Name: string
    BaseProviderRef: string option
}

type VersionControlCheckoutBranchRequest = { ProviderRef: string }

type VersionControlConfirmMergeResolutionRequest = {
    Path: string
    ExpectedConflictContent: string
    ResolvedContent: string
    AutoCommit: bool
}

type VersionControlConfirmMergeResolutionResult = {
    UpdatedStatus: VersionControlStatusDto
    RemainingConflictedPaths: string[]
    NextConflictedPath: string option
}

type VersionControlProvider = {
    Kind: VersionControlProviderKind
    Capabilities: VersionControlCapabilities
    CheckRequirements: unit -> JS.Promise<VersionControlResult<unit>>
    InstallRequirements: unit -> JS.Promise<VersionControlResult<unit>>
    GetStatus: string -> JS.Promise<VersionControlResult<VersionControlStatusDto>>
    GetBranches: string -> JS.Promise<VersionControlResult<VersionControlBranchRefDto[]>>
    GetRepositoryWebUrl: string -> JS.Promise<VersionControlResult<string option>>
    GetLargeObjectSettings: string -> JS.Promise<VersionControlResult<VersionControlLargeObjectSettings>>
    SetLargeObjectSettings: string -> VersionControlLargeObjectSettings -> JS.Promise<VersionControlResult<unit>>
    ListLargeObjects: string -> JS.Promise<VersionControlResult<VersionControlLargeObjectInfo[]>>
    PreviewPull:
        string ->
        VersionControlRemoteOperationRequest ->
        VersionControlProgressCallback option ->
            JS.Promise<VersionControlResult<VersionControlPullPreflightResult>>
    Fetch:
        string ->
        VersionControlRemoteOperationRequest ->
        VersionControlProgressCallback option ->
            JS.Promise<VersionControlResult<unit>>
    Pull:
        string ->
        VersionControlRemoteOperationRequest ->
        VersionControlProgressCallback option ->
            JS.Promise<VersionControlResult<VersionControlPullResult>>
    Push:
        string ->
        VersionControlRemoteOperationRequest ->
        VersionControlProgressCallback option ->
            JS.Promise<VersionControlResult<unit>>
    CancelPush: string -> VersionControlResult<unit>
    InitializeWorkspace: VersionControlInitializeWorkspaceRequest -> JS.Promise<VersionControlResult<string>>
    CloneRepository:
        VersionControlCloneRepositoryRequest ->
        VersionControlProgressCallback option ->
            JS.Promise<VersionControlResult<string>>
    ConnectRemote: string -> VersionControlRemoteConfigRequest -> JS.Promise<VersionControlResult<unit>>
    Commit: string -> VersionControlCommitRequest -> JS.Promise<VersionControlResult<string>>
    Discard: string -> VersionControlPathsRequest -> JS.Promise<VersionControlResult<unit>>
    CreateBranch: string -> VersionControlCreateBranchRequest -> JS.Promise<VersionControlResult<unit>>
    CheckoutBranch: string -> VersionControlCheckoutBranchRequest -> JS.Promise<VersionControlResult<unit>>
    GetDiffSummary: string -> JS.Promise<VersionControlResult<VersionControlDiffSummaryDto>>
    GetWordDiff: string -> VersionControlPathsRequest -> JS.Promise<VersionControlResult<string>>
    GetDiffViewData:
        string ->
        string ->
            JS.Promise<VersionControlResult<VersionControlPageLoadResultDto<VersionControlDiffViewDataDto>>>
    GetMergeConflictViewData:
        string ->
        string ->
            JS.Promise<VersionControlResult<VersionControlPageLoadResultDto<VersionControlMergeConflictViewDataDto>>>
    ConfirmMergeResolution:
        string ->
        VersionControlConfirmMergeResolutionRequest ->
            JS.Promise<VersionControlResult<VersionControlConfirmMergeResolutionResult>>
    SetPathLargeFilePolicy: string -> VersionControlLargeFilePolicyRequest -> JS.Promise<VersionControlResult<unit>>
    DownloadLargeObject: string -> VersionControlObjectRequest -> JS.Promise<VersionControlResult<unit>>
    FreeLocalObjectCopy: string -> VersionControlObjectRequest -> JS.Promise<VersionControlResult<unit>>
    PruneStorage: string -> VersionControlProgressCallback option -> JS.Promise<VersionControlResult<string>>
    DeduplicateStorage:
        string -> VersionControlProgressCallback option -> JS.Promise<VersionControlResult<string>>
}
