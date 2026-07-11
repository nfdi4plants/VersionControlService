namespace VersionControlService.Abstractions

/// Relationship between the workspace revisions and the configured target.
type RevisionRelationship =
    | UpToDate
    | LocalAhead
    | TargetAhead
    | Diverged
    | NoTarget
    | UnknownRelationship

/// Neutral synchronization state: explicit revisions plus optional relationship data.
/// Counts are optional because object-oriented providers cannot always compute them.
type SynchronizationState = {
    BaseRevision: RevisionId option
    WorkspaceRevision: RevisionId option
    TargetRevision: RevisionId option
    LocalRevisionCount: int option
    TargetRevisionCount: int option
    /// Advisory cache from the most recent Refresh/PreviewUpdate; may be None or
    /// stale. Never a substitute for running PreviewUpdate.
    RemoteChangedPaths: RepositoryPath[] option
    Relationship: RevisionRelationship
}

/// Authoritative result for preview UI before an Update.
type UpdatePreview = {
    /// Paths changed on the target side.
    ChangedPaths: RepositoryPath[]
    /// Target-side changes overlapping local dirty paths.
    OverlappingPaths: RepositoryPath[]
    HasDataLossRisk: bool
    WouldCreateConflictSession: bool
}

type UpdateRequest = {
    ExpectedWorkspaceVersion: string
}

type PublishRequest = {
    ExpectedWorkspaceVersion: string
    /// Consumer-observed target revision for the provider's client-side
    /// check-then-act-then-verify sequence. Never a server-side precondition.
    ExpectedTargetRevision: RevisionId option
}

/// Synchronization expressed as intent; providers own the mechanics.
type SynchronizationService = {
    /// Observe target state without changing workspace content.
    Refresh: OperationContext -> Async<OperationResult<SynchronizationState>>
    /// Return changed/overlapping paths, data-loss risk, and whether a conflict session would be created.
    PreviewUpdate: OperationContext -> Async<OperationResult<UpdatePreview>>
    /// Incorporate the target into the provider workspace.
    Update: UpdateRequest -> OperationContext -> Async<OperationResult<SynchronizationState>>
    /// Make workspace revisions visible on the configured target.
    Publish: PublishRequest -> OperationContext -> Async<OperationResult<SynchronizationState>>
}
