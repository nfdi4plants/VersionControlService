namespace VersionControlService.Abstractions

type FileChangeKind =
    | AddedChange
    | ModifiedChange
    | DeletedChange
    | RenamedChange
    | ConflictedChange

/// One changed workspace object. Renames report old and new literal paths.
type FileChange = {
    Path: RepositoryPath
    OldPath: RepositoryPath option
    Kind: FileChangeKind
}

type LogicalRefKind =
    | LocalRef
    | RemoteRef

/// A logical branch/ref shown to consumers. ProviderRef is opaque and carried
/// back to the provider unchanged.
type LogicalRef = {
    Name: string
    ProviderRef: ProviderRef
    Kind: LogicalRefKind
    IsCurrent: bool
}

/// Truthful neutral status. WorkspaceVersion is the opaque optimistic-concurrency
/// token: pure reads over an unchanged workspace return the same token.
type WorkspaceStatus = {
    CurrentRef: LogicalRef option
    WorkspaceVersion: string
    Changes: FileChange[]
    ActiveConflictSession: ConflictSessionSummary option
    Synchronization: SynchronizationState option
}

type CreateRefRequest = {
    Name: string
    /// Exact base ref; the provider must never silently retarget it.
    BaseRef: ProviderRef option
    /// Explicit create-only or create-and-switch behavior.
    SwitchTo: bool
    ExpectedWorkspaceVersion: string
}

type SwitchRefRequest = {
    TargetRef: ProviderRef
    ExpectedWorkspaceVersion: string
}

/// Destructive-change preflight for switching refs.
type SwitchPreflight = {
    /// Local changes that would be lost or need materialization decisions.
    PathsAtRisk: RepositoryPath[]
    IsSafe: bool
}

type CreateRevisionRequest = {
    Message: string
    /// Exact literal repository paths; never interpreted as patterns.
    Paths: RepositoryPath[]
    ExpectedWorkspaceVersion: string
}

type RestoreRequest = {
    /// Exact literal repository paths to restore.
    Paths: RepositoryPath[]
    ExpectedWorkspaceVersion: string
}

/// Object-level diff entry; line counts stay optional because object-oriented
/// providers cannot always compute them cheaply.
type DiffEntry = {
    Path: RepositoryPath
    OldPath: RepositoryPath option
    Kind: FileChangeKind
    LineInsertions: int option
    LineDeletions: int option
}

type DiffSummary = {
    Entries: DiffEntry[]
}

/// The required core every provider session supplies.
type CoreVersionControl = {
    GetStatus: OperationContext -> Async<OperationResult<WorkspaceStatus>>
    ListRefs: OperationContext -> Async<OperationResult<LogicalRef[]>>
    CreateRef: CreateRefRequest -> OperationContext -> Async<OperationResult<LogicalRef>>
    PreflightSwitchRef: SwitchRefRequest -> OperationContext -> Async<OperationResult<SwitchPreflight>>
    SwitchRef: SwitchRefRequest -> OperationContext -> Async<OperationResult<WorkspaceStatus>>
    /// Creates a revision from exact selected paths and an expected workspace version.
    CreateRevision: CreateRevisionRequest -> OperationContext -> Async<OperationResult<RevisionId>>
    RestorePaths: RestoreRequest -> OperationContext -> Async<OperationResult<unit>>
    GetDiffSummary: OperationContext -> Async<OperationResult<DiffSummary>>
}
