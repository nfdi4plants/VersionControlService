namespace VersionControlService.Abstractions

/// Diff/preview content: binary or otherwise unsupported content is data, not an exception.
type ContentView =
    | TextContent of content: string
    | UnsupportedContent of reason: string option

/// Optional text-diff extension.
type TextDiffService = {
    GetDiff: RepositoryPath -> OperationContext -> Async<OperationResult<ContentView>>
    GetWordDiff: RepositoryPath -> OperationContext -> Async<OperationResult<ContentView>>
}

/// Materialization state of one lazily-hydrated object.
type ObjectState = {
    Path: RepositoryPath
    IsMaterialized: bool
    SizeBytes: float option
    ObjectId: string option
}

/// Optional object-materialization extension (Git LFS hydration, lakeFS object download).
type ObjectMaterializationService = {
    ListObjects: OperationContext -> Async<OperationResult<ObjectState[]>>
    Materialize: RepositoryPath -> OperationContext -> Async<OperationResult<unit>>
    Dematerialize: RepositoryPath -> OperationContext -> Async<OperationResult<unit>>
}

type StoragePolicySettings = {
    /// Threshold in megabytes above which new objects use large-object storage, when supported.
    AutoPolicyThresholdMb: int option
    /// Whether large objects are materialized during clone/update by default.
    MaterializeLargeObjects: bool
}

/// Optional large-object storage-policy extension. Patterns are provider policy
/// patterns (e.g. Git attributes), not literal repository paths; any visible file
/// the policy changes is reported in AffectedPaths.
type StoragePolicyService = {
    SetPathPolicy: string -> bool -> OperationContext -> Async<OperationResult<unit>>
    GetSettings: OperationContext -> Async<OperationResult<StoragePolicySettings>>
    SetSettings: StoragePolicySettings -> OperationContext -> Async<OperationResult<unit>>
}

/// Optional local storage maintenance extension.
type StorageMaintenanceService = {
    Prune: OperationContext -> Async<OperationResult<string>>
    Deduplicate: OperationContext -> Async<OperationResult<string>>
}

/// Optional repository browser extension.
type RepositoryBrowserService = {
    GetRepositoryWebUrl: OperationContext -> Async<OperationResult<string option>>
}
