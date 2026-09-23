namespace VersionControlService.Abstractions

/// Diff/preview content: binary or otherwise unsupported content is data, not an exception.
type ContentView =
    | TextContent of content: string
    | UnsupportedContent of reason: string option

/// Optional text-diff extension.
type TextDiffService = {
    GetDiff: RepositoryPath -> OperationContext -> Async<OperationResult<ContentView>>
    GetWordDiff: RepositoryPath -> OperationContext -> Async<OperationResult<ContentView>>
    /// Committed/base content of a path; providers own base revision lookup.
    GetBaseContent: RepositoryPath -> OperationContext -> Async<OperationResult<ContentView>>
}

/// Materialization state of one lazily-hydrated object.
type ObjectState = {
    Path: RepositoryPath
    IsMaterialized: bool
    /// Whether the object bytes are available in provider-local storage.
    IsLocallyAvailable: bool
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

/// Optional large-object storage-policy extension over literal repository paths.
type StoragePolicyService = {
    /// True marks the path as a large object. False records an explicit opt-out that
    /// outranks the automatic policy of later revisions until the path is marked again.
    /// The path is reported as affected, and the next revision that includes it stores it
    /// under the new policy.
    SetPathPolicy: RepositoryPath -> bool -> OperationContext -> Async<OperationResult<unit>>
    GetSettings: OperationContext -> Async<OperationResult<StoragePolicySettings>>
    SetSettings: StoragePolicySettings -> OperationContext -> Async<OperationResult<unit>>
}

/// How one selected regular file is stored in the revision being created. Only providers
/// with a large-object representation (Git with Git LFS) act on it. Providers without one
/// accept the strategy and never call it.
[<RequireQualifiedAccess>]
type RevisionPathPolicy =
    /// The provider decides from its automatic threshold and its own rules.
    | Automatic
    /// The plain content, even above the threshold and even when a provider rule says large object.
    | Inline
    /// Large-object storage, even below the threshold.
    | LargeObject

/// What the strategy sees for each selected regular file. SizeInBytes is the size of the
/// content that would be committed, so for a file that is a recognized large-object
/// reference it is the declared payload size, not the length of the reference.
type RevisionPathPolicyRequest = {
    Path: RepositoryPath
    SizeInBytes: float
}

/// Immutable configuration handed to a factory at creation. The function must be fast,
/// deterministic and free of side effects. It must not touch the file system or the network.
type RevisionPolicyStrategy = {
    ResolvePathPolicy: RevisionPathPolicyRequest -> RevisionPathPolicy
}

module RevisionPolicyStrategy =

    let automatic: RevisionPolicyStrategy = {
        ResolvePathPolicy = fun _ -> RevisionPathPolicy.Automatic
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
