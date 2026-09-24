namespace VersionControlService.Abstractions

/// Result of probing a directory for provider ownership. Provider metadata on disk
/// is a hint, never a binding; explicit host bindings always win.
type ProbeResult =
    | NotDetected
    | Detected of root: string * confidence: int * bindingHint: string option
    | Invalid of message: string
    | ProbeFailed of OperationFailure

/// Schema-versioned, host-persisted binding of a local workspace root to a provider
/// and repository location. The host stores bindings in its own settings store keyed
/// by normalized workspace root; the library never writes binding files into the
/// managed workspace. Credentials are never serialized into a binding.
type WorkspaceBinding = {
    SchemaVersion: int
    ProviderId: ProviderId
    /// Normalized local workspace root.
    WorkspaceRoot: string
    /// Opaque provider state reference (e.g. a provider-owned workspace branch or index location).
    ProviderStateRef: string option
    Location: RepositoryLocation
    ConnectionProfileId: string option
}

module WorkspaceBinding =

    [<Literal>]
    let CurrentSchemaVersion = 1

/// Access intents a consumer can ask a provider to verify against a repository location.
type AccessIntent =
    | ReadIntent
    | WriteObjectIntent
    | CreateRevisionIntent
    | CreateBranchIntent
    | MergeIntent
    | PublishIntent

/// Result of remote access verification: which requested intents are granted.
type AccessReport = {
    Location: RepositoryLocation
    GrantedIntents: AccessIntent[]
    DeniedIntents: AccessIntent[]
}

type VerifyLocationRequest = {
    Location: RepositoryLocation
    Intents: AccessIntent[]
}

type InitializeRequest = {
    /// Local directory to initialize as a new provider workspace.
    TargetPath: string
    /// Optional repository location the new workspace should target.
    Location: RepositoryLocation option
}

type CloneRequest = {
    Location: RepositoryLocation
    TargetPath: string
    /// Optional exact provider ref to check out after cloning. A provider that cannot honor it
    /// returns an Unsupported failure. It never ignores the ref.
    TargetRef: ProviderRef option
    /// Whether large/lazily-materialized objects are hydrated during clone.
    MaterializeAllObjects: bool
}

/// Registers an existing provider-owned workspace with the host without cloning
/// or mutating the workspace. Provider discovery remains read-only; adoption is
/// explicit and may be unsupported by a provider.
type AdoptRequest = {
    WorkspaceRoot: string
    ConnectionProfileId: string option
}

/// Attaches or re-targets an existing local workspace to a repository location
/// without cloning (the neutral replacement for "connect remote").
type BindRequest = {
    WorkspaceRoot: string
    Location: RepositoryLocation
}

/// Identity of one opened workspace session.
type WorkspaceDescriptor = {
    ProviderId: ProviderId
    /// Normalized local workspace root.
    WorkspaceRoot: string
    Location: RepositoryLocation option
}

/// Installed/missing/incompatible dependency component with remediation instructions.
type DependencyStatus = {
    Component: string
    Installed: bool
    Version: string option
    Compatible: bool
    Remediation: string option
}
