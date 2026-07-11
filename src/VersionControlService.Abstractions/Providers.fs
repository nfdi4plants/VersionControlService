namespace VersionControlService.Abstractions

/// Identity of one opened workspace. Extended by the session/service decomposition.
type WorkspaceDescriptor = {
    ProviderId: ProviderId
    /// Normalized local workspace root.
    WorkspaceRoot: string
    Location: RepositoryLocation option
}

/// Carrier-spike session surface. The full service decomposition (core plus optional
/// services) replaces this shape before the abstractions package is published.
type WorkspaceSession = {
    Descriptor: WorkspaceDescriptor
    /// Pure read returning the opaque optimistic-concurrency workspace version token.
    GetWorkspaceVersion: OperationContext -> Async<OperationResult<string>>
}

/// Carrier-spike factory surface. Probe/VerifyLocation/Initialize/Clone/Bind follow
/// in the provider-contract task.
type ProviderFactory = {
    Id: ProviderId
    Open: WorkspaceDescriptor -> OperationContext -> Async<OperationResult<WorkspaceSession>>
}
