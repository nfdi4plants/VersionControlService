namespace VersionControlService.Abstractions

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
