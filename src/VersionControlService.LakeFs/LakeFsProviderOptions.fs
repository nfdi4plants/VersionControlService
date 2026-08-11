module VersionControlService.LakeFs.LakeFsProviderOptions

open VersionControlService.Abstractions

/// Host-owned location for all provider state. It must remain outside every
/// managed workspace and is never serialized into a workspace directory.
type LakeFsProviderOptions = {
    StateRoot: string
    PathCaseSensitivity: PathCaseSensitivity
}
