module VersionControlService.LakeFs.LakeFsProviderOptions

/// Host-owned location for all provider state. It must remain outside every
/// managed workspace and is never serialized into a workspace directory.
type LakeFsProviderOptions = {
    StateRoot: string
}
