/// Injected lakeFS credential resolution: a connection profile ID resolves to
/// endpoint + access keys. Secrets never enter bindings, indexes, or requests.
module VersionControlService.LakeFs.LakeFsCredentials

open VersionControlService.LakeFs.LakeFsTypes

type LakeFsCredentialStrategy = {
    /// Resolves the full connection (endpoint + keys) for a connection profile.
    ResolveConnection: string option -> Async<Result<LakeFsConnection, string>>
}

/// A strategy over one fixed connection (tests, single-profile hosts).
let fixedConnection (connection: LakeFsConnection) : LakeFsCredentialStrategy = {
    ResolveConnection = fun _ -> async { return Ok connection }
}

/// A strategy that refuses everything: no profile configured.
let unconfigured: LakeFsCredentialStrategy = {
    ResolveConnection =
        fun profileId ->
            async {
                let profile = profileId |> Option.defaultValue "<none>"
                return Error $"No lakeFS connection is configured for profile '{profile}'."
            }
}
