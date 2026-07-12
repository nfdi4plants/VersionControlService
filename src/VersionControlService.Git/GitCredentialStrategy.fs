/// Injected per-factory/per-connection-profile credential resolution for the
/// v2 Git provider. Anonymous HTTPS, SSH-agent, and local/file remotes need no
/// strategy at all; token flows scope their credential to one host and never
/// place secrets in bindings, requests, or output.
module VersionControlService.Git.GitCredentialStrategy

open Fable.Core

module GitTokenProvider = VersionControlService.Git.GitTokenProvider

/// Resolved credential material for one host. Never serialized.
type GitCredential = {
    Username: string
    Secret: string
}

/// Resolves a credential for (host, connection profile); None means anonymous
/// (public HTTPS, SSH agent, or local remotes).
type GitCredentialStrategy = {
    ResolveCredential: string -> string option -> Async<GitCredential option>
}

/// The default strategy: everything is anonymous.
let anonymous: GitCredentialStrategy = {
    ResolveCredential = fun _ _ -> async { return None }
}

[<Emit("Buffer.from($0, 'utf8').toString('base64')")>]
let private toBase64 (_value: string) : string = jsNative

/// Scoped `-c` arguments injecting a Basic credential for exactly the remote's
/// host. Non-HTTPS remotes (SSH, scp-like, local paths) get no header: git's own
/// transport auth applies.
let buildScopedAuthArguments (remoteUrl: string) (credential: GitCredential option) : string[] =
    match credential with
    | None -> [||]
    | Some resolved ->
        match GitTokenProvider.tryExtractHostFromRemoteUrl remoteUrl with
        | Ok host when remoteUrl.StartsWith "https://" ->
            let basicValue = toBase64 $"{resolved.Username}:{resolved.Secret}"

            [|
                "-c"
                $"http.https://{host}/.extraHeader=Authorization: Basic {basicValue}"
            |]
        | _ -> [||]

/// Resolves the scoped auth arguments for a repository location.
let resolveAuthArguments
    (strategy: GitCredentialStrategy)
    (providerLocation: string)
    (connectionProfileId: string option)
    : Async<string[]> =
    async {
        match GitTokenProvider.tryExtractHostFromRemoteUrl providerLocation with
        | Ok host ->
            let! credential = strategy.ResolveCredential host connectionProfileId
            return buildScopedAuthArguments providerLocation credential
        | Error _ ->
            // Local paths and non-URL remotes are anonymous by construction.
            return [||]
    }
