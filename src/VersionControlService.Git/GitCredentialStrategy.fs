/// Injected per-factory/per-connection-profile credential resolution for the
/// Git provider. Anonymous HTTPS, SSH-agent, and local/file remotes need no
/// strategy at all; token flows scope their credential to one host and never
/// place secrets in bindings, requests, or output.
module VersionControlService.Git.GitCredentialStrategy

open System.Text.RegularExpressions
open Fable.Core

module GitAuthAdapter = VersionControlService.Git.GitAuthAdapter

/// Resolved credential material for one host. Never serialized.
type GitCredential = {
    Username: string
    Secret: string
}

type RevisionIdentity = {
    Name: string
    Email: string
}

/// What the application receives when the Git provider asks it for a revision
/// identity. TargetHost is the DNS host of the remote the revision will be published
/// to: the configured publish remote of the current branch when there is one,
/// otherwise the bound location. It is None for local paths. ConnectionProfileId is
/// the session profile, the same value credential resolution receives, so an
/// application that keeps several accounts per host can pick the matching one.
type RevisionIdentityRequest = {
    WorkspaceRoot: string
    TargetHost: string option
    ConnectionProfileId: string option
}

type GitIdentityStrategy = {
    /// Resolves the identity to attribute to revisions created in the workspace, or None to use repository configuration.
    ResolveIdentity: RevisionIdentityRequest -> Async<RevisionIdentity option>
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

let anonymousIdentity: GitIdentityStrategy = {
    ResolveIdentity = fun _request -> async { return None }
}

let private scpRemoteHostPattern = Regex(@"^[^\s@/:]+@([^\s/:]+):.+$")

/// Host of a remote URL for identity selection. Credential lookup treats scp-style
/// SSH remotes (user@host:path) as anonymous because git's own transport
/// authenticates them, but an application can still match an account to that host,
/// so identity selection reads it. Local paths and other forms give None.
let tryIdentityHost (remoteUrl: string) : string option =
    match GitAuthAdapter.tryExtractHostFromRemoteUrl remoteUrl with
    | Ok host -> Some host
    | Error _ ->
        let scpMatch = scpRemoteHostPattern.Match(remoteUrl.Trim())

        if scpMatch.Success then
            Some(scpMatch.Groups.[1].Value.ToLowerInvariant())
        else
            None

[<Emit("Buffer.from($0, 'utf8').toString('base64')")>]
let private toBase64 (_value: string) : string = jsNative

[<Emit("encodeURIComponent($0)")>]
let private encodeUriComponent (_value: string) : string = jsNative

/// Scoped `-c` arguments injecting a Basic credential for exactly the remote's
/// host. Non-HTTPS remotes (SSH, scp-like, local paths) get no header: git's own
/// transport auth applies.
let internal buildScopedAuthArguments (remoteUrl: string) (credential: GitCredential option) : string[] =
    match credential with
    | None -> [||]
    | Some resolved ->
        match GitAuthAdapter.tryExtractHostFromRemoteUrl remoteUrl with
        | Ok host when remoteUrl.StartsWith "https://" ->
            let basicValue = toBase64 $"{resolved.Username}:{resolved.Secret}"

            [|
                "-c"
                $"http.https://{host}/.extraHeader=Authorization: Basic {basicValue}"
            |]
        | _ -> [||]

/// Builds one command-scoped authentication value that can be reused for core
/// Git and explicit Git LFS processes. The credential-bearing LFS endpoint is
/// supplied only through `-c` arguments and is never persisted in repository
/// configuration or a workspace binding.
let internal buildScopedCommandAuthentication
    (remoteName: string)
    (remoteUrl: string)
    (credential: GitCredential option)
    : GitAuthAdapter.GitCommandAuthentication =
    let lfsArguments =
        match credential with
        | Some resolved when remoteUrl.StartsWith("https://", System.StringComparison.OrdinalIgnoreCase) ->
            let remoteWithoutScheme = remoteUrl.Substring("https://".Length)
            let username = encodeUriComponent resolved.Username
            let secret = encodeUriComponent resolved.Secret
            let authenticatedRemote = $"https://{username}:{secret}@{remoteWithoutScheme}"
            let authenticatedLfsUrl = authenticatedRemote.TrimEnd('/') + "/info/lfs"

            [|
                "-c"
                $"remote.{remoteName}.lfsurl={authenticatedLfsUrl}"
                "-c"
                $"lfs.url={authenticatedLfsUrl}"
            |]
        | _ -> [||]

    {
        ConfigArgs = [|
            yield! buildScopedAuthArguments remoteUrl credential
            yield! lfsArguments
        |]
        Environment = GitAuthAdapter.createNonInteractiveEnv ()
    }

let internal buildScopedHeaderAuthentication
    (remoteUrl: string)
    (credential: GitCredential option)
    : GitAuthAdapter.GitCommandAuthentication =
    {
        ConfigArgs = buildScopedAuthArguments remoteUrl credential
        Environment = GitAuthAdapter.createNonInteractiveEnv ()
    }

/// Resolves the scoped auth arguments for a repository location.
let internal resolveAuthArguments
    (strategy: GitCredentialStrategy)
    (providerLocation: string)
    (connectionProfileId: string option)
    : Async<string[]> =
    async {
        match GitAuthAdapter.tryExtractHostFromRemoteUrl providerLocation with
        | Ok host ->
            let! credential = strategy.ResolveCredential host connectionProfileId
            return buildScopedAuthArguments providerLocation credential
        | Error _ ->
            // Local paths and non-URL remotes are anonymous by construction.
            return [||]
    }

/// Resolves the injected strategy once and returns command-scoped material for
/// the complete Git/LFS operation. Local, file, and SSH transports remain
/// anonymous and use Git's native credential mechanisms.
let internal resolveCommandAuthentication
    (strategy: GitCredentialStrategy)
    (providerLocation: string)
    (connectionProfileId: string option)
    (remoteName: string)
    : Async<GitAuthAdapter.GitCommandAuthentication> =
    async {
        match GitAuthAdapter.tryExtractHostFromRemoteUrl providerLocation with
        | Ok host ->
            let! credential = strategy.ResolveCredential host connectionProfileId
            return buildScopedCommandAuthentication remoteName providerLocation credential
        | Error _ ->
            return buildScopedCommandAuthentication remoteName providerLocation None
    }
