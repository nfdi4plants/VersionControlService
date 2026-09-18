/// Injected per-factory/per-connection-profile credential resolution for the
/// Git provider. Anonymous HTTPS, SSH-agent, and local/file remotes need no
/// strategy at all; token flows scope their credential to one host and never
/// place secrets in bindings, requests, or output.
module VersionControlService.Git.GitCredentialStrategy

open System
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
/// otherwise the bound location. It is None when no host can be read from that
/// value, which includes local paths. ConnectionProfileId is
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

/// Host of a URL-form remote. Userinfo is everything up to the last "@" before the
/// path, so a user name containing "@" does not leak into the host. Bracketed IPv6
/// hosts are captured whole. This does not use System.Uri because the Fable runtime
/// cuts a host at its first colon.
let private schemeHostPattern =
    Regex(
        @"^(?:https?|ssh|git|git\+ssh|ssh\+git)://(?:[^/]*@)?(\[[^\]]+\]|[^/:\[]+)(?::\d*)?(?:/|$)",
        RegexOptions.IgnoreCase
    )

/// scp-style `[user@]host:path`. git reads a colon before the first slash as this form
/// unless the text before the colon is a single drive letter.
let private scpHostPattern = Regex(@"^(?:[^/]*@)?(\[[^\]]+\]|[^/:\[]+):")

let private normalizeHost (host: string) =
    let trimmed = host.Trim()

    let unbracketed =
        if trimmed.StartsWith "[" && trimmed.EndsWith "]" then
            trimmed.Substring(1, trimmed.Length - 2)
        else
            trimmed

    unbracketed.ToLowerInvariant()

/// Host of a remote URL for identity selection. Credential lookup only reads https
/// and ssh URLs and treats everything else as anonymous, because git's own transport
/// authenticates it. An application can still match an account to the host of an
/// http, git, git+ssh or scp-style remote, so identity selection reads those too. The
/// parser lowercases hosts and drops IPv6 brackets. It passes percent-encoded and
/// internationalized names through as written. It gives None for local paths, for
/// file URLs, and for any other form without a readable host.
let tryIdentityHost (remoteUrl: string) : string option =
    let trimmed = remoteUrl.Trim()
    let schemeMatch = schemeHostPattern.Match trimmed

    if schemeMatch.Success then
        Some(normalizeHost schemeMatch.Groups.[1].Value)
    elif trimmed.Contains "://" then
        None
    else
        let scpMatch = scpHostPattern.Match trimmed

        if scpMatch.Success then
            let host = scpMatch.Groups.[1].Value

            if host.Length = 1 && Char.IsLetter host.[0] then
                None
            else
                Some(normalizeHost host)
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
