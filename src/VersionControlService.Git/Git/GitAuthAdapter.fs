module internal VersionControlService.Git.GitAuthAdapter

open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Bindings.SimpleGit

module GitExecution = VersionControlService.Git.GitExecution

type GitFactory = SimpleGitOptions -> ISimpleGit

/// Auth material that can be applied either through simple-git config entries or spawned git commands.
/// ConfigArgs contains `-c key=value` pairs; Environment includes non-interactive prompt suppression.
type GitCommandAuthentication = {
    ConfigArgs: string[]
    Environment: obj
}

let private authorizationRedactPattern =
    Regex(@"(Authorization:\s*)(?:Bearer\s*|Basic\s*)[^\s'""]+", RegexOptions.IgnoreCase)

let private tokenHeaderRedactPattern =
    Regex(@"((?:Private-Token|X-Access-Token)\s*:\s*)[^\s'""]+", RegexOptions.IgnoreCase)

let private credentialUrlPattern =
    Regex("(https?://)([^\\s/@]+(?::[^\\s/@]*)?@)", RegexOptions.IgnoreCase)

let private baseConfigEntries (options: SimpleGitOptions) =
    options.config |> Option.defaultValue [||]

/// Converts command-line `-c key=value` pairs into simple-git config entries.
let toConfigEntries (args: string[]) =
    args
    |> Array.mapi (fun index value ->
        if index > 0 && args.[index - 1] = "-c" then
            Some value
        else
            None
    )
    |> Array.choose id

/// Builds a git process environment that disables terminal prompts.
/// The environment starts from a fresh copy of process.env. PATH resolution covers
/// macOS and Linux desktop launches that miss the shell PATH. process.env itself is
/// never modified. Git output receives LC_ALL=C.
/// All Git entry points should use this so Git never blocks waiting for credentials.
let createNonInteractiveEnv () : obj =
    // Keep existing env variables but drop unsafe git/editor overrides that simple-git rejects by default.
    let safeEnv: obj =
        emitJsExpr
            ()
            """
            (() => {
                const blocked = new Set([
                    'editor',
                    'git_askpass',
                    'git_config_global',
                    'git_config_system',
                    'git_config_count',
                    'git_config',
                    'git_editor',
                    'git_exec_path',
                    'git_external_diff',
                    'git_pager',
                    'git_proxy_command',
                    'git_template_dir',
                    'git_sequence_editor',
                    'git_ssh',
                    'git_ssh_command',
                    'pager',
                    'prefix',
                    'ssh_askpass'
                ]);
                const source = process.env ?? {};
                const safeEnv = {};

                for (const [key, value] of Object.entries(source)) {
                    if (!blocked.has(String(key).toLowerCase())) {
                        safeEnv[key] = value;
                    }
                }

                safeEnv.GIT_TERMINAL_PROMPT = '0';
                return safeEnv;
            })()
            """

    GitCommandResolver.ensureGitToolPath safeEnv
    |> GitExecution.forceEnglishDiagnostics

let applyNonInteractiveEnv (git: ISimpleGit) = git.env (createNonInteractiveEnv ())

let private tryExtractHostFromAbsoluteUri (remoteUrl: string) =
    let mutable uri = Unchecked.defaultof<Uri>

    if
        Uri.TryCreate(remoteUrl, UriKind.Absolute, &uri)
        && not (String.IsNullOrWhiteSpace uri.Host)
    then
        Ok(uri.Host.Trim().ToLowerInvariant())
    else
        Error(exn $"Remote URL '{remoteUrl}' is not a valid absolute URI.")

/// Extracts hosts from HTTPS and SSH URLs. Other remote forms, including scp-style SSH, are anonymous to credential lookup.
let tryExtractHostFromRemoteUrl (remoteUrl: string) : Result<string, exn> =
    let normalized = remoteUrl.Trim()

    if String.IsNullOrWhiteSpace normalized then
        Error(exn "Remote URL is empty.")
    elif normalized.StartsWith("https://", StringComparison.OrdinalIgnoreCase) then
        tryExtractHostFromAbsoluteUri normalized
    elif normalized.StartsWith("ssh://", StringComparison.OrdinalIgnoreCase) then
        tryExtractHostFromAbsoluteUri normalized
    else
        Error(exn "Remote URL must use https:// or ssh://.")

/// Splits the authority of a `scheme://` URL into its host and optional port.
/// Userinfo is skipped and IPv6 literals keep their brackets. Fable's Uri drops
/// the port from Host and reports 80 as the port of every URL without one, so
/// the authority is read from the string.
let internal tryParseRemoteAuthority (remoteUrl: string) : (string * string option) option =
    let schemeSeparatorIndex = remoteUrl.IndexOf("://", StringComparison.Ordinal)

    if schemeSeparatorIndex < 0 then
        None
    else
        let authorityStart = schemeSeparatorIndex + 3

        let authorityEnd =
            match remoteUrl.IndexOfAny([| '/'; '?'; '#' |], authorityStart) with
            | -1 -> remoteUrl.Length
            | index -> index

        let authority = remoteUrl.Substring(authorityStart, authorityEnd - authorityStart)
        let hostAndPort = authority.Substring(authority.LastIndexOf('@') + 1)

        let lastColonIndex = hostAndPort.LastIndexOf(':')
        let portSeparatorIndex = if lastColonIndex > hostAndPort.LastIndexOf(']') then lastColonIndex else -1

        let host, port =
            if portSeparatorIndex < 0 then
                hostAndPort, None
            else
                hostAndPort.Substring(0, portSeparatorIndex), Some(hostAndPort.Substring(portSeparatorIndex + 1))

        if String.IsNullOrWhiteSpace host then
            None
        else
            Some(host.ToLowerInvariant(), port)

let internal isDefaultHttpsPort (port: string) =
    let mutable parsedPort = 0
    Int32.TryParse(port, &parsedPort) && parsedPort = 443

/// Extracts the URL authority used by Git's http.<url>.* matching. Git matches the
/// port too, so a non-default port stays, and https's default port 443 is left out.
let tryExtractAuthorityFromRemoteUrl (remoteUrl: string) : Result<string, exn> =
    let normalized = remoteUrl.Trim()

    if String.IsNullOrWhiteSpace normalized then
        Error(exn "Remote URL is empty.")
    elif
        normalized.StartsWith("https://", StringComparison.OrdinalIgnoreCase)
        || normalized.StartsWith("ssh://", StringComparison.OrdinalIgnoreCase)
    then
        match tryExtractHostFromAbsoluteUri normalized, tryParseRemoteAuthority normalized with
        | Error error, _ -> Error error
        | Ok _, None -> Error(exn "Remote URL is missing a host.")
        | Ok _, Some(host, Some port) when
            not (normalized.StartsWith("https://", StringComparison.OrdinalIgnoreCase) && isDefaultHttpsPort port)
            ->
            Ok $"{host}:{port}"
        | Ok _, Some(host, _) -> Ok host
    else
        Error(exn "Remote URL must use https:// or ssh://.")

/// Applies already-resolved command authentication to a simple-git instance.
/// This lets provider sessions resolve an injected strategy once and reuse the
/// same scoped material for planning, explicit LFS transfer, and ref publish.
let applyCommandAuthentication
    (gitFactory: GitFactory)
    (baseOptions: SimpleGitOptions)
    (commandAuth: GitCommandAuthentication)
    : ISimpleGit =
    let authConfig = toConfigEntries commandAuth.ConfigArgs

    let mergedConfig = [|
        yield! baseConfigEntries baseOptions
        yield! authConfig
    |]

    let scopedOptions: SimpleGitOptions =
        emitJsExpr (baseOptions, mergedConfig) "{ ...$0, config: $1 }"

    gitFactory scopedOptions |> fun git -> git.env commandAuth.Environment

/// Redacts bearer/basic headers and credential URLs before errors or diagnostics leave the service boundary.
let redactToken (text: string) : string =
    if String.IsNullOrWhiteSpace text then
        text
    else
        text
        |> fun t ->
            authorizationRedactPattern.Replace(
                t,
                MatchEvaluator(fun matched -> $"{matched.Groups.[1].Value}[REDACTED]")
            )
        |> fun t ->
            tokenHeaderRedactPattern.Replace(t, MatchEvaluator(fun matched -> $"{matched.Groups.[1].Value}[REDACTED]"))
        |> fun t -> credentialUrlPattern.Replace(t, "$1[REDACTED]@")

/// Redacts each command argument for diagnostic output.
let redactArgs (args: string[]) : string[] = args |> Array.map redactToken
