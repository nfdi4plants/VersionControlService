module VersionControlService.Tests.GitCredentialsTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Tests.NodePath
open Vitest

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module NodeProcess = VersionControlService.Runtime.Node.Process

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-git-cred-" |]
    fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>

let private removeDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private writeUtf8FileAsync (path: string) (content: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?writeFile (path, content, "utf8") |> unbox<JS.Promise<obj>>
    return ()
}

let private pathExistsAsync (path: string) : JS.Promise<bool> = promise {
    try
        let! _ = fsPromisesDynamic?access (path) |> unbox<JS.Promise<obj>>
        return true
    with _ ->
        return false
}

let private runGitIn (cwd: string) (arguments: string[]) : JS.Promise<string> = promise {
    let request = {
        NodeProcess.ProcessRequest.create "git" arguments with
            WorkingDirectory = Some cwd
    }

    let! result = Async.StartAsPromise(NodeProcess.run request (OperationContext.detached "git-cred-fixture"))

    match result with
    | Succeeded outcome when outcome.Value.ExitCode = 0 -> return outcome.Value.StdOut
    | Succeeded outcome ->
        let command = String.concat " " arguments
        return failwith $"fixture git {command} failed: {outcome.Value.StdErr}"
    | PartiallySucceeded _
    | Failed _ -> return failwith "fixture git invocation failed"
}

let private gitProviderId =
    match ProviderId.tryCreate "git" with
    | Ok providerId -> providerId
    | Error message -> failwith message

let private ctx (name: string) = OperationContext.detached name

let private expectValue (operationName: string) (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure) ->
        failwith $"{operationName} unexpectedly returned partial success ({failure.Code})."
    | Failed failure -> failwith $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private toFileRemoteUrl (path: string) =
    let normalized = path.Replace("\\", "/")

    if normalized.StartsWith("/", StringComparison.Ordinal) then
        $"file://{normalized}"
    else
        $"file:///{normalized}"

let private testHost = "git.local.test"
let private testHttpsUrl = $"https://{testHost}/origin.git"
let private testSecret = "secret-token-123"

Vitest.describe (
    "credential redaction",
    fun () ->
        let cases = [|
            "Authorization: Bearer abc123", "Authorization: [REDACTED]"
            "authorization:bearer XYZ", "authorization: [REDACTED]"
            "PRIVATE-TOKEN: abc123", "PRIVATE-TOKEN: [REDACTED]"
            "X-Access-Token: abc123", "X-Access-Token: [REDACTED]"
            "fatal: unable to access 'https://user:secret@example.test/repository.git/'",
            "fatal: unable to access 'https://[REDACTED]@example.test/repository.git/'"
            "clean message", "clean message"
        |]

        for input, expected in cases do
            Vitest.test (
                $"redacts credential material in '{input}'",
                fun () -> Vitest.expect(Redaction.redact input).toBe expected
            )

        Vitest.test (
            "keeps null unchanged",
            fun () ->
                let redacted: string = Redaction.redact null
                Vitest.expect(isNull redacted).toBe true
        )
)

Vitest.describe (
    "Git workspace credential strategies",
    fun () ->
        Vitest.test (
            "credential strategies support anonymous token SSH and local remotes",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let barePath = join [| root; "origin.git" |]
                    let workPath = join [| root; "work" |]

                    let! _ = runGitIn root [| "init"; "--bare"; "-b"; "main"; barePath |]
                    let! _ = runGitIn root [| "init"; "-b"; "main"; workPath |]
                    let! _ = runGitIn workPath [| "config"; "user.name"; "VCS Cred Tests" |]
                    let! _ = runGitIn workPath [| "config"; "user.email"; "cred@example.org" |]
                    let! _ = runGitIn workPath [| "config"; "core.autocrlf"; "false" |]
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "base\n"
                    let! _ = runGitIn workPath [| "add"; "-A" |]
                    let! _ = runGitIn workPath [| "commit"; "-m"; "init: base" |]
                    let! _ = runGitIn workPath [| "remote"; "add"; "origin"; testHttpsUrl |]

                    // Rewrite the HTTPS remote to the local bare so transfers stay offline.
                    let! _ =
                        runGitIn workPath [|
                            "config"
                            "--add"
                            $"url.{toFileRemoteUrl barePath}.insteadOf"
                            testHttpsUrl
                        |]

                    let! _ = runGitIn workPath [| "push"; "-u"; "origin"; "main" |]

                    // Process spy: records every provider invocation to observe injected auth.
                    let observedCommands = ResizeArray<string[]>()

                    let hooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request processContext ->
                                    async {
                                        observedCommands.Add request.Arguments
                                        return! NodeProcess.run request processContext
                                    })
                    }

                    // Per-session strategy: only the token profile resolves a credential.
                    let strategyCalls = ResizeArray<string * string option>()

                    let strategy: GitCredentialStrategy.GitCredentialStrategy = {
                        ResolveCredential =
                            fun host profileId ->
                                async {
                                    strategyCalls.Add(host, profileId)

                                    if host = testHost && profileId = Some "token-profile" then
                                        return
                                            Some {
                                                Username = "oauth2"
                                                Secret = testSecret
                                            }
                                    else
                                        return None
                                }
                    }

                    let binding: WorkspaceBinding = {
                        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
                        ProviderId = gitProviderId
                        WorkspaceRoot = workPath
                        ProviderStateRef = None
                        Location = {
                            ProviderId = gitProviderId
                            DisplayName = None
                            ProviderLocation = testHttpsUrl
                            ConnectionProfileId = Some "token-profile"
                        }
                        ConnectionProfileId = Some "token-profile"
                    }

                    let session = GitWorkspaceSession.createSessionWithCredentials hooks strategy binding

                    let syncService =
                        match session.Synchronization with
                        | Some service -> service
                        | None -> failwith "Expected the synchronization service."

                    // Token HTTPS: refresh succeeds and the credential travels as a
                    // host-scoped extraHeader on the fetch invocation.
                    let! refreshResult = Async.StartAsPromise(syncService.Refresh(ctx "cred-refresh"))
                    expectValue "token refresh" refreshResult |> ignore

                    let expectedHeaderPrefix = $"http.https://{testHost}/.extraHeader=Authorization: Basic "

                    let fetchCommands =
                        observedCommands
                        |> Seq.filter (fun arguments -> arguments |> Array.contains "fetch")
                        |> Seq.toArray

                    Vitest.expect(fetchCommands.Length > 0).toBe (true)

                    let fetchHasScopedAuth =
                        fetchCommands
                        |> Array.forall (fun arguments ->
                            arguments
                            |> Array.exists (fun argument -> argument.StartsWith expectedHeaderPrefix))

                    Vitest.expect(fetchHasScopedAuth).toBe (true)

                    // Refresh resolves exactly one scoped core-Git credential.
                    Vitest.expect(strategyCalls.ToArray()).toEqual [| testHost, Some "token-profile" |]

                    do! writeUtf8FileAsync (join [| workPath; "authenticated-publish.txt" |]) "published\n"
                    let! revisionStatusResult = session.Core.GetStatus(ctx "credential-revision-status") |> Async.StartAsPromise
                    let revisionStatus = expectValue "credential revision status" revisionStatusResult

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: authenticated publish"
                                Paths =
                                    [|
                                        RepositoryPath.tryCreate "authenticated-publish.txt"
                                        |> Result.defaultWith failwith
                                    |]
                                ExpectedWorkspaceVersion = revisionStatus.WorkspaceVersion
                            }
                            (ctx "credential-revision")
                        |> Async.StartAsPromise

                    expectValue "credential revision" revisionResult |> ignore
                    let! publishStatusResult = session.Core.GetStatus(ctx "credential-publish-status") |> Async.StartAsPromise
                    let publishStatus = expectValue "credential publish status" publishStatusResult

                    let! publishResult =
                        syncService.Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "credential-publish")
                        |> Async.StartAsPromise

                    expectValue "credential publish" publishResult |> ignore
                    let pushCommands =
                        observedCommands
                        |> Seq.filter (fun arguments -> arguments |> Array.contains "push")
                        |> Seq.toArray

                    let expectedLfsUrl = $"lfs.url=https://oauth2:{testSecret}@{testHost}/origin.git/info/lfs"
                    Vitest.expect(pushCommands.Length > 0).toBe true

                    Vitest.expect(
                        pushCommands
                        |> Array.forall (fun arguments ->
                            arguments |> Array.exists (fun argument -> argument.StartsWith expectedHeaderPrefix)
                            && arguments |> Array.contains expectedLfsUrl)
                    ).toBe true

                    // Publish resolves once and reuses that scoped material for core Git and LFS.
                    Vitest.expect(strategyCalls.ToArray()).toEqual [|
                        testHost, Some "token-profile"
                        testHost, Some "token-profile"
                    |]

                    // Anonymous local-path remote: a session with the anonymous strategy
                    // and no global token machinery synchronizes fine.
                    let anonymousBinding = {
                        binding with
                            Location = {
                                binding.Location with
                                    ProviderLocation = barePath
                                    ConnectionProfileId = None
                            }
                            ConnectionProfileId = None
                    }

                    let anonymousSession =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none anonymousBinding

                    match anonymousSession.Synchronization with
                    | Some anonymousSync ->
                        let! anonymousRefresh = Async.StartAsPromise(anonymousSync.Refresh(ctx "anon-refresh"))
                        expectValue "anonymous refresh" anonymousRefresh |> ignore
                    | None -> failwith "Expected the synchronization service."

                    // scp-like SSH and local/file remote forms are accepted by Bind.
                    let factory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none

                    let! scpBind =
                        Async.StartAsPromise(
                            factory.Bind
                                {
                                    WorkspaceRoot = workPath
                                    Location = {
                                        ProviderId = gitProviderId
                                        DisplayName = None
                                        ProviderLocation = "git@github.com:example/repo.git"
                                        ConnectionProfileId = None
                                    }
                                }
                                (ctx "scp-bind")
                        )

                    let scpBinding = expectValue "scp-like bind" scpBind
                    Vitest.expect(scpBinding.Location.ProviderLocation).toBe ("git@github.com:example/repo.git")

                    // Re-bind to the local path remote (also a valid form) and restore.
                    let! localBind =
                        Async.StartAsPromise(
                            factory.Bind
                                {
                                    WorkspaceRoot = workPath
                                    Location = anonymousBinding.Location
                                }
                                (ctx "local-bind")
                        )

                    expectValue "local-path bind" localBind |> ignore

                    // Redaction: a failing authenticated transfer never leaks the secret.
                    let! _ = runGitIn workPath [| "remote"; "set-url"; "origin"; testHttpsUrl |]

                    let! _ =
                        runGitIn workPath [|
                            "config"
                            "--unset-all"
                            $"url.{toFileRemoteUrl barePath}.insteadOf"
                        |]

                    let missingRewriteUrl = toFileRemoteUrl (join [| root; "missing.git" |])

                    let! _ =
                        runGitIn workPath [|
                            "config"
                            "--add"
                            $"url.{missingRewriteUrl}.insteadOf"
                            testHttpsUrl
                        |]

                    let! failingRefresh = Async.StartAsPromise(syncService.Refresh(ctx "failing-refresh"))

                    match failingRefresh with
                    | Failed failure ->
                        Vitest.expect(failure.Message.Contains testSecret).toBe (false)

                        for detail in failure.Details do
                            Vitest.expect(detail.Contains testSecret).toBe (false)
                    | Succeeded _
                    | PartiallySucceeded _ -> failwith "Expected the refresh against a missing rewrite to fail."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "publish never creates a missing remote repository",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let workPath = join [| root; "work" |]
                    let! _ = runGitIn root [| "init"; "-b"; "main"; workPath |]
                    let! _ = runGitIn workPath [| "config"; "user.name"; "VCS Cred Tests" |]
                    let! _ = runGitIn workPath [| "config"; "user.email"; "cred@example.org" |]
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "base\n"
                    let! _ = runGitIn workPath [| "add"; "-A" |]
                    let! _ = runGitIn workPath [| "commit"; "-m"; "init: base" |]

                    // The configured target does not exist.
                    let missingRemote = join [| root; "missing-remote.git" |]
                    let! _ = runGitIn workPath [| "remote"; "add"; "origin"; missingRemote |]

                    let binding: WorkspaceBinding = {
                        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
                        ProviderId = gitProviderId
                        WorkspaceRoot = workPath
                        ProviderStateRef = None
                        Location = {
                            ProviderId = gitProviderId
                            DisplayName = None
                            ProviderLocation = missingRemote
                            ConnectionProfileId = None
                        }
                        ConnectionProfileId = None
                    }

                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let syncService =
                        match session.Synchronization with
                        | Some service -> service
                        | None -> failwith "Expected the synchronization service."

                    let! statusResult = Async.StartAsPromise(session.Core.GetStatus(ctx "missing-remote-status"))
                    let status = expectValue "status" statusResult

                    let! publishResult =
                        Async.StartAsPromise(
                            syncService.Publish
                                {
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                    ExpectedTargetRevision = None
                                }
                                (ctx "missing-remote-publish")
                        )

                    match publishResult with
                    | Failed failure -> Vitest.expect(failure.Retryable).toBe (true)
                    | Succeeded _
                    | PartiallySucceeded _ -> failwith "Expected publish to a missing remote to fail."

                    let! remoteExists = pathExistsAsync missingRemote
                    Vitest.expect(remoteExists).toBe false

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
