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

let private createTempDirectoryAsync () : JS.Promise<string> = promise {
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-git-cred-" |]
    let! created = fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>
    // GitHub's Windows runners hand out TEMP as an 8.3 short path (RUNNER~1). Git reports the
    // long form, so resolve the directory once here and every path comparison agrees.
    return! fsPromisesDynamic?realpath (created) |> unbox<JS.Promise<string>>
}

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
            "lfs materialization credentials use the injected strategy like publish",
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

                    // insteadOf redirects the https remote to the local bare repository, so
                    // git reports the file URL as the effective fetch and push URL. The hook
                    // answers both get-url queries with the https URL so credential scoping
                    // stays under test.
                    let mutable failAuthenticatedFetch = false

                    let hooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request processContext ->
                                    async {
                                        observedCommands.Add request.Arguments

                                        if
                                            request.Arguments |> Array.contains "get-url"
                                            && request.Arguments |> Array.contains "origin"
                                        then
                                            let output: NodeProcess.ProcessOutput = {
                                                ExitCode = 0
                                                StdOut = testHttpsUrl + "
"
                                                StdErr = ""
                                            }

                                            return OperationResult.succeeded output
                                        elif failAuthenticatedFetch && (request.Arguments |> Array.contains "fetch") then
                                            let output: NodeProcess.ProcessOutput = {
                                                ExitCode = 128
                                                StdOut = ""
                                                StdErr = "fatal: Could not resolve host: git.local.test"
                                            }

                                            return OperationResult.succeeded output
                                        else
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

                    // The first entry is the refresh above. Publish adds one resolution for its
                    // ls-remote pre-check (fetch host) and one for the push (push host), and
                    // reuses that scoped material for core Git and LFS.
                    Vitest.expect(strategyCalls.ToArray()).toEqual [|
                        testHost, Some "token-profile"
                        testHost, Some "token-profile"
                        testHost, Some "token-profile"
                    |]

                    let! _ = runGitIn workPath [| "lfs"; "track"; "materialized.bin" |]
                    do! writeUtf8FileAsync (join [| workPath; "materialized.bin" |]) "materialized LFS content\n"
                    let! _ = runGitIn workPath [| "add"; "-A" |]
                    let! _ = runGitIn workPath [| "commit"; "-m"; "test: add materialized lfs file" |]
                    let! _ = runGitIn workPath [| "push"; "origin"; "main" |]

                    let materialization =
                        session.ObjectMaterialization
                        |> Option.defaultWith (fun () -> failwith "Expected Git object materialization.")

                    strategyCalls.Clear()
                    observedCommands.Clear()

                    let! dematerializeResult =
                        materialization.Dematerialize
                            (RepositoryPath.tryCreate "materialized.bin" |> Result.defaultWith failwith)
                            (ctx "credential-dematerialize")
                        |> Async.StartAsPromise

                    expectValue "credential dematerialization" dematerializeResult |> ignore

                    let! materializeResult =
                        materialization.Materialize
                            (RepositoryPath.tryCreate "materialized.bin" |> Result.defaultWith failwith)
                            (ctx "credential-materialize")
                        |> Async.StartAsPromise

                    expectValue "credential materialization" materializeResult |> ignore

                    // LFS transfers resolve the remote through git itself, which expands the
                    // insteadOf redirect to the local bare repository. A file transport needs no
                    // credential, so the strategy is not consulted. The test below covers the
                    // https case without a redirect.
                    Vitest.expect(strategyCalls.ToArray()).toEqual [||]

                    for arguments in observedCommands do
                        for argument in arguments do
                            Vitest.expect(argument.Contains testSecret).toBe false

                    strategyCalls.Clear()

                    let maintenance =
                        session.Maintenance
                        |> Option.defaultWith (fun () -> failwith "Expected Git maintenance.")

                    let! pruneResult = maintenance.Prune(ctx "credential-prune") |> Async.StartAsPromise
                    expectValue "credential prune" pruneResult |> ignore
                    Vitest.expect(strategyCalls.ToArray()).toEqual [||]

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

                    observedCommands.Clear()
                    failAuthenticatedFetch <- true
                    let! failingRefresh = Async.StartAsPromise(syncService.Refresh(ctx "failing-refresh"))

                    match failingRefresh with
                    | Failed failure ->
                        Vitest.expect(failure.Message.Contains "Could not resolve host").toBe (true)
                        Vitest.expect(failure.Message.Contains "Authorization").toBe (false)
                        Vitest.expect(failure.Message.Contains testSecret).toBe (false)

                        let failingFetch =
                            observedCommands
                            |> Seq.tryFind (fun arguments -> arguments |> Array.contains "fetch")
                            |> Option.defaultWith (fun () -> failwith "Expected an authenticated fetch invocation.")

                        Vitest.expect(
                            failingFetch
                            |> Array.exists (fun argument -> argument.StartsWith expectedHeaderPrefix)
                        ).toBe (true)

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

        // Without a redirect the LFS session reads the https remote as written and must
        // ask the strategy for that host before it touches the network. The prune itself
        // needs no remote round trip, so the result is not the point of the test.
        Vitest.test (
            "lfs maintenance scopes credentials to the remote's effective fetch url",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let workPath = join [| root; "scoped-work" |]
                    let! _ = runGitIn root [| "init"; "-b"; "main"; workPath |]
                    let! _ = runGitIn workPath [| "config"; "user.name"; "VCS Cred Tests" |]
                    let! _ = runGitIn workPath [| "config"; "user.email"; "cred@example.org" |]
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "base\n"
                    let! _ = runGitIn workPath [| "add"; "-A" |]
                    let! _ = runGitIn workPath [| "commit"; "-m"; "init: base" |]
                    let! _ = runGitIn workPath [| "remote"; "add"; "origin"; $"https://{testHost}/scoped.git" |]

                    let strategyCalls = ResizeArray<string * string option>()

                    let strategy: GitCredentialStrategy.GitCredentialStrategy = {
                        ResolveCredential =
                            fun host profileId ->
                                async {
                                    strategyCalls.Add(host, profileId)
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
                            ProviderLocation = workPath
                            ConnectionProfileId = Some "token-profile"
                        }
                        ConnectionProfileId = Some "token-profile"
                    }

                    let session =
                        GitWorkspaceSession.createSessionWithCredentials
                            GitWorkspaceSession.GitSessionHooks.none
                            strategy
                            binding

                    let maintenance =
                        session.Maintenance
                        |> Option.defaultWith (fun () -> failwith "Expected Git maintenance.")

                    let! _ = maintenance.Prune(ctx "scoped-prune") |> Async.StartAsPromise
                    Vitest.expect(strategyCalls.ToArray()).toEqual [| testHost, Some "token-profile" |]

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

Vitest.describe (
    "identity host parsing",
    fun () ->
        let cases: (string * string option) list = [
            "https://Hub.Example.org/group/project.git", Some "hub.example.org"
            "http://Hub.Example.org/repo", Some "hub.example.org"
            "git://Hub.Example.org/repo", Some "hub.example.org"
            "ssh://git@Hub.Example.org:2222/repo", Some "hub.example.org"
            "ssh://first@last@Hub.Example.org:2222/repo", Some "hub.example.org"
            "git+ssh://git@hub.example.org/repo", Some "hub.example.org"
            "ssh+git://git@hub.example.org/repo", Some "hub.example.org"
            "https://[2001:db8::1]:8443/repo", Some "2001:db8::1"
            "ssh://git@[fe80::1%25eth0]/repo", Some "fe80::1%25eth0"
            "git@Hub.Example.org:group/project.git", Some "hub.example.org"
            "Hub.Example.org:repo", Some "hub.example.org"
            "first@last@Hub.Example.org:repo", Some "hub.example.org"
            "git@[2001:db8::1]:repo", Some "2001:db8::1"
            "user@host:", Some "host"
            "host:", Some "host"
            "file:///C:/repos/project.git", None
            "file://server/share/project.git", None
            @"C:\repos\project.git", None
            "C:/repos/project.git", None
            @"\\server\share\project.git", None
            "//server/share/project.git", None
            "../relative/path:with-colon", None
            "./local", None
            "", None
        ]

        for input, expected in cases do
            Vitest.test (
                $"reads the identity host of '{input}'",
                TestOptions(timeout = 30000),
                fun () -> promise { Vitest.expect(GitCredentialStrategy.tryIdentityHost input).toEqual expected }
            )
)
