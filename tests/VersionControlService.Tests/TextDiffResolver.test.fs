module VersionControlService.Tests.TextDiffResolverTests

open System
open Fable.Core
open Vitest
open VersionControlService.Abstractions
open VersionControlService.Git
open VersionControlService.Git.TextDiff
open VersionControlService.Git.TextDiff.TextDiffSourceResolver
open VersionControlService.Git.TextDiff.TextDiffPreparation
open VersionControlService.Git.TextDiff.TextDiffInProcessTransport

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module TextDiffSupervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor
module TextDiffPool = VersionControlService.Git.TextDiff.TextDiffPool

[<Import("mkdtemp", "node:fs/promises")>]
let private mkdtemp (prefix: string) : JS.Promise<string> = jsNative

[<Import("tmpdir", "node:os")>]
let private systemTempDirectory () : string = jsNative

[<Import("symlink", "node:fs/promises")>]
let private symlink (target: string) (path: string) : JS.Promise<unit> = jsNative

[<Emit("Object.assign({}, process.env)")>]
let private processEnvironment () : obj = jsNative

[<Emit("Object.assign({}, process.env, { GIT_NO_LAZY_FETCH: '1' })")>]
let private noLazyFetchEnvironment () : obj = jsNative

[<Emit("performance.now()")>]
let private performanceNow () : float = jsNative

[<Emit("Buffer.from($0).toString('utf8')")>]
let private bytesToUtf8 (_bytes: byte[]) : string = jsNative

[<Emit("console.log($0)")>]
let private writeLog (_message: string) : unit = jsNative

[<Emit("$0?.code ?? String($0)")>]
let private errorCode (_error: obj) : string = jsNative

let private removeDirectory (path: string) = promise {
    try
        do! NodeFileSystem.rmAsync path (NodeFileSystem.RmOptions(recursive = true, force = true, maxRetries = 5, retryDelay = 100))
    with _ -> ()
}

let private runGitWith (environment: obj) (cwd: string) (arguments: string[]) = promise {
    let! result = NodeProcess.runBounded "git" arguments cwd environment (4 * 1024 * 1024) 65536
    return result
}

let private runGit (cwd: string) (arguments: string[]) : JS.Promise<string> = promise {
    let! result = runGitWith (processEnvironment ()) cwd arguments

    match result.ExitCode, result.Error with
    | Some 0, None -> return (bytesToUtf8 result.Stdout).Trim()
    | _ ->
        let command = String.concat " " arguments
        return raise (InvalidOperationException($"Git command failed: {command}. {result.Stderr} {result.Error}"))
}

let private runGitOk cwd arguments = promise {
    let! _ = runGit cwd arguments
    return ()
}

let private writeText (path: string) (content: string) = promise {
    do! NodePositionalFile.mkdirRecursive (NodePath.dirname path)
    NodeFileSystem.writeFileSync path content NodeFileSystem.Utf8
}

let private initializeRepository (repository: string) = promise {
    do! NodePositionalFile.mkdirRecursive repository
    do! runGitOk repository [| "init"; "-q" |]
    do! runGitOk repository [| "config"; "user.name"; "Text Diff Test" |]
    do! runGitOk repository [| "config"; "user.email"; "text-diff@example.invalid" |]
    do! runGitOk repository [| "config"; "core.autocrlf"; "false" |]
}

let private commitFile (repository: string) (path: string) (content: string) = promise {
    do! writeText (NodePath.join [| repository; path |]) content
    do! runGitOk repository [| "add"; "--"; path |]
    do! runGitOk repository [| "commit"; "-q"; "-m"; $"add {path}" |]
}

let private fileUrl (path: string) =
    let normalized = path.Replace('\\', '/')
    if normalized.StartsWith "/" then "file://" + normalized else "file:///" + normalized

let private mediaDirectory (repository: string) = NodePath.join [| repository; ".git"; "lfs"; "objects" |]

let private lfsObjectContent = "lfs object content\n"
let private lfsOid = String.replicate 32 "5e"

let private pointerText =
    $"version https://git-lfs.github.com/spec/v1\noid sha256:{lfsOid}\nsize {lfsObjectContent.Length}\n"

let private lfsObjectPath (repository: string) =
    NodePath.join [| mediaDirectory repository; lfsOid.Substring(0, 2); lfsOid.Substring(2, 2); lfsOid |]

type private Fixture = {
    Root: string
    Supervisor: TextDiffSupervisor.TextDiffSupervisor
}

let mutable private fixture: Fixture option = None
let mutable private nextRepository = 0

let private currentFixture () =
    match fixture with
    | Some value -> value
    | None -> failwith "The resolver fixture was not created."

let private newRepositoryPath () =
    nextRepository <- nextRepository + 1
    NodePath.join [| currentFixture().Root; $"repo-{nextRepository}" |]

let private newRepository () = promise {
    let repository = newRepositoryPath ()
    do! initializeRepository repository
    return repository
}

let private owner: TextDiffSupervisor.ChildOwner = { WorkerId = "resolver"; SessionId = "1"; RequestId = "1" }

/// Resolves through the real supervisor and records every Git result.
let private resolveWithLog (repository: string) (path: string) (previousPath: string option) = promise {
    let results = ResizeArray<GitShort>()

    let runGit arguments =
        async {
            let! result = currentFixture().Supervisor.RunShort(owner, repository, arguments) |> Async.AwaitPromise
            results.Add result
            return result
        }

    let input = {
        RepositoryRoot = repository
        LfsMediaDirectory = mediaDirectory repository
        Path = path
        PreviousPath = previousPath
    }

    let! outcome = resolve (TextDiffWorker.localFileHost runGit) input |> Async.StartAsPromise
    return outcome, results
}

let private resolveIn repository path previousPath =
    resolveWithLog repository path previousPath |> Promise.map fst

let private expectResolved (outcome: ResolveOutcome) =
    match outcome with
    | ResolveOutcome.Resolved sources -> sources
    | other -> failwith $"Expected resolved sources, got %A{other}"

let private expectBlocked (expected: DiffBlocker) (outcome: ResolveOutcome) =
    match outcome with
    | ResolveOutcome.Blocked blocker -> Vitest.expect(blocker).toEqual expected
    | other -> failwith $"Expected the blocker %A{expected}, got %A{other}"

let private expectBlob (expectedOid: string) (expectedSize: int64) (side: ResolvedSide) =
    match side with
    | ResolvedSide.GitBlob(oid, size) ->
        Vitest.expect(oid).toBe expectedOid
        Vitest.expect(size).toEqual expectedSize
    | other -> failwith $"Expected a Git blob, got %A{other}"

let private expectWorkingFile (expectedPath: string) (expectedSize: int64) (side: ResolvedSide) =
    match side with
    | ResolvedSide.WorkingFile(path, identity) ->
        Vitest.expect(path).toBe expectedPath
        Vitest.expect(identity.Size).toEqual expectedSize
    | other -> failwith $"Expected a working file, got %A{other}"

/// Makes a partial clone whose blobs are all missing, and returns the clone and the oid of a.txt.
let private partialClone () = promise {
    let source = newRepositoryPath ()
    do! initializeRepository source
    do! runGitOk source [| "config"; "uploadpack.allowFilter"; "true" |]
    do! commitFile source "a.txt" "promised content\n"
    let clone = newRepositoryPath ()

    do!
        runGitOk
            (currentFixture().Root)
            [| "-c"; "protocol.file.allow=always"; "clone"; "-q"; "--filter=blob:none"; "--no-checkout"; fileUrl source; clone |]

    let! oid = runGit clone [| "rev-parse"; "HEAD:a.txt" |]
    return clone, oid
}

// Git before 2.45 ignores GIT_NO_LAZY_FETCH, so protocol.allow=never keeps the check from fetching there.
let private objectIsMissing (repository: string) (oid: string) = promise {
    let! result =
        runGitWith (noLazyFetchEnvironment ()) repository [| "-c"; "protocol.allow=never"; "cat-file"; "-e"; oid |]
    return result.ExitCode <> Some 0
}

let private path value =
    match RepositoryPath.tryCreate value with
    | Ok created -> created
    | Error message -> failwith message

let private openRequest (value: string) : OpenDiffRequest = {
    Path = path value
    PreviousPath = None
    Preparation = None
    PreviousEncoding = None
    CurrentEncoding = None
    ContextLines = 3
    Continuation = None
}

let private gitProviderId =
    match ProviderId.tryCreate "git" with
    | Ok providerId -> providerId
    | Error message -> failwith message

let private bindingFor (repository: string) : WorkspaceBinding =
    let location: RepositoryLocation = {
        ProviderId = gitProviderId
        DisplayName = None
        ProviderLocation = repository
        ConnectionProfileId = None
    }

    {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = gitProviderId
        WorkspaceRoot = repository
        ProviderStateRef = None
        Location = location
        ConnectionProfileId = None
    }

let private sessionWith (textDiff: GitTextDiffService.GitTextDiffOptions option) (repository: string) =
    GitWorkspaceSession.createSessionWithOptions
        { Hooks = GitWorkspaceSession.GitSessionHooks.none; TextDiff = textDiff }
        GitCredentialStrategy.anonymous
        GitCredentialStrategy.anonymousIdentity
        RevisionPolicyStrategy.automatic
        (bindingFor repository)

let private textDiffOf (session: WorkspaceSession) =
    match session.TextDiff with
    | Some service -> service
    | None -> failwith "The session has no text diff service."

let private openDiff (session: WorkspaceSession) (value: string) = promise {
    return!
        (textDiffOf session).Open (openRequest value) (OperationContext.detached "open-diff")
        |> Async.StartAsPromise
}

let private openDiffReady (session: WorkspaceSession) (value: string) =
    TextDiffTestSupport.openUntilReady (textDiffOf session) (openRequest value) (OperationContext.detached "open-diff")

let private expectOpenResult (expected: OpenDiffResult) (result: OperationResult<Resumable<OpenDiffResult>>) =
    match result with
    | Succeeded outcome -> Vitest.expect(outcome.Value).toEqual (Resumable.Ready expected)
    | other -> failwith $"Expected %A{expected}, got %A{other}"

Vitest.describe (
    "Text diff source resolver",
    fun () ->
        Vitest.beforeAll (
            (fun () -> promise {
                let! root = mkdtemp (NodePath.join [| systemTempDirectory (); "vcs-text-diff-resolver-" |])

                let! supervisor =
                    TextDiffSupervisor.create {
                        TextDiffSupervisor.TextDiffSupervisorOptions.defaults with
                            TempRoot = root
                    }

                fixture <- Some { Root = root; Supervisor = supervisor }
            }),
            60000
        )

        Vitest.afterAll (
            (fun () -> promise {
                match fixture with
                | Some value ->
                    do! value.Supervisor.Dispose()
                    do! removeDirectory value.Root
                | None -> ()
            }),
            120000
        )

        Vitest.test (
            "resolves a modified file to the HEAD blob and the working file",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                let! head = runGit repository [| "rev-parse"; "HEAD" |]
                let! oid = runGit repository [| "rev-parse"; "HEAD:a.txt" |]
                do! writeText (NodePath.join [| repository; "a.txt" |]) "two lines\n"
                let! outcome = resolveIn repository "a.txt" None
                let sources = expectResolved outcome
                Vitest.expect(sources.CommitId).toEqual (Some head)
                expectBlob oid 4L sources.Previous
                expectWorkingFile (NodePath.join [| repository; "a.txt" |]) 10L sources.Current
            }
        )

        Vitest.test (
            "resolves an added file with an absent previous side",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                do! writeText (NodePath.join [| repository; "b.txt" |]) "new\n"
                let! outcome = resolveIn repository "b.txt" None
                let sources = expectResolved outcome
                Vitest.expect(sources.Previous).toEqual ResolvedSide.Absent
                expectWorkingFile (NodePath.join [| repository; "b.txt" |]) 4L sources.Current
            }
        )

        Vitest.test (
            "resolves a deleted file with an absent current side",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                let! oid = runGit repository [| "rev-parse"; "HEAD:a.txt" |]
                NodeFileSystem.unlinkSync (NodePath.join [| repository; "a.txt" |])
                let! outcome = resolveIn repository "a.txt" None
                let sources = expectResolved outcome
                expectBlob oid 4L sources.Previous
                Vitest.expect(sources.Current).toEqual ResolvedSide.Absent
            }
        )

        Vitest.test (
            "treats an unborn HEAD as an absent previous side",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! writeText (NodePath.join [| repository; "a.txt" |]) "first\n"
                let! outcome = resolveIn repository "a.txt" None
                let sources = expectResolved outcome
                Vitest.expect(sources.CommitId).toEqual None
                Vitest.expect(sources.Previous).toEqual ResolvedSide.Absent
                expectWorkingFile (NodePath.join [| repository; "a.txt" |]) 6L sources.Current
            }
        )

        Vitest.test (
            "reports a broken HEAD reference as a read error",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                let! reference = runGit repository [| "symbolic-ref"; "HEAD" |]
                let referencePath = NodePath.join [| yield repository; yield ".git"; yield! reference.Split('/') |]
                do! writeText referencePath "garbage\n"
                let! outcome = resolveIn repository "a.txt" None

                match outcome with
                | ResolveOutcome.ReadError _ -> ()
                | other -> failwith $"Expected a read error, got %A{other}"
            }
        )

        Vitest.test (
            "names the spawn error when Git cannot be started",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                let missingGit = NodePath.join [| currentFixture().Root; "missing-git-executable" |]

                let runMissing arguments =
                    async {
                        let! result =
                            NodeProcess.runBounded missingGit arguments repository (processEnvironment ()) (4 * 1024 * 1024) 65536
                            |> Async.AwaitPromise

                        let short: GitShort = {
                            ExitCode = result.ExitCode
                            Stdout = result.Stdout
                            Stderr = result.Stderr
                            Error = result.Error
                        }

                        return short
                    }

                let input = {
                    RepositoryRoot = repository
                    LfsMediaDirectory = mediaDirectory repository
                    Path = "a.txt"
                    PreviousPath = None
                }

                let! outcome = resolve (TextDiffWorker.localFileHost runMissing) input |> Async.StartAsPromise

                match outcome with
                | ResolveOutcome.ReadError message ->
                    Vitest.expect(message.StartsWith "git rev-parse failed:").toBe true
                    Vitest.expect(message.Contains "does not resolve to a commit").toBe false
                | other -> failwith $"Expected a read error, got %A{other}"
            }
        )

        Vitest.test (
            "reads the previous side at the previous path of a rename",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "old.txt" "moved\n"
                let! oid = runGit repository [| "rev-parse"; "HEAD:old.txt" |]
                NodeFileSystem.renameSync (NodePath.join [| repository; "old.txt" |]) (NodePath.join [| repository; "new.txt" |])
                let! outcome = resolveIn repository "new.txt" (Some "old.txt")
                let sources = expectResolved outcome
                expectBlob oid 6L sources.Previous
                expectWorkingFile (NodePath.join [| repository; "new.txt" |]) 6L sources.Current
            }
        )

        Vitest.test (
            "blocks a previous side that is a symlink, a gitlink or a directory",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "folder/inner.txt" "inner\n"
                do! writeText (NodePath.join [| repository; "target.txt" |]) "target.txt"
                let! linkOid = runGit repository [| "hash-object"; "-w"; "--"; "target.txt" |]
                let! head = runGit repository [| "rev-parse"; "HEAD" |]
                do! runGitOk repository [| "update-index"; "--add"; "--cacheinfo"; $"120000,{linkOid},link" |]
                do! runGitOk repository [| "update-index"; "--add"; "--cacheinfo"; $"160000,{head},module" |]
                do! runGitOk repository [| "commit"; "-q"; "-m"; "special entries" |]

                for special in [ "link"; "module"; "folder" ] do
                    let! outcome = resolveIn repository special None
                    expectBlocked (DiffBlocker.NotRegularFile DiffSide.Previous) outcome
            }
        )

        Vitest.test (
            "uses the local LFS object of a pointer in HEAD and blocks when it is missing",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "data.bin" pointerText
                NodeFileSystem.unlinkSync (NodePath.join [| repository; "data.bin" |])
                let! missing = resolveIn repository "data.bin" None
                expectBlocked (DiffBlocker.LocalContentUnavailable(DiffSide.Previous, Some lfsOid)) missing

                do! writeText (lfsObjectPath repository) lfsObjectContent
                let! present = resolveIn repository "data.bin" None
                let sources = expectResolved present

                match sources.Previous with
                | ResolvedSide.LfsObject(objectPath, size, identity, pointerIdentity) ->
                    Vitest.expect(objectPath).toBe (lfsObjectPath repository)
                    Vitest.expect(size).toEqual (int64 lfsObjectContent.Length)
                    Vitest.expect(identity.Size).toEqual (int64 lfsObjectContent.Length)
                    Vitest.expect(pointerIdentity).toEqual None
                | other -> failwith $"Expected an LFS object, got %A{other}"

                Vitest.expect(sources.Current).toEqual ResolvedSide.Absent
            }
        )

        Vitest.test (
            "uses the local LFS object of a pointer working file and blocks when it is missing",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                do! writeText (NodePath.join [| repository; "data.bin" |]) pointerText
                let! missing = resolveIn repository "data.bin" None
                expectBlocked (DiffBlocker.LocalContentUnavailable(DiffSide.Current, Some lfsOid)) missing

                // An object of another size is not the object the pointer names.
                do! writeText (lfsObjectPath repository) (lfsObjectContent + "x")
                let! wrongSize = resolveIn repository "data.bin" None
                expectBlocked (DiffBlocker.LocalContentUnavailable(DiffSide.Current, Some lfsOid)) wrongSize

                do! writeText (lfsObjectPath repository) lfsObjectContent
                let! present = resolveIn repository "data.bin" None
                let sources = expectResolved present
                Vitest.expect(sources.Previous).toEqual ResolvedSide.Absent

                match sources.Current with
                | ResolvedSide.LfsObject(objectPath, size, _, Some pointerIdentity) ->
                    Vitest.expect(objectPath).toBe (lfsObjectPath repository)
                    Vitest.expect(size).toEqual (int64 lfsObjectContent.Length)
                    Vitest.expect(pointerIdentity.Size).toEqual (int64 pointerText.Length)
                | other -> failwith $"Expected a pointer-backed LFS object, got %A{other}"
            }
        )

        Vitest.test (
            "blocks a current side that is a symbolic link",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                let linkPath = NodePath.join [| repository; "link.txt" |]
                let mutable created = true

                try
                    do! symlink "a.txt" linkPath
                with error ->
                    created <- false
                    writeLog $"Skipping the symlink check because this platform refused to create one: {errorCode error}"

                if created then
                    let! outcome = resolveIn repository "link.txt" None
                    expectBlocked (DiffBlocker.NotRegularFile DiffSide.Current) outcome
            }
        )

        Vitest.test (
            "refuses a working path that runs through a symlinked folder leaving the workspace",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                let outside = newRepositoryPath ()
                do! writeText (NodePath.join [| outside; "secret.txt" |]) "secret\n"
                let mutable created = true

                try
                    do! symlink outside (NodePath.join [| repository; "escape" |])
                with error ->
                    created <- false
                    writeLog $"Skipping the symlinked folder check because this platform refused to create one: {errorCode error}"

                if created then
                    let! outcome = resolveIn repository "escape/secret.txt" None

                    match outcome with
                    | ResolveOutcome.ReadError message -> Vitest.expect(message.Contains "outside the workspace").toBe true
                    | other -> failwith $"Expected a read error, got %A{other}"
            }
        )

        Vitest.test (
            "refuses a working path below the .git folder",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                let! outcome = resolveIn repository ".git/config" None

                match outcome with
                | ResolveOutcome.ReadError message -> Vitest.expect(message.Contains ".git folder").toBe true
                | other -> failwith $"Expected a read error, got %A{other}"
            }
        )

        Vitest.test (
            "matches a missing object message whatever its letter case",
            TestOptions(timeout = 60000),
            fun () -> promise {
                Vitest.expect(stderrReportsMissingObject "fatal: Not a valid object name abc123").toBe true
                Vitest.expect(stderrReportsMissingObject "fatal: bad revision").toBe false
            }
        )

        Vitest.test (
            "reports a missing promisor blob as unavailable without fetching it",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! clone, oid = partialClone ()
                let! missingBefore = objectIsMissing clone oid
                Vitest.expect(missingBefore).toBe true
                let! outcome = resolveIn clone "a.txt" None
                expectBlocked (DiffBlocker.LocalContentUnavailable(DiffSide.Previous, Some oid)) outcome
                let! missingAfter = objectIsMissing clone oid
                Vitest.expect(missingAfter).toBe true
            }
        )

        Vitest.test (
            "denies the transport to an allowed https promisor remote",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! clone, oid = partialClone ()
                let remote = "https://127.0.0.1:9/x"
                do! runGitOk clone [| "config"; "remote.origin.url"; remote |]
                do! runGitOk clone [| "config"; "protocol.https.allow"; "always" |]
                let started = performanceNow ()
                let! outcome, results = resolveWithLog clone "a.txt" None
                let elapsed = performanceNow () - started
                writeLog $"Missing blob resolution with a promisor remote: %.1f{elapsed} ms"
                expectBlocked (DiffBlocker.LocalContentUnavailable(DiffSide.Previous, Some oid)) outcome
                Vitest.expect(elapsed < 5000.0).toBe true

                // Git 2.42 to 2.44 start the promisor fetch, report the denied transport and print "could not
                // fetch", so the check looks for a connection attempt.
                for result in results do
                    Vitest.expect(result.Stderr.Contains("127.0.0.1:9", StringComparison.OrdinalIgnoreCase)).toBe false
                    Vitest.expect(result.Stderr.Contains("unable to access", StringComparison.OrdinalIgnoreCase)).toBe false
            }
        )

        Vitest.test (
            "answers Open from a Git session through the worker pool",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                do! commitFile repository "data.bin" pointerText
                do! writeText (NodePath.join [| repository; "a.txt" |]) "two\n"
                let! supervisorRoot = mkdtemp (NodePath.join [| currentFixture().Root; "pool-" |])

                let! supervisor =
                    TextDiffSupervisor.create {
                        TextDiffSupervisor.TextDiffSupervisorOptions.defaults with
                            TempRoot = supervisorRoot
                    }

                let pool =
                    TextDiffPool.create (
                        TextDiffPool.TextDiffPoolOptions.create
                            (fun _ -> InProcessTransport.create (TextDiffWorker.createDefaultHandler ()))
                            supervisor
                    )

                let session =
                    sessionWith (Some { Pool = (fun () -> Promise.lift (Some pool)); WindowOwnerOf = fun _ -> "window-1" }) repository
                let mutable failure = None

                try
                    let! modified = openDiffReady session "a.txt"

                    match modified with
                    | OpenDiffResult.Opened _ -> ()
                    | other -> failwith $"Expected an opened diff, got %A{other}"

                    let! lfs = openDiff session "data.bin"

                    expectOpenResult
                        (OpenDiffResult.NotDiffable(DiffBlocker.LocalContentUnavailable(DiffSide.Previous, Some lfsOid)))
                        lfs
                with error ->
                    failure <- Some error

                do! pool.Dispose()

                match failure with
                | Some error -> return raise error
                | None -> ()
            }
        )

        Vitest.test (
            "fails Open with diff_worker_failed in a Git session without a pool",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitFile repository "a.txt" "one\n"
                let session = sessionWith None repository
                let! result = openDiff session "a.txt"

                match result with
                | Failed failure -> Vitest.expect(failure.Code).toBe TextDiffFailureCodes.WorkerFailed
                | other -> failwith $"Expected a worker failure, got %A{other}"
            }
        )
)

Vitest.describe (
    "Text diff preparation tokens",
    fun () ->
        let identity: FileIdentity = { Size = 4L; MtimeNs = "1"; Ino = "2"; Dev = "3" }

        let binding: PreparationBinding = {
            Path = "a.txt"
            PreviousPath = None
            CommitId = Some(String.replicate 40 "a")
            Previous = SideIdentity.ObjectId(String.replicate 40 "b")
            Current = SideIdentity.File identity
        }

        let expectMismatch (result: Result<unit, OperationFailure>) =
            match result with
            | Error failure -> Vitest.expect(failure.Code).toBe TextDiffFailureCodes.PreparationMismatch
            | Ok() -> failwith "Expected preparation_mismatch."

        Vitest.test (
            "accepts the binding and window the token was issued for",
            fun () ->
                let store = PreparationTokenStore(fun () -> 0.0)
                let token = store.Issue(binding, "window-1")
                Vitest.expect(store.Validate(token, binding, "window-1")).toEqual (Ok())
        )

        Vitest.test (
            "fails when the bound sources changed",
            fun () ->
                let store = PreparationTokenStore(fun () -> 0.0)
                let token = store.Issue(binding, "window-1")
                let edited = { binding with Current = SideIdentity.File { identity with MtimeNs = "9" } }
                expectMismatch (store.Validate(token, edited, "window-1"))
                expectMismatch (store.Validate(token, { binding with PreviousPath = Some "old.txt" }, "window-1"))
        )

        Vitest.test (
            "fails for another window owner",
            fun () ->
                let store = PreparationTokenStore(fun () -> 0.0)
                let token = store.Issue(binding, "window-1")
                expectMismatch (store.Validate(token, binding, "window-2"))
        )

        Vitest.test (
            "expires after five minutes",
            fun () ->
                let mutable now = 0.0
                let store = PreparationTokenStore(fun () -> now)
                let token = store.Issue(binding, "window-1")
                now <- 299999.0
                Vitest.expect(store.Validate(token, binding, "window-1")).toEqual (Ok())
                now <- 300000.0
                expectMismatch (store.Validate(token, binding, "window-1"))
        )

        Vitest.test (
            "releases the tokens of a path when the same window opens it without a token",
            fun () ->
                let store = PreparationTokenStore(fun () -> 0.0)
                let token = store.Issue(binding, "window-1")
                store.ReleaseForPath("window-2", "a.txt")
                Vitest.expect(store.Validate(token, binding, "window-1")).toEqual (Ok())
                store.ReleaseForPath("window-1", "a.txt")
                expectMismatch (store.Validate(token, binding, "window-1"))
        )
)
