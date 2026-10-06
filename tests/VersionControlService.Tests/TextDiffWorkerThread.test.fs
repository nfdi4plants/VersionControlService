module VersionControlService.Tests.TextDiffWorkerThreadTests

open System
open System.Text
open Fable.Core
open Vitest
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffProtocol
open VersionControlService.Git.TextDiff.TextDiffTransport

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module TextDiffSupervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor
module TextDiffPool = VersionControlService.Git.TextDiff.TextDiffPool
module TextDiffWorker = VersionControlService.Git.TextDiff.TextDiffWorker

[<Import("mkdtemp", "node:fs/promises")>]
let private mkdtemp (prefix: string) : JS.Promise<string> = jsNative

[<Import("tmpdir", "node:os")>]
let private systemTempDirectory () : string = jsNative

[<Import("setTimeout", "node:timers/promises")>]
let private delay (milliseconds: int) : JS.Promise<unit> = jsNative

[<Import("fileURLToPath", "node:url")>]
let private fileUrlToPath (_url: obj) : string = jsNative

// The Fable output folder is plain ESM next to this file, so Node loads the compiled worker entry directly.
[<Emit("new URL('./TextDiffTestWorker.js', import.meta.url)")>]
let private testWorkerUrl () : obj = jsNative

[<Emit("process.env.VCS_TEXT_DIFF_TEST_WORKER_MODE = $0")>]
let private setWorkerMode (_mode: string) : unit = jsNative

[<Emit("delete process.env.VCS_TEXT_DIFF_TEST_WORKER_MODE")>]
let private clearWorkerMode () : unit = jsNative

[<Emit("Object.assign({}, process.env)")>]
let private processEnvironment () : obj = jsNative

[<Emit("performance.now()")>]
let private performanceNow () : float = jsNative

[<Emit("console.log($0)")>]
let private writeLog (_message: string) : unit = jsNative

let private testWorkerPath = fileUrlToPath (testWorkerUrl ())

let private createTempDirectory () : JS.Promise<string> =
    mkdtemp (NodePath.join [| systemTempDirectory (); "vcs-text-diff-thread-" |])

let private removeDirectory (path: string) = promise {
    try
        do! NodeFileSystem.rmAsync path (NodeFileSystem.RmOptions(recursive = true, force = true, maxRetries = 5, retryDelay = 100))
    with _ -> ()
}

let private runGitOk (cwd: string) (arguments: string[]) : JS.Promise<unit> = promise {
    let! result = NodeProcess.runBounded "git" arguments cwd (processEnvironment ()) (4 * 1024 * 1024) 65536

    match result.ExitCode, result.Error with
    | Some 0, None -> ()
    | _ ->
        let command = String.concat " " arguments
        return raise (InvalidOperationException($"Git command failed: {command}. {result.Stderr} {result.Error}"))
}

let private writeLargeTextFile (path: string) (size: int64) : JS.Promise<unit> = promise {
    let line = Encoding.UTF8.GetBytes("text-diff-worker-line-0123456789abcdef\n")
    let chunk = Array.init (64 * 1024) (fun index -> line[index % line.Length])
    let! descriptor = NodePositionalFile.openCreateExclusive path
    let mutable position = 0L

    try
        while position < size do
            let count = int (min (int64 chunk.Length) (size - position))
            let mutable written = 0

            while written < count do
                let! current = NodePositionalFile.writeAt descriptor chunk written (count - written) (position + int64 written)
                written <- written + current

            position <- position + int64 count

        do! NodePositionalFile.close descriptor
    with error ->
        do! NodePositionalFile.close descriptor
        return raise error
}

let private path value =
    match RepositoryPath.tryCreate value with
    | Ok created -> created
    | Error message -> failwith message

let private openRequest: OpenDiffRequest = {
    Path = path "large.txt"
    PreviousPath = None
    Preparation = None
    PreviousEncoding = None
    CurrentEncoding = None
    ContextLines = 3
    Continuation = None
    Storage = DiffStoragePolicy.PreferDisk(0L, 67108864L)
}

let private waitFor (description: string) (timeoutMs: float) (condition: unit -> JS.Promise<bool>) = promise {
    let started = performanceNow ()
    let mutable reached = false

    while not reached && performanceNow () - started < timeoutMs do
        let! current = condition ()
        reached <- current

        if not reached then
            do! delay 10

    if not reached then
        failwith $"Timed out waiting for {description}."
}

let private waitForGone (pid: int) (timeoutMs: float) : JS.Promise<float option> = promise {
    let started = performanceNow ()
    let mutable goneAfter = None

    while goneAfter.IsNone && performanceNow () - started < timeoutMs do
        match NodeProcess.processExistence pid with
        | NodeProcess.Gone -> goneAfter <- Some(performanceNow () - started)
        | NodeProcess.Alive
        | NodeProcess.Unknown _ -> do! delay 10

    return goneAfter
}

let private spawnedPids (events: ResizeArray<TextDiffSupervisor.SupervisorEvent>) =
    events
    |> Seq.choose (fun event ->
        match event.Kind with
        | TextDiffSupervisor.ChildSpawned pid -> Some pid
        | _ -> None)
    |> Seq.toArray

let private largestSpool (directory: string) : JS.Promise<int64> = promise {
    let mutable largest = 0L

    try
        let! workerNames = NodeFileSystem.readdirAsync directory

        for workerName in workerNames do
            let workerDirectory = NodePath.join [| directory; workerName |]
            let! fileNames = NodeFileSystem.readdirAsync workerDirectory

            for fileName in fileNames do
                if fileName.EndsWith ".spool" then
                    let! stats = NodePositionalFile.lstat (NodePath.join [| workerDirectory; fileName |])
                    largest <- max largest stats.Size
    with _ -> ()

    return largest
}

type private Fixture = {
    Root: string
    Repository: string
}

let mutable private fixture: Fixture option = None

let private currentFixture () =
    match fixture with
    | Some value -> value
    | None -> failwith "The repository fixture was not created."

let private withWorkerPool
    (mode: string)
    (events: ResizeArray<TextDiffSupervisor.SupervisorEvent>)
    (body: TextDiffPool.TextDiffPool -> TextDiffSupervisor.TextDiffSupervisor -> ResizeArray<ITextDiffWorkerTransport> -> JS.Promise<unit>)
    =
    promise {
        let! tempRoot = mkdtemp (NodePath.join [| currentFixture().Root; "supervisor-" |])

        let! supervisor =
            TextDiffSupervisor.create {
                TextDiffSupervisor.TextDiffSupervisorOptions.defaults with
                    TempRoot = tempRoot
                    OnEvent = Some(fun event -> events.Add event)
            }

        let transports = ResizeArray<ITextDiffWorkerTransport>()

        let factory: TextDiffWorkerFactory =
            fun _ ->
                setWorkerMode mode

                try
                    let transport = WorkerThreadTransport.create testWorkerPath
                    transports.Add transport
                    transport
                finally
                    clearWorkerMode ()

        let pool =
            TextDiffPool.create {
                (TextDiffPool.TextDiffPoolOptions.create factory supervisor) with
                    MaxWorkers = 1
            }

        let mutable failure = None

        try
            do! body pool supervisor transports
        with error ->
            failure <- Some error

        do! pool.Dispose()

        match failure with
        | Some error -> return raise error
        | None -> ()
    }

let private owner () : TextDiffOwner = {
    WorkspaceRoot = currentFixture().Repository
    LfsMediaDirectory = NodePath.join [| currentFixture().Repository; ".git"; "lfs"; "objects" |]
    WindowOwner = "window-1"
}

Vitest.describe (
    "Text diff worker thread",
    fun () ->
        Vitest.beforeAll (
            (fun () -> promise {
                let! root = createTempDirectory ()
                let repository = NodePath.join [| root; "repo" |]
                fixture <- Some { Root = root; Repository = repository }
                do! NodePositionalFile.mkdirRecursive repository
                do! runGitOk repository [| "init"; "-q" |]
                do! runGitOk repository [| "config"; "user.name"; "Text Diff Test" |]
                do! runGitOk repository [| "config"; "user.email"; "text-diff@example.invalid" |]
                do! writeLargeTextFile (NodePath.join [| repository; "large.txt" |]) (256L * 1024L * 1024L)
                do! runGitOk repository [| "add"; "--"; "large.txt" |]
                do! runGitOk repository [| "commit"; "-q"; "-m"; "large blob" |]
                do! writeLargeTextFile (NodePath.join [| repository; "retained.txt" |]) (2L * 1024L * 1024L)
                do! runGitOk repository [| "add"; "--"; "retained.txt" |]
                do! runGitOk repository [| "commit"; "-q"; "-m"; "retained blob" |]
            }),
            600000
        )

        Vitest.afterAll (
            (fun () -> promise {
                match fixture with
                | Some value -> do! removeDirectory value.Root
                | None -> ()
            }),
            120000
        )

        Vitest.test (
            "opens a diff with the default handler",
            TestOptions(timeout = 60000),
            fun () ->
                withWorkerPool "default" (ResizeArray()) (fun pool _ _ -> promise {
                    let! opened =
                        TextDiffTestSupport.openUntilReady (pool.Service(owner ())) openRequest (OperationContext.detached "open")

                    match opened with
                    | OpenDiffResult.Opened _ -> ()
                    | other -> failwith $"Expected an opened diff, got %A{other}"
                })
        )

        Vitest.test (
            "keeps an active session after a mismatched continuation",
            TestOptions(timeout = 60000),
            fun () ->
                withWorkerPool "default" (ResizeArray()) (fun pool _ transports -> promise {
                    let service = pool.Service(owner ())
                    let! opened = TextDiffTestSupport.openUntilReady service openRequest (OperationContext.detached "open")

                    match opened with
                    | OpenDiffResult.Opened(handle, _, _, _, _) ->
                        transports[0].Post(
                            encode(
                                TextDiffMessage.Request(
                                    "active-slot-mismatch",
                                    1,
                                    RequestBody.Open({ openRequest with Continuation = Some "invalid-continuation" }, owner ())
                                )
                            )
                        )

                        let! sourceInfo =
                            service.GetSourceInfo
                                { SourceInfoRequest.Handle = handle }
                                (OperationContext.detached "source-info-after-mismatch")
                            |> Async.StartAsPromise

                        Vitest.expect(sourceInfo.IsSucceeded).toBe true
                    | other -> failwith $"Expected an opened diff, got %A{other}"
                })
        )

        Vitest.test (
            "disposes the preparation slot after a mismatched continuation",
            TestOptions(timeout = 120000),
            fun () ->
                withWorkerPool "slow-preparation" (ResizeArray()) (fun pool _ transports -> promise {
                    let service = pool.Service(owner ())
                    let request = { openRequest with Path = path "retained.txt" }
                    let! initial = service.Open request (OperationContext.detached "preparation-open") |> Async.StartAsPromise

                    let continuation =
                        match initial with
                        | Succeeded outcome ->
                            match outcome.Value with
                            | Resumable.Scanning(_, continuation, _) -> continuation
                            | other -> failwith $"Expected a preparation continuation, got %A{other}"
                        | other -> failwith $"Open failed before returning a continuation: %A{other}"

                    let mismatched = { request with Path = path "large.txt"; Continuation = Some continuation }
                    let! mismatch = service.Open mismatched (OperationContext.detached "preparation-mismatch") |> Async.StartAsPromise

                    match mismatch with
                    | Failed failure -> Vitest.expect(failure.Code).toBe TextDiffFailureCodes.ContinuationMismatch
                    | other -> failwith $"Expected a continuation mismatch, got %A{other}"

                    let probeId = "preparation-slot-probe"
                    let probeReply: JS.Promise<OperationFailure> =
                        JS.Constructors.Promise.Create(fun resolve reject ->
                            transports[0].OnMessage(fun raw ->
                                match decodeMessage raw with
                                | Ok(TextDiffMessage.Error(requestId, _, failure)) when requestId = probeId -> resolve failure
                                | Ok(TextDiffMessage.Result(requestId, _, _)) when requestId = probeId ->
                                    reject (box (InvalidOperationException("The worker kept the mismatched preparation slot.")))
                                | _ -> ()))

                    transports[0].Post(
                        encode(
                            TextDiffMessage.Request(
                                probeId,
                                1,
                                RequestBody.Open({ request with Continuation = Some continuation }, owner ())
                            )
                        )
                    )

                    let! probeFailure = probeReply
                    Vitest.expect(probeFailure.Code).toBe TextDiffFailureCodes.SessionClosed
                })
        )

        Vitest.test (
            "answers source_changed to the continuation of an Open whose working file changed",
            TestOptions(timeout = 120000),
            fun () ->
                withWorkerPool "slow-preparation" (ResizeArray()) (fun pool _ _ -> promise {
                    let repository = currentFixture().Repository
                    let filePath = NodePath.join [| repository; "changing.txt" |]
                    do! writeLargeTextFile filePath (2L * 1024L * 1024L)
                    do! runGitOk repository [| "add"; "--"; "changing.txt" |]
                    do! runGitOk repository [| "commit"; "-q"; "-m"; "changing blob" |]
                    NodeFileSystem.writeFileSync filePath "changed after commit\n" NodeFileSystem.TextEncoding.Utf8
                    let service = pool.Service(owner ())
                    let request = { openRequest with Path = path "changing.txt" }
                    let! initial = service.Open request (OperationContext.detached "changing-open") |> Async.StartAsPromise

                    let continuation =
                        match initial with
                        | Succeeded { Value = Resumable.Scanning(_, continuation, _) } -> continuation
                        | other -> failwith $"Expected a Scanning answer, got %A{other}"

                    NodeFileSystem.writeFileSync filePath "changed again while scanning\n" NodeFileSystem.TextEncoding.Utf8

                    let! result =
                        service.Open { request with Continuation = Some continuation } (OperationContext.detached "changing-continue")
                        |> Async.StartAsPromise

                    match result with
                    | Failed failure -> Vitest.expect(failure.Code).toBe TextDiffFailureCodes.SourceChanged
                    | other -> failwith $"Expected source_changed, got %A{other}"
                })
        )

        Vitest.test (
            "answers a session closed failure for a continuation without a preparation slot",
            TestOptions(timeout = 120000),
            fun () ->
                withWorkerPool "default" (ResizeArray()) (fun pool _ _ -> promise {
                    let service = pool.Service(owner ())
                    let request = { openRequest with Path = path "retained.txt"; Continuation = Some "evicted-continuation" }
                    let! result = service.Open request (OperationContext.detached "unknown-continuation") |> Async.StartAsPromise

                    match result with
                    | Failed failure -> Vitest.expect(failure.Code).toBe TextDiffFailureCodes.SessionClosed
                    | other -> failwith $"Expected a closed session failure, got %A{other}"
                })
        )

        Vitest.test (
            "cancels a running Open and releases the resolver child",
            TestOptions(timeout = 120000),
            fun () ->
                let events = ResizeArray<TextDiffSupervisor.SupervisorEvent>()

                withWorkerPool "slow-resolver" events (fun pool _ _ -> promise {
                    let source = OperationCancellation.Source()
                    let context = OperationContext.create "open" source.Cancellation ignore
                    let opening = (pool.Service(owner ())).Open openRequest context |> Async.StartAsPromise
                    do! waitFor "the resolver Git child" 30000.0 (fun () -> Promise.lift ((spawnedPids events).Length >= 3))
                    let blobPid = (spawnedPids events)[2]

                    let started = performanceNow ()
                    source.Cancel()
                    let! result = opening
                    let elapsed = performanceNow () - started
                    writeLog $"Worker cancel latency: %.1f{elapsed} ms"

                    match result with
                    | Failed failure -> Vitest.expect(failure.Code).toBe "operation_canceled"
                    | other -> failwith $"Expected a canceled Open, got %A{other}"

                    Vitest.expect(elapsed <= 250.0).toBe true
                    let! goneAfter = waitForGone blobPid 5000.0
                    Vitest.expect(goneAfter.IsSome).toBe true
                })
        )

        Vitest.test (
            "fails the request and cleans up when the worker is terminated",
            TestOptions(timeout = 120000),
            fun () ->
                let events = ResizeArray<TextDiffSupervisor.SupervisorEvent>()

                withWorkerPool "spool" events (fun pool supervisor transports -> promise {
                    let opening = (pool.Service(owner ())).Open openRequest (OperationContext.detached "open") |> Async.StartAsPromise
                    do! waitFor "the blob child" 30000.0 (fun () -> Promise.lift ((spawnedPids events).Length >= 3))
                    let blobPid = (spawnedPids events)[2]

                    do!
                        waitFor "the spool to grow" 30000.0 (fun () ->
                            largestSpool supervisor.InstanceDirectory |> Promise.map (fun size -> size >= 8L * 1024L * 1024L))

                    let started = performanceNow ()
                    do! transports[0].Terminate()
                    let! result = opening

                    match result with
                    | Failed failure -> Vitest.expect(failure.Code).toBe TextDiffFailureCodes.WorkerFailed
                    | other -> failwith $"Expected a worker failure, got %A{other}"

                    let! goneAfter = waitForGone blobPid 2000.0
                    let elapsed = performanceNow () - started

                    match goneAfter with
                    | Some _ -> writeLog $"Worker termination to child exit: %.1f{elapsed} ms"
                    | None -> failwith "The blob child stayed alive for more than two seconds after the worker was terminated."

                    let findEvent predicate =
                        events |> Seq.tryFindIndex (fun (event: TextDiffSupervisor.SupervisorEvent) -> predicate event.Kind)

                    do!
                        waitFor "the worker directory removal" 10000.0 (fun () ->
                            Promise.lift (
                                (findEvent (function
                                    | TextDiffSupervisor.WorkerDirectoryDeleted _ -> true
                                    | _ -> false))
                                    .IsSome
                            ))

                    let childClosed =
                        findEvent (function
                            | TextDiffSupervisor.ChildClosed pid -> pid = blobPid
                            | _ -> false)

                    let spoolDeleted =
                        findEvent (function
                            | TextDiffSupervisor.SpoolDeleted _ -> true
                            | _ -> false)

                    let workerDeleted =
                        findEvent (function
                            | TextDiffSupervisor.WorkerDirectoryDeleted _ -> true
                            | _ -> false)

                    match childClosed, spoolDeleted, workerDeleted with
                    | Some child, Some spool, Some worker ->
                        Vitest.expect(child < spool).toBe true
                        Vitest.expect(spool < worker).toBe true
                    | _ -> failwith $"Missing cleanup events: %A{events}"
                })
        )
)

/// Counts the setTimeout calls without a delay until it is restored.
type private TimerSpy =
    abstract ZeroDelayCalls: int
    abstract Restore: unit -> unit

[<Emit("(() => { const original = globalThis.setTimeout; let zero = 0; globalThis.setTimeout = function (callback, delay, ...rest) { if (!delay) zero++; return original.call(this, callback, delay, ...rest); }; return { get ZeroDelayCalls() { return zero; }, Restore() { globalThis.setTimeout = original; } }; })()")>]
let private spyOnZeroDelayTimeouts () : TimerSpy = jsNative

// The hijack lives on the prototype of the trampoline that every running async carries in its context.
[<Emit("(ctx) => { ctx.onSuccess(Object.getPrototypeOf(ctx.trampoline).hijack); }")>]
let private currentHijack: Async<obj> = jsNative

[<Emit("(ctx) => { Object.getPrototypeOf(ctx.trampoline).hijack = $0; ctx.onSuccess(); }")>]
let private restoreHijack (_hijack: obj) : Async<unit> = jsNative

let rec private bindLoop (remaining: int) : Async<unit> = async {
    if remaining > 0 then
        let! _ = async.Return remaining
        return! bindLoop (remaining - 1)
}

Vitest.describe (
    "Text diff worker trampoline",
    fun () ->
        Vitest.test (
            "long bind chains hop through setImmediate and schedule no zero-delay timeout",
            fun () -> promise {
                let! original = Async.StartAsPromise currentHijack
                let spy = spyOnZeroDelayTimeouts ()

                try
                    VersionControlService.TextDiff.AsyncTrampoline.switchToSetImmediate ()
                    do! Async.StartAsPromise(bindLoop 20000)
                    Vitest.expect(spy.ZeroDelayCalls).toBe 0
                finally
                    spy.Restore()
                    Async.StartImmediate(restoreHijack original)
            }
        )
)
