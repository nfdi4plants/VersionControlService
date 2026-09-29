module VersionControlService.Tests.TextDiffPoolTests

open System
open Fable.Core
open Vitest
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffProtocol
open VersionControlService.Git.TextDiff.TextDiffTransport
open VersionControlService.Git.TextDiff.TextDiffWorkerDispatcher
open VersionControlService.Git.TextDiff.TextDiffInProcessTransport

module NodePath = VersionControlService.Runtime.Node.Path
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module TextDiffSupervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor
module TextDiffPool = VersionControlService.Git.TextDiff.TextDiffPool

[<Import("mkdtemp", "node:fs/promises")>]
let private mkdtemp (prefix: string) : JS.Promise<string> = jsNative

[<Import("tmpdir", "node:os")>]
let private systemTempDirectory () : string = jsNative

[<Import("setTimeout", "node:timers/promises")>]
let private delay (milliseconds: int) : JS.Promise<unit> = jsNative

[<Emit("performance.now()")>]
let private performanceNow () : float = jsNative

let private path value =
    match RepositoryPath.tryCreate value with
    | Ok created -> created
    | Error message -> failwith message

let private sourceInfo: DiffSourceInfo = {
    Path = path "a.txt"
    Revision = None
    IsAbsent = false
    ByteLength = 1L
    LineCount = Some 1L
    Encoding = Some "utf-8"
    EncodingWasChosen = false
    HasBom = false
}

let private page: DiffPage = {
    PageId = "page"
    NextCursor = None
    Parts = [||]
    Progress = { ValidatedBytes = 2L; TotalBytes = 2L; ScanComplete = true }
    OutputComplete = true
    Pending = None
}

let private openRequest: OpenDiffRequest = {
    Path = path "a.txt"
    PreviousPath = None
    Preparation = None
    PreviousEncoding = None
    CurrentEncoding = None
    ContextLines = 3
    Continuation = None
}

let private owner: TextDiffOwner = { WorkspaceRoot = "workspace"; LfsMediaDirectory = "media"; WindowOwner = "window-1" }
let private otherOwner: TextDiffOwner = { WorkspaceRoot = "workspace"; LfsMediaDirectory = "media"; WindowOwner = "window-2" }

/// Lets a test hold ReadPage calls inside the in-process worker and observe what the handler saw.
type private Control() =
    member val HoldReadPage = false with get, set
    member val ReadPageCalls = 0 with get, set
    member val Closed = ResizeArray<string>()
    member val Canceled = ResizeArray<string>()

let private sessionClosed () =
    OperationFailure.create Validation TextDiffFailureCodes.SessionClosed "closed" |> Error |> Promise.lift

let private testHandler (control: Control) =
    { new ITextDiffRequestHandler with
        member _.Open(host, _, _) =
            Promise.lift (
                Ok(
                    Resumable.Ready(
                        OpenDiffResult.Opened({ Id = $"worker-{host.RequestId}"; Version = "1" }, sourceInfo, sourceInfo, Resumable.Ready page)
                    )
                )
            )

        member _.ReadPage(host, request) = promise {
            control.ReadPageCalls <- control.ReadPageCalls + 1

            while control.HoldReadPage && not (host.IsCanceled()) do
                do! delay 10

            if host.IsCanceled() then
                return Error(canceledFailure ())
            else
                return Ok(Resumable.Ready { page with PageId = request.Cursor })
          }

        member _.ReplayPage(_, _) = sessionClosed ()
        member _.Expand(_, _) = sessionClosed ()
        member _.ReadLine(_, _) = sessionClosed ()
        member _.GetSourceInfo(_, _) = sessionClosed ()

        member _.Close(_, handle) =
            control.Closed.Add handle.Id
            Promise.lift ()

        member _.Cancel requestId = control.Canceled.Add requestId
    }

/// Wraps a transport so a test can hold messages on their way to the pool and inject its own.
type private Tap(inner: ITextDiffWorkerTransport, hold: obj -> bool) =
    let mutable host: (obj -> unit) option = None
    let held = ResizeArray<obj>()
    let posted = ResizeArray<obj>()

    member _.Posted = posted
    member _.Inner = inner

    member _.Inject(message: obj) =
        host |> Option.iter (fun receive -> receive message)

    member this.ReleaseHeld() =
        let messages = held.ToArray()
        held.Clear()

        for message in messages do
            this.Inject message

    interface ITextDiffWorkerTransport with
        member _.Post message =
            posted.Add message
            inner.Post message

        member _.OnMessage receive =
            host <- Some receive
            inner.OnMessage(fun message -> if hold message then held.Add message else receive message)

        member _.OnError receive = inner.OnError receive
        member _.OnExit receive = inner.OnExit receive
        member _.Terminate() = inner.Terminate()

let private createTempDirectory () : JS.Promise<string> =
    mkdtemp (NodePath.join [| systemTempDirectory (); "vcs-text-diff-pool-" |])

let private withPool
    (maxWorkers: int)
    (sessionsPerWorker: int)
    (factory: TextDiffWorkerFactory)
    (body: TextDiffPool.TextDiffPool -> JS.Promise<unit>)
    : JS.Promise<unit> =
    promise {
        let! root = createTempDirectory ()

        let! supervisor =
            TextDiffSupervisor.create {
                TextDiffSupervisor.TextDiffSupervisorOptions.defaults with
                    TempRoot = root
            }

        let pool =
            TextDiffPool.create {
                Factory = factory
                Supervisor = supervisor
                MaxWorkers = maxWorkers
                SessionsPerWorker = sessionsPerWorker
            }

        let mutable failure = None

        try
            do! body pool
        with error ->
            failure <- Some error

        do! pool.Dispose()

        try
            do! NodePositionalFile.removeWithRetry root 20 100
        with _ -> ()

        match failure with
        | Some error -> return raise error
        | None -> ()
    }

let private context () =
    let source = OperationCancellation.Source()
    source, OperationContext.create "test" source.Cancellation ignore

let private run (work: Async<'T>) = Async.StartAsPromise work

/// Tracks whether a promise has settled so a test can assert that a call is still waiting.
type private Watched<'T>(work: JS.Promise<'T>) =
    let mutable settled = false

    let observed =
        work
        |> Promise.map (fun value ->
            settled <- true
            value)

    member _.Settled = settled
    member _.Result = observed

let private openedHandle (result: OperationResult<Resumable<OpenDiffResult>>) =
    match result with
    | Succeeded outcome ->
        match outcome.Value with
        | Resumable.Ready(OpenDiffResult.Opened(handle, _, _, _)) -> handle
        | other -> failwith $"Unexpected open result %A{other}"
    | other -> failwith $"Open failed: %A{other}"

let private failureCode (result: OperationResult<'T>) =
    match result with
    | Failed failure -> failure.Code
    | other -> failwith $"Expected a failure, got %A{other}"

let private readPage (service: TextDiffService) (handle: DiffHandle) cursor operationContext =
    service.ReadPage { Handle = handle; Cursor = cursor } operationContext |> run

let private waitUntil (condition: unit -> bool) = promise {
    let started = performanceNow ()

    while not (condition ()) && performanceNow () - started < 5000.0 do
        do! delay 5

    if not (condition ()) then
        failwith "The condition was not reached within five seconds."
}

let private inProcessFactory (control: Control) (created: ResizeArray<ITextDiffWorkerTransport>) : TextDiffWorkerFactory =
    fun _ ->
        let transport = InProcessTransport.create (testHandler control)
        created.Add transport
        transport

Vitest.describe (
    "Text diff pool",
    fun () ->
        Vitest.test (
            "opens, reads and closes a session",
            TestOptions(timeout = 60000),
            fun () ->
                let control = Control()

                withPool 1 1 (inProcessFactory control (ResizeArray())) (fun pool -> promise {
                    let service = pool.Service owner
                    let! opened = service.Open openRequest (OperationContext.detached "open") |> run
                    let handle = openedHandle opened
                    Vitest.expect(handle.Id.StartsWith "worker-").toBe false
                    let! pageResult = readPage service handle "cursor-1" (OperationContext.detached "read")

                    match pageResult with
                    | Succeeded outcome -> Vitest.expect(outcome.Value = Resumable.Ready { page with PageId = "cursor-1" }).toBe true
                    | other -> failwith $"ReadPage failed: %A{other}"

                    let! foreign = readPage (pool.Service otherOwner) handle "cursor-1" (OperationContext.detached "read")
                    Vitest.expect(failureCode foreign).toBe TextDiffFailureCodes.SessionClosed

                    let! closed = service.Close handle (OperationContext.detached "close") |> run
                    Vitest.expect((closed = OperationResult.succeeded ())).toBe true
                    Vitest.expect(control.Closed.Count).toBe 1

                    let! afterClose = readPage service handle "cursor-1" (OperationContext.detached "read")
                    Vitest.expect(failureCode afterClose).toBe TextDiffFailureCodes.SessionClosed
                })
        )

        Vitest.test (
            "admits a waiting Open by closing the least recently used idle session",
            TestOptions(timeout = 60000),
            fun () ->
                let control = Control()

                withPool 1 1 (inProcessFactory control (ResizeArray())) (fun pool -> promise {
                    let service = pool.Service owner
                    let! first = service.Open openRequest (OperationContext.detached "open-1") |> run
                    let firstHandle = openedHandle first
                    let! second = service.Open openRequest (OperationContext.detached "open-2") |> run
                    let secondHandle = openedHandle second
                    Vitest.expect(control.Closed.Count).toBe 1

                    let! evicted = readPage service firstHandle "c" (OperationContext.detached "read")
                    Vitest.expect(failureCode evicted).toBe TextDiffFailureCodes.SessionClosed

                    let! current = readPage service secondHandle "c" (OperationContext.detached "read")
                    Vitest.expect(current.IsSucceeded).toBe true
                })
        )

        Vitest.test (
            "never evicts a session while its request runs",
            TestOptions(timeout = 60000),
            fun () ->
                let control = Control()

                withPool 1 1 (inProcessFactory control (ResizeArray())) (fun pool -> promise {
                    let service = pool.Service owner
                    let! first = service.Open openRequest (OperationContext.detached "open-1") |> run
                    let firstHandle = openedHandle first
                    control.HoldReadPage <- true
                    let running = Watched(readPage service firstHandle "held" (OperationContext.detached "read"))
                    do! waitUntil (fun () -> control.ReadPageCalls = 1)
                    let waiting = Watched(service.Open openRequest (OperationContext.detached "open-2") |> run)
                    do! delay 150

                    Vitest.expect(waiting.Settled).toBe false
                    Vitest.expect(running.Settled).toBe false
                    Vitest.expect(control.Closed.Count).toBe 0

                    control.HoldReadPage <- false
                    let! readResult = running.Result
                    Vitest.expect(readResult.IsSucceeded).toBe true
                    let! second = waiting.Result
                    openedHandle second |> ignore
                    Vitest.expect(control.Closed.Count).toBe 1
                })
        )

        Vitest.test (
            "cancels a queued request without running it",
            TestOptions(timeout = 60000),
            fun () ->
                let control = Control()

                withPool 1 1 (inProcessFactory control (ResizeArray())) (fun pool -> promise {
                    let service = pool.Service owner
                    let! opened = service.Open openRequest (OperationContext.detached "open") |> run
                    let handle = openedHandle opened
                    control.HoldReadPage <- true
                    let running = Watched(readPage service handle "held" (OperationContext.detached "read-1"))
                    do! waitUntil (fun () -> control.ReadPageCalls = 1)
                    let source, queuedContext = context ()
                    let queued = Watched(readPage service handle "queued" queuedContext)
                    do! delay 20
                    source.Cancel()
                    let! queuedResult = queued.Result

                    Vitest.expect(failureCode queuedResult).toBe "operation_canceled"
                    Vitest.expect(running.Settled).toBe false
                    control.HoldReadPage <- false
                    let! runningResult = running.Result
                    Vitest.expect(runningResult.IsSucceeded).toBe true
                    Vitest.expect(control.ReadPageCalls).toBe 1
                    Vitest.expect(control.Canceled.Count).toBe 0
                })
        )

        Vitest.test (
            "cancels a running request through the worker",
            TestOptions(timeout = 60000),
            fun () ->
                let control = Control()

                withPool 1 1 (inProcessFactory control (ResizeArray())) (fun pool -> promise {
                    let service = pool.Service owner
                    let! opened = service.Open openRequest (OperationContext.detached "open") |> run
                    let handle = openedHandle opened
                    control.HoldReadPage <- true
                    let source, runningContext = context ()
                    let running = Watched(readPage service handle "held" runningContext)
                    do! waitUntil (fun () -> control.ReadPageCalls = 1)
                    let started = performanceNow ()
                    source.Cancel()
                    let! result = running.Result
                    let elapsed = performanceNow () - started

                    Vitest.expect(failureCode result).toBe "operation_canceled"
                    Vitest.expect(control.Canceled.Count).toBe 1
                    Vitest.expect(elapsed < 250.0).toBe true

                    control.HoldReadPage <- false
                    let! again = readPage service handle "again" (OperationContext.detached "read-2")
                    Vitest.expect(again.IsSucceeded).toBe true
                })
        )

        Vitest.test (
            "fails every request of a failed worker and starts a replacement",
            TestOptions(timeout = 60000),
            fun () ->
                let control = Control()
                let created = ResizeArray<ITextDiffWorkerTransport>()

                withPool 1 2 (inProcessFactory control created) (fun pool -> promise {
                    let service = pool.Service owner
                    let! opened = service.Open openRequest (OperationContext.detached "open") |> run
                    let handle = openedHandle opened
                    control.HoldReadPage <- true
                    let running = Watched(readPage service handle "held" (OperationContext.detached "read-1"))
                    do! waitUntil (fun () -> control.ReadPageCalls = 1)
                    let queued = Watched(readPage service handle "queued" (OperationContext.detached "read-2"))
                    do! created[0].Terminate()

                    let! runningResult = running.Result
                    let! queuedResult = queued.Result
                    Vitest.expect(failureCode runningResult).toBe TextDiffFailureCodes.WorkerFailed
                    Vitest.expect(failureCode queuedResult).toBe TextDiffFailureCodes.WorkerFailed

                    let! afterFailure = readPage service handle "later" (OperationContext.detached "read-3")
                    Vitest.expect(failureCode afterFailure).toBe TextDiffFailureCodes.SessionClosed

                    control.HoldReadPage <- false
                    let! replacement = service.Open openRequest (OperationContext.detached "open-2") |> run
                    let replacementHandle = openedHandle replacement
                    Vitest.expect(created.Count).toBe 2
                    let! read = readPage service replacementHandle "fresh" (OperationContext.detached "read-4")
                    Vitest.expect(read.IsSucceeded).toBe true
                })
        )

        Vitest.test (
            "treats Close as idempotent",
            TestOptions(timeout = 60000),
            fun () ->
                let control = Control()

                withPool 1 2 (inProcessFactory control (ResizeArray())) (fun pool -> promise {
                    let service = pool.Service owner
                    let! opened = service.Open openRequest (OperationContext.detached "open") |> run
                    let handle = openedHandle opened

                    let! foreignClose = (pool.Service otherOwner).Close handle (OperationContext.detached "close-foreign") |> run
                    Vitest.expect(foreignClose.IsSucceeded).toBe true
                    let! stillOpen = readPage service handle "c" (OperationContext.detached "read")
                    Vitest.expect(stillOpen.IsSucceeded).toBe true

                    let! first = service.Close handle (OperationContext.detached "close-1") |> run
                    let! second = service.Close handle (OperationContext.detached "close-2") |> run
                    let! unknown = service.Close { Id = "unknown"; Version = "1" } (OperationContext.detached "close-3") |> run
                    Vitest.expect(first.IsSucceeded).toBe true
                    Vitest.expect(second.IsSucceeded).toBe true
                    Vitest.expect(unknown.IsSucceeded).toBe true
                    Vitest.expect(control.Closed.Count).toBe 1
                })
        )

        Vitest.test (
            "ignores replies with a stale epoch, generation or request id",
            TestOptions(timeout = 60000),
            fun () ->
                let control = Control()
                let taps = ResizeArray<Tap>()

                let isInitAck (message: obj) =
                    match decodeMessage message with
                    | Ok(TextDiffMessage.InitAck _) -> true
                    | _ -> false

                let factory: TextDiffWorkerFactory =
                    fun _ ->
                        let tap = Tap(InProcessTransport.create (testHandler control), isInitAck)
                        taps.Add tap
                        tap :> ITextDiffWorkerTransport

                withPool 1 1 factory (fun pool -> promise {
                    let service = pool.Service owner
                    let opening = Watched(service.Open openRequest (OperationContext.detached "open") |> run)
                    do! waitUntil (fun () -> taps.Count = 1 && taps[0].Posted.Count >= 1)
                    let tap = taps[0]

                    let workerId, epoch =
                        match decodeMessage tap.Posted[0] with
                        | Ok(TextDiffMessage.Init(workerId, epoch, _)) -> workerId, epoch
                        | other -> failwith $"Expected init, got %A{other}"

                    tap.Inject(encode (TextDiffMessage.InitAck(workerId, epoch + 1)))
                    tap.Inject(encode (TextDiffMessage.InitAck("another-worker", epoch)))
                    do! delay 50
                    Vitest.expect(opening.Settled).toBe false

                    tap.ReleaseHeld()
                    let! opened = opening.Result
                    let handle = openedHandle opened

                    control.HoldReadPage <- true
                    let reading = Watched(readPage service handle "held" (OperationContext.detached "read"))
                    do! waitUntil (fun () -> control.ReadPageCalls = 1)

                    let requestId, generation =
                        tap.Posted
                        |> Seq.choose (fun message ->
                            match decodeMessage message with
                            | Ok(TextDiffMessage.Request(requestId, generation, RequestBody.ReadPage _)) -> Some(requestId, generation)
                            | _ -> None)
                        |> Seq.last

                    tap.Inject(encode (TextDiffMessage.Result(requestId, generation + 1, ResultPayload.Close)))
                    tap.Inject(encode (TextDiffMessage.Result("unknown-request", generation, ResultPayload.Close)))
                    do! delay 50
                    Vitest.expect(reading.Settled).toBe false

                    control.HoldReadPage <- false
                    let! result = reading.Result

                    match result with
                    | Succeeded outcome -> Vitest.expect(outcome.Value = Resumable.Ready { page with PageId = "held" }).toBe true
                    | other -> failwith $"ReadPage failed: %A{other}"
                })
        )
)
