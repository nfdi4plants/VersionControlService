module VersionControlService.Tests.TextDiffPoolTests

open System
open Fable.Core
open Vitest
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffProtocol
open VersionControlService.Git.TextDiff.TextDiffPreparation
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

type private PreparationHandler(issueFirstOpen: bool) =
    let tokens = PreparationTokenStore()
    let mutable issueNext = issueFirstOpen

    let binding (request: OpenDiffRequest) : PreparationBinding = {
        Path = RepositoryPath.value request.Path
        PreviousPath = request.PreviousPath |> Option.map RepositoryPath.value
        CommitId = None
        Previous = SideIdentity.NoSource
        Current = SideIdentity.NoSource
    }

    let opened (host: WorkerHost) =
        Ok(
            Resumable.Ready(
                OpenDiffResult.Opened({ Id = host.WorkerId; Version = "1" }, sourceInfo, sourceInfo, Resumable.Ready page)
            )
        )

    member val PreparationAttempts = 0 with get, set
    member val IssuedOn: string option = None with get, set
    member val PreparationWorkers = ResizeArray<string>()
    member val OpenedOn = ResizeArray<string>()
    member val HoldReadPage = false with get, set
    member val ReadPageCalls = 0 with get, set

    interface ITextDiffRequestHandler with
        member this.Open(host, request, owner) =
            let sourceBinding = binding request

            match request.Preparation with
            | Some token ->
                this.PreparationAttempts <- this.PreparationAttempts + 1
                this.PreparationWorkers.Add host.WorkerId

                match tokens.Validate(token, sourceBinding, owner.WindowOwner) with
                | Ok() -> Promise.lift(opened host)
                | Error failure -> Promise.lift(Error failure)
            | None when issueNext ->
                issueNext <- false
                this.IssuedOn <- Some host.WorkerId
                let token = tokens.Issue(sourceBinding, owner.WindowOwner)

                Promise.lift(
                    Ok(
                        Resumable.Ready(
                            OpenDiffResult.NotDiffable(
                                DiffBlocker.EncodingRequired(
                                    DiffSide.Current,
                                    token,
                                    [| { Encoding = "utf-8"; Preview = "" } |]
                                )
                            )
                        )
                    )
                )
            | None ->
                this.OpenedOn.Add host.WorkerId
                Promise.lift(opened host)

        member this.ReadPage(host, request) = promise {
            this.ReadPageCalls <- this.ReadPageCalls + 1

            while this.HoldReadPage && not (host.IsCanceled()) do
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
        member _.Close(_, _) = Promise.lift ()
        member _.Cancel _ = ()

let private preparationToken (result: OperationResult<Resumable<OpenDiffResult>>) =
    match result with
    | Succeeded outcome ->
        match outcome.Value with
        | Resumable.Ready(OpenDiffResult.NotDiffable(DiffBlocker.EncodingRequired(_, token, _))) -> token
        | other -> failwith $"Expected an encoding token, got %A{other}"
    | other -> failwith $"Expected an encoding result, got %A{other}"

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

type private ManualTransport() =
    let posted = ResizeArray<obj>()
    let mutable receive = ignore

    member _.Posted = posted
    member _.Inject(message: obj) = receive message

    interface ITextDiffWorkerTransport with
        member _.Post message = posted.Add message
        member _.OnMessage handler = receive <- handler
        member _.OnError _ = ()
        member _.OnExit _ = ()
        member _.Terminate() = Promise.lift ()

let private createTempDirectory () : JS.Promise<string> =
    mkdtemp (NodePath.join [| systemTempDirectory (); "vcs-text-diff-pool-" |])

let private withPoolConfigured
    (maxWorkers: int)
    (sessionsPerWorker: int)
    (factory: TextDiffWorkerFactory)
    (configure: TextDiffPool.TextDiffPoolOptions -> TextDiffPool.TextDiffPoolOptions)
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
            TextDiffPool.create (
                configure {
                    (TextDiffPool.TextDiffPoolOptions.create factory supervisor) with
                        MaxWorkers = maxWorkers
                        SessionsPerWorker = sessionsPerWorker
                }
            )

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

let private withPool
    (maxWorkers: int)
    (sessionsPerWorker: int)
    (factory: TextDiffWorkerFactory)
    (body: TextDiffPool.TextDiffPool -> JS.Promise<unit>)
    : JS.Promise<unit> =
    withPoolConfigured maxWorkers sessionsPerWorker factory id body

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
                    do! waitUntil (fun () -> control.Canceled.Count = 1)
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
            "keeps a preparation token on its issuing worker",
            TestOptions(timeout = 60000),
            fun () ->
                let handlers = ResizeArray<PreparationHandler>()

                let factory: TextDiffWorkerFactory =
                    fun _ ->
                        let handler = PreparationHandler(handlers.Count = 0)
                        handlers.Add handler
                        InProcessTransport.create (handler :> ITextDiffRequestHandler)

                withPool 2 2 factory (fun pool -> promise {
                    let service = pool.Service owner
                    let! first = service.Open openRequest (OperationContext.detached "open-token") |> run
                    let token = preparationToken first
                    do! delay 100

                    let! openedOnA = service.Open openRequest (OperationContext.detached "open-a") |> run
                    openedHandle openedOnA |> ignore

                    let! openedOnB = service.Open openRequest (OperationContext.detached "open-b") |> run
                    openedHandle openedOnB |> ignore

                    let! openedOnAAgain = service.Open openRequest (OperationContext.detached "open-a-again") |> run
                    openedHandle openedOnAAgain |> ignore

                    let! reopened =
                        service.Open { openRequest with Preparation = Some token } (OperationContext.detached "reopen-token")
                        |> run

                    openedHandle reopened |> ignore
                    Vitest.expect(handlers[0].PreparationAttempts).toBe 1
                    Vitest.expect(handlers[1].PreparationAttempts).toBe 0
                })
        )

        Vitest.test (
            "keeps token affinity after a queued reopen is canceled",
            TestOptions(timeout = 60000),
            fun () ->
                let handlers = ResizeArray<PreparationHandler>()

                let factory: TextDiffWorkerFactory =
                    fun _ ->
                        let handler = PreparationHandler(handlers.Count = 0)
                        handlers.Add handler
                        InProcessTransport.create (handler :> ITextDiffRequestHandler)

                withPool 2 2 factory (fun pool -> promise {
                    pool.Prewarm()
                    do! waitUntil (fun () -> handlers.Count = 2)
                    let service = pool.Service owner
                    let! issued = service.Open openRequest (OperationContext.detached "issue-token") |> run
                    let token = preparationToken issued
                    let issuingHandler = handlers |> Seq.find (fun handler -> handler.IssuedOn.IsSome)
                    let issuingWorker = issuingHandler.IssuedOn |> Option.get
                    // The issuing worker's reservation for the token request releases asynchronously.
                    // Waiting here lets both workers settle back to idle before the next Open is admitted,
                    // so the pool's preference for an empty worker lands it on the issuing worker.
                    do! delay 100

                    let! opened = service.Open openRequest (OperationContext.detached "open-issuing-worker") |> run
                    let handle = openedHandle opened
                    Vitest.expect(issuingHandler.OpenedOn[0]).toBe issuingWorker

                    issuingHandler.HoldReadPage <- true
                    let running = Watched(readPage service handle "held" (OperationContext.detached "held-read"))
                    do! waitUntil (fun () -> issuingHandler.ReadPageCalls = 1)

                    let source, cancellationContext = context ()
                    let reopening =
                        Watched(
                            service.Open
                                { openRequest with Preparation = Some token }
                                cancellationContext
                            |> run
                        )

                    do! delay 30
                    source.Cancel()
                    let! canceledOpen = reopening.Result
                    Vitest.expect(failureCode canceledOpen).toBe "operation_canceled"
                    Vitest.expect(running.Settled).toBe false

                    issuingHandler.HoldReadPage <- false
                    let! _ = running.Result
                    let! retried =
                        service.Open { openRequest with Preparation = Some token } (OperationContext.detached "retry-token")
                        |> run

                    openedHandle retried |> ignore
                    Vitest.expect(issuingHandler.PreparationWorkers.ToArray()).toEqual [| issuingWorker |]
                    for handler in handlers do
                        if not (obj.ReferenceEquals(handler, issuingHandler)) then
                            Vitest.expect(handler.PreparationAttempts).toBe 0
                })
        )

        Vitest.test (
            "admits expired and unknown preparation tokens through normal worker selection",
            TestOptions(timeout = 60000),
            fun () ->
                let mutable now = 0.0
                let firstHandler = PreparationHandler(true)
                let secondHandler = PreparationHandler(true)
                let mutable created = 0

                let factory: TextDiffWorkerFactory =
                    fun _ ->
                        let handler =
                            if created = 0 then firstHandler else secondHandler

                        created <- created + 1
                        InProcessTransport.create (handler :> ITextDiffRequestHandler)

                withPoolConfigured
                    2
                    2
                    factory
                    (fun options -> { options with NowMilliseconds = (fun () -> now) })
                    (fun pool -> promise {
                        let service = pool.Service owner
                        let! issuedOnA = service.Open openRequest (OperationContext.detached "issue-on-a") |> run
                        let token = preparationToken issuedOnA
                        do! delay 100

                        let! openedOnA = service.Open openRequest (OperationContext.detached "open-a") |> run
                        openedHandle openedOnA |> ignore

                        let! issuedOnB = service.Open openRequest (OperationContext.detached "issue-on-b") |> run
                        preparationToken issuedOnB |> ignore
                        do! delay 100

                        now <- TokenLifetimeMilliseconds + 1.0
                        let! expired =
                            service.Open { openRequest with Preparation = Some token } (OperationContext.detached "open-expired")
                            |> run

                        do! delay 100
                        let unknown = { PreparationToken.Id = "unknown-preparation-token" }
                        let! unknownResult =
                            service.Open { openRequest with Preparation = Some unknown } (OperationContext.detached "open-unknown")
                            |> run

                        Vitest.expect(failureCode expired).toBe TextDiffFailureCodes.PreparationMismatch
                        Vitest.expect(failureCode unknownResult).toBe TextDiffFailureCodes.PreparationMismatch
                        Vitest.expect(secondHandler.PreparationAttempts).toBe 2
                    })
        )

        Vitest.test (
            "fails an Open when a worker misses its init timeout",
            TestOptions(timeout = 60000),
            fun () ->
                let control = Control()
                let mutable created = 0
                let mutable firstTap: Tap option = None

                let isInitAck (message: obj) =
                    match decodeMessage message with
                    | Ok(TextDiffMessage.InitAck _) -> true
                    | _ -> false

                let factory: TextDiffWorkerFactory =
                    fun _ ->
                        if created = 0 then
                            created <- created + 1
                            let tap = Tap(InProcessTransport.create (testHandler control), isInitAck)
                            firstTap <- Some tap
                            tap :> ITextDiffWorkerTransport
                        else
                            created <- created + 1
                            InProcessTransport.create (testHandler control)

                withPoolConfigured
                    1
                    1
                    factory
                    (fun options -> { options with InitTimeoutMs = 100 })
                    (fun pool -> promise {
                        let service = pool.Service owner
                        let opening = Watched(service.Open openRequest (OperationContext.detached "open-timeout") |> run)

                        do!
                            waitUntil (fun () ->
                                match firstTap with
                                | Some tap -> tap.Posted.Count > 0
                                | None -> false)

                        do! delay 250
                        let! failed = opening.Result
                        Vitest.expect(failureCode failed).toBe TextDiffFailureCodes.WorkerFailed

                        let! later = service.Open openRequest (OperationContext.detached "open-after-timeout") |> run
                        openedHandle later |> ignore
                        Vitest.expect(created).toBe 2
                    })
        )

        Vitest.test (
            "closes an Open result that arrives after cancellation",
            TestOptions(timeout = 60000),
            fun () ->
                let transport = ManualTransport()
                let factory: TextDiffWorkerFactory = fun _ -> transport :> ITextDiffWorkerTransport

                withPool 1 1 factory (fun pool -> promise {
                    let source, cancellationContext = context ()
                    let opening = Watched((pool.Service owner).Open openRequest cancellationContext |> run)

                    do! waitUntil (fun () -> transport.Posted.Count > 0)

                    let workerId, epoch =
                        match decodeMessage transport.Posted[0] with
                        | Ok(TextDiffMessage.Init(workerId, epoch, _)) -> workerId, epoch
                        | other -> failwith $"Expected init, got %A{other}"

                    transport.Inject(encode (TextDiffMessage.InitAck(workerId, epoch)))

                    do!
                        waitUntil (fun () ->
                            transport.Posted
                            |> Seq.exists (fun message ->
                                match decodeMessage message with
                                | Ok(TextDiffMessage.Request(_, _, RequestBody.Open _)) -> true
                                | _ -> false))

                    let requestId, generation =
                        transport.Posted
                        |> Seq.choose (fun message ->
                            match decodeMessage message with
                            | Ok(TextDiffMessage.Request(requestId, generation, RequestBody.Open _)) -> Some(requestId, generation)
                            | _ -> None)
                        |> Seq.last

                    source.Cancel()
                    let! canceled = opening.Result
                    Vitest.expect(failureCode canceled).toBe "operation_canceled"

                    let cancelWasPosted =
                        transport.Posted
                        |> Seq.exists (fun message ->
                            match decodeMessage message with
                            | Ok(TextDiffMessage.Cancel(id, currentGeneration)) -> id = requestId && currentGeneration = generation
                            | _ -> false)

                    Vitest.expect(cancelWasPosted).toBe true

                    let workerHandle = { DiffHandle.Id = "late-worker-handle"; Version = "1" }
                    let lateOpen =
                        Resumable.Ready(
                            OpenDiffResult.Opened(workerHandle, sourceInfo, sourceInfo, Resumable.Ready page)
                        )

                    transport.Inject(encode (TextDiffMessage.Result(requestId, generation, ResultPayload.Open lateOpen)))

                    do!
                        waitUntil (fun () ->
                            transport.Posted
                            |> Seq.exists (fun message ->
                                match decodeMessage message with
                                | Ok(TextDiffMessage.Request(_, _, RequestBody.Close _)) -> true
                                | _ -> false))

                    let closedHandles =
                        transport.Posted
                        |> Seq.choose (fun message ->
                            match decodeMessage message with
                            | Ok(TextDiffMessage.Request(_, _, RequestBody.Close handle)) -> Some handle
                            | _ -> None)
                        |> Seq.toArray

                    Vitest.expect(closedHandles).toEqual [| workerHandle |]
                })
        )

        Vitest.test (
            "runs the next queued request after a canceled ReadPage replies",
            TestOptions(timeout = 60000),
            fun () ->
                let transport = ManualTransport()
                let factory: TextDiffWorkerFactory = fun _ -> transport :> ITextDiffWorkerTransport

                withPool 1 1 factory (fun pool -> promise {
                    let service = pool.Service owner
                    let opening = Watched(service.Open openRequest (OperationContext.detached "open") |> run)

                    do! waitUntil (fun () -> transport.Posted.Count > 0)

                    let workerId, epoch =
                        match decodeMessage transport.Posted[0] with
                        | Ok(TextDiffMessage.Init(workerId, epoch, _)) -> workerId, epoch
                        | other -> failwith $"Expected init, got %A{other}"

                    transport.Inject(encode (TextDiffMessage.InitAck(workerId, epoch)))

                    do!
                        waitUntil (fun () ->
                            transport.Posted
                            |> Seq.exists (fun message ->
                                match decodeMessage message with
                                | Ok(TextDiffMessage.Request(_, _, RequestBody.Open _)) -> true
                                | _ -> false))

                    let openRequestId, openGeneration =
                        transport.Posted
                        |> Seq.choose (fun message ->
                            match decodeMessage message with
                            | Ok(TextDiffMessage.Request(requestId, generation, RequestBody.Open _)) -> Some(requestId, generation)
                            | _ -> None)
                        |> Seq.last

                    let workerHandle = { DiffHandle.Id = "worker-handle"; Version = "1" }
                    let opened =
                        Resumable.Ready(
                            OpenDiffResult.Opened(workerHandle, sourceInfo, sourceInfo, Resumable.Ready page)
                        )

                    transport.Inject(encode (TextDiffMessage.Result(openRequestId, openGeneration, ResultPayload.Open opened)))
                    let! openResult = opening.Result
                    let handle = openedHandle openResult

                    let source, cancellationContext = context ()
                    let running = Watched(readPage service handle "canceled" cancellationContext)

                    do!
                        waitUntil (fun () ->
                            transport.Posted
                            |> Seq.exists (fun message ->
                                match decodeMessage message with
                                | Ok(TextDiffMessage.Request(_, _, RequestBody.ReadPage request)) -> request.Cursor = "canceled"
                                | _ -> false))

                    let requestId, generation =
                        transport.Posted
                        |> Seq.choose (fun message ->
                            match decodeMessage message with
                            | Ok(TextDiffMessage.Request(requestId, generation, RequestBody.ReadPage request)) when request.Cursor = "canceled" ->
                                Some(requestId, generation)
                            | _ -> None)
                        |> Seq.last

                    let queued = Watched(readPage service handle "next" (OperationContext.detached "next-read"))
                    do! delay 20
                    source.Cancel()
                    let! canceled = running.Result
                    Vitest.expect(failureCode canceled).toBe "operation_canceled"
                    Vitest.expect(queued.Settled).toBe false

                    let nextWasPosted =
                        transport.Posted
                        |> Seq.exists (fun message ->
                            match decodeMessage message with
                            | Ok(TextDiffMessage.Request(_, _, RequestBody.ReadPage request)) -> request.Cursor = "next"
                            | _ -> false)

                    Vitest.expect(nextWasPosted).toBe false
                    transport.Inject(encode (TextDiffMessage.Result(requestId, generation, ResultPayload.ReadPage(Resumable.Ready page))))

                    do!
                        waitUntil (fun () ->
                            transport.Posted
                            |> Seq.exists (fun message ->
                                match decodeMessage message with
                                | Ok(TextDiffMessage.Request(_, _, RequestBody.ReadPage request)) -> request.Cursor = "next"
                                | _ -> false))

                    let nextRequestId, nextGeneration =
                        transport.Posted
                        |> Seq.choose (fun message ->
                            match decodeMessage message with
                            | Ok(TextDiffMessage.Request(requestId, generation, RequestBody.ReadPage request)) when request.Cursor = "next" ->
                                Some(requestId, generation)
                            | _ -> None)
                        |> Seq.last

                    transport.Inject(encode (TextDiffMessage.Result(nextRequestId, nextGeneration, ResultPayload.ReadPage(Resumable.Ready { page with PageId = "next" }))))
                    let! next = queued.Result

                    match next with
                    | Succeeded result -> Vitest.expect(result.Value = Resumable.Ready { page with PageId = "next" }).toBe true
                    | other -> failwith $"Queued ReadPage failed: %A{other}"
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
