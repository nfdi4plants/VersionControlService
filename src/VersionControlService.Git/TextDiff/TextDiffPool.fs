/// Main-process pool of text diff workers. It admits sessions into a bounded number of slots,
/// runs one request per worker at a time and executes the Git children its workers ask for.
module VersionControlService.Git.TextDiff.TextDiffPool

open System.Collections.Generic
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffProtocol
open VersionControlService.Git.TextDiff.TextDiffTransport

module NodeInterop = VersionControlService.Runtime.Node.Interop
module Supervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor

type TextDiffPoolOptions = {
    Factory: TextDiffWorkerFactory
    Supervisor: Supervisor.TextDiffSupervisor
    MaxWorkers: int
    SessionsPerWorker: int
}

module TextDiffPoolOptions =

    /// Options with two workers and four sessions per worker.
    let create (factory: TextDiffWorkerFactory) (supervisor: Supervisor.TextDiffSupervisor) = {
        Factory = factory
        Supervisor = supervisor
        MaxWorkers = 2
        SessionsPerWorker = 4
    }

type private WorkerPhase =
    | Starting
    | Ready
    | Gone

type private SessionPhase =
    | Opening
    | Scanning
    | Opened
    | Closing
    | Closed

type private PoolWorker(index: int, epoch: int, workerId: string, transport: ITextDiffWorkerTransport) =
    member _.Index = index
    member _.Epoch = epoch
    member _.WorkerId = workerId
    member _.Transport = transport
    member val Phase = Starting with get, set
    member val Sessions = ResizeArray<PoolSession>()
    member val Queue = ResizeArray<PendingRequest>()
    member val Running: PendingRequest option = None with get, set

and private PoolSession(generation: int, worker: PoolWorker, owner: TextDiffOwner) =
    member _.Generation = generation
    member _.Worker = worker
    member _.Owner = owner
    member val Phase = Opening with get, set
    member val WorkerHandle: DiffHandle option = None with get, set
    member val PublicId: string option = None with get, set
    member val Continuation: string option = None with get, set
    member val Active = 0 with get, set
    member val LastUsed = 0 with get, set

and private PendingRequest
    (requestId: string, session: PoolSession, body: RequestBody, context: OperationContext, complete: Result<ResultPayload, OperationFailure> -> unit) =
    member _.RequestId = requestId
    member _.Session = session
    member _.Body = body
    member _.Context = context
    member _.Complete = complete
    member val Settled = false with get, set
    member val CancelPosted = false with get, set

type private Admission(owner: TextDiffOwner, complete: Result<PoolSession, OperationFailure> -> unit) =
    member _.Owner = owner
    member _.Complete = complete
    member val Done = false with get, set

let private workerFailed (message: string) =
    OperationFailure.create ProviderError TextDiffFailureCodes.WorkerFailed message

let private sessionClosed () =
    OperationFailure.create Validation TextDiffFailureCodes.SessionClosed "The diff session is closed."

let private canceled () =
    OperationFailure.create Canceled "operation_canceled" "The text diff request was canceled."

let private observe (work: JS.Promise<'T>) (onSucceeded: 'T -> unit) (onFailed: obj -> unit) =
    NodeInterop.observePromise work onSucceeded onFailed

let private attempt (work: unit -> JS.Promise<'T>) : JS.Promise<'T> =
    try
        work ()
    with error ->
        Promise.reject error

type TextDiffPool internal (options: TextDiffPoolOptions) =
    let supervisor = options.Supervisor
    let workers: PoolWorker option[] = Array.create (max 1 options.MaxWorkers) None
    let sessionsPerWorker = max 1 options.SessionsPerWorker
    let admissions = ResizeArray<Admission>()
    let handles = Dictionary<string, PoolSession>()
    let continuations = Dictionary<string, PoolSession>()
    let mutable disposed = false
    let mutable nextEpoch = 1
    let mutable nextGeneration = 1
    let mutable nextRequestId = 1
    let mutable nextHandleId = 1
    let mutable clock = 0
    let mutable lastStartFailure: string option = None

    let tick () =
        clock <- clock + 1
        clock

    let liveWorkers () =
        workers |> Array.choose id |> Array.filter (fun worker -> worker.Phase <> Gone)

    let forgetSession (session: PoolSession) =
        session.PublicId |> Option.iter (handles.Remove >> ignore)

        session.Continuation
        |> Option.iter (fun continuation ->
            match continuations.TryGetValue continuation with
            | true, registered when obj.ReferenceEquals(registered, session) -> continuations.Remove continuation |> ignore
            | _ -> ())

    let rec post (worker: PoolWorker) (message: TextDiffMessage) =
        if worker.Phase <> Gone then
            try
                worker.Transport.Post(encode message)
            with error ->
                failWorker worker $"Posting to the text diff worker failed: {error.Message}"

    and settle (request: PendingRequest) (result: Result<ResultPayload, OperationFailure>) =
        if not request.Settled then
            request.Settled <- true
            let session = request.Session
            session.Active <- session.Active - 1
            session.LastUsed <- tick ()
            request.Complete result

            if session.Active = 0 && admissions.Count > 0 then
                pumpAdmission ()

    // Runs exactly once per worker. Every request and session of the worker ends here.
    and abandonWorker (worker: PoolWorker) (reason: string) : bool =
        if worker.Phase = Gone then
            false
        else
            worker.Phase <- Gone

            match workers[worker.Index] with
            | Some current when obj.ReferenceEquals(current, worker) -> workers[worker.Index] <- None
            | _ -> ()

            let requests = [
                yield! worker.Running |> Option.toList
                yield! worker.Queue
            ]

            worker.Running <- None
            worker.Queue.Clear()

            for session in worker.Sessions do
                session.Phase <- Closed
                forgetSession session

            worker.Sessions.Clear()

            for request in requests do
                settle request (Error(workerFailed reason))

            true

    and failWorker (worker: PoolWorker) (reason: string) =
        if abandonWorker worker reason then
            observe (attempt (fun () -> supervisor.ReleaseWorker worker.WorkerId)) ignore ignore
            observe (attempt (fun () -> worker.Transport.Terminate())) ignore ignore

            if not disposed then
                pumpAdmission ()

    // One request runs per worker in arrival order, so ReadPage calls on one handle never overlap.
    and pumpWorker (worker: PoolWorker) =
        if worker.Phase = Ready && worker.Running.IsNone && worker.Queue.Count > 0 then
            let request = worker.Queue[0]
            worker.Queue.RemoveAt 0
            worker.Running <- Some request
            post worker (TextDiffMessage.Request(request.RequestId, request.Session.Generation, request.Body))

    and completeRunning (worker: PoolWorker) (requestId: string) (generation: int) (result: Result<ResultPayload, OperationFailure>) =
        match worker.Running with
        | Some request when request.RequestId = requestId && request.Session.Generation = generation ->
            worker.Running <- None
            settle request result
            pumpWorker worker
        | _ -> ()

    and answerSpawn (worker: PoolWorker) (callId: int) (outcome: SpawnOutcome) =
        post worker (TextDiffMessage.SpawnResult(callId, outcome))

    and spawnFailed (worker: PoolWorker) (callId: int) (error: obj) =
        answerSpawn worker callId (SpawnOutcome.Failed(NodeInterop.errorMessage error))

    and onWorkerMessage (worker: PoolWorker) (raw: obj) =
        if worker.Phase <> Gone && not disposed then
            match decodeMessage raw with
            | Error reason -> failWorker worker $"The text diff worker sent an invalid message: {reason}"
            | Ok message ->
                match message with
                | TextDiffMessage.InitAck(workerId, epoch) ->
                    if worker.Phase = Starting && workerId = worker.WorkerId && epoch = worker.Epoch then
                        worker.Phase <- Ready
                        pumpWorker worker
                | TextDiffMessage.WorkerFailure reason -> failWorker worker $"The text diff worker reported a failure: {reason}"
                | TextDiffMessage.Progress(requestId, generation, validated, total) ->
                    match worker.Running with
                    | Some request when request.RequestId = requestId && request.Session.Generation = generation ->
                        request.Context.ReportProgress {
                            PhaseCode = "text_diff_scan"
                            Item = None
                            Completed = Some(float validated)
                            Total = Some(float total)
                            DisplayMessage = None
                        }
                    | _ -> ()
                | TextDiffMessage.Result(requestId, generation, payload) -> completeRunning worker requestId generation (Ok payload)
                | TextDiffMessage.Error(requestId, generation, failure) -> completeRunning worker requestId generation (Error failure)
                | TextDiffMessage.SpawnShort(callId, owner, cwd, arguments) ->
                    if owner.WorkerId <> worker.WorkerId then
                        answerSpawn worker callId (SpawnOutcome.Failed "The spawn owner does not belong to this worker.")
                    else
                        observe
                            (attempt (fun () -> supervisor.RunShort(owner, cwd, arguments)))
                            (fun result -> answerSpawn worker callId (SpawnOutcome.Short result))
                            (spawnFailed worker callId)
                | TextDiffMessage.SpawnBlob(callId, owner, cwd, oid, spoolPath) ->
                    if owner.WorkerId <> worker.WorkerId then
                        answerSpawn worker callId (SpawnOutcome.Failed "The spawn owner does not belong to this worker.")
                    else
                        observe
                            (attempt (fun () -> supervisor.StartBlobToSpool(owner, cwd, oid, spoolPath)))
                            (fun child -> observe child.Closed (fun exit -> answerSpawn worker callId (SpawnOutcome.Blob exit)) (spawnFailed worker callId))
                            (spawnFailed worker callId)
                | TextDiffMessage.ReleaseRequest owner ->
                    if owner.WorkerId = worker.WorkerId then
                        observe (attempt (fun () -> supervisor.ReleaseRequest owner)) ignore ignore
                | TextDiffMessage.ReleaseSession(workerId, sessionId) ->
                    if workerId = worker.WorkerId then
                        observe (attempt (fun () -> supervisor.ReleaseSession(workerId, sessionId))) ignore ignore
                | _ -> ()

    and startWorker (index: int) : PoolWorker option =
        let epoch = nextEpoch
        nextEpoch <- nextEpoch + 1
        let workerId = $"worker-{index}-{epoch}"

        let created =
            try
                Ok(options.Factory index)
            with error ->
                Error error.Message

        match created with
        | Error message ->
            lastStartFailure <- Some message
            None
        | Ok transport ->
            lastStartFailure <- None
            let worker = PoolWorker(index, epoch, workerId, transport)
            workers[index] <- Some worker
            transport.OnMessage(onWorkerMessage worker)
            transport.OnError(fun reason -> failWorker worker $"The text diff worker failed: {reason}")
            transport.OnExit(fun code -> failWorker worker $"The text diff worker exited with code {code}.")

            observe
                (attempt (fun () -> supervisor.WorkerDirectory workerId))
                (fun directory ->
                    if worker.Phase = Starting then
                        post worker (TextDiffMessage.Init(workerId, epoch, directory)))
                (fun error -> failWorker worker $"The text diff worker directory could not be created: {NodeInterop.errorMessage error}")

            Some worker

    // Prefers an empty worker, then a new worker, then the least loaded worker with a free slot.
    and tryReserve (owner: TextDiffOwner) : Result<PoolSession, OperationFailure> option =
        let candidate =
            liveWorkers ()
            |> Array.filter (fun worker -> worker.Sessions.Count < sessionsPerWorker)
            |> Array.sortBy (fun worker -> worker.Sessions.Count)
            |> Array.tryHead

        let emptyIndex = workers |> Array.tryFindIndex Option.isNone

        let chosen =
            match candidate, emptyIndex with
            | Some worker, _ when worker.Sessions.Count = 0 -> Some(Ok worker)
            | _, Some index ->
                match startWorker index, candidate with
                | Some worker, _ -> Some(Ok worker)
                | None, Some worker -> Some(Ok worker)
                | None, None ->
                    let reason = lastStartFailure |> Option.defaultValue "unknown error"
                    Some(Error(workerFailed $"The text diff worker could not be started: {reason}"))
            | Some worker, None -> Some(Ok worker)
            | None, None -> None

        chosen
        |> Option.map (
            Result.map (fun worker ->
                let session = PoolSession(nextGeneration, worker, owner)
                nextGeneration <- nextGeneration + 1
                session.LastUsed <- tick ()
                worker.Sessions.Add session
                session)
        )

    and idleSessions () =
        liveWorkers ()
        |> Seq.collect _.Sessions
        |> Seq.filter (fun session -> (session.Phase = Opened || session.Phase = Scanning) && session.Active = 0)

    and closingCount () =
        liveWorkers ()
        |> Seq.collect _.Sessions
        |> Seq.filter (fun session -> session.Phase = Closing)
        |> Seq.length

    and pumpAdmission () =
        let mutable blocked = false

        while not blocked && not disposed && admissions.Count > 0 do
            let admission = admissions[0]

            match tryReserve admission.Owner with
            | Some result ->
                admissions.RemoveAt 0
                admission.Done <- true
                admission.Complete result
            | None -> blocked <- true

        // Evicts only as many idle sessions as there are waiting Opens beyond the slots already being freed.
        let mutable evicting = true

        while evicting && not disposed && closingCount () < admissions.Count do
            match idleSessions () |> Seq.sortBy _.LastUsed |> Seq.tryHead with
            | Some session -> observe (closeSession session) ignore ignore
            | None -> evicting <- false

    and enqueue (session: PoolSession) (body: RequestBody) (context: OperationContext) : JS.Promise<Result<ResultPayload, OperationFailure>> =
        Promise.create (fun resolve _ ->
            let worker = session.Worker

            if context.Cancellation.IsCancellationRequested() then
                resolve (Error(canceled ()))
            elif worker.Phase = Gone || disposed then
                resolve (Error(workerFailed "The text diff worker is no longer running."))
            else
                let requestId = $"r{nextRequestId}"
                nextRequestId <- nextRequestId + 1
                let request = PendingRequest(requestId, session, body, context, resolve)
                session.Active <- session.Active + 1
                session.LastUsed <- tick ()
                worker.Queue.Add request
                context.Cancellation.Register(fun () -> cancelRequest request)
                pumpWorker worker)

    and cancelRequest (request: PendingRequest) =
        if not request.Settled then
            let worker = request.Session.Worker
            let index = worker.Queue.IndexOf request

            if index >= 0 then
                worker.Queue.RemoveAt index
                settle request (Error(canceled ()))
            else
                match worker.Running with
                | Some running when obj.ReferenceEquals(running, request) && not request.CancelPosted ->
                    request.CancelPosted <- true
                    post worker (TextDiffMessage.Cancel(request.RequestId, request.Session.Generation))
                | _ -> ()

    // The slot stays taken until the worker acknowledged the close and the supervisor released the session.
    and closeSession (session: PoolSession) : JS.Promise<unit> =
        match session.Phase with
        | Opened
        | Scanning ->
            session.Phase <- Closing
            forgetSession session
            let handle = session.WorkerHandle |> Option.defaultValue { DiffHandle.Id = ""; Version = "" }

            promise {
                let! _ = enqueue session (RequestBody.Close handle) (OperationContext.detached "text-diff-close")
                do! releaseSlot session
            }
        | Opening
        | Closing
        | Closed -> Promise.lift ()

    and releaseSlot (session: PoolSession) : JS.Promise<unit> = promise {
        if session.Worker.Phase <> Gone then
            try
                do! supervisor.ReleaseSession(session.Worker.WorkerId, string session.Generation)
            with _ -> ()

        session.Phase <- Closed
        session.Worker.Sessions.Remove session |> ignore
        pumpAdmission ()
    }

    let releaseReservation (session: PoolSession) =
        match session.Phase with
        | Opening
        | Scanning ->
            session.Phase <- Closing
            forgetSession session
            observe (releaseSlot session) ignore ignore
        | Opened
        | Closing
        | Closed -> ()

    let admit (owner: TextDiffOwner) (context: OperationContext) : JS.Promise<Result<PoolSession, OperationFailure>> =
        Promise.create (fun resolve _ ->
            if context.Cancellation.IsCancellationRequested() then
                resolve (Error(canceled ()))
            elif disposed then
                resolve (Error(workerFailed "The text diff pool was disposed."))
            else
                let admission = Admission(owner, resolve)
                admissions.Add admission

                context.Cancellation.Register(fun () ->
                    if not admission.Done then
                        admission.Done <- true
                        admissions.Remove admission |> ignore
                        resolve (Error(canceled ())))

                pumpAdmission ())

    let openDiff (owner: TextDiffOwner) (request: OpenDiffRequest) (context: OperationContext) = promise {
        let resumed =
            request.Continuation
            |> Option.bind (fun continuation ->
                match continuations.TryGetValue continuation with
                | true, session when session.Owner = owner && session.Phase = Scanning && session.Active = 0 ->
                    continuations.Remove continuation |> ignore
                    session.Continuation <- None
                    session.Phase <- Opening
                    Some session
                | _ -> None)

        let! reservation =
            match resumed with
            | Some session -> Promise.lift (Ok session)
            | None -> admit owner context

        match reservation with
        | Error failure -> return Failed failure
        | Ok session ->
            let! reply = enqueue session (RequestBody.Open(request, owner)) context

            match reply with
            | Ok(ResultPayload.Open result) when session.Phase = Opening ->
                match result with
                | Resumable.Ready(OpenDiffResult.Opened(handle, previous, current, first)) ->
                    let publicHandle = { DiffHandle.Id = $"h{nextHandleId}"; Version = handle.Version }
                    nextHandleId <- nextHandleId + 1
                    session.WorkerHandle <- Some handle
                    session.PublicId <- Some publicHandle.Id
                    session.Phase <- Opened
                    handles[publicHandle.Id] <- session
                    return OperationResult.succeeded (Resumable.Ready(OpenDiffResult.Opened(publicHandle, previous, current, first)))
                | Resumable.Scanning(_, continuation, _) ->
                    session.Phase <- Scanning
                    session.Continuation <- Some continuation
                    continuations[continuation] <- session
                    return OperationResult.succeeded result
                | Resumable.Ready(OpenDiffResult.NotDiffable _) ->
                    releaseReservation session
                    return OperationResult.succeeded result
            | Ok(ResultPayload.Open _) ->
                releaseReservation session
                return Failed(workerFailed "The text diff session ended while it was opening.")
            | Ok _ ->
                releaseReservation session
                return Failed(workerFailed "The text diff worker answered Open with a different result type.")
            | Error failure ->
                releaseReservation session
                return Failed failure
    }

    let handleCall
        (owner: TextDiffOwner)
        (handle: DiffHandle)
        (context: OperationContext)
        (build: DiffHandle -> RequestBody)
        (extract: ResultPayload -> 'T option)
        : JS.Promise<OperationResult<'T>> =
        let target =
            match handles.TryGetValue handle.Id with
            | true, session when session.Owner = owner && session.Phase = Opened ->
                session.WorkerHandle
                |> Option.filter (fun workerHandle -> workerHandle.Version = handle.Version)
                |> Option.map (fun workerHandle -> session, workerHandle)
            | _ -> None

        promise {
            match target with
            | None -> return Failed(sessionClosed ())
            | Some(session, workerHandle) ->
                let! reply = enqueue session (build workerHandle) context

                match reply with
                | Ok payload ->
                    match extract payload with
                    | Some value -> return OperationResult.succeeded value
                    | None -> return Failed(workerFailed "The text diff worker answered with a different result type.")
                | Error failure -> return Failed failure
        }

    let closeHandle (owner: TextDiffOwner) (handle: DiffHandle) = promise {
        match handles.TryGetValue handle.Id with
        | true, session when session.Owner = owner -> do! closeSession session
        | _ -> ()

        return OperationResult.succeeded ()
    }

    /// Starts every worker that is not running yet. Start failures are kept and retried by the next Open.
    member _.Prewarm() : unit =
        if not disposed then
            for index in 0 .. workers.Length - 1 do
                if workers[index].IsNone then
                    try
                        startWorker index |> ignore
                    with _ -> ()

    /// Shuts every worker down and disposes the supervisor. Waiting and running calls fail with diff_worker_failed.
    member _.Dispose() : JS.Promise<unit> = promise {
        if not disposed then
            disposed <- true

            for admission in admissions.ToArray() do
                if not admission.Done then
                    admission.Done <- true
                    admission.Complete(Error(workerFailed "The text diff pool was disposed."))

            admissions.Clear()

            for worker in liveWorkers () do
                try
                    worker.Transport.Post(encode TextDiffMessage.Shutdown)
                with _ -> ()

                abandonWorker worker "The text diff pool was disposed." |> ignore

                try
                    do! worker.Transport.Terminate()
                with _ -> ()

            handles.Clear()
            continuations.Clear()
            do! supervisor.Dispose()
    }

    /// The diff service for one owner. Handles opened through it are unknown to every other owner.
    member _.Service(owner: TextDiffOwner) : TextDiffService = {
        Open = fun request context -> openDiff owner request context |> Async.AwaitPromise
        ReadPage =
            fun request context ->
                handleCall owner request.Handle context (fun handle -> RequestBody.ReadPage { request with Handle = handle }) (function
                    | ResultPayload.ReadPage page -> Some page
                    | _ -> None)
                |> Async.AwaitPromise
        ReplayPage =
            fun request context ->
                handleCall owner request.Handle context (fun handle -> RequestBody.ReplayPage { request with Handle = handle }) (function
                    | ResultPayload.ReplayPage page -> Some page
                    | _ -> None)
                |> Async.AwaitPromise
        Expand =
            fun request context ->
                handleCall owner request.Handle context (fun handle -> RequestBody.Expand { request with Handle = handle }) (function
                    | ResultPayload.Expand parts -> Some parts
                    | _ -> None)
                |> Async.AwaitPromise
        ReadLine =
            fun request context ->
                handleCall owner request.Handle context (fun handle -> RequestBody.ReadLine { request with Handle = handle }) (function
                    | ResultPayload.ReadLine line -> Some line
                    | _ -> None)
                |> Async.AwaitPromise
        GetSourceInfo =
            fun request context ->
                handleCall owner request.Handle context (fun handle -> RequestBody.SourceInfo { SourceInfoRequest.Handle = handle }) (function
                    | ResultPayload.SourceInfo(previous, current) -> Some(previous, current)
                    | _ -> None)
                |> Async.AwaitPromise
        Close = fun handle _ -> closeHandle owner handle |> Async.AwaitPromise
    }

let create (options: TextDiffPoolOptions) : TextDiffPool = TextDiffPool(options)
