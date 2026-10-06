/// Worker-side message loop of a text diff worker. Control messages take effect as they arrive.
/// Requests run one at a time in arrival order through a pluggable request handler.
module VersionControlService.Git.TextDiff.TextDiffWorkerDispatcher

open System
open System.Collections.Generic
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffProtocol

module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads
module Supervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor

/// What a request handler may use while it serves one request.
type WorkerHost
    internal
    (
        workerId: string,
        tempDirectory: string,
        requestId: string,
        generation: int,
        isCanceled: unit -> bool,
        call: (int -> TextDiffMessage) -> JS.Promise<SpawnOutcome>,
        send: TextDiffMessage -> unit
    ) =

    let owner: Supervisor.ChildOwner = {
        WorkerId = workerId
        SessionId = string generation
        RequestId = requestId
    }

    member _.WorkerId = workerId

    /// The worker's own temp directory. Spool files must be created inside it.
    member _.TempDirectory = tempDirectory

    member _.RequestId = requestId

    /// The session generation the pool assigned. Git children of this session are tracked under it.
    member _.Generation = generation

    member _.Owner = owner

    /// True once the pool asked to cancel this request.
    member _.IsCanceled() = isCanceled ()

    /// Resolves on a later event-loop turn so control messages such as cancel can arrive.
    member _.Yield() : JS.Promise<unit> =
        Promise.create (fun resolve _ -> NodeWorkerThreads.setImmediate (fun () -> resolve ()))

    /// Runs a short read-only Git command through the main-process supervisor. The output limit applies to the
    /// supervisor only for `cat-file blob <oid>`, which may ask for up to 16 MiB. Other commands keep 64 KiB.
    member _.SpawnShort(cwd: string, arguments: string[], ?outputLimit: int) : JS.Promise<Supervisor.ShortResult> =
        call (fun callId -> TextDiffMessage.SpawnShort(callId, owner, cwd, arguments, outputLimit))
        |> Promise.map (fun outcome ->
            match outcome with
            | SpawnOutcome.Short result -> result
            | SpawnOutcome.Blob _ -> raise (InvalidOperationException("The supervisor answered a short command with a blob result."))
            | SpawnOutcome.Failed message -> raise (InvalidOperationException(message)))

    /// Streams a blob into a spool file and resolves with the exit of the Git child once it has ended.
    member _.SpawnBlob(cwd: string, oid: string, spoolPath: string) : JS.Promise<NodeProcess.ChildExit> =
        call (fun callId -> TextDiffMessage.SpawnBlob(callId, owner, cwd, oid, spoolPath))
        |> Promise.map (fun outcome ->
            match outcome with
            | SpawnOutcome.Blob exit -> exit
            | SpawnOutcome.Short _ -> raise (InvalidOperationException("The supervisor answered a blob command with a short result."))
            | SpawnOutcome.Failed message -> raise (InvalidOperationException(message)))

    member _.ReportProgress(validatedBytes: int64, totalBytes: int64) =
        send (TextDiffMessage.Progress(requestId, generation, validatedBytes, totalBytes))

    /// Asks the supervisor to stop the Git children of this request and delete their spools.
    member _.ReleaseRequest() = send (TextDiffMessage.ReleaseRequest owner)

    /// Asks the supervisor to stop the Git children of this session and delete their spools.
    member _.ReleaseSession() =
        send (TextDiffMessage.ReleaseSession(workerId, owner.SessionId))

    member _.ReportSessionExpired() =
        send (TextDiffMessage.SessionExpired generation)

/// Serves the diff calls inside a worker. Cancel is called for the running request when the pool cancels it.
type ITextDiffRequestHandler =
    abstract member Open: host: WorkerHost * request: OpenDiffRequest * owner: TextDiffOwner -> JS.Promise<Result<Resumable<OpenDiffResult>, OperationFailure>>
    abstract member ReadPage: host: WorkerHost * request: ReadPageRequest -> JS.Promise<Result<Resumable<DiffPage>, OperationFailure>>
    abstract member ReplayPage: host: WorkerHost * request: ReplayPageRequest -> JS.Promise<Result<DiffPage, OperationFailure>>
    abstract member Expand: host: WorkerHost * request: ExpandRequest -> JS.Promise<Result<Resumable<DiffPart[]>, OperationFailure>>
    abstract member ReadLine: host: WorkerHost * request: ReadLineRequest -> JS.Promise<Result<Resumable<DiffLine>, OperationFailure>>
    abstract member GetSourceInfo: host: WorkerHost * request: SourceInfoRequest -> JS.Promise<Result<DiffSourceInfo * DiffSourceInfo, OperationFailure>>
    abstract member Close: host: WorkerHost * handle: DiffHandle option -> JS.Promise<unit>
    abstract member Cancel: requestId: string -> unit

let canceledFailure () =
    OperationFailure.create Canceled "operation_canceled" "The text diff request was canceled."

let private workerFailure (message: string) =
    OperationFailure.create ProviderError TextDiffFailureCodes.WorkerFailed message

type private QueuedRequest = {
    RequestId: string
    Generation: int
    Body: RequestBody
    mutable Canceled: bool
}

type TextDiffWorkerDispatcher(post: obj -> unit, close: unit -> unit, handler: ITextDiffRequestHandler) =
    let queue = ResizeArray<QueuedRequest>()
    let pendingCalls = Dictionary<int, SpawnOutcome -> unit>()
    let mutable running: QueuedRequest option = None
    let mutable workerId = ""
    let mutable tempDirectory = ""
    let mutable stopped = false
    let mutable nextCallId = 0

    let send message =
        if not stopped then
            post (encode message)

    let call (build: int -> TextDiffMessage) : JS.Promise<SpawnOutcome> =
        Promise.create (fun resolve _ ->
            let callId = nextCallId
            nextCallId <- nextCallId + 1
            pendingCalls[callId] <- resolve
            send (build callId))

    let mapResult (payload: 'T -> ResultPayload) (work: JS.Promise<Result<'T, OperationFailure>>) =
        work |> Promise.map (Result.map payload)

    let runHandler (host: WorkerHost) (body: RequestBody) : JS.Promise<Result<ResultPayload, OperationFailure>> =
        try
            match body with
            | RequestBody.Open(request, owner) -> handler.Open(host, request, owner) |> mapResult ResultPayload.Open
            | RequestBody.ReadPage request -> handler.ReadPage(host, request) |> mapResult ResultPayload.ReadPage
            | RequestBody.ReplayPage request -> handler.ReplayPage(host, request) |> mapResult ResultPayload.ReplayPage
            | RequestBody.Expand request -> handler.Expand(host, request) |> mapResult ResultPayload.Expand
            | RequestBody.ReadLine request -> handler.ReadLine(host, request) |> mapResult ResultPayload.ReadLine
            | RequestBody.SourceInfo request -> handler.GetSourceInfo(host, request) |> mapResult ResultPayload.SourceInfo
            | RequestBody.Close handle -> handler.Close(host, handle) |> Promise.map (fun () -> Ok ResultPayload.Close)
        with error ->
            Promise.reject error

    let rec pump () =
        if not stopped && running.IsNone && queue.Count > 0 then
            let request = queue[0]
            queue.RemoveAt 0
            running <- Some request

            let host =
                WorkerHost(workerId, tempDirectory, request.RequestId, request.Generation, (fun () -> request.Canceled), call, send)

            let finish (outcome: Result<ResultPayload, OperationFailure>) =
                match outcome with
                | Ok payload -> send (TextDiffMessage.Result(request.RequestId, request.Generation, payload))
                | Error failure -> send (TextDiffMessage.Error(request.RequestId, request.Generation, failure))

                running <- None
                pump ()

            NodeInterop.observePromise (runHandler host request.Body) finish (fun error ->
                match request.Body with
                | RequestBody.Close _ -> finish (Ok ResultPayload.Close)
                | _ -> finish (Error(workerFailure (NodeInterop.errorMessage error))))

    let cancel requestId generation =
        match running with
        | Some request when request.RequestId = requestId && request.Generation = generation && not request.Canceled ->
            request.Canceled <- true

            try
                handler.Cancel requestId
            with _ -> ()
        | _ -> ()

    let receive (message: TextDiffMessage) =
        match message with
        | TextDiffMessage.Init(id, epoch, directory) ->
            workerId <- id
            tempDirectory <- directory
            send (TextDiffMessage.InitAck(id, epoch))
        | TextDiffMessage.Shutdown ->
            queue.Clear()
            stopped <- true
            close ()
        | TextDiffMessage.Cancel(requestId, generation) -> cancel requestId generation
        | TextDiffMessage.SpawnResult(callId, outcome) ->
            match pendingCalls.TryGetValue callId with
            | true, resolve ->
                pendingCalls.Remove callId |> ignore
                resolve outcome
            | _ -> ()
        | TextDiffMessage.Request(requestId, generation, body) ->
            queue.Add {
                RequestId = requestId
                Generation = generation
                Body = body
                Canceled = false
            }

            pump ()
        | _ -> ()

    /// Handles one raw message from the pool.
    member _.Receive(message: obj) =
        if not stopped then
            match decodeMessage message with
            | Ok decoded -> receive decoded
            | Error reason -> send (TextDiffMessage.WorkerFailure reason)

/// Creates a dispatcher that posts through `post` and returns the function that receives messages for it.
let attachDispatcher (post: obj -> unit, close: unit -> unit, handler: ITextDiffRequestHandler) : obj -> unit =
    let dispatcher = TextDiffWorkerDispatcher(post, close, handler)
    fun message -> dispatcher.Receive message
