module VersionControlService.Git.TextDiff.TextDiffTransport

open Fable.Core

module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads

/// The main-process end of one text diff worker.
type ITextDiffWorkerTransport =
    abstract member Post: obj -> unit
    abstract member OnMessage: (obj -> unit) -> unit
    abstract member OnError: (string -> unit) -> unit
    abstract member OnExit: (int -> unit) -> unit
    abstract member Terminate: unit -> JS.Promise<unit>

/// Creates the transport for the worker with the given index. It may throw when the worker cannot start.
type TextDiffWorkerFactory = int -> ITextDiffWorkerTransport

[<Emit("$0?.message ?? String($0)")>]
let private errorText (_error: obj) : string = jsNative

type private WorkerThreadTransport(path: string) =
    let worker = NodeWorkerThreads.Worker(path, NodeWorkerThreads.WorkerOptions(eval = false))

    interface ITextDiffWorkerTransport with
        member _.Post message = worker.postMessage message
        member _.OnMessage handler = worker.onMessage handler
        member _.OnError handler = worker.onError (fun error -> handler (errorText error))
        member _.OnExit handler = worker.onExit handler
        member _.Terminate() = worker.terminate () |> Promise.map ignore

module WorkerThreadTransport =

    /// Starts a worker_threads Worker from an absolute file path.
    let create (path: string) : ITextDiffWorkerTransport = WorkerThreadTransport(path)
