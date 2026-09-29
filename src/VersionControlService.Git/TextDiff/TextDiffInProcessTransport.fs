module VersionControlService.Git.TextDiff.TextDiffInProcessTransport

open Fable.Core
open VersionControlService.Git.TextDiff.TextDiffTransport
open VersionControlService.Git.TextDiff.TextDiffWorkerDispatcher

module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads

[<Emit("JSON.parse(JSON.stringify($0))")>]
let private cloneMessage (_message: obj) : obj = jsNative

type private InProcessTransport(handler: ITextDiffRequestHandler) =
    let mutable active = true
    let mutable hostReceiver: (obj -> unit) option = None
    let mutable exitReceiver: (int -> unit) option = None

    let deliver (receiver: unit -> (obj -> unit) option) (message: obj) =
        let copy = cloneMessage message

        NodeWorkerThreads.setImmediate (fun () ->
            if active then
                receiver () |> Option.iter (fun callback -> callback copy))

    let stop code =
        if active then
            active <- false
            exitReceiver |> Option.iter (fun callback -> NodeWorkerThreads.setImmediate (fun () -> callback code))

    let workerReceiver =
        attachDispatcher ((fun message -> deliver (fun () -> hostReceiver) message), (fun () -> stop 0), handler)

    interface ITextDiffWorkerTransport with
        member _.Post message = deliver (fun () -> Some workerReceiver) message
        member _.OnMessage callback = hostReceiver <- Some callback
        member _.OnError _ = ()
        member _.OnExit callback = exitReceiver <- Some callback

        member _.Terminate() =
            stop 1
            Promise.lift ()

/// Runs a dispatcher with the given request handler on the calling thread. Messages in both directions
/// are copied through JSON and delivered on a later event-loop turn. This transport exists for tests only.
module InProcessTransport =
    let create (handler: ITextDiffRequestHandler) : ITextDiffWorkerTransport = InProcessTransport(handler)
