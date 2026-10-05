module VersionControlService.Runtime.Node.WorkerThreads

open Fable.Core

[<JS.PojoAttribute>]
type WorkerOptions(?eval: bool, ?workerData: obj) =
    member val eval: bool option = eval with get, set
    member val workerData: obj option = workerData with get, set

[<AllowNullLiteral>]
type MessagePort =
    abstract member postMessage: message: obj -> unit
    abstract member close: unit -> unit

    [<Emit("$0.on('message', $1)")>]
    abstract member onMessage: handler: (obj -> unit) -> unit

[<Import("Worker", "node:worker_threads")>]
type Worker(absolutePath: string, options: WorkerOptions) =
    member _.postMessage(message: obj) : unit = jsNative
    member _.terminate() : JS.Promise<int> = jsNative

    [<Emit("$0.on('message', $1)")>]
    member _.onMessage(handler: obj -> unit) : unit = jsNative

    [<Emit("$0.on('error', $1)")>]
    member _.onError(handler: obj -> unit) : unit = jsNative

    [<Emit("$0.on('exit', $1)")>]
    member _.onExit(handler: int -> unit) : unit = jsNative

[<Import("parentPort", "node:worker_threads")>]
let parentPort: MessagePort option = jsNative

[<Import("setImmediate", "node:timers")>]
let setImmediate (callback: unit -> unit) : unit = jsNative

[<Import("tmpdir", "node:os")>]
let tmpdir () : string = jsNative
