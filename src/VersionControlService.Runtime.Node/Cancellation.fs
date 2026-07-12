/// Bridges the portable OperationCancellation contract to Node primitives.
module VersionControlService.Runtime.Node.Cancellation

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions

[<Emit("new AbortController()")>]
let private createAbortController () : obj = jsNative

/// Creates a Node AbortSignal that fires when the portable cancellation is requested.
/// Useful for fetch/HTTP clients and other AbortSignal-aware APIs.
let toAbortSignal (cancellation: OperationCancellation) : obj =
    let controller = createAbortController ()

    if cancellation.IsCancellationRequested() then
        controller?abort ()
    else
        cancellation.Register(fun () -> controller?abort ())

    controller?signal
