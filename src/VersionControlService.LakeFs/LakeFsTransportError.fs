/// Classifies the errors that Node's fetch client (undici) raises while it talks to lakeFS.
module VersionControlService.LakeFs.LakeFsTransportError

open Fable.Core
open VersionControlService.Abstractions

module NodeInterop = VersionControlService.Runtime.Node.Interop

/// undici reports its timeouts with UND_ERR_*_TIMEOUT codes, and the operating system reports a
/// dead TCP connection as ETIMEDOUT. The abort signal of a request comes from the caller's
/// cancellation and aborts without a timeout reason, so no cancellation carries one of these codes.
let private timeoutCodes = [|
    "UND_ERR_CONNECT_TIMEOUT"
    "UND_ERR_HEADERS_TIMEOUT"
    "UND_ERR_BODY_TIMEOUT"
    "ETIMEDOUT"
|]

/// fetch hides the transport error one level down: a TypeError with the message "fetch failed"
/// for a request, or "terminated" for a body that breaks while it is read. A connect to several
/// addresses adds an AggregateError level that carries the code of its first error.
[<Emit("(() => { let current = $0; for (let depth = 0; depth < 4 && current != null && typeof current === 'object'; depth++) { if ($1.includes(current.code)) { return current; } current = current.cause; } return null; })()")>]
let private findErrorWithCode (_error: obj) (_codes: string[]) : obj = jsNative

/// The message of the timeout error behind a failed request, or None when the error is no timeout.
let tryTimeoutDetail (error: obj) : string option =
    findErrorWithCode error timeoutCodes
    |> Option.ofObj
    |> Option.map NodeInterop.errorMessage

/// The failure for a transport error that is not a cancellation. A timeout gets the category and
/// code that the Git provider uses. Both outcomes are retryable.
let toFailure (networkPrefix: string) (timeoutPrefix: string) (error: obj) : OperationFailure =
    match tryTimeoutDetail error with
    | Some detail -> {
        OperationFailure.createRedacted Timeout "timeout" $"{timeoutPrefix}: {detail}" with
            Retryable = true
      }
    | None -> {
        OperationFailure.createRedacted Network "network_failure" $"{networkPrefix}: {NodeInterop.errorMessage error}" with
            Retryable = true
      }
