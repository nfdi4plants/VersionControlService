/// The lakeFS side of the output channel. Every HTTP request of the provider is one command. The header shows the
/// method and the request target relative to /api/v1, and a notice carries the status and the duration. A failed
/// request adds its error body or its reason as stderr lines. Object bodies and successful JSON bodies are never
/// reported. All of this runs only for a context whose host set an output callback.
module VersionControlService.LakeFs.LakeFsRequestOutput

open System
open System.Text.RegularExpressions
open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes

module NodeOutput = VersionControlService.Runtime.Node.OutputReporter

[<Emit("Buffer.from($0, 'utf8').toString('base64')")>]
let private toBase64 (_value: string) : string = jsNative

/// The credential values of a connection: the access key id, the secret, "id:secret" and the Basic payload of the
/// Authorization header, each with its decoded and percent-encoded forms. The channel leaves out a value shorter
/// than four characters, so a credential that short is not replaced.
let connectionSecrets (connection: LakeFsConnection) : string[] =
    let pair = $"{connection.AccessKeyId}:{connection.SecretAccessKey}"

    NodeOutput.knownSecrets [|
        connection.AccessKeyId
        connection.SecretAccessKey
        pair
        toBase64 pair
    |]

let private sensitiveQueryPattern =
    Regex(
        @"([?&][^=&\s""']*(?:signature|token|secret|credential|passw|auth|key|sig)[^=&\s""']*=)[^&\s""']*",
        RegexOptions.IgnoreCase
    )

/// Replaces the value of a query parameter whose name looks like a credential, such as a signature or a token.
/// Plain API parameters (path, prefix, after, amount) stay. The provider sends no presigned URLs today, so this
/// keeps one that a server echoes in an error body out of the output and the failure text.
let redactQueryValues (text: string) : string =
    if isNull text then
        text
    else
        sensitiveQueryPattern.Replace(text, "$1[REDACTED]")

/// Replaces the secrets of the connection and the sensitive query values, then applies the structural redaction.
let redactText (connection: LakeFsConnection) (text: string) : string =
    NodeOutput.redactText (connectionSecrets connection) (redactQueryValues text)

/// The output of one request. It exists only when the host asked for output.
[<AllowNullLiteral>]
type RequestOutput(command: NodeOutput.CommandOutput, clock: NodeOutput.OutputClock) =
    let startedAt = clock.Now()
    let mutable errorBodyReported = false

    let elapsed () =
        int (Math.Round(clock.Now() - startedAt))

    member _.Status(status: int) =
        command.ObserveNotice $"status {status} after {elapsed ()} ms"

    member _.ErrorBody(body: string) =
        errorBodyReported <- true
        command.ObserveStderr(redactQueryValues body)

    member _.Transferred(bytes: float) =
        command.ObserveNotice $"transferred {bytes} bytes in {elapsed ()} ms"

    /// A cancellation gets a notice. Any other failure gets a notice and its reason, unless the error body of
    /// the response already told it.
    member _.Failed(failure: OperationFailure) =
        if failure.Category = Canceled then
            command.ObserveNotice $"canceled after {elapsed ()} ms"
        elif not errorBodyReported then
            command.ObserveNotice $"failed after {elapsed ()} ms"
            command.ObserveStderr(redactQueryValues failure.Message)

    member _.Finish() = command.Finish()

/// Opens the command of a request that is about to be sent, or returns None when the context has no output
/// callback. The caller checks for a cancellation first, so a request that never starts creates no command.
/// `target` is the path and query relative to /api/v1.
let tryBegin
    (connection: LakeFsConnection)
    (method: string)
    (target: string)
    (context: OperationContext)
    : RequestOutput option =
    match NodeOutput.tryOf context with
    | None -> None
    | Some channel ->
        let command =
            NodeOutput.beginCommandWithSecrets
                channel
                "lakefs"
                [| method; redactQueryValues target |]
                (connectionSecrets connection)
                false

        Some(RequestOutput(command, channel.Clock))

let reportStatus (output: RequestOutput option) (status: int) =
    match output with
    | Some value -> value.Status status
    | None -> ()

let reportErrorBody (output: RequestOutput option) (body: string) =
    match output with
    | Some value -> value.ErrorBody body
    | None -> ()

let reportTransferred (output: RequestOutput option) (bytes: float) =
    match output with
    | Some value -> value.Transferred bytes
    | None -> ()

let reportFailed (output: RequestOutput option) (failure: OperationFailure) =
    match output with
    | Some value -> value.Failed failure
    | None -> ()

/// Ends the command and delivers what waits in the channel. Callers run it in a finally, so a failure and a
/// cancellation finish the command too.
let finish (output: RequestOutput option) =
    match output with
    | Some value -> value.Finish()
    | None -> ()

/// The context of follow-up work that has to run after the caller canceled, such as a recovery or a cleanup.
/// The cancellation never fires, and the output callback of the original stays, so the requests of the
/// follow-up reach the same log.
let withoutCancellation (context: OperationContext) (suffix: string) : OperationContext = {
    context with
        OperationId = $"{context.OperationId}-{suffix}"
        Cancellation = OperationCancellation.none
}
