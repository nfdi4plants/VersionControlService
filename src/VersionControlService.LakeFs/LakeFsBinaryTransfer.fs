module VersionControlService.LakeFs.LakeFsBinaryTransfer

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes

module NodeCancellation = VersionControlService.Runtime.Node.Cancellation
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeBinaryIO = VersionControlService.Runtime.Node.BinaryIO
module LakeFsTransportError = VersionControlService.LakeFs.LakeFsTransportError
module LakeFsRequestOutput = VersionControlService.LakeFs.LakeFsRequestOutput

type StreamCopyResult = NodeBinaryIO.StreamCopyResult

let private fileSystemDynamic: obj = importAll "node:fs"
let private streamPromisesDynamic: obj = importAll "node:stream/promises"
// A default import, not importAll: bundlers that turn `import * as` into a CommonJS namespace
// copy walk the module with for...in and crash on the members node:stream inherits from
// EventEmitter. The default export is the same module object, so nothing else changes.
let private streamDynamic: obj = importDefault "node:stream"

[<Emit("fetch($0, $1)")>]
let private fetchJs (_url: string) (_options: obj) : JS.Promise<obj> = jsNative

[<Emit("Buffer.from($0, 'utf8').toString('base64')")>]
let private toBase64 (_value: string) : string = jsNative

[<Emit("encodeURIComponent($0)")>]
let private encodeUriComponent (_value: string) : string = jsNative

// A Transform that hands every chunk to the callback and passes it on unchanged. Hashing and
// progress used to hang off a "data" listener on the source stream, which switches the stream
// to flowing mode before fetch or pipeline has attached. On Linux the first chunks were gone by
// then, so uploads arrived empty while the byte count still reported the full file.
// A callback that throws (a progress reporter, say) must fail the stream through the
// transform callback. Thrown from inside the stream machinery it would escape the F# catch.
[<Emit("new $0.Transform({ transform(chunk, encoding, callback) { try { $1(chunk); } catch (error) { callback(error); return; } callback(null, chunk); } })")>]
let private createByteTap (_stream: obj) (_onChunk: obj -> unit) : obj = jsNative

let private classifyStatus (connection: LakeFsConnection) status body =
    let category, code =
        match status with
        | 401 -> Authentication, "unauthorized"
        | 403 -> Authorization, "forbidden"
        | 404 -> NotFound, "not_found"
        | 409 -> Conflict, "conflict"
        | 412 -> Concurrency, "precondition_failed"
        | value when value >= 500 -> ProviderError, "server_error"
        | _ -> ProviderError, "request_failed"

    // A server can echo the credentials it received, so the body is cleaned of this connection's secrets too.
    {
        OperationFailure.createRedacted
            category
            code
            (LakeFsRequestOutput.redactText connection $"lakeFS request failed ({status}): {body}") with
            Retryable = status >= 500
    }

let private canceledFailure () =
    OperationFailure.create Canceled "operation_canceled" "The lakeFS object transfer was canceled."

let private networkFailure message =
    {
        OperationFailure.createRedacted
            Network
            "network_failure"
            $"The lakeFS object transfer failed: {message}" with
                Retryable = true
    }

let private transportFailure (error: exn) =
    LakeFsTransportError.toFailure
        "The lakeFS object transfer failed"
        "The lakeFS object transfer timed out"
        (box error)

// Reports a failure on the request's output and hands it back as the result.
let private failWith (output: LakeFsRequestOutput.RequestOutput option) (failure: OperationFailure) =
    LakeFsRequestOutput.reportFailed output failure
    Error failure

let private reportBytes phase item total (context: OperationContext) completed =
    context.ReportProgress {
        PhaseCode = phase
        Item = Some item
        Completed = Some completed
        Total = total
        DisplayMessage = Some "lakeFS object bytes transferred"
    }

// The request targets are relative to /api/v1. The output header shows the same text that the URL ends with.
let private objectTarget repository reference key =
    "/repositories/"
    + encodeUriComponent repository
    + "/refs/"
    + encodeUriComponent reference
    + "/objects?path="
    + encodeUriComponent key

let private uploadTarget repository branch key =
    "/repositories/"
    + encodeUriComponent repository
    + "/branches/"
    + encodeUriComponent branch
    + "/objects?path="
    + encodeUriComponent key

let private objectUrl connection repository reference key =
    connection.Endpoint.TrimEnd '/'
    + "/api/v1"
    + objectTarget repository reference key

let private uploadUrl connection repository branch key =
    connection.Endpoint.TrimEnd '/'
    + "/api/v1"
    + uploadTarget repository branch key

let private headers connection =
    let credentials = toBase64 $"{connection.AccessKeyId}:{connection.SecretAccessKey}"
    createObj [ "Authorization" ==> $"Basic {credentials}" ]

let downloadObjectToFile
    (connection: LakeFsConnection)
    (repository: string)
    (reference: string)
    (key: string)
    (temporaryPath: string)
    (context: OperationContext)
    : Async<Result<StreamCopyResult, OperationFailure>> =
    async {
        let mutable completed = false
        let mutable owned = false
        let mutable source: obj option = None
        let mutable target: obj option = None
        let mutable output: LakeFsRequestOutput.RequestOutput option = None

        try
            if context.Cancellation.IsCancellationRequested() then
                return Error(canceledFailure ())
            else
                let signal = NodeCancellation.toAbortSignal context.Cancellation
                let options = createObj [ "method" ==> "GET"; "headers" ==> headers connection; "signal" ==> signal ]

                context.Cancellation.Register(fun () ->
                    source |> Option.iter (fun value -> value?destroy (NodeInterop.createError "canceled") |> ignore)
                    target |> Option.iter (fun value -> value?destroy (NodeInterop.createError "canceled") |> ignore))

                try
                    output <-
                        LakeFsRequestOutput.tryBegin connection "GET" (objectTarget repository reference key) context

                    let! response =
                        fetchJs (objectUrl connection repository reference key) options
                        |> Async.AwaitPromise

                    let status = unbox<int> response?status
                    LakeFsRequestOutput.reportStatus output status

                    if status < 200 || status >= 300 then
                        let! body = response?text () |> unbox<JS.Promise<string>> |> Async.AwaitPromise
                        LakeFsRequestOutput.reportErrorBody output body
                        return Error(classifyStatus connection status body)
                    else
                        let readable: obj = streamDynamic?Readable?fromWeb (response?body)
                        let writable: obj =
                            fileSystemDynamic?createWriteStream (
                                temporaryPath,
                                createObj [ "flags" ==> "wx"; "mode" ==> 384 ]
                            )

                        source <- Some readable
                        target <- Some writable
                        writable?on ("open", fun (_: obj) -> owned <- true) |> ignore
                        let hash = NodeInterop.createSha256Hash ()
                        let mutable copied = 0.0

                        let total =
                            response?headers?get ("content-length")
                            |> Option.ofObj
                            |> Option.bind (fun value ->
                                match System.Double.TryParse(unbox<string> value) with
                                | true, parsed -> Some parsed
                                | _ -> None)

                        let tap =
                            createByteTap streamDynamic (fun chunk ->
                                NodeInterop.updateHash hash chunk
                                copied <- copied + float (NodeInterop.bufferLength chunk)
                                reportBytes "lakefs-download" key total context copied)

                        let pipeline: JS.Promise<obj> =
                            streamPromisesDynamic?pipeline (readable, tap, writable) |> unbox
                        do! pipeline |> Async.AwaitPromise |> Async.Ignore

                        if context.Cancellation.IsCancellationRequested() then
                            return failWith output (canceledFailure ())
                        else
                            completed <- true
                            LakeFsRequestOutput.reportTransferred output copied

                            return
                                Ok {
                                    BytesCopied = copied
                                    Sha256 = NodeInterop.digestHashHex hash
                                }
                with error ->
                    if context.Cancellation.IsCancellationRequested() then
                        return failWith output (canceledFailure ())
                    else
                        return failWith output (transportFailure error)
        finally
            source |> Option.iter (fun value -> value?destroy () |> ignore)
            target |> Option.iter (fun value -> value?destroy () |> ignore)

            // Only remove a file this call created. With "wx" the open fails on an existing
            // path, and that file belongs to someone else.
            if not completed && owned && NodeFileSystem.existsSync temporaryPath then
                try
                    NodeFileSystem.unlinkSync temporaryPath
                with _ ->
                    ()

            // The command ends when the transfer has settled, not when fetch has answered.
            LakeFsRequestOutput.finish output
    }

let private uploadObjectFromFileCore
    (connection: LakeFsConnection)
    (repository: string)
    (branch: string)
    (key: string)
    (sourcePath: string)
    (validateSource: (NodeFileSystem.Stats -> Result<unit, OperationFailure>) option)
    (context: OperationContext)
    : Async<Result<StreamCopyResult, OperationFailure>> =
    async {
        let mutable source: obj option = None
        let mutable body: obj option = None
        let mutable descriptorToClose: int option = None
        let mutable output: LakeFsRequestOutput.RequestOutput option = None

        try
            if context.Cancellation.IsCancellationRequested() then
                return Error(canceledFailure ())
            else
                let descriptor, openedStats = NodeFileSystem.openReadOnlyNoFollowSync sourcePath
                descriptorToClose <- Some descriptor

                match validateSource |> Option.map (fun validate -> validate openedStats) with
                | Some(Error failure) -> return Error failure
                | None
                | Some(Ok()) ->
                    let readable: obj =
                        fileSystemDynamic?createReadStream (
                            sourcePath,
                            createObj [
                                "fd" ==> descriptor
                                "autoClose" ==> true
                            ]
                        )

                    descriptorToClose <- None
                    source <- Some readable
                    let hash = NodeInterop.createSha256Hash ()
                    let mutable copied = 0.0
                    let total = Some openedStats.size

                    let tap =
                        createByteTap streamDynamic (fun chunk ->
                            NodeInterop.updateHash hash chunk
                            copied <- copied + float (NodeInterop.bufferLength chunk)
                            reportBytes "lakefs-upload" key total context copied)

                    // pipe does not forward errors, so a failed read has to tear down the tap
                    // by hand or fetch would wait on a body that never ends. The tap needs its
                    // own error listener from the start: until fetch attaches a reader, an
                    // error on an unobserved stream is thrown at the process instead.
                    let mutable readFailure: obj option = None
                    tap?on ("error", fun (error: obj) -> readFailure <- Some error) |> ignore
                    readable?on ("error", fun (error: obj) -> tap?destroy (error) |> ignore) |> ignore
                    readable?pipe (tap) |> ignore
                    body <- Some tap

                    context.Cancellation.Register(fun () ->
                        readable?destroy (NodeInterop.createError "canceled") |> ignore
                        tap?destroy (NodeInterop.createError "canceled") |> ignore)

                    let encodedCredentials =
                        toBase64 $"{connection.AccessKeyId}:{connection.SecretAccessKey}"

                    let options =
                        createObj [
                            "method" ==> "POST"
                            "headers" ==>
                                createObj [
                                    "Authorization" ==> $"Basic {encodedCredentials}"
                                    "Content-Type" ==> "application/octet-stream"
                                ]
                            "body" ==> tap
                            "duplex" ==> "half"
                            "signal" ==> NodeCancellation.toAbortSignal context.Cancellation
                        ]

                    try
                        // The request starts here, after the local file passed its checks.
                        output <-
                            LakeFsRequestOutput.tryBegin connection "POST" (uploadTarget repository branch key) context

                        let! response =
                            fetchJs (uploadUrl connection repository branch key) options
                            |> Async.AwaitPromise

                        let status = unbox<int> response?status
                        LakeFsRequestOutput.reportStatus output status

                        if status >= 200 && status < 300 then
                            if context.Cancellation.IsCancellationRequested() then
                                return failWith output (canceledFailure ())
                            else
                                match readFailure with
                                | Some error -> return failWith output (networkFailure (NodeInterop.errorMessage error))
                                | None ->
                                    // The counter and the hash run ahead of the socket, so a
                                    // success answer only counts once the body really ended.
                                    if not (unbox<bool> tap?readableEnded) then
                                        return
                                            failWith
                                                output
                                                (networkFailure "lakeFS answered before the upload body was fully sent.")
                                    else
                                        LakeFsRequestOutput.reportTransferred output copied

                                        return
                                            Ok {
                                                BytesCopied = copied
                                                Sha256 = NodeInterop.digestHashHex hash
                                            }
                        else
                            let! body = response?text () |> unbox<JS.Promise<string>> |> Async.AwaitPromise
                            LakeFsRequestOutput.reportErrorBody output body
                            return Error(classifyStatus connection status body)
                    with error ->
                        if context.Cancellation.IsCancellationRequested() then
                            return failWith output (canceledFailure ())
                        else
                            return failWith output (transportFailure error)
        finally
            source |> Option.iter (fun value -> value?destroy () |> ignore)
            body |> Option.iter (fun value -> value?destroy () |> ignore)
            descriptorToClose |> Option.iter NodeFileSystem.closeFileDescriptorSync

            // The command ends when the upload body is verified or the request failed, not when fetch has answered.
            LakeFsRequestOutput.finish output
    }

let uploadObjectFromFile
    (connection: LakeFsConnection)
    (repository: string)
    (branch: string)
    (key: string)
    (sourcePath: string)
    (context: OperationContext)
    =
    uploadObjectFromFileCore connection repository branch key sourcePath None context

let uploadObjectFromFileChecked
    (connection: LakeFsConnection)
    (repository: string)
    (branch: string)
    (key: string)
    (sourcePath: string)
    (validateSource: NodeFileSystem.Stats -> Result<unit, OperationFailure>)
    (context: OperationContext)
    =
    uploadObjectFromFileCore
        connection
        repository
        branch
        key
        sourcePath
        (Some validateSource)
        context
