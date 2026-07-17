module VersionControlService.LakeFs.LakeFsBinaryTransfer

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes

module NodeCancellation = VersionControlService.Runtime.Node.Cancellation
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeBinaryIO = VersionControlService.Runtime.Node.BinaryIO

type StreamCopyResult = NodeBinaryIO.StreamCopyResult

let private fileSystemDynamic: obj = importAll "node:fs"
let private streamPromisesDynamic: obj = importAll "node:stream/promises"
let private streamDynamic: obj = importAll "node:stream"

[<Emit("fetch($0, $1)")>]
let private fetchJs (_url: string) (_options: obj) : JS.Promise<obj> = jsNative

[<Emit("Buffer.from($0, 'utf8').toString('base64')")>]
let private toBase64 (_value: string) : string = jsNative

[<Emit("encodeURIComponent($0)")>]
let private encodeUriComponent (_value: string) : string = jsNative

let private classifyStatus status body =
    let category, code =
        match status with
        | 401 -> Authentication, "unauthorized"
        | 403 -> Authorization, "forbidden"
        | 404 -> NotFound, "not_found"
        | 409 -> Conflict, "conflict"
        | 412 -> Concurrency, "precondition_failed"
        | value when value >= 500 -> ProviderError, "server_error"
        | _ -> ProviderError, "request_failed"

    {
        OperationFailure.createRedacted category code $"lakeFS request failed ({status}): {body}" with
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

let private reportBytes phase item total (context: OperationContext) completed =
    context.ReportProgress {
        PhaseCode = phase
        Item = Some item
        Completed = Some completed
        Total = total
        DisplayMessage = Some "lakeFS object bytes transferred"
    }

let private objectUrl connection repository reference key =
    connection.Endpoint.TrimEnd '/'
    + "/api/v1/repositories/"
    + encodeUriComponent repository
    + "/refs/"
    + encodeUriComponent reference
    + "/objects?path="
    + encodeUriComponent key

let private uploadUrl connection repository branch key =
    connection.Endpoint.TrimEnd '/'
    + "/api/v1/repositories/"
    + encodeUriComponent repository
    + "/branches/"
    + encodeUriComponent branch
    + "/objects?path="
    + encodeUriComponent key

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
        let mutable source: obj option = None
        let mutable target: obj option = None

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
                    let! response =
                        fetchJs (objectUrl connection repository reference key) options
                        |> Async.AwaitPromise

                    let status = unbox<int> response?status

                    if status < 200 || status >= 300 then
                        let! body = response?text () |> unbox<JS.Promise<string>> |> Async.AwaitPromise
                        return Error(classifyStatus status body)
                    else
                        let readable: obj = streamDynamic?Readable?fromWeb (response?body)
                        let writable: obj =
                            fileSystemDynamic?createWriteStream (
                                temporaryPath,
                                createObj [ "flags" ==> "wx"; "mode" ==> 384 ]
                            )

                        source <- Some readable
                        target <- Some writable
                        let hash = NodeInterop.createSha256Hash ()
                        let mutable copied = 0.0

                        let total =
                            response?headers?get ("content-length")
                            |> Option.ofObj
                            |> Option.bind (fun value ->
                                match System.Double.TryParse(unbox<string> value) with
                                | true, parsed -> Some parsed
                                | _ -> None)

                        readable?on (
                            "data",
                            fun (chunk: obj) ->
                                NodeInterop.updateHash hash chunk
                                copied <- copied + float (NodeInterop.bufferLength chunk)
                                reportBytes "lakefs-download" key total context copied
                        )
                        |> ignore

                        let pipeline: JS.Promise<obj> = streamPromisesDynamic?pipeline (readable, writable) |> unbox
                        do! pipeline |> Async.AwaitPromise |> Async.Ignore

                        if context.Cancellation.IsCancellationRequested() then
                            return Error(canceledFailure ())
                        else
                            completed <- true
                            return
                                Ok {
                                    BytesCopied = copied
                                    Sha256 = NodeInterop.digestHashHex hash
                                }
                with error ->
                    if context.Cancellation.IsCancellationRequested() then
                        return Error(canceledFailure ())
                    else
                        return Error(networkFailure (NodeInterop.errorMessage error))
        finally
            source |> Option.iter (fun value -> value?destroy () |> ignore)
            target |> Option.iter (fun value -> value?destroy () |> ignore)

            if not completed && NodeFileSystem.existsSync temporaryPath then
                try
                    NodeFileSystem.unlinkSync temporaryPath
                with _ ->
                    ()
    }

let uploadObjectFromFile
    (connection: LakeFsConnection)
    (repository: string)
    (branch: string)
    (key: string)
    (sourcePath: string)
    (context: OperationContext)
    : Async<Result<StreamCopyResult, OperationFailure>> =
    async {
        let mutable source: obj option = None

        try
            if context.Cancellation.IsCancellationRequested() then
                return Error(canceledFailure ())
            else
                let readable: obj = fileSystemDynamic?createReadStream sourcePath
                source <- Some readable
                let hash = NodeInterop.createSha256Hash ()
                let mutable copied = 0.0
                let total = Some((NodeFileSystem.statSync sourcePath).size)

                readable?on (
                    "data",
                    fun (chunk: obj) ->
                        NodeInterop.updateHash hash chunk
                        copied <- copied + float (NodeInterop.bufferLength chunk)
                        reportBytes "lakefs-upload" key total context copied
                )
                |> ignore

                context.Cancellation.Register(fun () ->
                    readable?destroy (NodeInterop.createError "canceled") |> ignore)

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
                        "body" ==> readable
                        "duplex" ==> "half"
                        "signal" ==> NodeCancellation.toAbortSignal context.Cancellation
                    ]

                try
                    let! response =
                        fetchJs (uploadUrl connection repository branch key) options
                        |> Async.AwaitPromise

                    let status = unbox<int> response?status

                    if status >= 200 && status < 300 then
                        if context.Cancellation.IsCancellationRequested() then
                            return Error(canceledFailure ())
                        else
                            return
                                Ok {
                                    BytesCopied = copied
                                    Sha256 = NodeInterop.digestHashHex hash
                                }
                    else
                        let! body = response?text () |> unbox<JS.Promise<string>> |> Async.AwaitPromise
                        return Error(classifyStatus status body)
                with error ->
                    if context.Cancellation.IsCancellationRequested() then
                        return Error(canceledFailure ())
                    else
                        return Error(networkFailure (NodeInterop.errorMessage error))
        finally
            source |> Option.iter (fun value -> value?destroy () |> ignore)
    }
