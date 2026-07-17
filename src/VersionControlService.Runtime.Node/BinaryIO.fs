module VersionControlService.Runtime.Node.BinaryIO

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodePath = VersionControlService.Runtime.Node.Path

let private streamPromisesDynamic: obj = importAll "node:stream/promises"

type StreamCopyResult = {
    BytesCopied: float
    Sha256: string
}

let private canceledMessage = "The binary operation was canceled."

/// Copies bytes to a caller-owned temporary path while calculating SHA-256.
/// Any failure or cancellation removes the incomplete target.
let copyFileWithHash
    (sourcePath: string)
    (temporaryTargetPath: string)
    (cancellation: OperationCancellation)
    (report: float -> unit)
    : Async<Result<StreamCopyResult, string>> =
    Async.FromContinuations(fun (resolve, _, _) ->
        if cancellation.IsCancellationRequested() then
            NodeFileSystem.removeFileIfExistsSync temporaryTargetPath
            resolve (Error canceledMessage)
        else
            let mutable finished = false
            let mutable canceled = false
            let mutable bytesCopied = 0.0
            let hash = NodeInterop.createSha256Hash ()

            let complete result =
                if not finished then
                    finished <- true

                    match result with
                    | Error _ -> NodeFileSystem.removeFileIfExistsSync temporaryTargetPath
                    | Ok _ -> ()

                    resolve result

            try
                let source = NodeFileSystem.createReadStream sourcePath
                let target = NodeFileSystem.createWriteStreamExclusive temporaryTargetPath

                let abort message =
                    let error = NodeInterop.createError message
                    source?destroy (error) |> ignore
                    target?destroy (error) |> ignore

                source?on (
                    "data",
                    fun (chunk: obj) ->
                        if not finished then
                            try
                                NodeInterop.updateHash hash chunk
                                bytesCopied <- bytesCopied + float (NodeInterop.bufferLength chunk)
                                report bytesCopied
                            with error ->
                                abort (NodeInterop.errorMessage error)
                )
                |> ignore

                cancellation.Register(fun () ->
                    if not finished then
                        canceled <- true
                        abort canceledMessage)

                let pipeline: JS.Promise<obj> = streamPromisesDynamic?pipeline (source, target) |> unbox

                NodeInterop.observePromise
                    pipeline
                    (fun _ ->
                        if canceled || cancellation.IsCancellationRequested() then
                            complete (Error canceledMessage)
                        else
                            complete (
                                Ok {
                                    BytesCopied = bytesCopied
                                    Sha256 = NodeInterop.digestHashHex hash
                                }
                            ))
                    (fun error ->
                        let message =
                            if canceled || cancellation.IsCancellationRequested() then
                                canceledMessage
                            else
                                NodeInterop.errorMessage error

                        complete (Error message))
            with error ->
                complete (Error(NodeInterop.errorMessage error)))

/// Writes a raw buffer through an exclusive sibling temporary file and atomically
/// publishes it only after the bytes have been flushed.
let writeBufferAtomic
    (targetPath: string)
    (buffer: obj)
    (cancellation: OperationCancellation)
    : Async<Result<unit, string>> =
    async {
        let temporaryPath =
            NodePath.join [|
                NodePath.dirname targetPath
                $".{NodePath.basename targetPath}.{NodeInterop.randomUuid()}.tmp"
            |]

        if cancellation.IsCancellationRequested() then
            return Error canceledMessage
        else
            let mutable handle: NodeFileSystem.FileHandle option = None
            let mutable published = false

            try
                try
                    let! opened = NodeFileSystem.openWriteExclusiveAsync temporaryPath |> Async.AwaitPromise
                    handle <- Some opened
                    do! opened.writeFile buffer |> Async.AwaitPromise
                    do! opened.sync () |> Async.AwaitPromise
                    do! opened.close () |> Async.AwaitPromise
                    handle <- None

                    if cancellation.IsCancellationRequested() then
                        return Error canceledMessage
                    else
                        do! NodeFileSystem.renameAsync temporaryPath targetPath |> Async.AwaitPromise
                        published <- true
                        return Ok()
                with error ->
                    return Error(NodeInterop.errorMessage error)
            finally
                match handle with
                | Some opened ->
                    NodeInterop.observePromise (opened.close ()) ignore ignore
                | None -> ()

                if not published then
                    NodeFileSystem.removeFileIfExistsSync temporaryPath
    }
