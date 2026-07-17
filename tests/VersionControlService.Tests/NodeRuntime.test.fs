module VersionControlService.Tests.NodeRuntimeTests

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open Vitest

module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeBinaryIO = VersionControlService.Runtime.Node.BinaryIO
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path

let private run (operation: Async<'T>) : JS.Promise<'T> = Async.StartAsPromise operation

let private nodeExecutable: string = Fable.Core.JsInterop.emitJsExpr () "process.execPath"

let private fsPromisesDynamic: obj = importAll "node:fs/promises"
let private osDynamic: obj = importAll "node:os"

[<Emit("Buffer.from($0)")>]
let private bufferFromBytes (_bytes: int[]) : obj = jsNative

[<Emit("Buffer.alloc($0, $1)")>]
let private filledBuffer (_length: int) (_value: int) : obj = jsNative

[<Emit("$0.toString('base64')")>]
let private bufferBase64 (_buffer: obj) : string = jsNative

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = NodePath.join [| osDynamic?tmpdir () |> unbox<string>; "vcs-node-runtime-" |]
    fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>

let private removeDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

Vitest.describe (
    "Node runtime process adapter",
    fun () ->
        Vitest.test (
            "runs a child process to completion and captures output",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let request =
                    NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; "console.log('hello runtime')" |]

                let! result = run (NodeProcess.run request (OperationContext.detached "runtime-run"))

                match result with
                | Succeeded outcome ->
                    Vitest.expect(outcome.Value.ExitCode).toBe (0)
                    Vitest.expect(outcome.Value.StdOut.Contains "hello runtime").toBe (true)
                | PartiallySucceeded _
                | Failed _ -> failwith "Expected the process to succeed."
            }
        )

        Vitest.test (
            "forwards stdin data to the child process",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let script =
                    "let d='';process.stdin.on('data',c=>d+=c);process.stdin.on('end',()=>console.log('got:'+d))"

                let request = {
                    NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |] with
                        StdinData = Some "a\000b\000c"
                }

                let! result = run (NodeProcess.run request (OperationContext.detached "runtime-stdin"))

                match result with
                | Succeeded outcome -> Vitest.expect(outcome.Value.StdOut.Contains "got:a\000b\000c").toBe (true)
                | PartiallySucceeded _
                | Failed _ -> failwith "Expected the stdin process to succeed."
            }
        )

        Vitest.test (
            "captures stdout bytes before decoding split UTF-8 chunks",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let script =
                    "const b=Buffer.from('prefix € suffix\\n','utf8');" +
                    "process.stdout.write(b.subarray(0,8));" +
                    "setTimeout(()=>process.stdout.write(b.subarray(8)),25);"

                let request = NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |]

                let! result =
                    run (NodeProcess.runBytes request (OperationContext.detached "runtime-byte-output"))

                match result with
                | Succeeded outcome ->
                    Vitest.expect(outcome.Value.ExitCode).toBe (0)
                    Vitest.expect(NodeInterop.bufferToUtf8String outcome.Value.StdOut).toBe ("prefix € suffix\n")
                | PartiallySucceeded _
                | Failed _ -> failwith "Expected byte-oriented process output."
            }
        )

        Vitest.test (
            "decodes split UTF-8 stdout chunks without replacement characters",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let script =
                    "const b=Buffer.from('prefix € suffix\\n','utf8');" +
                    "process.stdout.write(b.subarray(0,8));" +
                    "setTimeout(()=>process.stdout.write(b.subarray(8)),25);"

                let request = NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |]
                let! result = run (NodeProcess.run request (OperationContext.detached "runtime-text-output"))

                match result with
                | Succeeded outcome -> Vitest.expect(outcome.Value.StdOut).toBe ("prefix € suffix\n")
                | PartiallySucceeded _
                | Failed _ -> failwith "Expected text process output."
            }
        )

        Vitest.test (
            "cancellation terminates the process tree with a structured canceled result",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // A child that ticks forever until killed.
                let script = "setInterval(()=>console.log('tick'), 25)"
                let request = NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |]

                let source = OperationCancellation.Source()
                let mutable observedTicks = 0

                // Deterministic: cancel after the third observed progress line.
                let context =
                    OperationContext.create "runtime-cancel" source.Cancellation (fun progress ->
                        match progress.DisplayMessage with
                        | Some message when message.Contains "tick" ->
                            observedTicks <- observedTicks + 1

                            if observedTicks = 3 then
                                source.Cancel()
                        | _ -> ())

                let! result = run (NodeProcess.run request context)

                match result with
                | Failed failure ->
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.StateChanged).toBe (false)
                | Succeeded _
                | PartiallySucceeded _ -> failwith "Expected a structured canceled failure."

                Vitest.expect(observedTicks >= 3).toBe (true)
            }
        )

        Vitest.test (
            "observed process output is redacted before progress reporting",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let script =
                    "console.log('Authorization: Bearer topsecret123'); console.log('fetch https://user:hunter2@host.example/repo.git')"

                let request = NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |]
                let progressMessages = ResizeArray<string>()

                let context =
                    OperationContext.create "runtime-redaction" OperationCancellation.none (fun progress ->
                        match progress.DisplayMessage with
                        | Some message -> progressMessages.Add message
                        | None -> ())

                let! result = run (NodeProcess.run request context)

                match result with
                | Succeeded _ -> ()
                | PartiallySucceeded _
                | Failed _ -> failwith "Expected the redaction process to succeed."

                Vitest.expect(progressMessages.Count >= 2).toBe (true)

                for message in progressMessages do
                    Vitest.expect(message.Contains "topsecret123").toBe (false)
                    Vitest.expect(message.Contains "hunter2").toBe (false)
            }
        )

        Vitest.test (
            "a missing executable becomes a structured dependency failure",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let request =
                    NodeProcess.ProcessRequest.create "definitely-not-a-real-executable-42" [| "--version" |]

                let! result = run (NodeProcess.run request (OperationContext.detached "runtime-missing"))

                match result with
                | Failed failure ->
                    Vitest.expect(failure.Category).toEqual (DependencyMissing)
                    Vitest.expect(failure.Code).toBe ("spawn_failed")
                | Succeeded _
                | PartiallySucceeded _ -> failwith "Expected a structured spawn failure."
            }
        )
)

Vitest.describe (
    "Node binary IO",
    fun () ->
        Vitest.test (
            "copies invalid UTF-8 bytes exactly while hashing and detects links",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let sourcePath = NodePath.join [| root; "source.bin" |]
                    let copyPath = NodePath.join [| root; "copy.tmp" |]
                    let atomicPath = NodePath.join [| root; "atomic.bin" |]
                    let bytes = [| 0; 255; 254; 128; 65; 0; 66; 195; 40 |]
                    let sourceBuffer = bufferFromBytes bytes
                    let! _ = fsPromisesDynamic?writeFile (sourcePath, sourceBuffer) |> unbox<JS.Promise<obj>>
                    let reports = ResizeArray<float>()

                    let! copied =
                        NodeBinaryIO.copyFileWithHash
                            sourcePath
                            copyPath
                            OperationCancellation.none
                            reports.Add
                        |> Async.StartAsPromise

                    let copyResult =
                        match copied with
                        | Ok value -> value
                        | Error message -> failwith $"Expected binary copy to succeed: {message}"

                    let! copiedBuffer = fsPromisesDynamic?readFile (copyPath) |> unbox<JS.Promise<obj>>
                    Vitest.expect(bufferBase64 copiedBuffer).toBe(bufferBase64 sourceBuffer)
                    Vitest.expect(copyResult.BytesCopied).toBe(float bytes.Length)
                    Vitest.expect(copyResult.Sha256).toBe "14db38cf730a4dea6ede56fbef5ed1baa0f126118110c5124e762a56526caea0"
                    Vitest.expect(reports.Count > 0).toBe true

                    let! written =
                        NodeBinaryIO.writeBufferAtomic
                            atomicPath
                            sourceBuffer
                            OperationCancellation.none
                        |> Async.StartAsPromise

                    match written with
                    | Ok () -> ()
                    | Error message -> failwith $"Expected atomic binary write to succeed: {message}"

                    let! atomicBuffer = fsPromisesDynamic?readFile (atomicPath) |> unbox<JS.Promise<obj>>
                    Vitest.expect(bufferBase64 atomicBuffer).toBe(bufferBase64 sourceBuffer)

                    let targetDirectory = NodePath.join [| root; "target-directory" |]
                    let linkPath = NodePath.join [| root; "directory-link" |]
                    let! _ = fsPromisesDynamic?mkdir (targetDirectory) |> unbox<JS.Promise<obj>>
                    let! _ = fsPromisesDynamic?symlink (targetDirectory, linkPath, "junction") |> unbox<JS.Promise<obj>>
                    let! linkStats = NodeFileSystem.lstatAsync linkPath
                    Vitest.expect(linkStats.isSymbolicLink()).toBe true
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "cancellation removes the incomplete binary copy",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let sourcePath = NodePath.join [| root; "large-source.bin" |]
                    let temporaryPath = NodePath.join [| root; "canceled-copy.tmp" |]
                    let! _ =
                        fsPromisesDynamic?writeFile (sourcePath, filledBuffer (8 * 1024 * 1024) 173)
                        |> unbox<JS.Promise<obj>>

                    let cancellation = OperationCancellation.Source()
                    let mutable reportCount = 0

                    let! copied =
                        NodeBinaryIO.copyFileWithHash
                            sourcePath
                            temporaryPath
                            cancellation.Cancellation
                            (fun _ ->
                                reportCount <- reportCount + 1

                                if reportCount = 1 then
                                    cancellation.Cancel())
                        |> Async.StartAsPromise

                    match copied with
                    | Error _ -> ()
                    | Ok _ -> failwith "Expected the binary copy to observe cancellation."

                    let! temporaryExists =
                        fsPromisesDynamic?access (temporaryPath)
                        |> unbox<JS.Promise<obj>>
                        |> Promise.map (fun _ -> true)
                        |> Promise.catch (fun _ -> false)

                    Vitest.expect(temporaryExists).toBe false
                    Vitest.expect(reportCount > 0).toBe true
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
