module VersionControlService.Tests.NodeRuntimeTests

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open Vitest

module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeOutput = VersionControlService.Runtime.Node.OutputReporter
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeBinaryIO = VersionControlService.Runtime.Node.BinaryIO
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile

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

[<Emit("""
(() => {
    const childProcess = require('node:child_process');
    const moduleApi = require('node:module');
    const originalSpawn = childProcess.spawn;
    childProcess.spawn = function(command, args, options) {
        const child = originalSpawn.call(childProcess, command, args, options);
        process.nextTick(() => child.once('exit', () => $0()));
        return child;
    };
    moduleApi.syncBuiltinESMExports();
    return () => {
        childProcess.spawn = originalSpawn;
        moduleApi.syncBuiltinESMExports();
    };
})()
""")>]
let private cancelAfterChildExit (_cancel: unit -> unit) : unit -> unit = jsNative

[<Import("EventEmitter", "node:events")>]
let private eventEmitterClass: obj = jsNative

/// A child as Node returns it after an EMFILE failure: no pid and no stdio streams.
[<Emit("(() => { const child = new $0(); child.pid = undefined; child.stdout = null; child.stderr = null; return child; })()")>]
let private streamlessChild (_emitterClass: obj) : NodeProcess.TypedChildProcess = jsNative

[<Emit("$0.emit('error', new Error($1))")>]
let private emitSpawnError (_child: obj) (_message: string) : unit = jsNative

[<Emit("$0.emit('close', -1, null)")>]
let private emitClose (_child: obj) : unit = jsNative

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = NodePath.join [| osDynamic?tmpdir () |> unbox<string>; "vcs-node-runtime-" |]
    fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>

let private removeDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (
            path,
            createObj [ "recursive" ==> true; "force" ==> true; "maxRetries" ==> 10; "retryDelay" ==> 50 ]
        )
        |> unbox<JS.Promise<obj>>

    return ()
}

Vitest.describe (
    "Node runtime process adapter",
    fun () ->
        Vitest.test (
            "parses progress meter lines",
            fun () ->
                let cases: (string * NodeProcess.ProgressMeter option)[] = [|
                    "Uploading LFS objects:  45% (9/20), 1.2 MB | 1.0 MB/s",
                    Some {
                        Label = "Uploading LFS objects"
                        Percent = 45.0
                        Count = Some(9, 20)
                        Transferred = Some "1.2 MB"
                    };
                    "Uploading LFS objects:   0% (0/1), 1.2 MB | 1.0 MB/s",
                    Some {
                        Label = "Uploading LFS objects"
                        Percent = 0.0
                        Count = Some(0, 1)
                        Transferred = Some "1.2 MB"
                    };
                    "Downloading LFS objects: 100% (3/3), 3.1 MB | 2 MB/s, done.",
                    Some {
                        Label = "Downloading LFS objects"
                        Percent = 100.0
                        Count = Some(3, 3)
                        Transferred = Some "3.1 MB"
                    };
                    "Writing objects: 100% (3/3), 250 bytes | 250.00 KiB/s, done.",
                    Some {
                        Label = "Writing objects"
                        Percent = 100.0
                        Count = Some(3, 3)
                        Transferred = Some "250 bytes"
                    };
                    "Receiving objects: 12.5% (1/8)",
                    Some {
                        Label = "Receiving objects"
                        Percent = 12.5
                        Count = Some(1, 8)
                        Transferred = None
                    };
                    "Receiving objects: 7% complete",
                    Some {
                        Label = "Receiving objects"
                        Percent = 7.0
                        Count = None
                        Transferred = None
                    };
                    "Uploading LFS objects: 150% (3/2)",
                    Some {
                        Label = "Uploading LFS objects"
                        Percent = 100.0
                        Count = Some(3, 2)
                        Transferred = None
                    };
                    "remote: Counting objects: 100% (5/5)", None;
                    "To https://localhost:3443/e2eadmin/arc.git", None;
                    "   abc1234..def5678  main -> main", None;
                    "4b825dc642cb6eb9a060e54bf8d69288fbee4904", None;
                    ".git/MERGE_HEAD", None;
                    "https://localhost:3443/x/y.git", None;
                    "", None
                |]

                for line, expected in cases do
                    Vitest.expect(NodeProcess.tryParseProgressMeter line).toEqual expected

                let formatted =
                    NodeProcess.tryParseProgressMeter "Uploading LFS objects:   0% (0/1), 1.2 MB | 1.0 MB/s"
                    |> Option.map NodeProcess.formatProgressMeter

                Vitest.expect(formatted).toEqual (Some "Uploading LFS objects (0/1), 1.2 MB")
        )

        Vitest.test (
            "uses meter counts to decide whether to report percentages",
            fun () ->
                let singleObject: NodeProcess.ProgressMeter = {
                    Label = "Uploading LFS objects"
                    Percent = 0.0
                    Count = Some(0, 1)
                    Transferred = Some "1.2 MB"
                }

                let multipleObjects: NodeProcess.ProgressMeter = {
                    Label = "Uploading LFS objects"
                    Percent = 33.0
                    Count = Some(1, 3)
                    Transferred = None
                }

                let countlessMeter: NodeProcess.ProgressMeter = {
                    Label = "Receiving objects"
                    Percent = 7.0
                    Count = None
                    Transferred = None
                }

                Vitest.expect(NodeProcess.progressMeterCompletedTotal singleObject).toEqual None
                Vitest.expect(NodeProcess.progressMeterCompletedTotal multipleObjects).toEqual (Some(33.0, 100.0))
                Vitest.expect(NodeProcess.progressMeterCompletedTotal countlessMeter).toEqual (Some(7.0, 100.0))
        )

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
            "reports meter lines and preserves unreported child output",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let stdoutText = "4b825dc642cb6eb9a060e54bf8d69288fbee4904\n"
                let stderrText =
                    "Uploading LFS objects:  45% (9/20), 1.2 MB | 1.0 MB/s\r" +
                    "Uploading LFS objects: 100% (20/20), 2.4 MB | 1.0 MB/s, done.\n"

                let script =
                    "process.stdout.write('4b825dc642cb6eb9a060e54bf8d69288fbee4904\\n');" +
                    "process.stderr.write('Uploading LFS objects:  45% (9/20), 1.2 MB | 1.0 MB/s\\r" +
                    "Uploading LFS objects: 100% (20/20), 2.4 MB | 1.0 MB/s, done.\\n');"

                let request = NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |]
                let reports = ResizeArray<OperationProgress>()
                let context =
                    OperationContext.create "runtime-progress-meter" OperationCancellation.none reports.Add

                let! result = run (NodeProcess.run request context)

                match result with
                | Succeeded outcome ->
                    Vitest.expect(outcome.Value.ExitCode).toBe 0
                    Vitest.expect(outcome.Value.StdOut).toBe stdoutText
                    Vitest.expect(outcome.Value.StdErr).toBe stderrText
                | PartiallySucceeded _
                | Failed _ -> failwith "Expected the process to succeed."

                Vitest.expect(reports.Count).toBe 2
                Vitest.expect(reports[0].DisplayMessage).toEqual (Some "Uploading LFS objects (9/20), 1.2 MB")
                Vitest.expect(reports[0].Completed).toEqual (Some 45.0)
                Vitest.expect(reports[0].Total).toEqual (Some 100.0)
                Vitest.expect(reports[1].DisplayMessage).toEqual (Some "Uploading LFS objects (20/20), 2.4 MB")
                Vitest.expect(reports[1].Completed).toEqual (Some 100.0)
                Vitest.expect(reports[1].Total).toEqual (Some 100.0)
            }
        )

        Vitest.test (
            "reports stdout meters under the request phase code when the request opts in",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let script =
                    "process.stdout.write('Downloading LFS objects:  50% (1/2), 1 B | 1 B/s\\r" +
                    "Downloading LFS objects: 100% (2/2), 2 B | 1 B/s, done.\\n');"

                let request = {
                    NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |] with
                        ProgressPhase = "lfs-pull"
                        ObserveStdoutProgress = true
                }

                let reports = ResizeArray<OperationProgress>()
                let context = OperationContext.create "runtime-stdout-meters" OperationCancellation.none reports.Add
                let! result = run (NodeProcess.run request context)

                match result with
                | Succeeded outcome -> Vitest.expect(outcome.Value.ExitCode).toBe 0
                | PartiallySucceeded _
                | Failed _ -> failwith "Expected the process to succeed."

                Vitest.expect(reports |> Seq.map (fun report -> report.PhaseCode) |> Seq.toArray).toEqual [|
                    "lfs-pull"
                    "lfs-pull"
                |]

                Vitest.expect(reports |> Seq.map (fun report -> report.Completed) |> Seq.toArray).toEqual [|
                    Some 50.0
                    Some 100.0
                |]
            }
        )

        Vitest.test (
            "does not parse stdout for meters unless the request opts in",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let script =
                    "process.stdout.write('Downloading LFS objects:  50% (1/2), 1 B | 1 B/s\\n');"

                let request = NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |]
                let reports = ResizeArray<OperationProgress>()
                let context = OperationContext.create "runtime-stdout-plain" OperationCancellation.none reports.Add
                let! result = run (NodeProcess.run request context)

                match result with
                | Succeeded outcome -> Vitest.expect(outcome.Value.StdOut.Contains "Downloading LFS objects").toBe true
                | PartiallySucceeded _
                | Failed _ -> failwith "Expected the process to succeed."

                Vitest.expect(reports.Count).toBe 0
            }
        )

        Vitest.test (
            "keeps a stdout meter whole when stderr output arrives in the middle of it",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // The pauses put each write into its own chunk, so the stderr line lands inside the stdout meter.
                let script =
                    "process.stdout.write('Downloading LFS objects:  5');" +
                    "setTimeout(() => process.stderr.write('warning: slow link\\n'), 150);" +
                    "setTimeout(() => process.stdout.write('0% (1/3), 1 B | 1 B/s\\n'), 300);"

                let request = {
                    NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |] with
                        ObserveStdoutProgress = true
                }

                let reports = ResizeArray<OperationProgress>()
                let context = OperationContext.create "runtime-split-meter" OperationCancellation.none reports.Add
                let! result = run (NodeProcess.run request context)

                match result with
                | Succeeded outcome -> Vitest.expect(outcome.Value.ExitCode).toBe 0
                | PartiallySucceeded _
                | Failed _ -> failwith "Expected the process to succeed."

                Vitest.expect(reports |> Seq.map (fun report -> report.DisplayMessage) |> Seq.toArray).toEqual [|
                    Some "Downloading LFS objects (1/3), 1 B"
                |]
            }
        )

        Vitest.test (
            "cancellation after child exit preserves success",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let request = NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; "" |]
                let source = OperationCancellation.Source()
                let context = OperationContext.create "runtime-late-cancel" source.Cancellation ignore
                let restore = cancelAfterChildExit source.Cancel

                try
                    let! result = run (NodeProcess.run request context)

                    match result with
                    | Succeeded outcome -> Vitest.expect(outcome.Value.ExitCode).toBe (0)
                    | PartiallySucceeded _
                    | Failed _ -> failwith "Expected late cancellation to preserve process success."
                finally
                    restore ()
            }
        )

        Vitest.test (
            "byte-process cancellation after child exit preserves success",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let request = NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; "" |]
                let source = OperationCancellation.Source()
                let context = OperationContext.create "runtime-bytes-late-cancel" source.Cancellation ignore
                let restore = cancelAfterChildExit source.Cancel

                try
                    let! result = run (NodeProcess.runBytes request context)

                    match result with
                    | Succeeded outcome -> Vitest.expect(outcome.Value.ExitCode).toBe (0)
                    | PartiallySucceeded _
                    | Failed _ -> failwith "Expected late byte-process cancellation to preserve success."
                finally
                    restore ()
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
                let script = "setInterval(()=>console.error('tick: 50%'), 25)"
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
            "progress headlines include transfer sizes and redact secrets",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let script =
                    "console.error('Uploading LFS objects: 50% (1/2), 1.2 MB | Authorization: Bearer topsecret123');" +
                    "console.error('Downloading LFS objects: 100% (2/2), 2.4 MB | fetch https://user:hunter2@host.example/repo.git')"

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

                Vitest.expect(progressMessages.Count).toBe 2
                Vitest.expect(progressMessages[0]).toBe "Uploading LFS objects (1/2), 1.2 MB"
                Vitest.expect(progressMessages[1]).toBe "Downloading LFS objects (2/2), 2.4 MB"

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

Vitest.describe (
    "Node positional file IO",
    fun () ->
        Vitest.test (
            "writes at offset zero after the descriptor offset advances",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let filePath = NodePath.join [| root; "position-zero.bin" |]
                    let! fd = NodePositionalFile.openCreateExclusiveReadWrite filePath

                    try
                        let initial = [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy |]
                        let firstWrite = [| 201uy; 202uy |]
                        let! initialWritten =
                            NodePositionalFile.writeAt fd initial 0 initial.Length 0L

                        Vitest.expect(initialWritten).toBe initial.Length

                        let! overwritten =
                            NodePositionalFile.writeAt fd firstWrite 0 firstWrite.Length 0L

                        Vitest.expect(overwritten).toBe firstWrite.Length

                        let actual = Array.zeroCreate<byte> initial.Length
                        let! bytesRead = NodePositionalFile.readAt fd actual 0 actual.Length 0L

                        Vitest.expect(bytesRead).toBe initial.Length
                        Vitest.expect(actual).toEqual [| 201uy; 202uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy |]
                    with error ->
                        do! NodePositionalFile.close fd
                        return raise error

                    do! NodePositionalFile.close fd
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "overwrites bytes in the middle of a file",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let filePath = NodePath.join [| root; "middle-overwrite.bin" |]
                    let! fd = NodePositionalFile.openCreateExclusiveReadWrite filePath

                    try
                        let initial = [| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy; 10uy |]
                        let replacement = [| 151uy; 152uy |]
                        let! initialWritten =
                            NodePositionalFile.writeAt fd initial 0 initial.Length 0L

                        Vitest.expect(initialWritten).toBe initial.Length

                        let! overwritten =
                            NodePositionalFile.writeAt fd replacement 0 replacement.Length 4L

                        Vitest.expect(overwritten).toBe replacement.Length

                        let actual = Array.zeroCreate<byte> initial.Length
                        let! bytesRead = NodePositionalFile.readAt fd actual 0 actual.Length 0L

                        Vitest.expect(bytesRead).toBe initial.Length
                        Vitest.expect(actual).toEqual [| 1uy; 2uy; 3uy; 4uy; 151uy; 152uy; 7uy; 8uy; 9uy; 10uy |]
                    with error ->
                        do! NodePositionalFile.close fd
                        return raise error

                    do! NodePositionalFile.close fd
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "Node process spawn failures",
    fun () ->
        Vitest.test (
            "reports a spawn error for a bounded child without stdio streams",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let child = streamlessChild eventEmitterClass

                let pending =
                    NodeProcess.runBoundedWithSpawn
                        (fun _ -> child)
                        "git"
                        [||]
                        "."
                        (createObj [])
                        1024
                        1024
                        (fun _ _ -> ())

                emitSpawnError child "spawn EMFILE"
                emitClose child
                let! result = pending
                Vitest.expect(result.Error.IsSome && result.Error.Value.Contains "EMFILE").toBe true
            }
        )

        Vitest.test (
            "reports a spawn error for a file-writing child without stdio streams",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    let child = streamlessChild eventEmitterClass
                    let! descriptor = NodePositionalFile.openCreateExclusive (NodePath.join [| root; "out.bin" |])

                    let! handle =
                        NodeProcess.spawnToFileTrackedWith
                            (fun _ -> child)
                            "git"
                            [||]
                            "."
                            (createObj [])
                            descriptor
                            (fun _ _ -> ())

                    emitSpawnError child "spawn EMFILE"
                    emitClose child
                    let! exit = handle.Closed
                    Vitest.expect(exit.SpawnError.IsSome && exit.SpawnError.Value.Contains "EMFILE").toBe true
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

/// A clock the test moves by hand. Timers run when the clock passes their time.
type private FakeTimer = {
    At: float
    Callback: unit -> unit
    mutable Done: bool
}

type private FakeClock() =
    let mutable now = 0.0
    let timers = ResizeArray<FakeTimer>()

    member _.Clock: NodeOutput.OutputClock = {
        Now = fun () -> now
        Schedule =
            fun delay callback ->
                let timer = {
                    At = now + delay
                    Callback = callback
                    Done = false
                }

                timers.Add timer
                fun () -> timer.Done <- true
    }

    member _.Advance(milliseconds: float) =
        let target = now + milliseconds
        let mutable running = true

        while running do
            let due =
                timers
                |> Seq.filter (fun timer -> not timer.Done && timer.At <= target)
                |> Seq.sortBy (fun timer -> timer.At)
                |> Seq.tryHead

            match due with
            | None -> running <- false
            | Some timer ->
                timer.Done <- true
                now <- max now timer.At
                timer.Callback()

        now <- target

let private newOutputChannel (clock: FakeClock) =
    let batches = ResizeArray<OperationOutput[]>()
    NodeOutput.createChannel clock.Clock batches.Add, batches

let private beginOutputCommand (channel: NodeOutput.OutputChannel) =
    NodeOutput.beginCommand channel "git" [| "fetch" |] [| "fetch" |] [||] true

let private allEntries (batches: ResizeArray<OperationOutput[]>) = batches |> Seq.collect id |> Seq.toArray

let private stderrEntries (batches: ResizeArray<OperationOutput[]>) =
    allEntries batches
    |> Array.filter (fun entry -> entry.Kind = OperationOutputKind.StdErr)

Vitest.describe (
    "Node runtime output channel",
    fun () ->
        Vitest.test (
            "a context has a channel only when its host set an output callback, and all copies share it",
            fun () ->
                Vitest.expect((NodeOutput.tryOf (OperationContext.detached "output-none")).IsNone).toBe true

                let context =
                    OperationContext.createWithOutput "output-shared" OperationCancellation.none ignore ignore

                let copy = { context with Cancellation = OperationCancellation.none }
                let first = NodeOutput.tryOf context
                let second = NodeOutput.tryOf copy

                Vitest.expect(first.IsSome && second.IsSome).toBe true
                Vitest.expect(obj.ReferenceEquals(first.Value, second.Value)).toBe true
        )

        Vitest.test (
            "reads a CRLF split across two chunks as one newline",
            fun () ->
                let channel, batches = newOutputChannel (FakeClock())
                let command = beginOutputCommand channel

                command.ObserveStderr "abc\r"
                command.ObserveStderr "\ndef\n"
                channel.Flush()

                // A lone CR followed by text would have rewritten "abc". The CRLF completes it.
                let lines = stderrEntries batches
                Vitest.expect(lines |> Array.map (fun entry -> entry.Text)).toEqual [| "abc"; "def" |]
                Vitest.expect(lines |> Array.exists (fun entry -> entry.IsPartial)).toBe false
        )

        Vitest.test (
            "collapses carriage return updates within 250 ms into one snapshot and the final state",
            fun () ->
                let clock = FakeClock()
                let channel, batches = newOutputChannel clock
                let command = beginOutputCommand channel

                command.ObserveStderr "Receiving objects:  10% (1/10)\r"
                clock.Advance 100.0
                command.ObserveStderr "Receiving objects:  50% (5/10)\r"
                clock.Advance 100.0
                command.ObserveStderr "Receiving objects: 100% (10/10), done.\n"
                channel.Flush()

                let lines = stderrEntries batches

                Vitest.expect(lines |> Array.map (fun entry -> entry.Text)).toEqual [|
                    "Receiving objects:  10% (1/10)"
                    "Receiving objects: 100% (10/10), done."
                |]

                Vitest.expect(lines |> Array.map (fun entry -> entry.IsPartial)).toEqual [| true; false |]
        )

        Vitest.test (
            "strips terminal escape sequences and control characters but keeps tabs",
            fun () ->
                let channel, batches = newOutputChannel (FakeClock())
                let command = beginOutputCommand channel

                command.ObserveStderr "\u001b[1;32mok\u001b[0m\tdone\u0001\u001b]0;title\u0007!\n"
                channel.Flush()

                Vitest.expect(stderrEntries batches |> Array.map (fun entry -> entry.Text)).toEqual [| "ok\tdone!" |]
        )

        Vitest.test (
            "replaces a line over the length limit with a notice and keeps one at the limit",
            fun () ->
                let channel, batches = newOutputChannel (FakeClock())
                let command = beginOutputCommand channel
                let atLimit = String.replicate NodeOutput.MaxLineLength "x"

                command.ObserveStderr(atLimit + "\n" + atLimit + "y\nafter\n")
                channel.Flush()

                let entries = allEntries batches |> Array.filter (fun entry -> entry.Kind <> OperationOutputKind.Command)

                Vitest.expect(entries |> Array.map (fun entry -> entry.Kind = OperationOutputKind.Notice)).toEqual [|
                    false
                    true
                    false
                |]

                Vitest.expect(entries[0].Text).toBe atLimit
                Vitest.expect(entries[1].Text.Contains "longer than").toBe true
                Vitest.expect(entries[2].Text).toBe "after"
        )

        Vitest.test (
            "drops the oldest entries when the queue is full and says how many",
            fun () ->
                let channel, batches = newOutputChannel (FakeClock())
                let command = beginOutputCommand channel
                let lines = [ for number in 1..200 -> $"line {number}\n" ] |> String.concat ""

                // The clock stands still, so the header went out and nothing else did.
                command.ObserveStderr lines
                command.Finish()

                let entries = allEntries batches |> Array.filter (fun entry -> entry.Kind <> OperationOutputKind.Command)
                let notice = entries[0]
                let texts = entries |> Array.skip 1 |> Array.map (fun entry -> entry.Text)

                Vitest.expect(notice.Kind = OperationOutputKind.Notice).toBe true
                Vitest.expect(notice.Text.Contains "72").toBe true
                Vitest.expect(texts.Length).toBe NodeOutput.MaxQueuedEntries
                Vitest.expect(texts[0]).toBe "line 73"
                Vitest.expect(texts[texts.Length - 1]).toBe "line 200"
        )

        Vitest.test (
            "delivers at most twenty entries per batch and ten batches per second",
            fun () ->
                let clock = FakeClock()
                let channel, batches = newOutputChannel clock
                let command = beginOutputCommand channel
                let lines = [ for number in 1..60 -> $"line {number}\n" ] |> String.concat ""

                command.ObserveStderr lines
                Vitest.expect(batches.Count).toBe 1
                clock.Advance 99.0
                Vitest.expect(batches.Count).toBe 1
                clock.Advance 1.0
                Vitest.expect(batches.Count).toBe 2
                clock.Advance 100.0
                Vitest.expect(batches.Count).toBe 3
                Vitest.expect(batches |> Seq.map (fun batch -> $"{batch.Length}") |> String.concat ",").toBe "1,20,20"
        )

        Vitest.test (
            "delivers the final output of a canceled process before the run completes",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // The text after the newline has no terminator, so only the final flush can deliver it.
                let script = "process.stderr.write('line one\\ntail'); setInterval(() => {}, 1000);"
                let request = NodeProcess.ProcessRequest.create nodeExecutable [| "-e"; script |]
                let batches = ResizeArray<OperationOutput[]>()
                let source = OperationCancellation.Source()

                let context =
                    OperationContext.createWithOutput "runtime-output-cancel" source.Cancellation ignore batches.Add

                let running = run (NodeProcess.run request context)
                let attempts = ref 0

                while not (allEntries batches |> Array.exists (fun entry -> entry.Text = "line one"))
                      && attempts.Value < 200 do
                    do! run (Async.Sleep 50)
                    attempts.Value <- attempts.Value + 1

                source.Cancel()
                let! result = running

                match result with
                | Failed failure -> Vitest.expect(failure.Category).toEqual Canceled
                | Succeeded _
                | PartiallySucceeded _ -> failwith "Expected the canceled process to fail."

                let tail =
                    stderrEntries batches
                    |> Array.tryFind (fun entry -> entry.Text = "tail")

                Vitest.expect(tail.IsSome && not tail.Value.IsPartial).toBe true
            }
        )
)
