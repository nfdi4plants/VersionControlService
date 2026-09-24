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
            "parses progress meter lines",
            fun () ->
                let cases: (string * NodeProcess.ProgressMeter option)[] = [|
                    "Uploading LFS objects:  45% (9/20), 1.2 MB | 1.0 MB/s",
                    Some {
                        Label = "Uploading LFS objects"
                        Percent = 45.0
                        Count = Some(9, 20)
                    };
                    "Downloading LFS objects: 100% (3/3), 3.1 MB | 2 MB/s, done.",
                    Some {
                        Label = "Downloading LFS objects"
                        Percent = 100.0
                        Count = Some(3, 3)
                    };
                    "Writing objects: 100% (3/3), 250 bytes | 250.00 KiB/s, done.",
                    Some {
                        Label = "Writing objects"
                        Percent = 100.0
                        Count = Some(3, 3)
                    };
                    "Receiving objects: 12.5% (1/8)",
                    Some {
                        Label = "Receiving objects"
                        Percent = 12.5
                        Count = Some(1, 8)
                    };
                    "Receiving objects: 7% complete",
                    Some {
                        Label = "Receiving objects"
                        Percent = 7.0
                        Count = None
                    };
                    "Uploading LFS objects: 150% (3/2)",
                    Some {
                        Label = "Uploading LFS objects"
                        Percent = 100.0
                        Count = Some(3, 2)
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
                Vitest.expect(reports[0].DisplayMessage).toEqual (Some "Uploading LFS objects (9/20)")
                Vitest.expect(reports[0].Completed).toEqual (Some 45.0)
                Vitest.expect(reports[0].Total).toEqual (Some 100.0)
                Vitest.expect(reports[1].DisplayMessage).toEqual (Some "Uploading LFS objects (20/20)")
                Vitest.expect(reports[1].Completed).toEqual (Some 100.0)
                Vitest.expect(reports[1].Total).toEqual (Some 100.0)
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
            "progress headlines contain meter labels only",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let script =
                    "console.error('Uploading LFS objects: 50% (1/2), Authorization: Bearer topsecret123');" +
                    "console.error('Downloading LFS objects: 100% (2/2), fetch https://user:hunter2@host.example/repo.git')"

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
                Vitest.expect(progressMessages[0]).toBe "Uploading LFS objects (1/2)"
                Vitest.expect(progressMessages[1]).toBe "Downloading LFS objects (2/2)"

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
