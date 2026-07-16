module VersionControlService.Tests.NodeRuntimeTests

open Fable.Core
open VersionControlService.Abstractions
open Vitest

module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeInterop = VersionControlService.Runtime.Node.Interop

let private run (operation: Async<'T>) : JS.Promise<'T> = Async.StartAsPromise operation

let private nodeExecutable: string = Fable.Core.JsInterop.emitJsExpr () "process.execPath"

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
