/// Cancelable child-process execution with progress observation and redacted output.
/// Provider implementations use this instead of talking to child_process directly so
/// cancellation semantics and process-tree cleanup stay consistent.
module VersionControlService.Runtime.Node.Process

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions

type ProcessRequest = {
    Command: string
    Arguments: string[]
    WorkingDirectory: string option
    /// Data written to stdin after spawn (e.g. NUL-delimited path lists), then stdin closes.
    StdinData: string option
    /// Additional environment entries layered over the current process environment.
    Environment: (string * string)[]
    /// Stable progress phase code attached to observed output lines.
    ProgressPhase: string
}

module ProcessRequest =

    let create (command: string) (arguments: string[]) = {
        Command = command
        Arguments = arguments
        WorkingDirectory = None
        StdinData = None
        Environment = [||]
        ProgressPhase = "process"
    }

type ProcessOutput = {
    ExitCode: int
    StdOut: string
    StdErr: string
}

let private childProcessModule: obj = importAll "child_process"
let private processGlobal: obj = emitJsExpr () "process"

[<Emit("Object.assign({}, process.env, $0)")>]
let private mergeEnvironment (_extra: obj) : obj = jsNative

let private isWindows () : bool =
    unbox<string> processGlobal?platform = "win32"

/// Terminates a whole process tree. Windows needs taskkill /T; detached POSIX
/// children get their process group signaled, with a direct SIGKILL fallback.
let killProcessTree (pid: int) : unit =
    if isWindows () then
        childProcessModule?spawn ("taskkill", [| "/pid"; string pid; "/T"; "/F" |])
        |> ignore
    else
        try
            processGlobal?kill (-pid, "SIGKILL") |> ignore
        with _ ->
            try
                processGlobal?kill (pid, "SIGKILL") |> ignore
            with _ ->
                ()

/// True when a process with the given pid is still alive (signal 0 probe).
let isProcessAlive (pid: int) : bool =
    try
        processGlobal?kill (pid, 0) |> ignore
        true
    with _ ->
        false

/// Runs a child process under the operation context. Cancellation terminates the
/// process tree and yields a structured canceled failure with StateChanged = false;
/// observed output lines are redacted before they reach progress reporting.
let run (request: ProcessRequest) (context: OperationContext) : Async<OperationResult<ProcessOutput>> =
    Async.FromContinuations(fun (resolve, _, _) ->
        if context.Cancellation.IsCancellationRequested() then
            resolve (OperationResult.canceled "The operation was canceled before the process started.")
        else
            let options =
                createObj [
                    "env" ==> mergeEnvironment (createObj [ for key, value in request.Environment -> key ==> value ])
                    "windowsHide" ==> true
                    // Detached POSIX children get their own process group so the
                    // whole tree can be signaled; Windows uses taskkill /T instead.
                    "detached" ==> not (isWindows ())
                ]

            match request.WorkingDirectory with
            | Some workingDirectory -> options?cwd <- workingDirectory
            | None -> ()

            let mutable finished = false

            let child: obj =
                try
                    childProcessModule?spawn (request.Command, request.Arguments, options)
                with error ->
                    finished <- true

                    resolve (
                        OperationResult.failed (
                            OperationFailure.createRedacted
                                DependencyMissing
                                "spawn_failed"
                                $"Could not start '{request.Command}': {error.Message}"
                        )
                    )

                    null

            if not finished then
                let stdout = System.Text.StringBuilder()
                let stderr = System.Text.StringBuilder()
                let mutable canceled = false

                let reportLine (line: string) =
                    if not (String.IsNullOrWhiteSpace line) then
                        context.ReportProgress {
                            PhaseCode = request.ProgressPhase
                            Item = None
                            Completed = None
                            Total = None
                            DisplayMessage = Some(Redaction.redact line)
                        }

                let observeStream (stream: obj) (buffer: System.Text.StringBuilder) =
                    if not (isNull stream) then
                        stream?on ("data", fun (data: obj) ->
                            let text: string = unbox (data?toString "utf8")
                            buffer.Append text |> ignore
                            text.Split '\n' |> Array.iter reportLine)
                        |> ignore

                observeStream child?stdout stdout
                observeStream child?stderr stderr

                child?on ("error", fun (error: obj) ->
                    if not finished then
                        finished <- true

                        resolve (
                            OperationResult.failed (
                                OperationFailure.createRedacted
                                    DependencyMissing
                                    "spawn_failed"
                                    $"""Could not run '{request.Command}': {unbox<string> error?message}"""
                            )
                        ))
                |> ignore

                child?on ("close", fun (code: obj) ->
                    if not finished then
                        finished <- true

                        if canceled then
                            resolve (
                                OperationResult.canceled "The operation was canceled and its process tree terminated."
                            )
                        else
                            resolve (
                                OperationResult.succeeded {
                                    ExitCode = (if isNull code then -1 else unbox<int> code)
                                    StdOut = stdout.ToString()
                                    StdErr = stderr.ToString()
                                }
                            ))
                |> ignore

                context.Cancellation.Register(fun () ->
                    if not finished then
                        canceled <- true
                        killProcessTree (unbox<int> child?pid))

                match request.StdinData with
                | Some data ->
                    child?stdin?write (data) |> ignore
                    child?stdin?``end`` () |> ignore
                | None -> child?stdin?``end`` () |> ignore)
