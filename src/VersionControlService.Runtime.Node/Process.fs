/// Cancelable child-process execution with progress observation and redacted output.
/// Provider implementations use this instead of talking to child_process directly so
/// cancellation semantics and process-tree cleanup stay consistent.
module VersionControlService.Runtime.Node.Process

open System
open System.Text.RegularExpressions
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
    /// Stable progress phase code attached to parsed meter events.
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

/// Child-process output whose stdout remains byte-exact. Providers use this for
/// object content that must not be decoded independently per stream chunk.
type ByteProcessOutput = {
    ExitCode: int
    StdOut: obj
    StdErr: string
}

type ProgressMeter = {
    Label: string
    Percent: float
    Count: (int * int) option
}

let private progressMeterPattern =
    Regex(
        @"^(?<label>[A-Za-z][^:%\r\n]*?):\s+(?<percent>\d{1,3}(?:\.\d+)?)%(?:\s*\((?<done>\d+)/(?<total>\d+)\))?"
    )

let tryParseProgressMeter (line: string) =
    let text = (line |> Option.ofObj |> Option.defaultValue String.Empty).Trim()
    let matched =
        progressMeterPattern.Match text

    if not matched.Success then
        None
    else
        match
            Double.TryParse(
                matched.Groups["percent"].Value,
                Globalization.NumberStyles.Float,
                Globalization.CultureInfo.InvariantCulture
            )
        with
        | false, _ -> None
        | true, percent ->
            let count =
                if matched.Groups["done"].Success then
                    match
                        Int32.TryParse matched.Groups["done"].Value,
                        Int32.TryParse matched.Groups["total"].Value
                    with
                    | (true, doneCount), (true, totalCount) -> Some(doneCount, totalCount)
                    | _ -> None
                else
                    None

            Some {
                Label = matched.Groups["label"].Value.Trim()
                Percent = max 0.0 (min 100.0 percent)
                Count = count
            }

let formatProgressMeter (meter: ProgressMeter) =
    match meter.Count with
    | Some(completed, total) -> $"{meter.Label} ({completed}/{total})"
    | None -> meter.Label

let private reportProgressMeter (phaseCode: string) (context: OperationContext) (meter: ProgressMeter) =
    context.ReportProgress {
        PhaseCode = phaseCode
        Item = None
        Completed = Some meter.Percent
        Total = Some 100.0
        DisplayMessage = Some(Redaction.redact (formatProgressMeter meter))
    }

let createMeterObserver (reportLine: string -> unit) =
    let pending = System.Text.StringBuilder()

    let observe (text: string) =
        let lines = (pending.ToString() + text).Split([| '\r'; '\n' |], StringSplitOptions.None)
        pending.Clear().Append(lines.[lines.Length - 1]) |> ignore

        for index = 0 to lines.Length - 2 do
            reportLine lines.[index]

    let flush () =
        if pending.Length > 0 then
            reportLine (pending.ToString())
            pending.Clear() |> ignore

    observe, flush

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
        // Wait for taskkill itself to finish. Otherwise the canceled process can
        // report close while a descendant still holds its working directory.
        childProcessModule?spawnSync ("taskkill", [| "/pid"; string pid; "/T"; "/F" |])
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

/// Runs a child process under the operation context. On cancellation, it terminates
/// the process tree and returns a canceled failure with StateChanged = false.
/// Parsed progress meter lines are redacted before progress reporting.
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
            let mutable exited = false
            let mutable exitCode = -1

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
                let stdoutDecoder = Interop.createUtf8StringDecoder ()
                let stderrDecoder = Interop.createUtf8StringDecoder ()
                let mutable canceled = false

                let reportLine (line: string) =
                    tryParseProgressMeter line
                    |> Option.iter (reportProgressMeter request.ProgressPhase context)

                let observeStderr, flushStderr = createMeterObserver reportLine

                let observeStream
                    (stream: obj)
                    (decoder: obj)
                    (buffer: System.Text.StringBuilder)
                    (observeProgress: (string -> unit) option)
                    =
                    if not (isNull stream) then
                        stream?on ("data", fun (data: obj) ->
                            let text = Interop.decodeUtf8Chunk decoder data
                            buffer.Append text |> ignore
                            observeProgress |> Option.iter (fun observe -> observe text))
                        |> ignore

                observeStream child?stdout stdoutDecoder stdout None
                observeStream child?stderr stderrDecoder stderr (Some observeStderr)

                child?on ("exit", fun (code: obj) ->
                    exited <- true
                    exitCode <- if isNull code then -1 else unbox<int> code)
                |> ignore

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

                child?on ("close", fun (_code: obj) ->
                    if not finished then
                        finished <- true

                        if canceled then
                            resolve (
                                OperationResult.canceled "The operation was canceled and its process tree terminated."
                            )
                        else
                            let finishStream
                                (decoder: obj)
                                (buffer: System.Text.StringBuilder)
                                =
                                let tail = Interop.finishUtf8Decoding decoder

                                if not (String.IsNullOrEmpty tail) then
                                    buffer.Append tail |> ignore
                                tail

                            finishStream stdoutDecoder stdout |> ignore
                            let stderrTail = finishStream stderrDecoder stderr

                            if not (String.IsNullOrEmpty stderrTail) then
                                observeStderr stderrTail

                            flushStderr ()

                            resolve (
                                OperationResult.succeeded {
                                    ExitCode = exitCode
                                    StdOut = stdout.ToString()
                                    StdErr = stderr.ToString()
                                }
                            ))
                |> ignore

                context.Cancellation.Register(fun () ->
                    // A cancel after exit must not turn a finished process into a canceled one.
                    if not finished && not exited then
                        canceled <- true
                        killProcessTree (unbox<int> child?pid))

                match request.StdinData with
                | Some data ->
                    child?stdin?write (data) |> ignore
                    child?stdin?``end`` () |> ignore
                | None -> child?stdin?``end`` () |> ignore)

/// Runs a child process while preserving stdout as raw bytes. Stderr is decoded
/// only after every chunk has arrived so diagnostics remain byte-boundary safe.
let runBytes
    (request: ProcessRequest)
    (context: OperationContext)
    : Async<OperationResult<ByteProcessOutput>> =
    Async.FromContinuations(fun (resolve, _, _) ->
        if context.Cancellation.IsCancellationRequested() then
            resolve (OperationResult.canceled "The operation was canceled before the process started.")
        else
            let options =
                createObj [
                    "env" ==> mergeEnvironment (createObj [ for key, value in request.Environment -> key ==> value ])
                    "windowsHide" ==> true
                    "detached" ==> not (isWindows ())
                ]

            match request.WorkingDirectory with
            | Some workingDirectory -> options?cwd <- workingDirectory
            | None -> ()

            let mutable finished = false
            let mutable exited = false
            let mutable exitCode = -1

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
                let stdout = ResizeArray<obj>()
                let stderr = ResizeArray<obj>()
                let mutable canceled = false

                let observeBytes (stream: obj) (chunks: ResizeArray<obj>) =
                    if not (isNull stream) then
                        stream?on ("data", fun (data: obj) -> chunks.Add data) |> ignore

                observeBytes child?stdout stdout
                observeBytes child?stderr stderr

                child?on ("exit", fun (code: obj) ->
                    exited <- true
                    exitCode <- if isNull code then -1 else unbox<int> code)
                |> ignore

                child?on ("error", fun (error: obj) ->
                    if not finished then
                        finished <- true

                        resolve (
                            OperationResult.failed (
                                OperationFailure.createRedacted
                                    DependencyMissing
                                    "spawn_failed"
                                    $"Could not run '{request.Command}': {unbox<string> error?message}"
                            )
                        ))
                |> ignore

                child?on ("close", fun (_code: obj) ->
                    if not finished then
                        finished <- true

                        if canceled then
                            resolve (
                                OperationResult.canceled "The operation was canceled and its process tree terminated."
                            )
                        else
                            let stderrText =
                                stderr.ToArray()
                                |> Interop.bufferConcat
                                |> Interop.bufferToUtf8String

                            let reportLine (line: string) =
                                tryParseProgressMeter line
                                |> Option.iter (reportProgressMeter request.ProgressPhase context)

                            let observeProgress, flushProgress = createMeterObserver reportLine
                            observeProgress stderrText
                            flushProgress ()

                            resolve (
                                OperationResult.succeeded {
                                    ExitCode = exitCode
                                    StdOut = stdout.ToArray() |> Interop.bufferConcat
                                    StdErr = stderrText
                                }
                            ))
                |> ignore

                context.Cancellation.Register(fun () ->
                    if not finished && not exited then
                        canceled <- true
                        killProcessTree (unbox<int> child?pid))

                match request.StdinData with
                | Some data ->
                    child?stdin?write (data) |> ignore
                    child?stdin?``end`` () |> ignore
                | None -> child?stdin?``end`` () |> ignore)
