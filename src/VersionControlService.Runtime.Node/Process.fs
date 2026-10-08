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
    Transferred: string option
}

let private progressMeterPattern =
    Regex(
        @"^(?<label>[A-Za-z][^:%\r\n]*?):\s+(?<percent>\d{1,3}(?:\.\d+)?)%(?:\s*\((?<done>\d+)/(?<total>\d+)\)(?:,\s*(?<transferred>[^|\r\n]+)\s*\|)?)?"
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

            let transferred =
                if count.IsSome && matched.Groups["transferred"].Success then
                    Some(matched.Groups["transferred"].Value.Trim())
                else
                    None

            Some {
                Label = matched.Groups["label"].Value.Trim()
                Percent = max 0.0 (min 100.0 percent)
                Count = count
                Transferred = transferred
            }

let formatProgressMeter (meter: ProgressMeter) =
    match meter.Count, meter.Transferred with
    | Some(completed, total), Some transferred -> $"{meter.Label} ({completed}/{total}), {transferred}"
    | Some(completed, total), None -> $"{meter.Label} ({completed}/{total})"
    | None, _ -> meter.Label

let progressMeterCompletedTotal (meter: ProgressMeter) =
    match meter.Count with
    | Some(_, 1) -> None
    | _ -> Some(meter.Percent, 100.0)

let private reportProgressMeter (phaseCode: string) (context: OperationContext) (meter: ProgressMeter) =
    let completedTotal = progressMeterCompletedTotal meter

    context.ReportProgress {
        PhaseCode = phaseCode
        Item = None
        Completed = completedTotal |> Option.map fst
        Total = completedTotal |> Option.map snd
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

type ProcessExistence =
    | Alive
    | Gone
    | Unknown of string

type ChildExit = {
    ExitCode: int option
    Signal: string option
    Stderr: string
    StderrTruncated: bool
    SpawnError: string option
}

type SpawnToFileHandle = {
    Pid: int
    Closed: JS.Promise<ChildExit>
}

type BoundedResult = {
    ExitCode: int option
    Stdout: byte[]
    Stderr: string
    Error: string option
}

[<AllowNullLiteral>]
type TypedProcessStream =
    abstract member on: eventName: string * listener: (obj -> unit) -> TypedProcessStream

[<AllowNullLiteral>]
type TypedChildProcess =
    abstract member pid: int
    abstract member stdout: TypedProcessStream
    abstract member stderr: TypedProcessStream
    abstract member on: eventName: string * listener: (obj -> unit) -> TypedChildProcess
    abstract member on: eventName: string * listener: (obj -> obj -> unit) -> TypedChildProcess

type private TypedChildProcessModule =
    abstract member spawn: command: string * arguments: string[] * options: obj -> TypedChildProcess

type private TypedProcessGlobal =
    abstract member platform: string
    abstract member kill: pid: int * signal: obj -> bool

[<ImportAll("node:child_process")>]
let private typedChildProcess: TypedChildProcessModule = jsNative

[<Emit("process")>]
let private typedProcessGlobal () : TypedProcessGlobal = jsNative

let private closeDescriptor (descriptor: int) : JS.Promise<unit> =
    VersionControlService.Runtime.Node.PositionalFile.close descriptor

[<Emit("$0 && $0.code ? $0.code : ''")>]
let private nodeErrorCode (_error: obj) : string = jsNative

let private processKill (pid: int) (signal: obj) =
    (typedProcessGlobal ()).kill(pid, signal) |> ignore

let killProcessTreeAsync (pid: int) : JS.Promise<unit> =
    JS.Constructors.Promise.Create(fun resolve _ ->
        if isWindows () then
            let options = createObj [ "windowsHide" ==> true; "shell" ==> false; "stdio" ==> "ignore" ]

            try
                let child =
                    typedChildProcess.spawn("taskkill", [| "/pid"; string pid; "/T"; "/F" |], options)

                let mutable settled = false
                let finish () =
                    if not settled then
                        settled <- true
                        resolve ()

                child.on("close", fun _ -> finish ()) |> ignore
                child.on("error", fun _ -> ()) |> ignore
            with _ ->
                resolve ()
        else
            try processKill (-pid) (box "SIGKILL") with _ -> ()
            try processKill pid (box "SIGKILL") with _ -> ()
            resolve ())

let processExistence (pid: int) : ProcessExistence =
    try
        processKill pid (box 0)
        Alive
    with error ->
        match nodeErrorCode (box error) with
        | "EPERM" -> Alive
        | "ESRCH" -> Gone
        | _ -> Unknown(Interop.errorMessage error)

let private spawnOptions (cwd: string) (environment: obj) (stdio: obj) =
    createObj [
        "cwd" ==> cwd
        "env" ==> environment
        "stdio" ==> stdio
        "shell" ==> false
        "windowsHide" ==> true
        "detached" ==> not (isWindows ())
    ]

let private createClosedPromise () =
    let mutable complete: (ChildExit -> unit) option = None
    let task =
        JS.Constructors.Promise.Create(fun resolve _ ->
            complete <- Some resolve)

    let finish value = complete |> Option.iter (fun resolve -> resolve value)
    task, finish

let private emptyChildExit (spawnError: string option) = {
    ExitCode = None
    Signal = None
    Stderr = ""
    StderrTruncated = false
    SpawnError = spawnError
}

let private captureBounded
    (chunks: ResizeArray<obj>)
    (limit: int)
    (length: int ref)
    (truncated: bool ref)
    (data: obj)
    =
    let bytes = Interop.bufferLength data
    let available = max 0 (limit - length.Value)
    let copied = min bytes available

    if copied > 0 then
        chunks.Add(if copied = bytes then data else Interop.bufferSubarray data 0 copied)
        length.Value <- length.Value + copied

    if copied < bytes then
        truncated.Value <- true

let private stdoutBytes (chunks: ResizeArray<obj>) =
    Interop.bufferConcatBytes (chunks.ToArray())

let private stderrText (chunks: ResizeArray<obj>) =
    Interop.bufferConcat (chunks.ToArray()) |> Interop.bufferToUtf8String

let private closeDescriptorThen
    (descriptor: int)
    (onClosed: unit -> unit)
    (onError: obj -> unit)
    =
    Interop.observePromise (closeDescriptor descriptor) onClosed onError

/// Spawns a child that writes its standard output to the descriptor. The spawn function is a parameter so a
/// caller can substitute a child, for example one without stdio streams as after an EMFILE failure.
let spawnToFileTrackedWith
    (spawn: string * string[] * obj -> TypedChildProcess)
    (command: string)
    (arguments: string[])
    (cwd: string)
    (environment: obj)
    (descriptor: int)
    (onStarted: int -> JS.Promise<ChildExit> -> unit)
    : JS.Promise<SpawnToFileHandle> =
    JS.Constructors.Promise.Create(fun resolve reject ->
        let closed, finishClosed = createClosedPromise ()
        let options = spawnOptions cwd environment (box [| box "ignore"; box descriptor; box "pipe" |])

        try
            let child = spawn (command, arguments, options)
            let pid =
                try unbox<int> (box child.pid)
                with _ -> 0

            let stderrChunks = ResizeArray<obj>()
            let stderrLength = ref 0
            let stderrTruncated = ref false
            let spawnError = ref None

            // A failed spawn emits 'error' and can leave the stdio streams null, so the listener comes first.
            child.on("error", fun error -> spawnError.Value <- Some(Interop.errorMessage error)) |> ignore

            if not (isNull child.stderr) then
                child.stderr.on("data", fun data ->
                    captureBounded stderrChunks 65536 stderrLength stderrTruncated data)
                |> ignore

            child.on("close", fun code signal ->
                let exitCode = if isNull code then None else Some(unbox<int> code)
                let signalName = if isNull signal then None else Some(unbox<string> signal)

                finishClosed {
                    ExitCode = exitCode
                    Signal = signalName
                    Stderr = stderrText stderrChunks
                    StderrTruncated = stderrTruncated.Value
                    SpawnError = spawnError.Value
                })
            |> ignore

            if pid > 0 then
                onStarted pid closed

            closeDescriptorThen
                descriptor
                (fun () -> resolve { Pid = pid; Closed = closed })
                (fun error ->
                    if pid > 0 then
                        let rejectAfterClose () =
                            Interop.observePromise
                                closed
                                (fun _ -> reject (Exception(Interop.errorMessage error)))
                                (fun _ -> reject (Exception(Interop.errorMessage error)))

                        Interop.observePromise
                            (killProcessTreeAsync pid)
                            (fun () -> rejectAfterClose ())
                            (fun _ -> rejectAfterClose ())
                    else
                        reject (Exception(Interop.errorMessage error)))
        with error ->
            let failure = Interop.errorMessage (box error)
            finishClosed (emptyChildExit (Some failure))

            closeDescriptorThen
                descriptor
                (fun () -> resolve { Pid = 0; Closed = closed })
                (fun closeError -> reject (Exception(Interop.errorMessage closeError))))

let spawnToFileTracked
    (command: string)
    (arguments: string[])
    (cwd: string)
    (environment: obj)
    (descriptor: int)
    (onStarted: int -> JS.Promise<ChildExit> -> unit)
    : JS.Promise<SpawnToFileHandle> =
    spawnToFileTrackedWith typedChildProcess.spawn command arguments cwd environment descriptor onStarted

/// Runs a child with bounded output. The spawn function is a parameter so a caller can substitute a child, for
/// example one without stdio streams as after an EMFILE failure.
let runBoundedWithSpawn
    (spawn: string * string[] * obj -> TypedChildProcess)
    (command: string)
    (arguments: string[])
    (cwd: string)
    (environment: obj)
    (stdoutLimit: int)
    (stderrLimit: int)
    (onStarted: int -> JS.Promise<ChildExit> -> unit)
    : JS.Promise<BoundedResult> =
    JS.Constructors.Promise.Create(fun resolve _ ->
        let options = spawnOptions cwd environment (box [| box "ignore"; box "pipe"; box "pipe" |])

        try
            let child = spawn (command, arguments, options)
            let pid =
                try unbox<int> (box child.pid)
                with _ -> 0

            let closed, finishClosed = createClosedPromise ()
            let stdoutChunks = ResizeArray<obj>()
            let stderrChunks = ResizeArray<obj>()
            let stdoutLength = ref 0
            let stderrLength = ref 0
            let stdoutTruncated = ref false
            let stderrTruncated = ref false
            let spawnError = ref None
            let mutable overflowKillStarted = false

            // A failed spawn emits 'error' and can leave the stdio streams null, so the listener comes first.
            child.on("error", fun error -> spawnError.Value <- Some(Interop.errorMessage error)) |> ignore

            if not (isNull child.stdout) then
                child.stdout.on("data", fun data ->
                    captureBounded stdoutChunks stdoutLimit stdoutLength stdoutTruncated data

                    if stdoutTruncated.Value && not overflowKillStarted then
                        overflowKillStarted <- true

                        if pid > 0 then
                            Interop.observePromise (killProcessTreeAsync pid) ignore ignore)
                |> ignore

            if not (isNull child.stderr) then
                child.stderr.on("data", fun data ->
                    captureBounded stderrChunks stderrLimit stderrLength stderrTruncated data)
                |> ignore

            child.on("close", fun code signal ->
                let exitCode = if isNull code then None else Some(unbox<int> code)
                let signalName = if isNull signal then None else Some(unbox<string> signal)
                let stderr = stderrText stderrChunks
                let childExit = {
                    ExitCode = exitCode
                    Signal = signalName
                    Stderr = stderr
                    StderrTruncated = stderrTruncated.Value
                    SpawnError = spawnError.Value
                }

                finishClosed childExit

                let failure =
                    if stdoutTruncated.Value then
                        Some $"Standard output exceeded the {stdoutLimit}-byte limit."
                    else
                        spawnError.Value

                resolve {
                    ExitCode = exitCode
                    Stdout = stdoutBytes stdoutChunks
                    Stderr = stderr
                    Error = failure
                })
            |> ignore

            if pid > 0 then
                onStarted pid closed
        with error ->
            resolve {
                ExitCode = None
                Stdout = [||]
                Stderr = ""
                Error = Some(Interop.errorMessage (box error))
            })

let runBoundedWithLifecycle
    (command: string)
    (arguments: string[])
    (cwd: string)
    (environment: obj)
    (stdoutLimit: int)
    (stderrLimit: int)
    (onStarted: int -> JS.Promise<ChildExit> -> unit)
    : JS.Promise<BoundedResult> =
    runBoundedWithSpawn typedChildProcess.spawn command arguments cwd environment stdoutLimit stderrLimit onStarted

let runBounded
    (command: string)
    (arguments: string[])
    (cwd: string)
    (environment: obj)
    (stdoutLimit: int)
    (stderrLimit: int)
    : JS.Promise<BoundedResult> =
    runBoundedWithLifecycle command arguments cwd environment stdoutLimit stderrLimit (fun _ _ -> ())

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
