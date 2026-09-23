[<AutoOpenAttribute>]
module Util

open System

let inline printGreenfn fmt =
    Printf.kprintf
        (fun s ->
            let oldColor = Console.ForegroundColor
            Console.ForegroundColor <- ConsoleColor.Green
            Console.WriteLine s
            Console.ForegroundColor <- oldColor
        )
        fmt

let inline printRedfn fmt =
    Printf.kprintf
        (fun s ->
            let oldColor = Console.ForegroundColor
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine s
            Console.ForegroundColor <- oldColor
        )
        fmt

open SimpleExec
open System.Diagnostics
open System.IO
open System.Threading.Tasks
open System.Threading

let private shellCommand cmd args =
    let argStr = args |> String.concat " "

    if OperatingSystem.IsWindows() then
        "cmd", $"/c {cmd} {argStr}"
    else
        "/bin/bash", $"-c \"{cmd} {argStr}\""

let run (cmd: string) (args: seq<string>) (workingDir: string) =
    Command.Run(cmd, args = args, workingDirectory = workingDir)


let runReadAsync (cmd: string) (args: seq<string>) (workingDir: string) =
    Command.ReadAsync(cmd, args = args, workingDirectory = workingDir)
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> _.ToTuple()


let runAsync (prefix: string) (cmd: string) (args: seq<string>) (workingDir: string) = async {
    try
        do!
            Command.RunAsync(cmd, args = args, workingDirectory = workingDir, echoPrefix = prefix)
            |> Async.AwaitTask

        return Ok()
    with ex ->
        printRedfn "[%s] Error: %s" prefix ex.Message
        return Error ex
}

// let runAsyncColored prefix color (cmd: string) (args: seq<string>) (workingDir: string) = async {
//     // Start the process directly (no cmd/bash wrapper) so killing the tree is reliable
//     let psi = ProcessStartInfo()
//     psi.FileName <- cmd

//     for a in args do
//         psi.ArgumentList.Add(a)

//     psi.WorkingDirectory <- workingDir
//     psi.RedirectStandardOutput <- false
//     psi.RedirectStandardError <- false
//     psi.RedirectStandardInput <- false
//     psi.UseShellExecute <- false
//     psi.CreateNoWindow <- true

//     let! ct = Async.CancellationToken

//     use proc = new Process()
//     proc.StartInfo <- psi

//     let oldColor = Console.ForegroundColor

//     let print (isError: bool) (line: string) =
//         if not (String.IsNullOrWhiteSpace line) then
//             Console.ForegroundColor <- color
//             Console.Write($"[{prefix}] ")
//             Console.ForegroundColor <- if isError then ConsoleColor.Red else oldColor
//             Console.WriteLine(line)
//             Console.ResetColor()

//     proc.OutputDataReceived.Add(fun e ->
//         if e.Data <> null then
//             print false e.Data
//     )

//     proc.ErrorDataReceived.Add(fun e ->
//         if e.Data <> null then
//             print true e.Data
//     )

//     // Ensure we kill the full process tree on cancellation (with a short grace period), with a Windows fallback
//     // use _killReg =
//     //     ct.Register(fun () ->
//     //         // Always log the intent so both client and server show a line
//     //         print true $"Killing {prefix} (pid {proc.Id})..."

//     //         if not proc.HasExited then

//     //             if not proc.HasExited then
//     //                 try
//     //                     proc.Kill(entireProcessTree = true)
//     //                     proc.WaitForExit(5000) |> ignore
//     //                 with _ ->
//     //                     ()

//     //             if not proc.HasExited && OperatingSystem.IsWindows() then
//     //                 try
//     //                     let tk = ProcessStartInfo()
//     //                     tk.FileName <- "taskkill"
//     //                     tk.Arguments <- $"/PID {proc.Id} /T /F"
//     //                     tk.UseShellExecute <- false
//     //                     tk.CreateNoWindow <- true
//     //                     use p = Process.Start(tk)

//     //                     if not (isNull p) then
//     //                         p.WaitForExit(5000) |> ignore
//     //                 with _ ->
//     //                     ()
//     //     )

//     try
//         try
//             if not (proc.Start()) then
//                 failwithf "Failed to start %s" cmd

//             proc.BeginOutputReadLine()
//             proc.BeginErrorReadLine()

//             do! proc.WaitForExitAsync(ct) |> Async.AwaitTask

//             if proc.ExitCode = 0 then
//                 return Ok()
//             else
//                 print true $"Exited with code {proc.ExitCode}"
//                 return Error(exn (sprintf "Process exited with code %d" proc.ExitCode))
//         with ex ->
//             if not ct.IsCancellationRequested then
//                 print true $"Exception: {ex.Message}"

//             return Error(ex)
//     finally
//         Console.ResetColor()
// }


let runParallel (tasks: Async<Result<unit, exn>> list) =
    let cts = new CancellationTokenSource()
    Console.ResetColor()

    Console.CancelKeyPress.Add(fun e ->
        e.Cancel <- true
        cts.Cancel()
    )

    async {
        // Print immediately when cancellation is requested
        use! onCancel =
            Async.OnCancel(fun () ->
                printRedfn "Ctrl+C pressed → cancelling all tasks..."
                Console.ResetColor()
            )

        let! results = tasks |> Async.Parallel

        let errors =
            results
            |> Array.choose (
                function
                | Error e -> Some e
                | Ok _ -> None
            )

        if errors.Length > 0 then
            printRedfn "%d task(s) failed" errors.Length
            errors |> Array.iter (fun e -> printRedfn " - %s" e.Message)
            exit 1
        else
            printGreenfn "All tasks completed successfully"
    }
    |> fun c ->
        try
            Async.RunSynchronously(c, cancellationToken = cts.Token)
        with :? OperationCanceledException ->
            // Swallow here; each child has already handled its own cancellation and printed
            ()


let getEnvironementVariableOrFail (name: string) =
    let value = Environment.GetEnvironmentVariable(name)

    if String.IsNullOrWhiteSpace value then
        failwithf "Environment variable %s is not set or empty" name
        exit 1
    else
        value


/// .NET starts processes without probing PATHEXT, so the npm shims need their extension
/// on Windows. Commands that Fable launches through `--run` are spelled plainly instead.
let npx = if OperatingSystem.IsWindows() then "npx.cmd" else "npx"

/// Runs a command that is allowed to fail and returns its exit code with the captured
/// output, for probes whose result is an answer rather than an error.
let runReadResult (cmd: string) (args: seq<string>) (workingDir: string) =
    let mutable exitCode = 0

    let standardOutput, standardError =
        Command.ReadAsync(
            cmd,
            args = args,
            workingDirectory = workingDir,
            handleExitCode =
                (fun code ->
                    exitCode <- code
                    true
                )
        )
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> _.ToTuple()

    exitCode, standardOutput, standardError

/// Runs a command, echoing every line as it arrives and returning the exit code together
/// with everything that was printed, so a caller can also assert on the output.
let runCaptured (prefix: string) (cmd: string) (args: seq<string>) (workingDir: string) =
    let startInfo = ProcessStartInfo(FileName = cmd, WorkingDirectory = workingDir)
    startInfo.UseShellExecute <- false
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true

    for arg in args do
        startInfo.ArgumentList.Add arg

    use proc = new Process()
    proc.StartInfo <- startInfo

    let captured = Text.StringBuilder()

    let onLine (e: DataReceivedEventArgs) =
        if not (isNull e.Data) then
            lock captured (fun () -> captured.AppendLine e.Data |> ignore)
            Console.WriteLine $"[{prefix}] {e.Data}"

    proc.OutputDataReceived.Add onLine
    proc.ErrorDataReceived.Add onLine

    Console.WriteLine $"""[{prefix}] {cmd} {String.Join(" ", args)}"""

    if not (proc.Start()) then
        failwithf "Failed to start %s" cmd

    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
    proc.WaitForExit()

    proc.ExitCode, captured.ToString()

/// Reads a `--name=value` argument. Targets are matched case-insensitively, but the value
/// keeps its case because paths, versions and test filters depend on it.
let tryFlagValue (name: string) (args: string list) =
    let prefix = name + "="

    args
    |> List.tryPick (fun arg ->
        if arg.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
            Some(arg.Substring prefix.Length)
        else
            None
    )

let flagValue (name: string) (args: string list) =
    match tryFlagValue name args with
    | Some value -> value
    | None -> failwithf "Missing required argument %s=<value>." name
