module VersionControlService.Git.GitLfsAdapter

open System
open Fable.Core.JsInterop
open Fable.Core.JS
open VersionControlService.Contracts.Git
open VersionControlService.Runtime.Node.Interop
open VersionControlService.Git.GitAuthAdapter

[<Literal>]
let private repoValidationTimeoutMs = 5000

[<Literal>]
let private terminationGraceMs = 250

[<Literal>]
let private terminationSettlementMs = 1000

let private isWindows () = processPlatform () = "win32"

let private signalProcessTree (proc: obj) (signal: string) =
    let pid: int = proc?pid |> unbox

    if isWindows () then
        childProcessDynamic?spawn (
            "taskkill",
            [| "/pid"; string pid; "/T"; "/F" |],
            createObj [ "windowsHide" ==> true; "stdio" ==> "ignore" ]
        )
        |> ignore
    else
        try
            let signalName = signal.Replace("SIG", "")

            childProcessDynamic?execFileSync (
                "kill",
                [| $"-{signalName}"; $"-{pid}" |],
                createObj [ "stdio" ==> "ignore" ]
            )
            |> ignore
        with _ ->
            try
                proc?kill (signal) |> ignore
            with _ ->
                ()

/// Low-level spawned `git` request used when simple-git cannot stream or feed stdin in the shape needed by LFS planning.
type GitSpawnRequest = {
    WorkingDirectory: string option
    Arguments: string[]
    Environment: obj option
    StandardInput: string option
    CancelCheck: (unit -> bool) option
    TimeoutMs: int option
}

/// Captured process result for spawned git commands, including raw stdout for binary-safe batch parsing.
type GitSpawnResult = {
    ExitCode: int
    StdoutBuffer: obj
    StdoutText: string
    StderrText: string
    TimedOut: bool
}

let private resolveGitEnvironment (environment: obj option) =
    environment
    |> Option.defaultWith createNonInteractiveEnv
    |> GitCommandResolver.ensureGitToolPath

let private runGitProcess
    (captureStdout: bool)
    (onStarted: unit -> unit)
    (onOutput: string -> unit)
    (request: GitSpawnRequest)
    : Promise<GitSpawnResult> =
    promise {
    let! result =
        Fable.Core.JS.Constructors.Promise.Create(fun resolve _ ->
            let spawnOptions =
                createObj [
                    "shell" ==> false
                    "windowsHide" ==> true
                    "env" ==> resolveGitEnvironment request.Environment
                    "detached" ==> not (isWindows ())

                    match request.WorkingDirectory with
                    | Some value -> "cwd" ==> value
                    | None -> ()
                ]

            let proc: obj = childProcessDynamic?spawn ("git", request.Arguments, spawnOptions)
            let stdoutChunks = ResizeArray<obj>()
            let stderrChunks = ResizeArray<string>()
            let mutable finished = false
            let mutable timedOut = false
            let mutable cancelRequested = false
            let mutable timeoutId: int option = None
            let mutable cancelInterval: int option = None
            let mutable escalationId: int option = None
            let mutable settlementId: int option = None

            let clearTimer (timer: int option) = timer |> Option.iter Fable.Core.JS.clearTimeout

            let clearTimers () =
                clearTimer timeoutId
                cancelInterval |> Option.iter Fable.Core.JS.clearInterval
                clearTimer escalationId
                clearTimer settlementId
                timeoutId <- None
                cancelInterval <- None
                escalationId <- None
                settlementId <- None

            let clearIdleTimer () =
                clearTimer timeoutId
                timeoutId <- None

            let finish exitCode =
                if not finished then
                    finished <- true
                    clearTimers ()
                    let effectiveExitCode = if cancelRequested && exitCode = 0 then -1 else exitCode

                    let stdoutBuffer =
                        if captureStdout then
                            bufferConcat (stdoutChunks.ToArray())
                        else
                            bufferConcat [||]

                    resolve {
                        ExitCode = effectiveExitCode
                        StdoutBuffer = stdoutBuffer
                        StdoutText = bufferToUtf8String stdoutBuffer
                        StderrText = String.Concat(stderrChunks.ToArray())
                        TimedOut = timedOut
                    }

            let resetIdleTimer () =
                clearIdleTimer ()

                match request.TimeoutMs with
                | Some timeoutMs when timeoutMs > 0 ->
                    timeoutId <-
                        Some(
                            Fable.Core.JS.setTimeout
                                (fun () ->
                                    if not finished then
                                        timedOut <- true
                                        stderrChunks.Add $"Git command timed out after no output for {timeoutMs} ms."
                                        signalProcessTree proc "SIGTERM"

                                        if not (isWindows ()) then
                                            escalationId <-
                                                Some(
                                                    Fable.Core.JS.setTimeout
                                                        (fun () ->
                                                            if not finished then
                                                                signalProcessTree proc "SIGKILL")
                                                        terminationGraceMs
                                                )

                                        settlementId <-
                                            Some(Fable.Core.JS.setTimeout (fun () -> finish -1) terminationSettlementMs)
                                )
                                timeoutMs
                        )
                | _ -> ()

            proc?stdout?on (
                "data",
                fun d ->
                    resetIdleTimer ()
                    let text = d?toString ("utf8") |> unbox<string>
                    onOutput text

                    if captureStdout then
                        stdoutChunks.Add d
            )
            |> ignore

            proc?stderr?on (
                "data",
                fun d ->
                    resetIdleTimer ()
                    let text = d?toString ("utf8") |> unbox<string>
                    stderrChunks.Add text
                    onOutput text
            )
            |> ignore

            resetIdleTimer ()

            let requestCancellation () =
                if not finished && not cancelRequested then
                    cancelRequested <- true
                    stderrChunks.Add "Git command cancelled."
                    signalProcessTree proc "SIGTERM"

                    if not (isWindows ()) then
                        escalationId <-
                            Some(
                                Fable.Core.JS.setTimeout
                                    (fun () ->
                                        if not finished then
                                            signalProcessTree proc "SIGKILL")
                                    terminationGraceMs
                            )

                    settlementId <-
                        Some(Fable.Core.JS.setTimeout (fun () -> finish -1) terminationSettlementMs)

            cancelInterval <-
                request.CancelCheck
                |> Option.map (fun cancelCheck ->
                    Fable.Core.JS.setInterval
                        (fun () ->
                            if cancelCheck () then
                                requestCancellation ()
                        )
                        300
                )

            proc?on (
                "close",
                fun code ->
                    finish (if isNull code then -1 else int (unbox<float> code))
            )
            |> ignore

            proc?on (
                "error",
                fun error ->
                    stderrChunks.Add(
                        error
                        |> Option.ofObj
                        |> Option.map string
                        |> Option.defaultValue "Failed to start git process."
                    )

                    finish -1
            )
            |> ignore

            onStarted ()

            match request.CancelCheck with
            | Some cancelCheck when cancelCheck () -> requestCancellation ()
            | _ -> ()

            match request.StandardInput with
            | Some input ->
                proc?stdin?setDefaultEncoding ("utf8") |> ignore
                proc?stdin?``end`` (input) |> ignore
            | None -> proc?stdin?``end`` () |> ignore
        )

    return result
}

/// Runs `git` without a shell and captures stdout/stderr for callers that need exact output or stdin support.
let runGitCaptured (request: GitSpawnRequest) : Promise<GitSpawnResult> = runGitProcess true ignore ignore request

/// Runs `git` with a notification after the child process and its cancellation handlers are active.
let runGitCapturedWithStarted
    (onStarted: unit -> unit)
    (request: GitSpawnRequest)
    : Promise<GitSpawnResult> =
    runGitProcess true onStarted ignore request

/// Assembly-internal streaming variant used by maintenance operations so
/// provider progress originates from the real child process.
let internal runGitCapturedWithStartedAndOutput
    (onStarted: unit -> unit)
    (onOutput: string -> unit)
    (request: GitSpawnRequest)
    : Promise<GitSpawnResult> =
    runGitProcess true onStarted onOutput request

/// Runs `git` while draining and discarding stdout.
/// This is used for commands such as `git lfs smudge`, whose stdout may contain a large file.
let runGitDiscardingStdout (request: GitSpawnRequest) : Promise<GitSpawnResult> = runGitProcess false ignore ignore request

/// Runs a small git command and returns stdout text, or None on command failure.
/// Used for feature probes where failure should not surface as a user-facing Git error.
let tryExecGitText (workingDirectory: string option) (timeoutMs: int) (args: string[]) : Promise<string option> = promise {
    let! result =
        runGitCaptured {
            WorkingDirectory = workingDirectory
            Arguments = args
            Environment = None
            StandardInput = None
            CancelCheck = None
            TimeoutMs = Some timeoutMs
        }

    if result.ExitCode = 0 && not result.TimedOut then
        return Some result.StdoutText
    else
        return None
}


/// Adapter contract for Git LFS commands. Services depend on this shape instead of direct child-process calls.
type IGitLfs =
    abstract Run:
        request: GitLfsRequest -> onProgress: (string -> unit) -> cancel: (unit -> bool) -> Promise<GitLfsResult>

    abstract Install:
        timeoutMs: int option -> onProgress: (string -> unit) -> cancel: (unit -> bool) -> Promise<GitLfsResult>

    abstract IsTrackedByAttributes: repoRoot: string -> relativePath: string -> bool


type NodeGitLfsAdapter() =
    let activeLockKeys = System.Collections.Generic.HashSet<string>()

    let toArgs (request: GitLfsRequest) =
        match request.Command, request.FilePath with
        | Pull, None -> Ok [ "lfs"; "pull" ]
        | Pull, Some file -> Ok [ "lfs"; "pull"; "--include"; file ]
        | Fetch, None -> Ok [ "lfs"; "fetch" ]
        | Fetch, Some file -> Ok [ "lfs"; "fetch"; "--include"; file ]
        | Install, _ -> Ok [ "lfs"; "install" ]
        | Track, Some file -> Ok [ "lfs"; "track"; "--"; file ]
        | Untrack, Some file -> Ok [ "lfs"; "untrack"; "--"; file ]
        | Status, Some file -> Ok [ "lfs"; "ls-files"; "--name-only"; "--"; file ]
        | Track, None
        | Untrack, None
        | Status, None -> Error "FilePath is required for this Git LFS command"

    let normalizeLockKey (workingDirectory: string option) =
        workingDirectory
        |> Option.defaultValue "__system__"
        |> _.Trim()
        |> _.ToLowerInvariant()

    let validateRepoPath (repoPath: string) : Promise<bool> = promise {
        let! output = tryExecGitText (Some repoPath) repoValidationTimeoutMs [| "rev-parse"; "--is-inside-work-tree" |]

        return
            output
            |> Option.exists (fun text -> text.Trim().Equals("true", System.StringComparison.OrdinalIgnoreCase))
    }

    let validateRepoPathSync (repoPath: string) =
        try
            let output: string =
                childProcessDynamic?execFileSync (
                    "git",
                    [| "rev-parse"; "--is-inside-work-tree" |],
                    createObj [
                        "cwd" ==> repoPath
                        "encoding" ==> "utf8"
                        "stdio" ==> "pipe"
                        "shell" ==> false
                        "env" ==> createNonInteractiveEnv ()
                    ]
                )
                |> unbox<string>

            output.Trim().Equals("true", System.StringComparison.OrdinalIgnoreCase)
        with _ ->
            false

    let runProcess
        (args: string list)
        (workingDirectory: string option)
        (timeoutMs: int option)
        (onProgress: string -> unit)
        (cancelCheck: unit -> bool)
        : Promise<GitLfsResult> =
        promise {
            let lockKey = normalizeLockKey workingDirectory

            if activeLockKeys.Contains lockKey then
                return {
                    Success = false
                    Output = ""
                    Error = "Another Git LFS process is running for this repository."
                }
            else
                activeLockKeys.Add lockKey |> ignore

                try
                    let! result =
                        runGitProcess
                            true
                            ignore
                            onProgress
                            {
                                WorkingDirectory = workingDirectory
                                Arguments = args |> List.toArray
                                Environment = None
                                StandardInput = None
                                CancelCheck = Some cancelCheck
                                TimeoutMs = timeoutMs
                            }

                    return {
                        Success = result.ExitCode = 0 && not result.TimedOut
                        Output = result.StdoutText
                        Error = result.StderrText
                    }
                finally
                    activeLockKeys.Remove lockKey |> ignore
        }

    let runGitLfs
        (request: GitLfsRequest)
        (onProgress: string -> unit)
        (cancelCheck: unit -> bool)
        : Promise<GitLfsResult> =
        promise {
            let! isValidRepo = validateRepoPath request.RepoPath

            if not isValidRepo then
                return {
                    Success = false
                    Output = ""
                    Error = "Not a git repository"
                }
            else
                match toArgs request with
                | Error err ->
                    return {
                        Success = false
                        Output = ""
                        Error = err
                    }
                | Ok args ->
                    let! (result: GitLfsResult) =
                        runProcess args (Some request.RepoPath) request.TimeoutMs onProgress cancelCheck

                    if not result.Success then
                        return result
                    else
                        match request.Command, request.FilePath with
                        | Pull, Some file
                        | Fetch, Some file ->
                            return!
                                runProcess
                                    [ "checkout"; "--"; file ]
                                    (Some request.RepoPath)
                                    request.TimeoutMs
                                    onProgress
                                    cancelCheck
                        | _ -> return result
        }

    let installGitLfs
        (timeoutMs: int option)
        (onProgress: string -> unit)
        (cancelCheck: unit -> bool)
        : Promise<GitLfsResult> =
        promise { return! runProcess [ "lfs"; "install" ] None timeoutMs onProgress cancelCheck }

    let isTrackedByAttributes (repoRoot: string) (relativePath: string) =
        if
            not (validateRepoPathSync repoRoot)
            || System.String.IsNullOrWhiteSpace relativePath
        then
            false
        else
            try
                let output: string =
                    childProcessDynamic?execFileSync (
                        "git",
                        [| "check-attr"; "filter"; "--"; relativePath |],
                        createObj [
                            "cwd" ==> repoRoot
                            "encoding" ==> "utf8"
                            "stdio" ==> "pipe"
                            "shell" ==> false
                            "env" ==> createNonInteractiveEnv ()
                        ]
                    )
                    |> unbox<string>

                output.Contains(": filter: lfs")
            with _ ->
                false

    interface IGitLfs with
        member _.Run request onProgress cancel = runGitLfs request onProgress cancel

        member _.Install timeoutMs onProgress cancel =
            installGitLfs timeoutMs onProgress cancel

        member _.IsTrackedByAttributes repoRoot relativePath =
            isTrackedByAttributes repoRoot relativePath



/// Factory for the default Node-backed Git LFS adapter.
let gitLfsAdapter () : IGitLfs = NodeGitLfsAdapter() :> IGitLfs

/// Process-wide adapter instance used by GitLfsService.
let gitLfs = gitLfsAdapter ()
