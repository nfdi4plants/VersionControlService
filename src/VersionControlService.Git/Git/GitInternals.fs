module internal VersionControlService.Git.GitInternals

open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Git.GitEngineTypes
open VersionControlService.Bindings.SimpleGit
open VersionControlService.Git.GitAuthAdapter

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeProcess = VersionControlService.Runtime.Node.Process

type GitProgressCallback = GitProgressDto -> unit

let private indexLockFailurePattern = Regex(@"Unable to create '(.*index\.lock)': File exists", RegexOptions.Singleline)

[<Emit("Date.now()")>]
let private nowMilliseconds () : float = jsNative

let internal tryFileAgeSeconds (path: string) : float option =
    try
        let modifiedAtMilliseconds = (NodeFileSystem.statSync path).mtimeMs
        Some(max 0.0 ((nowMilliseconds () - modifiedAtMilliseconds) / 1000.0))
    with _ ->
        None

/// Finds Git's index lock diagnostic and reads the lock's age when the path is accessible.
let internal tryIndexLockFailure (output: string) : (string * float option) option =
    let output = output |> Option.ofObj |> Option.defaultValue String.Empty
    let matchResult = indexLockFailurePattern.Match output

    if matchResult.Success then
        let path = matchResult.Groups.[1].Value
        Some(path, tryFileAgeSeconds path)
    else
        None

let internal indexLockFailure (path: string) (ageSeconds: float option) : OperationFailure =
    let details =
        match ageSeconds with
        | Some age -> [| $"Lock file: {path}"; $"Lock file age: {int age} s" |]
        | None -> [| $"Lock file: {path}" |]

    let failure =
        OperationFailure.createRedacted
            Concurrency
            "index_locked"
            "Another git process holds the repository index, or a previous one left its lock behind."
        |> OperationFailure.withDetails details

    {
        failure with
            Retryable = true
            AffectedPaths = [||]
            StateChanged = false
            RecoveryAction =
                Some {
                    Code = "remove_index_lock"
                    Instructions =
                        Some "Make sure no git process is running on the repository, remove the lock file, then retry."
                }
    }

let internal createProgressDto methodName stage progress processed total output : GitProgressDto = {
    Method = methodName
    Stage = stage
    Progress = progress
    Processed = processed
    Total = total
    Output = output
}

let internal progressFromSimpleGit (progressEvent: SimpleGitProgressEvent) =
    createProgressDto
        (Some progressEvent.method)
        (Some progressEvent.stage)
        (Some progressEvent.progress)
        (Some progressEvent.processed)
        (Some progressEvent.total)
        None

let internal reportPhase (progressCallback: GitProgressCallback option) methodName stage =
    progressCallback
    |> Option.iter (fun report -> createProgressDto (Some methodName) (Some stage) None None None None |> report)

let internal reportOutputText (progressCallback: GitProgressCallback option) (text: string) =
    match NodeProcess.tryParseProgressMeter text with
    | Some meter ->
        let output = NodeProcess.formatProgressMeter meter |> Redaction.redact

        progressCallback
        |> Option.iter (fun report ->
            createProgressDto
                None
                None
                (Some meter.Percent)
                (Some meter.Percent)
                (Some 100.0)
                (Some output)
            |> report)
    | None -> ()

let private createOutputObserver (progressCallback: GitProgressCallback option) =
    NodeProcess.createMeterObserver (reportOutputText progressCallback)

[<Emit("$0 != null && typeof $0.on === 'function'")>]
let private hasStreamListener (_stream: obj) : bool = jsNative

[<Emit("$0.outputHandler(function(command, stdout, stderr, args) { $1(command, stdout, stderr, args); })")>]
let private attachOutputHandler (_git: ISimpleGit) (_handler: string -> obj -> obj -> string[] -> unit) : ISimpleGit =
    jsNative

let internal withGitOutputProgress (progressCallback: GitProgressCallback option) (git: ISimpleGit) =
    match progressCallback with
    | None -> git
    | Some _ ->
        attachOutputHandler
            git
            (fun (_command: string) (stdout: obj) (stderr: obj) (_args: string[]) ->
                let observeStream (stream: obj) =
                    if hasStreamListener stream then
                        let observe, flush = createOutputObserver progressCallback
                        stream?on ("data", fun (chunk: obj) -> observe (string chunk)) |> ignore
                        stream?on ("end", fun (_: obj) -> flush ()) |> ignore

                observeStream stdout
                observeStream stderr
            )

// `internal` stays inside the library assembly boundary.
let internal unsafeOptions =
    SimpleGitUnsafeOptions(
        allowUnsafeCustomBinary = false,
        allowUnsafeProtocolOverride = false,
        allowUnsafePack = false
    )

let internal standardTimeout =
    SimpleGitTimeoutOptions(block = 30000, stdOut = true, stdErr = true)

let internal syncTimeout =
    SimpleGitTimeoutOptions(block = 120000, stdOut = true, stdErr = true)

/// Creates the standard simple-git options used by Git services.
/// maxConcurrentProcesses is intentionally one to serialize commands for a repository-scoped instance.
let internal createOptions
    (baseDir: string)
    (timeout: SimpleGitTimeoutOptions)
    (progressCallback: GitProgressCallback option)
    =
    let progressHandler =
        progressCallback
        |> Option.map (fun progress -> progressFromSimpleGit >> progress)

    match progressHandler with
    | Some handler ->
        SimpleGitOptions(
            baseDir = baseDir,
            binary = U3.Case1 "git",
            maxConcurrentProcesses = 1,
            timeout = timeout,
            ``unsafe`` = unsafeOptions,
            progress = handler
        )
    | None ->
        SimpleGitOptions(
            baseDir = baseDir,
            binary = U3.Case1 "git",
            maxConcurrentProcesses = 1,
            timeout = timeout,
            ``unsafe`` = unsafeOptions
        )

/// Creates a simple-git instance with non-interactive prompt suppression applied.
let internal createGit (options: SimpleGitOptions) : ISimpleGit =
    SimpleGit.create options |> applyNonInteractiveEnv

/// Converts exceptions from simple-git/spawned Git into the service-specific failure record.
/// Messages are redacted here before they leave the service boundary.
let internal toFailure
    (classifyFailureKind: string -> 'GitFailureKind)
    (createFailure: 'GitFailureKind -> string -> 'GitFailure)
    (error: exn)
    : 'GitFailure =
    let message =
        error.Message
        |> Option.ofObj
        |> Option.filter (fun m -> not (String.IsNullOrWhiteSpace m))
        |> Option.defaultValue (string error)
        |> redactToken

    createFailure (classifyFailureKind message) message

/// Convenience wrapper for returning a redacted, classified failure as Result.Error.
let internal errorResult
    (classifyFailureKind: string -> 'GitFailureKind)
    (createFailure: 'GitFailureKind -> string -> 'GitFailure)
    (error: exn)
    : Result<'T, 'GitFailure> =
    Error(toFailure classifyFailureKind createFailure error)

/// Runs a simple-git operation and maps thrown exceptions into the caller's GitResult shape.
/// Services use this boundary so validation and command failures are reported consistently.
let internal runSimpleGit
    (toFailure: exn -> 'GitFailure)
    (operation: ISimpleGit -> JS.Promise<'T>)
    (git: ISimpleGit)
    : JS.Promise<Result<'T, 'GitFailure>> =
    promise {
        try
            let! result = operation git
            return Ok result
        with error ->
            return Error(toFailure error)
    }
