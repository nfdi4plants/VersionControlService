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
module NodeOutput = VersionControlService.Runtime.Node.OutputReporter
module NodeInterop = VersionControlService.Runtime.Node.Interop
module GitExecution = VersionControlService.Git.GitExecution

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
        let completedTotal = NodeProcess.progressMeterCompletedTotal meter

        progressCallback
        |> Option.iter (fun report ->
            createProgressDto
                None
                None
                (completedTotal |> Option.map fst)
                (completedTotal |> Option.map fst)
                (completedTotal |> Option.map snd)
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

/// Attaches an output handler that reads the meter lines of both streams for the progress callback. When the
/// operation has an output channel, each command also reports a header and its stderr lines there. The
/// stdout of a simple-git command is data, so it never reaches the channel. `secretArguments` are
/// authentication arguments that the instance applies itself, in case the handler does not see them.
let internal withGitOutput
    (progressCallback: GitProgressCallback option)
    (outputTarget: NodeOutput.OutputTarget option)
    (secretArguments: string[])
    (git: ISimpleGit)
    =
    match progressCallback, outputTarget with
    | None, None -> git
    | _ ->
        attachOutputHandler
            git
            (fun (_command: string) (stdout: obj) (stderr: obj) (args: string[]) ->
                let commandArguments = if isNull (box args) then [||] else args

                let commandOutput =
                    outputTarget
                    |> Option.map (fun target ->
                        NodeOutput.beginCommand
                            target.Channel
                            "git"
                            commandArguments
                            (Array.append secretArguments commandArguments)
                            [||]
                            false)

                // The command ends when both streams have ended.
                let openStreams = ref 0

                let observeStream (stream: obj) (observeLog: NodeOutput.CommandOutput -> string -> unit) =
                    if hasStreamListener stream then
                        openStreams.Value <- openStreams.Value + 1
                        let observeMeters, flushMeters =
                            if progressCallback.IsSome then
                                createOutputObserver progressCallback
                            else
                                ignore, ignore

                        let decoder = NodeInterop.createUtf8StringDecoder ()
                        let ended = ref false

                        let observe (text: string) =
                            observeMeters text
                            commandOutput |> Option.iter (fun output -> observeLog output text)

                        let finish () =
                            if not ended.Value then
                                ended.Value <- true
                                let tail = NodeInterop.finishUtf8Decoding decoder

                                if not (String.IsNullOrEmpty tail) then
                                    observe tail

                                flushMeters ()
                                openStreams.Value <- openStreams.Value - 1

                                if openStreams.Value = 0 then
                                    commandOutput |> Option.iter (fun output -> output.Finish())

                        stream?on ("data", fun (chunk: obj) -> observe (NodeInterop.decodeUtf8Chunk decoder chunk))
                        |> ignore

                        stream?on ("end", fun (_: obj) -> finish ()) |> ignore
                        stream?on ("close", fun (_: obj) -> finish ()) |> ignore

                observeStream stdout (fun output text -> output.ObserveStdout text)
                observeStream stderr (fun output text -> output.ObserveStderr text)

                // A command whose streams cannot be observed would otherwise leave its header waiting.
                if openStreams.Value = 0 then
                    commandOutput |> Option.iter (fun output -> output.Finish())
            )

let internal withGitOutputProgress (progressCallback: GitProgressCallback option) (git: ISimpleGit) =
    withGitOutput progressCallback None [||] git

// `internal` stays inside the library assembly boundary.
// A host-selected path can contain spaces, which simple-git refuses in a custom binary unless the
// option allows them. The bare name "git" needs no exception.
let internal unsafeOptionsFor (invocation: GitExecution.GitInvocation) =
    SimpleGitUnsafeOptions(
        allowUnsafeCustomBinary = invocation.Explicit,
        allowUnsafeProtocolOverride = false,
        allowUnsafePack = false
    )

let internal standardTimeout =
    SimpleGitTimeoutOptions(block = 30000, stdOut = true, stdErr = true)

let internal syncTimeout =
    SimpleGitTimeoutOptions(block = 120000, stdOut = true, stdErr = true)

/// Creates the standard simple-git options used by Git services.
/// maxConcurrentProcesses is intentionally one to serialize commands for a repository-scoped instance.
/// The instance runs the executable of `invocation`.
let internal createOptions
    (invocation: GitExecution.GitInvocation)
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
            binary = U3.Case1 invocation.Executable,
            maxConcurrentProcesses = 1,
            timeout = timeout,
            ``unsafe`` = unsafeOptionsFor invocation,
            progress = handler
        )
    | None ->
        SimpleGitOptions(
            baseDir = baseDir,
            binary = U3.Case1 invocation.Executable,
            maxConcurrentProcesses = 1,
            timeout = timeout,
            ``unsafe`` = unsafeOptionsFor invocation
        )

let internal createUntimedOptions
    (invocation: GitExecution.GitInvocation)
    (baseDir: string)
    (progressCallback: GitProgressCallback option)
    (abortSignal: obj option)
    =
    // Optional constructor arguments become plain keys of the options object. The generated
    // property setters would write to a different key that simple-git never reads.
    SimpleGitOptions(
        baseDir = baseDir,
        binary = U3.Case1 invocation.Executable,
        maxConcurrentProcesses = 1,
        ``unsafe`` = unsafeOptionsFor invocation,
        ?progress = (progressCallback |> Option.map (fun progress -> progressFromSimpleGit >> progress)),
        ?abort = (abortSignal |> Option.map unbox<IAbortSignal>)
    )

/// Creates a simple-git instance with non-interactive prompt suppression applied.
/// `invocation` is the one that built the options, so the environment matches the binary.
let internal createGit (invocation: GitExecution.GitInvocation) (options: SimpleGitOptions) : ISimpleGit =
    SimpleGit.create options |> applyNonInteractiveEnv invocation

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
