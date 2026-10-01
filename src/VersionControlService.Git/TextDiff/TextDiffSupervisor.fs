module VersionControlService.Git.TextDiff.TextDiffSupervisor

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.JS
open VersionControlService.Git
open VersionControlService.Runtime.Node

module NodePath = VersionControlService.Runtime.Node.Path
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads
module GitExecution = VersionControlService.Git.GitExecution

type ChildOwner = {
    WorkerId: string
    SessionId: string
    RequestId: string
}

type TextDiffSupervisorOptions = {
    TempRoot: string
    GitExecutable: string option
    OnEvent: (SupervisorEvent -> unit) option
}

and SupervisorEventKind =
    | ChildSpawned of int
    | ChildClosed of int
    | SpoolDeleted of string
    | WorkerDirectoryDeleted of string

and SupervisorEvent = {
    Kind: SupervisorEventKind
    Timestamp: float
}

type ShortResult = {
    ExitCode: int option
    Stdout: byte[]
    Stderr: string
    Error: string option
}

type SpoolChild = {
    Pid: int
    Closed: JS.Promise<NodeProcess.ChildExit>
}

module TextDiffSupervisorOptions =

    let defaults = {
        TempRoot = NodeWorkerThreads.tmpdir ()
        GitExecutable = None
        OnEvent = None
    }

[<AllowNullLiteral>]
type private NodeStat =
    abstract member isFile: unit -> bool

[<Import("access", "node:fs/promises")>]
let private accessAsync (path: string) (mode: int) : JS.Promise<unit> = jsNative

[<Import("stat", "node:fs/promises")>]
let private statAsync (path: string) : JS.Promise<NodeStat> = jsNative

[<Emit("process.pid")>]
let private processId () : int = jsNative

[<Emit("process.platform")>]
let private processPlatform () : string = jsNative

/// The id of the current user, or -1 where the platform has none (Windows).
[<Emit("(typeof process.getuid === 'function' ? process.getuid() : -1)")>]
let private currentUserId () : int = jsNative

[<Emit("performance.now()")>]
let private performanceNow () : float = jsNative

[<Emit("$0.PATH || $0.Path || $0.path || ''")>]
let private environmentPath (_environment: obj) : string = jsNative

[<Emit("Object.assign({}, $0, { GIT_NO_LAZY_FETCH: '1', GIT_ALLOW_PROTOCOL: '', GIT_TERMINAL_PROMPT: '0', GIT_OPTIONAL_LOCKS: '0', LC_ALL: 'C', LANG: 'C' })")>]
let private localOnlyEnvironment (_environment: obj) : obj = jsNative

let private isWindows () = processPlatform () = "win32"

let private isWrapperPath (path: string) =
    match NodePath.extname(path).ToLowerInvariant() with
    | ".cmd"
    | ".bat" -> true
    | _ -> false

let private isObjectId (value: string) =
    not (isNull value)
    && (value.Length = 40 || value.Length = 64)
    && (value
        |> Seq.forall (fun character ->
            (character >= '0' && character <= '9')
            || (character >= 'a' && character <= 'f')
            || (character >= 'A' && character <= 'F')))

let private isRefPattern (value: string) =
    not (String.IsNullOrEmpty value)
    && not (value.StartsWith("-", StringComparison.Ordinal))
    && not (value |> Seq.exists Char.IsWhiteSpace)

let private isCommitVerification (value: string) =
    let suffix = "^{commit}"

    not (String.IsNullOrEmpty value)
    && value.EndsWith(suffix, StringComparison.Ordinal)
    && value.Length > suffix.Length
    && not (value.StartsWith("-", StringComparison.Ordinal))
    && not (value |> Seq.exists Char.IsWhiteSpace)

let private isAllowedShortCommand (arguments: string[]) =
    match arguments with
    | [| "rev-parse"; "--absolute-git-dir" |]
    | [| "rev-parse"; "--git-common-dir" |]
    | [| "rev-parse"; "--git-dir" |]
    | [| "symbolic-ref"; "-q"; "HEAD" |] -> true
    | [| "rev-parse"; "--verify"; "--quiet"; expression |] -> isCommitVerification expression
    | [| "for-each-ref"; "--format=%(refname)"; "--"; refPattern |] -> isRefPattern refPattern
    | [| "ls-tree"; "-z"; "--full-tree"; oid; "--"; _path |] -> isObjectId oid
    | [| "cat-file"; "-s"; oid |]
    | [| "cat-file"; "blob"; oid |] -> isObjectId oid
    | _ -> false

let private checkRegularFile (path: string) : JS.Promise<bool> = promise {
    try
        do! accessAsync path 1
        let! stats = statAsync path
        return stats.isFile ()
    with _ ->
        return false
}

let private checkExecutable (path: string) : JS.Promise<bool> = promise {
    let! isFile = checkRegularFile path
    return isFile && not (isWrapperPath path)
}

let private resolveGitExecutable (configured: string option) : JS.Promise<string> = promise {
    match configured with
    | Some executable ->
        if not (NodePath.isAbsolute executable) then
            return raise (InvalidOperationException("GitExecutable must be an absolute path to a real Git executable."))
        elif isWrapperPath executable then
            return raise (InvalidOperationException("GitExecutable points to a .cmd or .bat wrapper. Configure the real Git executable."))
        else
            let! isExecutable = checkExecutable executable

            if isExecutable then
                return executable
            else
                return raise (InvalidOperationException($"GitExecutable is not an accessible file: {executable}"))
    | None ->
        let environment = GitExecution.resolvedEnvironment ()
        let separator = if isWindows () then ';' else ':'
        let directories = environmentPath environment
        let entries = directories.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)
        let mutable found: string option = None
        let mutable wrapperFound = false

        for directory in entries do
            if found.IsNone then
                let names = if isWindows () then [| "git.exe"; "git.cmd"; "git.bat" |] else [| "git" |]

                for name in names do
                    if found.IsNone then
                        let candidate = NodePath.resolve [| directory; name |]

                        if isWrapperPath candidate then
                            let! isAccessible = checkRegularFile candidate
                            wrapperFound <- wrapperFound || isAccessible
                        else
                            let! isExecutable = checkExecutable candidate

                            if isExecutable then
                                found <- Some candidate

        match found with
        | Some executable -> return executable
        | None when wrapperFound ->
            return raise (InvalidOperationException("Git was found only as a .cmd or .bat wrapper. Install Git with a real executable and make it available on PATH."))
        | None ->
            return raise (InvalidOperationException("A real Git executable was not found on PATH."))
}

let private tryGetInstanceOwnerPid (name: string) =
    match name.Split([| '-' |], StringSplitOptions.None) with
    | [| pidText; randomText |] when randomText.Length = 8 ->
        let isHex character =
            (character >= '0' && character <= '9')
            || (character >= 'a' && character <= 'f')
            || (character >= 'A' && character <= 'F')

        let mutable parsedPid = 0
        let mutable validPid = pidText.Length > 0

        for character in pidText do
            if character < '0' || character > '9' then
                validPid <- false
            else
                let digit = int character - int '0'

                if parsedPid > (Int32.MaxValue - digit) / 10 then
                    validPid <- false
                else
                    parsedPid <- parsedPid * 10 + digit

        let hasHexSuffix = randomText |> Seq.forall isHex

        if validPid && parsedPid > 0 && hasHexSuffix then Some parsedPid else None
    | _ -> None

let private isOwnerWithin (scope: ChildOwner -> bool) (owner: ChildOwner) = scope owner

type TextDiffSupervisor internal (instanceDirectory: string, gitExecutable: string, options: TextDiffSupervisorOptions) =
    let children = Dictionary<int, ChildOwner * JS.Promise<NodeProcess.ChildExit>>()
    let spoolOwners = Dictionary<string, ChildOwner>()
    let workerDirectories = HashSet<string>()
    let activeOperations = Dictionary<int, ChildOwner * JS.Promise<unit>>()
    let blockedRequests = HashSet<ChildOwner>()
    let blockedSessions = HashSet<string * string>()
    let blockedWorkers = HashSet<string>()
    let mutable disposed = false
    let mutable nextOperationId = 0

    let emit kind =
        options.OnEvent
        |> Option.iter (fun handler ->
            try
                handler { Kind = kind; Timestamp = performanceNow () }
            with _ -> ())

    let validateWorkerId (workerId: string) =
        if String.IsNullOrWhiteSpace workerId
           || workerId = "."
           || workerId = ".."
           || workerId.Contains "/"
           || workerId.Contains "\\"
           || NodePath.isAbsolute workerId then
            invalidArg (nameof workerId) "WorkerId must be a single non-empty path segment."

    let workerDirectoryPath workerId =
        validateWorkerId workerId
        NodePath.join [| instanceDirectory; workerId |]

    let requestBlocked (owner: ChildOwner) =
        disposed
        || blockedRequests.Contains owner
        || blockedSessions.Contains(owner.WorkerId, owner.SessionId)
        || blockedWorkers.Contains owner.WorkerId

    let ensureOwnerMaySpawn owner =
        validateWorkerId owner.WorkerId

        if requestBlocked owner then
            invalidOp "The worker request is being released and cannot start another Git child."

    let trackOperation (owner: ChildOwner) (operation: JS.Promise<'T>) : JS.Promise<'T> =
        let operationId = nextOperationId
        nextOperationId <- nextOperationId + 1

        let completion =
            JS.Constructors.Promise.Create(fun resolve _ ->
                NodeInterop.observePromise operation (fun _ -> resolve ()) (fun _ -> resolve ()))

        activeOperations[operationId] <- owner, completion
        NodeInterop.observePromise completion (fun () -> activeOperations.Remove operationId |> ignore) ignore
        operation

    let registerChild (owner: ChildOwner) (pid: int) (closed: JS.Promise<NodeProcess.ChildExit>) =
        children[pid] <- owner, closed
        emit (ChildSpawned pid)

        NodeInterop.observePromise
            closed
            (fun _ ->
                emit (ChildClosed pid)

                match children.TryGetValue pid with
                | true, (registeredOwner, registeredClosed) when registeredOwner = owner && obj.ReferenceEquals(registeredClosed, closed) ->
                    children.Remove pid |> ignore
                | _ -> ())
            ignore

    let registeredSpoolsWithin scope =
        spoolOwners
        |> Seq.choose (fun entry -> if isOwnerWithin scope entry.Value then Some entry.Key else None)
        |> Seq.toArray

    let trackedChildrenWithin scope =
        children
        |> Seq.choose (fun entry ->
            let owner, closed = entry.Value
            if isOwnerWithin scope owner then Some(entry.Key, closed) else None)
        |> Seq.toArray

    let pendingOperationsWithin scope =
        activeOperations
        |> Seq.choose (fun entry ->
            let owner, completion = entry.Value
            if isOwnerWithin scope owner then Some completion else None)
        |> Seq.toArray

    let waitForOperations (operations: JS.Promise<unit>[]) = promise {
        for operation in operations do
            let! () = operation
            ()
    }

    let killChildrenThenWait (childrenToStop: (int * JS.Promise<NodeProcess.ChildExit>)[]) = promise {
        for pid, _ in childrenToStop do
            do! NodeProcess.killProcessTreeAsync pid

        for _, closed in childrenToStop do
            let! _ = closed
            ()
    }

    let deleteOneSpool path = promise {
        do! NodePositionalFile.removeWithRetry path 5 40
        spoolOwners.Remove path |> ignore
        emit (SpoolDeleted path)
    }

    let deleteSpools (paths: string[]) = promise {
        for path in paths do
            do! deleteOneSpool path
    }

    let releaseScope mark scope deleteWorkerDirectory = promise {
        mark ()
        let operationSnapshot = pendingOperationsWithin scope
        do! waitForOperations operationSnapshot
        let childSnapshot = trackedChildrenWithin scope
        let spoolSnapshot = registeredSpoolsWithin scope
        do! killChildrenThenWait childSnapshot
        do! deleteSpools spoolSnapshot

        match deleteWorkerDirectory with
        | Some path ->
            do! NodePositionalFile.removeWithRetry path 5 40
            workerDirectories.Remove path |> ignore
            emit (WorkerDirectoryDeleted path)
        | None -> ()
    }

    member _.InstanceDirectory = instanceDirectory

    member _.GitExecutable = gitExecutable

    member _.WorkerDirectory(workerId: string) : JS.Promise<string> =
        let owner = { WorkerId = workerId; SessionId = ""; RequestId = "" }

        let operation = promise {
            ensureOwnerMaySpawn owner
            let directory = workerDirectoryPath workerId
            workerDirectories.Add directory |> ignore
            do! NodePositionalFile.mkdirRecursive directory

            if blockedWorkers.Contains workerId || disposed then
                return raise (InvalidOperationException("The worker is being released and cannot create a directory."))
            else
                return directory
        }

        trackOperation owner operation

    member _.RunShort(owner: ChildOwner, cwd: string, arguments: string[]) : JS.Promise<ShortResult> =
        // A release waits for the spawn step only. Once the child is registered the release kills it.
        let mutable markSpawned: unit -> unit = ignore
        let spawning = JS.Constructors.Promise.Create(fun resolve _ -> markSpawned <- fun () -> resolve ())

        let operation = promise {
            ensureOwnerMaySpawn owner

            let validCommand = isAllowedShortCommand arguments

            if not validCommand then
                return raise (InvalidOperationException("The Git command is not allowed for a text diff request."))
            else
                let gitArguments = Microsoft.FSharp.Collections.Array.concat [| [| "--literal-pathspecs"; "-c"; "protocol.allow=never" |]; arguments |]
                let environment = GitExecution.resolvedEnvironment () |> localOnlyEnvironment

                let! result =
                    NodeProcess.runBoundedWithLifecycle
                        gitExecutable
                        gitArguments
                        cwd
                        environment
                        65536
                        65536
                        (fun pid closed ->
                            registerChild owner pid closed
                            markSpawned ())

                return {
                    ExitCode = result.ExitCode
                    Stdout = result.Stdout
                    Stderr = result.Stderr
                    Error = result.Error
                }
        }

        NodeInterop.observePromise operation (fun _ -> markSpawned ()) (fun _ -> markSpawned ())
        trackOperation owner spawning |> ignore
        operation

    member this.StartBlobToSpool(owner: ChildOwner, cwd: string, oid: string, spoolPath: string) : JS.Promise<SpoolChild> =
        let operation = promise {
            ensureOwnerMaySpawn owner
            if not (isObjectId oid) then
                return raise (InvalidOperationException("A blob object id must contain 40 or 64 hexadecimal characters."))
            else
                let! workerDirectory = this.WorkerDirectory owner.WorkerId
                let targetPath = NodePath.resolve [| spoolPath |]
                let relativePath = NodePath.relative workerDirectory targetPath

                if String.IsNullOrEmpty relativePath
                   || NodePath.isAbsolute relativePath
                   || relativePath = ".."
                   || relativePath.StartsWith("../", StringComparison.Ordinal)
                   || relativePath.StartsWith("..\\", StringComparison.Ordinal) then
                    return raise (InvalidOperationException("The spool path must be inside the worker directory."))
                else
                    ensureOwnerMaySpawn owner
                    let! descriptor = NodePositionalFile.openCreateExclusive targetPath
                    let mutable descriptorTransferred = false

                    try
                        ensureOwnerMaySpawn owner
                        spoolOwners[targetPath] <- owner
                        let arguments = [| "--literal-pathspecs"; "-c"; "protocol.allow=never"; "cat-file"; "blob"; oid |]
                        let environment = GitExecution.resolvedEnvironment () |> localOnlyEnvironment
                        descriptorTransferred <- true

                        let! child =
                            NodeProcess.spawnToFileTracked
                                gitExecutable
                                arguments
                                cwd
                                environment
                                descriptor
                                (fun pid closed -> registerChild owner pid closed)

                        if child.Pid > 0 then
                            return { Pid = child.Pid; Closed = child.Closed }
                        else
                            let! _ = child.Closed
                            return raise (InvalidOperationException("Git could not start the blob command."))
                    with error ->
                        if not descriptorTransferred then
                            try
                                do! NodePositionalFile.close descriptor
                            with _ -> ()

                        do! deleteOneSpool targetPath
                        return raise error
        }

        trackOperation owner operation

    member _.ReleaseRequest(owner: ChildOwner) : JS.Promise<unit> =
        releaseScope
            (fun () -> blockedRequests.Add owner |> ignore)
            (fun candidate -> candidate = owner)
            None

    member _.ReleaseSession(workerId: string, sessionId: string) : JS.Promise<unit> =
        releaseScope
            (fun () -> blockedSessions.Add(workerId, sessionId) |> ignore)
            (fun candidate -> candidate.WorkerId = workerId && candidate.SessionId = sessionId)
            None

    member _.ReleaseWorker(workerId: string) : JS.Promise<unit> = promise {
        validateWorkerId workerId
        blockedWorkers.Add workerId |> ignore
        let directory = workerDirectoryPath workerId
        do! releaseScope ignore (fun candidate -> candidate.WorkerId = workerId) (Some directory)
    }

    member _.Dispose() : JS.Promise<unit> = promise {
        disposed <- true
        let operationSnapshot = pendingOperationsWithin (fun _ -> true)
        do! waitForOperations operationSnapshot
        let childSnapshot = trackedChildrenWithin (fun _ -> true)
        let spoolSnapshot = registeredSpoolsWithin (fun _ -> true)
        do! killChildrenThenWait childSnapshot
        do! deleteSpools spoolSnapshot

        for directory in workerDirectories |> Seq.toArray do
            do! NodePositionalFile.removeWithRetry directory 5 40
            workerDirectories.Remove directory |> ignore
            emit (WorkerDirectoryDeleted directory)

        do! NodePositionalFile.removeWithRetry instanceDirectory 5 40
    }

/// Fails when a folder that already exists in the temp root belongs to another user or is not a plain folder.
/// Another local user could plant such a folder and read the spooled content. A negative user id skips the check.
let checkFolderOwner (userId: int) (folder: string) (stats: NodePositionalFile.PositionalFileStats) : unit =
    if userId >= 0 then
        if stats.IsSymbolicLink || not stats.IsDirectory then
            failwith $"The text diff folder {folder} exists and is not a plain folder. Use a temp root that the current user owns."
        elif stats.Uid <> string userId then
            failwith $"The text diff folder {folder} belongs to user {stats.Uid} and the current user is {userId}. Use a temp root that the current user owns."

let create (options: TextDiffSupervisorOptions) : JS.Promise<TextDiffSupervisor> = promise {
    let! gitExecutable = resolveGitExecutable options.GitExecutable
    let parentDirectory = NodePath.join [| options.TempRoot; "text-diff" |]

    let! existing = promise {
        try
            let! stats = NodePositionalFile.lstat parentDirectory
            return Some stats
        with _ ->
            return None
    }

    existing |> Option.iter (checkFolderOwner (currentUserId ()) parentDirectory)
    do! NodePositionalFile.mkdirRecursive parentDirectory
    let! siblingNames = NodeFileSystem.readdirAsync parentDirectory

    for siblingName in siblingNames do
        match tryGetInstanceOwnerPid siblingName with
        | Some ownerPid ->
            let siblingPath = NodePath.join [| parentDirectory; siblingName |]

            try
                let! stats = NodePositionalFile.lstat siblingPath

                if stats.IsDirectory && not stats.IsSymbolicLink then
                    match NodeProcess.processExistence ownerPid with
                    | NodeProcess.Gone -> do! NodePositionalFile.removeWithRetry siblingPath 5 40
                    | NodeProcess.Alive
                    | NodeProcess.Unknown _ -> ()
            with _ -> ()
        | None -> ()

    let randomSuffix = NodeInterop.randomUuid().Replace("-", "").Substring(0, 8).ToLowerInvariant()
    let instanceName = $"{processId ()}-{randomSuffix}"
    let instanceDirectory = NodePath.join [| parentDirectory; instanceName |]
    do! NodePositionalFile.mkdirRecursive instanceDirectory
    return TextDiffSupervisor(instanceDirectory, gitExecutable, options)
}
