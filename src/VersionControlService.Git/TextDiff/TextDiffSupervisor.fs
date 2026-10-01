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

[<Emit("process.pid")>]
let private processId () : int = jsNative

/// The id of the current user, or -1 where the platform has none (Windows).
[<Emit("(typeof process.getuid === 'function' ? process.getuid() : -1)")>]
let private currentUserId () : int = jsNative

[<Emit("performance.now()")>]
let private performanceNow () : float = jsNative

[<Emit("Object.assign({}, $0, { GIT_NO_LAZY_FETCH: '1', GIT_ALLOW_PROTOCOL: '', GIT_TERMINAL_PROMPT: '0', GIT_OPTIONAL_LOCKS: '0', LC_ALL: 'C', LANG: 'C' })")>]
let private localOnlyEnvironment (_environment: obj) : obj = jsNative

/// A full Git object id in the lower case hex that Git prints, as a SHA-1 or SHA-256 id.
let internal isObjectId (value: string) =
    not (isNull value)
    && (value.Length = 40 || value.Length = 64)
    && value |> Seq.forall (fun character -> (character >= '0' && character <= '9') || (character >= 'a' && character <= 'f'))

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
    | [| "symbolic-ref"; "-q"; "HEAD" |] -> true
    | [| "rev-parse"; "--verify"; "--quiet"; expression |] -> isCommitVerification expression
    | [| "for-each-ref"; "--format=%(refname)"; "--"; refPattern |] -> isRefPattern refPattern
    | [| "ls-tree"; "-z"; "--full-tree"; oid; "--"; _path |] -> isObjectId oid
    | [| "cat-file"; "-s"; oid |]
    | [| "cat-file"; "blob"; oid |] -> isObjectId oid
    | _ -> false

let private tryGetInstanceOwnerPid (name: string) =
    match name.Split([| '-' |], StringSplitOptions.None) with
    | [| pidText; randomText |] when randomText.Length = 8 ->
        let isHex character =
            (character >= '0' && character <= '9')
            || (character >= 'a' && character <= 'f')
            || (character >= 'A' && character <= 'F')

        // Int32.TryParse accepts signs and white space, so the digits check stays.
        let hasDigitsOnly = pidText.Length > 0 && pidText |> Seq.forall (fun character -> character >= '0' && character <= '9')
        let hasHexSuffix = randomText |> Seq.forall isHex

        match Int32.TryParse pidText with
        | true, pid when hasDigitsOnly && pid > 0 && hasHexSuffix -> Some pid
        | _ -> None
    | _ -> None

/// The sessions and requests released on one worker. They stay recorded until their worker is released.
type internal ReleasedWorker = {
    Sessions: HashSet<string>
    Requests: HashSet<ChildOwner>
}

let private releasedOwnerMessage = "The worker request is being released and cannot start another Git child."

/// Removes a folder tree. Windows keeps handles on files that a killed child wrote, so rm retries a few times.
let private removeTree (path: string) : JS.Promise<unit> =
    NodeFileSystem.rmAsync path (NodeFileSystem.RmOptions(recursive = true, force = true, maxRetries = 4, retryDelay = 40))

type TextDiffSupervisor internal (instanceDirectory: string, gitExecutable: string, options: TextDiffSupervisorOptions) =
    let children = Dictionary<int, ChildOwner * JS.Promise<NodeProcess.ChildExit>>()
    let spoolOwners = Dictionary<string, ChildOwner>()
    let workerDirectories = HashSet<string>()
    let released = Dictionary<string, ReleasedWorker>()
    // The set holds one id per released worker, so a start that resumes after ReleaseWorker is still refused.
    // The pool never reuses a worker id.
    let releasedWorkers = HashSet<string>()
    let mutable disposed = false

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

    let releasedEntry workerId =
        match released.TryGetValue workerId with
        | true, entry -> entry
        | _ ->
            let entry = { Sessions = HashSet<string>(); Requests = HashSet<ChildOwner>() }
            released[workerId] <- entry
            entry

    /// True when the owner's request, session or worker was released, or the supervisor was disposed.
    let ownerReleased (owner: ChildOwner) : bool =
        disposed
        || releasedWorkers.Contains owner.WorkerId
        || (match released.TryGetValue owner.WorkerId with
            | true, entry -> entry.Sessions.Contains owner.SessionId || entry.Requests.Contains owner
            | _ -> false)

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
        |> Seq.choose (fun entry -> if scope entry.Value then Some entry.Key else None)
        |> Seq.toArray

    let trackedChildrenWithin scope =
        children
        |> Seq.choose (fun entry ->
            let owner, closed = entry.Value
            if scope owner then Some(entry.Key, closed) else None)
        |> Seq.toArray

    let killChildrenThenWait (childrenToStop: (int * JS.Promise<NodeProcess.ChildExit>)[]) = promise {
        for pid, _ in childrenToStop do
            do! NodeProcess.killProcessTreeAsync pid

        for _, closed in childrenToStop do
            let! _ = closed
            ()
    }

    let deleteOneSpool path = promise {
        do! removeTree path
        spoolOwners.Remove path |> ignore
        emit (SpoolDeleted path)
    }

    let deleteSpools (paths: string[]) = promise {
        for path in paths do
            do! deleteOneSpool path
    }

    let releaseScope mark scope deleteWorkerDirectory = promise {
        mark ()
        let childSnapshot = trackedChildrenWithin scope
        let spoolSnapshot = registeredSpoolsWithin scope
        do! killChildrenThenWait childSnapshot
        do! deleteSpools spoolSnapshot

        match deleteWorkerDirectory with
        | Some path ->
            do! removeTree path
            workerDirectories.Remove path |> ignore
            emit (WorkerDirectoryDeleted path)
        | None -> ()
    }

    member _.InstanceDirectory = instanceDirectory

    member _.GitExecutable = gitExecutable

    member _.WorkerDirectory(workerId: string) : JS.Promise<string> =
        let owner = { WorkerId = workerId; SessionId = ""; RequestId = "" }

        promise {
            let directory = workerDirectoryPath workerId
            workerDirectories.Add directory |> ignore
            do! NodePositionalFile.mkdirRecursive directory

            if ownerReleased owner then
                return raise (InvalidOperationException("The worker is being released and cannot create a directory."))
            else
                return directory
        }

    member _.RunShort(owner: ChildOwner, cwd: string, arguments: string[]) : JS.Promise<ShortResult> =
        promise {
            let validCommand = isAllowedShortCommand arguments

            if not validCommand then
                return raise (InvalidOperationException("The Git command is not allowed for a text diff request."))
            else
                if ownerReleased owner then
                    return raise (InvalidOperationException(releasedOwnerMessage))
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
                            (fun pid closed -> registerChild owner pid closed)

                    return {
                        ExitCode = result.ExitCode
                        Stdout = result.Stdout
                        Stderr = result.Stderr
                        Error = result.Error
                    }
        }

    member this.StartBlobToSpool(owner: ChildOwner, cwd: string, oid: string, spoolPath: string) : JS.Promise<SpoolChild> =
        promise {
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
                    if ownerReleased owner then
                        return raise (InvalidOperationException(releasedOwnerMessage))
                    else
                        let! descriptor = NodePositionalFile.openCreateExclusive targetPath
                        let mutable descriptorTransferred = false

                        try
                            if ownerReleased owner then
                                return raise (InvalidOperationException(releasedOwnerMessage))
                            else
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

    member _.ReleaseRequest(owner: ChildOwner) : JS.Promise<unit> =
        releaseScope
            (fun () -> (releasedEntry owner.WorkerId).Requests.Add owner |> ignore)
            (fun candidate -> candidate = owner)
            None

    member _.ReleaseSession(workerId: string, sessionId: string) : JS.Promise<unit> =
        releaseScope
            (fun () -> (releasedEntry workerId).Sessions.Add sessionId |> ignore)
            (fun candidate -> candidate.WorkerId = workerId && candidate.SessionId = sessionId)
            None

    member _.ReleaseWorker(workerId: string) : JS.Promise<unit> = promise {
        let directory = workerDirectoryPath workerId
        do! releaseScope (fun () -> releasedWorkers.Add workerId |> ignore) (fun candidate -> candidate.WorkerId = workerId) (Some directory)
        released.Remove workerId |> ignore
    }

    member _.Dispose() : JS.Promise<unit> = promise {
        disposed <- true
        let childSnapshot = trackedChildrenWithin (fun _ -> true)
        let spoolSnapshot = registeredSpoolsWithin (fun _ -> true)
        do! killChildrenThenWait childSnapshot
        do! deleteSpools spoolSnapshot

        for directory in workerDirectories |> Seq.toArray do
            do! removeTree directory
            workerDirectories.Remove directory |> ignore
            emit (WorkerDirectoryDeleted directory)

        do! removeTree instanceDirectory
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
    let gitExecutable = defaultArg options.GitExecutable "git"
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
                    | NodeProcess.Gone -> do! removeTree siblingPath
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
