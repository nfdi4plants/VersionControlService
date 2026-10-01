module VersionControlService.Tests.TextDiffSupervisorTests

open System
open System.Text
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.JS
open Vitest

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module TextDiffSupervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor

[<Import("mkdtemp", "node:fs/promises")>]
let private mkdtemp (prefix: string) : JS.Promise<string> = jsNative

[<Import("tmpdir", "node:os")>]
let private systemTempDirectory () : string = jsNative

[<Import("setTimeout", "node:timers/promises")>]
let private delay (milliseconds: int) : JS.Promise<unit> = jsNative

[<Emit("Object.assign({}, process.env)")>]
let private processEnvironment () : obj = jsNative

[<Emit("process.execPath")>]
let private nodeExecutable () : string = jsNative

[<Emit("process.pid")>]
let private processId () : int = jsNative

[<Emit("performance.now()")>]
let private performanceNow () : float = jsNative

[<Emit("Buffer.from($0).toString('utf8')")>]
let private bytesToUtf8 (_bytes: byte[]) : string = jsNative

[<Emit("console.log($0)")>]
let private writeLog (_message: string) : unit = jsNative

[<Import("statSync", "node:fs")>]
let private statSync (path: string) : obj = jsNative

[<Emit("$0.mode & 0o777")>]
let private permissionBits (_stats: obj) : int = jsNative

[<Emit("process.platform")>]
let private platformName () : string = jsNative

let private createTempDirectory () : JS.Promise<string> =
    mkdtemp (NodePath.join [| systemTempDirectory (); "vcs-text-diff-" |])

let private pathExists path = NodeFileSystem.existsSync path

let private removeDirectory (path: string) = promise {
    try
        do! NodeFileSystem.rmAsync path (NodeFileSystem.RmOptions(recursive = true, force = true, maxRetries = 5, retryDelay = 100))
    with _ -> ()
}

let private runGit (cwd: string) (arguments: string[]) : JS.Promise<string> = promise {
    let! result =
        NodeProcess.runBounded "git" arguments cwd (processEnvironment ()) (4 * 1024 * 1024) 65536

    match result.ExitCode, result.Error with
    | Some 0, None -> return bytesToUtf8 result.Stdout
    | _ ->
        let command = String.concat " " arguments
        return raise (InvalidOperationException($"Git command failed: {command}. {result.Stderr} {result.Error}"))
}

let private runGitOk cwd arguments = promise {
    let! _ = runGit cwd arguments
    return ()
}

let private initializeRepository (repo: string) = promise {
    do! NodePositionalFile.mkdirRecursive repo
    do! runGitOk repo [| "init"; "-q" |]
    do! runGitOk repo [| "config"; "user.name"; "Text Diff Test" |]
    do! runGitOk repo [| "config"; "user.email"; "text-diff@example.invalid" |]
}

let private createSupervisor (tempRoot: string) (events: ResizeArray<TextDiffSupervisor.SupervisorEvent> option) =
    TextDiffSupervisor.create {
        TextDiffSupervisor.TextDiffSupervisorOptions.defaults with
            TempRoot = tempRoot
            OnEvent = events |> Option.map (fun values -> fun event -> values.Add event |> ignore)
    }

let private waitForSpoolSize (path: string) (minimumSize: int64) : JS.Promise<unit> = promise {
    let started = performanceNow ()
    let mutable size = 0L

    while size < minimumSize && performanceNow () - started < 30000.0 do
        try
            let! stats = NodePositionalFile.lstat path
            size <- stats.Size
        with _ -> ()

        if size < minimumSize then
            do! delay 10

    if size < minimumSize then
        return raise (TimeoutException("The Git blob spool did not reach the expected size."))
    else
        return ()
}

let private waitForGone (pid: int) (timeoutMs: float) : JS.Promise<float option> = promise {
    let started = performanceNow ()
    let mutable goneAfter = None

    while goneAfter.IsNone && performanceNow () - started < timeoutMs do
        match NodeProcess.processExistence pid with
        | NodeProcess.Gone -> goneAfter <- Some(performanceNow () - started)
        | NodeProcess.Alive
        | NodeProcess.Unknown _ -> do! delay 10

    return goneAfter
}

let private writeLargeTextFile (path: string) (size: int64) : JS.Promise<unit> = promise {
    let line = Encoding.UTF8.GetBytes("text-diff-spool-line-0123456789abcdef\n")
    let chunk =
        Microsoft.FSharp.Collections.Array.init (64 * 1024) (fun index -> line[index % line.Length])

    let! descriptor = NodePositionalFile.openCreateExclusive path
    let mutable position = 0L

    try
        while position < size do
            let count = int (min (int64 chunk.Length) (size - position))
            let mutable written = 0

            while written < count do
                let! current = NodePositionalFile.writeAt descriptor chunk written (count - written) (position + int64 written)

                if current <= 0 then
                    return raise (InvalidOperationException("Writing the large test blob made no progress."))

                written <- written + current

            position <- position + int64 count

        do! NodePositionalFile.close descriptor
    with error ->
        do! NodePositionalFile.close descriptor
        return raise error
}

Vitest.describe (
    "Text diff supervisor",
    fun () ->
        Vitest.test (
            "removes stale instance directories for dead process ids",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectory ()

                try
                    let mutable deadPid = 0

                    let! _ =
                        NodeProcess.runBoundedWithLifecycle
                            (nodeExecutable ())
                            [| "-e"; "process.exit(0)" |]
                            root
                            (processEnvironment ())
                            1024
                            1024
                            (fun pid _ -> deadPid <- pid)

                    let parent = NodePath.join [| root; "text-diff" |]
                    let deadDirectory = NodePath.join [| parent; $"{deadPid}-deadbeef" |]
                    let activeDirectory = NodePath.join [| parent; $"{processId ()}-11111111" |]
                    let malformedDirectory = NodePath.join [| parent; "old-instance" |]
                    do! NodePositionalFile.mkdirRecursive deadDirectory
                    do! NodePositionalFile.mkdirRecursive activeDirectory
                    do! NodePositionalFile.mkdirRecursive malformedDirectory

                    let! supervisor = createSupervisor root None
                    Vitest.expect(pathExists deadDirectory).toBe false
                    Vitest.expect(pathExists activeDirectory).toBe true
                    Vitest.expect(pathExists malformedDirectory).toBe true
                    do! supervisor.Dispose ()
                with error ->
                    do! removeDirectory root
                    return raise error

                do! removeDirectory root
            }
        )

        Vitest.test (
            "rejects commands outside the read-only allowlist and runs a short Git command",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectory ()
                let repo = NodePath.join [| root; "repo" |]
                let events = ResizeArray<TextDiffSupervisor.SupervisorEvent>()
                let mutable supervisor: TextDiffSupervisor.TextDiffSupervisor option = None

                try
                    do! initializeRepository repo
                    let! created = createSupervisor root (Some events)
                    supervisor <- Some created
                    let owner: TextDiffSupervisor.ChildOwner = { WorkerId = "worker-a"; SessionId = "session-a"; RequestId = "request-a" }
                    let assertRejected arguments = promise {
                        let mutable rejected = false

                        try
                            let! _ = created.RunShort(owner, repo, arguments)
                            ()
                        with _ ->
                            rejected <- true

                        return rejected
                    }

                    for arguments in
                        [| [| "fetch"; "origin" |]
                           [| "symbolic-ref"; "HEAD"; "refs/heads/x" |]
                           [| "rev-parse"; "--git-path"; "x" |]
                           [| "rev-parse"; "--git-dir" |]
                           [| "for-each-ref"; "--contains"; "HEAD" |]
                           [| "cat-file"; "-s"; "HEAD" |] |] do
                        let! rejected = assertRejected arguments
                        Vitest.expect(rejected).toBe true

                    Vitest.expect(events |> Seq.exists (fun event -> match event.Kind with | TextDiffSupervisor.ChildSpawned _ -> true | _ -> false)).toBe false

                    let! result = created.RunShort(owner, repo, [| "symbolic-ref"; "-q"; "HEAD" |])
                    Vitest.expect(result.ExitCode).toEqual (Some 0)
                    Vitest.expect(result.Error).toBe None
                    Vitest.expect((bytesToUtf8 result.Stdout |> _.Trim()).StartsWith "refs/heads/").toBe true

                    NodeFileSystem.writeFileSync (NodePath.join [| repo; "small.txt" |]) "small blob\n" NodeFileSystem.Utf8
                    do! runGitOk repo [| "add"; "--"; "small.txt" |]
                    do! runGitOk repo [| "commit"; "-q"; "-m"; "small blob" |]
                    let! oid = runGit repo [| "rev-parse"; "HEAD:small.txt" |]
                    let oid = oid.Trim()
                    let! blob = created.RunShort(owner, repo, [| "cat-file"; "blob"; oid |])
                    Vitest.expect(blob.ExitCode).toEqual (Some 0)
                    Vitest.expect(bytesToUtf8 blob.Stdout).toBe "small blob\n"
                    let mutable prettyPrintRejected = false

                    try
                        let! _ = created.RunShort(owner, repo, [| "cat-file"; "-p"; oid |])
                        ()
                    with _ ->
                        prettyPrintRejected <- true

                    Vitest.expect(prettyPrintRejected).toBe true
                    do! created.Dispose ()
                    supervisor <- None
                with error ->
                    match supervisor with
                    | Some created -> do! created.Dispose ()
                    | None -> ()

                    do! removeDirectory root
                    return raise error

                do! removeDirectory root
            }
        )

        Vitest.test (
            "reports an error when short-command stdout exceeds its limit",
            TestOptions(timeout = 180000),
            fun () -> promise {
                let! root = createTempDirectory ()
                let repo = NodePath.join [| root; "repo" |]
                let mutable supervisor: TextDiffSupervisor.TextDiffSupervisor option = None

                try
                    do! initializeRepository repo
                    let! created = createSupervisor root None
                    supervisor <- Some created
                    let largePath = NodePath.join [| repo; "large.txt" |]
                    do! writeLargeTextFile largePath (128L * 1024L)
                    do! runGitOk repo [| "add"; "--"; "large.txt" |]
                    do! runGitOk repo [| "commit"; "-q"; "-m"; "large blob" |]
                    let! oid = runGit repo [| "rev-parse"; "HEAD:large.txt" |]
                    let oid = oid.Trim()
                    let owner: TextDiffSupervisor.ChildOwner = { WorkerId = "worker-b"; SessionId = "session-b"; RequestId = "request-b" }
                    let! result = created.RunShort(owner, repo, [| "cat-file"; "blob"; oid |])

                    Vitest.expect(result.Error.IsSome).toBe true
                    Vitest.expect(result.Stdout.Length <= 65536).toBe true
                    do! created.Dispose ()
                    supervisor <- None
                with error ->
                    match supervisor with
                    | Some created -> do! created.Dispose ()
                    | None -> ()

                    do! removeDirectory root
                    return raise error

                do! removeDirectory root
            }
        )

        Vitest.test (
            "stops a short command whose request is released while it starts",
            TestOptions(timeout = 180000),
            fun () -> promise {
                let! root = createTempDirectory ()
                let repo = NodePath.join [| root; "repo" |]
                let events = ResizeArray<TextDiffSupervisor.SupervisorEvent>()
                let mutable supervisor: TextDiffSupervisor.TextDiffSupervisor option = None

                try
                    do! initializeRepository repo
                    let largePath = NodePath.join [| repo; "large.txt" |]
                    do! writeLargeTextFile largePath (16L * 1024L * 1024L)
                    do! runGitOk repo [| "add"; "--"; "large.txt" |]
                    do! runGitOk repo [| "commit"; "-q"; "-m"; "large blob" |]
                    let! oid = runGit repo [| "rev-parse"; "HEAD:large.txt" |]
                    let oid = oid.Trim()
                    let! created = createSupervisor root (Some events)
                    supervisor <- Some created
                    let owner: TextDiffSupervisor.ChildOwner = { WorkerId = "worker-release"; SessionId = "session-release"; RequestId = "request-release" }
                    let run = created.RunShort(owner, repo, [| "cat-file"; "blob"; oid |])
                    let release = created.ReleaseRequest owner
                    let mutable runSettled = false
                    let observedRun = promise {
                        try
                            let! result = run
                            runSettled <- true
                            return Some result
                        with _ ->
                            runSettled <- true
                            return None
                    }

                    let childClosed () =
                        events
                        |> Seq.exists (fun event ->
                            match event.Kind with
                            | TextDiffSupervisor.ChildClosed _ -> true
                            | _ -> false)

                    do! release
                    // The release waits for the children it kills, so the close is recorded before it resolves.
                    let closedWhenReleaseResolved = childClosed ()
                    let! _ = observedRun
                    Vitest.expect(runSettled).toBe true
                    Vitest.expect(closedWhenReleaseResolved).toBe true
                    Vitest.expect(childClosed ()).toBe true
                    let childPid =
                        events
                        |> Seq.tryPick (fun event ->
                            match event.Kind with
                            | TextDiffSupervisor.ChildSpawned pid -> Some pid
                            | _ -> None)

                    match childPid with
                    | Some pid ->
                        let! goneAfter = waitForGone pid 2000.0
                        Vitest.expect(goneAfter.IsSome).toBe true
                    | None -> failwith "The short command did not register a child process."

                    do! created.Dispose ()
                    supervisor <- None
                with error ->
                    match supervisor with
                    | Some created -> do! created.Dispose ()
                    | None -> ()

                    do! removeDirectory root
                    return raise error

                do! removeDirectory root
            }
        )

        Vitest.test (
            "refuses a worker directory whose worker is released while the directory is created",
            TestOptions(timeout = 180000),
            fun () -> promise {
                let! root = createTempDirectory ()
                let mutable supervisor: TextDiffSupervisor.TextDiffSupervisor option = None

                try
                    let! created = createSupervisor root None
                    supervisor <- Some created
                    let directory = created.WorkerDirectory "worker-directory-race"
                    let release = created.ReleaseWorker "worker-directory-race"
                    let mutable refused = false

                    try
                        let! _ = directory
                        ()
                    with _ ->
                        refused <- true

                    do! release
                    Vitest.expect(refused).toBe true
                    do! created.Dispose ()
                    supervisor <- None
                with error ->
                    match supervisor with
                    | Some created -> do! created.Dispose ()
                    | None -> ()

                    do! removeDirectory root
                    return raise error

                do! removeDirectory root
            }
        )

        Vitest.test (
            "refuses a blob spool whose session is released while the worker directory is created",
            TestOptions(timeout = 180000),
            fun () -> promise {
                let! root = createTempDirectory ()
                let repo = NodePath.join [| root; "repo" |]
                let events = ResizeArray<TextDiffSupervisor.SupervisorEvent>()
                let mutable supervisor: TextDiffSupervisor.TextDiffSupervisor option = None

                try
                    do! initializeRepository repo
                    NodeFileSystem.writeFileSync (NodePath.join [| repo; "small.txt" |]) "small blob\n" NodeFileSystem.Utf8
                    do! runGitOk repo [| "add"; "--"; "small.txt" |]
                    do! runGitOk repo [| "commit"; "-q"; "-m"; "small blob" |]
                    let! oidText = runGit repo [| "rev-parse"; "HEAD:small.txt" |]
                    let oid = oidText.Trim()
                    let! created = createSupervisor root (Some events)
                    supervisor <- Some created
                    let owner: TextDiffSupervisor.ChildOwner = { WorkerId = "worker-race"; SessionId = "session-race"; RequestId = "request-race" }
                    let spoolPath = NodePath.join [| created.InstanceDirectory; owner.WorkerId; "blob.spool" |]
                    let start = created.StartBlobToSpool(owner, repo, oid, spoolPath)
                    let release = created.ReleaseSession(owner.WorkerId, owner.SessionId)
                    let mutable refused = false

                    try
                        let! _ = start
                        ()
                    with _ ->
                        refused <- true

                    do! release
                    Vitest.expect(refused).toBe true

                    let childSpawned =
                        events
                        |> Seq.exists (fun event ->
                            match event.Kind with
                            | TextDiffSupervisor.ChildSpawned _ -> true
                            | _ -> false)

                    Vitest.expect(childSpawned).toBe false
                    Vitest.expect(pathExists spoolPath).toBe false
                    do! created.Dispose ()
                    supervisor <- None
                with error ->
                    match supervisor with
                    | Some created -> do! created.Dispose ()
                    | None -> ()

                    do! removeDirectory root
                    return raise error

                do! removeDirectory root
            }
        )

        Vitest.test (
            "keeps a missing promisor object lookup local",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectory ()
                let repo = NodePath.join [| root; "repo" |]
                let mutable supervisor: TextDiffSupervisor.TextDiffSupervisor option = None

                try
                    do! initializeRepository repo
                    do! runGitOk repo [| "config"; "protocol.https.allow"; "always" |]
                    do! runGitOk repo [| "config"; "remote.origin.url"; "https://127.0.0.1:9/unreachable" |]
                    do! runGitOk repo [| "config"; "remote.origin.promisor"; "true" |]
                    do! runGitOk repo [| "config"; "extensions.partialClone"; "origin" |]
                    let! created = createSupervisor root None
                    supervisor <- Some created
                    let owner: TextDiffSupervisor.ChildOwner = { WorkerId = "worker-c"; SessionId = "session-c"; RequestId = "request-c" }
                    let started = performanceNow ()
                    let! result = created.RunShort(owner, repo, [| "cat-file"; "-s"; Microsoft.FSharp.Core.String.replicate 40 "0" |])
                    let elapsed = performanceNow () - started

                    Vitest.expect(result.ExitCode <> Some 0).toBe true
                    Vitest.expect(elapsed < 5000.0).toBe true
                    Vitest.expect(result.Stderr.Contains("fetch", StringComparison.OrdinalIgnoreCase)).toBe false
                    Vitest.expect(result.Stderr.Contains("https://127.0.0.1:9/unreachable", StringComparison.OrdinalIgnoreCase)).toBe false
                    do! created.Dispose ()
                    supervisor <- None
                with error ->
                    match supervisor with
                    | Some created -> do! created.Dispose ()
                    | None -> ()

                    do! removeDirectory root
                    return raise error

                do! removeDirectory root
            }
        )

        Vitest.test (
            "terminates a blob writer before removing its spool and worker directory",
            TestOptions(timeout = 600000),
            fun () -> promise {
                let! root = createTempDirectory ()
                let repo = NodePath.join [| root; "repo" |]
                let events = ResizeArray<TextDiffSupervisor.SupervisorEvent>()
                let mutable supervisor: TextDiffSupervisor.TextDiffSupervisor option = None

                try
                    do! initializeRepository repo
                    let blobPath = NodePath.join [| repo; "large.txt" |]
                    do! writeLargeTextFile blobPath (256L * 1024L * 1024L)
                    do! runGitOk repo [| "add"; "--"; "large.txt" |]
                    do! runGitOk repo [| "commit"; "-q"; "-m"; "large blob" |]
                    let! oidText = runGit repo [| "rev-parse"; "HEAD:large.txt" |]
                    let oid = oidText.Trim()
                    let! created = createSupervisor root (Some events)
                    supervisor <- Some created
                    let owner: TextDiffSupervisor.ChildOwner = { WorkerId = "worker-large"; SessionId = "session-large"; RequestId = "request-large" }
                    let! workerDirectory = created.WorkerDirectory owner.WorkerId
                    let spoolPath = NodePath.join [| workerDirectory; "blob.spool" |]
                    let! child = created.StartBlobToSpool(owner, repo, oid, spoolPath)
                    do! waitForSpoolSize spoolPath (8L * 1024L * 1024L)

                    let releaseStarted = performanceNow ()
                    let release = created.ReleaseWorker owner.WorkerId
                    let! goneAfter = waitForGone child.Pid 2000.0
                    let! () = release
                    let releaseElapsed = performanceNow () - releaseStarted
                    match goneAfter with
                    | Some milliseconds ->
                        writeLog ($"Worker child termination latency: %.1f{milliseconds} ms")
                        Vitest.expect(milliseconds <= 2000.0).toBe true
                    | None -> failwith "The blob child remained alive for more than two seconds after worker release."

                    let childClosed = events |> Seq.find (fun event -> match event.Kind with | TextDiffSupervisor.ChildClosed pid -> pid = child.Pid | _ -> false)
                    let spoolDeleted = events |> Seq.find (fun event -> match event.Kind with | TextDiffSupervisor.SpoolDeleted path -> path = spoolPath | _ -> false)
                    let workerDeleted = events |> Seq.find (fun event -> match event.Kind with | TextDiffSupervisor.WorkerDirectoryDeleted path -> path = workerDirectory | _ -> false)

                    Vitest.expect(childClosed.Timestamp < spoolDeleted.Timestamp).toBe true
                    Vitest.expect(spoolDeleted.Timestamp < workerDeleted.Timestamp).toBe true
                    Vitest.expect(pathExists spoolPath).toBe false
                    Vitest.expect(pathExists workerDirectory).toBe false
                    Vitest.expect(releaseElapsed >= 0.0).toBe true
                    do! created.Dispose ()
                    supervisor <- None
                    do! removeDirectory root
                with error ->
                    match supervisor with
                    | Some created ->
                        do! created.Dispose ()
                    | None -> ()

                    do! removeDirectory root
                    return raise error
            }
        )
)

Vitest.describe (
    "Text diff supervisor folder permissions",
    fun () ->
        Vitest.test (
            "creates folders for the current user only and files with owner access only",
            TestOptions(timeout = 120000),
            fun () -> promise {
                if platformName () = "win32" then
                    writeLog "Skipping the folder mode check because Windows has no POSIX modes."
                else
                    let! root = createTempDirectory ()

                    try
                        let! supervisor = createSupervisor root None
                        let! workerDirectory = supervisor.WorkerDirectory "worker-mode"
                        let filePath = NodePath.join [| workerDirectory; "scratch.bin" |]
                        let! descriptor = NodePositionalFile.openCreateExclusive filePath
                        do! NodePositionalFile.close descriptor

                        Vitest.expect(permissionBits (statSync (NodePath.join [| root; "text-diff" |]))).toBe 0o700
                        Vitest.expect(permissionBits (statSync supervisor.InstanceDirectory)).toBe 0o700
                        Vitest.expect(permissionBits (statSync workerDirectory)).toBe 0o700
                        Vitest.expect(permissionBits (statSync filePath)).toBe 0o600
                        do! supervisor.Dispose ()
                        do! removeDirectory root
                    with error ->
                        do! removeDirectory root
                        return raise error
            }
        )

        Vitest.test (
            "rejects an existing folder that belongs to another user",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let folderStats uid : NodePositionalFile.PositionalFileStats = {
                    Size = 0L
                    MtimeNs = "0"
                    Ino = "1"
                    Dev = "1"
                    Uid = uid
                    IsFile = false
                    IsSymbolicLink = false
                    IsDirectory = true
                }

                let rejects userId stats =
                    try
                        TextDiffSupervisor.checkFolderOwner userId "/tmp/text-diff" stats
                        false
                    with error ->
                        error.Message.Contains "/tmp/text-diff"

                Vitest.expect(rejects 1000 (folderStats "0")).toBe true
                Vitest.expect(rejects 1000 { folderStats "1000" with IsSymbolicLink = true }).toBe true
                Vitest.expect(rejects 1000 (folderStats "1000")).toBe false
                Vitest.expect(rejects -1 (folderStats "0")).toBe false
            }
        )
)
