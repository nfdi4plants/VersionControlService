module VersionControlService.Tests.TextDiffEndToEndTests

open System
open System.Text
open Fable.Core
open Fable.Core.JsInterop
open Vitest
open VersionControlService.Abstractions
open VersionControlService.Git
open VersionControlService.Git.TextDiff
open VersionControlService.Git.TextDiff.TextDiffProtocol
open VersionControlService.Git.TextDiff.TextDiffTransport
open VersionControlService.TextDiff

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads
module TextDiffPool = VersionControlService.Git.TextDiff.TextDiffPool
module TextDiffSupervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor
module TextDiffSourceResolver = VersionControlService.Git.TextDiff.TextDiffSourceResolver
module TextDiffSources = VersionControlService.Git.TextDiff.TextDiffSources

[<Import("mkdtemp", "node:fs/promises")>]
let private mkdtemp (prefix: string) : JS.Promise<string> = jsNative

[<Import("tmpdir", "node:os")>]
let private systemTempDirectory () : string = jsNative

[<Import("setTimeout", "node:timers/promises")>]
let private delay (milliseconds: int) : JS.Promise<unit> = jsNative

[<Import("fileURLToPath", "node:url")>]
let private fileUrlToPath (_url: obj) : string = jsNative

[<Emit("new URL('./TextDiffTestWorker.js', import.meta.url)")>]
let private testWorkerUrl () : obj = jsNative

[<Emit("process.env.VCS_TEXT_DIFF_TEST_WORKER_MODE = $0")>]
let private setWorkerMode (_mode: string) : unit = jsNative

[<Emit("delete process.env.VCS_TEXT_DIFF_TEST_WORKER_MODE")>]
let private clearWorkerMode () : unit = jsNative

[<Emit("Object.assign({}, process.env)")>]
let private processEnvironment () : obj = jsNative

[<Emit("Buffer.from($0).toString('utf8')")>]
let private bytesToUtf8 (_bytes: byte[]) : string = jsNative

[<Emit("Buffer.from($0)")>]
let private bufferFromBytes (_bytes: int[]) : obj = jsNative

[<Emit("performance.now()")>]
let private performanceNow () : float = jsNative

[<Emit("console.log($0)")>]
let private writeLog (_message: string) : unit = jsNative

let private testWorkerPath = fileUrlToPath (testWorkerUrl ())
let private fsPromisesDynamic: obj = importAll "node:fs/promises"

let private path value =
    match RepositoryPath.tryCreate value with
    | Ok created -> created
    | Error message -> failwith message

let private runGit (cwd: string) (arguments: string[]) : JS.Promise<string> = promise {
    let! result = NodeProcess.runBounded "git" arguments cwd (processEnvironment ()) (4 * 1024 * 1024) 65536

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

let private writeText (filePath: string) (content: string) = promise {
    do! NodePositionalFile.mkdirRecursive (NodePath.dirname filePath)
    NodeFileSystem.writeFileSync filePath content NodeFileSystem.TextEncoding.Utf8
}

let private writeBytes (filePath: string) (bytes: int[]) = promise {
    do! NodePositionalFile.mkdirRecursive (NodePath.dirname filePath)
    let! _ = fsPromisesDynamic?writeFile (filePath, bufferFromBytes bytes) |> unbox<JS.Promise<obj>>
    return ()
}

/// An ASCII file of about 600,000 bytes. The first 64 KiB, middle and last 64 KiB samples never reach the
/// positions used for the umlaut in the tests. A negative position leaves out the umlaut or the changed byte.
let private lateUmlautFile (umlautAt: int) (changeAt: int) =
    let line = "0123456789abcdefghij\n"
    let lines = 600000 / line.Length
    let bytes = Array.init (lines * line.Length) (fun index -> int line[index % line.Length])
    if changeAt >= 0 then bytes[changeAt] <- int 'X'
    if umlautAt >= 0 then bytes[umlautAt] <- 0xE4
    bytes

let private initializeRepository (repository: string) = promise {
    do! NodePositionalFile.mkdirRecursive repository
    do! runGitOk repository [| "init"; "-q" |]
    do! runGitOk repository [| "config"; "user.name"; "Text Diff End To End" |]
    do! runGitOk repository [| "config"; "user.email"; "text-diff-e2e@example.invalid" |]
    do! runGitOk repository [| "config"; "core.autocrlf"; "false" |]
}

let private commitText (repository: string) (relativePath: string) (content: string) = promise {
    do! writeText (NodePath.join [| repository; relativePath |]) content
    do! runGitOk repository [| "add"; "--"; relativePath |]
    do! runGitOk repository [| "commit"; "-q"; "-m"; $"update {relativePath}" |]
}

let private commitBytes (repository: string) (relativePath: string) (bytes: int[]) = promise {
    do! writeBytes (NodePath.join [| repository; relativePath |]) bytes
    do! runGitOk repository [| "add"; "--"; relativePath |]
    do! runGitOk repository [| "commit"; "-q"; "-m"; $"update {relativePath}" |]
}

let private removeWorkingFile (repository: string) (relativePath: string) = promise {
    NodeFileSystem.unlinkSync (NodePath.join [| repository; relativePath |])
}

let private writePatternFile (filePath: string) (size: int64) (firstLine: string) (repeatLine: string) = promise {
    let first = Encoding.UTF8.GetBytes(firstLine + "\n")
    let repeated = Encoding.UTF8.GetBytes(repeatLine + "\n")
    let chunkLength =
        if repeated.Length > 64 * 1024 then repeated.Length
        else (64 * 1024 / repeated.Length) * repeated.Length

    let chunk = Array.init chunkLength (fun index -> repeated[index % repeated.Length])
    do! NodeFileSystem.rmAsync filePath (NodeFileSystem.RmOptions(recursive = true, force = true, maxRetries = 0, retryDelay = 0))
    let! descriptor = NodePositionalFile.openCreateExclusive filePath
    let mutable position = 0L

    let writeFully (bytes: byte[]) (count: int) = promise {
        let mutable written = 0

        while written < count do
            let! current = NodePositionalFile.writeAt descriptor bytes written (count - written) (position + int64 written)

            if current <= 0 then
                return raise (InvalidOperationException("Writing the text diff fixture made no progress."))

            written <- written + current

        position <- position + int64 count
    }

    try
        let firstCount = min first.Length (int size)
        do! writeFully first firstCount

        while position < size do
            let count = int (min (int64 chunk.Length) (size - position))
            do! writeFully chunk count

        do! NodePositionalFile.close descriptor
    with error ->
        do! NodePositionalFile.close descriptor
        return raise error
}

let private removeDirectory (directory: string) = promise {
    try
        do! NodeFileSystem.rmAsync directory (NodeFileSystem.RmOptions(recursive = true, force = true, maxRetries = 5, retryDelay = 100))
    with _ -> ()
}

let private gitProviderId =
    match ProviderId.tryCreate "git" with
    | Ok providerId -> providerId
    | Error message -> failwith message

let private bindingFor (repository: string) : WorkspaceBinding =
    let location: RepositoryLocation = {
        ProviderId = gitProviderId
        DisplayName = None
        ProviderLocation = repository
        ConnectionProfileId = None
    }

    {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = gitProviderId
        WorkspaceRoot = repository
        ProviderStateRef = None
        Location = location
        ConnectionProfileId = None
    }

let private mediaDirectory (repository: string) = NodePath.join [| repository; ".git"; "lfs"; "objects" |]

let private createSession (pool: TextDiffPool.TextDiffPool) (repository: string) =
    GitWorkspaceSession.createSessionWithOptions
        {
            Hooks = GitWorkspaceSession.GitSessionHooks.none
            TextDiff =
                Some {
                    Pool = pool
                    WindowOwnerOf = fun context ->
                        if context.OperationId.Contains("window-b", StringComparison.Ordinal) then "window-b" else "window-a"
                }
        }
        GitCredentialStrategy.anonymous
        GitCredentialStrategy.anonymousIdentity
        RevisionPolicyStrategy.automatic
        (bindingFor repository)

let private serviceFor (session: WorkspaceSession) =
    match session.TextDiff with
    | Some service -> service
    | None -> failwith "The Git session has no text diff service."

let private context name = OperationContext.detached name

let private openRequest relativePath = {
    Path = path relativePath
    PreviousPath = None
    Preparation = None
    PreviousEncoding = None
    CurrentEncoding = None
    ContextLines = 3
    Continuation = None
}

let private operationValue operationName result =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(outcome, failure) ->
        failwith $"{operationName} returned partial success: {failure.Code} {outcome.Value}"
    | Failed failure -> failwith $"{operationName} failed: {failure.Code} {failure.Message}"

let private openUntilReady (service: TextDiffService) (initial: OpenDiffRequest) (operationName: string) =
    TextDiffTestSupport.openUntilReady service initial (context operationName)

let private readPageReady
    (service: TextDiffService)
    (handle: DiffHandle)
    (initialCursor: string)
    (operationName: string)
    =
    promise {
        let mutable cursor = initialCursor
        let mutable value = None

        while value.IsNone do
            let! result =
                service.ReadPage { Handle = handle; Cursor = cursor } (context operationName)
                |> Async.StartAsPromise

            let outcome = operationValue "ReadPage" result

            match outcome with
            | Resumable.Ready page -> value <- Some page
            | Resumable.Scanning(_, continuation, _) -> cursor <- continuation

        return value.Value
    }

let private openedWithFirstPage service request operationName = promise {
    let! opened = openUntilReady service request operationName

    match opened with
    | OpenDiffResult.Opened(handle, previous, current, Resumable.Ready first) ->
        return handle, previous, current, first
    | OpenDiffResult.Opened(handle, previous, current, Resumable.Scanning(_, continuation, _)) ->
        let! first = readPageReady service handle continuation (operationName + "-first-page")
        return handle, previous, current, first
    | OpenDiffResult.NotDiffable blocker ->
        return raise (InvalidOperationException($"Expected an opened diff, got %A{blocker}."))
}

let private readAllPages service handle first operationName = promise {
    let pages = ResizeArray<DiffPage>()
    pages.Add first
    let mutable next = first.NextCursor
    let mutable operationIndex = 0

    while next.IsSome do
        let cursor = next.Value
        let! page = readPageReady service handle cursor (operationName + "-" + string operationIndex)
        pages.Add page
        next <- page.NextCursor
        operationIndex <- operationIndex + 1

    return pages.ToArray()
}

let private expandReady
    (service: TextDiffService)
    (handle: DiffHandle)
    (gapId: string)
    (fromStart: bool)
    (count: int)
    (operationName: string)
    =
    promise {
        let mutable continuation = None
        let mutable parts = None

        while parts.IsNone do
            let request = {
                Handle = handle
                GapId = gapId
                FromStart = fromStart
                Count = count
                Continuation = continuation
            }
            let! result = service.Expand request (context operationName) |> Async.StartAsPromise

            match operationValue "Expand" result with
            | Resumable.Ready expanded -> parts <- Some expanded
            | Resumable.Scanning(_, next, _) -> continuation <- Some next

        return parts.Value
    }

let private readLineReady
    (service: TextDiffService)
    (handle: DiffHandle)
    (side: DiffSide)
    (line: int64)
    (offsetUtf16: int64)
    (maxUtf16: int)
    (operationName: string)
    =
    promise {
        let mutable continuation = None
        let mutable value = None

        while value.IsNone do
            let request = {
                Handle = handle
                Side = side
                Line = line
                OffsetUtf16 = offsetUtf16
                MaxUtf16 = maxUtf16
                Continuation = continuation
            }
            let! result = service.ReadLine request (context operationName) |> Async.StartAsPromise

            match operationValue "ReadLine" result with
            | Resumable.Ready slice -> value <- Some slice
            | Resumable.Scanning(_, next, _) -> continuation <- Some next

        return value.Value
    }

let private hiddenGaps (parts: DiffPart[]) =
    parts
    |> Array.choose (function
        | DiffPart.HiddenEqual gap -> Some gap
        | _ -> None)

let private expandedPreviousLines (parts: DiffPart[]) =
    parts
    |> Array.collect (function
        | DiffPart.ExpandedContext(_, rows) -> rows |> Array.choose (fun row -> row.Previous)
        | _ -> Array.empty)

let private changedRow (row: DiffRow) =
    match row.Kind with
    | DiffRowKind.Context -> false
    | DiffRowKind.Added
    | DiffRowKind.Removed
    | DiffRowKind.Replaced
    | DiffRowKind.EndingChanged -> true

let private partHasChange part =
    match part with
    | DiffPart.Hunk hunk ->
        match hunk.Body with
        | HunkBody.AlignedRows rows -> rows |> Array.exists changedRow
        | HunkBody.UnalignedSides(previous, current) -> previous.Length > 0 || current.Length > 0
    | DiffPart.HiddenEqual _
    | DiffPart.ExpandedContext _ -> false

let private pageHasChange page = page.Parts |> Array.exists partHasChange

let private expectOpened result =
    match result with
    | OpenDiffResult.Opened(handle, previous, current, first) -> handle, previous, current, first
    | OpenDiffResult.NotDiffable blocker -> failwith $"Expected Opened, got %A{blocker}."

type private Fixture = {
    Root: string
    Supervisor: TextDiffSupervisor.TextDiffSupervisor
    Pool: TextDiffPool.TextDiffPool
    Transports: ResizeArray<ITextDiffWorkerTransport>
    Events: ResizeArray<TextDiffSupervisor.SupervisorEvent>
}

let mutable private fixture: Fixture option = None
let mutable private nextRepository = 0

let private currentFixture () =
    match fixture with
    | Some value -> value
    | None -> failwith "The text diff fixture was not created."

let private newRepository () = promise {
    let fixture = currentFixture ()
    nextRepository <- nextRepository + 1
    let repository = NodePath.join [| fixture.Root; $"repo-{nextRepository}" |]
    do! initializeRepository repository
    return repository
}

let private closeSession (session: WorkspaceSession) (failure: exn option) = promise {
    let mutable closeFailure = None

    try
        do! session.Close() |> Async.StartAsPromise
    with error ->
        closeFailure <- Some error

    match failure, closeFailure with
    | Some error, _ -> return raise error
    | None, Some error -> return raise error
    | None, None -> ()
}

let private waitFor (description: string) (timeoutMs: float) (condition: unit -> JS.Promise<bool>) = promise {
    let started = performanceNow ()
    let mutable reached = false

    while not reached && performanceNow () - started < timeoutMs do
        let! current = condition ()
        reached <- current

        if not reached then
            do! delay 10

    if not reached then
        failwith $"Timed out waiting for {description}."
}

let private hasWorkerDirectoryDeletedSince
    (events: ResizeArray<TextDiffSupervisor.SupervisorEvent>)
    (startIndex: int)
    =
    events
    |> Seq.skip startIndex
    |> Seq.exists (fun event ->
        match event.Kind with
        | TextDiffSupervisor.WorkerDirectoryDeleted _ -> true
        | _ -> false)

let private largestSpool (directory: string) : JS.Promise<int64> = promise {
    let mutable largest = 0L

    try
        let! workerNames = NodeFileSystem.readdirAsync directory

        for workerName in workerNames do
            let workerDirectory = NodePath.join [| directory; workerName |]
            let! fileNames = NodeFileSystem.readdirAsync workerDirectory

            for fileName in fileNames do
                if fileName.EndsWith ".blob" then
                    let! stats = NodePositionalFile.lstat (NodePath.join [| workerDirectory; fileName |])
                    largest <- max largest stats.Size
    with _ -> ()

    return largest
}

let private assertClosedCode operation result =
    match result with
    | Failed failure -> Vitest.expect(failure.Code).toBe TextDiffFailureCodes.SessionClosed
    | Succeeded _
    | PartiallySucceeded _ -> failwith $"{operation} unexpectedly succeeded."

Vitest.describe (
    "Git text diff end to end",
    fun () ->
        Vitest.beforeAll (
            (fun () -> promise {
                let! root = mkdtemp (NodePath.join [| systemTempDirectory (); "vcs-text-diff-e2e-" |])
                let events = ResizeArray<TextDiffSupervisor.SupervisorEvent>()

                let! supervisor =
                    TextDiffSupervisor.create {
                        TextDiffSupervisor.TextDiffSupervisorOptions.defaults with
                            TempRoot = root
                            OnEvent = Some(fun event -> events.Add event)
                    }

                let transports = ResizeArray<ITextDiffWorkerTransport>()

                let factory: TextDiffWorkerFactory =
                    fun _ ->
                        setWorkerMode "default"

                        try
                            let transport = WorkerThreadTransport.create testWorkerPath
                            transports.Add transport
                            transport
                        finally
                            clearWorkerMode ()

                let pool =
                    TextDiffPool.create {
                        TextDiffPool.TextDiffPoolOptions.create factory supervisor with
                            MaxWorkers = 1
                    }

                fixture <- Some { Root = root; Supervisor = supervisor; Pool = pool; Transports = transports; Events = events }
            }),
            600000
        )

        Vitest.afterAll (
            (fun () -> promise {
                match fixture with
                | Some value ->
                    do! value.Pool.Dispose()
                    do! removeDirectory value.Root
                | None -> ()
            }),
            120000
        )

        Vitest.test (
            "creates and removes a temporary store with a safe file name",
            fun () -> promise {
                let directory = NodePath.join [| (currentFixture ()).Root; "safe-store" |]
                do! NodePositionalFile.mkdirRecursive directory
                let! store = TextDiffSources.NodeTempStore.Create(directory, "pairs:test.tmp")
                let! stats = NodePositionalFile.lstat store.Path
                let path = store.Path
                do! store.Dispose() |> Async.StartAsPromise

                Vitest.expect(path.EndsWith "pairs-test.tmp").toBe true
                Vitest.expect(stats.IsFile).toBe true
                Vitest.expect(NodeFileSystem.existsSync path).toBe false
            }
        )

        Vitest.test (
            "rejects temporary store names that are empty or contain path syntax",
            fun () ->
                for fileName in [| ""; "."; ".."; "pairs/data.tmp"; "pairs\\data.tmp" |] do
                    let mutable rejected = false

                    try
                        TextDiffSources.NodeTempStore.Create((currentFixture ()).Root, fileName) |> ignore
                    with _ ->
                        rejected <- true

                    Vitest.expect(rejected).toBe true
        )

        Vitest.test (
            "validates a spool after its child exits during a pending read",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let filePath = NodePath.join [| (currentFixture ()).Root; "racing-spool.blob" |]
                let! writer = NodePositionalFile.openCreateExclusiveReadWrite filePath
                let initial = Encoding.UTF8.GetBytes "part"
                let! _ = NodePositionalFile.writeAt writer initial 0 initial.Length 0L
                let mutable resolveChild: (NodeProcess.ChildExit -> unit) option = None
                let childClosed: JS.Promise<NodeProcess.ChildExit> =
                    JS.Constructors.Promise.Create(fun resolve _ -> resolveChild <- Some resolve)
                // The first two fills wait: the one of the pending ReadAt and the one the child exit starts on its own.
                // Only the pending ReadAt can then bring the rest of the file in after the exit.
                let heldReads = ResizeArray<unit -> unit>()
                let mutable fills = 0

                let readAt file buffer offset count position =
                    fills <- fills + 1

                    if fills <= 2 then
                        JS.Constructors.Promise.Create(fun resolve reject ->
                            heldReads.Add(fun () ->
                                NodeInterop.observePromise
                                    (NodePositionalFile.readAt file buffer offset count position)
                                    resolve
                                    reject))
                    else
                        NodePositionalFile.readAt file buffer offset count position

                let! source =
                    TextDiffSources.SpoolSource.Open(
                        filePath,
                        16L,
                        Some(Array.zeroCreate 16),
                        childClosed,
                        readAt = readAt
                    )

                let target = [| 0uy |]
                let pending = source.ReadAt 4L target 0 1 |> Async.StartAsPromise
                do! waitFor "the pending spool read" 5000.0 (fun () -> Promise.lift (heldReads.Count = 1))

                let finalBytes = Encoding.UTF8.GetBytes "abcdefghijklmnop"
                let! _ = NodePositionalFile.writeAt writer finalBytes 0 finalBytes.Length 0L
                resolveChild.Value {
                    ExitCode = Some 0
                    Signal = None
                    Stderr = ""
                    StderrTruncated = false
                    SpawnError = None
                }
                do! waitFor "the fill started by the child exit" 5000.0 (fun () -> Promise.lift (heldReads.Count = 2))
                heldReads[0] ()

                let! outcome = pending
                Vitest.expect(outcome).toEqual(ReadOutcome.Bytes 1)
                Vitest.expect(target[0]).toBe finalBytes[4]
                Vitest.expect(source.Failure.IsNone).toBe true
                Vitest.expect(source.IsComplete()).toBe true
                heldReads[1] ()

                for position = 0 to finalBytes.Length - 1 do
                    let single = [| 0uy |]
                    let! result = source.ReadAt(int64 position) single 0 1 |> Async.StartAsPromise

                    match result with
                    | ReadOutcome.Bytes 1 -> Vitest.expect(single[0]).toBe finalBytes[position]
                    | other -> failwith $"Expected byte {position}, got %A{other}"

                do! source.Dispose()
                do! NodePositionalFile.close writer
            }
        )

        Vitest.test (
            "fails a retained spool read when the file returns no bytes inside the observed size",
            TestOptions(timeout = 30000),
            fun () -> promise {
                let filePath = NodePath.join [| (currentFixture ()).Root; "shrinking-spool.blob" |]
                do! writeText filePath "abcdefghijklmnop"

                let childClosed: JS.Promise<NodeProcess.ChildExit> =
                    Promise.lift {
                        ExitCode = Some 0
                        Signal = None
                        Stderr = ""
                        StderrTruncated = false
                        SpawnError = None
                    }

                // The file keeps reporting 16 bytes, but only the first four can be read.
                let mutable fills = 0

                let readAt file buffer offset count position =
                    fills <- fills + 1

                    if fills = 1 then
                        NodePositionalFile.readAt file buffer offset (min count 4) position
                    else
                        Promise.lift 0

                let! source =
                    TextDiffSources.SpoolSource.Open(
                        filePath,
                        16L,
                        Some(Array.zeroCreate 16),
                        childClosed,
                        readAt = readAt
                    )

                do! waitFor "the spool to settle" 5000.0 (fun () -> Promise.lift (source.IsComplete() || source.Failure.IsSome))
                let mutable failureCode = None
                let mutable answer = None

                try
                    let! outcome = source.ReadAt 8L [| 0uy |] 0 1 |> Async.StartAsPromise
                    answer <- Some outcome
                with :? TextDiffSources.TextDiffSourceException as error ->
                    failureCode <- Some error.Code

                do! source.Dispose()
                Vitest.expect(answer).toEqual None
                Vitest.expect(failureCode).toEqual(Some TextDiffFailureCodes.ReadFailed)
            }
        )

        Vitest.test (
            "signals when a spool read has to wait for more bytes",
            fun () -> promise {
                let filePath = NodePath.join [| (currentFixture ()).Root; "waiting-spool.blob" |]
                do! writeText filePath "part"
                let childClosed: JS.Promise<NodeProcess.ChildExit> = JS.Constructors.Promise.Create(fun _ _ -> ())
                let mutable waits = 0
                let! source =
                    TextDiffSources.SpoolSource.Open(
                        filePath,
                        16L,
                        Some(Array.zeroCreate 16),
                        childClosed,
                        onWait = (fun () -> waits <- waits + 1)
                    )
                let buffer = Array.zeroCreate<byte> 1
                let! outcome = source.ReadAt 8L buffer 0 1 |> Async.StartAsPromise
                do! source.Dispose()

                Vitest.expect(outcome).toEqual(ReadOutcome.NotYetAvailable)
                Vitest.expect(waits).toBe 1
            }
        )

        Vitest.test (
            "reports a deleted working file as changed",
            fun () -> promise {
                let filePath = NodePath.join [| (currentFixture ()).Root; "deleted-working-file.txt" |]
                do! writeText filePath "working source"
                let! stats = NodePositionalFile.lstat filePath
                let identity: TextDiffSourceResolver.FileIdentity = {
                    Size = stats.Size
                    MtimeNs = stats.MtimeNs
                    Ino = stats.Ino
                    Dev = stats.Dev
                }
                let! source = TextDiffSources.FileSource.OpenWorkingFile(filePath, identity)
                NodeFileSystem.unlinkSync filePath
                let mutable failureCode = None

                try
                    do! source.CheckIdentity() |> Async.StartAsPromise
                with :? TextDiffSources.TextDiffSourceException as error ->
                    failureCode <- Some error.Code

                do! source.Dispose()
                Vitest.expect(failureCode).toEqual(Some TextDiffFailureCodes.SourceChanged)
            }
        )

        Vitest.test (
            "reports a working file saved shorter in place as changed instead of a read failure",
            fun () -> promise {
                let filePath = NodePath.join [| (currentFixture ()).Root; "truncated-working-file.txt" |]
                do! writeText filePath (new System.String('a', 100000))
                let! stats = NodePositionalFile.lstat filePath
                let identity: TextDiffSourceResolver.FileIdentity = {
                    Size = stats.Size
                    MtimeNs = stats.MtimeNs
                    Ino = stats.Ino
                    Dev = stats.Dev
                }
                let! source = TextDiffSources.FileSource.OpenWorkingFile(filePath, identity)
                let buffer = Array.zeroCreate<byte> 4096
                let! _ = source.ReadAt 0L buffer 0 4096 |> Async.StartAsPromise
                NodeFileSystem.writeFileSync filePath "0123456789" NodeFileSystem.Utf8
                let mutable readCode = None
                let mutable checkCode = None

                try
                    let! _ = source.ReadAt 50000L buffer 0 4096 |> Async.StartAsPromise
                    ()
                with :? TextDiffSources.TextDiffSourceException as error ->
                    readCode <- Some error.Code

                try
                    do! source.CheckIdentity() |> Async.StartAsPromise
                with :? TextDiffSources.TextDiffSourceException as error ->
                    checkCode <- Some error.Code

                do! source.Dispose()
                Vitest.expect(readCode).toEqual(Some TextDiffFailureCodes.SourceChanged)
                Vitest.expect(checkCode).toEqual(Some TextDiffFailureCodes.SourceChanged)
            }
        )

        Vitest.test (
            "pages large diffs in turns with shared scratch",
            TestOptions(timeout = 60000),
            fun () -> promise {
                let! repository = newRepository ()
                let relativePaths = [| "scratch-a.txt"; "scratch-b.txt"; "scratch-c.txt" |]
                let fileSize = 4L * 1024L * 1024L + 64L
                let sharedLine = new System.String(Array.create (1024 * 1024) 'x')

                for relativePath in relativePaths do
                    do! writePatternFile (NodePath.join [| repository; relativePath |]) fileSize "before-first-line" sharedLine

                do! runGitOk repository (Array.append [| "add"; "--" |] relativePaths)
                do! runGitOk repository [| "commit"; "-q"; "-m"; "large scratch sources" |]

                for relativePath in relativePaths do
                    do! writePatternFile (NodePath.join [| repository; relativePath |]) fileSize "after-first-line" sharedLine

                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    // The fixture pool has one slot, so each Open closes the previous idle diff.
                    // Every diff is paged to its end before the next one opens.
                    for relativePath in relativePaths do
                        let! handle, _, _, first =
                            openedWithFirstPage service (openRequest relativePath) ("scratch-" + relativePath)
                        Vitest.expect(pageHasChange first).toBe true
                        let! pages = readAllPages service handle first ("scratch-page-" + relativePath)
                        Vitest.expect(pages.Length > 0).toBe true
                        Vitest.expect(pages[pages.Length - 1].NextCursor.IsNone).toBe true
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "opens a modified text file, pages its changes, and replays a page",
            TestOptions(timeout = 180000),
            fun () -> promise {
                let! repository = newRepository ()
                let previous = String.concat "" [| for index in 0 .. 1199 -> $"before-{index}\n" |]
                let current = String.concat "" [| for index in 0 .. 1199 -> $"after-{index}\n" |]
                do! commitText repository "changed.txt" previous
                do! writeText (NodePath.join [| repository; "changed.txt" |]) current
                let! headOutput = runGit repository [| "rev-parse"; "HEAD" |]
                let head = headOutput.Trim()
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    let! handle, previousInfo, currentInfo, first =
                        openedWithFirstPage service (openRequest "changed.txt") "modified-window-a"

                    Vitest.expect(pageHasChange first).toBe true
                    Vitest.expect(first.OutputComplete).toBe false

                    let! sourceResult = service.GetSourceInfo { Handle = handle } (context "source-info-window-a") |> Async.StartAsPromise
                    let sourcePrevious, sourceCurrent = operationValue "GetSourceInfo" sourceResult
                    Vitest.expect(RepositoryPath.value previousInfo.Path).toBe "changed.txt"
                    Vitest.expect(RepositoryPath.value currentInfo.Path).toBe "changed.txt"
                    Vitest.expect(RepositoryPath.value sourcePrevious.Path).toBe "changed.txt"
                    Vitest.expect(RepositoryPath.value sourceCurrent.Path).toBe "changed.txt"
                    Vitest.expect(sourcePrevious.Revision |> Option.map RevisionId.value).toEqual(Some head)
                    Vitest.expect(sourcePrevious.IsAbsent).toBe false
                    Vitest.expect(sourceCurrent.IsAbsent).toBe false
                    Vitest.expect(sourcePrevious.ByteLength).toEqual(int64 previous.Length)
                    Vitest.expect(sourceCurrent.ByteLength).toEqual(int64 current.Length)
                    Vitest.expect(sourceCurrent.Encoding).toEqual(Some "utf-8")
                    Vitest.expect(sourceCurrent.EncodingWasChosen).toBe false
                    Vitest.expect(sourceCurrent.HasBom).toBe false

                    let! replayResult = service.ReplayPage { Handle = handle; PageId = first.PageId } (context "replay-window-a") |> Async.StartAsPromise
                    let replayed = operationValue "ReplayPage" replayResult
                    Vitest.expect(replayed).toEqual first

                    let! pages = readAllPages service handle first "read-window-a"
                    Vitest.expect(pages.Length > 1).toBe true
                    Vitest.expect(pages[pages.Length - 1].OutputComplete).toBe true
                    Vitest.expect(pages |> Array.exists pageHasChange).toBe true

                    let! closeResult = service.Close handle (context "close-window-a") |> Async.StartAsPromise
                    ignore (operationValue "Close" closeResult)
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "reads a page with a one-line edit and a line ending change between context rows",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                let lines = [| for index in 0 .. 11 -> $"line-{index}" |]
                let previous = String.concat "" [| for line in lines -> line + "\n" |]

                let current =
                    String.concat "" [|
                        for index in 0 .. 11 ->
                            if index = 3 then "line-3 edited\n"
                            elif index = 8 then "line-8\r\n"
                            else lines[index] + "\n"
                    |]

                do! commitText repository "small.txt" previous
                do! writeText (NodePath.join [| repository; "small.txt" |]) current
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    let! handle, _, _, first = openedWithFirstPage service (openRequest "small.txt") "small-edit-window-a"
                    let! pages = readAllPages service handle first "small-edit-read-window-a"

                    let rows =
                        pages
                        |> Array.collect (fun page -> page.Parts)
                        |> Array.collect (function
                            | DiffPart.Hunk { Body = HunkBody.AlignedRows rows } -> rows
                            | _ -> Array.empty)

                    Vitest.expect(rows.Length).toBe 12

                    for index in 0 .. rows.Length - 1 do
                        let row = rows[index]
                        let previousLine = row.Previous.Value
                        let currentLine = row.Current.Value
                        Vitest.expect(previousLine.Slice.Text).toBe lines[index]
                        Vitest.expect(previousLine.Number).toEqual (int64 index)
                        Vitest.expect(currentLine.Number).toEqual (int64 index)

                        match index with
                        | 3 ->
                            Vitest.expect(row.Kind).toEqual DiffRowKind.Replaced
                            Vitest.expect(currentLine.Slice.Text).toBe "line-3 edited"
                        | 8 ->
                            Vitest.expect(row.Kind).toEqual DiffRowKind.EndingChanged
                            Vitest.expect(currentLine.Slice.Text).toBe lines[index]
                            Vitest.expect(currentLine.Ending).toEqual LineEnding.CRLF
                        | _ ->
                            Vitest.expect(row.Kind).toEqual DiffRowKind.Context
                            Vitest.expect(currentLine.Slice.Text).toBe lines[index]

                    let! closeResult = service.Close handle (context "small-edit-close-window-a") |> Async.StartAsPromise
                    ignore (operationValue "Close" closeResult)
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "expands a large equal gap from both ends and replays a replaced gap",
            TestOptions(timeout = 180000),
            fun () -> promise {
                let! repository = newRepository ()
                let previousLines = Array.init 1024 (fun index -> $"line-{(string index).PadLeft(5, '0')}")
                let currentLines = Array.copy previousLines
                currentLines[8] <- "changed-near-start"
                currentLines[1015] <- "changed-near-end"
                let previous = String.concat "\n" previousLines + "\n"
                let current = String.concat "\n" currentLines + "\n"
                do! commitText repository "expanded.txt" previous
                do! writeText (NodePath.join [| repository; "expanded.txt" |]) current
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    let! handle, _, _, first =
                        openedWithFirstPage service (openRequest "expanded.txt") "expand-gap-window-a"
                    let! pages = readAllPages service handle first "expand-gap-pages-window-a"
                    let allParts = pages |> Array.collect (fun page -> page.Parts)
                    let gap =
                        hiddenGaps allParts
                        |> Array.filter (fun value -> value.PreviousRange.Count > 900L)
                        |> Array.tryHead
                        |> Option.defaultWith (fun () -> failwith "The fixture has no large equal gap.")
                    let collected = ResizeArray<DiffLine>()

                    let! fromStart = expandReady service handle gap.GapId true 5 "expand-gap-start-window-a"
                    collected.AddRange(expandedPreviousLines fromStart)

                    let! retried = expandReady service handle gap.GapId true 5 "expand-gap-retry-window-a"
                    Vitest.expect(retried).toEqual fromStart

                    let afterStart =
                        hiddenGaps fromStart
                        |> Array.tryHead
                        |> Option.defaultWith (fun () -> failwith "The first expansion did not leave a hidden gap.")
                    let! fromEnd = expandReady service handle afterStart.GapId false 5 "expand-gap-end-window-a"
                    collected.AddRange(expandedPreviousLines fromEnd)

                    let mutable remaining = hiddenGaps fromEnd |> Array.tryHead
                    let mutable expansionIndex = 0

                    while remaining.IsSome do
                        let currentGap = remaining.Value
                        let! expanded =
                            expandReady
                                service
                                handle
                                currentGap.GapId
                                true
                                (int currentGap.PreviousRange.Count)
                                $"expand-gap-rest-{expansionIndex}-window-a"
                        collected.AddRange(expandedPreviousLines expanded)
                        remaining <- hiddenGaps expanded |> Array.tryHead
                        expansionIndex <- expansionIndex + 1

                    let expected =
                        Array.init (int gap.PreviousRange.Count) (fun index ->
                            previousLines[int gap.PreviousRange.Start + index])
                    let orderedLines = collected.ToArray() |> Array.sortBy (fun line -> line.Number)
                    let lineNumbers = orderedLines |> Array.map (fun line -> line.Number)
                    let actual = orderedLines |> Array.map (fun line -> line.Slice.Text)

                    Vitest.expect(actual.Length).toBe expected.Length
                    Vitest.expect(lineNumbers |> Array.distinct |> Array.length).toBe expected.Length
                    Vitest.expect(actual).toEqual expected

                    let! closeResult = service.Close handle (context "close-expand-gap-window-a") |> Async.StartAsPromise
                    ignore (operationValue "Close" closeResult)
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "reads a long source line in UTF-16 slices",
            TestOptions(timeout = 180000),
            fun () -> promise {
                let! repository = newRepository ()
                let line = String.replicate 20_000 "x"
                do! commitText repository "long-line.txt" "previous\n"
                do! writeText (NodePath.join [| repository; "long-line.txt" |]) (line + "\n")
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    let! handle, _, _, _ =
                        openedWithFirstPage service (openRequest "long-line.txt") "read-line-window-a"
                    let result = StringBuilder()
                    let mutable offset = 0L
                    let mutable sliceIndex = 0

                    while offset < int64 line.Length do
                        let! value =
                            readLineReady
                                service
                                handle
                                DiffSide.Current
                                0L
                                offset
                                8192
                                $"read-line-slice-{sliceIndex}-window-a"
                        let expectedSliceLength = min 8192 (line.Length - int offset)
                        Vitest.expect(value.Slice.Text.Length).toBe expectedSliceLength
                        result.Append(value.Slice.Text) |> ignore
                        offset <- offset + int64 value.Slice.Text.Length
                        sliceIndex <- sliceIndex + 1

                    Vitest.expect(sliceIndex).toBe 3
                    Vitest.expect(result.ToString()).toBe line

                    let! closeResult = service.Close handle (context "close-read-line-window-a") |> Async.StartAsPromise
                    ignore (operationValue "Close" closeResult)
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "reports source line counts after the scan completes",
            TestOptions(timeout = 300000),
            fun () -> promise {
                let! repository = newRepository ()
                let lineCount = 40_000
                let previousLines = Array.init lineCount (fun index -> $"shared-line-{(string index).PadLeft(5, '0')}")
                let currentLines = Array.copy previousLines
                currentLines[0] <- "changed-first-line"
                currentLines[lineCount - 1] <- "changed-last-line"
                let previous = String.concat "\n" previousLines + "\n"
                let current = String.concat "\n" currentLines + "\n"
                do! commitText repository "line-counts.txt" previous
                do! writeText (NodePath.join [| repository; "line-counts.txt" |]) current
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    let! handle, previousAtOpen, currentAtOpen, first =
                        openedWithFirstPage service (openRequest "line-counts.txt") "line-count-window-a"
                    let! beforeResult =
                        service.GetSourceInfo { Handle = handle } (context "line-count-before-window-a")
                        |> Async.StartAsPromise
                    let beforePrevious, beforeCurrent = operationValue "GetSourceInfo" beforeResult

                    Vitest.expect(beforePrevious.LineCount).toEqual None
                    Vitest.expect(beforeCurrent.LineCount).toEqual None

                    let! pages = readAllPages service handle first "line-count-pages-window-a"
                    Vitest.expect(pages[pages.Length - 1].Progress.ScanComplete).toBe true

                    let! afterResult =
                        service.GetSourceInfo { Handle = handle } (context "line-count-after-window-a")
                        |> Async.StartAsPromise
                    let afterPrevious, afterCurrent = operationValue "GetSourceInfo" afterResult
                    let expectedCount = Some(int64 lineCount)

                    Vitest.expect(afterPrevious).toEqual { previousAtOpen with LineCount = expectedCount }
                    Vitest.expect(afterCurrent).toEqual { currentAtOpen with LineCount = expectedCount }

                    let! closeResult = service.Close handle (context "close-line-count-window-a") |> Async.StartAsPromise
                    ignore (operationValue "Close" closeResult)
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "opens an added file with an absent previous source",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitText repository "base.txt" "base\n"
                do! writeText (NodePath.join [| repository; "added.txt" |]) "added\n"
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    let! handle, previousInfo, currentInfo, first =
                        openedWithFirstPage service (openRequest "added.txt") "added-window-a"

                    Vitest.expect(previousInfo.IsAbsent).toBe true
                    Vitest.expect(currentInfo.IsAbsent).toBe false
                    Vitest.expect(pageHasChange first).toBe true
                    let! closeResult = service.Close handle (context "close-added-window-a") |> Async.StartAsPromise
                    ignore (operationValue "Close" closeResult)
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "opens a deleted file with an absent current source",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitText repository "deleted.txt" "deleted\n"
                do! removeWorkingFile repository "deleted.txt"
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    let! handle, previousInfo, currentInfo, first =
                        openedWithFirstPage service (openRequest "deleted.txt") "deleted-window-a"

                    Vitest.expect(previousInfo.IsAbsent).toBe false
                    Vitest.expect(currentInfo.IsAbsent).toBe true
                    Vitest.expect(pageHasChange first).toBe true
                    let! closeResult = service.Close handle (context "close-deleted-window-a") |> Async.StartAsPromise
                    ignore (operationValue "Close" closeResult)
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "blocks a PNG file as binary content",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                let pngHeader = [| 137; 80; 78; 71; 13; 10; 26; 10; 0; 0; 0; 13; 73; 72; 68; 82 |]
                do! commitBytes repository "image.png" pngHeader
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! result = openUntilReady service (openRequest "image.png") "binary-window-a"

                match result with
                | OpenDiffResult.NotDiffable(DiffBlocker.Binary(_, evidence)) ->
                    Vitest.expect(evidence).toContain "PNG"
                | other -> failwith $"Expected a binary blocker, got %A{other}."

                do! session.Close() |> Async.StartAsPromise
            }
        )

        Vitest.test (
            "blocks an ASCII file that starts with a PDF signature",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                let content = "%PDF-1.4\n" + String.concat "" [| for index in 0 .. 2999 -> $"plain line {index}\n" |]
                do! commitText repository "document.pdf" content
                do! writeText (NodePath.join [| repository; "document.pdf" |]) (content + "appended\n")
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! result = openUntilReady service (openRequest "document.pdf") "pdf-window-a"

                match result with
                | OpenDiffResult.NotDiffable(DiffBlocker.Binary(_, evidence)) ->
                    Vitest.expect(evidence).toContain "PDF"
                | other -> failwith $"Expected a binary blocker, got %A{other}."

                do! session.Close() |> Async.StartAsPromise
            }
        )

        Vitest.test (
            "opens a UTF-16 LE file and reports its BOM metadata",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                let content = "first\nsecond\n"
                let encoded = Encoding.Unicode.GetBytes content
                let bytes = Array.concat [| [| 255uy; 254uy |]; encoded |] |> Array.map int
                do! commitBytes repository "utf16.txt" bytes
                let currentContent = "first\nthird\n"
                let currentEncoded = Encoding.Unicode.GetBytes currentContent
                let currentBytes = Array.concat [| [| 255uy; 254uy |]; currentEncoded |] |> Array.map int
                do! writeBytes (NodePath.join [| repository; "utf16.txt" |]) currentBytes
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    let! handle, _, currentInfo, first =
                        openedWithFirstPage service (openRequest "utf16.txt") "utf16-window-a"

                    Vitest.expect(currentInfo.Encoding).toEqual(Some "utf-16le")
                    Vitest.expect(currentInfo.EncodingWasChosen).toBe false
                    Vitest.expect(currentInfo.HasBom).toBe true
                    Vitest.expect(currentInfo.ByteLength).toEqual(int64 currentBytes.Length)
                    Vitest.expect(pageHasChange first).toBe true

                    let! closeResult = service.Close handle (context "close-utf16-window-a") |> Async.StartAsPromise
                    ignore (operationValue "Close" closeResult)
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "opens a UTF-8 file with umlauts without an encoding choice",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitBytes repository "umlauts.txt" [| 0x4B; 0xC3; 0xA4; 0x73; 0x65; 0x0A |]
                do! writeBytes (NodePath.join [| repository; "umlauts.txt" |]) [| 0x4B; 0xC3; 0xA4; 0x73; 0x65; 0x21; 0x0A |]
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! opened = openedWithFirstPage service (openRequest "umlauts.txt") "umlauts-window-a"
                let handle, _, currentInfo, firstPage = opened
                Vitest.expect(currentInfo.Encoding).toEqual(Some "utf-8")
                Vitest.expect(currentInfo.EncodingWasChosen).toBe false
                Vitest.expect(pageHasChange firstPage).toBe true
                let! closeResult = service.Close handle (context "close-umlauts-window-a") |> Async.StartAsPromise
                ignore (operationValue "Close" closeResult)
                do! session.Close() |> Async.StartAsPromise
            }
        )

        Vitest.test (
            "opens a file with bytes that are invalid as UTF-8 as Windows-1252",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitBytes repository "legacy.txt" [| 0x63; 0x61; 0x66; 0xE9; 0x0A |]
                do! writeBytes (NodePath.join [| repository; "legacy.txt" |]) [| 0x63; 0x61; 0x66; 0xE8; 0x0A |]
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! opened = openedWithFirstPage service (openRequest "legacy.txt") "legacy-window-a"
                let handle, _, currentInfo, firstPage = opened
                Vitest.expect(currentInfo.Encoding).toEqual(Some "windows-1252")
                Vitest.expect(pageHasChange firstPage).toBe true
                let! closeResult = service.Close handle (context "close-legacy-window-a") |> Async.StartAsPromise
                ignore (operationValue "Close" closeResult)
                do! session.Close() |> Async.StartAsPromise
            }
        )

        Vitest.test (
            "asks for an encoding when a late Windows-1252 byte is reached before the first page",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitBytes repository "late.csv" (lateUmlautFile -1 -1)
                do! writeBytes (NodePath.join [| repository; "late.csv" |]) (lateUmlautFile 100000 599980)
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! opened = openUntilReady service (openRequest "late.csv") "late-umlaut-open"

                match opened with
                | OpenDiffResult.NotDiffable(DiffBlocker.EncodingRequired(side, _, candidates)) ->
                    Vitest.expect(side).toEqual DiffSide.Current
                    Vitest.expect(candidates |> Array.map (fun candidate -> candidate.Encoding)).toEqual [| "utf-8"; "windows-1252" |]
                | other -> failwith $"Expected EncodingRequired, got %A{other}."

                do! session.Close() |> Async.StartAsPromise
            }
        )

        Vitest.test (
            "fails a session with an encoding mismatch when a late Windows-1252 byte follows the first page",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitBytes repository "after.csv" (lateUmlautFile -1 -1)
                do! writeBytes (NodePath.join [| repository; "after.csv" |]) (lateUmlautFile 450000 10)
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! handle, _, _, first = openedWithFirstPage service (openRequest "after.csv") "after-umlaut-window-a"
                let mutable cursor = first.NextCursor
                let mutable failure = None

                while failure.IsNone && cursor.IsSome do
                    let! result =
                        service.ReadPage { Handle = handle; Cursor = cursor.Value } (context "after-umlaut-page")
                        |> Async.StartAsPromise

                    match result with
                    | Failed error -> failure <- Some error
                    | Succeeded { Value = Resumable.Ready page } -> cursor <- page.NextCursor
                    | Succeeded { Value = Resumable.Scanning(_, continuation, _) } -> cursor <- Some continuation
                    | other -> failwith $"Unexpected ReadPage result %A{other}."

                match failure with
                | Some error ->
                    Vitest.expect(error.Code).toBe TextDiffFailureCodes.EncodingMismatch
                    Vitest.expect(error.DiffDetail.Value.Side).toEqual DiffSide.Current
                    Vitest.expect(error.DiffDetail.Value.Evidence.Contains "at byte 450000").toBe true
                | None -> failwith "The session did not fail."

                do! session.Close() |> Async.StartAsPromise
            }
        )

        Vitest.test (
            "reports an invalid UTF-8 sequence as content that is not text when the caller chose UTF-8",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitBytes repository "chosen.csv" (lateUmlautFile -1 -1)
                do! writeBytes (NodePath.join [| repository; "chosen.csv" |]) (lateUmlautFile 100000 599980)
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! opened = openUntilReady service { openRequest "chosen.csv" with CurrentEncoding = Some "utf-8" } "chosen-utf8-open"

                match opened with
                | OpenDiffResult.NotDiffable(DiffBlocker.Binary(side, evidence)) ->
                    Vitest.expect(side).toEqual DiffSide.Current
                    Vitest.expect(evidence.Contains "invalid utf-8").toBe true
                | other -> failwith $"Expected a Binary blocker, got %A{other}."

                do! session.Close() |> Async.StartAsPromise
            }
        )

        Vitest.test (
            "opens the first page while a large blob is being spooled",
            TestOptions(timeout = 600000),
            fun () -> promise {
                let! repository = newRepository ()
                let largeSize = 256L * 1024L * 1024L
                let largePath = NodePath.join [| repository; "large.txt" |]
                do! writePatternFile largePath largeSize "before-first-line" "unchanged-line-0123456789abcdef"
                do! runGitOk repository [| "add"; "--"; "large.txt" |]
                do! runGitOk repository [| "commit"; "-q"; "-m"; "large text" |]
                do! writePatternFile largePath largeSize "after-first-line" "unchanged-line-0123456789abcdef"
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let started = performanceNow ()
                let! opened = openedWithFirstPage service (openRequest "large.txt") "large-window-a"
                let elapsed = performanceNow () - started
                let handle, _, _, first = opened
                let! spoolSize = largestSpool (currentFixture ()).Supervisor.InstanceDirectory
                writeLog $"Large blob first page: %.1f{elapsed} ms, largest spool at first page: {spoolSize} of {largeSize} bytes."
                Vitest.expect(pageHasChange first).toBe true
                Vitest.expect(spoolSize > 0L).toBe true
                Vitest.expect(spoolSize < largeSize).toBe true
                let! closeResult = service.Close handle (context "close-large-window-a") |> Async.StartAsPromise
                ignore (operationValue "Close" closeResult)
                do! session.Close() |> Async.StartAsPromise
            }
        )

        Vitest.test (
            "invalidates a session after its working source changes",
            TestOptions(timeout = 180000),
            fun () -> promise {
                let! repository = newRepository ()
                let previous = String.concat "" [| for index in 0 .. 4999 -> $"old-{index}\n" |]
                let current = String.concat "" [| for index in 0 .. 4999 -> $"new-{index}\n" |]
                do! commitText repository "changing.txt" previous
                do! writeText (NodePath.join [| repository; "changing.txt" |]) current
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let mutable failure = None

                try
                    let! handle, _, _, first =
                        openedWithFirstPage service (openRequest "changing.txt") "source-change-window-a"

                    let cursor =
                        first.NextCursor
                        |> Option.defaultWith (fun () -> failwith "The source change fixture produced one complete page.")

                    do! writeText (NodePath.join [| repository; "changing.txt" |]) (current + "changed-after-open\n")
                    let! result =
                        service.ReadPage { Handle = handle; Cursor = cursor } (context "source-change-read-window-a")
                        |> Async.StartAsPromise

                    match result with
                    | Failed failure -> Vitest.expect(failure.Code).toBe TextDiffFailureCodes.SourceChanged
                    | Succeeded _
                    | PartiallySucceeded _ -> failwith "ReadPage succeeded after the source changed."
                with error ->
                    failure <- Some error

                do! closeSession session failure
            }
        )

        Vitest.test (
            "closes every handle when the Git session closes",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitText repository "close-all.txt" "before\n"
                do! writeText (NodePath.join [| repository; "close-all.txt" |]) "after\n"
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! firstHandle, _, _, first =
                    openedWithFirstPage service (openRequest "close-all.txt") "close-all-window-a-open"
                let! secondHandle, _, _, _ =
                    openedWithFirstPage service (openRequest "close-all.txt") "close-all-window-b-open"

                do! session.Close() |> Async.StartAsPromise

                let! firstRead =
                    service.ReadPage
                        { Handle = firstHandle; Cursor = first.NextCursor |> Option.defaultValue "closed" }
                        (context "close-all-window-a-read")
                    |> Async.StartAsPromise
                let! secondRead =
                    service.ReadPage
                        { Handle = secondHandle; Cursor = "closed" }
                        (context "close-all-window-b-read")
                    |> Async.StartAsPromise
                assertClosedCode "first handle" firstRead
                assertClosedCode "second handle" secondRead
            }
        )

        Vitest.test (
            "keeps Close idempotent for an opened handle",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitText repository "idempotent.txt" "before\n"
                do! writeText (NodePath.join [| repository; "idempotent.txt" |]) "after\n"
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! handle, _, _, first =
                    openedWithFirstPage service (openRequest "idempotent.txt") "idempotent-window-a"

                let! firstClose = service.Close handle (context "idempotent-close-one") |> Async.StartAsPromise
                let! secondClose = service.Close handle (context "idempotent-close-two") |> Async.StartAsPromise
                ignore (operationValue "first Close" firstClose)
                ignore (operationValue "second Close" secondClose)

                let! later =
                    service.ReadPage
                        { Handle = handle; Cursor = first.NextCursor |> Option.defaultValue "closed" }
                        (context "idempotent-read")
                    |> Async.StartAsPromise
                assertClosedCode "read after Close" later
                do! session.Close() |> Async.StartAsPromise
            }
        )

        Vitest.test (
            "fails a ReadPage when its worker terminates and removes the worker directory",
            TestOptions(timeout = 300000),
            fun () -> promise {
                let! repository = newRepository ()
                let size = 128L * 1024L * 1024L
                let filePath = NodePath.join [| repository; "termination.txt" |]
                do! writePatternFile filePath size "before-first-line" "termination-line-0123456789abcdef"
                do! runGitOk repository [| "add"; "--"; "termination.txt" |]
                do! runGitOk repository [| "commit"; "-q"; "-m"; "termination source" |]
                do! writePatternFile filePath size "after-first-line" "termination-line-0123456789abcdef"
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let! handle, _, _, first =
                    openedWithFirstPage service (openRequest "termination.txt") "terminate-window-a"
                let cursor =
                    first.NextCursor
                    |> Option.defaultWith (fun () -> failwith "The termination fixture produced one complete page.")
                let transport =
                    let transports = (currentFixture ()).Transports
                    transports[transports.Count - 1]
                let eventStart = (currentFixture ()).Events.Count
                let started = performanceNow ()
                let reading =
                    service.ReadPage { Handle = handle; Cursor = cursor } (context "terminate-read-window-a")
                    |> Async.StartAsPromise

                do! delay 25
                do! transport.Terminate()
                let! result = reading
                let elapsed = performanceNow () - started

                match result with
                | Failed failure ->
                    Vitest.expect(failure.Code).toBe TextDiffFailureCodes.WorkerFailed
                    writeLog $"Worker termination during ReadPage: %.1f{elapsed} ms."
                | Succeeded _
                | PartiallySucceeded _ -> failwith "ReadPage completed after the worker terminated."

                do!
                    waitFor "the terminated worker directory cleanup" 10000.0 (fun () ->
                        Promise.lift (hasWorkerDirectoryDeletedSince (currentFixture ()).Events eventStart))

                do! session.Close() |> Async.StartAsPromise
            }
        )
)
