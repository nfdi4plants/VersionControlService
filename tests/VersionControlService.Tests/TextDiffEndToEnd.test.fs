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

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodePositionalFile = VersionControlService.Runtime.Node.PositionalFile
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeWorkerThreads = VersionControlService.Runtime.Node.WorkerThreads
module TextDiffPool = VersionControlService.Git.TextDiff.TextDiffPool
module TextDiffSupervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor

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
    let chunk = Array.init (64 * 1024) (fun index -> repeated[index % repeated.Length])
    do! NodePositionalFile.removeWithRetry filePath 1 0
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
        do! NodePositionalFile.removeWithRetry directory 20 100
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

let private openUntilReady (service: TextDiffService) (initial: OpenDiffRequest) (operationName: string) = promise {
    let mutable request = initial
    let mutable value = None

    while value.IsNone do
        let! result = service.Open request (context operationName) |> Async.StartAsPromise
        let outcome = operationValue "Open" result

        match outcome with
        | Resumable.Ready opened -> value <- Some opened
        | Resumable.Scanning(_, continuation, _) -> request <- { request with Continuation = Some continuation }

    return value.Value
}

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
                            SessionsPerWorker = 4
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
            "reopens an ambiguous file after choosing UTF-8",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! repository = newRepository ()
                do! commitBytes repository "ambiguous.txt" [| 0xC3; 0xA9; 0x0A |]
                do! writeBytes (NodePath.join [| repository; "ambiguous.txt" |]) [| 0xC3; 0xA8; 0x0A |]
                let session = createSession (currentFixture ()).Pool repository
                let service = serviceFor session
                let firstRequest = openRequest "ambiguous.txt"
                let! first = openUntilReady service firstRequest "ambiguous-window-a"
                let token, candidates =
                    match first with
                    | OpenDiffResult.NotDiffable(DiffBlocker.EncodingRequired(_, token, candidates)) -> token, candidates
                    | other -> failwith $"Expected an encoding choice, got %A{other}."

                Vitest.expect(candidates |> Array.exists (fun candidate -> candidate.Encoding = "utf-8")).toBe true
                Vitest.expect(candidates |> Array.exists (fun candidate -> candidate.Encoding = "windows-1252")).toBe true

                let selected =
                    {
                        firstRequest with
                            Preparation = Some token
                            PreviousEncoding = Some "utf-8"
                            CurrentEncoding = Some "utf-8"
                    }
                let! opened = openedWithFirstPage service selected "ambiguous-window-a-reopen"
                let handle, _, currentInfo, firstPage = opened
                Vitest.expect(currentInfo.Encoding).toEqual(Some "utf-8")
                Vitest.expect(currentInfo.EncodingWasChosen).toBe true
                Vitest.expect(pageHasChange firstPage).toBe true
                let! closeResult = service.Close handle (context "close-ambiguous-window-a") |> Async.StartAsPromise
                ignore (operationValue "Close" closeResult)
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
                writeLog $"Large blob first page: {elapsed:F1} ms, largest spool at first page: {spoolSize} of {largeSize} bytes."
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
                    writeLog $"Worker termination during ReadPage: {elapsed:F1} ms."
                | Succeeded _
                | PartiallySucceeded _ -> failwith "ReadPage completed after the worker terminated."

                do!
                    waitFor "the terminated worker directory cleanup" 10000.0 (fun () ->
                        Promise.lift (hasWorkerDirectoryDeletedSince (currentFixture ()).Events eventStart))

                do! session.Close() |> Async.StartAsPromise
            }
        )
)
