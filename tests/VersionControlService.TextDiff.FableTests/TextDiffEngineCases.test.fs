namespace VersionControlService.TextDiff.FableTests

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.TextDiff
open VersionControlService.TextDiff.Tests

module TextDiffEngineCasesTests =
    let private benchmarkSize = 64 * 1024 * 1024

    [<Import("setImmediate", "node:timers")>]
    let private setImmediate (callback: unit -> unit) : unit = jsNative

    let private trampolineYield () =
        Async.FromContinuations(fun (success, _, _) -> setImmediate success)

    [<Emit("$0[$1] = $2")>]
    let private writeBenchmarkByte (data: byte[]) (index: int) (value: int) : unit = jsNative

    let private createMeter () =
        let clock = ManualClock 0.0
        Meter.create
            (clock :> IClock)
            { Limits.defaults with MaxUnits = 2_147_483_647; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }

    /// Runs `work` three times and reports the best rate of the last two runs in MB/s, so that JIT
    /// warm-up does not count against the measured code.
    let private bestRate (size: int) (work: unit -> 'T) =
        let mutable best = 0.0
        let mutable result = work ()
        for _ in 1..2 do
            let started = BrowserClock.nowMs()
            result <- work ()
            let elapsed = max 1.0 (BrowserClock.nowMs() - started)
            best <- max best (float size / 1_000_000.0 * 1_000.0 / elapsed)
        result, best

    let private scanThroughput encoding (data: byte[]) =
        let batch = LineBatch()
        bestRate data.Length (fun () ->
            let state = Scanner.create encoding 0L
            Scanner.scanChunk state data 0 data.Length true (createMeter ()) batch ignore)

    let private noObservation (_: float) (_: int) (_: int) (_: int) (_: float) (_: float) = ()

    let private commonRunThroughput encoding (left: byte[]) (right: byte[]) =
        bestRate (left.Length * 2) (fun () ->
            let mutable position = 0
            let mutable pendingCR = false
            let mutable lines = 0

            while position < left.Length do
                let run = CommonRun.findObserved encoding (float position) (float position) pendingCR 0.0 0.0 noObservation noObservation left position right position (left.Length - position)
                if run.Length = 0 then failwith "The common run did not advance."
                position <- position + run.Length
                pendingCR <- run.PendingCR
                lines <- lines + run.Lines

            lines)

    let private asciiLines size =
        let data = Array.create size 0x61uy
        let mutable lineEnd = 39

        while lineEnd < size do
            writeBenchmarkByte data lineEnd 0x0A
            lineEnd <- lineEnd + 40

        data

    let private repeatWithAsciiTail size (pattern: byte[]) =
        let data = Array.zeroCreate<byte> size
        let mutable offset = 0

        while offset + pattern.Length <= size do
            Array.blit pattern 0 data offset pattern.Length
            offset <- offset + pattern.Length

        while offset < size do
            data[offset] <- 0x61uy
            offset <- offset + 1

        data

    let private repeatBytes size (pattern: byte[]) =
        let data = Array.zeroCreate<byte> size
        let mutable offset = 0

        while offset < size do
            let copied = min pattern.Length (size - offset)
            Array.blit pattern 0 data offset copied
            offset <- offset + copied

        data

    let private singleLongLineData size =
        let firstLine = [| 0x66uy; 0x69uy; 0x72uy; 0x73uy; 0x74uy; 0x2Duy; 0x6Cuy; 0x69uy; 0x6Euy; 0x65uy; 0x0Auy |]
        let pattern = [| 0x77uy; 0x6Fuy; 0x72uy; 0x64uy; 0x20uy |]
        let previous = Array.zeroCreate<byte> (firstLine.Length + size)
        Array.blit firstLine 0 previous 0 firstLine.Length
        Array.blit (repeatBytes size pattern) 0 previous firstLine.Length size
        let current = Array.copy previous
        let changedWord = (size / pattern.Length - 1) * pattern.Length
        current[firstLine.Length + changedWord] <- 0x62uy
        previous, current

    let private utf8Data () =
        let cycle =
            Array.concat [
                Array.create 14 0x61uy
                [| 0xC3uy; 0xA9uy; 0xC3uy; 0xA9uy; 0xC3uy; 0xA9uy |]
                [| 0xE2uy; 0x82uy; 0xACuy; 0xE2uy; 0x82uy; 0xACuy; 0xE2uy; 0x82uy; 0xACuy |]
            ]
        let pattern = Array.zeroCreate<byte> (cycle.Length * 16)

        for repeat = 0 to 15 do
            Array.blit cycle 0 pattern (repeat * cycle.Length) cycle.Length

        repeatWithAsciiTail benchmarkSize pattern

    let private utf16LeData () =
        let units =
            Array.concat [
                Array.create 14 0x0061
                [| 0x00E9; 0x00E9; 0x00E9 |]
                [| 0x20AC; 0x20AC; 0x20AC |]
            ]
        let pattern = Array.zeroCreate<byte> (units.Length * 2)

        for index = 0 to units.Length - 1 do
            let value = units[index]
            pattern[index * 2] <- byte value
            pattern[index * 2 + 1] <- byte (value >>> 8)

        repeatBytes benchmarkSize pattern

    let private reportScan label encoding (data: byte[]) =
        let result, rate = scanThroughput encoding data
        Vitest.log ($"{label}: %.1f{rate} MB/s")
        if result.Status <> EndOfInput then failwith $"{label} did not finish the 64 MiB input."
        Async.StartAsPromise(async.Return())

    let private reportCommonRun label (data: byte[]) =
        let _, rate = commonRunThroughput TextEncoding.Utf8 data (Array.copy data)
        Vitest.log ($"{label}: %.1f{rate} MB/s")
        Async.StartAsPromise(async.Return())

    /// Three 64 KiB samples taken from the start, the middle and the end of a 256 MiB source.
    let private classificationSamples (pattern: byte[]) =
        let length = 65_536 / pattern.Length * pattern.Length
        let bytes = repeatBytes length pattern
        let sourceLength = 256L * 1024L * 1024L
        let samples =
            [| for offset in [| 0L; sourceLength / 2L; sourceLength - int64 length |] ->
                ({ BufferOffset = offset; Bytes = bytes; SampleOffset = 0; SampleLength = length }: ClassificationSample) |]
        sourceLength, samples

    let private reportClassification label (pattern: byte[]) =
        let sourceLength, samples = classificationSamples pattern
        let started = BrowserClock.nowMs()
        let mutable result = Classification.classify sourceLength samples
        let first = BrowserClock.nowMs() - started
        let mutable best = first

        for _ in 1..3 do
            let repeated = BrowserClock.nowMs()
            result <- Classification.classify sourceLength samples
            best <- min best (BrowserClock.nowMs() - repeated)

        match result with
        | BinaryEvidence evidence -> failwith $"{label} was classified as binary: {evidence}."
        | _ -> ()

        Vitest.log ($"{label}: %.2f{first} ms for the first run, %.2f{best} ms best of the rest, for three 64 KiB samples")
        Async.StartAsPromise(async.Return())

    let private benchmarkSessionConfig sessionId contextLines pageRows chunkBytes = {
        SessionConfig.defaults sessionId with
            ContextLines = contextLines
            PageMaxRows = pageRows
            PageMaxBytes = 512 * 1024
            PageMaxFragments = 32
            WindowMaxLines = 65_536
            WindowMaxBytes = 32 * 1024 * 1024
            CommonChunkBytes = chunkBytes
            MyersStepsPerGap = 1_000_000
            HashMaskForTesting = None
            Limits = { Limits.defaults with MaxUnits = 2_147_483_647; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
    }

    let private benchmarkSource (data: byte[]) = {
        Source = Some(MemoryByteSource(data) :> IByteSource)
        Encoding = "utf-8"
        BomLength = 0
        ByteLength = int64 data.Length
    }

    /// Counts the reads that the session makes on one source.
    type private CountingByteSource(data: byte[]) =
        let inner = MemoryByteSource(data) :> IByteSource
        let mutable reads = 0

        member _.Reads = reads

        interface IByteSource with
            member _.IsComplete() = inner.IsComplete()
            member _.ReadAt position buffer offset count =
                reads <- reads + 1
                inner.ReadAt position buffer offset count

    let private countedSource (source: CountingByteSource) (data: byte[]) = {
        Source = Some(source :> IByteSource)
        Encoding = "utf-8"
        BomLength = 0
        ByteLength = int64 data.Length
    }

    let private startRequest work = work |> Async.StartAsPromise |> Async.AwaitPromise

    let rec private finishFirstPage (session: TextDiffSession) (result: EngineResult<Resumable<DiffPage>>) = async {
        match result with
        | EngineResult.Ok(Resumable.Ready page) -> return page
        | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
            let! next = startRequest (session.ReadPage continuation (fun () -> false))
            return! startRequest (finishFirstPage session next)
        | EngineResult.Failed(code, message, detail) ->
            return failwith $"The measured session failed with {code}: {message}. Detail: {detail}."
        | EngineResult.Canceled -> return failwith "The measured session was canceled."
    }

    let private readNextPage (session: TextDiffSession) cursor = async {
        let! result = session.ReadPage cursor (fun () -> false)
        return! startRequest (finishFirstPage session result)
    }

    let rec private finishSessionOutput (session: TextDiffSession) (page: DiffPage) = async {
        if page.OutputComplete then return ()
        else
            match page.NextCursor with
            | Some cursor ->
                let! nextPage = readNextPage session cursor |> Async.StartAsPromise |> Async.AwaitPromise
                return! finishSessionOutput session nextPage
            | None -> return failwith "The measured scan stopped before output was complete."
    }

    [<Emit("Promise.resolve($0)")>]
    let private promiseResolve (value: 'T) : JS.Promise<'T> = jsNative

    [<Emit("Promise.resolve(undefined)")>]
    let private promiseResolveUnit () : JS.Promise<unit> = jsNative

    [<Emit("Promise.reject(new Error($0))")>]
    let private promiseReject<'T> (message: string) : JS.Promise<'T> = jsNative

    [<Emit("$0.then($1)")>]
    let private promiseThen (source: JS.Promise<'T>) (continuation: 'T -> JS.Promise<'U>) : JS.Promise<'U> = jsNative

    let rec private finishFirstPagePromise (session: TextDiffSession) (result: EngineResult<Resumable<DiffPage>>) : JS.Promise<DiffPage> =
        match result with
        | EngineResult.Ok(Resumable.Ready page) -> promiseResolve page
        | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
            promiseThen (Async.StartAsPromise(session.ReadPage continuation (fun () -> false))) (finishFirstPagePromise session)
        | EngineResult.Failed(code, message, detail) -> promiseReject $"The measured session failed with {code}: {message}. Detail: {detail}."
        | EngineResult.Canceled -> promiseReject "The measured session was canceled."

    let rec private finishSessionOutputPromise (observe: DiffPage -> unit) (session: TextDiffSession) (page: DiffPage) : JS.Promise<unit> =
        observe page
        if page.OutputComplete then promiseResolveUnit ()
        else
            match page.NextCursor with
            | Some cursor ->
                promiseThen
                    (Async.StartAsPromise(session.ReadPage cursor (fun () -> false)))
                    (fun result ->
                        promiseThen (finishFirstPagePromise session result) (fun nextPage -> finishSessionOutputPromise observe session nextPage))
            | None -> promiseReject "The measured scan stopped before output was complete."

    /// Builds lines of a fixed width that start with a marker letter and an eight digit counter, so every line differs.
    let private numberedLines (marker: byte) (lineBytes: int) (count: int) =
        let data = Array.create (count * lineBytes) 0x61uy

        for line = 0 to count - 1 do
            let start = line * lineBytes
            writeBenchmarkByte data start (int marker)
            let mutable value = line

            for digit = 8 downto 1 do
                writeBenchmarkByte data (start + digit) (0x30 + value % 10)
                value <- value / 10

            writeBenchmarkByte data (start + lineBytes - 1) 0x0A

        data

    // The diff worker threads hop the async trampoline through setImmediate, and these tests do the same, so the
    // large alignment cases run at the speed they have in the app.
    AsyncTrampoline.switchToSetImmediate ()

    let private createBenchmarkSession config previous current : Async<TextDiffSession> = async {
        let clock = ManualClock 0.0
        let host = Host.createInMemory (clock :> IClock)
        return! startRequest (TextDiffSession.create host (Ledger()) config (fun _ -> 1) previous current)
    }

    Vitest.describe (
        "Text diff engine shared cases",
        fun () ->
            for name, run in TextDiffEngineCases.cases do
                Vitest.it(name, fun () -> Async.StartAsPromise(run ()))
            for name, run in TextDiffSessionCases.cases do
                Vitest.it(name, fun () -> Async.StartAsPromise(run ()))
            for name, run in TextDiffStreamingCases.cases do
                Vitest.it(name, fun () -> Async.StartAsPromise(run ()))
            for name, run in TextDiffResyncCases.cases do
                Vitest.it(name, fun () -> Async.StartAsPromise(run ()))
            for name, run in RowLinesCases.cases do
                Vitest.it(name, fun () -> Async.StartAsPromise(run ()))
    )

    Vitest.it (
        "scans a 64 MiB ASCII buffer with 40-byte lines",
        fun () -> reportScan "ASCII scanner throughput" TextEncoding.Utf8 (asciiLines benchmarkSize)
    )

    Vitest.it (
        "scans a 64 MiB UTF-8 buffer with mixed character widths",
        fun () -> reportScan "UTF-8 scanner throughput" TextEncoding.Utf8 (utf8Data ())
    )

    Vitest.it (
        "scans a 64 MiB UTF-16 LE buffer",
        fun () -> reportScan "UTF-16 LE scanner throughput" TextEncoding.Utf16LE (utf16LeData ())
    )

    Vitest.it (
        "finds the common run of two identical 64 MiB ASCII buffers",
        fun () -> reportCommonRun "Phase 1 ASCII common run throughput" (asciiLines benchmarkSize)
    )

    Vitest.it (
        "finds the common run of two identical 64 MiB mixed UTF-8 buffers",
        fun () -> reportCommonRun "UTF-8 common run throughput" (utf8Data ())
    )

    Vitest.it (
        "measures a 64 MiB identical session scan",
        fun () -> Async.StartAsPromise(async {
            let size = 64 * 1024 * 1024
            let data = asciiLines size
            let source = benchmarkSource data
            let config = benchmarkSessionConfig "benchmark-identical" 3 1_000 (8 * 1024 * 1024)
            let started = BrowserClock.nowMs()
            let! session = createBenchmarkSession config source source
            let! first = startRequest (session.FirstPage(fun () -> false))
            let! page = startRequest (finishFirstPage session first)
            let elapsed = max 1.0 (BrowserClock.nowMs() - started)
            if not page.OutputComplete then failwith "The identical scan did not complete on its first page."
            let rate = 2.0 * float size / 1_000_000.0 * 1_000.0 / elapsed
            Vitest.log ($"Identical 64 MiB session scan: %.1f{rate} MB/s across both sources")
            do! session.Close()
        })
    )

    let private measureSingleLongLine size =
        Async.StartAsPromise(async {
            let previous, current = singleLongLineData size
            let config = SessionConfig.defaults $"benchmark-single-line-{size}"
            let clock = { new IClock with member _.NowMs() = BrowserClock.nowMs() }
            let baseHost = Host.createInMemory clock
            let host = { baseHost with Yield = trampolineYield }
            let started = BrowserClock.nowMs()
            let! session = startRequest (TextDiffSession.create host (Ledger()) config (fun _ -> 1) (benchmarkSource previous) (benchmarkSource current))
            let! first = startRequest (session.FirstPage(fun () -> false))
            let! page = startRequest (finishFirstPage session first)
            let firstPageMs = max 0.0 (BrowserClock.nowMs() - started)
            if page.Parts.Length = 0 then failwith "The single-line change produced an empty first page."
            do! finishSessionOutput session page
            let elapsed = max 1.0 (BrowserClock.nowMs() - started)
            let mebibytes = size / (1024 * 1024)
            Vitest.log ($"{mebibytes} MiB single-line change: first page %.1f{firstPageMs} ms, total %.1f{elapsed} ms")
            do! session.Close()
        })

    Vitest.it (
        "measures time to the first page for three edits in a 1 MiB file",
        fun () -> Async.StartAsPromise(async {
            let size = 1024 * 1024
            let previous = asciiLines size
            let current = Array.copy previous
            for lineIndex in [| 500; 10_000; 20_000 |] do
                current[lineIndex * 40] <- 0x62uy
            let config = benchmarkSessionConfig "benchmark-first-page" 3 1_000 (8 * 1024 * 1024)
            let started = BrowserClock.nowMs()
            let! session = createBenchmarkSession config (benchmarkSource previous) (benchmarkSource current)
            let! first = startRequest (session.FirstPage(fun () -> false))
            let! page = startRequest (finishFirstPage session first)
            let elapsed = max 0.0 (BrowserClock.nowMs() - started)
            if page.Parts.Length = 0 then failwith "The first page has no edit rows."
            Vitest.log ($"1 MiB file with three edits, first page: %.1f{elapsed} ms")
            do! session.Close()
        })
    )

    Vitest.it (
        "measures total time and throughput for a 64 MiB file with three early edits",
        fun () -> Async.StartAsPromise(async {
            let size = 64 * 1024 * 1024
            let previous = asciiLines size
            let current = Array.copy previous
            for lineIndex in [| 10; 100; 1_000 |] do
                current[lineIndex * 40] <- 0x62uy
            let config = benchmarkSessionConfig "benchmark-early-edits-64m" 3 1_000 (8 * 1024 * 1024)
            let started = BrowserClock.nowMs()
            let! session = createBenchmarkSession config (benchmarkSource previous) (benchmarkSource current)
            let! first = startRequest (session.FirstPage(fun () -> false))
            let! page = startRequest (finishFirstPage session first)
            if page.Parts.Length = 0 then failwith "The first page has no edit rows."
            do! finishSessionOutput session page
            let elapsed = max 1.0 (BrowserClock.nowMs() - started)
            let rate = 2.0 * float size / 1_000_000.0 * 1_000.0 / elapsed
            Vitest.log ($"64 MiB file with three early edits: %.1f{elapsed} ms, %.1f{rate} MB/s across both sources")
            do! session.Close()
        })
    )

    Vitest.it (
        "measures total time and throughput for a 64 MiB file with an 8 MiB insertion near the start",
        fun () -> Async.StartAsPromise(async {
            let lineBytes = 40
            let lineCount = benchmarkSize / lineBytes
            let previous = numberedLines 0x70uy lineBytes lineCount
            let cutLine = 1_000
            let insertion = numberedLines 0x69uy 256 (8 * 1024 * 1024 / 256)
            let cutByte = cutLine * lineBytes
            let current = Array.zeroCreate<byte> (previous.Length + insertion.Length)
            Array.blit previous 0 current 0 cutByte
            Array.blit insertion 0 current cutByte insertion.Length
            Array.blit previous cutByte current (cutByte + insertion.Length) (previous.Length - cutByte)
            let ledger = Ledger()
            let config = benchmarkSessionConfig "benchmark-insertion-64m" 3 1_000 (8 * 1024 * 1024)
            let started = BrowserClock.nowMs()
            let previousSource = CountingByteSource previous
            let currentSource = CountingByteSource current
            let! session = startRequest (TextDiffSession.create (Host.createInMemory (ManualClock 0.0 :> IClock)) ledger config (fun _ -> 1) (countedSource previousSource previous) (countedSource currentSource current))
            let! first = startRequest (session.FirstPage(fun () -> false))
            let! page = startRequest (finishFirstPage session first)
            if page.Parts.Length = 0 then failwith "The first page has no edit rows."
            do! finishSessionOutput session page
            let elapsed = max 1.0 (BrowserClock.nowMs() - started)
            let rate = float (previous.Length + current.Length) / 1_000_000.0 * 1_000.0 / elapsed
            // The equal-byte phase after the insertion measured 282 reads. A session that stays in window or resync mode measured about 13,000.
            let reads = previousSource.Reads + currentSource.Reads
            if reads > 600 then failwith $"The session made {reads} reads, so it did not return to the equal-byte phase after the insertion."
            Vitest.log ($"64 MiB file with an 8 MiB insertion near the start: %.1f{elapsed} ms, %.1f{rate} MB/s across both sources")
            do! session.Close()
        })
    )

    /// Measures a whole session with the default budgets and logs the time to the first page and the total time.
    let private reportSessionRun label sessionId (previous: byte[]) (current: byte[]) (observe: DiffPage -> unit) (checkRun: Ledger -> int -> unit) =
        let ledger = Ledger()
        let config = { SessionConfig.defaults sessionId with Limits = { Limits.defaults with MaxUnits = 2_147_483_647; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 } }
        let previousSource = CountingByteSource previous
        let currentSource = CountingByteSource current
        let started = BrowserClock.nowMs()
        let create = TextDiffSession.create (Host.createInMemory (ManualClock 0.0 :> IClock)) ledger config (fun _ -> 1) (countedSource previousSource previous) (countedSource currentSource current)
        promiseThen (Async.StartAsPromise create) (fun session ->
            promiseThen (Async.StartAsPromise(session.FirstPage(fun () -> false))) (fun result ->
                promiseThen (finishFirstPagePromise session result) (fun page ->
                    let firstPageMs = BrowserClock.nowMs() - started
                    if page.Parts.Length = 0 then promiseReject "The first page has no edit rows."
                    else
                        promiseThen (finishSessionOutputPromise observe session page) (fun () ->
                            let elapsed = max 1.0 (BrowserClock.nowMs() - started)
                            checkRun ledger (previousSource.Reads + currentSource.Reads)
                            Vitest.log ($"{label}: first page %.1f{firstPageMs} ms, total %.1f{elapsed} ms")
                            promiseThen (Async.StartAsPromise(session.Close())) (fun () -> promiseResolveUnit ())
                        )
                )
            )
        )

    /// Adds the rows of a page to a tally of added rows, removed rows and lines in unaligned regions.
    let private tallyPage (tally: int[]) (page: DiffPage) =
        for part in page.Parts do
            match part with
            | DiffPart.Hunk fragment ->
                match fragment.Body with
                | HunkBody.AlignedRows rows ->
                    for row in rows do
                        if row.Kind = DiffRowKind.Added then tally[0] <- tally[0] + 1
                        elif row.Kind = DiffRowKind.Removed then tally[1] <- tally[1] + 1
                | HunkBody.UnalignedSides(previous, current) -> tally[2] <- tally[2] + previous.Length + current.Length
            | _ -> ()

    let private measureMiddleInsertion () =
        let lineBytes = 40
        let lineCount = benchmarkSize / lineBytes
        let previous = numberedLines 0x70uy lineBytes lineCount
        let cutLine = lineCount / 2
        let insertedLines = 8 * 1024 * 1024 / lineBytes
        let insertion = numberedLines 0x69uy lineBytes insertedLines
        let cutByte = cutLine * lineBytes
        let current = Array.zeroCreate<byte> (previous.Length + insertion.Length)
        Array.blit previous 0 current 0 cutByte
        Array.blit insertion 0 current cutByte insertion.Length
        Array.blit previous cutByte current (cutByte + insertion.Length) (previous.Length - cutByte)
        // The insertion is larger than one alignment window, so the search has to find the unchanged lines after it.
        let tally = Array.zeroCreate<int> 3
        let check (ledger: Ledger) (reads: int) =
            // The equal-byte phase after the insertion measured 512 reads. A session that stays in window or resync mode measured about 7,000.
            if reads > 1_000 then failwith $"The session made {reads} reads, so it did not return to the equal-byte phase after the insertion."
            if tally[0] <> insertedLines || tally[1] <> 0 || tally[2] <> 0 then
                failwith $"The insertion shows {tally[0]} added rows, {tally[1]} removed rows and {tally[2]} unaligned lines, expected {insertedLines} added rows and nothing else."
        reportSessionRun "64 MiB file with an 8 MiB insertion in the middle" "benchmark-insertion-middle" previous current (tallyPage tally) check

    let private measureFullRewrite () =
        let lineBytes = 40
        let lineCount = benchmarkSize / lineBytes
        let previous = numberedLines 0x70uy lineBytes lineCount
        let current = numberedLines 0x71uy lineBytes lineCount
        reportSessionRun "64 MiB full rewrite" "benchmark-rewrite" previous current ignore (fun _ _ -> ())

    Vitest.itWithTimeout("measures a 64 MiB file with an 8 MiB insertion of 40-byte lines in the middle", measureMiddleInsertion, 1_800_000)

    Vitest.itWithTimeout("measures a 64 MiB full rewrite of 40-byte lines", measureFullRewrite, 1_800_000)

    Vitest.it (
        "classifies three 64 KiB samples of mixed UTF-8 text",
        fun () ->
            let pattern =
                Array.concat [
                    Array.create 14 0x61uy
                    [| 0xC3uy; 0xA9uy; 0xC3uy; 0xA9uy; 0xC3uy; 0xA9uy |]
                    [| 0xE2uy; 0x82uy; 0xACuy; 0xE2uy; 0x82uy; 0xACuy; 0xE2uy; 0x82uy; 0xACuy |]
                    [| 0xF0uy; 0x9Fuy; 0x98uy; 0x80uy; 0x0Auy |]
                ]
            reportClassification "Classification of UTF-8 text" pattern
    )

    Vitest.it (
        "classifies three 64 KiB samples of Windows-1252 text",
        fun () ->
            let pattern =
                Array.concat [ Array.create 20 0x61uy; [| 0xE9uy; 0xE9uy; 0x80uy; 0x93uy; 0x94uy; 0xFCuy; 0x0Auy |] ]
            reportClassification "Classification of Windows-1252 text" pattern
    )

    Vitest.it (
        "classifies three 64 KiB samples of UTF-16 LE text",
        fun () ->
            let units = Array.concat [ Array.create 20 0x0061; [| 0x00E9; 0x20AC; 0xD83D; 0xDE00; 0x000A |] ]
            let pattern = Array.zeroCreate<byte> (units.Length * 2)

            for index = 0 to units.Length - 1 do
                pattern[index * 2] <- byte units[index]
                pattern[index * 2 + 1] <- byte (units[index] >>> 8)

            reportClassification "Classification of UTF-16 LE text" pattern
    )

    Vitest.itWithTimeout("measures a 1 MiB single line change", (fun () -> measureSingleLongLine (1024 * 1024)), 600_000)
    Vitest.itWithTimeout("measures a 4 MiB single line change", (fun () -> measureSingleLongLine (4 * 1024 * 1024)), 600_000)
    Vitest.itWithTimeout("measures a 16 MiB single line change", (fun () -> measureSingleLongLine (16 * 1024 * 1024)), 600_000)
