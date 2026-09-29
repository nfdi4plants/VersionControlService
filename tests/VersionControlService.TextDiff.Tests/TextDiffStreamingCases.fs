namespace VersionControlService.TextDiff.Tests

open System
open System.Text
open VersionControlService.Abstractions
open VersionControlService.TextDiff

module TextDiffStreamingCases =
    type private SourceLine = { Text: string; Ending: LineEnding }

    let private endingText = function
        | LineEnding.NoEnding -> ""
        | LineEnding.LF -> "\n"
        | LineEnding.CRLF -> "\r\n"
        | LineEnding.CR -> "\r"

    let private encodeLines (lines: SourceLine array) =
        lines
        |> Array.map (fun line -> line.Text + endingText line.Ending)
        |> String.concat ""
        |> Encoding.UTF8.GetBytes

    let private sourceSpec (bytes: byte[]) = {
        Source = Some(MemoryByteSource(bytes) :> IByteSource)
        Encoding = "utf-8"
        BomLength = 0
        ByteLength = int64 bytes.Length
    }

    let private config sessionId windowLines myersSteps limits hashMask = {
        SessionConfig.defaults sessionId with
            ContextLines = 1
            PageMaxRows = 2_048
            PageMaxBytes = 512 * 1024
            PageMaxFragments = 128
            WindowMaxLines = windowLines
            WindowMaxBytes = 4 * 1024 * 1024
            CommonChunkBytes = 64 * 1024
            MyersStepsPerGap = myersSteps
            Limits = limits
            HashMaskForTesting = hashMask
    }

    let private openSession ledger sessionConfig previous current : Async<TextDiffSession> = async {
        let clock = ManualClock 0.0
        let host = Host.createInMemory (clock :> IClock)
        return! TextDiffSession.create host ledger sessionConfig (fun _ -> 1) previous current
    }

    let private resolvePageAsync (session: TextDiffSession) cancel (initial: EngineResult<Resumable<DiffPage>>) = async {
        let mutable result = initial
        let mutable scans = 0
        let mutable page = None
        let mutable attempts = 0

        while page.IsNone do
            attempts <- attempts + 1
            if attempts > 100_000 then failwith "The session did not finish after many continuations."
            match result with
            | EngineResult.Ok(Resumable.Ready value) -> page <- Some value
            | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                scans <- scans + 1
                let! next = session.ReadPage continuation cancel
                result <- next
            | EngineResult.Failed(code, message, detail) ->
                failwith $"The session failed with {code}: {message}. Detail: {detail}."
            | EngineResult.Canceled -> failwith "The session canceled an uncanceled request."

        return page.Value, scans
    }

    let private readAll (session: TextDiffSession) = async {
        let pages = ResizeArray<DiffPage>()
        let! first = session.FirstPage(fun () -> false)
        let mutable result = first
        let mutable scans = 0
        let mutable doneReading = false

        while not doneReading do
            let! page, pageScans = resolvePageAsync session (fun () -> false) result
            scans <- scans + pageScans
            pages.Add page
            match page.NextCursor with
            | None -> doneReading <- true
            | Some cursor ->
                let! next = session.ReadPage cursor (fun () -> false)
                result <- next

        return pages.ToArray(), scans
    }

    let private allParts (pages: DiffPage array) = pages |> Array.collect (fun page -> page.Parts)

    let private rebuild expected parts previousSide =
        let output = ResizeArray<SourceLine>()

        let addRange (lines: SourceLine array) (range: LineRange) =
            if range.Start < 0L || range.Count < 0L || range.Start + range.Count > int64 lines.Length then
                failwith $"The session returned an invalid equal range {range}."
            for index = int range.Start to int (range.Start + range.Count) - 1 do
                output.Add lines[index]

        let addDiffLine (line: DiffLine) =
            if line.Slice.OffsetUtf16 <> 0L then failwith "A displayed line starts at a nonzero slice offset."
            match line.Slice.TotalUtf16 with
            | Some total when total = int64 line.Slice.Text.Length -> ()
            | Some total -> failwith $"A displayed line has {line.Slice.Text.Length} units and reports {total}."
            | None -> failwith "A complete diff line has no known length."
            output.Add { Text = line.Slice.Text; Ending = line.Ending }

        for part in parts do
            match part with
            | DiffPart.HiddenEqual gap ->
                addRange expected (if previousSide then gap.PreviousRange else gap.CurrentRange)
            | DiffPart.Hunk fragment ->
                match fragment.Body with
                | HunkBody.AlignedRows values ->
                    for row in values do
                        let selected = if previousSide then row.Previous else row.Current
                        selected |> Option.iter addDiffLine
                | HunkBody.UnalignedSides(previous, current) ->
                    let selected = if previousSide then previous else current
                    for line in selected do addDiffLine line
            | DiffPart.ExpandedContext _ -> failwith "The session returned an unsupported expanded context part."

        output.ToArray()

    let private checkOracle previous current pages =
        let parts = allParts pages
        Check.sequence previous (rebuild previous parts true) "The page stream covers every previous line exactly once."
        Check.sequence current (rebuild current parts false) "The page stream covers every current line exactly once."

    let private makeLines count prefix =
        Array.init count (fun index -> { Text = prefix + string index; Ending = LineEnding.LF })

    let private asciiFile lineCount =
        let bytes = Array.create (lineCount * 4) 0x61uy
        for line = 0 to lineCount - 1 do bytes[line * 4 + 3] <- 0x0Auy
        bytes

    /// Runs a large file with a shifting edit near the start and a small edit near the end, then checks that the
    /// equal-byte phase consumed almost every equal byte after the shift.
    let private shiftedTailCase (sessionId: string) (shift: SourceLine array -> SourceLine array) = async {
        let previous = makeLines 200_000 "line-"
        let current = shift previous
        current[current.Length - 100] <- { current[current.Length - 100] with Text = "edit-near-end" }
        let previousBytes = encodeLines previous
        let currentBytes = encodeLines current
        let ledger = Ledger()
        let limits = { Limits.defaults with MaxUnits = 2_147_483_647; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
        let sessionConfig = config sessionId 8_192 1_000_000 limits None
        let! session = openSession ledger sessionConfig (sourceSpec previousBytes) (sourceSpec currentBytes)
        let! pages, _ = readAll session
        checkOracle previous current pages
        let prefixBytes = float (encodeLines previous[.. 99]).Length
        let equalAfterShift = float previousBytes.Length - prefixBytes
        let commonAfterShift = ledger.CommonRunBytes - prefixBytes
        Check.true' (commonAfterShift > 0.9 * equalAfterShift) $"The equal-byte phase consumed {commonAfterShift} of {equalAfterShift} equal bytes after the shift."
        do! session.Close()
        return ()
    }

    let cases = [
        "validated byte progress stops at an incomplete scalar", fun () -> async {
            let clock = ManualClock 0.0
            let meter = Meter.create (clock :> IClock) Limits.defaults
            let state = Scanner.create TextEncoding.Utf8 0L None
            let batch = LineBatch()
            let first = Scanner.scanChunk state [| 0xE2uy |] 0 1 false meter batch ignore ignore
            Check.equal InputConsumed first.Status "The incomplete scalar waits for its next byte."
            Check.equal 0.0 state.ValidatedBytes "An incomplete scalar does not advance validated progress."
            let second = Scanner.scanChunk state [| 0x28uy |] 0 1 true meter batch ignore ignore
            Check.equal DecodeFailure second.Status "An invalid continuation stops the scan."
            match second.Error with
            | Some error -> Check.equal 0L error.Offset "The scanner reports the incomplete scalar start."
            | None -> failwith "The scanner returned no decoding error."
            Check.equal 0.0 state.ValidatedBytes "Validated progress stops before the invalid scalar."
            return ()
        }
        "the equal-byte phase resumes after a large insertion", fun () ->
            shiftedTailCase "stream-insertion-reentry" (fun previous ->
                let inserted = makeLines 5_000 "inserted-"
                Array.concat [ previous[.. 99]; inserted; previous[100 ..] ])
        "the equal-byte phase resumes after a large deletion", fun () ->
            shiftedTailCase "stream-deletion-reentry" (fun previous ->
                Array.concat [ previous[.. 99]; previous[5_100 ..] ])
        "window storage stays within its configured line bound", fun () -> async {
            let previous = makeLines 50_000 "line-"
            let current = Array.copy previous
            for index in 500 .. 1_000 .. 49_500 do
                current[index] <- { current[index] with Text = "edit-" + string index }
            let ledger = Ledger()
            let sessionConfig = config "stream-window-cap" 256 1_000_000 Limits.defaults None
            let! session = openSession ledger sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages, _ = readAll session
            checkOracle previous current pages
            Check.true' (ledger.PeakWindowLines AllocationCategory.PreviousWindows <= 256) "Previous metadata stays within the configured window."
            Check.true' (ledger.PeakWindowLines AllocationCategory.CurrentWindows <= 256) "Current metadata stays within the configured window."
            Check.true' (ledger.PeakWindowLines AllocationCategory.PreviousWindows > 0) "Previous window tracking records loaded lines."
            Check.true' (ledger.PeakWindowLines AllocationCategory.CurrentWindows > 0) "Current window tracking records loaded lines."
            do! session.Close()
            return ()
        }
        "the first page stops near an early edit in a million-line file", fun () -> async {
            let previous = asciiFile 1_000_000
            let current = Array.copy previous
            current[10 * 4] <- 0x62uy
            let limits = { Limits.defaults with MaxUnits = 2_147_483_647; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let sessionConfig = config "stream-early-page" 256 1_000_000 limits None
            let! session = openSession (Ledger()) sessionConfig (sourceSpec previous) (sourceSpec current)
            let! initial = session.FirstPage(fun () -> false)
            let page =
                match initial with
                | EngineResult.Ok(Resumable.Ready value) -> value
                | EngineResult.Ok(Resumable.Scanning _) -> failwith "The first request did not finish its nearby hunk."
                | EngineResult.Failed(code, message, _) -> failwith $"The first request failed with {code}: {message}."
                | EngineResult.Canceled -> failwith "The first request was canceled."
            Check.true' page.NextCursor.IsSome "The first page leaves later lines for a continuation."
            Check.true' (page.Progress.ValidatedBytes * 10L < page.Progress.TotalBytes) "The first page validates less than one tenth of both files."
            Check.true' (page.Parts |> Array.exists (function DiffPart.Hunk _ -> true | _ -> false)) "The early edit appears on the first page."
            do! session.Close()
            return ()
        }
        "alignment suspends across small request budgets and matches an uninterrupted run", fun () -> async {
            let previous = Array.init 36 (fun index -> {
                Text = if index % 3 = 0 then "shared" else "old-" + string index
                Ending = LineEnding.LF
            })
            let current = Array.init 36 (fun index -> {
                Text = if index % 3 = 0 then "shared" else "new-" + string index
                Ending = LineEnding.LF
            })
            let previousSpec = sourceSpec (encodeLines previous)
            let currentSpec = sourceSpec (encodeLines current)
            let tinyLimits = { Limits.defaults with MaxUnits = 120; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! suspended = openSession (Ledger()) (config "stream-suspend" 64 1_000_000 tinyLimits None) previousSpec currentSpec
            let! pages, scans = readAll suspended
            Check.true' (scans > 1) "Small work budgets return repeated continuations."
            checkOracle previous current pages

            let! uninterrupted = openSession (Ledger()) (config "stream-uninterrupted" 64 1_000_000 Limits.defaults None) previousSpec currentSpec
            let! uninterruptedPages, _ = readAll uninterrupted
            checkOracle previous current uninterruptedPages
            let interruptedParts = allParts pages
            let uninterruptedParts = allParts uninterruptedPages
            Check.sequence (rebuild previous interruptedParts true) (rebuild previous uninterruptedParts true) "Suspended work keeps the same previous-side output."
            Check.sequence (rebuild current interruptedParts false) (rebuild current uninterruptedParts false) "Suspended work keeps the same current-side output."
            do! suspended.Close()
            do! uninterrupted.Close()
            return ()
        }
        "a work-limited gap becomes unaligned before a later edit", fun () -> async {
            let prefix = makeLines 4 "before-"
            let oldGap = makeLines 10 "old-gap-"
            let newGap = makeLines 10 "new-gap-"
            let suffix = makeLines 4 "after-"
            let previous = Array.concat [ prefix; oldGap; [| { Text = "sync"; Ending = LineEnding.LF }; { Text = "old-tail"; Ending = LineEnding.LF } |]; suffix ]
            let current = Array.concat [ prefix; newGap; [| { Text = "sync"; Ending = LineEnding.LF }; { Text = "new-tail"; Ending = LineEnding.LF } |]; suffix ]
            let sessionConfig = config "stream-gap-limit" 64 1 Limits.defaults None
            let! session = openSession (Ledger()) sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages, _ = readAll session
            checkOracle previous current pages
            let parts = allParts pages
            let unaligned = parts |> Array.tryFindIndex (function DiffPart.Hunk { Body = HunkBody.UnalignedSides _ } -> true | _ -> false)
            match unaligned with
            | None -> failwith "The work-limited gap did not become unaligned."
            | Some index ->
                Check.true' (parts |> Array.skip (index + 1) |> Array.exists (function DiffPart.Hunk { Body = HunkBody.AlignedRows rows } -> rows |> Array.exists (fun row -> row.Kind = DiffRowKind.Replaced) | _ -> false)) "A later edit remains aligned after the unaligned gap."
            do! session.Close()
            return ()
        }
        "colliding line hashes are confirmed against source text", fun () -> async {
            let previous = [|
                { Text = "lead"; Ending = LineEnding.LF }
                { Text = "cat"; Ending = LineEnding.LF }
                { Text = "red"; Ending = LineEnding.LF }
                { Text = "tail"; Ending = LineEnding.NoEnding }
            |]
            let current = [|
                { Text = "lead"; Ending = LineEnding.LF }
                { Text = "dog"; Ending = LineEnding.LF }
                { Text = "red"; Ending = LineEnding.LF }
                { Text = "tail"; Ending = LineEnding.NoEnding }
            |]
            let sessionConfig = config "stream-hash-collision" 32 1_000_000 Limits.defaults (Some(0u, 0u))
            let! session = openSession (Ledger()) sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages, _ = readAll session
            checkOracle previous current pages
            let changed = allParts pages |> Array.collect (function DiffPart.Hunk { Body = HunkBody.AlignedRows rows } -> rows | _ -> [||])
            Check.true' (changed |> Array.exists (fun row -> row.Kind = DiffRowKind.Replaced)) "Different lines with equal masked hashes stay changed."
            do! session.Close()
            return ()
        }
    ]
