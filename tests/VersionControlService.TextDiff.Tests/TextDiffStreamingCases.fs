namespace VersionControlService.TextDiff.Tests

open System
open System.Text
open VersionControlService.Abstractions
open VersionControlService.TextDiff

module TextDiffStreamingCases =
    type private SourceLine = { Text: string; Ending: LineEnding }

    type private GrowingByteSource(bytes: byte[], step: int, everyReads: int) =
        let mutable available = 0
        let mutable reads = 0

        interface IByteSource with
            member _.KnownLength = Some(int64 bytes.Length)
            member _.AvailableLength() = int64 available
            member _.IsComplete() = available >= bytes.Length
            member _.ReadAt position buffer offset count = async {
                reads <- reads + 1
                if reads % everyReads = 0 then available <- min bytes.Length (available + step)
                let start = int position
                if start >= bytes.Length then return ReadOutcome.EndOfSource
                elif start >= available then return ReadOutcome.NotYetAvailable
                else
                    let count = min count (available - start)
                    Array.blit bytes start buffer offset count
                    return ReadOutcome.Bytes count
            }

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

    let private sourceSpecWith source bytes = { sourceSpec bytes with Source = Some source }

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

    /// Reads every page and calls `observe` after each one while the session still holds its windows.
    let private readAllObserving (observe: unit -> unit) (session: TextDiffSession) = async {
        let pages = ResizeArray<DiffPage>()
        let! first = session.FirstPage(fun () -> false)
        let mutable result = first
        let mutable scans = 0
        let mutable doneReading = false

        while not doneReading do
            let! page, pageScans = resolvePageAsync session (fun () -> false) result
            scans <- scans + pageScans
            pages.Add page
            observe ()
            match page.NextCursor with
            | None -> doneReading <- true
            | Some cursor ->
                let! next = session.ReadPage cursor (fun () -> false)
                result <- next

        return pages.ToArray(), scans
    }

    let private readAll (session: TextDiffSession) = readAllObserving ignore session

    let private allParts (pages: DiffPage array) = pages |> Array.collect (fun page -> page.Parts)

    let private diffCounts pages =
        let mutable added = 0
        let mutable removed = 0
        let mutable unalignedPrevious = 0
        let mutable unalignedCurrent = 0
        for part in allParts pages do
            match part with
            | DiffPart.Hunk fragment ->
                match fragment.Body with
                | HunkBody.AlignedRows rows ->
                    for row in rows do
                        if row.Kind = DiffRowKind.Added then added <- added + 1
                        elif row.Kind = DiffRowKind.Removed then removed <- removed + 1
                | HunkBody.UnalignedSides(previous, current) ->
                    unalignedPrevious <- unalignedPrevious + previous.Length
                    unalignedCurrent <- unalignedCurrent + current.Length
            | _ -> ()
        added, removed, unalignedPrevious, unalignedCurrent

    /// Counts equal lines (hidden and context), added, removed and replaced rows, and unaligned lines.
    let private rowCounts pages =
        let mutable equal = 0
        let mutable added = 0
        let mutable removed = 0
        let mutable replaced = 0
        let mutable unaligned = 0
        for part in allParts pages do
            match part with
            | DiffPart.HiddenEqual gap -> equal <- equal + int gap.PreviousRange.Count
            | DiffPart.Hunk fragment ->
                match fragment.Body with
                | HunkBody.AlignedRows rows ->
                    for row in rows do
                        match row.Kind with
                        | DiffRowKind.Context -> equal <- equal + 1
                        | DiffRowKind.Added -> added <- added + 1
                        | DiffRowKind.Removed -> removed <- removed + 1
                        | DiffRowKind.Replaced -> replaced <- replaced + 1
                        | _ -> ()
                | HunkBody.UnalignedSides(previous, current) -> unaligned <- unaligned + previous.Length + current.Length
            | _ -> ()
        equal, added, removed, replaced, unaligned

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

    /// Walks the parts in order. Each side must advance by one line per row without a gap or an overlap, and every
    /// equal row and every hidden equal range must name the same text and ending on both sides.
    let private checkAcrossSides (previous: SourceLine array) (current: SourceLine array) parts =
        let mutable nextPrevious = 0L
        let mutable nextCurrent = 0L
        let takePrevious (line: DiffLine) =
            if line.Number <> nextPrevious then failwith $"The previous side expects line {nextPrevious} and the diff shows {line.Number}."
            nextPrevious <- nextPrevious + 1L
        let takeCurrent (line: DiffLine) =
            if line.Number <> nextCurrent then failwith $"The current side expects line {nextCurrent} and the diff shows {line.Number}."
            nextCurrent <- nextCurrent + 1L
        for part in parts do
            match part with
            | DiffPart.HiddenEqual gap ->
                if gap.PreviousRange.Start <> nextPrevious || gap.CurrentRange.Start <> nextCurrent then
                    failwith $"A hidden equal range starts at {gap.PreviousRange.Start} and {gap.CurrentRange.Start}, but the sides are at {nextPrevious} and {nextCurrent}."
                if gap.PreviousRange.Count <> gap.CurrentRange.Count then failwith "A hidden equal range has different counts on the two sides."
                for offset = 0 to int gap.PreviousRange.Count - 1 do
                    if previous[int gap.PreviousRange.Start + offset] <> current[int gap.CurrentRange.Start + offset] then
                        failwith $"A hidden equal range differs at previous line {gap.PreviousRange.Start + int64 offset}."
                nextPrevious <- nextPrevious + gap.PreviousRange.Count
                nextCurrent <- nextCurrent + gap.CurrentRange.Count
            | DiffPart.Hunk fragment ->
                match fragment.Body with
                | HunkBody.AlignedRows rows ->
                    for row in rows do
                        if row.Kind = DiffRowKind.Context then
                            match row.Previous, row.Current with
                            | Some left, Some right ->
                                if left.Slice.Text <> right.Slice.Text || left.Ending <> right.Ending then
                                    failwith $"An equal row differs between previous line {left.Number} and current line {right.Number}."
                            | _ -> failwith "An equal row lacks a line on one side."
                        row.Previous |> Option.iter takePrevious
                        row.Current |> Option.iter takeCurrent
                | HunkBody.UnalignedSides(left, right) ->
                    for line in left do takePrevious line
                    for line in right do takeCurrent line
            | DiffPart.ExpandedContext _ -> ()
        if nextPrevious <> int64 previous.Length || nextCurrent <> int64 current.Length then
            failwith $"The sides end at {nextPrevious} and {nextCurrent} of {previous.Length} and {current.Length} lines."

    let private checkOracle previous current pages =
        let parts = allParts pages
        checkAcrossSides previous current parts
        Check.sequence previous (rebuild previous parts true) "The page stream covers every previous line exactly once."
        Check.sequence current (rebuild current parts false) "The page stream covers every current line exactly once."

    let private makeLines count prefix =
        Array.init count (fun index -> { Text = prefix + string index; Ending = LineEnding.LF })

    let private asciiFile lineCount =
        let bytes = Array.create (lineCount * 4) 0x61uy
        for line = 0 to lineCount - 1 do bytes[line * 4 + 3] <- 0x0Auy
        bytes

    /// Runs a large file with a shifting edit near the start and a small edit near the end, then checks that the
    /// page stream still covers every line after the shift.
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
        do! session.Close()
        return ()
    }

    let private hdf5Signature = [| 0x89uy; 0x48uy; 0x44uy; 0x46uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]

    /// UTF-16LE text of 49-letter lines with an HDF5 signature written over the bytes at the given even offset.
    /// The signature bytes form valid UTF-16 code units, so only the offset check can flag them.
    let private withSignatureAt (head: string) (length: int) (offset: int) =
        let bytes = Array.zeroCreate<byte> length
        bytes[0] <- 0xFFuy
        bytes[1] <- 0xFEuy
        let headBytes = Encoding.Unicode.GetBytes head
        for index in 2 .. length - 1 do
            let rel = index - 2
            bytes[index] <-
                if rel < headBytes.Length then headBytes[rel]
                elif (rel - headBytes.Length) % 2 = 1 then 0uy
                elif ((rel - headBytes.Length) / 2) % 50 = 49 then 10uy
                else 65uy
        Array.blit hdf5Signature 0 bytes offset hdf5Signature.Length
        bytes

    let private utf16Spec (bytes: byte[]) = { sourceSpec bytes with Encoding = "utf-16le"; BomLength = 2 }

    let cases = [
        "an HDF5 signature at 65536 is binary evidence before the first page", fun () -> async {
            let bytes = withSignatureAt "" 100_000 65_536
            let! session = openSession (Ledger()) (config "hdf5-early" 64 16 Limits.defaults None) (utf16Spec bytes) (utf16Spec bytes)
            let! result = session.FirstPage(fun () -> false)
            match result with
            | EngineResult.Failed(code, _, Some detail) ->
                Check.equal TextDiffFailureCodes.ContentNotText code "The later signature reports the text failure code."
                Check.true' (detail.Evidence.Contains "65536") "The evidence names the signature offset."
            | other -> failwith $"The later signature returned {other}."
            do! session.Close()
            return ()
        }
        "an HDF5 signature at 131072 invalidates a session after its first page", fun () -> async {
            let previous = withSignatureAt "before\n" 300_000 131_072
            let current = withSignatureAt "after\n" 300_000 131_072
            let! session = openSession (Ledger()) (config "hdf5-late" 64 16 Limits.defaults None) (utf16Spec previous) (utf16Spec current)
            let! firstResult = session.FirstPage(fun () -> false)
            let! first, _ = resolvePageAsync session (fun () -> false) firstResult
            match first.NextCursor with
            | None -> failwith "The first page did not leave more content to validate."
            | Some cursor ->
                let mutable outcome = None
                let mutable attempts = 0
                let mutable next = cursor
                while outcome.IsNone do
                    attempts <- attempts + 1
                    if attempts > 10_000 then failwith "The session did not report the signature."
                    let! result = session.ReadPage next (fun () -> false)
                    match result with
                    | EngineResult.Failed(code, _, Some detail) -> outcome <- Some(code, detail.Evidence)
                    | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) -> next <- continuation
                    | EngineResult.Ok(Resumable.Ready page) ->
                        match page.NextCursor with
                        | Some following -> next <- following
                        | None -> failwith "The session finished without reporting the signature."
                    | other -> failwith $"The session returned {other}."
                let code, evidence = outcome.Value
                Check.equal TextDiffFailureCodes.ContentNotText code "The later signature reports the text failure code."
                Check.true' (evidence.Contains "131072") "The evidence names the signature offset."
            do! session.Close()
            return ()
        }
        "journal records reach the temp store as typed arrays and read back", fun () -> async {
            let previous = makeLines 40 "line-"
            let current = Array.copy previous
            current[7] <- { current[7] with Text = "changed" }
            let untyped = ref 0
            let writes = ref 0
            let recording (inner: ITempStore) =
                let check (bytes: byte[]) =
                    writes.Value <- writes.Value + 1
                    if not (Native.isTypedBytes bytes) then untyped.Value <- untyped.Value + 1
                { new ITempStore with
                    member _.Append bytes offset count = check bytes; inner.Append bytes offset count
                    member _.WriteAt position bytes offset count = check bytes; inner.WriteAt position bytes offset count
                    member _.ReadAt position bytes offset count = inner.ReadAt position bytes offset count
                    member _.Length() = inner.Length()
                    member _.Dispose() = inner.Dispose() }
            let host = {
                Host.createInMemory (ManualClock 0.0 :> IClock) with
                    CreateTempStore = fun _ -> async.Return(recording (MemoryTempStore() :> ITempStore))
            }
            let! session = TextDiffSession.create host (Ledger()) (config "stream-journal-bytes" 64 1_000_000 Limits.defaults None) (fun _ -> 1) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! first = session.FirstPage(fun () -> false)
            let! pageId =
                async {
                    let! page, _ = resolvePageAsync session (fun () -> false) first
                    return page.PageId
                }
            let! replayed = session.ReplayPage pageId
            match replayed with
            | EngineResult.Ok page -> Check.equal pageId page.PageId "A replayed page reads back from the journal."
            | _ -> failwith "The journal did not return the recorded page."
            Check.true' (writes.Value > 0) "The session wrote to the temp store."
            Check.equal 0 untyped.Value "Every temp store write passes a typed array."
            do! session.Close()
            return ()
        }
        "validated byte progress stops at an incomplete scalar", fun () -> async {
            let clock = ManualClock 0.0
            let meter = Meter.create (clock :> IClock) Limits.defaults
            let state = Scanner.create TextEncoding.Utf8 0L
            let batch = LineBatch()
            let first = Scanner.scanChunk state [| 0xE2uy |] 0 1 false meter batch ignore
            Check.equal InputConsumed first.Status "The incomplete scalar waits for its next byte."
            Check.equal 0.0 state.ValidatedBytes "An incomplete scalar does not advance validated progress."
            let second = Scanner.scanChunk state [| 0x28uy |] 0 1 true meter batch ignore
            Check.equal DecodeFailure second.Status "An invalid continuation stops the scan."
            match second.Error with
            | Some error -> Check.equal 0L error.Offset "The scanner reports the incomplete scalar start."
            | None -> failwith "The scanner returned no decoding error."
            Check.equal 0.0 state.ValidatedBytes "Validated progress stops before the invalid scalar."
            return ()
        }
        "validated byte progress stops before a pending high surrogate and a partial unit", fun () -> async {
            let meter = Meter.create (ManualClock 0.0 :> IClock) Limits.defaults
            let state = Scanner.create TextEncoding.Utf16LE 0L
            let result = Scanner.scanChunk state [| 0x00uy; 0xD8uy; 0x41uy |] 0 3 true meter (LineBatch()) ignore
            Check.equal DecodeFailure result.Status "A surrogate without a partner is a decoding failure."
            match result.Error with
            | Some error -> Check.equal 2L error.Offset "The failure is reported where the partial unit starts."
            | None -> failwith "The scanner returned no decoding error."
            Check.equal 0.0 state.ValidatedBytes "Validated progress stops before the pending high surrogate."
            return ()
        }
        "a due quantum still permits the first scanner segment", fun () -> async {
            let clock = ManualClock 0.0
            let meter = Meter.create (clock :> IClock) { MaxUnits = Int32.MaxValue; RequestMs = 1_000_000.0; QuantumMs = 0.0 }
            let state = Scanner.create TextEncoding.Utf8 0L
            let input = Array.create 8_192 0x61uy
            let result = Scanner.scanChunk state input 0 input.Length false meter (LineBatch()) ignore
            Check.equal 4_096 result.Consumed "The scanner makes one segment of progress."
            Check.equal QuantumReached result.Status "The scanner suspends after the first segment."
            return ()
        }
        "the equal-byte phase resumes after a large insertion", fun () ->
            shiftedTailCase "stream-insertion-reentry" (fun previous ->
                let inserted = makeLines 5_000 "inserted-"
                Array.concat [ previous[.. 99]; inserted; previous[100 ..] ])
        "the equal-byte phase resumes after a large deletion", fun () ->
            shiftedTailCase "stream-deletion-reentry" (fun previous ->
                Array.concat [ previous[.. 99]; previous[5_100 ..] ])
        "growing previous input preserves the complete insertion diff", fun () -> async {
            let previous = makeLines 40_000 "line "
            let inserted = makeLines 70_000 "new "
            let current = Array.concat [ previous[.. 19_999]; inserted; previous[20_000 ..] ]
            let previousBytes = encodeLines previous
            let currentBytes = encodeLines current
            let previousSource = sourceSpec previousBytes
            let currentSource = sourceSpec currentBytes

            let! complete = openSession (Ledger()) (SessionConfig.defaults "stream-growing-insertion-complete") previousSource currentSource
            let! completePages, _ = readAll complete
            checkOracle previous current completePages
            let completeCounts = diffCounts completePages
            Check.equal (70_000, 0, 0, 0) completeCounts "The complete input reports the full insertion without unaligned lines."
            do! complete.Close()

            for name, step, everyReads in [
                "16k-2", 16_384, 2
                "64k-3", 65_536, 3
                "4k-1", 4_096, 1
            ] do
                let growing = GrowingByteSource(previousBytes, step, everyReads) :> IByteSource
                let growingPrevious = sourceSpecWith growing previousBytes
                let! session = openSession (Ledger()) (SessionConfig.defaults ("stream-growing-insertion-" + name)) growingPrevious currentSource
                let! pages, _ = readAll session
                checkOracle previous current pages
                Check.equal completeCounts (diffCounts pages) $"Growth pattern {name} matches the complete input."
                do! session.Close()
            return ()
        }
        "repeated lines keep large insertions and deletions exact as source bytes arrive", fun () -> async {
            let sourceLine text = { Text = text; Ending = LineEnding.LF }
            let prose prefix count =
                Array.init count (fun index -> sourceLine (if index % 2 = 1 then "" else sprintf "%s paragraph %d with some words." prefix index))
            let jsonRecords prefix count finalRecord =
                Array.init count (fun index ->
                    [| sourceLine "  {"
                       sourceLine (sprintf "    \"id\": \"%s-%d\"," prefix index)
                       sourceLine (sprintf "    \"value\": %d" (if prefix = "old" then index * 7 else 1_000_000 + index))
                       sourceLine (if finalRecord && index = count - 1 then "  }" else "  },") |])
                |> Array.concat
            let splice (lines: SourceLine[]) index (inserted: SourceLine[]) = Array.concat [ lines[.. index - 1]; inserted; lines[index ..] ]
            let proseBase = prose "old" 1_000
            let jsonBase = Array.concat [ [| sourceLine "[" |]; jsonRecords "old" 512 true; [| sourceLine "]" |] ]
            let cases = ResizeArray<string * SourceLine[] * SourceLine[] * int * int>()
            for kind, baseLines, makeInserted, lineStride in [
                "prose", proseBase, (fun amount -> prose "new" amount), 2
                "json", jsonBase, (fun amount -> jsonRecords "new" (amount / 4) false), 4
            ] do
                for amount in [| 600; 2_000 |] do
                    for positionName, recordIndex in [ "start", 10; "middle", (if kind = "prose" then 250 else 256) ] do
                        let lineIndex = if kind = "prose" then recordIndex * lineStride else 1 + recordIndex * lineStride
                        let inserted = makeInserted amount
                        Check.equal amount inserted.Length $"The {kind} {amount}-line edit has its requested size."
                        let extended = splice baseLines lineIndex inserted
                        cases.Add($"{kind}-{amount}-{positionName}-insert", baseLines, extended, amount, 0)
                        cases.Add($"{kind}-{amount}-{positionName}-delete", extended, baseLines, 0, amount)

            for name, previous, current, expectedAdded, expectedRemoved in cases do
                let previousBytes = encodeLines previous
                let currentBytes = encodeLines current
                let sessionConfig = SessionConfig.defaults ("repeat-" + name)
                let! baseline = openSession (Ledger()) sessionConfig (sourceSpec previousBytes) (sourceSpec currentBytes)
                let! baselinePages, _ = readAll baseline
                checkOracle previous current baselinePages
                let baselineCounts = diffCounts baselinePages
                Check.equal (expectedAdded, expectedRemoved, 0, 0) baselineCounts $"The {name} input has only the requested line changes."
                let rows = allParts baselinePages |> Array.collect (function DiffPart.Hunk { Body = HunkBody.AlignedRows values } -> values | _ -> Array.empty)
                for row in rows do
                    match row.Kind with
                    | DiffRowKind.Context ->
                        let previousLine = row.Previous.Value
                        let currentLine = row.Current.Value
                        Check.equal previousLine.Slice.Text currentLine.Slice.Text $"The {name} input keeps context text equal."
                        Check.equal previousLine.Ending currentLine.Ending $"The {name} input keeps context endings equal."
                    | DiffRowKind.Added
                    | DiffRowKind.Removed -> ()
                    | other -> failwith $"The {name} input returned unexpected row kind {other}."
                let replacedRows = rows |> Array.filter (fun row -> row.Kind = DiffRowKind.Replaced)
                Check.equal 0 replacedRows.Length $"The {name} input has no replaced rows."
                let baselineParts = allParts baselinePages
                do! baseline.Close()

                for growth in [| 256; 1_024; 4_096 |] do
                    let previousSource = GrowingByteSource(previousBytes, growth, 1) :> IByteSource
                    let currentSource = GrowingByteSource(currentBytes, growth, 1) :> IByteSource
                    let previousSpec = sourceSpecWith previousSource previousBytes
                    let currentSpec = sourceSpecWith currentSource currentBytes
                    let! session = openSession (Ledger()) sessionConfig previousSpec currentSpec
                    let! pages, _ = readAll session
                    checkOracle previous current pages
                    Check.equal baselineCounts (diffCounts pages) $"Growth by {growth} bytes preserves the {name} counts."
                    Check.true' (Unchecked.equals baselineParts (allParts pages)) $"Growth by {growth} bytes preserves the {name} output."
                    do! session.Close()
            return ()
        }
        "edits followed by more lines than the largest window stay aligned", fun () -> async {
            let sourceLine text = { Text = text; Ending = LineEnding.LF }
            let lineCount = 70_000
            let editIndex = 102
            let edited (lines: SourceLine[]) =
                let copy = Array.copy lines
                copy[editIndex] <- sourceLine (copy[editIndex].Text + " edited")
                copy
            let prose = Array.init lineCount (fun index -> sourceLine (if index % 2 = 1 then "" else $"Paragraph {index} has some words in it."))
            let column = Array.init lineCount (fun index -> sourceLine (match index % 3 with 0 -> "low" | 1 -> "mid" | _ -> "high"))
            let table = Array.init lineCount (fun index -> sourceLine (if index % 50 = 0 then $"block {index}" else "0\t0\t0\t0\tNA"))
            let inserted = Array.concat [ table[.. editIndex - 1]; Array.create 2_000 (sourceLine "0\t0\t0\t0\tNA"); table[editIndex ..] ]
            for name, previous, current, expectedAdded in [
                "prose", prose, edited prose, 0
                "column", column, edited column, 0
                "table", table, edited table, 0
                "table insertion", table, inserted, 2_000
            ] do
                let! session = openSession (Ledger()) (SessionConfig.defaults ("far-" + name)) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages, _ = readAll session
                checkOracle previous current pages
                Check.equal (expectedAdded, 0, 0, 0) (diffCounts pages) $"The {name} input has no unaligned lines and only its own changes."
                let changed =
                    allParts pages
                    |> Array.collect (function DiffPart.Hunk { Body = HunkBody.AlignedRows values } -> values | _ -> Array.empty)
                    |> Array.filter (fun row -> row.Kind = DiffRowKind.Replaced)
                    |> Array.map (fun row -> row.Previous.Value.Number, row.Current.Value.Number)
                let expectedReplaced = if expectedAdded = 0 then [| int64 editIndex, int64 editIndex |] else Array.empty
                Check.sequence expectedReplaced changed $"The {name} input pairs the edited line with its new text."
                do! session.Close()
            return ()
        }
        "insertions and deletions larger than the largest window stay exact in text with blank lines", fun () -> async {
            let sourceLine text = { Text = text; Ending = LineEnding.LF }
            let prose prefix count =
                Array.init count (fun index -> sourceLine (if index % 2 = 1 then "" else $"{prefix} paragraph {index} has some words in it."))
            let windowLines = 2_048
            let original = prose "old" 12_000
            let extended = Array.concat [ original[.. 999]; prose "new" (windowLines * 2 + 500); original[1_000 ..] ]
            let sessionConfig = { SessionConfig.defaults "large-block" with WindowMaxLines = windowLines }
            for name, previous, current, expectedAdded, expectedRemoved in [
                "insertion", original, extended, extended.Length - original.Length, 0
                "deletion", extended, original, 0, extended.Length - original.Length
            ] do
                let! session = openSession (Ledger()) sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages, _ = readAll session
                checkOracle previous current pages
                Check.equal (expectedAdded, expectedRemoved, 0, 0) (diffCounts pages) $"The {name} shows exactly the block and no unaligned lines."
                do! session.Close()
            return ()
        }
        "window storage stays within its configured line bound", fun () -> async {
            let previous = makeLines 50_000 "line-"
            let current = Array.copy previous
            for index in 500 .. 1_000 .. 49_500 do
                current[index] <- { current[index] with Text = "edit-" + string index }
            let ledger = Ledger()
            let sessionConfig = config "stream-window-cap" 256 1_000_000 Limits.defaults None
            let! session = openSession ledger sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let mutable previousPeak = 0L
            let mutable currentPeak = 0L
            let observe () =
                previousPeak <- max previousPeak (ledger.Used AllocationCategory.PreviousWindows)
                currentPeak <- max currentPeak (ledger.Used AllocationCategory.CurrentWindows)
            let! pages, _ = readAllObserving observe session
            checkOracle previous current pages
            let windowBudget = 40L * 1_024L
            Check.true' (previousPeak <= windowBudget) "Previous window reservation stays within the configured window."
            Check.true' (currentPeak <= windowBudget) "Current window reservation stays within the configured window."
            Check.true' (previousPeak > 0L) "Previous window tracking reserves memory for loaded lines."
            Check.true' (currentPeak > 0L) "Current window tracking reserves memory for loaded lines."
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
            Check.true' (page.Progress.ValidatedBytes * 10L < page.Progress.TotalBytes) $"The first page validates less than one tenth of both files. Validated {page.Progress.ValidatedBytes} of {page.Progress.TotalBytes} bytes."
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
        "a long replacement resumes its page search within request budgets", fun () -> async {
            let unchanged = String('a', 160_000)
            let changed = unchanged.Substring(0, 80_000) + "b" + unchanged.Substring(80_001)
            let previous = [|
                { Text = "head"; Ending = LineEnding.LF }
                { Text = unchanged; Ending = LineEnding.LF }
                { Text = "tail"; Ending = LineEnding.NoEnding }
            |]
            let current = [|
                { Text = "head"; Ending = LineEnding.LF }
                { Text = changed; Ending = LineEnding.LF }
                { Text = "tail"; Ending = LineEnding.NoEnding }
            |]
            let previousSpec = sourceSpec (encodeLines previous)
            let currentSpec = sourceSpec (encodeLines current)
            let baselineLimits = { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let tinyLimits = { baselineLimits with MaxUnits = 64 }
            let! baseline = openSession (Ledger()) (config "stream-long-pair-baseline" 64 100_000 baselineLimits None) previousSpec currentSpec
            let! baselinePages, _ = readAll baseline
            let! limited = openSession (Ledger()) (config "stream-long-pair-limited" 64 100_000 tinyLimits None) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! limitedPages, scans = readAll limited
            Check.true' (scans > 1) "The long pair search returns more than one page request."
            let replacedRow pages =
                allParts pages
                |> Array.collect (function
                    | DiffPart.Hunk { Body = HunkBody.AlignedRows rows } -> rows
                    | _ -> [||])
                |> Array.find (fun (row: DiffRow) -> row.Kind = DiffRowKind.Replaced)
            let lineShape (line: DiffLine) =
                line.Number, line.Ending, line.Slice.OffsetUtf16, line.Slice.TotalUtf16, line.Slice.Text, line.Slice.Highlights
            let rowShape (row: DiffRow) =
                row.Kind,
                row.Previous |> Option.map lineShape,
                row.Current |> Option.map lineShape
            Check.equal (rowShape (replacedRow baselinePages)) (rowShape (replacedRow limitedPages)) "The budgeted page has the same long pair slice and highlights."
            do! baseline.Close()
            do! limited.Close()
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
        "a repeated line run before a unique line keeps the single insertion exact", fun () -> async {
            let sourceLine text = { Text = text; Ending = LineEnding.LF }
            let previous = Array.append (Array.create 1_000 (sourceLine "a")) [| sourceLine "MARK" |]
            let current = Array.append [| sourceLine "b" |] previous
            let! session = openSession (Ledger()) (SessionConfig.defaults "stream-run-anchor-growth") (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages, _ = readAll session
            checkOracle previous current pages
            Check.equal (1_001, 1, 0, 0, 0) (rowCounts pages) "The window grows before a long equal run settles it, so one line is added and nothing is replaced."
            do! session.Close()
            return ()
        }
        "a unique line that moved across a long equal run is one moved line", fun () -> async {
            let sourceLine text = { Text = text; Ending = LineEnding.LF }
            let repeated count text = Array.create count (sourceLine text)
            let marker = [| sourceLine "X" |]
            let cases = [
                "up 600", Array.concat [ [| sourceLine "p" |]; repeated 600 "a"; marker; repeated 600 "b" ],
                          Array.concat [ [| sourceLine "qq" |]; marker; repeated 600 "a"; repeated 600 "b" ]
                "up 2000", Array.concat [ [| sourceLine "p" |]; repeated 2_000 "a"; marker; repeated 2_000 "b" ],
                           Array.concat [ [| sourceLine "qq" |]; marker; repeated 2_000 "a"; repeated 2_000 "b" ]
                "down 600", Array.concat [ [| sourceLine "qq" |]; marker; repeated 600 "a"; repeated 600 "b" ],
                            Array.concat [ [| sourceLine "p" |]; repeated 600 "a"; marker; repeated 600 "b" ]
                "partly down 600", Array.concat [ [| sourceLine "p" |]; repeated 600 "a"; marker; repeated 600 "b" ],
                                   Array.concat [ [| sourceLine "qq" |]; repeated 5 "a"; marker; repeated 595 "a"; repeated 600 "b" ]
            ]
            for name, previous, current in cases do
                let! session = openSession (Ledger()) (SessionConfig.defaults ("stream-moved-marker-" + name)) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages, _ = readAll session
                checkOracle previous current pages
                Check.equal (previous.Length - 2, 1, 1, 1, 0) (rowCounts pages) $"The {name} case aligns both runs, with one added line, one removed line and one replaced row."
                do! session.Close()
            return ()
        }
        "unique lines that moved together across long equal runs are moved lines", fun () -> async {
            let sourceLine text = { Text = text; Ending = LineEnding.LF }
            let runLength = 600
            // Previous: p, a-run, X, b-run, Y, c-run and so on. Current: qq, all markers, then the runs.
            let cluster markerCount =
                let markers = [| "X"; "Y"; "Z" |][.. markerCount - 1]
                let runs = [| "a"; "b"; "c"; "d" |][.. markerCount]
                let previous =
                    Array.concat [
                        [| sourceLine "p" |]
                        yield! runs |> Array.mapi (fun index run ->
                            Array.append (Array.create runLength (sourceLine run)) (if index < markerCount then [| sourceLine markers[index] |] else [||]))
                    ]
                let current =
                    Array.concat [
                        [| sourceLine "qq" |]
                        markers |> Array.map sourceLine
                        runs |> Array.collect (fun run -> Array.create runLength (sourceLine run))
                    ]
                previous, current
            let up2, up2Current = cluster 2
            let up3, up3Current = cluster 3
            for name, previous, current, markerCount in [ "2 up", up2, up2Current, 2; "3 up", up3, up3Current, 3; "2 down", up2Current, up2, 2 ] do
                let! session = openSession (Ledger()) (SessionConfig.defaults ("stream-moved-markers-" + name)) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages, _ = readAll session
                checkOracle previous current pages
                Check.equal ((markerCount + 1) * runLength, markerCount, markerCount, 1, 0) (rowCounts pages) $"The {name} case aligns every run, with one added and one removed line per marker."
                do! session.Close()
            return ()
        }
        "unique lines that stay in place keep their rows between long equal runs", fun () -> async {
            let sourceLine text = { Text = text; Ending = LineEnding.LF }
            let previous = Array.init 3_000 (fun index -> sourceLine (if index % 300 = 299 then $"M{index}" else "a"))
            let inserted =
                previous
                |> Array.mapi (fun index line -> if index % 300 = 299 then Array.append (Array.create 5 (sourceLine "b")) [| line |] else [| line |])
                |> Array.concat
            let edited = Array.copy previous
            edited[0] <- sourceLine "edited"
            for name, current, expected in [ "inserted", inserted, (3_000, 50, 0, 0, 0); "edited", edited, (2_999, 0, 0, 1, 0) ] do
                let! session = openSession (Ledger()) (SessionConfig.defaults ("stream-marker-in-place-" + name)) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages, _ = readAll session
                checkOracle previous current pages
                Check.equal expected (rowCounts pages) $"The {name} case keeps every marker row in place."
                do! session.Close()
            return ()
        }
        "a moved unique line keeps its rows at the run thresholds and in odd layouts", fun () -> async {
            let one text = [| { Text = text; Ending = LineEnding.LF } |]
            let times count text = Array.create count { Text = text; Ending = LineEnding.LF }
            let runs = Array.append (times 32 "a") (times 32 "b")
            let cases = [
                "budget family", Array.concat [ times 300 "p"; times 64 "a"; one "X"; times 300 "b" ],
                                 Array.concat [ times 300 "qq"; one "X"; times 64 "a"; times 300 "c" ], (1, 64, 64, 600, 0)
                "below the threshold", Array.concat [ one "p"; times 63 "a"; one "X"; times 63 "b" ],
                                       Array.concat [ one "qq"; one "X"; times 63 "a"; times 63 "b" ], (64, 63, 63, 1, 0)
                "at the threshold", Array.concat [ one "p"; times 64 "a"; one "X"; times 64 "b" ],
                                    Array.concat [ one "qq"; one "X"; times 64 "a"; times 64 "b" ], (128, 1, 1, 1, 0)
                "mixed run up", Array.concat [ one "p"; runs; one "X"; times 64 "c" ],
                                Array.concat [ one "qq"; one "X"; runs; times 64 "c" ], (128, 1, 1, 1, 0)
                "mixed run down", Array.concat [ one "qq"; one "X"; runs; times 64 "c" ],
                                  Array.concat [ one "p"; runs; one "X"; times 64 "c" ], (128, 1, 1, 1, 0)
                "two markers", Array.concat [ one "p"; times 64 "a"; one "X"; one "Y"; times 64 "b" ],
                               Array.concat [ one "qq"; one "X"; one "Y"; times 64 "a"; times 64 "b" ], (128, 2, 2, 1, 0)
                "keys on one side", Array.concat [ one "p"; times 64 "a"; one "X"; times 64 "b" ],
                                    Array.concat [ one "qq"; times 64 "c"; one "X"; times 64 "d" ], (1, 0, 0, 129, 0)
                "inner boundaries up", Array.concat [ one "head"; one "p"; times 64 "a"; one "X"; one "tail" ],
                                       Array.concat [ one "head"; one "qq"; one "X"; times 64 "a"; one "tail" ], (66, 1, 1, 1, 0)
                "inner boundaries down", Array.concat [ one "head"; one "qq"; one "X"; times 64 "a"; one "tail" ],
                                         Array.concat [ one "head"; one "p"; times 64 "a"; one "X"; one "tail" ], (66, 1, 1, 1, 0)
            ]
            for name, previous, current, expected in cases do
                let! session = openSession (Ledger()) (SessionConfig.defaults ("stream-moved-marker-edge-" + name)) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages, _ = readAll session
                checkOracle previous current pages
                Check.equal expected (rowCounts pages) $"The {name} case has the expected rows."
                do! session.Close()
            return ()
        }
        "a gap that exhausts its budget gets its rejected moved lines back", fun () -> async {
            let one text = [| { Text = text; Ending = LineEnding.LF } |]
            let times count text = Array.create count { Text = text; Ending = LineEnding.LF }
            let cases = [
                "restored gaps exhaust again", Array.concat [ times 700 "p"; times 64 "a"; one "X"; times 700 "b" ],
                                               Array.concat [ times 700 "qq"; one "X"; times 64 "a"; times 700 "c" ], (1, 0, 0, 0, 2928)
                "two restored lines", Array.concat [ times 300 "p"; times 64 "a"; one "X"; times 64 "b"; one "Y"; times 300 "c" ],
                                      Array.concat [ times 300 "qq"; one "X"; times 64 "a"; one "Y"; times 64 "b"; times 300 "d" ], (2, 64, 64, 664, 0)
            ]
            for name, previous, current, expected in cases do
                let! session = openSession (Ledger()) (SessionConfig.defaults ("stream-restore-" + name)) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages, _ = readAll session
                checkOracle previous current pages
                Check.equal expected (rowCounts pages) $"The {name} case has the expected rows."
                do! session.Close()
            return ()
        }
        "a cyclic log with a growing previous source misses no equal lines", fun () -> async {
            let sourceLine text = { Text = text; Ending = LineEnding.LF }
            let cycle index = sourceLine $"cycle line {index % 13}"
            let previous = Array.init 6_000 (fun index -> if index % 97 = 0 then sourceLine $"m marker {index}" else cycle index)
            let current = Array.concat [ previous[.. 1_999]; Array.init 700 cycle; previous[2_000 ..] ]
            let previousBytes = encodeLines previous
            let currentBytes = encodeLines current
            for growth in [| 1_024; 4_096; 16_384 |] do
                let growing = GrowingByteSource(previousBytes, growth, 2) :> IByteSource
                let! session = openSession (Ledger()) (SessionConfig.defaults $"stream-cyclic-growth-{growth}") (sourceSpecWith growing previousBytes) (sourceSpec currentBytes)
                let! pages, _ = readAll session
                checkOracle previous current pages
                Check.equal (6_000, 700, 0, 0, 0) (rowCounts pages) $"Growth by {growth} bytes keeps every original line equal and shows only the inserted lines."
                do! session.Close()
            return ()
        }
    ]
