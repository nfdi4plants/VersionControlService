namespace VersionControlService.TextDiff.Tests

open System
open System.Text
open VersionControlService.Abstractions
open VersionControlService.TextDiff

/// Cases for edits that are larger than one alignment window.
module TextDiffResyncCases =
    type private SourceLine = { Text: string; Ending: LineEnding }

    let private encode (lines: SourceLine array) =
        lines
        |> Array.map (fun line -> line.Text + "\n")
        |> String.concat ""
        |> Encoding.UTF8.GetBytes

    let private spec (bytes: byte[]) = {
        Source = Some(MemoryByteSource(bytes) :> IByteSource)
        Encoding = "utf-8"
        BomLength = 0
        ByteLength = int64 bytes.Length
    }

    /// The bytes start with a UTF-8 byte order mark that the session does not decode as text.
    let private bomSpec (bytes: byte[]) =
        let withMark = Array.append [| 0xEFuy; 0xBBuy; 0xBFuy |] bytes
        {
            Source = Some(MemoryByteSource(withMark) :> IByteSource)
            Encoding = "utf-8"
            BomLength = 3
            ByteLength = int64 withMark.Length
        }

    let private lines prefix start count =
        Array.init count (fun index -> { Text = prefix + string (start + index); Ending = LineEnding.LF })

    let private smallConfig sessionId = {
        SessionConfig.defaults sessionId with
            ContextLines = 1
            PageMaxRows = 4_096
            PageMaxBytes = 4 * 1024 * 1024
            PageMaxFragments = 256
            WindowMaxLines = 64
            WindowMaxBytes = 4 * 1024 * 1024
            CommonChunkBytes = 1_024
            ResyncSampleModulus = 1
            CheckpointIntervalBytes = 1_024.0
            Limits = { Limits.defaults with RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
    }

    let private addDiffLine (output: ResizeArray<SourceLine>) (line: DiffLine) =
        output.Add { Text = line.Slice.Text; Ending = line.Ending }

    let private rebuild (expected: SourceLine array) parts previousSide =
        let output = ResizeArray<SourceLine>()
        for part in parts do
            match part with
            | DiffPart.HiddenEqual gap ->
                let range = if previousSide then gap.PreviousRange else gap.CurrentRange
                for index = int range.Start to int (range.Start + range.Count) - 1 do output.Add expected[index]
            | DiffPart.Hunk fragment ->
                match fragment.Body with
                | HunkBody.AlignedRows values ->
                    for row in values do
                        (if previousSide then row.Previous else row.Current) |> Option.iter (addDiffLine output)
                | HunkBody.UnalignedSides(previous, current) ->
                    for line in (if previousSide then previous else current) do addDiffLine output line
            | DiffPart.ExpandedContext _ -> failwith "The session returned an unsupported expanded context part."
        output.ToArray()

    type private Shape = {
        Pages: DiffPage array
        Scans: int
        UnalignedPrevious: int
        UnalignedCurrent: int
        Added: int
        Removed: int
        Equal: int
    }

    let private shapeOf (pages: DiffPage array) scans =
        let mutable unalignedPrevious = 0
        let mutable unalignedCurrent = 0
        let mutable added = 0
        let mutable removed = 0
        let mutable equal = 0
        for page in pages do
            for part in page.Parts do
                match part with
                | DiffPart.HiddenEqual gap -> equal <- equal + int gap.PreviousRange.Count
                | DiffPart.Hunk fragment ->
                    match fragment.Body with
                    | HunkBody.UnalignedSides(previous, current) ->
                        unalignedPrevious <- unalignedPrevious + previous.Length
                        unalignedCurrent <- unalignedCurrent + current.Length
                    | HunkBody.AlignedRows values ->
                        for row in values do
                            match row.Kind with
                            | DiffRowKind.Added -> added <- added + 1
                            | DiffRowKind.Removed -> removed <- removed + 1
                            | DiffRowKind.Context -> equal <- equal + 1
                            | _ -> ()
                | _ -> ()
        { Pages = pages; Scans = scans; UnalignedPrevious = unalignedPrevious; UnalignedCurrent = unalignedCurrent; Added = added; Removed = removed; Equal = equal }

    /// Reads every page. The hook runs after each request and before the next one.
    let private readAll (session: TextDiffSession) (between: TextDiffSession -> Async<unit>) = async {
        let pages = ResizeArray<DiffPage>()
        let mutable scans = 0
        let mutable requests = 0
        let mutable result = EngineResult.Canceled
        let mutable pending = true
        let! first = session.FirstPage(fun () -> false)
        result <- first
        while pending do
            requests <- requests + 1
            if requests > 200_000 then failwith $"The session did not finish. Last: {result}"
            do! between session
            match result with
            | EngineResult.Ok(Resumable.Ready page) ->
                pages.Add page
                match page.NextCursor with
                | None -> pending <- false
                | Some cursor ->
                    let! next = session.ReadPage cursor (fun () -> false)
                    result <- next
            | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                scans <- scans + 1
                let! next = session.ReadPage continuation (fun () -> false)
                result <- next
            | EngineResult.Failed(code, message, detail) -> failwith $"The session failed with {code}: {message}. Detail: {detail}."
            | EngineResult.Canceled -> failwith "The session canceled an uncanceled request."
        return shapeOf (pages.ToArray()) scans
    }

    let private noHook (_: TextDiffSession) = async.Return()

    let private runWith (sessionConfig: SessionConfig) (ledger: Ledger) between (previous: SourceLine array) (current: SourceLine array) = async {
        let host = Host.createInMemory (ManualClock 0.0 :> IClock)
        let! session = TextDiffSession.create host ledger sessionConfig (fun _ -> 1) (spec (encode previous)) (spec (encode current))
        let! shape = readAll session between
        let parts = shape.Pages |> Array.collect (fun page -> page.Parts)
        Check.sequence previous (rebuild previous parts true) "The pages cover every previous line exactly once."
        Check.sequence current (rebuild current parts false) "The pages cover every current line exactly once."
        Check.true' shape.Pages[shape.Pages.Length - 1].OutputComplete "The last page completes the output."
        do! session.Close()
        Check.equal 0L (ledger.Used AllocationCategory.SampledIndexes) "The resync index memory is released."
        return shape
    }

    let private run sessionConfig previous current = runWith sessionConfig (Ledger()) noHook previous current

    let private phaseCases: (string * (unit -> Async<unit>)) list = [
        "an insertion larger than a window resyncs at the unchanged side", fun () -> async {
            let previous = lines "line-" 0 500
            let current = Array.concat [ previous[.. 99]; lines "new-" 0 300; previous[100 ..] ]
            let! shape = run (smallConfig "resync-insert") previous current
            Check.equal 300 shape.Added "Every inserted line is an added row."
            Check.equal 0 shape.Removed "No line is removed."
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "A pure insertion is not unaligned."
        }
        "an insertion larger than a window resyncs when the alignment step budget ends the window", fun () -> async {
            // The window alignment gives up on a gap of unmatched lines after its step budget. The window is then
            // one unaligned region, which must not hide the place where the sources line up again.
            let pad (value: int) = (string value).PadLeft(7, '0')
            let generated index = { Text = $"row {pad index},value {pad ((index * 7) % 10_000_000)},sample S{pad index}"; Ending = LineEnding.LF }
            let previous = Array.init 600 (fun index -> generated (index + 1))
            let inserted = Array.init 300 (fun index -> { Text = $"inserted {pad (index + 1)} in the middle of the file"; Ending = LineEnding.LF })
            let current = Array.concat [ previous[.. 99]; inserted; previous[100 ..] ]
            let sessionConfig = { smallConfig "resync-step-budget" with MyersStepsPerGap = 1_000; ResyncSampleModulus = 4 }
            let! shape = run sessionConfig previous current
            Check.equal 300 shape.Added "Every inserted line is an added row."
            Check.equal 0 shape.Removed "No line is removed."
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "A pure insertion is not unaligned."
        }
        "a deletion larger than a window resyncs at the unchanged side", fun () -> async {
            let previous = lines "line-" 0 500
            let current = Array.append previous[.. 99] previous[400 ..]
            let! shape = run (smallConfig "resync-delete") previous current
            Check.equal 300 shape.Removed "Every deleted line is a removed row."
            Check.equal 0 shape.Added "No line is added."
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "A pure deletion is not unaligned."
        }
        "an insertion and a deletion together leave one unaligned region", fun () -> async {
            let previous = lines "line-" 0 500
            let current = Array.concat [ previous[.. 99]; lines "other-" 0 300; previous[400 ..] ]
            let! shape = run (smallConfig "resync-both") previous current
            Check.equal 300 shape.UnalignedPrevious "The replaced previous lines are unaligned."
            Check.equal 300 shape.UnalignedCurrent "The replacement lines are unaligned."
        }
        "a full rewrite is one unaligned remainder", fun () -> async {
            let previous = lines "old-" 0 400
            let current = lines "new-" 0 400
            let! shape = run (smallConfig "resync-rewrite") previous current
            Check.equal 400 shape.UnalignedPrevious "Every previous line is unaligned."
            Check.equal 400 shape.UnalignedCurrent "Every current line is unaligned."
        }
        "a matching EOF tail confirms a rewrite with extra current lines", fun () -> async {
            for extra in [ 0; 1; 20 ] do
                let previous = Array.append (lines "old-" 0 3_000) (lines "trailer-" 0 3)
                let current = Array.concat [ lines "new-" 0 3_000; lines "trailer-" 0 3; lines "tail-" 0 extra ]
                let! shape = run (smallConfig $"resync-eof-{extra}") previous current
                Check.equal 3_000 shape.UnalignedPrevious "The rewritten previous lines form an unaligned region."
                Check.equal 3_000 shape.UnalignedCurrent "The rewritten current lines form an unaligned region."
                Check.equal extra shape.Added "Every extra current line is added."
                Check.equal 0 shape.Removed "No previous line is removed."
        }
        "a sync candidate that fails its confirmation is rejected", fun () -> async {
            let previous = lines "line-" 0 500
            let inserted = lines "other-" 0 300
            inserted[100] <- previous[250]
            let current = Array.concat [ previous[.. 99]; inserted; previous[400 ..] ]
            let! shape = run (smallConfig "resync-false-candidate") previous current
            Check.equal 300 shape.UnalignedPrevious "The rejected candidate leaves the whole region unaligned."
            Check.equal 300 shape.UnalignedCurrent "The rejected candidate leaves the whole region unaligned."
        }
        "colliding hashes still resync correctly", fun () -> async {
            let previous = lines "line-" 0 500
            let current = Array.concat [ previous[.. 99]; lines "new-" 0 300; previous[100 ..] ]
            let sessionConfig = { smallConfig "resync-collisions" with HashMaskForTesting = Some(3u, 0u) }
            let! shape = run sessionConfig previous current
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "Confirmation by text resolves every collision."
        }
    ]


    /// Every line of a source, found with one uninterrupted scan.
    let private scanAll (bytes: byte[]) =
        let state = Scanner.create TextEncoding.Utf8 0L
        let found = ResizeArray<ScannedLine>()
        let batch = LineBatch(256)
        let meter = Meter.create (ManualClock 0.0 :> IClock) { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1e15; QuantumMs = 1e15 }
        let onLines (lines: LineBatch) =
            for index = 0 to lines.Count - 1 do found.Add(lines.Line index)
        Scanner.scanChunk state bytes 0 bytes.Length true meter batch onLines |> ignore
        found.ToArray()

    /// Reads one line to completion through the public line read, following its continuations.
    let private readWholeLine (session: TextDiffSession) side (number: int64) = async {
        let mutable result = EngineResult.Canceled
        let! initial = session.ReadLine(side, number, 0L, 1_000, None, fun () -> false)
        result <- initial
        let mutable line = None
        while line.IsNone do
            match result with
            | EngineResult.Ok(Resumable.Ready value) -> line <- Some value
            | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                let! next = session.ReadLine(side, number, 0L, 1_000, Some continuation, fun () -> false)
                result <- next
            | other -> failwith $"The line read returned {other}."
        return line.Value
    }

    let private seekCases: (string * (unit -> Async<unit>)) list = [
        "reading a line returns the text of that source line on both sides", fun () -> async {
            let previous = lines "line-" 0 500
            let current = Array.concat [ previous[.. 99]; lines "new-" 0 300; previous[100 .. 399]; lines "other-" 0 50; previous[450 ..] ]
            let host = Host.createInMemory (ManualClock 0.0 :> IClock)
            let! session = TextDiffSession.create host (Ledger()) (smallConfig "seek") (fun _ -> 1) (spec (encode previous)) (spec (encode current))
            let! _ = readAll session noHook
            for side, source in [ DiffSide.Previous, previous; DiffSide.Current, current ] do
                for number in [ 0; 1; 63; 64; 65; 99; 100; 250; 399; 400; source.Length - 1 ] do
                    let! found = readWholeLine session side (int64 number)
                    Check.equal source[number].Text found.Slice.Text "A line read matches the source line."
                    Check.equal (int64 number) found.Number "A line read reports the requested line number."
            do! session.Close()
        }
    ]
    let private tightLimits = { Limits.defaults with MaxUnits = 256; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }

    let private checkSameShape (expected: Shape) (actual: Shape) =
        Check.equal expected.Added actual.Added "The suspended run has the same added rows."
        Check.equal expected.Removed actual.Removed "The suspended run has the same removed rows."
        Check.equal expected.UnalignedPrevious actual.UnalignedPrevious "The suspended run has the same unaligned previous lines."
        Check.equal expected.UnalignedCurrent actual.UnalignedCurrent "The suspended run has the same unaligned current lines."

    /// A replacement of the same length keeps both sides at the same line numbers, so a resync scan that is
    /// cut short by a budget still finds the end of the region.
    let private replacedMiddle () =
        let previous = lines "line-" 0 700
        let current = Array.copy previous
        for index = 100 to 499 do current[index] <- { current[index] with Text = "other-" + string index }
        current[600] <- { current[600] with Text = "edited" }
        previous, current

    let private hasAlignedRowWith (shape: Shape) (text: string) =
        shape.Pages
        |> Array.exists (fun page ->
            page.Parts
            |> Array.exists (fun part ->
                match part with
                | DiffPart.Hunk { Body = HunkBody.AlignedRows values } ->
                    values |> Array.exists (fun row -> row.Current |> Option.exists (fun line -> line.Slice.Text = text))
                | _ -> false))

    /// A budget that runs out ends the region and lets the scan go on. A later edit is still found, and a match
    /// that only the exhausted scan could have found is left unaligned.
    let private budgetCase name (adjust: SessionConfig -> SessionConfig) = fun () -> async {
        let previous, current = replacedMiddle ()
        let! limited = run (adjust (smallConfig (name + "-limited"))) previous current
        Check.true' (limited.UnalignedPrevious > 0) "The replaced lines are unaligned."
        Check.true' (hasAlignedRowWith limited "edited") "A later edit is an aligned row."
        let shifted = Array.concat [ previous[.. 99]; lines "new-" 0 300; previous[100 ..] ]
        let! unbounded = run (smallConfig (name + "-shift-plain")) previous shifted
        let! cut = run (adjust (smallConfig (name + "-shift-limited"))) previous shifted
        Check.equal 0 (unbounded.UnalignedPrevious + unbounded.UnalignedCurrent) "The unbounded scan finds the shifted match."
        Check.true' (cut.UnalignedCurrent > 0) "The exhausted budget leaves an unaligned region."
    }

    let private budgetCases: (string * (unit -> Async<unit>)) list = [
        "an index that fills up ends an unaligned region and the scan continues", budgetCase "budget-index" (fun sessionConfig -> { sessionConfig with ResyncIndexCapacity = 40 })
        "a cumulative line limit ends an unaligned region and the scan continues", budgetCase "budget-lines" (fun sessionConfig -> { sessionConfig with ResyncScanLines = 60 })
        "a cumulative byte limit ends an unaligned region and the scan continues", budgetCase "budget-bytes" (fun sessionConfig -> { sessionConfig with ResyncScanBytes = 500.0 })
        "a request budget that suspends the search gives the same output", fun () -> async {
            let previous = lines "line-" 0 800
            let current = Array.concat [ previous[.. 99]; lines "new-" 0 300; previous[100 .. 399]; lines "other-" 0 200; previous[600 ..] ]
            let! plain = run (smallConfig "suspend-plain") previous current
            let limited = { smallConfig "suspend-limited" with Limits = { Limits.defaults with MaxUnits = 32; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 } }
            let! suspended = run limited previous current
            Check.true' (suspended.Scans > 0) "The search suspends between pages."
            checkSameShape plain suspended
        }
    ]

    /// An insertion of the given size at line 100 that a deletion of the same size at line 500 cancels out, and an
    /// edit near the end. Both sides scan the same number of lines from the start of each search, so the search
    /// finds the shifted match only when the insertion is shorter than the scan limit.
    let private shiftedBlock (size: int) =
        let previous = lines "line-" 0 800
        let current = Array.concat [ previous[.. 99]; lines "new-" 0 size; previous[100 .. 499]; previous[500 + size ..] ]
        let edited = current.Length - 50
        current[edited] <- { current[edited] with Text = "edited" }
        previous, current

    let private scanLimitCases: (string * (unit -> Async<unit>)) list = [
        "an insertion below the forward scan limit realigns", fun () -> async {
            let previous, current = shiftedBlock 80
            let! shape = run { smallConfig "limit-below" with ResyncScanLines = 100 } previous current
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "The insertion is not unaligned."
            Check.equal 80 shape.Added "Every inserted line is an added row."
            Check.equal 80 shape.Removed "Every deleted line is a removed row."
            Check.true' (hasAlignedRowWith shape "edited") "The later edit is an aligned row."
        }
        "an insertion above the forward scan limit becomes unaligned blocks and alignment resumes after them", fun () -> async {
            let previous, current = shiftedBlock 150
            let! shape = run { smallConfig "limit-above" with ResyncScanLines = 100 } previous current
            Check.true' (shape.UnalignedPrevious > 0 && shape.UnalignedCurrent > 0) "The insertion shows as unaligned lines on both sides."
            Check.true' (hasAlignedRowWith shape "edited") "The later edit is an aligned row, so the count restarted with each search."
        }
    ]

    let private byteOrderMarkCases: (string * (unit -> Async<unit>)) list = [
        "a resync that starts in the first window skips a byte order mark", fun () -> async {
            let previous = lines "old-" 0 300
            let current = Array.append (lines "new-" 0 150) previous[150 ..]
            let host = Host.createInMemory (ManualClock 0.0 :> IClock)
            let! session = TextDiffSession.create host (Ledger()) (smallConfig "bom-resync") (fun _ -> 1) (bomSpec (encode previous)) (spec (encode current))
            let! shape = readAll session noHook
            let parts = shape.Pages |> Array.collect (fun page -> page.Parts)
            Check.sequence previous (rebuild previous parts true) "The pages cover every previous line without the mark."
            Check.sequence current (rebuild current parts false) "The pages cover every current line."
            do! session.Close()
        }
    ]

    /// Opens a session on the shared ledger, makes up to limit requests, and closes it. Returns None when the
    /// session was still open at the close, otherwise whether it ended in a failure.
    let private closeAfter (ledger: Ledger) sessionConfig (previous: byte[]) (current: byte[]) (limit: int) = async {
        let host = Host.createInMemory (ManualClock 0.0 :> IClock)
        let! session = TextDiffSession.create host ledger sessionConfig (fun _ -> 1) (spec previous) (spec current)
        let! first = session.FirstPage(fun () -> false)
        let mutable result = first
        let mutable requests = 1
        let mutable ended = None
        while ended.IsNone && requests < limit do
            match result with
            | EngineResult.Ok(Resumable.Ready page) ->
                match page.NextCursor with
                | None -> ended <- Some false
                | Some cursor ->
                    let! next = session.ReadPage cursor (fun () -> false)
                    result <- next
                    requests <- requests + 1
            | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                let! next = session.ReadPage continuation (fun () -> false)
                result <- next
                requests <- requests + 1
            | EngineResult.Failed _ -> ended <- Some true
            | EngineResult.Canceled -> failwith "The session canceled an uncanceled request."
        do! session.Close()
        return ended
    }

    let private allCategories = [
        AllocationCategory.PreviousWindows
        AllocationCategory.CurrentWindows
        AllocationCategory.SampledIndexes
        AllocationCategory.ChunkScratch
        AllocationCategory.AlignmentScratch
        AllocationCategory.ResponseData
        AllocationCategory.RetainedBlobs
    ]

    let private checkLedgerEmpty (ledger: Ledger) (message: string) =
        for category in allCategories do
            Check.equal 0L (ledger.Used category) $"{message} ({category})"

    let private giantLimits = { tightLimits with MaxUnits = 2 }

    let private giantConfig sessionId = { smallConfig sessionId with WindowMaxBytes = 2_048 }

    let private giantText marker length = marker + String('g', length - marker.Length)

    /// Rows show at most one slice of a long line. The slice carries the full length and a prefix of the text.
    let private sliceMatches (expected: SourceLine) (line: DiffLine) =
        line.Slice.TotalUtf16 = Some(int64 expected.Text.Length)
        && line.Slice.OffsetUtf16 = 0L
        && expected.Text.StartsWith(line.Slice.Text, StringComparison.Ordinal)

    /// Checks that every source line appears once, in order, with a matching slice.
    let private checkCoverage (expected: SourceLine array) (pages: DiffPage array) previousSide =
        let seen = ResizeArray<DiffLine>()
        let add (line: DiffLine) = seen.Add line
        for page in pages do
            for part in page.Parts do
                match part with
                | DiffPart.Hunk fragment ->
                    match fragment.Body with
                    | HunkBody.AlignedRows values ->
                        for row in values do
                            (if previousSide then row.Previous else row.Current) |> Option.iter add
                    | HunkBody.UnalignedSides(previous, current) ->
                        for line in (if previousSide then previous else current) do add line
                | DiffPart.HiddenEqual gap ->
                    let range = if previousSide then gap.PreviousRange else gap.CurrentRange
                    for index = int range.Start to int (range.Start + range.Count) - 1 do
                        add { Number = int64 index; Ending = LineEnding.LF; Slice = { OffsetUtf16 = 0L; TotalUtf16 = Some(int64 expected[index].Text.Length); Text = expected[index].Text; Highlights = Array.empty } }
                | DiffPart.ExpandedContext _ -> failwith "The session returned an unsupported expanded context part."
        Check.equal expected.Length seen.Count "Every source line appears once."
        for index = 0 to expected.Length - 1 do
            Check.equal (int64 index) seen[index].Number "The lines keep their order."
            Check.true' (sliceMatches expected[index] seen[index]) "Each line carries its full length and a text prefix."

    let private giantRun sessionConfig (limits: Limits) (previous: SourceLine array) (current: SourceLine array) = async {
        let host = Host.createInMemory (ManualClock 0.0 :> IClock)
        let! session = TextDiffSession.create host (Ledger()) { sessionConfig with Limits = limits } (fun _ -> 1) (spec (encode previous)) (spec (encode current))
        let! shape = readAll session noHook
        checkCoverage previous shape.Pages true
        checkCoverage current shape.Pages false
        do! session.Close()
        return shape
    }

    let private giantLineCases: (string * (unit -> Async<unit>)) list = [
        "a changed line larger than the window on one side pairs with its previous line", fun () -> async {
            let previous = lines "line-" 0 60
            let current = Array.copy previous
            current[30] <- { current[30] with Text = giantText "giant-" 20_000 }
            let! shape = giantRun (giantConfig "giant-one-side") giantLimits previous current
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "The giant line pairs with its previous line."
            Check.true' (shape.Scans > 0) "The giant line spans more than one request."
        }
        "changed lines larger than the window on both sides pair as one changed row", fun () -> async {
            let previous = lines "line-" 0 60
            previous[30] <- { previous[30] with Text = giantText "old-" 30_000 }
            let current = Array.copy previous
            current[30] <- { current[30] with Text = giantText "new-" 25_000 }
            let! shape = giantRun (giantConfig "giant-both-sides") giantLimits previous current
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "The two giant lines pair as one changed row."
        }
        "an inserted giant line is one added row", fun () -> async {
            let previous = lines "line-" 0 60
            let current = Array.concat [ previous[.. 29]; [| { Text = giantText "giant-" 20_000; Ending = LineEnding.LF } |]; previous[30 ..] ]
            let! shape = giantRun (giantConfig "giant-insert") giantLimits previous current
            Check.equal 1 shape.Added "The giant line is one added row."
            Check.equal 0 shape.Removed "No line is removed."
        }
        "a giant line inside a large insertion is skipped over by the resync scan", fun () -> async {
            let previous = lines "line-" 0 300
            let inserted = lines "new-" 0 200
            inserted[80] <- { inserted[80] with Text = giantText "giant-" 20_000 }
            let current = Array.concat [ previous[.. 49]; inserted; previous[50 ..] ]
            let! shape = giantRun (giantConfig "giant-resync") { giantLimits with MaxUnits = 32 } previous current
            Check.equal 200 shape.Added "Every inserted line is an added row."
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "The insertion is not unaligned."
        }
    ]

    let private ledgerCases: (string * (unit -> Async<unit>)) list = [
        "closing a session in any state releases its reservations on a shared ledger", fun () -> async {
            let ledger = Ledger()
            let tiny maxUnits (sessionConfig: SessionConfig) = { sessionConfig with Limits = { sessionConfig.Limits with MaxUnits = maxUnits } }
            let line text = { Text = text; Ending = LineEnding.LF }
            let alignPrevious = Array.init 80 (fun index -> line (if index % 3 = 0 then "shared" else "old-" + string index))
            let alignCurrent = Array.init 80 (fun index -> line (if index % 3 = 0 then "shared" else "new-" + string index))
            let resyncPrevious = lines "line-" 0 800
            let resyncCurrent = Array.concat [ resyncPrevious[.. 99]; lines "new-" 0 300; resyncPrevious[100 ..] ]
            let longText = String('a', 160_000)
            let longPrevious = [| line "head"; line longText; line "tail" |]
            let longCurrent = [| line "head"; line (longText.Substring(0, 80_000) + "b" + longText.Substring 80_001); line "tail" |]
            let invalid (bytes: byte[]) = Array.mapi (fun index value -> if index = bytes.Length - 20 then 0xFFuy else value) bytes
            let contentPrevious = encode (lines "line-" 0 3_000)
            let scenarios = [
                "mid-alignment", tiny 40 (smallConfig "ledger-align"), encode alignPrevious, encode alignCurrent, false
                "in resync", tiny 32 (smallConfig "ledger-resync"), encode resyncPrevious, encode resyncCurrent, false
                "with a pending long pair", tiny 64 { smallConfig "ledger-long" with WindowMaxBytes = 2_048 }, encode longPrevious, encode longCurrent, false
                "after a content failure", tiny 64 (smallConfig "ledger-content"), contentPrevious, invalid contentPrevious, true
            ]
            for name, sessionConfig, previous, current, expectFailure in scenarios do
                let mutable closedOpen = 0
                let mutable outcome = None
                let mutable requests = 1
                while outcome.IsNone && requests <= 400 do
                    let! ended = closeAfter ledger sessionConfig previous current requests
                    checkLedgerEmpty ledger $"The ledger is empty after closing {name} at request {requests}"
                    match ended with
                    | None -> closedOpen <- closedOpen + 1
                    | Some failed -> outcome <- Some failed
                    requests <- requests + 1
                Check.true' (closedOpen >= 3) $"The sweep for {name} closed {closedOpen} open sessions."
                Check.equal (Some expectFailure) outcome $"The sweep for {name} ends as expected."
            let! shape = runWith (smallConfig "ledger-second") ledger noHook resyncPrevious resyncCurrent
            Check.equal 300 shape.Added "A second session on the same ledger runs to completion."
            checkLedgerEmpty ledger "The ledger is empty after the second session"
        }
    ]

    /// Texts with a given low hash bit. A modulus of 2 samples the lines whose bit is clear.
    let private textsWithSampling (sampled: bool) (prefix: string) (count: int) =
        let found = ResizeArray<string>()
        let mutable number = 0
        while found.Count < count do
            let text = prefix + string number
            let scanned = scanAll (Encoding.UTF8.GetBytes(text + "\n"))
            let key = int scanned[0].KeyLo
            if ((key &&& 1) = 0) = sampled then found.Add text
            number <- number + 1
        found.ToArray()

    let private plainLine text = { Text = text; Ending = LineEnding.LF }

    let private extensionCases: (string * (unit -> Async<unit>)) list = [
        "a match that a sliding verification window reached is extended back to the cursors", fun () -> async {
            // The repeated lines are not sampled, so the first sampled match lies far behind the start of the region
            // and the verification window has slid past the true start of the match.
            let repeated = plainLine (Array.head (textsWithSampling false "repeat-" 1))
            let inserted = plainLine (Array.head (textsWithSampling false "inserted-" 1))
            let tail = textsWithSampling true "tail-" 8 |> Array.map plainLine
            let previous = Array.concat [ Array.create 5_000 repeated; tail ]
            let current = Array.concat [ [| inserted |]; Array.create 5_000 repeated; tail ]
            let sessionConfig = { smallConfig "resync-extend-back" with WindowMaxLines = 1; ResyncSampleModulus = 2 }
            let! shape = run sessionConfig previous current
            Check.equal 1 shape.Added "The inserted line is one added row."
            Check.equal 0 shape.Removed "No line is removed."
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "The insertion is not unaligned."
            Check.true' (hasAlignedRowWith shape inserted.Text) "The inserted line is an aligned row."
        }
        "a sampled index never holds more entries of one hash than the chain limit", fun () -> async {
            let index = SampledIndex 64
            index.Allocate()
            // These hashes share bucket 0 of an index with 64 entries.
            let hashes = [| 0; 4448; 4592; 4736; 4880 |]
            let line = ref 0.0
            let feed (hash: int) =
                index.TryAdd(line.Value * 10.0, line.Value, hash, 32, 8) |> ignore
                line.Value <- line.Value + 1.0
            for hash in [ 0; 4448; 4592; 4736 ] do
                for _ in 1..8 do feed hash
            feed 4880
            feed 0
            let counts = Array.zeroCreate<int> hashes.Length
            let mutable id = index.Newest 0
            while id >= 0 do
                let position = hashes |> Array.findIndex (fun hash -> hash = index.Hash id)
                counts[position] <- counts[position] + 1
                id <- index.Older id
            for position = 0 to hashes.Length - 1 do
                Check.true' (counts[position] <= 8) "No hash has more entries than the chain limit."
            Check.equal 8 counts[0] "The first hash keeps its entries."
            Check.true' (counts[1] > 0 && counts[2] > 0 && counts[3] > 0) "The fed hashes share one bucket."
        }
    ]

    // The cases below use files larger than one window with many separate edits. A window at its largest size
    // settles on the unchanged runs between the edits.
    let private proseLines (prefix: string) (count: int) =
        Array.init count (fun index -> plainLine (if index % 2 = 1 then "" else $"{prefix} paragraph {index} has some words in it."))

    let private defaultConfig sessionId =
        { SessionConfig.defaults sessionId with Limits = { Limits.defaults with RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 } }

    /// A block of ten new lines after every 42nd old line, or one new line after every fifth old line.
    let private spreadInsertions (count: int) (every: int) (block: int) =
        let previous = proseLines "old" count
        let current = ResizeArray<SourceLine>()
        let mutable blocks = 0
        for index = 0 to previous.Length - 1 do
            current.Add previous[index]
            if index % every = every - 1 then
                if block = 1 then current.Add(plainLine $"new line {blocks}") else current.AddRange(proseLines $"new{blocks}" block)
                blocks <- blocks + 1
        previous, current.ToArray()

    let private jsonRecord (id: string) (value: int) (last: bool) =
        [| "  {"; $"    \"id\": \"{id}\","; $"    \"value\": {value}"; (if last then "  }" else "  },") |] |> Array.map plainLine

    /// Ten-line records where only the value line differs between two records with the same id.
    let private wideRecord (id: string) (value: int) =
        [| "  {"; $"    \"id\": \"{id}\","; "    \"kind\": \"item\","; "    \"enabled\": true,"; "    \"tags\": [],"; "    \"owner\": null,"; "    \"note\": \"\","; $"    \"value\": {value}"; "  },"; "" |]
        |> Array.map plainLine

    let private uniqueLines (prefix: string) (start: int) (count: int) =
        Array.init count (fun index -> plainLine $"{prefix} line {start + index} text")

    let private brackets (body: SourceLine array) = Array.concat [ [| plainLine "[" |]; body; [| plainLine "]" |] ]

    let private checkExact name (shape: Shape) equal added removed =
        Check.equal equal shape.Equal $"{name}: every unchanged line is an equal row."
        Check.equal added shape.Added $"{name}: every inserted line is an added row."
        Check.equal removed shape.Removed $"{name}: every deleted line is a removed row."
        Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) $"{name}: no line is unaligned."

    let private settleCases: (string * (unit -> Async<unit>)) list = [
        "spread insertions in a file of two windows align without unaligned lines", fun () -> async {
            let previous, current = spreadInsertions 56_000 42 10
            let! shape = run (defaultConfig "settle-spread-56k") previous current
            checkExact "spread 56,000" shape 56_000 13_330 0
        }
        "spread insertions in a file of more than one window align without unaligned lines", fun () -> async {
            let previous, current = spreadInsertions 80_000 42 10
            let! shape = run (defaultConfig "settle-spread-80k") previous current
            checkExact "spread 80,000" shape 80_000 19_040 0
        }
        "spread deletions in a file of more than one window align without unaligned lines", fun () -> async {
            let previous, current = spreadInsertions 80_000 42 10
            let! shape = run (defaultConfig "settle-spread-delete") current previous
            checkExact "spread deletion 80,000" shape 80_000 0 19_040
        }
        "one inserted line after every fifth line aligns in a file of more than one window", fun () -> async {
            let previous, current = spreadInsertions 80_000 5 1
            let! shape = run (defaultConfig "settle-drift") previous current
            checkExact "drift 80,000" shape 80_000 16_000 0
        }
        "duplicated paragraphs inserted before a file align with the unchanged lines", fun () -> async {
            // Every line occurs twice in its own window, and the window ends inside a short equal run of repeats.
            let paragraph (prefix: string) (index: int) =
                [| plainLine $"{prefix}{index}"; plainLine $"{prefix}{index}"; plainLine ""; plainLine "" |]
            let previous = Array.init 3_000 (paragraph "old") |> Array.concat
            let current = Array.append (Array.init 1_152 (paragraph "new") |> Array.concat) previous
            let! shape = run { defaultConfig "settle-duplicates" with WindowMaxLines = 2_048 } previous current
            checkExact "duplicated paragraphs" shape 12_000 4_608 0
        }
        "a chance match inside an insertion of 70,000 lines does not pin the window", fun () -> async {
            let records = 18_750
            let previous = brackets (Array.init records (fun index -> jsonRecord $"old-{index}" (index * 7) (index = records - 1)) |> Array.concat)
            let at = 1 + 250 * 4
            // New record 5000 repeats the value of old record 300 and sits in the first half of the window.
            let inserted = Array.init 17_500 (fun index -> jsonRecord $"new-{index}" (if index = 5_000 then 300 * 7 else 1_000_000 + index) false) |> Array.concat
            let current = Array.concat [ previous[.. at - 1]; inserted; previous[at ..] ]
            let! shape = run (defaultConfig "settle-chance-json") previous current
            checkExact "chance match in JSON" shape 75_002 70_000 0
        }
        "records that reuse the ids of the old records inside an insertion of 70,000 lines align", fun () -> async {
            let records = 18_750
            let previous = brackets (Array.init records (fun index -> jsonRecord $"old-{index}" (index * 7) (index = records - 1)) |> Array.concat)
            let at = 1 + 250 * 4
            let inserted = Array.init 17_500 (fun index -> jsonRecord $"old-{index}" (1_000_000 + index) false) |> Array.concat
            let current = Array.concat [ previous[.. at - 1]; inserted; previous[at ..] ]
            let! shape = run (defaultConfig "settle-colliding-json") previous current
            checkExact "colliding ids in JSON" shape 75_002 70_000 0
        }
        "ten-line records that reuse the ids of the old records inside an insertion align", fun () -> async {
            let records = 7_500
            let previous = brackets (Array.init records (fun index -> wideRecord $"old-{index}" (index * 7)) |> Array.concat)
            let at = 1 + 250 * 10
            let inserted = Array.init 7_000 (fun index -> wideRecord $"old-{index}" (1_000_000 + index)) |> Array.concat
            let current = Array.concat [ previous[.. at - 1]; inserted; previous[at ..] ]
            let! shape = run (defaultConfig "settle-colliding-wide") previous current
            checkExact "colliding ids of wide records" shape 75_002 70_000 0
        }
        "a copied block of seven or eight lines inside an insertion of 70,000 lines does not pin the window", fun () -> async {
            for block in [ 8; 7 ] do
                let previous = uniqueLines "old" 0 100_000
                let inserted = uniqueLines "new" 0 70_000
                Array.blit previous (1_000 + 30_000) inserted 20_000 block
                let current = Array.concat [ previous[.. 999]; inserted; previous[1_000 ..] ]
                let! shape = run (defaultConfig $"settle-copied-block-{block}") previous current
                checkExact $"copied block of {block}" shape 100_000 70_000 0
        }
        "eight copied blocks of eight lines inside an insertion of 70,000 lines do not pin the window", fun () -> async {
            // Each block is a copy of old lines. Together they hold 64 anchored lines, but they sit among replaced
            // lines, so the window goes to the forward search instead of settling on them.
            let previous = uniqueLines "old" 0 100_000
            let inserted = uniqueLines "new" 0 70_000
            for block = 0 to 7 do
                Array.blit previous (1_000 + 30_000 + block * 500) inserted (20_000 + block * 500) 8
            let current = Array.concat [ previous[.. 999]; inserted; previous[1_000 ..] ]
            let! shape = run (defaultConfig "settle-copied-blocks-8x8") previous current
            checkExact "eight copied blocks of eight lines" shape 100_000 70_000 0
        }
        "a drifting region before an insertion of 70,000 lines keeps its alignment", fun () -> async {
            let drifting = uniqueLines "old" 0 3_000
            let spread = Array.init 600 (fun block -> Array.append drifting[block * 5 .. block * 5 + 4] (uniqueLines $"ins{block}" 0 10)) |> Array.concat
            let tail = uniqueLines "c" 0 100_000
            let inserted = uniqueLines "new" 0 70_000
            inserted[30_000] <- tail[40_000]
            let! shape = run (defaultConfig "settle-drift-before-insertion") (Array.append drifting tail) (Array.concat [ spread; inserted; tail ])
            checkExact "drift before an insertion" shape 103_000 76_000 0
        }
        "65 old lines in runs of one settle the window before an insertion of 70,000 lines and 64 go to the forward search", fun () -> async {
            for anchored in [ 65; 64 ] do
                let old = uniqueLines "old" 0 anchored
                let spread = Array.init anchored (fun index -> Array.append [| old[index] |] (uniqueLines $"ins{index}" 0 20)) |> Array.concat
                let tail = uniqueLines "c" 0 100_000
                let! shape = run (defaultConfig $"settle-anchored-{anchored}") (Array.append old tail) (Array.concat [ spread; uniqueLines "new" 0 70_000; tail ])
                if anchored = 65 then checkExact "65 anchored lines" shape 100_065 71_300 0
                else
                    Check.equal 100_001 shape.Equal "64 anchored lines: the forward search keeps one chance line."
                    Check.equal 63 shape.UnalignedPrevious "64 anchored lines: previous lines are unaligned."
                    Check.equal 71_343 shape.UnalignedCurrent "64 anchored lines: current lines are unaligned."
        }
        "nine runs of eight old lines settle the window before an insertion of 70,000 lines and runs of seven go to the forward search", fun () -> async {
            for run' in [ 8; 7 ] do
                let old = uniqueLines "old" 0 (9 * run')
                let spread = Array.init 9 (fun index -> Array.append old[index * run' .. (index + 1) * run' - 1] (uniqueLines $"ins{index}" 0 20)) |> Array.concat
                let tail = uniqueLines "c" 0 100_000
                let! shape = run (defaultConfig $"settle-runs-{run'}") (Array.append old tail) (Array.concat [ spread; uniqueLines "new" 0 70_000; tail ])
                if run' = 8 then checkExact "runs of eight" shape (100_000 + 72) (70_000 + 180) 0
                else
                    Check.equal 100_007 shape.Equal "Runs of seven: the forward search keeps seven lines."
                    Check.equal 56 shape.UnalignedPrevious "Runs of seven: previous lines are unaligned."
        }
        "long lines of three contents with one edit every 60 lines align in byte-limited windows", fun () -> async {
            let contents = [| 'A'; 'B'; 'C' |] |> Array.map (fun letter -> plainLine (String(letter, 2_048)))
            let previous = Array.init 6_000 (fun index -> contents[index % 3])
            let current = previous |> Array.mapi (fun index line -> if index > 0 && index % 60 = 0 then contents[(index + 1) % 3] else line)
            let! shape = run { defaultConfig "settle-long-repeats" with WindowMaxBytes = (512 * 1024) } previous current
            Check.true' (shape.Equal >= 5_800) $"Almost every unchanged line is an equal row, got {shape.Equal}."
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "No line is unaligned."
        }
        "a window of repeated lines settles on an equal run of 64 lines and sends one of 63 to the forward search", fun () -> async {
            for run' in [ 64; 63 ] do
                let pair (prefix: string) (index: int) = [| plainLine $"{prefix}{index}"; plainLine $"{prefix}{index}" |]
                let shared = Array.create run' (plainLine "row")
                // Every line occurs twice in its window, and the last pair of the odd case gets a third copy.
                let side (prefix: string) =
                    Array.concat [
                        Array.init 48 (pair prefix) |> Array.concat
                        (if run' % 2 = 1 then [| plainLine $"{prefix}47" |] else [||])
                        shared
                        Array.init 1_500 (pair (prefix + "t")) |> Array.concat
                    ]
                let! shape = run { defaultConfig $"settle-repeated-run-{run'}" with WindowMaxLines = 256 } (side "old") (side "new")
                Check.equal (if run' = 64 then 64 else 0) shape.Equal $"A run of {run'} equal lines: equal rows."
        }
    ]

    // Small edits for which git's line diff reports the fewest changed lines. A case has one added or removed row
    // per changed line and no replaced row.
    let private parityCases: (string * (unit -> Async<unit>)) list = [
        "two adjacent lines that swapped places are one added and one removed row", fun () -> async {
            let previous = lines "line-" 0 20
            let current = Array.copy previous
            current[10] <- previous[11]
            current[11] <- previous[10]
            let! shape = run (defaultConfig "parity-swap") previous current
            checkExact "swapped lines" shape 19 1 1
        }
        "a window that mostly differs after its last long run grows before it settles on chance matches", fun () -> async {
            // Every third inserted line up to line 132 copies a block line, four block lines apart. Each copy occurs
            // once per window and the copies chain in order. The block itself starts behind the end of the first
            // window of 512 current lines, so only a larger window sees the real match.
            let shared = lines "shared-" 0 330
            let block = lines "block-" 0 182
            let tail = lines "tail-" 0 600
            let inserted = lines "inserted-" 0 450
            for copy = 0 to 44 do
                inserted[3 * copy] <- block[4 * copy]
            let previous = Array.concat [ shared; block; tail ]
            let current = Array.concat [ [| plainLine "head" |]; shared; inserted; block; tail ]
            let! shape = run (defaultConfig "parity-copied-block") previous current
            checkExact "copied block" shape 1_112 451 0
        }
        "a line that moved across a short run of equal lines is one added and one removed row", fun () -> async {
            // The moved line and the lines behind the run occur once in each file, so a chain of unique lines
            // would anchor the moved line and report the whole run as changed. The files hold more lines than
            // the first window of 512, and a file of that size is aligned as one window with one pass over its
            // middle, which keeps the run equal as git does.
            let same = Array.create 10 (plainLine "same")
            let tail = lines "tail-" 0 600
            let previous = Array.concat [ [| plainLine "marker" |]; same; tail ]
            let current = Array.concat [ same; [| plainLine "marker" |]; tail; [| plainLine "extra" |] ]
            let! shape = run (defaultConfig "parity-moved-line") previous current
            checkExact "moved line" shape 610 2 1
        }
        "a heavy rewrite that exceeds the budget of the single pass keeps the alignment around its anchors", fun () -> async {
            // Four runs of 60 lines that all changed are separated by three lines that stay. The single pass gets
            // twice the step budget of a gap and needs more for the whole file, so it gives up. Each run between two
            // anchors fits the budget of a gap. A unique line sits behind fifty identical lines in the previous file
            // and in front of them in the current file. The single pass would keep the identical lines equal and
            // move the unique line (53 equal rows). The anchor path keeps the unique line equal and changes the
            // identical lines (4 equal rows), so the result shows that the single pass gave up.
            let rewrite prefix = Array.init 4 (fun run' -> Array.append (lines $"{prefix}-{run'}-" 0 60) [| plainLine $"anchor {run'}" |]) |> Array.concat
            let identical = Array.create 50 (plainLine "r")
            let previous = Array.concat [ [| plainLine "X" |]; identical; rewrite "old" ]
            let current = Array.concat [ identical; [| plainLine "X" |]; rewrite "new" ]
            let! shape = run { defaultConfig "parity-heavy-rewrite" with MyersStepsPerGap = 20_000 } previous.[.. previous.Length - 2] current.[.. current.Length - 2]
            Check.equal 4 shape.Equal "Heavy rewrite: the unique line and the three anchors are equal rows."
            Check.equal 50 shape.Added "Heavy rewrite: the identical lines in the current file are added rows."
            Check.equal 50 shape.Removed "Heavy rewrite: the identical lines in the previous file are removed rows."
            Check.equal 0 (shape.UnalignedPrevious + shape.UnalignedCurrent) "Heavy rewrite: no line is unaligned."
        }
    ]

    let private padding (prefix: string) (count: int) = Array.init count (fun index -> plainLine $"{prefix}{index}")

    // The cases below repeat small edits inside files with a shared trailing block, so that the files are larger
    // than the 20,000 lines that one window can take with the single Myers pass. The alignment then runs its anchor
    // path, which has its own rules for moved lines, swaps and sparse chance anchors.
    let private paddedCases: (string * (unit -> Async<unit>)) list = [
        "unique lines that moved across long equal runs are added and removed rows in a file above 20,000 lines", fun () -> async {
            let times count text = Array.create count (plainLine text)
            let runs = [| "a"; "b"; "c"; "d" |]
            let markers = [| "X"; "Y"; "Z" |]
            // The previous file holds p, a run, a marker, the next run and so on. The current file lists the markers first.
            let cluster markerCount =
                let previous = Array.concat [ [| plainLine "p" |]; Array.concat [ for index in 0 .. markerCount -> Array.append (times 600 runs[index]) (if index < markerCount then [| plainLine markers[index] |] else [||]) ] ]
                let current = Array.concat [ [| plainLine "qq" |]; Array.map plainLine markers[.. markerCount - 1]; Array.concat [ for index in 0 .. markerCount -> times 600 runs[index] ] ]
                previous, current
            let tail = padding "pad line " 25_000
            let one, oneCurrent = cluster 1
            let two, twoCurrent = cluster 2
            let three, threeCurrent = cluster 3
            for name, previous, current, markerCount in [ "1 up", one, oneCurrent, 1; "3 up", three, threeCurrent, 3; "2 down", twoCurrent, two, 2 ] do
                let! shape = run (defaultConfig ("padded-moved-" + name)) (Array.append previous tail) (Array.append current tail)
                checkExact name shape ((markerCount + 1) * 600 + 25_000) markerCount markerCount
        }
        "two adjacent lines that swapped places are one added and one removed row in a file above 20,000 lines", fun () -> async {
            let previous = Array.append (lines "line-" 0 20) (padding "pad line " 25_000)
            let current = Array.copy previous
            current[10] <- previous[11]
            current[11] <- previous[10]
            let! shape = run (defaultConfig "padded-swap") previous current
            checkExact "swapped lines" shape 25_019 1 1
        }
        "a line that moved across a short run of equal lines is one added and one removed row in a file of 15,000 lines", fun () -> async {
            // The files stay below 20,000 lines, so one window takes them with the single pass over its middle.
            let same = Array.create 10 (plainLine "same")
            let tail = padding "pad-" 14_400
            let previous = Array.concat [ [| plainLine "marker" |]; same; lines "tail-" 0 600; tail ]
            let current = Array.concat [ same; [| plainLine "marker" |]; lines "tail-" 0 600; [| plainLine "extra" |]; tail ]
            let! shape = run (defaultConfig "padded-moved-line-15k") previous current
            checkExact "moved line in 15,000 lines" shape 15_010 2 1
        }
        "a window that mostly differs after its confirmed run grows before it settles on chance matches in a file above 20,000 lines", fun () -> async {
            // The confirmed run is 330 unique lines, 330 identical lines, or 330 unique lines that the current file
            // edits at every 30th line. The inserted lines copy every fourth line of a block that starts behind the
            // first window, and each copy occurs once per window.
            let block = lines "block-" 0 182
            let tail = Array.append (lines "tail-" 0 600) (padding "pad line " 25_000)
            let unique = lines "shared-" 0 330
            let identical = Array.create 330 (plainLine "same")
            let edited = unique |> Array.mapi (fun index line -> if index % 30 = 29 then plainLine $"{line.Text}x{index}" else line)
            for name, previousShared, currentShared, replaced in [ "unique", unique, unique, 0; "identical", identical, identical, 0; "edited", unique, edited, 11 ] do
                let inserted = lines "inserted-" 0 450
                for copy = 0 to 44 do
                    inserted[3 * copy] <- block[4 * copy]
                let previous = Array.concat [ previousShared; block; tail ]
                let current = Array.concat [ [| plainLine "head" |]; currentShared; inserted; block; tail ]
                let! shape = run (defaultConfig ("padded-copied-block-" + name)) previous current
                checkExact name shape (330 - replaced + 182 + 600 + 25_000) 451 0
        }
        "a short cycle of repeated rows with a sparse edit is aligned by its first window", fun () -> async {
            // Every line occurs many times in its window, and one line in sixty is replaced by the next row of the
            // cycle. The first window of 512 lines already aligns almost all of them, so a larger window only
            // repeats the same alignment. The scan request count bounds the work, since growing to the full size
            // exhausts the step budget of the single pass and restarts the search after each edit.
            let rows = [| plainLine "row A"; plainLine "row B"; plainLine "row C" |]
            let previous = Array.init 25_000 (fun index -> rows[index % 3])
            let current = previous |> Array.mapi (fun index line -> if index > 0 && index % 60 = 0 then rows[(index + 1) % 3] else line)
            let! shape = run { defaultConfig "padded-repeated-rows" with Limits = { Limits.defaults with MaxUnits = 256; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 } } previous current
            checkExact "repeated rows" shape 24_584 92 92
            Check.true' (shape.Scans < 8_000) $"The search finishes within 8,000 scan requests (used {shape.Scans})."
        }
        "a short cycle of repeated rows with a sparse edit in a file of 15,000 lines is aligned as git aligns it", fun () -> async {
            // The file is below 20,000 lines, so its window grows to hold it whole. The single pass over that
            // window exhausts its step budget. The session then aligns from the first window, as it does for a
            // larger file. The scan request count bounds the work.
            let rows = [| plainLine "row A"; plainLine "row B"; plainLine "row C" |]
            let previous = Array.init 15_000 (fun index -> rows[index % 3])
            let current = previous |> Array.mapi (fun index line -> if index > 0 && index % 60 = 0 then rows[(index + 1) % 3] else line)
            let! shape = run { defaultConfig "repeated-rows-15k" with Limits = { Limits.defaults with MaxUnits = 256; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 } } previous current
            checkExact "repeated rows in 15,000 lines" shape 14_751 56 56
            Check.true' (shape.Scans < 14_000) $"The search finishes within 14,000 scan requests (used {shape.Scans})."
        }
        "the last window of a large file keeps its alignment when its single pass runs out", fun () -> async {
            // The window that holds the end of both files did not grow for small files, so the session keeps its
            // alignment instead of aligning the window again from its first lines. The scan request count bounds
            // the work.
            let shared = lines "shared line " 0 30_000
            let previous = Array.append shared (lines "old tail " 0 3_000)
            let current = Array.append shared (lines "new tail " 0 3_000)
            let! shape = run { defaultConfig "large-file-tail-rewrite" with Limits = { Limits.defaults with MaxUnits = 256; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 } } previous current
            Check.equal 30_000 shape.Equal "Tail rewrite: the shared lines are equal rows."
            Check.true' (shape.Scans < 32_000) $"The tail rewrite finishes within 32,000 scan requests (used {shape.Scans})."
        }
        "an insertion before a run of identical rows in a window of repeated lines is one added row in a file above 20,000 lines", fun () -> async {
            // The window of 64 lines holds only identical rows and no line that is unique within it. It does not settle
            // on its own alignment, because that pairs the inserted row with the first identical row.
            let previous = Array.append (Array.create 100 (plainLine "rep 0")) (padding "tail-" 20_000)
            let current = Array.append [| plainLine "insert x" |] previous
            let! shape = run { defaultConfig "padded-identical-rows-insertion" with WindowMaxLines = 64 } previous current
            checkExact "insertion before identical rows" shape 20_100 1 0
        }
        "a deletion before a run of identical rows in a window of repeated lines is one removed row in a file above 20,000 lines", fun () -> async {
            let current = Array.append (Array.create 100 (plainLine "rep 0")) (padding "tail-" 20_000)
            let previous = Array.append [| plainLine "insert x" |] current
            let! shape = run { defaultConfig "padded-identical-rows-deletion" with WindowMaxLines = 64 } previous current
            checkExact "deletion before identical rows" shape 20_100 0 1
        }
        "an insertion before a run of identical rows grows the window before it settles in a file above 20,000 lines", fun () -> async {
            // The window of 512 lines holds only identical rows, so growing it to reach the distinct tail lines
            // decides the alignment.
            let previous = Array.append (Array.create 1_000 (plainLine "rep 0")) (padding "tail-" 21_000)
            let current = Array.append [| plainLine "insert x" |] previous
            let! shape = run (defaultConfig "padded-identical-rows-growth") previous current
            checkExact "insertion before 1,000 identical rows" shape 22_000 1 0
        }
    ]

    let cases = phaseCases @ settleCases @ parityCases @ paddedCases @ budgetCases @ scanLimitCases @ seekCases @ extensionCases @ byteOrderMarkCases @ giantLineCases @ ledgerCases
