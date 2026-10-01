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
    }

    let private shapeOf (pages: DiffPage array) scans =
        let mutable unalignedPrevious = 0
        let mutable unalignedCurrent = 0
        let mutable added = 0
        let mutable removed = 0
        for page in pages do
            for part in page.Parts do
                match part with
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
                            | _ -> ()
                | _ -> ()
        { Pages = pages; Scans = scans; UnalignedPrevious = unalignedPrevious; UnalignedCurrent = unalignedCurrent; Added = added; Removed = removed }

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

    let cases = phaseCases @ budgetCases @ scanLimitCases @ seekCases @ extensionCases @ byteOrderMarkCases @ giantLineCases
