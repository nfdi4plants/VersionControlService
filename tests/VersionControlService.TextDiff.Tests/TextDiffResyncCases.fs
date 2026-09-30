namespace VersionControlService.TextDiff.Tests

open System
open System.Text
open VersionControlService.Abstractions
open VersionControlService.TextDiff

/// Cases for edits that are larger than one alignment window.
module TextDiffResyncCases =
    type private SourceLine = { Text: string; Ending: LineEnding }

    type private TestScratchHolder() =
        let mutable holdsScratch = false
        let mutable busy = false
        let mutable mustKeepScratch = false

        member _.Set(holds, isBusy, mustKeep) =
            holdsScratch <- holds
            busy <- isBusy
            mustKeepScratch <- mustKeep

        interface IScratchHolder with
            member _.HoldsScratch = holdsScratch
            member _.IsBusy = busy
            member _.MustKeepScratch = mustKeepScratch
            member _.YieldScratch() = async {
                holdsScratch <- false
                mustKeepScratch <- false
            }
            member _.Spill() = async {
                holdsScratch <- false
                mustKeepScratch <- false
            }

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
        let state = Scanner.create TextEncoding.Utf8 0L None
        let found = ResizeArray<ScannedLine>()
        let batch = LineBatch(256)
        let meter = Meter.create (ManualClock 0.0 :> IClock) { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1e15; QuantumMs = 1e15 }
        let onLines (lines: LineBatch) =
            for index = 0 to lines.Count - 1 do found.Add(lines.Line index)
        Scanner.scanChunk state bytes 0 bytes.Length true meter batch onLines ignore |> ignore
        found.ToArray()

    let private seekCases: (string * (unit -> Async<unit>)) list = [
        "seeking a line or an offset matches a full scan", fun () -> async {
            let previous = lines "line-" 0 500
            let current = Array.concat [ previous[.. 99]; lines "new-" 0 300; previous[100 .. 399]; lines "other-" 0 50; previous[450 ..] ]
            let host = Host.createInMemory (ManualClock 0.0 :> IClock)
            let! session = TextDiffSession.create host (Ledger()) (smallConfig "seek") (fun _ -> 1) (spec (encode previous)) (spec (encode current))
            let! _ = readAll session noHook
            for side, source in [ DiffSide.Previous, encode previous; DiffSide.Current, encode current ] do
                let expected = scanAll source
                for number in [ 0; 1; 63; 64; 65; 99; 100; 250; 399; 400; expected.Length - 1 ] do
                    let! found = session.SeekLine(side, int64 number)
                    Check.equal (EngineResult.Ok(Some expected[number])) found "A line seek matches the full scan."
                for number in [ 0; 7; 1_000; 2_047; 2_048; 4_096; source.Length - 1 ] do
                    let! found = session.SeekOffset(side, int64 number)
                    let wanted = expected |> Array.find (fun line -> int64 number < line.EndOffset)
                    Check.equal (EngineResult.Ok(Some wanted)) found "An offset seek matches the full scan."
                let! past = session.SeekLine(side, int64 expected.Length)
                Check.equal (EngineResult.Ok None) past "A line past the end has no result."
            do! session.Close()
        }
    ]
    let private tightLimits = { Limits.defaults with MaxUnits = 256; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }

    /// Asks for a spill after every few requests and performs it at the first request boundary that allows one.
    /// A session in the middle of an alignment step or a restore keeps its memory, so the hook waits for it.
    let private spillEvery (every: int) (spills: int ref) =
        let seen = ref 0
        let wanted = ref false
        fun (session: TextDiffSession) -> async {
            seen.Value <- seen.Value + 1
            if seen.Value % every = 0 then wanted.Value <- true
            let holder = session :> IScratchHolder
            if wanted.Value && holder.HoldsScratch && not holder.MustKeepScratch then
                wanted.Value <- false
                let! result = session.Spill()
                match result with
                | EngineResult.Ok() -> spills.Value <- spills.Value + 1
                | _ -> failwith "The spill failed."
        }

    let private checkSameShape (expected: Shape) (actual: Shape) =
        Check.equal expected.Added actual.Added "The spilled run has the same added rows."
        Check.equal expected.Removed actual.Removed "The spilled run has the same removed rows."
        Check.equal expected.UnalignedPrevious actual.UnalignedPrevious "The spilled run has the same unaligned previous lines."
        Check.equal expected.UnalignedCurrent actual.UnalignedCurrent "The spilled run has the same unaligned current lines."

    let private spillMatchesUninterrupted name (every: int) (previous: SourceLine array) (current: SourceLine array) (adjust: SessionConfig -> SessionConfig) = async {
        let! plain = run (adjust (smallConfig (name + "-plain"))) previous current
        let spills = ref 0
        let sessionConfig = { adjust (smallConfig (name + "-spilled")) with Limits = tightLimits }
        let! spilled = runWith sessionConfig (Ledger()) (spillEvery every spills) previous current
        Check.true' (spills.Value > 0) "The run spilled at least once."
        checkSameShape plain spilled
        return spills.Value
    }


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

    let private tinyLimits = { tightLimits with MaxUnits = 32 }

    /// A session on a shared worker whose request budget is a few units, so one alignment step spans many requests.
    let private openOnWorker (coordinator: WorkerScratch) host sessionId previous current = async {
        let sessionConfig = { smallConfig sessionId with Limits = tinyLimits }
        return! TextDiffSession.createWithScratch coordinator host (Ledger()) sessionConfig (fun _ -> 1) (spec (encode previous)) (spec (encode current))
    }

    type private Runner = {
        Session: TextDiffSession
        Pages: ResizeArray<DiffPage>
        mutable Result: EngineResult<Resumable<DiffPage>>
        mutable Finished: bool
    }

    let private newRunner session result = { Session = session; Pages = ResizeArray<DiffPage>(); Result = result; Finished = false }

    /// Consumes the runner's current result and issues the request that follows it.
    let private advanceRunner (runner: Runner) = async {
        match runner.Result with
        | EngineResult.Ok(Resumable.Ready page) ->
            runner.Pages.Add page
            match page.NextCursor with
            | None -> runner.Finished <- true
            | Some cursor ->
                let! next = runner.Session.ReadPage cursor (fun () -> false)
                runner.Result <- next
        | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
            let! next = runner.Session.ReadPage continuation (fun () -> false)
            runner.Result <- next
        | EngineResult.Failed(code, message, detail) -> failwith $"The session failed with {code}: {message}. Detail: {detail}."
        | EngineResult.Canceled -> failwith "The session canceled an uncanceled request."
    }

    /// Every aligned row of the pages in order. Page boundaries do not change this sequence.
    let private flatRows (pages: DiffPage array) =
        pages
        |> Array.collect (fun page ->
            page.Parts
            |> Array.collect (fun part ->
                match part with
                | DiffPart.Hunk { Body = HunkBody.AlignedRows values } ->
                    values
                    |> Array.map (fun row ->
                        row.Kind, row.Previous |> Option.map (fun line -> line.Slice.Text), row.Current |> Option.map (fun line -> line.Slice.Text))
                | _ -> [||]))

    /// Checks that a session produced the same diff as the uninterrupted run. Requests can split pages at
    /// other places, so the comparison covers rows and covered lines and leaves the page boundaries out.
    let private checkSameOutput (plain: Shape) (runner: Runner) (previous: SourceLine array) (current: SourceLine array) =
        let pages = runner.Pages.ToArray()
        let parts = pages |> Array.collect (fun page -> page.Parts)
        Check.sequence previous (rebuild previous parts true) "The pages cover every previous line exactly once."
        Check.sequence current (rebuild current parts false) "The pages cover every current line exactly once."
        checkSameShape plain (shapeOf pages 0)
        Check.sequence (flatRows plain.Pages) (flatRows pages) "The rows equal the uninterrupted run."

    /// Serves the runners one request at a time in turn until every one has read its last page.
    let private alternate (runners: Runner array) = async {
        let mutable requests = 0
        while runners |> Array.exists (fun runner -> not runner.Finished) do
            for runner in runners do
                if not runner.Finished then
                    requests <- requests + 1
                    if requests > 400_000 then failwith "The sessions did not finish."
                    do! advanceRunner runner
    }

    let private spillCases: (string * (unit -> Async<unit>)) list = [
        "spills between requests around window alignment keep the output", fun () -> async {
            let previous = lines "line-" 0 600
            let current = Array.copy previous
            for index in [ 20; 300; 480 ] do
                current[index] <- { current[index] with Text = "edited-" + string index }
            let! _ = spillMatchesUninterrupted "spill-window" 1 previous current id
            return ()
        }
        "spills between requests during the forward search keep the output", fun () -> async {
            let previous = lines "line-" 0 800
            let current = Array.concat [ previous[.. 99]; lines "new-" 0 300; previous[100 .. 399]; lines "other-" 0 200; previous[600 ..] ]
            let! _ = spillMatchesUninterrupted "spill-search" 2 previous current id
            return ()
        }
        "a spill in the middle of a rewrite keeps the output", fun () -> async {
            let previous = lines "old-" 0 700
            let current = lines "new-" 0 650
            let! _ = spillMatchesUninterrupted "spill-rewrite" 3 previous current id
            return ()
        }
        "another session spills an idle session that holds scratch memory", fun () -> async {
            let coordinator = WorkerScratch()
            let ledger = Ledger()
            let host = Host.createInMemory (ManualClock 0.0 :> IClock)
            let previous = lines "line-" 0 600
            let current = Array.concat [ previous[.. 99]; lines "new-" 0 300; previous[100 ..] ]
            let openSession id = async {
                let sessionConfig = { smallConfig id with Limits = tightLimits }
                return! TextDiffSession.createWithScratch coordinator host ledger sessionConfig (fun _ -> 1) (spec (encode previous)) (spec (encode current))
            }
            let! first = openSession "shared-first"
            let! second = openSession "shared-second"
            let idle = first :> IScratchHolder
            let! started = first.FirstPage(fun () -> false)
            let runner = newRunner first started
            let mutable requests = 0
            while not (idle.HoldsScratch && not idle.MustKeepScratch) && not runner.Finished do
                requests <- requests + 1
                if requests > 200_000 then failwith "The first session never became idle while holding scratch memory."
                do! advanceRunner runner
            Check.true' (idle.HoldsScratch && not idle.MustKeepScratch) "The suspended session is idle and holds scratch memory."
            let! _ = second.FirstPage(fun () -> false)
            Check.true' (not (first :> IScratchHolder).HoldsScratch) "The second request spilled the first session."
            let! shapeSecond = readAll second noHook
            let! shapeFirst = readAll first noHook
            checkSameShape shapeFirst shapeSecond
            do! first.Close()
            do! second.Close()
            Check.equal 0 coordinator.Count "Closed sessions leave the coordinator."
        }
        "four sessions alternating on one worker all finish with their uninterrupted output", fun () -> async {
            let previous = lines "line-" 0 300
            let current = Array.concat [ previous[.. 39]; lines "new-" 0 100; previous[40 .. 149]; lines "other-" 0 60; previous[220 ..] ]
            let! plain = run { smallConfig "alternate-plain" with Limits = tinyLimits } previous current
            let coordinator = WorkerScratch()
            let host = Host.createInMemory (ManualClock 0.0 :> IClock)
            let! first = openOnWorker coordinator host "alternate-first" previous current
            let! second = openOnWorker coordinator host "alternate-second" previous current
            let! third = openOnWorker coordinator host "alternate-third" previous current
            let! fourth = openOnWorker coordinator host "alternate-fourth" previous current
            let! firstStart = first.FirstPage(fun () -> false)
            let! secondStart = second.FirstPage(fun () -> false)
            let! thirdStart = third.FirstPage(fun () -> false)
            let! fourthStart = fourth.FirstPage(fun () -> false)
            let runners = [| newRunner first firstStart; newRunner second secondStart; newRunner third thirdStart; newRunner fourth fourthStart |]
            do! alternate runners
            for runner in runners do
                checkSameOutput plain runner previous current
                do! runner.Session.Close()
            Check.equal 0 coordinator.Count "Closed sessions leave the coordinator."
        }
        "a stopped alignment yields to a tiny diff and resumes from committed lines", fun () -> async {
            // Scattered edits queue finished hunks while the next window still aligns, so a page can end mid step.
            let random = Random(3)
            let previous = Array.init 800 (fun index -> { Text = $"b {index} {random.Next 1000}"; Ending = LineEnding.LF })
            let current = previous |> Array.map (fun line -> if random.Next 4 = 0 then { line with Text = line.Text + " changed" } else line)
            let smallPrevious = [| { Text = "one"; Ending = LineEnding.LF }; { Text = "two"; Ending = LineEnding.LF }; { Text = "three"; Ending = LineEnding.LF } |]
            let smallCurrent = [| { Text = "one"; Ending = LineEnding.LF }; { Text = "TWO"; Ending = LineEnding.LF }; { Text = "three"; Ending = LineEnding.LF } |]
            let! plainLarge = run { smallConfig "yield-plain-large" with Limits = tinyLimits } previous current
            let! plainSmall = run { smallConfig "yield-plain-small" with Limits = tinyLimits } smallPrevious smallCurrent
            let coordinator = WorkerScratch()
            let clock = ManualClock 0.0
            let host = Host.createInMemory (clock :> IClock)
            let! first = openOnWorker coordinator host "yield-large" previous current
            let! second = openOnWorker coordinator host "yield-small" smallPrevious smallCurrent
            let holder = first :> IScratchHolder
            let! firstStart = first.FirstPage(fun () -> false)
            let firstRunner = newRunner first firstStart
            let mutable requests = 0
            let hasReadyPage = function
                | EngineResult.Ok(Resumable.Ready _) -> true
                | _ -> false
            while not (holder.MustKeepScratch && hasReadyPage firstRunner.Result) && not firstRunner.Finished do
                requests <- requests + 1
                if requests > 200_000 then failwith "The first session never entered an alignment step."
                do! advanceRunner firstRunner
            Check.true' (holder.MustKeepScratch && hasReadyPage firstRunner.Result) "The viewer stops after a ready page while alignment is incomplete."
            clock.Advance 2_000.0
            let! secondStart = second.FirstPage(fun () -> false)
            let secondRunner = newRunner second secondStart
            let mutable secondRequests = 1
            while not secondRunner.Finished && secondRequests < 64 do
                do! advanceRunner secondRunner
                secondRequests <- secondRequests + 1
            Check.true' secondRunner.Finished "The small diff reaches its end within a bounded number of requests."
            Check.true' (not holder.MustKeepScratch) "The unfinished alignment released its scratch."
            checkSameOutput plainSmall secondRunner smallPrevious smallCurrent
            let mutable firstRequests = 0
            while not firstRunner.Finished && firstRequests < 200_000 do
                do! advanceRunner firstRunner
                firstRequests <- firstRequests + 1
            Check.true' firstRunner.Finished "The original diff resumes and reaches its end."
            checkSameOutput plainLarge firstRunner previous current
            do! first.Close()
            do! second.Close()
            Check.equal 0 coordinator.Count "Closed sessions leave the coordinator."
        }
        "a requester that stops waiting does not keep its place", fun () -> async {
            let clock = ManualClock 0.0
            let coordinator = WorkerScratch()
            let holder = TestScratchHolder()
            let stopped = TestScratchHolder()
            let next = TestScratchHolder()
            holder.Set(true, true, true)
            coordinator.Register(holder :> IScratchHolder, clock :> IClock)
            coordinator.Register(stopped :> IScratchHolder, clock :> IClock)
            coordinator.Register(next :> IScratchHolder, clock :> IClock)
            let! firstAttempt = coordinator.BeginRequest(stopped :> IScratchHolder)
            Check.equal false firstAttempt "The requester waits while a busy session keeps scratch."
            holder.Set(true, false, false)
            let! nextAttempt = coordinator.BeginRequest(next :> IScratchHolder)
            Check.true' nextAttempt "A requester that stopped asking does not hold up the next session."
            coordinator.Unregister(holder :> IScratchHolder)
            coordinator.Unregister(stopped :> IScratchHolder)
            coordinator.Unregister(next :> IScratchHolder)
            Check.equal 0 coordinator.Count "Unregistered holders leave the coordinator."
        }
        "an invalidated session releases worker scratch for the next session", fun () -> async {
            let coordinator = WorkerScratch()
            let ledger = Ledger()
            let host = Host.createInMemory (ManualClock 0.0 :> IClock)
            let invalid = Encoding.UTF8.GetBytes("first\nbad\u0000line\n")
            let previous = [| { Text = "one"; Ending = LineEnding.LF }; { Text = "two"; Ending = LineEnding.LF } |]
            let current = Array.copy previous
            current[1] <- { Text = "TWO"; Ending = LineEnding.LF }
            let create id previousSpec currentSpec =
                TextDiffSession.createWithScratch coordinator host ledger { smallConfig id with Limits = tinyLimits } (fun _ -> 1) previousSpec currentSpec
            let! invalidSession = create "invalid-scratch" (spec (Encoding.UTF8.GetBytes "first\nbad\n")) (spec invalid)
            let! invalidResult = invalidSession.FirstPage(fun () -> false)
            match invalidResult with
            | EngineResult.Failed("diff_content_not_text", _, _) -> ()
            | other -> failwith $"The invalid source returned {other}."
            let invalidHolder = invalidSession :> IScratchHolder
            Check.true' invalidHolder.HoldsScratch "The invalidated session still holds worker scratch before another request."
            let! expected = run { smallConfig "next-after-invalid-plain" with Limits = tinyLimits } previous current
            let! nextSession = create "next-after-invalid" (spec (encode previous)) (spec (encode current))
            let! actual = readAll nextSession noHook
            checkSameShape expected actual
            let parts = actual.Pages |> Array.collect (fun page -> page.Parts)
            Check.sequence previous (rebuild previous parts true) "The next session emits every previous line."
            Check.sequence current (rebuild current parts false) "The next session emits every current line."
            Check.true' (not invalidHolder.HoldsScratch) "The next request releases scratch held by the invalidated session."
            do! invalidSession.Close()
            do! nextSession.Close()
            Check.equal 0 coordinator.Count "Closed sessions leave the coordinator."
        }
        "a session that finishes in the request that completed its restore does not keep scratch", fun () -> async {
            let previous = lines "line-" 0 300
            let current = Array.copy previous
            for index in 20 .. 25 do
                current[index] <- { current[index] with Text = "edited-" + string index }
            let host = Host.createInMemory (ManualClock 0.0 :> IClock)
            let spills = ref 0
            let! session = TextDiffSession.create host (Ledger()) { smallConfig "restore-finish" with Limits = tightLimits } (fun _ -> 1) (spec (encode previous)) (spec (encode current))
            let! _ = readAll session (spillEvery 1 spills)
            Check.true' (spills.Value > 0) "The run spilled at least once."
            Check.true' (not (session :> IScratchHolder).MustKeepScratch) "A finished session releases its claim on scratch memory."
            do! session.Close()
        }
        "a yield with a byte order mark on one side reloads from after the mark", fun () -> async {
            let random = Random(5)
            let previous = Array.init 400 (fun index -> { Text = $"line {index} {random.Next 100_000}"; Ending = LineEnding.LF })
            let current = previous |> Array.mapi (fun index line -> if index % 3 = 0 then { line with Text = line.Text + " x" } else line)
            let smallPrevious = [| { Text = "one"; Ending = LineEnding.LF }; { Text = "two"; Ending = LineEnding.LF } |]
            let smallCurrent = [| { Text = "one"; Ending = LineEnding.LF }; { Text = "TWO"; Ending = LineEnding.LF } |]
            let coordinator = WorkerScratch()
            let clock = ManualClock 0.0
            let host = Host.createInMemory (clock :> IClock)
            let! first =
                TextDiffSession.createWithScratch coordinator host (Ledger()) { smallConfig "bom-yield-large" with Limits = tinyLimits } (fun _ -> 1) (bomSpec (encode previous)) (spec (encode current))
            let! second = openOnWorker coordinator host "bom-yield-small" smallPrevious smallCurrent
            let holder = first :> IScratchHolder
            let! firstStart = first.FirstPage(fun () -> false)
            let firstRunner = newRunner first firstStart
            let mutable requests = 0
            while not holder.MustKeepScratch && not firstRunner.Finished do
                requests <- requests + 1
                if requests > 200_000 then failwith "The first session never entered an alignment step."
                do! advanceRunner firstRunner
            Check.true' holder.MustKeepScratch "The first session stops inside an alignment step."
            clock.Advance 2_000.0
            let! secondStart = second.FirstPage(fun () -> false)
            Check.true' (not holder.MustKeepScratch) "The idle session yields to the other request."
            let secondRunner = newRunner second secondStart
            while not secondRunner.Finished do do! advanceRunner secondRunner
            while not firstRunner.Finished do do! advanceRunner firstRunner
            let parts = firstRunner.Pages.ToArray() |> Array.collect (fun page -> page.Parts)
            Check.sequence previous (rebuild previous parts true) "The yielded session covers every previous line without the mark."
            Check.sequence current (rebuild current parts false) "The yielded session covers every current line."
            do! first.Close()
            do! second.Close()
        }
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

    let cases = phaseCases @ budgetCases @ scanLimitCases @ seekCases @ extensionCases @ spillCases @ giantLineCases
