namespace VersionControlService.TextDiff.Tests

open System
open System.Text
open VersionControlService.Abstractions
open VersionControlService.TextDiff

module TextDiffSessionCases =
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

    let private absentSpec = {
        Source = None
        Encoding = "utf-8"
        BomLength = 0
        ByteLength = 0L
    }

    let mutable private sessionNumber = 0

    let private config contextLines pageRows chunkBytes windowLines limits =
        sessionNumber <- sessionNumber + 1
        {
            SessionId = $"textdiff-test-{sessionNumber}"
            ContextLines = contextLines
            PageMaxRows = pageRows
            PageMaxBytes = 512 * 1024
            PageMaxFragments = 32
            WindowMaxLines = windowLines
            WindowMaxBytes = 32 * 1024 * 1024
            CommonChunkBytes = chunkBytes
            MyersStepsPerGap = 100_000
            HashMaskForTesting = None
            Limits = limits
        }

    let private defaultConfig () = config 3 1_000 1_024 4_096 Limits.defaults

    let private openSession sessionConfig previous current : Async<TextDiffSession> = async {
        let clock = ManualClock 0.0
        let host = Host.createInMemory (clock :> IClock)
        return! TextDiffSession.create host (Ledger()) sessionConfig (fun _ -> 1) previous current
    }

    let private unwrap = function
        | EngineResult.Ok value -> value
        | EngineResult.Failed(code, message, detail) ->
            failwith $"The session failed with {code}: {message}. Detail: {detail}."
        | EngineResult.Canceled -> failwith "The session canceled an uncanceled request."

    let private rows (parts: DiffPart array) =
        parts
        |> Array.collect (function
            | DiffPart.Hunk fragment ->
                match fragment.Body with
                | HunkBody.AlignedRows values -> values
                | HunkBody.UnalignedSides _ -> [||]
            | _ -> [||])

    let private countRows (part: DiffPart) =
        match part with
        | DiffPart.Hunk fragment ->
            match fragment.Body with
            | HunkBody.AlignedRows values -> values.Length
            | HunkBody.UnalignedSides(previous, current) -> previous.Length + current.Length
        | _ -> 0

    let rec private resolvePage (session: TextDiffSession) cancel (result: EngineResult<Resumable<DiffPage>>) = async {
        match result with
        | EngineResult.Ok(Resumable.Ready page) -> return page
        | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
            let! next = session.ReadPage continuation cancel
            return! resolvePage session cancel next
        | EngineResult.Failed(code, message, detail) ->
            return failwith $"The session failed with {code}: {message}. Detail: {detail}."
        | EngineResult.Canceled -> return failwith "The session canceled an uncanceled request."
    }

    let private readAll (session: TextDiffSession) cancel = async {
        let! first = session.FirstPage cancel
        let! initial = resolvePage session cancel first
        let pages = ResizeArray<DiffPage>()
        let mutable current = Some initial

        while current.IsSome do
            let page = current.Value
            pages.Add page

            match page.NextCursor with
            | None -> current <- None
            | Some cursor ->
                let! next = session.ReadPage cursor cancel
                let! nextPage = resolvePage session cancel next
                current <- Some nextPage

        if pages.Count = 0 || not pages[pages.Count - 1].OutputComplete then
            failwith "The session ended without a complete final page."

        return pages.ToArray()
    }

    let private allParts pages = pages |> Array.collect (fun page -> page.Parts)

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

    let private collectPages (session: TextDiffSession) cancel = async {
        let! pages = readAll session cancel
        return pages, allParts pages
    }

    let private checkOracle previous current pages =
        let parts = allParts pages
        let rebuiltPrevious = rebuild previous parts true
        let rebuiltCurrent = rebuild current parts false
        Check.sequence previous rebuiltPrevious "The page stream covers every previous line exactly once."
        Check.sequence current rebuiltCurrent "The page stream covers every current line exactly once."

    let private checkNotImplemented = function
        | EngineResult.Failed("not_implemented", _, None) -> ()
        | other -> failwith $"An extension point returned {other}."

    type private RandomState(seed: uint32) =
        let mutable value = if seed = 0u then 1u else seed
        member _.Next(bound: int) =
            value <- value ^^^ (value <<< 13)
            value <- value ^^^ (value >>> 17)
            value <- value ^^^ (value <<< 5)
            int (value % uint32 bound)

    let private nextEnding (random: RandomState) allowNoEnding =
        let value = random.Next 100
        if allowNoEnding && value >= 96 then LineEnding.NoEnding
        elif value < 75 then LineEnding.LF
        elif value < 90 then LineEnding.CRLF
        else LineEnding.CR

    let private makeText (random: RandomState) caseIndex lineIndex =
        match random.Next 80 with
        | 0 -> "shared-prefix-repeat-" + string (lineIndex % 5)
        | 1 -> "shared-prefix-long-" + String('x', 64 + random.Next 7_800)
        | 2 -> "unicode-prefix-€-😀-" + string lineIndex
        | _ -> "shared-prefix-" + string caseIndex + "-" + string lineIndex

    let private makeSourceLines (random: RandomState) caseIndex =
        let count = random.Next 2_501
        Array.init count (fun index -> {
            Text = makeText random caseIndex index
            Ending = nextEnding random (index = count - 1)
        })

    let private mutateLines (random: RandomState) caseIndex (previous: SourceLine array) =
        let current = ResizeArray<SourceLine>(previous)
        let editCount = 1 + random.Next 7

        for edit in 0 .. editCount - 1 do
            match random.Next 4 with
            | 0 when current.Count > 0 ->
                let start = random.Next current.Count
                let amount = min (1 + random.Next 12) (current.Count - start)
                for _ in 1 .. amount do current.RemoveAt start
            | 1 ->
                let start = random.Next(current.Count + 1)
                let amount = 1 + random.Next 12
                if start = current.Count && current.Count > 0 && current[current.Count - 1].Ending = LineEnding.NoEnding then
                    current[current.Count - 1] <- { current[current.Count - 1] with Ending = LineEnding.LF }
                for offset in 0 .. amount - 1 do
                    let line = {
                        Text = makeText random caseIndex (100_000 + edit * 100 + offset)
                        Ending = nextEnding random false
                    }
                    current.Insert(start + offset, line)
            | 2 when current.Count > 0 ->
                let index = random.Next current.Count
                current[index] <- { current[index] with Text = makeText random caseIndex (200_000 + edit) }
            | _ when current.Count > 0 ->
                let index = random.Next current.Count
                current[index] <- { current[index] with Ending = nextEnding random (index = current.Count - 1) }
            | _ ->
                current.Add { Text = makeText random caseIndex (300_000 + edit); Ending = LineEnding.LF }

        for index = 0 to current.Count - 2 do
            if current[index].Ending = LineEnding.NoEnding then
                current[index] <- { current[index] with Ending = LineEnding.LF }

        current.ToArray()

    let cases: (string * (unit -> Async<unit>)) list = [
        "deferred session operations return a named failure", fun () -> async {
            let source = sourceSpec (Encoding.UTF8.GetBytes "line\n")
            let! session = openSession (defaultConfig ()) source source
            let! expanded = session.Expand "gap-id"
            let! line = session.ReadLine(DiffSide.Previous, 0L, 0L, 64)
            let preview = session.PendingPreview()
            let resync = session.Resync()
            let! spill = session.Spill()
            let! restore = session.Restore()
            checkNotImplemented expanded
            checkNotImplemented line
            checkNotImplemented preview
            checkNotImplemented resync
            checkNotImplemented spill
            checkNotImplemented restore
            do! session.Close()
            return ()
        }
        "identical inputs return one complete hidden gap", fun () -> async {
            let expected = [| { Text = "first"; Ending = LineEnding.CRLF }; { Text = "last"; Ending = LineEnding.NoEnding } |]
            let source = sourceSpec (encodeLines expected)
            let! session = openSession (defaultConfig ()) source source
            let! pages, parts = collectPages session (fun () -> false)
            Check.equal 1 parts.Length "Identical files have one hidden equal part."
            match parts[0] with
            | DiffPart.HiddenEqual gap ->
                Check.equal 2L gap.PreviousRange.Count "The hidden range includes both lines."
                Check.equal gap.PreviousRange.Count gap.CurrentRange.Count "Both source ranges have equal length."
            | _ -> failwith "Identical files returned a hunk."
            Check.true' pages[pages.Length - 1].OutputComplete "The final page completes output."
            Check.true' pages[pages.Length - 1].Progress.ScanComplete "The final page completes validation."
            do! session.Close()
            return ()
        }
        "empty inputs return no parts", fun () -> async {
            let source = sourceSpec Array.empty
            let! session = openSession (defaultConfig ()) source source
            let! pages, parts = collectPages session (fun () -> false)
            Check.equal 0 parts.Length "Empty files have no visible ranges."
            Check.true' pages[0].OutputComplete "The empty result is complete."
            Check.true' pages[0].Progress.ScanComplete "Both empty sources are complete."
            do! session.Close()
            return ()
        }
        "line edits and ending-only changes produce visible rows", fun () -> async {
            let previous = [| { Text = "top"; Ending = LineEnding.LF }; { Text = "old"; Ending = LineEnding.CRLF }; { Text = "bottom"; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = "top"; Ending = LineEnding.LF }; { Text = "new"; Ending = LineEnding.CRLF }; { Text = "bottom"; Ending = LineEnding.NoEnding } |]
            let! session = openSession (defaultConfig ()) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! _, parts = collectPages session (fun () -> false)
            let changed = rows parts
            Check.true' (changed |> Array.exists (fun row -> row.Kind <> DiffRowKind.Context)) "A changed line appears in the hunk."
            do! session.Close()

            let oldEnding = [| { Text = "first"; Ending = LineEnding.CRLF }; { Text = "last"; Ending = LineEnding.LF } |]
            let newEnding = [| { Text = "first"; Ending = LineEnding.LF }; { Text = "last"; Ending = LineEnding.LF } |]
            let! endingSession = openSession (defaultConfig ()) (sourceSpec (encodeLines oldEnding)) (sourceSpec (encodeLines newEnding))
            let! _, endingParts = collectPages endingSession (fun () -> false)
            Check.true' (rows endingParts |> Array.exists (fun row -> row.Kind = DiffRowKind.EndingChanged)) "A line ending difference appears as an ending change."
            do! endingSession.Close()
            return ()
        }
        "absent sides produce added or removed lines", fun () -> async {
            let content = [| { Text = "one"; Ending = LineEnding.LF }; { Text = "two"; Ending = LineEnding.NoEnding } |]
            let! addedSession = openSession (defaultConfig ()) absentSpec (sourceSpec (encodeLines content))
            let! _, addedParts = collectPages addedSession (fun () -> false)
            Check.true' (rows addedParts |> Array.exists (fun row -> row.Kind = DiffRowKind.Added)) "The absent previous side produces additions."
            do! addedSession.Close()

            let! removedSession = openSession (defaultConfig ()) (sourceSpec (encodeLines content)) absentSpec
            let! _, removedParts = collectPages removedSession (fun () -> false)
            Check.true' (rows removedParts |> Array.exists (fun row -> row.Kind = DiffRowKind.Removed)) "The absent current side produces removals."
            do! removedSession.Close()
            return ()
        }
        "small pages split a hunk and replay stable results", fun () -> async {
            let previous = Array.init 24 (fun index -> { Text = "line-" + string index; Ending = LineEnding.LF })
            let current = Array.init 24 (fun index -> {
                Text = if index >= 5 && index <= 17 then "changed-" + string index else "line-" + string index
                Ending = LineEnding.LF
            })
            let limits = { Limits.defaults with MaxUnits = 20_000; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let sessionConfig = config 1 3 32 32 limits
            let! session = openSession sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! firstResult = session.FirstPage(fun () -> false)
            let first = unwrap firstResult
            let firstPage =
                match first with
                | Resumable.Ready page -> page
                | Resumable.Scanning _ -> failwith "The small page request returned no finalized row."
            Check.true' firstPage.NextCursor.IsSome "The split hunk has a continuation."
            let cursor = firstPage.NextCursor.Value
            let! recorded = session.ReadPage cursor (fun () -> false)
            let! retry = session.ReadPage cursor (fun () -> false)
            Check.true' (Unchecked.equals recorded retry) "Retrying a cursor returns its recorded result."

            let! replay = session.ReplayPage firstPage.PageId
            let replayed = unwrap replay
            Check.equal firstPage.PageId replayed.PageId "Replay preserves the original page id."
            Check.true' (Unchecked.equals firstPage.Parts replayed.Parts) "Replay preserves the original page parts."
            Check.equal None replayed.Pending "Replay omits pending preview data."

            let! allPages = readAll session (fun () -> false)
            Check.true' (allPages.Length > 1) "The hunk spans multiple pages."
            for page in allPages do
                let used = page.Parts |> Array.sumBy countRows
                Check.true' (used <= sessionConfig.PageMaxRows) "Every page stays within its row limit."
            let fragments =
                allPages
                |> Array.collect (fun page -> page.Parts)
                |> Array.choose (function DiffPart.Hunk fragment -> Some fragment | _ -> None)
            Check.true' (fragments.Length > 1) "The large hunk has multiple fragments."
            Check.true' fragments[0].StartsHunk "The first fragment starts the hunk."
            Check.true' fragments[fragments.Length - 1].EndsHunk "The last fragment ends the hunk."
            Check.true' (fragments |> Array.forall (fun fragment -> fragment.HunkId = fragments[0].HunkId)) "Every fragment keeps the same hunk identifier."
            do! session.Close()
            return ()
        }
        "requests without a finalized row return a resumable scan", fun () -> async {
            let expected = Array.init 4_096 (fun index -> { Text = "line-" + string index; Ending = LineEnding.LF })
            let source = sourceSpec (encodeLines expected)
            let limits = { Limits.defaults with MaxUnits = 1; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 3 10 64 128 limits) source source
            let! initial = session.FirstPage(fun () -> false)
            let continuation =
                match initial with
                | EngineResult.Ok(Resumable.Scanning(_, value, None)) -> value
                | other -> failwith $"The bounded request returned {other}."
            let! recorded = session.ReadPage continuation (fun () -> false)
            let! retry = session.ReadPage continuation (fun () -> false)
            Check.true' (Unchecked.equals recorded retry) "Retrying a scan cursor returns the same recorded result."
            let mutable currentResult = recorded
            let mutable continuationCount = 1
            let mutable completedPage: DiffPage option = None
            let mutable failure: string option = None
            while completedPage.IsNone && failure.IsNone do
                match currentResult with
                | EngineResult.Ok(Resumable.Ready page) -> completedPage <- Some page
                | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                    continuationCount <- continuationCount + 1
                    let! next = session.ReadPage continuation (fun () -> false)
                    match next with
                    | EngineResult.Failed(code, message, _) -> failure <- Some $"Continuation {continuationCount} ({continuation}) failed with {code}: {message}."
                    | EngineResult.Canceled -> failure <- Some $"Continuation {continuationCount} ({continuation}) canceled."
                    | _ -> currentResult <- next
                | EngineResult.Failed(code, message, _) -> failure <- Some $"The bounded request failed with {code}: {message}."
                | EngineResult.Canceled -> failure <- Some "The bounded request was canceled."
            match failure, completedPage with
            | Some message, _ -> failwith message
            | None, Some page ->
                Check.true' page.OutputComplete "The continuation reaches a complete page."
                Check.equal 1 page.Parts.Length "The completed identical scan has one visible range."
                Check.true' page.Progress.ScanComplete "The continuation validates both complete sources."
            | _ -> failwith "The bounded request did not produce a result."
            do! session.Close()
            return ()
        }
        "deadline returns a partial page after finalized rows", fun () -> async {
            let previous = Array.init 80 (fun index -> { Text = (if index = 0 then "old" else "same-" + string index); Ending = LineEnding.LF })
            let current = Array.init 80 (fun index -> { Text = (if index = 0 then "new" else "same-" + string index); Ending = LineEnding.LF })
            let previousBytes = encodeLines previous
            let currentBytes = encodeLines current
            let previousSource = MemoryByteSource(previousBytes, growing = true)
            let currentSource = MemoryByteSource(currentBytes, growing = true)
            previousSource.AdvanceTo 64L
            currentSource.AdvanceTo 64L
            let clock = ManualClock 0.0
            let mutable yieldCount = 0
            let host = {
                Clock = clock :> IClock
                Yield = fun () -> async {
                    yieldCount <- yieldCount + 1
                    clock.Advance 20.0
                    previousSource.AdvanceBy 64L
                    currentSource.AdvanceBy 64L
                }
                CreateTempStore = fun _ -> async.Return(MemoryTempStore() :> ITempStore)
            }
            let limits = { Limits.defaults with RequestMs = 10.0; QuantumMs = 1_000.0 }
            let sessionConfig = config 100 1_000 64 1_000 limits
            let previousBase = sourceSpec previousBytes
            let currentBase = sourceSpec currentBytes
            let previousSpec = { previousBase with Source = Some(previousSource :> IByteSource) }
            let currentSpec = { currentBase with Source = Some(currentSource :> IByteSource) }
            let! session = TextDiffSession.create host (Ledger()) sessionConfig (fun _ -> 1) previousSpec currentSpec
            let! result = session.FirstPage(fun () -> false)
            let page =
                match unwrap result with
                | Resumable.Ready value -> value
                | Resumable.Scanning _ -> failwith "The deadline arrived after a changed row was available."
            Check.true' page.NextCursor.IsSome "The request leaves work for its next page."
            Check.true' (not page.OutputComplete) "The page does not complete either growing source."
            Check.true' (rows page.Parts |> Array.exists (fun row -> row.Kind = DiffRowKind.Replaced)) "The request returns its finalized changed row."
            Check.true' (not page.Progress.ScanComplete) "The progress keeps both sources open."
            Check.true' (yieldCount > 0) "The host yield advances the clock beyond the request deadline."
            do! session.Close()
            return ()
        }
        "canceled work can resume from the same session", fun () -> async {
            let previous = Array.init 160 (fun index -> { Text = "before-" + string index; Ending = LineEnding.LF })
            let current = Array.init 160 (fun index -> { Text = "after-" + string index; Ending = LineEnding.LF })
            let previousSpec = sourceSpec (encodeLines previous)
            let currentSpec = sourceSpec (encodeLines current)
            let sessionConfig = config 0 100 64 32 Limits.defaults
            let! session = openSession sessionConfig previousSpec currentSpec
            let mutable checks = 0
            let cancel () =
                checks <- checks + 1
                checks >= 12
            let! canceled = session.FirstPage cancel
            match canceled with
            | EngineResult.Canceled -> ()
            | _ -> failwith "A canceled request returned work."
            Check.true' (session.Progress.ValidatedBytes > 0L) "The canceled request keeps the bytes it already validated."
            let! pages = readAll session (fun () -> false)
            checkOracle previous current pages
            let! uninterrupted = openSession sessionConfig previousSpec currentSpec
            let! uninterruptedPages = readAll uninterrupted (fun () -> false)
            Check.true' (Unchecked.equals (allParts pages) (allParts uninterruptedPages)) "Retrying canceled work returns the same parts as an uninterrupted scan."
            do! session.Close()
            do! uninterrupted.Close()
            return ()
        }
        "growing sources can produce a page before reaching EOF", fun () -> async {
            let previous = Array.append [| { Text = "old"; Ending = LineEnding.LF } |] (Array.init 60 (fun index -> { Text = "line-" + string index; Ending = LineEnding.LF }))
            let current = Array.append [| { Text = "new"; Ending = LineEnding.LF } |] (Array.init 60 (fun index -> { Text = "line-" + string index; Ending = LineEnding.LF }))
            let previousBytes = encodeLines previous
            let currentBytes = encodeLines current
            let previousSource = MemoryByteSource(previousBytes, growing = true)
            let currentSource = MemoryByteSource(currentBytes, growing = true)
            let clock = ManualClock 0.0
            let host = {
                Clock = clock :> IClock
                Yield = fun () -> async {
                    previousSource.AdvanceBy 8L
                    currentSource.AdvanceBy 8L
                }
                CreateTempStore = fun _ -> async.Return(MemoryTempStore() :> ITempStore)
            }
            let previousBase = sourceSpec previousBytes
            let currentBase = sourceSpec currentBytes
            let previousSpec = { previousBase with Source = Some(previousSource :> IByteSource) }
            let currentSpec = { currentBase with Source = Some(currentSource :> IByteSource) }
            let! session = TextDiffSession.create host (Ledger()) (config 0 1 16 8 Limits.defaults) (fun _ -> 1) previousSpec currentSpec
            let! first = session.FirstPage(fun () -> false)
            let! page = resolvePage session (fun () -> false) first
            Check.true' (page.Parts.Length > 0) "The available prefix contains a visible edit."
            Check.true' (not ((previousSource :> IByteSource).IsComplete()) && not ((currentSource :> IByteSource).IsComplete())) "Both sources still have bytes beyond the returned page."
            do! session.Close()
            return ()
        }
        "binary evidence before the first page reports its source", fun () -> async {
            let bytes = Encoding.UTF8.GetBytes("text\u0000tail\n")
            let! session = openSession (defaultConfig ()) (sourceSpec bytes) (sourceSpec bytes)
            let! result = session.FirstPage(fun () -> false)
            match result with
            | EngineResult.Failed(code, _, Some detail) ->
                Check.equal TextDiffFailureCodes.ContentNotText code "Initial evidence reports the text failure code."
                Check.true' (detail.Evidence.Length > 0) "The failure detail includes the evidence."
            | other -> failwith $"Initial binary evidence returned {other}."
            do! session.Close()
            return ()
        }
        "late binary evidence invalidates a ready session", fun () -> async {
            let previous = Array.init 40 (fun index -> { Text = (if index = 0 then "before" elif index = 30 then "binary\u0000tail" else "same-" + string index); Ending = LineEnding.LF })
            let current = Array.init 40 (fun index -> { Text = (if index = 0 then "after" elif index = 30 then "binary\u0000tail" else "same-" + string index); Ending = LineEnding.LF })
            let limits = { Limits.defaults with MaxUnits = 2_000; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 1 16 4 limits) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! firstResult = session.FirstPage(fun () -> false)
            let first = unwrap firstResult
            let page =
                match first with
                | Resumable.Ready value -> value
                | Resumable.Scanning _ -> failwith "The first changed row was not ready."
            match page.NextCursor with
            | None -> failwith "The first page did not leave more content to validate."
            | Some cursor ->
                let! result = session.ReadPage cursor (fun () -> false)
                match result with
                | EngineResult.Failed(code, _, Some _) -> Check.equal TextDiffFailureCodes.ContentNotText code "Late evidence reports the text failure code."
                | other -> failwith $"Late evidence returned {other}."
                let! repeated = session.ReadPage cursor (fun () -> false)
                match repeated with
                | EngineResult.Failed(code, _, _) -> Check.equal TextDiffFailureCodes.ContentNotText code "The invalidated session keeps failing."
                | other -> failwith $"The invalidated session returned {other}."
            do! session.Close()
            return ()
        }
        "small UTF-8 chunks preserve multibyte text around a change", fun () -> async {
            let prefix = String('a', 61)
            let previous = [| { Text = prefix + "€😀-old"; Ending = LineEnding.CRLF }; { Text = "tail"; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = prefix + "€😀-new"; Ending = LineEnding.CRLF }; { Text = "tail"; Ending = LineEnding.NoEnding } |]
            let! session = openSession (config 1 100 64 8 Limits.defaults) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            checkOracle previous current pages
            do! session.Close()
            return ()
        }
        "start middle and end edits preserve lines across window edges", fun () -> async {
            let previous = Array.init 48 (fun index -> { Text = "line-" + string index; Ending = LineEnding.LF })
            let current = Array.copy previous
            for index in [| 0; 19; 47 |] do current[index] <- { current[index] with Text = "edited-" + string index }
            current[47] <- { current[47] with Ending = LineEnding.NoEnding }
            let sessionConfig = config 2 1_000 64 16 Limits.defaults
            let! session = openSession sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            checkOracle previous current pages
            let changed = rows (allParts pages) |> Array.filter (fun row -> row.Kind <> DiffRowKind.Context)
            Check.true' (changed |> Array.exists (fun row -> row.Previous |> Option.exists (fun line -> line.Number = 0L))) "The first line edit appears in the page stream."
            Check.true' (changed |> Array.exists (fun row -> row.Current |> Option.exists (fun line -> line.Number = 47L && line.Ending = LineEnding.NoEnding))) "The final line edit keeps its missing terminator."
            do! session.Close()
            return ()
        }
        "large insertions deletions and rewrites preserve every line", fun () -> async {
            let baseLines = Array.init 96 (fun index -> { Text = "base-" + string index; Ending = LineEnding.LF })
            let inserted = Array.init 48 (fun index -> { Text = "inserted-" + string index; Ending = LineEnding.LF })
            let insertion = Array.concat [ baseLines[0..31]; inserted; baseLines[32..] ]
            let! insertSession = openSession (config 2 1_000 64 16 Limits.defaults) (sourceSpec (encodeLines baseLines)) (sourceSpec (encodeLines insertion))
            let! insertPages = readAll insertSession (fun () -> false)
            checkOracle baseLines insertion insertPages
            Check.true' (rows (allParts insertPages) |> Array.exists (fun row -> row.Kind = DiffRowKind.Added)) "The large insertion contains added rows."
            do! insertSession.Close()

            let! deleteSession = openSession (config 2 1_000 64 16 Limits.defaults) (sourceSpec (encodeLines insertion)) (sourceSpec (encodeLines baseLines))
            let! deletePages = readAll deleteSession (fun () -> false)
            checkOracle insertion baseLines deletePages
            Check.true' (rows (allParts deletePages) |> Array.exists (fun row -> row.Kind = DiffRowKind.Removed)) "The large deletion contains removed rows."
            do! deleteSession.Close()

            let rewritten = Array.init 48 (fun index -> { Text = "rewrite-" + string index; Ending = LineEnding.LF })
            let! rewriteSession = openSession (config 1 1_000 64 8 Limits.defaults) (sourceSpec (encodeLines baseLines[0..47])) (sourceSpec (encodeLines rewritten))
            let! rewritePages = readAll rewriteSession (fun () -> false)
            checkOracle baseLines[0..47] rewritten rewritePages
            Check.true' (allParts rewritePages |> Array.exists (function DiffPart.Hunk { Body = HunkBody.UnalignedSides _ } -> true | _ -> false)) "A full rewrite may use unaligned regions when its windows have no match."
            do! rewriteSession.Close()
            return ()
        }
        "equal prefixes and repeated keys do not lose unique lines", fun () -> async {
            let previous = Array.init 512 (fun index -> {
                Text = if index % 7 = 0 then "repeat-prefix" else "shared-prefix-" + String('x', 24) + string index
                Ending = LineEnding.LF
            })
            let current = ResizeArray<SourceLine>(previous)
            current.Insert(137, { Text = "shared-prefix-inserted"; Ending = LineEnding.CRLF })
            current[420] <- { current[420] with Text = "shared-prefix-replaced" }
            let current = current.ToArray()
            let! session = openSession (config 2 1_000 128 32 Limits.defaults) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            checkOracle previous current pages
            do! session.Close()
            return ()
        }
        "long equal lines keep their full key while displaying a bounded slice", fun () -> async {
            let longText = String('z', 9_000)
            let previous = [| { Text = longText; Ending = LineEnding.LF }; { Text = "old"; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = longText; Ending = LineEnding.LF }; { Text = "new"; Ending = LineEnding.NoEnding } |]
            let! session = openSession (config 0 100 64 8 Limits.defaults) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let parts = allParts pages
            Check.true' (parts |> Array.exists (function DiffPart.HiddenEqual gap -> gap.PreviousRange.Start = 0L && gap.PreviousRange.Count = 1L | _ -> false)) "The unchanged long line stays in a verified equal gap."
            let changed = rows parts |> Array.filter (fun row -> row.Kind <> DiffRowKind.Context)
            Check.equal 1 changed.Length "Only the short line appears as a change."
            match changed[0].Previous, changed[0].Current with
            | Some previousLine, Some currentLine ->
                Check.equal (Some 3L) previousLine.Slice.TotalUtf16 "The old short line keeps its total length."
                Check.equal (Some 3L) currentLine.Slice.TotalUtf16 "The new short line keeps its total length."
            | _ -> failwith "The replacement row has an absent side."
            do! session.Close()
            return ()
        }
        "an exhausted Myers gap becomes unaligned before a later edit", fun () -> async {
            let before = Array.init 4 (fun index -> { Text = "before-" + string index; Ending = LineEnding.LF })
            let oldMiddle = Array.init 8 (fun index -> { Text = "old-middle-" + string index; Ending = LineEnding.LF })
            let newMiddle = Array.init 8 (fun index -> { Text = "new-middle-" + string index; Ending = LineEnding.LF })
            let oldTail = { Text = "old-tail"; Ending = LineEnding.LF }
            let newTail = { Text = "new-tail"; Ending = LineEnding.LF }
            let sync = { Text = "sync"; Ending = LineEnding.LF }
            let after = Array.init 4 (fun index -> { Text = "after-" + string index; Ending = LineEnding.LF })
            let previous = Array.concat [ before; oldMiddle; [| sync; oldTail |]; after ]
            let current = Array.concat [ before; newMiddle; [| sync; newTail |]; after ]
            let sessionConfig = { config 0 1_000 128 64 Limits.defaults with MyersStepsPerGap = 1 }
            let! session = openSession sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            checkOracle previous current pages
            let parts = allParts pages
            let unalignedIndex = parts |> Array.tryFindIndex (function DiffPart.Hunk { Body = HunkBody.UnalignedSides _ } -> true | _ -> false)
            match unalignedIndex with
            | None -> failwith "The bounded edit gap did not become unaligned."
            | Some index ->
                Check.true' (parts |> Array.skip (index + 1) |> Array.exists (function DiffPart.Hunk { Body = HunkBody.AlignedRows values } -> values |> Array.exists (fun row -> row.Kind = DiffRowKind.Replaced) | _ -> false)) "A later edit remains aligned after the unaligned region."
            do! session.Close()
            return ()
        }
        "deterministic edit scripts rebuild both complete sources", fun () -> async {
            let random = RandomState 0x5EED1234u

            for caseIndex in 0 .. 199 do
                let previous = makeSourceLines random caseIndex
                let current = mutateLines random caseIndex previous
                let sessionConfig = config 2 10_000 1_024 4_096 Limits.defaults
                let! session = openSession sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages = readAll session (fun () -> false)
                checkOracle previous current pages
                do! session.Close()

            return ()
        }
    ]
