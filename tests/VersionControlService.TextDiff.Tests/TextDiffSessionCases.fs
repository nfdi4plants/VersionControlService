namespace VersionControlService.TextDiff.Tests

open System
open System.Text
open VersionControlService.Abstractions
open VersionControlService.TextDiff

module TextDiffSessionCases =
    type private SourceLine = { Text: string; Ending: LineEnding }

    type private CountingByteSource(bytes: byte[]) =
        let inner = MemoryByteSource(bytes) :> IByteSource
        let mutable bytesRead = 0L

        member _.BytesRead = bytesRead
        member _.Reset() = bytesRead <- 0L

        interface IByteSource with
            member _.KnownLength = inner.KnownLength
            member _.AvailableLength() = inner.AvailableLength()
            member _.IsComplete() = inner.IsComplete()
            member _.ReadAt position buffer offset count = async {
                let! outcome = inner.ReadAt position buffer offset count
                match outcome with
                | ReadOutcome.Bytes actual when actual > 0 -> bytesRead <- bytesRead + int64 actual
                | _ -> ()
                return outcome
            }

    type private DelayedByteSource(bytes: byte[], clock: ManualClock, delayMs: float) =
        let inner = MemoryByteSource(bytes) :> IByteSource

        interface IByteSource with
            member _.KnownLength = inner.KnownLength
            member _.AvailableLength() = inner.AvailableLength()
            member _.IsComplete() = inner.IsComplete()
            member _.ReadAt position buffer offset count = async {
                clock.Advance delayMs
                return! inner.ReadAt position buffer offset count
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

    let private absentSpec = {
        Source = None
        Encoding = "utf-8"
        BomLength = 0
        ByteLength = 0L
    }

    let mutable private sessionNumber = 0

    let private config contextLines pageRows chunkBytes windowLines limits =
        sessionNumber <- sessionNumber + 1
        { SessionConfig.defaults $"textdiff-test-{sessionNumber}" with
            ContextLines = contextLines
            PageMaxRows = pageRows
            PageMaxBytes = 512 * 1024
            PageMaxFragments = 32
            WindowMaxLines = windowLines
            WindowMaxBytes = 32 * 1024 * 1024
            CommonChunkBytes = chunkBytes
            MyersStepsPerGap = 100_000
            ResyncSampleModulus = 1
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

    let private faultHost sessionId =
        let pairWrites = ref false
        let indexWrites = ref false
        let indexWriteCount = ref 0
        let plain = Host.createInMemory (ManualClock 0.0 :> IClock)
        let createStore name = async {
            let! inner = plain.CreateTempStore name
            return
                { new ITempStore with
                    member _.Append buffer offset count = inner.Append buffer offset count
                    member _.WriteAt position buffer offset count = async {
                        if name = sessionId + ":pairs" && pairWrites.Value then
                            failwith "injected pair store write failure"
                        if name = sessionId + ":journal-index" && indexWrites.Value then
                            indexWriteCount.Value <- indexWriteCount.Value + 1
                            if indexWriteCount.Value = 2 then failwith "injected journal link failure"
                        return! inner.WriteAt position buffer offset count
                    }
                    member _.ReadAt position buffer offset count = inner.ReadAt position buffer offset count
                    member _.Length() = inner.Length()
                    member _.Dispose() = inner.Dispose() }
        }
        { plain with CreateTempStore = createStore }, pairWrites, indexWrites

    let private sourceSpecWith (source: IByteSource) (encoding: string) (bomLength: int) (byteLength: int64) = {
        Source = Some source
        Encoding = encoding
        BomLength = bomLength
        ByteLength = byteLength
    }

    let private resolveExpansion (session: TextDiffSession) (gapId: string) (fromStart: bool) (count: int) (initial: EngineResult<Resumable<DiffPart[]>>) = async {
        let mutable result = initial
        let mutable parts = None
        let mutable attempts = 0
        while parts.IsNone do
            attempts <- attempts + 1
            if attempts > 1_000 then failwith "The expansion did not finish after many continuations."
            match result with
            | EngineResult.Ok(Resumable.Ready value) -> parts <- Some value
            | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                let! next = session.Expand(gapId, fromStart, count, Some continuation, fun () -> false)
                result <- next
            | EngineResult.Failed(code, message, detail) -> failwith $"The expansion failed with {code}: {message}. Detail: {detail}."
            | EngineResult.Canceled -> failwith "The expansion canceled an uncanceled request."
        return parts.Value
    }

    let private resolveLine (session: TextDiffSession) (side: DiffSide) (line: int64) (offset: int64) (maxUtf16: int) (initial: EngineResult<Resumable<DiffLine>>) = async {
        let mutable result = initial
        let mutable value = None
        let mutable attempts = 0
        while value.IsNone do
            attempts <- attempts + 1
            if attempts > 1_000 then failwith "The line read did not finish after many continuations."
            match result with
            | EngineResult.Ok(Resumable.Ready lineValue) -> value <- Some lineValue
            | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                let! next = session.ReadLine(side, line, offset, maxUtf16, Some continuation, fun () -> false)
                result <- next
            | EngineResult.Failed(code, message, detail) -> failwith $"The line read failed with {code}: {message}. Detail: {detail}."
            | EngineResult.Canceled -> failwith "The line read canceled an uncanceled request."
        return value.Value
    }

    let private findPendingPreview (session: TextDiffSession) predicate initial = async {
        let mutable result = initial
        let mutable preview = None
        let mutable readyPreview = false
        let mutable attempts = 0
        let seen = ResizeArray<string>()
        while preview.IsNone && attempts < 64 do
            attempts <- attempts + 1
            match result with
            | EngineResult.Ok(Resumable.Scanning(_, continuation, Some value)) ->
                let sideSummary = function
                    | PendingSide.NoActiveLine -> "none"
                    | PendingSide.Exhausted count -> "end:" + string count
                    | PendingSide.Snippet snippet -> "line:" + string snippet.Line + ",offset:" + string snippet.OffsetUtf16 + ",length:" + string snippet.Text.Length + ",end:" + string snippet.End
                if seen.Count < 16 then seen.Add(sideSummary value.Previous + " / " + sideSummary value.Current + " mismatch=" + string value.Mismatch)
                if predicate value then preview <- Some value
                else
                    let! next = session.ReadPage continuation (fun () -> false)
                    result <- next
            | EngineResult.Ok(Resumable.Scanning(_, continuation, None)) ->
                let! next = session.ReadPage continuation (fun () -> false)
                result <- next
            | EngineResult.Ok(Resumable.Ready page) ->
                match page.Pending with
                | Some value when predicate value -> preview <- Some value; readyPreview <- true
                | None ->
                    match page.NextCursor with
                    | Some cursor ->
                        let! next = session.ReadPage cursor (fun () -> false)
                        result <- next
                    | None -> attempts <- 2_000
                | Some _ ->
                    match page.NextCursor with
                    | Some cursor ->
                        let! next = session.ReadPage cursor (fun () -> false)
                        result <- next
                    | None -> attempts <- 2_000
            | EngineResult.Failed(code, message, detail) -> failwith $"The session failed with {code}: {message}. Detail: {detail}."
            | EngineResult.Canceled -> failwith "The session canceled an uncanceled request."
        if preview.IsNone then
            let state =
                match result with
                | EngineResult.Ok(Resumable.Scanning(progress, _, pending)) -> $"Scanning at {progress.ValidatedBytes}, preview={pending}"
                | EngineResult.Ok(Resumable.Ready page) -> $"Ready at {page.Progress.ValidatedBytes}, cursor={page.NextCursor.IsSome}, preview={page.Pending}"
                | other -> sprintf "%A" other
            let previewTrace = String.concat " | " seen
            failwith $"No matching pending preview after {attempts} reads. {state}. Seen: {previewTrace}."
        return preview, readyPreview
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

    let private jsonStringBytes (value: string) =
        let mutable size = 2
        let mutable index = 0
        while index < value.Length do
            let unit = int value[index]
            if unit = 0x22 || unit = 0x5C then size <- size + 2
            elif unit < 0x20 then
                if unit = 0x08 || unit = 0x09 || unit = 0x0A || unit = 0x0C || unit = 0x0D then size <- size + 2
                else size <- size + 6
            elif unit < 0x80 then size <- size + 1
            elif unit < 0x800 then size <- size + 2
            elif unit >= 0xD800 && unit <= 0xDBFF && index + 1 < value.Length && Char.IsLowSurrogate value[index + 1] then
                size <- size + 4
                index <- index + 1
            elif unit >= 0xD800 && unit <= 0xDFFF then size <- size + 6
            else size <- size + 3
            index <- index + 1
        size

    let private jsonFieldBytes name valueBytes = jsonStringBytes name + 1 + valueBytes

    let private jsonObjectBytes (fields: int[]) =
        2 + (if fields.Length = 0 then 0 else fields.Length - 1) + Array.sum fields

    let private jsonArrayBytes (values: int[]) =
        2 + (if values.Length = 0 then 0 else values.Length - 1) + Array.sum values

    let private jsonTagBytes tag = jsonObjectBytes [| jsonFieldBytes "tag" (jsonStringBytes tag) |]
    let private jsonIntBytes value = (string value).Length
    let private jsonInt64Bytes (value: int64) = jsonStringBytes (string value)

    let private jsonRangeBytes (range: LineRange) =
        jsonObjectBytes [|
            jsonFieldBytes "start" (jsonInt64Bytes range.Start)
            jsonFieldBytes "count" (jsonInt64Bytes range.Count)
        |]

    let private jsonEndingBytes = function
        | LineEnding.NoEnding -> jsonTagBytes "NoEnding"
        | LineEnding.LF -> jsonTagBytes "LF"
        | LineEnding.CRLF -> jsonTagBytes "CRLF"
        | LineEnding.CR -> jsonTagBytes "CR"

    let private jsonHighlightBytes (highlight: Highlight) =
        let kind =
            match highlight.Kind with
            | HighlightKind.UnchangedText -> jsonTagBytes "UnchangedText"
            | HighlightKind.ChangedText -> jsonTagBytes "ChangedText"
        jsonObjectBytes [|
            jsonFieldBytes "start" (jsonIntBytes highlight.Start)
            jsonFieldBytes "length" (jsonIntBytes highlight.Length)
            jsonFieldBytes "kind" kind
        |]

    let private jsonLineBytes (line: DiffLine) =
        let total = line.Slice.TotalUtf16 |> Option.map jsonInt64Bytes |> Option.defaultValue 4
        let highlights = line.Slice.Highlights |> Array.map jsonHighlightBytes |> jsonArrayBytes
        let slice =
            jsonObjectBytes [|
                jsonFieldBytes "offsetUtf16" (jsonInt64Bytes line.Slice.OffsetUtf16)
                jsonFieldBytes "totalUtf16" total
                jsonFieldBytes "text" (jsonStringBytes line.Slice.Text)
                jsonFieldBytes "highlights" highlights
            |]
        jsonObjectBytes [|
            jsonFieldBytes "number" (jsonInt64Bytes line.Number)
            jsonFieldBytes "ending" (jsonEndingBytes line.Ending)
            jsonFieldBytes "slice" slice
        |]

    let private jsonOptionalLineBytes = function
        | Some line -> jsonLineBytes line
        | None -> 4

    let private jsonRowBytes (row: DiffRow) =
        let kind =
            match row.Kind with
            | DiffRowKind.Context -> "Context"
            | DiffRowKind.Added -> "Added"
            | DiffRowKind.Removed -> "Removed"
            | DiffRowKind.Replaced -> "Replaced"
            | DiffRowKind.EndingChanged -> "EndingChanged"
        jsonObjectBytes [|
            jsonFieldBytes "id" (jsonStringBytes row.Id)
            jsonFieldBytes "kind" (jsonTagBytes kind)
            jsonFieldBytes "previous" (jsonOptionalLineBytes row.Previous)
            jsonFieldBytes "current" (jsonOptionalLineBytes row.Current)
        |]

    let private jsonHunkBodyBytes = function
        | HunkBody.AlignedRows values ->
            jsonObjectBytes [|
                jsonFieldBytes "tag" (jsonStringBytes "AlignedRows")
                jsonFieldBytes "rows" (values |> Array.map jsonRowBytes |> jsonArrayBytes)
            |]
        | HunkBody.UnalignedSides(previous, current) ->
            jsonObjectBytes [|
                jsonFieldBytes "tag" (jsonStringBytes "UnalignedSides")
                jsonFieldBytes "previous" (previous |> Array.map jsonLineBytes |> jsonArrayBytes)
                jsonFieldBytes "current" (current |> Array.map jsonLineBytes |> jsonArrayBytes)
            |]

    let private jsonGapBytes (gap: EqualGap) =
        jsonObjectBytes [|
            jsonFieldBytes "gapId" (jsonStringBytes gap.GapId)
            jsonFieldBytes "previousRange" (jsonRangeBytes gap.PreviousRange)
            jsonFieldBytes "currentRange" (jsonRangeBytes gap.CurrentRange)
        |]

    let private jsonPartBytes = function
        | DiffPart.Hunk fragment ->
            let hunk =
                jsonObjectBytes [|
                    jsonFieldBytes "hunkId" (jsonStringBytes fragment.HunkId)
                    jsonFieldBytes "previousRange" (jsonRangeBytes fragment.PreviousRange)
                    jsonFieldBytes "currentRange" (jsonRangeBytes fragment.CurrentRange)
                    jsonFieldBytes "startsHunk" (if fragment.StartsHunk then 4 else 5)
                    jsonFieldBytes "endsHunk" (if fragment.EndsHunk then 4 else 5)
                    jsonFieldBytes "body" (jsonHunkBodyBytes fragment.Body)
                |]
            jsonObjectBytes [|
                jsonFieldBytes "tag" (jsonStringBytes "Hunk")
                jsonFieldBytes "fragment" hunk
            |]
        | DiffPart.HiddenEqual gap ->
            jsonObjectBytes [|
                jsonFieldBytes "tag" (jsonStringBytes "HiddenEqual")
                jsonFieldBytes "gap" (jsonGapBytes gap)
            |]
        | DiffPart.ExpandedContext(gapId, values) ->
            jsonObjectBytes [|
                jsonFieldBytes "tag" (jsonStringBytes "ExpandedContext")
                jsonFieldBytes "gapId" (jsonStringBytes gapId)
                jsonFieldBytes "rows" (values |> Array.map jsonRowBytes |> jsonArrayBytes)
            |]

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

    let private fetchCompleteLine (session: TextDiffSession) side (line: DiffLine) = async {
        let total =
            match line.Slice.TotalUtf16 with
            | Some value -> value
            | None -> failwith "An emitted line has no known length."
        let text = StringBuilder()
        let highlights = ResizeArray<Highlight>()
        let mutable offset = 0L
        let mutable ending = line.Ending
        let mutable slices = 0
        while offset < total && slices < 100_000 do
            slices <- slices + 1
            let! initial = session.ReadLine(side, line.Number, offset, 8_192, None, fun () -> false)
            let! slice = resolveLine session side line.Number offset 8_192 initial
            if slice.Slice.OffsetUtf16 <> offset || slice.Slice.Text.Length = 0 then failwith "A line slice did not advance from its requested offset."
            text.Append(slice.Slice.Text) |> ignore
            for span in slice.Slice.Highlights do
                let absolute = { span with Start = int offset + span.Start }
                if highlights.Count > 0 then
                    let previous = highlights[highlights.Count - 1]
                    if previous.Kind = absolute.Kind && previous.Start + previous.Length = absolute.Start then
                        highlights[highlights.Count - 1] <- { previous with Length = previous.Length + absolute.Length }
                    else
                        highlights.Add absolute
                else
                    highlights.Add absolute
            ending <- slice.Ending
            offset <- offset + int64 slice.Slice.Text.Length
        if offset <> total then failwith "The line slices did not reach the end of the source line."
        let sliceHighlights = highlights.ToArray()
        if total <= 8_192L then
            let! wholeInitial = session.ReadLine(side, line.Number, 0L, max 1 (int total), None, fun () -> false)
            let! whole = resolveLine session side line.Number 0L (max 1 (int total)) wholeInitial
            let wholeHighlights =
                whole.Slice.Highlights
                |> Array.map (fun span -> { span with Start = int whole.Slice.OffsetUtf16 + span.Start })
            Check.sequence wholeHighlights sliceHighlights "Line slices join to the highlights from one full line read."
            let pageStart = int line.Slice.OffsetUtf16
            let pageFinish = pageStart + line.Slice.Text.Length
            let pageHighlights =
                wholeHighlights
                |> Array.choose (fun span ->
                    let start = max pageStart span.Start
                    let finish = min pageFinish (span.Start + span.Length)
                    if finish <= start then None
                    else Some { span with Start = start - pageStart; Length = finish - start })
            Check.sequence pageHighlights line.Slice.Highlights "A displayed line slice matches the full line highlights."
        return { Text = text.ToString(); Ending = ending }, sliceHighlights
    }

    let private checkSliceOracle (session: TextDiffSession) (previous: SourceLine[]) (current: SourceLine[]) (pages: DiffPage[]) = async {
        let rebuiltPrevious = ResizeArray<SourceLine>()
        let rebuiltCurrent = ResizeArray<SourceLine>()
        let appendLine side (target: ResizeArray<SourceLine>) line = async {
            let! full, _ = fetchCompleteLine session side line
            target.Add full
        }
        let appendRange (expected: SourceLine[]) (target: ResizeArray<SourceLine>) (range: LineRange) =
            if range.Start < 0L || range.Count < 0L || range.Start + range.Count > int64 expected.Length then failwith $"The session returned an invalid equal range {range}."
            for index = int range.Start to int (range.Start + range.Count) - 1 do target.Add expected[index]
        let appendRows (values: DiffRow[]) = async {
            for row in values do
                match row.Previous with
                | Some line -> do! appendLine DiffSide.Previous rebuiltPrevious line
                | None -> ()
                match row.Current with
                | Some line -> do! appendLine DiffSide.Current rebuiltCurrent line
                | None -> ()
        }
        for part in allParts pages do
            match part with
            | DiffPart.HiddenEqual gap ->
                let count = min 3 (int (min gap.PreviousRange.Count 3L))
                let! initial = session.Expand(gap.GapId, true, count, None, fun () -> false)
                let! expanded = resolveExpansion session gap.GapId true count initial
                for expandedPart in expanded do
                    match expandedPart with
                    | DiffPart.ExpandedContext(_, values) -> do! appendRows values
                    | DiffPart.HiddenEqual remaining ->
                        appendRange previous rebuiltPrevious remaining.PreviousRange
                        appendRange current rebuiltCurrent remaining.CurrentRange
                    | DiffPart.Hunk fragment ->
                        match fragment.Body with
                        | HunkBody.AlignedRows values -> do! appendRows values
                        | HunkBody.UnalignedSides(previousLines, currentLines) ->
                            for line in previousLines do do! appendLine DiffSide.Previous rebuiltPrevious line
                            for line in currentLines do do! appendLine DiffSide.Current rebuiltCurrent line
            | DiffPart.Hunk fragment ->
                match fragment.Body with
                | HunkBody.AlignedRows values -> do! appendRows values
                | HunkBody.UnalignedSides(previousLines, currentLines) ->
                    for line in previousLines do do! appendLine DiffSide.Previous rebuiltPrevious line
                    for line in currentLines do do! appendLine DiffSide.Current rebuiltCurrent line
            | DiffPart.ExpandedContext(_, values) -> do! appendRows values
        Check.sequence previous rebuiltPrevious "The page, gap and slice results rebuild the previous source."
        Check.sequence current rebuiltCurrent "The page, gap and slice results rebuild the current source."
    }

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

    /// A source of several hundred lines and a copy of it with one to three large insertions, deletions or rewrites.
    let private largeEdit (random: RandomState) caseIndex =
        let count = 300 + random.Next 700
        let previous = Array.init count (fun index -> { Text = makeText random caseIndex index; Ending = nextEnding random false })
        let current = ResizeArray<SourceLine>(previous)
        let fresh edit amount =
            Array.init amount (fun offset -> { Text = "large-" + string caseIndex + "-" + string edit + "-" + string offset; Ending = LineEnding.LF })
        for edit in 0 .. random.Next 3 do
            let amount = 20 + random.Next 80
            let start = random.Next(max 1 (current.Count - amount))
            match random.Next 3 with
            | 0 -> current.RemoveRange(start, min amount (current.Count - start))
            | 1 -> current.InsertRange(start, fresh edit amount)
            | _ ->
                current.RemoveRange(start, min amount (current.Count - start))
                current.InsertRange(start, fresh edit amount)
        previous, current.ToArray()

    /// Reads every page and spills the session between requests whenever it may give up its memory.
    let private readAllSpilling (session: TextDiffSession) = async {
        let pages = ResizeArray<DiffPage>()
        let mutable spills = 0
        let mutable requests = 0
        let mutable result = EngineResult.Canceled
        let mutable pending = true
        let! first = session.FirstPage(fun () -> false)
        result <- first
        while pending do
            requests <- requests + 1
            if requests > 200_000 then failwith "The session did not finish."
            let holder = session :> IScratchHolder
            if holder.HoldsScratch && not holder.MustKeepScratch then
                let! spilled = session.Spill()
                match spilled with
                | EngineResult.Ok() -> spills <- spills + 1
                | other -> failwith $"The spill failed: {other}."
            match result with
            | EngineResult.Ok(Resumable.Ready page) ->
                pages.Add page
                match page.NextCursor with
                | None -> pending <- false
                | Some cursor ->
                    let! next = session.ReadPage cursor (fun () -> false)
                    result <- next
            | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                let! next = session.ReadPage continuation (fun () -> false)
                result <- next
            | other -> failwith $"The session returned {other}."
        return pages.ToArray(), spills
    }

    let cases: (string * (unit -> Async<unit>)) list = [
        "gap expansion reads both ends and returns the recorded replacement", fun () -> async {
            let expected = Array.init 12 (fun index -> { Text = "line-" + string index; Ending = LineEnding.LF })
            let source = sourceSpec (encodeLines expected)
            let! session = openSession (defaultConfig ()) source source
            let! pages = readAll session (fun () -> false)
            let gap = allParts pages |> Array.pick (function DiffPart.HiddenEqual value -> Some value | _ -> None)
            let! fromStartInitial = session.Expand(gap.GapId, true, 4, None, fun () -> false)
            let! fromStart = resolveExpansion session gap.GapId true 4 fromStartInitial
            Check.equal 2 fromStart.Length "The first expansion leaves one hidden range."
            match fromStart[0], fromStart[1] with
            | DiffPart.ExpandedContext(id, rows), DiffPart.HiddenEqual remaining ->
                Check.equal gap.GapId id "The expansion keeps the replaced gap id."
                Check.equal 4 rows.Length "The expansion returns the requested leading lines."
                Check.equal 8L remaining.PreviousRange.Count "The leading expansion leaves the trailing range."
            | _ -> failwith "The leading expansion returned parts in the wrong order."
            let! retryInitial = session.Expand(gap.GapId, true, 4, None, fun () -> false)
            let! retry = resolveExpansion session gap.GapId true 4 retryInitial
            let firstDebug = sprintf "%A" fromStart
            let retryDebug = sprintf "%A" retry
            Check.true' (Unchecked.equals fromStart retry) $"A replaced gap returns its first result. First: {firstDebug}; retry: {retryDebug}."
            let visible = ResizeArray<int64>()
            let addVisible (parts: DiffPart[]) =
                for part in parts do
                    match part with
                    | DiffPart.ExpandedContext(_, values) ->
                        for row in values do visible.Add row.Previous.Value.Number
                    | _ -> ()
            addVisible fromStart
            let mutable remaining = Some(fromStart |> Array.pick (function DiffPart.HiddenEqual value -> Some value | _ -> None))
            while remaining.IsSome do
                let currentGap = remaining.Value
                let! expandedInitial = session.Expand(currentGap.GapId, false, 3, None, fun () -> false)
                let! expanded = resolveExpansion session currentGap.GapId false 3 expandedInitial
                addVisible expanded
                remaining <- expanded |> Array.tryPick (function DiffPart.HiddenEqual value -> Some value | _ -> None)
            Check.equal expected.Length visible.Count "Expanding both ends covers every line."
            Check.sequence [| 0L .. int64 expected.Length - 1L |] (visible |> Seq.sort) "Expansion results contain each line once."
            let previousInfo, currentInfo = session.SourceInfo
            Check.equal (int64 (encodeLines expected).Length) previousInfo.ByteLength "Source info reports the byte length."
            Check.equal (Some(int64 expected.Length)) previousInfo.LineCount "Source info reports the scanned line count."
            Check.equal "utf-8" currentInfo.Encoding "Source info reports the encoding."
            Check.true' (not previousInfo.HasBom && not currentInfo.HasBom) "Source info reports that the inputs have no BOM."
            do! session.Close()
            return ()
        }
        "pending expansion continuations bind their gap fields and a fresh request replaces pending work", fun () -> async {
            let expected = Array.init 64 (fun index -> { Text = "line-" + string index; Ending = LineEnding.LF })
            let source = sourceSpec (encodeLines expected)
            let limits = { Limits.defaults with MaxUnits = 1; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 1 1_000 1_024 4_096 limits) source source
            let! pages = readAll session (fun () -> false)
            let gap = allParts pages |> Array.pick (function DiffPart.HiddenEqual value -> Some value | _ -> None)
            let! initial = session.Expand(gap.GapId, true, 32, None, fun () -> false)
            let firstContinuation =
                match initial with
                | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) -> continuation
                | other -> failwith $"The bounded expansion returned {other}."
            let! mismatch = session.Expand(gap.GapId, false, 32, Some firstContinuation, fun () -> false)
            match mismatch with
            | EngineResult.Failed("continuation_mismatch", _, _) -> ()
            | other -> failwith $"Changing expansion fields returned {other}."
            let! recorded = session.Expand(gap.GapId, true, 32, Some firstContinuation, fun () -> false)
            let! retry = session.Expand(gap.GapId, true, 32, Some firstContinuation, fun () -> false)
            Check.true' (Unchecked.equals recorded retry) $"Retrying an expansion continuation returns its journaled result. First: {recorded}; retry: {retry}."
            let staleContinuation =
                match recorded with
                | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) -> continuation
                | other -> failwith $"The small expansion did not suspend after its first continuation: {other}."
            let! fresh = session.Expand(gap.GapId, true, 32, None, fun () -> false)
            let! stale = session.Expand(gap.GapId, true, 32, Some staleContinuation, fun () -> false)
            match stale with
            | EngineResult.Failed("continuation_mismatch", _, _) -> ()
            | other -> failwith $"The discarded continuation returned {other}."
            let! completed = resolveExpansion session gap.GapId true 32 fresh
            Check.true' (completed |> Array.exists (function DiffPart.ExpandedContext _ -> true | _ -> false)) "The fresh expansion completes."
            do! session.Close()
            return ()
        }
        "line reads return resumable surrogate safe slices and replay continuations", fun () -> async {
            let text = String('a', 8_191) + "😀" + String('z', 10_000)
            let source = sourceSpec (Encoding.UTF8.GetBytes(text + "\n"))
            let limits = { Limits.defaults with MaxUnits = 1; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 1 1_000 1_024 4_096 limits) source source
            let! _ = readAll session (fun () -> false)
            let! initial = session.ReadLine(DiffSide.Previous, 0L, 0L, 9_000, None, fun () -> false)
            let continuation =
                match initial with
                | EngineResult.Ok(Resumable.Scanning(_, value, _)) -> value
                | other -> failwith $"The long line read returned {other}."
            let! next = session.ReadLine(DiffSide.Previous, 0L, 0L, 9_000, Some continuation, fun () -> false)
            let! retry = session.ReadLine(DiffSide.Previous, 0L, 0L, 9_000, Some continuation, fun () -> false)
            Check.true' (Unchecked.equals next retry) "Retrying a line continuation returns its journaled result."
            let! first = resolveLine session DiffSide.Previous 0L 0L 9_000 next
            Check.equal (Some(int64 text.Length)) first.Slice.TotalUtf16 "The line read reports its total length."
            Check.equal 8_191 first.Slice.Text.Length "The first slice ends before the high surrogate at its boundary."
            let! secondInitial = session.ReadLine(DiffSide.Previous, 0L, int64 first.Slice.Text.Length, 128, None, fun () -> false)
            let! second = resolveLine session DiffSide.Previous 0L (int64 first.Slice.Text.Length) 128 secondInitial
            Check.true' (second.Slice.Text.StartsWith("😀", StringComparison.Ordinal)) "The next slice starts with the complete surrogate pair."
            do! session.Close()
            return ()
        }
        "abandoned line reads release scratch and expire old continuations", fun () -> async {
            let words count (format: int -> string) = String.Join(" ", Array.init count format)
            let previousLine = words 3_500 (fun index -> sprintf "w%06d" index)
            let currentLine = words 3_500 (fun index -> if index = 2_000 then sprintf "X%06d" index else sprintf "w%06d" index)
            let previous = [|
                { Text = "head"; Ending = LineEnding.LF }
                { Text = previousLine; Ending = LineEnding.LF }
                { Text = "tail"; Ending = LineEnding.LF }
            |]
            let current = [|
                { Text = "head"; Ending = LineEnding.LF }
                { Text = currentLine; Ending = LineEnding.LF }
                { Text = "tail"; Ending = LineEnding.LF }
            |]
            let limits = { Limits.defaults with MaxUnits = 12; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let lineConfig = { defaultConfig () with Limits = limits }
            let clock = ManualClock 0.0
            let host = Host.createInMemory (clock :> IClock)
            let ledger = Ledger()
            let! session = TextDiffSession.create host ledger lineConfig (fun _ -> 100) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let changed = rows (allParts pages) |> Array.find (fun row -> row.Kind = DiffRowKind.Replaced)
            Check.equal 1L changed.Current.Value.Number "The long changed line is available for line reads."

            let pagePrevious = Array.init 2_001 (fun index -> { Text = "row " + string index; Ending = LineEnding.LF })
            let pageCurrent = Array.init 2_001 (fun index -> { Text = (if index % 7 = 3 then "edit " + string index else "row " + string index); Ending = LineEnding.LF })
            let pageConfig = { config 3 1_000 1_024 4 limits with PageMaxFragments = 1 }
            let resolveFirstPageWithCount (value: TextDiffSession) = async {
                let! first = value.FirstPage(fun () -> false)
                let mutable result = first
                let mutable page = None
                let mutable requests = 1
                while page.IsNone && requests < 100 do
                    match result with
                    | EngineResult.Ok(Resumable.Ready ready) -> page <- Some ready
                    | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                        let! next = value.ReadPage continuation (fun () -> false)
                        result <- next
                        requests <- requests + 1
                    | other -> failwith $"The page read returned {other}."
                match page with
                | Some ready -> return ready, requests
                | None -> return failwith "The page read did not finish within its request budget."
            }
            let openPageSession sessionId =
                TextDiffSession.create host ledger { pageConfig with SessionId = sessionId } (fun _ -> 100) (sourceSpec (encodeLines pagePrevious)) (sourceSpec (encodeLines pageCurrent))
            let! baselineSession = openPageSession "abandoned-read-baseline"
            let! _, baselineRequests = resolveFirstPageWithCount baselineSession
            do! baselineSession.Close()

            let continuations = ResizeArray<string>()
            for _ in 1 .. 16 do
                let! result = session.ReadLine(DiffSide.Current, 1L, 8_192L, 8_192, None, fun () -> false)
                match result with
                | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) -> continuations.Add continuation
                | other -> failwith $"The abandoned line read returned {other}."
                Check.equal 0L (ledger.Used AllocationCategory.AlignmentScratch) "A suspended line read releases its pair-analysis reservation."

            let! expired = session.ReadLine(DiffSide.Current, 1L, 8_192L, 8_192, Some continuations[0], fun () -> false)
            match expired with
            | EngineResult.Failed("continuation_mismatch", _, _) -> ()
            | other -> failwith $"The oldest line continuation returned {other}."

            let! freshInitial = session.ReadLine(DiffSide.Current, 1L, 0L, 8_192, None, fun () -> false)
            let mutable freshResult = freshInitial
            let mutable freshLine = None
            let mutable freshRequests = 1
            while freshLine.IsNone && freshRequests < 40 do
                match freshResult with
                | EngineResult.Ok(Resumable.Ready line) -> freshLine <- Some line
                | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                    let! next = session.ReadLine(DiffSide.Current, 1L, 0L, 8_192, Some continuation, fun () -> false)
                    freshResult <- next
                    freshRequests <- freshRequests + 1
                | other -> failwith $"The fresh line read returned {other}."
            Check.true' freshLine.IsSome "A fresh line read finishes within its usual request budget."
            Check.equal (Some(int64 currentLine.Length)) freshLine.Value.Slice.TotalUtf16 "The fresh line read keeps its total length."
            Check.equal 0L (ledger.Used AllocationCategory.AlignmentScratch) "A completed line read releases pair-analysis scratch."

            let! pageSession = openPageSession "abandoned-read-page"
            let! page, pageRequests = resolveFirstPageWithCount pageSession
            Check.true' (page.Parts.Length > 0) "A new page is available after the abandoned reads."
            Check.true' (pageRequests <= baselineRequests + 1) "The page takes its usual number of requests after the abandoned reads."
            do! pageSession.Close()
            do! session.Close()
            return ()
        }
        "aligned replacements mark changed tokens", fun () -> async {
            let previous = [| { Text = "prefix old suffix"; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = "prefix new suffix"; Ending = LineEnding.NoEnding } |]
            let! session = openSession (config 0 100 64 8 Limits.defaults) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let row = rows (allParts pages) |> Array.find (fun row -> row.Kind = DiffRowKind.Replaced)
            let previousHighlights = row.Previous.Value.Slice.Highlights
            let currentHighlights = row.Current.Value.Slice.Highlights
            let previousChange = previousHighlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
            let currentChange = currentHighlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
            Check.equal 1 previousChange.Length "The old token has one changed span."
            Check.equal 1 currentChange.Length "The new token has one changed span."
            Check.equal "old" (row.Previous.Value.Slice.Text.Substring(previousChange[0].Start, previousChange[0].Length)) "The old span covers its token."
            Check.equal "new" (row.Current.Value.Slice.Text.Substring(currentChange[0].Start, currentChange[0].Length)) "The new span covers its token."
            do! session.Close()

            let punctuationOld = String('.', 12_000)
            let punctuationNew = punctuationOld.Substring(0, 5_000) + "!" + punctuationOld.Substring(5_001)
            let oldLines = [| { Text = punctuationOld; Ending = LineEnding.NoEnding } |]
            let newLines = [| { Text = punctuationNew; Ending = LineEnding.NoEnding } |]
            let! fallbackSession = openSession (config 0 100 64 8 Limits.defaults) (sourceSpec (encodeLines oldLines)) (sourceSpec (encodeLines newLines))
            let! fallbackPages = readAll fallbackSession (fun () -> false)
            let fallbackRow = rows (allParts fallbackPages) |> Array.find (fun row -> row.Kind = DiffRowKind.Replaced)
            Check.equal 4_872L fallbackRow.Previous.Value.Slice.OffsetUtf16 "The long-line slice starts before the changed punctuation."
            let oldChanged = fallbackRow.Previous.Value.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
            let newChanged = fallbackRow.Current.Value.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
            Check.equal 1 oldChanged.Length "The old line uses a bounded changed span."
            Check.equal 1 newChanged.Length "The new line uses a bounded changed span."
            Check.equal 1 oldChanged[0].Length "The old changed span covers one punctuation mark."
            Check.equal 1 newChanged[0].Length "The new changed span covers one punctuation mark."
            do! fallbackSession.Close()
            return ()
        }
        "long replacement slices keep whole line highlights", fun () -> async {
            let prefix = String('a', 19_997) + " "
            let suffix = " " + String('b', 9_998)
            let previous = [| { Text = prefix + "old" + suffix; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = prefix + "new" + suffix; Ending = LineEnding.NoEnding } |]
            let limits = { Limits.defaults with MaxUnits = 256; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 100 64 8 limits) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let row = rows (allParts pages) |> Array.find (fun row -> row.Kind = DiffRowKind.Replaced)
            Check.equal DiffRowKind.Replaced row.Kind "The long lines form one replacement row."
            let readSlice side offset = async {
                let! initial = session.ReadLine(side, 0L, offset, 8_192, None, fun () -> false)
                return! resolveLine session side 0L offset 8_192 initial
            }
            let wordStart = int64 prefix.Length
            for offset in [| 0L; 8_192L; 16_384L; 24_576L |] do
                let! oldSlice = readSlice DiffSide.Previous offset
                let! newSlice = readSlice DiffSide.Current offset
                let oldChanged = oldSlice.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
                let newChanged = newSlice.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
                let expectedCount = if offset = 16_384L then 1 else 0
                Check.equal expectedCount oldChanged.Length "The previous slice has changed text only where the old word falls."
                Check.equal expectedCount newChanged.Length "The current slice has changed text only where the new word falls."
                if expectedCount = 1 then
                    Check.equal (int (wordStart - offset)) oldChanged[0].Start "The previous word span is relative to its slice."
                    Check.equal (int (wordStart - offset)) newChanged[0].Start "The current word span is relative to its slice."
                    Check.equal 3 oldChanged[0].Length "The previous span covers the changed word."
                    Check.equal 3 newChanged[0].Length "The current span covers the changed word."
                    Check.equal "old" (oldSlice.Slice.Text.Substring(oldChanged[0].Start, oldChanged[0].Length)) "The old slice span selects its word."
                    Check.equal "new" (newSlice.Slice.Text.Substring(newChanged[0].Start, newChanged[0].Length)) "The new slice span selects its word."
            do! session.Close()
            return ()
        }
        "large replacement slices clip the differing middle", fun () -> async {
            let shared = String.concat " " (Array.create 16_500 "p")
            let middleLength = 9_000
            let previous = [| { Text = shared + String('o', middleLength) + shared; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = shared + String('n', middleLength) + shared; Ending = LineEnding.NoEnding } |]
            let limits = { Limits.defaults with MaxUnits = 256; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 100 64 8 limits) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let row = rows (allParts pages) |> Array.find (fun row -> row.Kind = DiffRowKind.Replaced)
            Check.equal DiffRowKind.Replaced row.Kind "The long lines form one replacement row."
            let changedStart = shared.Length
            let changedFinish = changedStart + middleLength
            let total = previous[0].Text.Length
            for offset in [| 0L; 32_768L; 40_960L; 65_536L |] do
                let! oldInitial = session.ReadLine(DiffSide.Previous, 0L, offset, 4_096, None, fun () -> false)
                let! oldSlice = resolveLine session DiffSide.Previous 0L offset 4_096 oldInitial
                let! newInitial = session.ReadLine(DiffSide.Current, 0L, offset, 4_096, None, fun () -> false)
                let! newSlice = resolveLine session DiffSide.Current 0L offset 4_096 newInitial
                let sliceFinish = min total (int offset + oldSlice.Slice.Text.Length)
                let overlapStart = max changedStart (int offset)
                let overlapFinish = min changedFinish sliceFinish
                let expectedLength = max 0 (overlapFinish - overlapStart)
                let oldChanged = oldSlice.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
                let newChanged = newSlice.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
                let expectedCount = if expectedLength > 0 then 1 else 0
                Check.equal expectedCount oldChanged.Length "The previous fallback slice has one clipped middle span when it overlaps the change."
                Check.equal expectedCount newChanged.Length "The current fallback slice has one clipped middle span when it overlaps the change."
                if expectedLength > 0 then
                    Check.equal (overlapStart - int offset) oldChanged[0].Start "The previous fallback span starts at the clipped middle."
                    Check.equal (overlapStart - int offset) newChanged[0].Start "The current fallback span starts at the clipped middle."
                    Check.equal expectedLength oldChanged[0].Length "The previous fallback span ends at the slice edge or changed middle."
                    Check.equal expectedLength newChanged[0].Length "The current fallback span ends at the slice edge or changed middle."
            do! session.Close()
            return ()
        }
        "long fallback slices match line reads", fun () -> async {
            let words (count: int) (choose: int -> string) = String.Join(" ", Array.init count choose)
            let limits = { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1e12; QuantumMs = 1e12 }
            let run previousText currentText check = async {
                let previous = [|
                    { Text = "head"; Ending = LineEnding.LF }
                    { Text = previousText; Ending = LineEnding.LF }
                    { Text = "tail"; Ending = LineEnding.NoEnding }
                |]
                let current = [|
                    { Text = "head"; Ending = LineEnding.LF }
                    { Text = currentText; Ending = LineEnding.LF }
                    { Text = "tail"; Ending = LineEnding.NoEnding }
                |]
                let! session = openSession (config 0 100 64 8 limits) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages = readAll session (fun () -> false)
                let row = rows (allParts pages) |> Array.find (fun value -> value.Kind = DiffRowKind.Replaced)
                let compare side (pageLine: DiffLine) = async {
                    let count = pageLine.Slice.Text.Length
                    let! initial = session.ReadLine(side, pageLine.Number, pageLine.Slice.OffsetUtf16, count, None, fun () -> false)
                    let! line = resolveLine session side pageLine.Number pageLine.Slice.OffsetUtf16 count initial
                    Check.equal pageLine.Slice.OffsetUtf16 line.Slice.OffsetUtf16 "The page and line read start at the same unit."
                    Check.equal pageLine.Slice.Text line.Slice.Text "The page and line read return the same text."
                    Check.sequence pageLine.Slice.Highlights line.Slice.Highlights "The page and line read return the same highlights."
                }
                do! compare DiffSide.Previous row.Previous.Value
                do! compare DiffSide.Current row.Current.Value
                check row
                do! session.Close()
            }

            let previousText = words 10_000 (fun index -> sprintf "w%06d" index)
            let changedText = words 10_000 (fun index -> if index = 5_000 || index = 5_400 then sprintf "X%06d" index else sprintf "w%06d" index)
            do! run previousText changedText ignore

            let insertionSourceText = words 6_000 (fun index -> sprintf "w%05d" index)
            let insertionStart = insertionSourceText.IndexOf("w03000", StringComparison.Ordinal)
            let insertedText = insertionSourceText.Insert(insertionStart, "NEW ")
            do!
                run insertionSourceText insertedText (fun row ->
                    let previousChanged = row.Previous.Value.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
                    let currentChanged = row.Current.Value.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
                    Check.equal 0 previousChanged.Length "An insertion leaves the previous line plain."
                    Check.true' (currentChanged.Length > 0) "The inserted text has a changed highlight."
                    for span in currentChanged do
                        let start = row.Current.Value.Slice.OffsetUtf16 + int64 span.Start
                        let finish = start + int64 span.Length
                        Check.true' (start >= int64 insertionStart && finish <= int64 insertionStart + 4L) "A changed span stays within the inserted text.")
            return ()
        }
        "request budgets preserve page highlights", fun () -> async {
            let previous =
                Array.init 3_000 (fun index ->
                    let text = if index % 10 = 5 then sprintf "a%d X b Y c" index else sprintf "line %d" index
                    { Text = text; Ending = LineEnding.LF })
            let current =
                Array.init 3_000 (fun index ->
                    let text = if index % 10 = 5 then sprintf "a%d Z b W c" index else sprintf "line %d" index
                    { Text = text; Ending = LineEnding.LF })
            let baselineLimits = { Limits.defaults with RequestMs = 1e12; QuantumMs = 1e12 }
            let sessionConfig = config 0 1_000 1_024 4_096 baselineLimits
            let run limits = async {
                let! session = openSession { sessionConfig with Limits = limits } (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages = readAll session (fun () -> false)
                do! session.Close()
                return pages
            }
            let! baseline = run baselineLimits
            let! limited = run { baselineLimits with MaxUnits = 3_000 }
            let baselineRows = rows (allParts baseline) |> Array.filter (fun row -> row.Kind = DiffRowKind.Replaced)
            let replacements = rows (allParts limited) |> Array.filter (fun row -> row.Kind = DiffRowKind.Replaced)
            Check.sequence baselineRows replacements "The request budget does not change replacement rows or slices."
            Check.equal 300 replacements.Length "Every changed source line forms a replacement."
            for row in replacements do
                let changedText line =
                    line.Slice.Highlights
                    |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
                    |> Array.map (fun span -> line.Slice.Text.Substring(span.Start, span.Length))
                Check.sequence [| "X"; "Y" |] (changedText row.Previous.Value) "The previous highlights leave the middle word plain."
                Check.sequence [| "Z"; "W" |] (changedText row.Current.Value) "The current highlights leave the middle word plain."
            return ()
        }
        "token highlight text limits apply to each side", fun () -> async {
            let previousText = "start old same " + String('p', 33_000) + " first end"
            let currentText = "start new same q second end"
            let previous = [| { Text = previousText; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = currentText; Ending = LineEnding.NoEnding } |]
            let limits = { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1e12; QuantumMs = 1e12 }
            let! session = openSession (config 0 100 64 8 limits) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! _ = readAll session (fun () -> false)
            let! initial = session.ReadLine(DiffSide.Previous, 0L, 0L, 8_192, None, fun () -> false)
            let! line = resolveLine session DiffSide.Previous 0L 0L 8_192 initial
            let changed = line.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
            let sharedStart = line.Slice.Text.IndexOf("same", StringComparison.Ordinal)
            Check.equal 1 changed.Length "A side above the text limit uses one bounded middle."
            Check.true' (changed[0].Start <= sharedStart && changed[0].Start + changed[0].Length >= sharedStart + 4) "The bounded middle covers text between the outer matches."
            do! session.Close()
            return ()
        }
        "unequal long lines highlight the unmatched suffix", fun () -> async {
            let previousText = String.replicate 40_000 "ab"
            let currentText = String.replicate 40_001 "ab"
            let previous = [| { Text = previousText; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = currentText; Ending = LineEnding.NoEnding } |]
            let limits = { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1e12; QuantumMs = 1e12 }
            let! session = openSession (config 0 100 64 8 limits) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! _ = readAll session (fun () -> false)
            let! initial = session.ReadLine(DiffSide.Current, 0L, int64 previousText.Length, 2, None, fun () -> false)
            let! line = resolveLine session DiffSide.Current 0L (int64 previousText.Length) 2 initial
            let changed = line.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
            Check.equal "ab" line.Slice.Text "The line read returns the unmatched suffix."
            Check.equal 1 changed.Length "The unmatched suffix has one changed span."
            Check.equal 0 changed[0].Start "The suffix highlight starts at the slice edge."
            Check.equal 2 changed[0].Length "The suffix highlight covers both units."
            do! session.Close()
            return ()
        }
        "line read sizing reserves encoded highlights", fun () -> async {
            let anchorCount = 317
            let previousCore = String.Join(" ", Array.init anchorCount (fun index -> sprintf "a%03x" index))
            let currentCore =
                String.Join(
                    " ",
                    Array.init anchorCount (fun index ->
                        let token = sprintf "a%03x" index
                        if index + 1 = anchorCount then token else token + ",")
                )
            let prefix = String('x', 8_192 - currentCore.Length - 1) + ";"
            let previousText = prefix + previousCore
            let currentText = prefix + currentCore
            let previous = [| { Text = previousText; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = currentText; Ending = LineEnding.NoEnding } |]
            let limits = { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1e12; QuantumMs = 1e12 }
            let! session = openSession (config 0 100 64 8 limits) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! _ = readAll session (fun () -> false)
            let! initial = session.ReadLine(DiffSide.Current, 0L, 0L, 8_192, None, fun () -> false)
            let! line = resolveLine session DiffSide.Current 0L 0L 8_192 initial
            let estimate = line.Slice.Text.Length * 3 + line.Slice.Highlights.Length * 64 + 512
            Check.true' (line.Slice.Highlights.Length > 0) "The line read keeps its inline highlights."
            Check.true' (line.Slice.Text.Length < currentText.Length) $"The line read returns {line.Slice.Text.Length} units and {line.Slice.Highlights.Length} highlights, with estimate {estimate}."
            Check.true' (estimate <= 64 * 1024) $"The conservative line estimate is {estimate} bytes."
            Check.true' (jsonLineBytes line <= 64 * 1024) $"The encoded line uses {jsonLineBytes line} bytes."
            do! session.Close()
            return ()
        }
        "line read retries preserve replacement highlights", fun () -> async {
            let prefix = String('a', 5_000) + " "
            let suffix = " " + String('b', 5_000)
            let previous = [| { Text = prefix + "old" + suffix; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = prefix + "new" + suffix; Ending = LineEnding.NoEnding } |]
            let limits = { Limits.defaults with MaxUnits = 8; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 100 64 8 limits) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! _ = readAll session (fun () -> false)
            let offset = int64 prefix.Length
            let! initial = session.ReadLine(DiffSide.Previous, 0L, offset, 64, None, fun () -> false)
            let mutable result = initial
            let mutable attempts = 0
            let mutable retriedHighlights = false
            while not retriedHighlights && attempts < 1_000 do
                match result with
                | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                    attempts <- attempts + 1
                    let! next = session.ReadLine(DiffSide.Previous, 0L, offset, 64, Some continuation, fun () -> false)
                    let! retry = session.ReadLine(DiffSide.Previous, 0L, offset, 64, Some continuation, fun () -> false)
                    Check.true' (Unchecked.equals next retry) "Retrying a line continuation returns the same highlight result."
                    match next with
                    | EngineResult.Ok(Resumable.Ready line) ->
                        let changed = line.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
                        Check.equal 1 changed.Length "The retried slice contains its changed word highlight."
                        Check.equal "old" (line.Slice.Text.Substring(changed[0].Start, changed[0].Length)) "The retried highlight selects the previous word."
                        retriedHighlights <- true
                    | _ -> result <- next
                | _ -> attempts <- 1_000
            Check.true' retriedHighlights "A continuation retry reaches the recorded changed highlight."
            do! session.Close()
            return ()
        }
        "context and ending line reads have no highlights", fun () -> async {
            let previous = [|
                { Text = "context"; Ending = LineEnding.LF }
                { Text = "old"; Ending = LineEnding.LF }
                { Text = "ending"; Ending = LineEnding.LF }
            |]
            let current = [|
                { Text = "context"; Ending = LineEnding.LF }
                { Text = "new"; Ending = LineEnding.LF }
                { Text = "ending"; Ending = LineEnding.CRLF }
            |]
            let! session = openSession (config 1 100 64 8 Limits.defaults) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let allRows = rows (allParts pages)
            Check.true' (allRows |> Array.exists (fun row -> row.Kind = DiffRowKind.Context && row.Previous.Value.Number = 0L)) "The unchanged line appears as context."
            Check.true' (allRows |> Array.exists (fun row -> row.Kind = DiffRowKind.EndingChanged && row.Previous.Value.Number = 2L)) "The final line appears as an ending change."
            for side in [| DiffSide.Previous; DiffSide.Current |] do
                for line in [| 0L; 2L |] do
                    let! initial = session.ReadLine(side, line, 0L, 32, None, fun () -> false)
                    let! value = resolveLine session side line 0L 32 initial
                    Check.equal 0 value.Slice.Highlights.Length "Context and ending-only reads have no highlights."
            do! session.Close()
            return ()
        }
        "line read highlights keep surrogate pairs intact", fun () -> async {
            let prefix = String.concat " " (Array.create 4_096 "a")
            let previous = [| { Text = prefix + "😀-tail"; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = prefix + "😁-tail"; Ending = LineEnding.NoEnding } |]
            let! session = openSession (config 0 100 64 8 Limits.defaults) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let row = rows (allParts pages) |> Array.find (fun row -> row.Kind = DiffRowKind.Replaced)
            Check.equal DiffRowKind.Replaced row.Kind "The surrogate pair belongs to a replacement row."
            for (side, expected) in [| DiffSide.Previous, "😀"; DiffSide.Current, "😁" |] do
                let! initial = session.ReadLine(side, 0L, int64 prefix.Length, 2, None, fun () -> false)
                let! value = resolveLine session side 0L (int64 prefix.Length) 2 initial
                Check.equal expected value.Slice.Text "The slice contains the complete changed surrogate pair."
                let changed = value.Slice.Highlights |> Array.filter (fun span -> span.Kind = HighlightKind.ChangedText)
                Check.equal 1 changed.Length "The changed emoji has one highlight span."
                Check.equal 0 changed[0].Start "The highlight starts at the slice edge."
                Check.equal 2 changed[0].Length "The highlight covers both surrogate units."
            do! session.Close()
            return ()
        }
        "ending changes have no inline highlights", fun () -> async {
            let previous = [| { Text = "same"; Ending = LineEnding.LF } |]
            let current = [| { Text = "same"; Ending = LineEnding.CRLF } |]
            let! session = openSession (config 0 100 64 8 Limits.defaults) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let row = rows (allParts pages) |> Array.find (fun row -> row.Kind = DiffRowKind.EndingChanged)
            Check.equal 0 row.Previous.Value.Slice.Highlights.Length "The previous ending-only line has no highlights."
            Check.equal 0 row.Current.Value.Slice.Highlights.Length "The current ending-only line has no highlights."
            do! session.Close()
            return ()
        }
        "page results rebuild through gap expansion and line slices", fun () -> async {
            let before = Array.init 4 (fun index -> { Text = "before-" + string index; Ending = LineEnding.LF })
            let after = Array.init 4 (fun index -> { Text = "after-" + string index; Ending = LineEnding.LF })
            let oldLong = String('a', 6_000) + "old" + String('z', 6_000)
            let newLong = String('a', 6_000) + "new" + String('z', 6_000)
            let previous = Array.concat [ before; [| { Text = oldLong; Ending = LineEnding.LF } |]; after ]
            let current = Array.concat [ before; [| { Text = newLong; Ending = LineEnding.LF } |]; after ]
            let! session = openSession (config 0 1_000 1_024 32 Limits.defaults) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            Check.true' (allParts pages |> Array.exists (function DiffPart.HiddenEqual _ -> true | _ -> false)) "The page stream has hidden ranges to expand."
            do! checkSliceOracle session previous current pages
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
                | EngineResult.Ok(Resumable.Scanning(_, value, Some preview)) ->
                    match preview.Previous, preview.Current with
                    | PendingSide.Snippet previous, PendingSide.Snippet current ->
                        Check.true' (previous.Text.Length > 0 && current.Text.Length > 0) "The pending lines carry decoded text."
                    | _ -> failwith "Both equal sources have an active pending line."
                    value
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
        "pending previews mark an early change and keep the equal tail plain", fun () -> async {
            let tail = String('x', 50_000)
            let previous = sourceSpec (Encoding.UTF8.GetBytes("a" + tail + "\n"))
            let current = sourceSpec (Encoding.UTF8.GetBytes("b" + tail + "\n"))
            let limits = { Limits.defaults with MaxUnits = 4; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 1_000 1_024 8 limits) previous current
            let! initial = session.FirstPage(fun () -> false)
            let! preview, _ = findPendingPreview session (fun value -> value.Mismatch.IsSome) initial
            match preview with
            | Some value ->
                match value.Previous, value.Current, value.Mismatch with
                | PendingSide.Snippet oldText, PendingSide.Snippet newText, Some marker ->
                    Check.equal 0L marker.PreviousOffsetUtf16 "The marker names the first changed unit."
                    Check.equal 0L marker.CurrentOffsetUtf16 "The marker uses the same offset on the current side."
                    Check.equal 'a' oldText.Text[0] "The previous snippet shows the old character."
                    Check.equal 'b' newText.Text[0] "The current snippet shows the new character."
                    Check.equal (oldText.Text.Substring(1)) (newText.Text.Substring(1)) "The shared tail has no other mismatch marker."
                | _ -> failwith "Both changed lines have snippets and a mismatch marker."
            | None -> failwith "The pending changed line has no preview."
            do! session.Close()
            return ()
        }
        "inserted line previews do not claim a paired change", fun () -> async {
            let giant = String('g', 50_000)
            let previous = sourceSpec (Encoding.UTF8.GetBytes("before\nend\n"))
            let currentBytes = Array.append [| 0xFFuy; 0xFEuy |] (Encoding.Unicode.GetBytes("before\n" + giant + "\nend\n"))
            let current = sourceSpecWith (MemoryByteSource(currentBytes) :> IByteSource) "utf-16le" 2 (int64 currentBytes.Length)
            let limits = { Limits.defaults with MaxUnits = 8; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 1_000 1_024 8 limits) previous current
            let! initial = session.FirstPage(fun () -> false)
            let predicate (value: PendingPreview) =
                match value.Previous, value.Current with
                | PendingSide.Snippet oldText, PendingSide.Snippet newText -> oldText.Line = 1L && newText.Line = 1L && value.Mismatch.IsNone
                | _ -> false
            let! preview, _ = findPendingPreview session predicate initial
            match preview with
            | Some value ->
                match value.Previous, value.Current with
                | PendingSide.Snippet oldText, PendingSide.Snippet newText ->
                    Check.true' (oldText.Text.Length > 0 && newText.Text.Length > 0) "Both sides show their current text."
                    Check.true' (oldText.End = SnippetEnd.LineEnd || oldText.End = SnippetEnd.EndOfFile) "The completed peer shows its end."
                    Check.true' (newText.End = SnippetEnd.MoreTextPending || newText.End = SnippetEnd.Truncated) "The inserted line remains pending."
                | _ -> failwith "The inserted line preview has both sides."
            | None -> failwith "The inserted line has no pending preview."
            do! session.Close()
            return ()
        }
        "pending previews follow a short change into a later long line", fun () -> async {
            let giant = String('a', 50_000)
            let previous = sourceSpec (Encoding.UTF8.GetBytes("old\n" + giant + "\n"))
            let current = sourceSpec (Encoding.UTF8.GetBytes("new\n" + giant + "\n"))
            let limits = { Limits.defaults with MaxUnits = 4; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 1_000 1_024 8 limits) previous current
            let! initial = session.FirstPage(fun () -> false)
            let predicate (value: PendingPreview) =
                match value.Previous, value.Current with
                | PendingSide.Snippet oldText, PendingSide.Snippet newText -> oldText.Line = 1L && newText.Line = 1L && value.Mismatch.IsNone
                | _ -> false
            let! preview, _ = findPendingPreview session predicate initial
            Check.true' preview.IsSome "The later long line has a preview."
            do! session.Close()
            return ()
        }
        "pending previews mark EOF after a complete unterminated line", fun () -> async {
            let longText = String('q', 50_000)
            let previous = sourceSpec (Encoding.UTF8.GetBytes("abc"))
            let current = sourceSpec (Encoding.UTF8.GetBytes("abc" + longText + "\n"))
            let limits = { Limits.defaults with MaxUnits = 4; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 1_000 1_024 8 limits) previous current
            let! initial = session.FirstPage(fun () -> false)
            let predicate (value: PendingPreview) =
                match value.Previous, value.Current with
                | PendingSide.Snippet oldText, PendingSide.Snippet newText -> oldText.Line = 0L && newText.Line = 0L && oldText.End = SnippetEnd.EndOfFile
                | _ -> false
            let! preview, _ = findPendingPreview session predicate initial
            match preview with
            | Some value ->
                match value.Previous, value.Current with
                | PendingSide.Snippet oldText, PendingSide.Snippet newText ->
                    Check.equal "abc" oldText.Text "The completed source keeps its whole line."
                    Check.equal SnippetEnd.EndOfFile oldText.End "EOF is shown after the line is proven."
                    Check.true' (newText.End = SnippetEnd.Truncated || newText.End = SnippetEnd.MoreTextPending) "The long peer remains visible while it is read."
                | _ -> failwith "Both sides have a pending preview."
            | None -> failwith "The incomplete peer has no preview."
            do! session.Close()
            return ()
        }
        "pending previews mark a completed line against a continuing peer", fun () -> async {
            let longText = String('q', 50_000)
            let previous = sourceSpec (Encoding.UTF8.GetBytes("abc\n"))
            let current = sourceSpec (Encoding.UTF8.GetBytes("abc" + longText + "\n"))
            let limits = { Limits.defaults with MaxUnits = 4; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 1_000 1_024 8 limits) previous current
            let! initial = session.FirstPage(fun () -> false)
            let predicate (value: PendingPreview) = value.Mismatch.IsSome
            let! preview, _ = findPendingPreview session predicate initial
            match preview with
            | Some value ->
                match value.Previous, value.Current, value.Mismatch with
                | PendingSide.Snippet oldText, PendingSide.Snippet _, Some marker ->
                    Check.equal SnippetEnd.LineEnd oldText.End "The completed peer reaches its line ending."
                    Check.equal 3L marker.PreviousOffsetUtf16 "The marker sits at the completed line end."
                    Check.equal 3L marker.CurrentOffsetUtf16 "The continuing side uses the same text offset."
                | _ -> failwith "The compared lines have snippets and an end marker."
            | None -> failwith "The completed and continuing lines have no preview."
            do! session.Close()
            return ()
        }
        "pending previews mark an empty completed line against a continuing peer", fun () -> async {
            let previousBytes = Encoding.UTF8.GetBytes("\n")
            let currentBytes = Encoding.UTF8.GetBytes(String('x', 50_000))
            let previous = sourceSpec previousBytes
            let current = sourceSpec currentBytes
            let limits = { Limits.defaults with MaxUnits = 4; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 32 16 8 limits) previous current
            let! initial = session.FirstPage(fun () -> false)
            let! preview, _ = findPendingPreview session (fun (value: PendingPreview) -> value.Mismatch.IsSome) initial
            match preview with
            | Some value ->
                match value.Previous, value.Current, value.Mismatch with
                | PendingSide.Snippet left, PendingSide.Snippet _, Some marker ->
                    Check.equal "" left.Text "The completed empty line stays an empty snippet."
                    Check.equal SnippetEnd.LineEnd left.End "The empty snippet reaches its line ending."
                    Check.equal 0L marker.PreviousOffsetUtf16 "The mismatch sits at the end of the empty line."
                | other -> failwith $"The empty completed line preview is {other}."
            | None -> failwith "The empty line and continuing peer have no preview."
            do! session.Close()
            return ()
        }
        "pending previews show an exhausted side beside a long line", fun () -> async {
            let giant = String('g', 50_000)
            let previous = sourceSpec (Encoding.UTF8.GetBytes("base\n"))
            let current = sourceSpec (Encoding.UTF8.GetBytes("base\n" + giant + "\n"))
            let limits = { Limits.defaults with MaxUnits = 4; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 1_000 1_024 8 limits) previous current
            let! initial = session.FirstPage(fun () -> false)
            let predicate (value: PendingPreview) =
                match value.Previous, value.Current with
                | PendingSide.Exhausted 1L, PendingSide.Snippet line -> line.Line = 1L
                | _ -> false
            let! preview, _ = findPendingPreview session predicate initial
            match preview with
            | Some value ->
                Check.equal (PendingSide.Exhausted 1L) value.Previous "The finished side is proven exhausted."
                match value.Current with
                | PendingSide.Snippet line -> Check.true' (line.Text.Length > 0) "The long line remains visible."
                | _ -> failwith "The long line has no snippet."
            | None -> failwith "The long line preview has no exhausted peer."
            do! session.Close()
            return ()
        }
        "pending previews keep a long completed peer truncated at a first-unit change", fun () -> async {
            let oldText = String('z', 5_000)
            let currentText = "y" + String('z', 4_999) + String('q', 50_000)
            let previous = sourceSpec (Encoding.UTF8.GetBytes(oldText + "\n"))
            let current = sourceSpec (Encoding.UTF8.GetBytes(currentText + "\n"))
            let limits = { Limits.defaults with MaxUnits = 4; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 1_000 1_024 8 limits) previous current
            let! initial = session.FirstPage(fun () -> false)
            let! preview, _ =
                findPendingPreview session
                    (fun (value: PendingPreview) ->
                        match value.Previous, value.Current with
                        | PendingSide.Snippet oldLine, PendingSide.Snippet _ -> value.Mismatch.IsSome && oldLine.End = SnippetEnd.Truncated
                        | _ -> false)
                    initial
            match preview with
            | Some value ->
                match value.Previous, value.Current, value.Mismatch with
                | PendingSide.Snippet oldLine, PendingSide.Snippet _, Some marker ->
                    Check.equal SnippetEnd.Truncated oldLine.End "The completed peer shows that text remains beyond the snippet."
                    Check.equal 0L marker.PreviousOffsetUtf16 "The marker stays at the first changed unit."
                | _ -> failwith "The long peer pair has a mismatch marker."
            | None -> failwith "The long completed peer has no preview."
            do! session.Close()
            return ()
        }
        "pending previews place long matching prefixes around the mismatch", fun () -> async {
            let oldText = String('z', 5_000)
            let changed = oldText.Substring(0, 3_000) + "y" + oldText.Substring(3_001)
            let currentText = changed + String('q', 50_000)
            let previous = sourceSpec (Encoding.UTF8.GetBytes(oldText + "\n"))
            let current = sourceSpec (Encoding.UTF8.GetBytes(currentText + "\n"))
            let limits = { Limits.defaults with MaxUnits = 4; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let! session = openSession (config 0 1_000 1_024 8 limits) previous current
            let! initial = session.FirstPage(fun () -> false)
            let! preview, _ =
                findPendingPreview session
                    (fun (value: PendingPreview) ->
                        match value.Previous, value.Current with
                        | PendingSide.Snippet oldLine, PendingSide.Snippet newLine -> oldLine.OffsetUtf16 = 2_744L && newLine.OffsetUtf16 = 2_744L && oldLine.Text.Length > 0 && newLine.Text.Length > 0 && value.Mismatch.IsSome
                        | _ -> false)
                    initial
            match preview with
            | Some value ->
                match value.Previous, value.Current, value.Mismatch with
                | PendingSide.Snippet oldLine, PendingSide.Snippet newLine, Some marker ->
                    Check.equal 3_000L marker.PreviousOffsetUtf16 "The marker keeps the mismatch offset within the line."
                    Check.equal 3_000L marker.CurrentOffsetUtf16 "Both snippets use the same line offset."
                    Check.true' (oldLine.Text.Length > 0 && newLine.Text.Length > 0) "Both placed snippets contain text."
                | _ -> failwith "The mismatching line pair has a marker."
            | None -> failwith "The later mismatch has no placed preview."
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
        "a cancellation during the journal commit does not append a second record on retry", fun () -> async {
            let previous = Array.init 40 (fun index -> { Text = "line-" + string index; Ending = LineEnding.LF })
            let current = Array.copy previous
            current[20] <- { Text = "edited"; Ending = LineEnding.LF }
            let previousSpec = sourceSpec (encodeLines previous)
            let currentSpec = sourceSpec (encodeLines current)
            let sessionConfig = defaultConfig ()
            let countedHost (appends: int ref) (onAppend: int -> unit) =
                let plain = Host.createInMemory (ManualClock 0.0 :> IClock)
                { plain with
                    CreateTempStore = fun name -> async {
                        let! created = plain.CreateTempStore name
                        if name <> sessionConfig.SessionId then return created
                        else
                            return
                                { new ITempStore with
                                    member _.Append buffer offset count = async {
                                        appends.Value <- appends.Value + 1
                                        onAppend appends.Value
                                        return! created.Append buffer offset count
                                    }
                                    member _.WriteAt position buffer offset count = created.WriteAt position buffer offset count
                                    member _.ReadAt position buffer offset count = created.ReadAt position buffer offset count
                                    member _.Length() = created.Length()
                                    member _.Dispose() = created.Dispose()
                                }
                    } }
            let controlAppends = ref 0
            let! control = TextDiffSession.create (countedHost controlAppends ignore) (Ledger()) sessionConfig (fun _ -> 1) previousSpec currentSpec
            let! _ = readAll control (fun () -> false)
            do! control.Close()
            let appends = ref 0
            use cts = new System.Threading.CancellationTokenSource()
            let! session = TextDiffSession.create (countedHost appends (fun count -> if count = 1 then cts.Cancel())) (Ledger()) sessionConfig (fun _ -> 1) previousSpec currentSpec
            let! _ =
                Async.FromContinuations(fun (resolve, reject, _) ->
                    Async.StartWithContinuations(session.FirstPage(fun () -> false), (fun _ -> resolve false), reject, (fun _ -> resolve true), cts.Token))
            let! pages = readAll session (fun () -> false)
            checkOracle previous current pages
            Check.equal controlAppends.Value appends.Value "A retry of the canceled request appends no second record."
            do! session.Close()
        }
        "a page returned by the session cannot change what a later replay returns", fun () -> async {
            let previous = Array.init 40 (fun index -> { Text = "line-" + string index; Ending = LineEnding.LF })
            let current = Array.copy previous
            current[20] <- { Text = "edited"; Ending = LineEnding.LF }
            let! session = openSession (defaultConfig ()) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! first = session.FirstPage(fun () -> false)
            let page = unwrap first |> function
                | Resumable.Ready value -> value
                | Resumable.Scanning _ -> failwith "The first request did not return a page."
            Check.true' (page.Parts.Length > 1) "The page has several parts to change."
            let original = Array.copy page.Parts
            page.Parts[0] <- page.Parts[1]
            let! replayed = session.ReplayPage page.PageId
            let replay = unwrap replayed
            Check.true' (Unchecked.equals original replay.Parts) "The replay returns the parts the session produced."
            replay.Parts[0] <- replay.Parts[1]
            let! again = session.ReplayPage page.PageId
            Check.true' (Unchecked.equals original (unwrap again).Parts) "A changed replay does not change a later replay."
            do! session.Close()
        }
        "growing sources produce a partial page with a pending preview", fun () -> async {
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
            Check.true' (page.NextCursor.IsSome && page.Pending.IsSome) $"The partial page carries its pending preview. Cursor: {page.NextCursor}; preview: {page.Pending}."
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
        "late seek paths validate UTF-16 content beyond a checkpoint", fun () -> async {
            let line = String('A', 5_000)
            let content = String.concat "\n" (Array.create 40 line) + "\n"
            let utf16 = Array.append [| 0xFFuy; 0xFEuy |] (Encoding.Unicode.GetBytes content)
            let signature = [| 0x89uy; 0x48uy; 0x44uy; 0x46uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]
            Array.blit signature 0 utf16 131_072 signature.Length
            let byteSource () = MemoryByteSource(utf16) :> IByteSource
            let previous = sourceSpecWith (byteSource ()) "utf-16le" 2 (int64 utf16.Length)
            let current = sourceSpecWith (byteSource ()) "utf-16le" 2 (int64 utf16.Length)
            let limits = { Limits.defaults with MaxUnits = 1; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let sessionConfig = { config 0 1_000 4_096 128 limits with CheckpointIntervalBytes = 16_384.0 }
            let! session = openSession sessionConfig previous current
            let! initial = session.FirstPage(fun () -> false)
            let mutable result = initial
            let mutable progress = 0L
            let mutable attempts = 0
            while progress < 40_000L && attempts < 20 do
                attempts <- attempts + 1
                match result with
                | EngineResult.Ok(Resumable.Scanning(value, continuation, _)) ->
                    progress <- value.ValidatedBytes
                    if progress < 180_000L then
                        let! next = session.ReadPage continuation (fun () -> false)
                        result <- next
                | other -> failwith $"The equal UTF-16 sources returned {other} before the seek."
            Check.true' (progress >= 40_000L && progress < 131_072L) "The session pauses after an earlier checkpoint and before the signature."
            let! seek = session.SeekOffset(DiffSide.Previous, 180_000L)
            match seek with
            | EngineResult.Failed(code, _, Some detail) ->
                Check.equal TextDiffFailureCodes.ContentNotText code "The seek reports late content evidence."
                Check.equal DiffSide.Previous detail.Side "The seek identifies the source that contains the signature."
                Check.true' (detail.Evidence.Contains("131072", StringComparison.Ordinal)) "The evidence names the signature offset."
            | other -> failwith $"The seek returned {other}."
            do! session.Close()
            return ()
        }
        "line checkpoints keep seeks near the end within one interval", fun () -> async {
            let lines = Array.init 40_000 (fun _ -> { Text = "x"; Ending = LineEnding.LF })
            let bytes = encodeLines lines
            let previousSource = CountingByteSource bytes
            let currentSource = CountingByteSource bytes
            let previous = sourceSpecWith (previousSource :> IByteSource) "utf-8" 0 (int64 bytes.Length)
            let current = sourceSpecWith (currentSource :> IByteSource) "utf-8" 0 (int64 bytes.Length)
            let sessionConfig = { config 0 1_000 4_096 512 Limits.defaults with CheckpointIntervalBytes = 32_768.0 }
            let! session = openSession sessionConfig previous current
            let! _ = readAll session (fun () -> false)
            previousSource.Reset()
            let! seek = session.SeekLine(DiffSide.Previous, int64 lines.Length - 1L)
            match seek with
            | EngineResult.Ok(Some line) ->
                Check.equal 1L line.Utf16Length "The seek found the final line."
                Check.equal (int64 bytes.Length) line.EndOffset "The final line reaches the source end."
            | other -> failwith $"The late line seek returned {other}."
            Check.true' (previousSource.BytesRead <= 32_770L) $"The seek read {previousSource.BytesRead} bytes after the counter reset."
            do! session.Close()
            return ()
        }
        "source info learns a line count when a seek reaches eof", fun () -> async {
            let lines = Array.init 200 (fun index -> { Text = string index + String('x', 96); Ending = LineEnding.LF })
            let bytes = encodeLines lines
            let previous = sourceSpec bytes
            let current = sourceSpec bytes
            let limits = { Limits.defaults with MaxUnits = 1; RequestMs = 1_000_000.0; QuantumMs = 1_000_000.0 }
            let sessionConfig = { config 0 1_000 4_096 128 limits with CheckpointIntervalBytes = 16_384.0 }
            let! session = openSession sessionConfig previous current
            let! initial = session.FirstPage(fun () -> false)
            match initial with
            | EngineResult.Ok(Resumable.Scanning _) -> ()
            | other -> failwith $"The first scan completed before the source line count was known: {other}."
            let initialInfo, _ = session.SourceInfo
            Check.equal None initialInfo.LineCount "Source info has no line count before the source reaches eof."
            let! seek = session.SeekLine(DiffSide.Previous, int64 lines.Length + 10L)
            Check.equal (EngineResult.Ok None) seek "The seek ends past the last line."
            let previousInfo, _ = session.SourceInfo
            Check.equal (Some(int64 lines.Length)) previousInfo.LineCount "Source info reports the count learned by the seek."
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
        "long changed lines locate edits beyond the first slice", fun () -> async {
            let prefix = String('p', 90_000)
            let suffix = String('s', 10_000)
            let previous = [| { Text = prefix + "old" + suffix; Ending = LineEnding.NoEnding } |]
            let current = [| { Text = prefix + "new" + suffix; Ending = LineEnding.NoEnding } |]
            let! session = openSession (config 0 100 64 8 Limits.defaults) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let row = rows (allParts pages) |> Array.find (fun row -> row.Kind = DiffRowKind.Replaced)
            Check.equal 89_872L row.Previous.Value.Slice.OffsetUtf16 "The first slice starts before the late changed word."
            Check.true' (row.Previous.Value.Slice.Text.Contains("old", StringComparison.Ordinal)) "The first slice displays the previous changed word."
            Check.true' (row.Current.Value.Slice.Text.Contains("new", StringComparison.Ordinal)) "The first slice displays the current changed word."
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
                match parts[index] with
                | DiffPart.Hunk { Body = HunkBody.UnalignedSides(previousLines, currentLines) } ->
                    for line in Array.append previousLines currentLines do
                        Check.equal 0 line.Slice.Highlights.Length "An unaligned line has no inline highlights."
                | _ -> failwith "The selected edit region is aligned."
                Check.true' (parts |> Array.skip (index + 1) |> Array.exists (function DiffPart.Hunk { Body = HunkBody.AlignedRows values } -> values |> Array.exists (fun row -> row.Kind = DiffRowKind.Replaced) | _ -> false)) "A later edit remains aligned after the unaligned region."
            do! session.Close()
            return ()
        }
        "generated large edits rebuild both complete sources", fun () -> async {
            let random = RandomState 0x1A26E5u

            for caseIndex in 0 .. 59 do
                let previous, current = largeEdit random caseIndex
                let sessionConfig = { config 2 10_000 256 16 Limits.defaults with WindowMaxBytes = 4 * 1024 }
                let! session = openSession sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages = readAll session (fun () -> false)
                checkOracle previous current pages
                do! session.Close()

            return ()
        }
        "generated large edits rebuild both complete sources across forced spills", fun () -> async {
            let random = RandomState 0x1A26E5u
            let limits = { Limits.defaults with MaxUnits = 256 }
            let mutable totalSpills = 0

            for caseIndex in 0 .. 29 do
                let previous, current = largeEdit random caseIndex
                let sessionConfig = { config 2 10_000 256 16 limits with WindowMaxBytes = 4 * 1024 }
                let! session = openSession sessionConfig (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
                let! pages, spills = readAllSpilling session
                totalSpills <- totalSpills + spills
                checkOracle previous current pages
                do! session.Close()

            Check.true' (totalSpills > 0) "The sessions were spilled between requests."
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
        "journal entries stay readable after many page and gap keys", fun () -> async {
            let lines changed =
                Array.init 400 (fun index ->
                    let text =
                        if changed && index % 10 = 5 then "changed line " + string index + " with some words"
                        else "line " + string index + " with some words"
                    { Text = text; Ending = LineEnding.LF })
            let sessionConfig = { defaultConfig () with PageMaxFragments = 1 }
            let! session = openSession sessionConfig (sourceSpec (encodeLines (lines false))) (sourceSpec (encodeLines (lines true)))
            let! pages = readAll session (fun () -> false)
            Check.true' (pages.Length > 13) "The session records more than thirteen pages."
            let gaps = allParts pages |> Array.choose (function DiffPart.HiddenEqual gap -> Some gap | _ -> None)
            Check.true' (gaps.Length > 12) "The session records gap keys above sequence twelve."
            for gap in gaps do
                let! initial = session.Expand(gap.GapId, true, 100, None, fun () -> false)
                let! _ = resolveExpansion session gap.GapId true 100 initial
                ()
            do! session.Close()
            return ()
        }
        "line reads do not corrupt later page records", fun () -> async {
            let lines changed =
                Array.init 400 (fun index ->
                    let text =
                        if changed && index % 10 = 5 then "changed line " + string index + " with some words"
                        else "line " + string index + " with some words"
                    { Text = text; Ending = LineEnding.LF })
            let sessionConfig = { defaultConfig () with PageMaxFragments = 1 }
            let! session = openSession sessionConfig (sourceSpec (encodeLines (lines false))) (sourceSpec (encodeLines (lines true)))
            let! initial = session.FirstPage(fun () -> false)
            let! first = resolvePage session (fun () -> false) initial
            let mutable page = first
            for line in 0L .. 39L do
                let! result = session.ReadLine(DiffSide.Current, line, 0L, 128, None, fun () -> false)
                let! _ = resolveLine session DiffSide.Current line 0L 128 result
                ()
            let mutable pages = 1
            while page.NextCursor.IsSome do
                let! next = session.ReadPage page.NextCursor.Value (fun () -> false)
                let! ready = resolvePage session (fun () -> false) next
                page <- ready
                pages <- pages + 1
            Check.true' (pages > 13) "The page sequence remains readable through the end."
            do! session.Close()
            return ()
        }
        "slow source reads still advance scanner chunks", fun () -> async {
            let previousLines = Array.init 180 (fun index -> { Text = "line " + string index; Ending = LineEnding.LF })
            let currentLines = Array.copy previousLines
            currentLines[90] <- { currentLines[90] with Text = "changed line ninety" }
            let previousBytes = encodeLines previousLines
            let currentBytes = encodeLines currentLines
            let normalConfig = { defaultConfig () with Limits = { Limits.defaults with RequestMs = 1e12; QuantumMs = 1_000_000.0 } }
            let! normal = openSession normalConfig (sourceSpec previousBytes) (sourceSpec currentBytes)
            let! normalPages = readAll normal (fun () -> false)
            let rowShape pages =
                allParts pages
                |> Array.collect (function
                    | DiffPart.Hunk { Body = HunkBody.AlignedRows values } -> values |> Array.map (fun row -> row.Kind, row.Previous |> Option.map (fun line -> line.Slice.Text), row.Current |> Option.map (fun line -> line.Slice.Text))
                    | _ -> Array.empty)
            let normalRows = rowShape normalPages
            do! normal.Close()
            for delay in [| 11.0; 50.0 |] do
                let clock = ManualClock 0.0
                let host = Host.createInMemory (clock :> IClock)
                let sessionConfig = { defaultConfig () with Limits = { Limits.defaults with RequestMs = 1e12; QuantumMs = 10.0 } }
                let previous = { sourceSpec previousBytes with Source = Some(DelayedByteSource(previousBytes, clock, delay) :> IByteSource) }
                let current = { sourceSpec currentBytes with Source = Some(DelayedByteSource(currentBytes, clock, delay) :> IByteSource) }
                let! session = TextDiffSession.create host (Ledger()) sessionConfig (fun _ -> 1) previous current
                let! pages = readAll session (fun () -> false)
                Check.sequence normalRows (rowShape pages) "Slow reads preserve the normal diff rows."
                do! session.Close()
            return ()
        }
        "control evidence follows complete windows on both sides", fun () -> async {
            let buildControlText tag addEsc =
                let builder = StringBuilder()
                for index in 0 .. 999 do builder.Append(sprintf "common line %04d with some padding text\n" index) |> ignore
                for index in 0 .. 519 do
                    builder.Append(sprintf "%s%s line %05d padding padding padding pad\n" (if addEsc then "\u001b" else " ") tag index) |> ignore
                for index in 0 .. 299 do builder.Append(sprintf "tail line %04d plain text here\n" index) |> ignore
                Encoding.UTF8.GetBytes(builder.ToString())
            let previous = buildControlText "old" false
            let current = buildControlText "new" true
            let! session = openSession (defaultConfig ()) (sourceSpec previous) (sourceSpec current)
            let! pages = readAll session (fun () -> false)
            Check.true' (pages.Length > 0) "Sparse control characters pass through the session."
            do! session.Close()

            let plain prefix index = (sprintf "%s %06d plain text line padding........" prefix index).Substring(0, 39) + "\n"
            let text = StringBuilder()
            let mutable number = 0
            while text.Length < 131_072 - 650 do text.Append(plain "p" number) |> ignore; number <- number + 1
            while text.Length < 131_072 + 650 do text.Append(String('\u0001', 49) + "\n") |> ignore
            while text.Length < 220_000 do text.Append(plain "q" number) |> ignore; number <- number + 1
            let previousText = text.ToString()
            let inserted = String.concat "" [ for index in 0 .. 818 -> plain "ins" index ]
            let currentText = inserted + previousText
            let! deltaSession = openSession (defaultConfig ()) (sourceSpec (Encoding.UTF8.GetBytes previousText)) (sourceSpec (Encoding.UTF8.GetBytes currentText))
            let rec findFailure (session: TextDiffSession) result = async {
                match result with
                | EngineResult.Failed(code, _, detail) -> return Some(code, detail)
                | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                    let! next = session.ReadPage continuation (fun () -> false)
                    return! findFailure session next
                | EngineResult.Ok(Resumable.Ready page) ->
                    match page.NextCursor with
                    | Some cursor ->
                        let! next = session.ReadPage cursor (fun () -> false)
                        return! findFailure session next
                    | None -> return None
                | EngineResult.Canceled -> return None
            }
            let! deltaStart = deltaSession.FirstPage(fun () -> false)
            let! deltaFailure = findFailure deltaSession deltaStart
            match deltaFailure with
            | Some(TextDiffFailureCodes.ContentNotText, Some detail) ->
                Check.equal DiffSide.Current detail.Side "The shifted current side reports its evidence."
                let marker = detail.Evidence.LastIndexOf("byte ", StringComparison.Ordinal)
                let offset = Int64.Parse(detail.Evidence.Substring(marker + 5))
                Check.true' (offset >= 131_072L && offset < 196_608L) "The current side reports the window that contains its controls."
            | other -> failwith $"The shifted control window did not fail on the current side: {other}."
            do! deltaSession.Close()

            let wholeHasEvidence (bytes: byte[]) =
                let state = Scanner.create TextEncoding.Utf8 0L None
                let evidence = ResizeArray<ScannerEvidence>()
                let meter = Meter.create (ManualClock 0.0 :> IClock) { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1e15; QuantumMs = 1e15 }
                Scanner.scanChunk state bytes 0 bytes.Length true meter (LineBatch(4)) ignore evidence.Add |> ignore
                evidence |> Seq.exists (fun item -> item.Kind = "control ratio")
            for perMille in [| 0; 5; 12; 25 |] do
                let random = RandomState(uint32 (100 + perMille))
                let bytes = Array.init 131_072 (fun _ -> if random.Next(1_000) < perMille then 1uy else 0x61uy)
                let expected = wholeHasEvidence bytes
                let! densitySession = openSession (defaultConfig ()) (sourceSpec bytes) (sourceSpec bytes)
                let! first = densitySession.FirstPage(fun () -> false)
                let! failure = findFailure densitySession first
                Check.equal expected failure.IsSome $"Density {perMille} agrees with the whole-file scan."
                do! densitySession.Close()
            return ()
        }
        "a short final window joins the previous control window", fun () -> async {
            let body first =
                let builder = StringBuilder(first + "\n")
                let mutable index = 0
                while builder.Length < 65_575 do
                    builder.Append(sprintf "line %05d of plain text in a text file\n" index) |> ignore
                    index <- index + 1
                Array.append (Encoding.UTF8.GetBytes(builder.ToString().Substring(0, 65_575))) [| 0x1Auy |]
            let previous = body "first line A"
            let current = body "first line B"
            let! same = openSession (defaultConfig ()) (sourceSpec previous) (sourceSpec previous)
            let! samePages = readAll same (fun () -> false)
            Check.true' (samePages.Length > 0) "An identical file with a short tail is accepted."
            do! same.Close()
            let! changed = openSession (defaultConfig ()) (sourceSpec previous) (sourceSpec current)
            let! pages = readAll changed (fun () -> false)
            let changedRows =
                allParts pages
                |> Array.collect (function
                    | DiffPart.Hunk { Body = HunkBody.AlignedRows values } -> values |> Array.filter (fun row -> row.Kind <> DiffRowKind.Context)
                    | _ -> Array.empty)
            Check.true' (changedRows.Length > 0) "A one-line change with the same short tail is accepted."
            do! changed.Close()
            return ()
        }
        "a page commit failure poisons later requests", fun () -> async {
            let previous = Array.init 40 (fun index -> { Text = "line " + string index; Ending = LineEnding.LF })
            let current = Array.copy previous
            current[20] <- { current[20] with Text = "changed line twenty" }
            let sessionConfig = { defaultConfig () with PageMaxFragments = 1 }
            let host, pairWrites, _ = faultHost sessionConfig.SessionId
            pairWrites.Value <- true
            let! session = TextDiffSession.create host (Ledger()) sessionConfig (fun _ -> 1) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! initial = session.FirstPage(fun () -> false)
            let rec resolveResult result = async {
                match result with
                | EngineResult.Ok(Resumable.Scanning(_, continuation, _)) ->
                    let! next = session.ReadPage continuation (fun () -> false)
                    return! resolveResult next
                | other -> return other
            }
            let! failed = resolveResult initial
            match failed with
            | EngineResult.Failed(code, _, _) -> Check.equal TextDiffFailureCodes.WorkerFailed code "The failed page commit returns worker_failed."
            | other -> failwith $"The injected page write did not fail: {other}."
            let! later = session.FirstPage(fun () -> false)
            match later with
            | EngineResult.Failed(code, _, _) -> Check.equal TextDiffFailureCodes.WorkerFailed code "A later page request sees the failed session."
            | other -> failwith $"The failed session accepted another page request: {other}."
            let holder = session :> IScratchHolder
            Check.true' (not holder.MustKeepScratch) "A failed session does not keep its scratch."
            do! holder.Spill()
            Check.true' (not holder.HoldsScratch) "A failed session releases its scratch when asked to spill."
            do! session.Close()
            return ()
        }
        "an expansion commit failure poisons later requests", fun () -> async {
            let previous = Array.init 80 (fun index -> { Text = "line " + string index; Ending = LineEnding.LF })
            let current = Array.copy previous
            current[40] <- { current[40] with Text = "changed line forty" }
            let sessionConfig = { defaultConfig () with PageMaxFragments = 1 }
            let host, _, indexWrites = faultHost sessionConfig.SessionId
            let! session = TextDiffSession.create host (Ledger()) sessionConfig (fun _ -> 1) (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! initial = session.FirstPage(fun () -> false)
            let! page = resolvePage session (fun () -> false) initial
            let gap = page.Parts |> Array.pick (function DiffPart.HiddenEqual value -> Some value | _ -> None)
            indexWrites.Value <- true
            let! expansion = session.Expand(gap.GapId, true, 2, None, fun () -> false)
            match expansion with
            | EngineResult.Failed(code, _, _) -> Check.equal TextDiffFailureCodes.WorkerFailed code "The failed expansion commit returns worker_failed."
            | other -> failwith $"The injected expansion link did not fail: {other}."
            let! later = session.FirstPage(fun () -> false)
            match later with
            | EngineResult.Failed(code, _, _) -> Check.equal TextDiffFailureCodes.WorkerFailed code "A later request sees the failed expansion session."
            | other -> failwith $"The failed session accepted another request: {other}."
            do! session.Close()
            return ()
        }
        "page sizing bounds source reads", fun () -> async {
            let makeLine ending index =
                let suffix = string index
                { Text = String('"', 8_000 - suffix.Length) + suffix; Ending = ending }
            let previous = Array.init 300 (makeLine LineEnding.LF)
            let current = Array.init 300 (makeLine LineEnding.CRLF)
            let previousBytes = encodeLines previous
            let currentBytes = encodeLines current
            let previousSource = CountingByteSource previousBytes
            let currentSource = CountingByteSource currentBytes
            let previousSpec = sourceSpecWith (previousSource :> IByteSource) "utf-8" 0 (int64 previousBytes.Length)
            let currentSpec = sourceSpecWith (currentSource :> IByteSource) "utf-8" 0 (int64 currentBytes.Length)
            let limits = { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1e12; QuantumMs = 1e12 }
            let sessionConfig = config 0 1_000 65_536 4_096 limits
            let size part = jsonPartBytes part + 1
            let host = Host.createInMemory (ManualClock 0.0 :> IClock)
            let! session = TextDiffSession.create host (Ledger()) sessionConfig size previousSpec currentSpec
            let! firstResult = session.FirstPage(fun () -> false)
            let! first = resolvePage session (fun () -> false) firstResult
            let pageBytes (page: DiffPage) = page.Parts |> Array.sumBy size
            let firstBytes = pageBytes first
            Check.true' (firstBytes <= sessionConfig.PageMaxBytes) "The first page stays within its byte limit."
            let mutable emittedBytes = int64 firstBytes
            let mutable emittedRows = first.Parts |> Array.sumBy countRows
            let mutable page = first
            let mutable pageCount = 1
            while page.NextCursor.IsSome do
                let! result = session.ReadPage page.NextCursor.Value (fun () -> false)
                let! next = resolvePage session (fun () -> false) result
                let bytes = pageBytes next
                Check.true' (bytes <= sessionConfig.PageMaxBytes) $"Page {pageCount + 1} uses {bytes} bytes."
                emittedBytes <- emittedBytes + int64 bytes
                emittedRows <- emittedRows + (next.Parts |> Array.sumBy countRows)
                page <- next
                pageCount <- pageCount + 1
            let bytesRead = previousSource.BytesRead + currentSource.BytesRead
            let readLimit = emittedBytes * 3L / 2L + 131_072L
            Check.true' (pageCount > 1) "The source spans several bounded pages."
            Check.equal 300 emittedRows "Every ending change appears in the page stream."
            Check.true' (emittedBytes > 0L) "The pages emit source text."
            Check.true' (bytesRead <= readLimit) $"The sources read {bytesRead} bytes for {emittedBytes} emitted bytes."
            do! session.Close()
            return ()
        }
        "trimmed from-end expansions assign distinct row identifiers", fun () -> async {
            let previous = Array.init 60 (fun index -> { Text = String('a', 8_000) + string index; Ending = LineEnding.LF })
            let current = Array.copy previous
            current[30] <- { current[30] with Text = String('b', 8_000) + "30" }
            let sessionConfig = {
                defaultConfig () with
                    ContextLines = 0
                    PageMaxBytes = 40_000
                    PageMaxRows = 1_000
                    Limits = { Limits.defaults with MaxUnits = Int32.MaxValue; RequestMs = 1e12; QuantumMs = 1e12 }
            }
            let rowSize (row: DiffRow) =
                let lineSize = function Some line -> line.Slice.Text.Length + 64 | None -> 0
                lineSize row.Previous + lineSize row.Current + 64
            let size (part: DiffPart) =
                match part with
                | DiffPart.ExpandedContext(_, values) -> 64 + (values |> Array.sumBy rowSize)
                | DiffPart.Hunk fragment ->
                    match fragment.Body with
                    | HunkBody.AlignedRows values -> 128 + (values |> Array.sumBy rowSize)
                    | HunkBody.UnalignedSides(previousLines, currentLines) -> 128 + (Array.append previousLines currentLines |> Array.sumBy (fun line -> line.Slice.Text.Length + 64))
                | _ -> 128
            let host = Host.createInMemory (ManualClock 0.0 :> IClock)
            let! session = TextDiffSession.create host (Ledger()) sessionConfig size (sourceSpec (encodeLines previous)) (sourceSpec (encodeLines current))
            let! pages = readAll session (fun () -> false)
            let ids = ResizeArray<string>()
            let collect parts =
                for part in parts do
                    match part with
                    | DiffPart.ExpandedContext(_, values) -> for row in values do ids.Add row.Id
                    | DiffPart.Hunk { Body = HunkBody.AlignedRows values } -> for row in values do ids.Add row.Id
                    | _ -> ()
            for page in pages do collect page.Parts
            let mutable gap =
                allParts pages
                |> Array.pick (function DiffPart.HiddenEqual value when value.PreviousRange.Count > 10L -> Some value | _ -> None)
                |> Some
            let mutable expansions = 0
            while gap.IsSome && expansions < 6 do
                let value = gap.Value
                let! initial = session.Expand(value.GapId, false, 10, None, fun () -> false)
                let! parts = resolveExpansion session value.GapId false 10 initial
                collect parts
                gap <- parts |> Array.tryPick (function DiffPart.HiddenEqual residual -> Some residual | _ -> None)
                expansions <- expansions + 1
            let duplicates = ids |> Seq.countBy id |> Seq.filter (fun (_, count) -> count > 1) |> Seq.toArray
            Check.equal 0 duplicates.Length "Rows from trimmed expansions keep unique identifiers."
            do! session.Close()
            return ()
        }
        "pending mismatch offsets stay on the differing character", fun () -> async {
            let prefix = String.replicate 10 "a\n"
            let body = String('x', 20_000_000)
            let previous = Encoding.UTF8.GetBytes(prefix + body.Substring(0, 50) + "P" + body.Substring(51) + "\nend\n")
            let current = Encoding.UTF8.GetBytes(prefix + body.Substring(0, 50) + "C" + body.Substring(51) + "\nend\n")
            let limits = { Limits.defaults with MaxUnits = 500; RequestMs = 1e12; QuantumMs = 1e12 }
            let sessionConfig = { defaultConfig () with ContextLines = 0; Limits = limits }
            let! session = openSession sessionConfig (sourceSpec previous) (sourceSpec current)
            let! first = session.FirstPage(fun () -> false)
            let! previewValue, _ = findPendingPreview session (fun value -> value.Mismatch.IsSome) first
            let preview = previewValue.Value
            let mismatch = preview.Mismatch.Value
            Check.equal 50L mismatch.PreviousOffsetUtf16 "The previous marker names the differing character."
            Check.equal 50L mismatch.CurrentOffsetUtf16 "The current marker names the differing character."
            do! session.Close()
            return ()
        }
        "pending previews keep paired line numbers after an insertion", fun () -> async {
            let prefix = String.replicate 5 "a\n"
            let between = String.replicate 5 "a\n"
            let body = String('x', 20_000_000)
            let previous = Encoding.UTF8.GetBytes(prefix + between + body.Substring(0, 50) + "P" + body.Substring(51) + "\nend\n")
            let current = Encoding.UTF8.GetBytes(prefix + "inserted\n" + between + body.Substring(0, 50) + "C" + body.Substring(51) + "\nend\n")
            let limits = { Limits.defaults with MaxUnits = 500; RequestMs = 1e12; QuantumMs = 1e12 }
            let sessionConfig = { defaultConfig () with ContextLines = 0; PageMaxRows = 1; PageMaxFragments = 1; Limits = limits }
            let! session = openSession sessionConfig (sourceSpec previous) (sourceSpec current)
            let! first = session.FirstPage(fun () -> false)
            let! previewValue, _ =
                findPendingPreview session
                    (fun value ->
                        match value.Previous, value.Current with
                        | PendingSide.Snippet oldLine, PendingSide.Snippet newLine -> oldLine.Line = 10L && newLine.Line = 11L
                        | _ -> false)
                    first
            let preview = previewValue.Value
            match preview.Previous, preview.Current with
            | PendingSide.Snippet oldLine, PendingSide.Snippet newLine ->
                Check.equal 10L oldLine.Line "The preview keeps the previous line number."
                Check.equal 11L newLine.Line "The preview maps the inserted current line."
            | other -> failwith $"The mismatch preview omitted a paired line: {other}."
            do! session.Close()
            return ()
        }
    ]
