namespace VersionControlService.TextDiff

open System
open VersionControlService.Abstractions

type internal ReadStep =
    | ReadData of count: int * endOfSource: bool
    | ReadWaiting
    | ReadWindowFull
    | ReadFinished
    | ReadChanged

[<RequireQualifiedAccess>]
type internal CompareStep =
    | Continue
    | Waiting
    | Finished of matched: int
    | Changed

module internal Widths =
    let unitBytes (encoding: TextEncoding) =
        match encoding with
        | TextEncoding.Utf16LE
        | TextEncoding.Utf16BE -> 2
        | TextEncoding.Utf32LE
        | TextEncoding.Utf32BE -> 4
        | _ -> 1

    let isLittleEndian (encoding: TextEncoding) =
        match encoding with
        | TextEncoding.Utf16LE
        | TextEncoding.Utf32LE -> true
        | _ -> false

    let endingWidth (encoding: TextEncoding) (code: int) =
        let width = float (unitBytes encoding)
        if code = LineEndingCode.CRLF then width * 2.0
        elif code = LineEndingCode.LF || code = LineEndingCode.CR then width
        else 0.0

    /// Reads the code unit that starts at index. UTF-32 units above 24 bits are irrelevant because only
    /// the values of CR and LF are compared.
    let unitAt (encoding: TextEncoding) (data: byte[]) (index: int) =
        match unitBytes encoding with
        | 1 -> Native.readByte data index
        | 2 ->
            if isLittleEndian encoding then Native.readByte data index ||| (Native.readByte data (index + 1) <<< 8)
            else (Native.readByte data index <<< 8) ||| Native.readByte data (index + 1)
        | _ ->
            if isLittleEndian encoding then
                Native.readByte data index
                ||| (Native.readByte data (index + 1) <<< 8)
                ||| (Native.readByte data (index + 2) <<< 16)
                ||| (Native.readByte data (index + 3) <<< 24)
            else
                (Native.readByte data index <<< 24)
                ||| (Native.readByte data (index + 1) <<< 16)
                ||| (Native.readByte data (index + 2) <<< 8)
                ||| Native.readByte data (index + 3)

type internal ScanSide(
    spec: SourceSpec,
    encoding: TextEncoding,
    category: AllocationCategory,
    side: DiffSide,
    ledger: Ledger,
    hashMask: (uint32 * uint32) option,
    reportEvidence: DiffSide -> string -> int64 -> unit,
    checkpointIntervalBytes: float,
    recordLineCount: int64 -> unit
) =
    let table = LineTable(category, ledger, hashMask)
    let mutable scanner = Scanner.create encoding (int64 spec.BomLength) None
    let mutable finished = spec.Source.IsNone
    let mutable coverage = float spec.BomLength
    let mutable windowStart = float spec.BomLength
    let mutable windowLineLimit = Int32.MaxValue
    let mutable windowByteLimit = Int32.MaxValue
    let mutable windowAccountedBytes = 0.0
    let mutable recordPeaks = true
    let mutable observer: ScannerState -> float -> unit = fun _ _ -> ()
    let mutable lineObserver: float -> float -> float = fun _ _ -> checkpointIntervalBytes
    let mutable hasLineObserver = false
    let mutable lineCheckpointDue = checkpointIntervalBytes

    member _.Spec = spec
    member _.Encoding = encoding
    member _.Side = side
    member _.Table = table
    member _.Scanner = scanner
    member _.Finished = finished
    member _.Coverage = coverage

    member _.SetLimits(maxLines: int, maxBytes: int) =
        windowLineLimit <- max 1 maxLines
        windowByteLimit <- max 1 maxBytes

    member _.BeginWindow(firstLine: int64, maxLines: int, maxBytes: int) =
        windowLineLimit <- max 1 maxLines
        windowByteLimit <- max 1 maxBytes
        windowStart <- if scanner.LineLengthUtf16 > 0.0 then scanner.LineStart else scanner.NextOffset
        windowAccountedBytes <- 0.0
        table.BeginWindow firstLine

    member _.WindowFull =
        table.Count >= windowLineLimit
        || windowAccountedBytes >= float windowByteLimit
           && scanner.LineLengthUtf16 = 0.0
           && not scanner.PendingCR
           && scanner.PendingCount = 0
           && scanner.PendingHigh = 0

    member _.ByteFull = windowAccountedBytes >= float windowByteLimit

    /// Continues with the same scanner after the caller consumed every line of the window.
    member this.Advance(firstLine: int64, maxLines: int, maxBytes: int) =
        table.ReleaseWindow()
        this.BeginWindow(firstLine, maxLines, maxBytes)

    /// Restarts scanning at a line start.
    member _.SetCursor(offset: float, firstLine: int64, maxLines: int, maxBytes: int) =
        table.ReleaseWindow()
        scanner <- Scanner.create encoding (int64 offset) None
        finished <- spec.Source.IsNone
        windowLineLimit <- max 1 maxLines
        windowByteLimit <- max 1 maxBytes
        windowStart <- offset
        windowAccountedBytes <- 0.0
        table.BeginWindow firstLine

    member _.ReadInto(buffer: byte[], maximum: int) = async {
        if finished then return ReadFinished
        elif table.Count >= windowLineLimit then return ReadWindowFull
        else
            match spec.Source with
            | None ->
                finished <- true
                return ReadFinished
            | Some source ->
                let position = int64 scanner.NextOffset
                if position > spec.ByteLength then return ReadChanged
                elif position = spec.ByteLength then
                    if source.IsComplete() then return ReadData(0, true)
                    else return ReadWaiting
                else
                    let remaining = spec.ByteLength - position
                    let consumedWindowBytes = int (scanner.NextOffset - windowStart)
                    let rawRoom = max 1 (windowByteLimit - consumedWindowBytes)
                    let readLimit =
                        if consumedWindowBytes >= windowByteLimit && scanner.LineLengthUtf16 > 0.0 then maximum
                        else min maximum rawRoom
                    let boundary = (Math.Floor(scanner.NextOffset / checkpointIntervalBytes) + 1.0) * checkpointIntervalBytes
                    let boundaryRoom = max 1.0 (boundary - scanner.NextOffset)
                    let count = min buffer.Length (int (min boundaryRoom (min (float readLimit) (float remaining))))
                    let! outcome = source.ReadAt position buffer 0 count
                    match outcome with
                    | ReadOutcome.NotYetAvailable -> return ReadWaiting
                    | ReadOutcome.EndOfSource -> return if source.IsComplete() then ReadChanged else ReadWaiting
                    | ReadOutcome.Bytes actual when actual = 0 ->
                        return if source.IsComplete() && position < spec.ByteLength then ReadChanged else ReadWaiting
                    | ReadOutcome.Bytes actual -> return ReadData(actual, position + int64 actual >= spec.ByteLength && source.IsComplete())
    }

    member _.Consume(buffer: byte[], count: int, endOfSource: bool, meter: Meter) =
        let emitLineBatch batch = table.AppendBatch batch
        let emitEvidence (evidence: ScannerEvidence) = reportEvidence side evidence.Kind evidence.Offset
        let remainingLines = max 1 (windowLineLimit - table.Count)
        let batch = LineBatch(max 1 (min 4_096 remainingLines))
        batch.StopAtFull <- true
        let previousCount = table.Count
        let result = Scanner.scanChunk scanner buffer 0 count endOfSource meter batch emitLineBatch emitEvidence
        for index = previousCount to table.Count - 1 do
            windowAccountedBytes <- windowAccountedBytes + table.Finish index - table.Start index + table.Length index * 2.0 + 40.0
        if hasLineObserver && table.Count > previousCount then
            let last = table.Count - 1
            let offset = table.Finish last
            if offset >= lineCheckpointDue then
                lineCheckpointDue <- lineObserver offset (float table.LineBase + float table.Count)
        if recordPeaks then ledger.RecordWindowLines(category, table.Count)
        observer scanner (float table.LineBase + float table.Count)
        coverage <- max coverage (float scanner.StartOffset + scanner.ValidatedBytes)
        match result.Error with
        | Some error -> reportEvidence side ("invalid " + Decoders.name encoding + " sequence: " + error.Reason) error.Offset
        | None -> ()
        if result.Status = EndOfInput then
            finished <- true
            recordLineCount (table.LineBase + int64 table.Count)
        result

    member _.SetCoverage(value: float) = coverage <- max coverage value

    /// False for helper sides whose window sizes do not count toward the session peaks.
    member _.RecordPeaks with get () = recordPeaks and set value = recordPeaks <- value

    /// Called after every consumed chunk with the scanner and the number of lines that ended before its position.
    member _.Observer with get () = observer and set value = observer <- value

    member _.LineObserver with get () = lineObserver and set value = lineObserver <- value; hasLineObserver <- true

    /// Writes the scan position and the window counters. The line arrays are written by the caller.
    member _.Export(header: HeaderBuilder) =
        header.Bool finished
        header.Number coverage
        header.Number windowStart
        header.Int windowLineLimit
        header.Int windowByteLimit
        header.Number windowAccountedBytes
        header.Number(float table.LineBase)
        header.Int table.Count
        ScannerStateCodec.write header scanner

    /// Reads what Export wrote and returns the number of window lines that the caller has to restore.
    member _.Import(header: HeaderReader) =
        finished <- header.Bool()
        coverage <- header.Number()
        windowStart <- header.Number()
        windowLineLimit <- header.Int()
        windowByteLimit <- header.Int()
        windowAccountedBytes <- header.Number()
        let firstLine = header.Number()
        let lines = header.Int()
        scanner <- ScannerStateCodec.read encoding header
        struct (int64 firstLine, lines)

    member _.Dispose() = table.Dispose()

/// Confirms a claimed run of equal lines against the source bytes when both sources share an encoding.
/// Consecutive pairs with equal endings and equal byte spans are compared as one byte range.
type internal RunCompare(previous: ScanSide, current: ScanSide, previousIndex: int, currentIndex: int, count: int, bufferA: byte[], bufferB: byte[]) =
    let previousTable = previous.Table
    let currentTable = current.Table
    let encoding = previous.Encoding
    let mutable pair = 0
    let mutable groupOpen = false
    let mutable groupEnd = 0
    let mutable wholeGroup = false
    let mutable groupBytes = 0.0
    let mutable groupPreviousStart = 0.0
    let mutable groupCurrentStart = 0.0
    let mutable doneBytes = 0.0

    let span (table: LineTable) index = table.Finish index - table.Start index

    let sameShape offset =
        previousTable.EndingCode(previousIndex + offset) = currentTable.EndingCode(currentIndex + offset)
        && span previousTable (previousIndex + offset) = span currentTable (currentIndex + offset)

    let readOnce (source: IByteSource) (position: float) (buffer: byte[]) (want: int) =
        source.ReadAt (int64 position) buffer 0 want

    member _.Step(meter: Meter) : Async<CompareStep> = async {
        if pair >= count then return CompareStep.Finished count
        elif not groupOpen then
            Meter.charge meter 1
            let endingP = previousTable.EndingCode(previousIndex + pair)
            let endingC = currentTable.EndingCode(currentIndex + pair)
            groupPreviousStart <- previousTable.Start(previousIndex + pair)
            groupCurrentStart <- currentTable.Start(currentIndex + pair)
            doneBytes <- 0.0
            if sameShape pair then
                let mutable next = pair + 1
                while next < count && sameShape next do
                    next <- next + 1
                Meter.charge meter ((next - pair) / 64)
                groupEnd <- next
                wholeGroup <- true
                groupBytes <- previousTable.Finish(previousIndex + next - 1) - groupPreviousStart
                groupOpen <- true
                return CompareStep.Continue
            else
                let contentP = span previousTable (previousIndex + pair) - Widths.endingWidth encoding endingP
                let contentC = span currentTable (currentIndex + pair) - Widths.endingWidth encoding endingC
                if contentP <> contentC then return CompareStep.Finished pair
                else
                    groupEnd <- pair + 1
                    wholeGroup <- false
                    groupBytes <- contentP
                    groupOpen <- true
                    return CompareStep.Continue
        else
            let remaining = groupBytes - doneBytes
            if remaining <= 0.0 then
                pair <- groupEnd
                groupOpen <- false
                return CompareStep.Continue
            else
                let want = int (min remaining (float (min 65_536 (min bufferA.Length bufferB.Length))))
                let! first = readOnce previous.Spec.Source.Value (groupPreviousStart + doneBytes) bufferA want
                let! second = readOnce current.Spec.Source.Value (groupCurrentStart + doneBytes) bufferB want
                match first, second with
                | ReadOutcome.NotYetAvailable, _
                | _, ReadOutcome.NotYetAvailable -> return CompareStep.Waiting
                | ReadOutcome.EndOfSource, _
                | _, ReadOutcome.EndOfSource -> return CompareStep.Changed
                | ReadOutcome.Bytes a, ReadOutcome.Bytes b ->
                    let got = min a b
                    if got <= 0 then return CompareStep.Waiting
                    else
                        Meter.chargeBytes meter got
                        if Native.bytesEqual bufferA 0 bufferB 0 got then
                            doneBytes <- doneBytes + float got
                            return CompareStep.Continue
                        else
                            let mutable index = 0
                            while Native.readByte bufferA index = Native.readByte bufferB index do
                                index <- index + 1
                            let mismatchAt = doneBytes + float index
                            if not wholeGroup then return CompareStep.Finished pair
                            else
                                let mutable matched = pair
                                while matched < groupEnd
                                      && previousTable.Finish(previousIndex + matched) - groupPreviousStart <= mismatchAt do
                                    matched <- matched + 1
                                Meter.charge meter ((matched - pair) / 64)
                                return CompareStep.Finished matched
    }

/// Decodes one side of a line pair in bounded chunks so that lines of different encodings can be compared.
type internal DecodeFeed(side: ScanSide, buffer: byte[]) =
    let units = Array.zeroCreate<int> 4_104
    let mutable decoder = Decoders.createAt side.Encoding 0L
    let mutable head = 0
    let mutable filled = 0
    let mutable position = 0.0
    let mutable finish = 0.0

    member _.Begin(start: float, contentEnd: float) =
        decoder <- Decoders.createAt side.Encoding (int64 start)
        position <- start
        finish <- contentEnd
        head <- 0
        filled <- 0

    member _.Available = filled - head
    member _.Exhausted = position >= finish
    member _.Peek(offset: int) = units[head + offset]
    member _.Consume(count: int) = head <- head + count

    /// Reads and decodes the next chunk. It returns 1 after decoding data, 0 when the source has no data yet
    /// and -1 when the source ended early.
    member _.Fill(meter: Meter) = async {
        head <- 0
        filled <- 0
        let want = int (min 4_096.0 (min (finish - position) (float buffer.Length)))
        let! outcome = side.Spec.Source.Value.ReadAt (int64 position) buffer 0 want
        match outcome with
        | ReadOutcome.Bytes got when got > 0 ->
            let next, _ =
                Decoders.decode decoder buffer 0 got (fun _ _ value ->
                    units[filled] <- value
                    filled <- filled + 1)
            decoder <- next
            position <- position + float got
            Meter.chargeBytes meter got
            return 1
        | ReadOutcome.EndOfSource -> return -1
        | _ -> return 0
    }

/// Confirms a claimed run of equal lines when the two sources use different encodings, by comparing the
/// decoded UTF-16 units of each line pair.
type internal DecodeCompare(previous: ScanSide, current: ScanSide, previousIndex: int, currentIndex: int, count: int, bufferA: byte[], bufferB: byte[]) =
    let feedA = DecodeFeed(previous, bufferA)
    let feedB = DecodeFeed(current, bufferB)
    let mutable pair = 0
    let mutable open' = false

    member _.Step(meter: Meter) : Async<CompareStep> = async {
        if pair >= count then return CompareStep.Finished count
        elif not open' then
            Meter.charge meter 1
            let pt = previous.Table
            let ct = current.Table
            let startP = pt.Start(previousIndex + pair)
            let startC = ct.Start(currentIndex + pair)
            feedA.Begin(startP, pt.Finish(previousIndex + pair) - Widths.endingWidth previous.Encoding (pt.EndingCode(previousIndex + pair)))
            feedB.Begin(startC, ct.Finish(currentIndex + pair) - Widths.endingWidth current.Encoding (ct.EndingCode(currentIndex + pair)))
            open' <- true
            return CompareStep.Continue
        elif feedA.Available = 0 && not feedA.Exhausted then
            let! filled = feedA.Fill meter
            return if filled > 0 then CompareStep.Continue elif filled = 0 then CompareStep.Waiting else CompareStep.Changed
        elif feedB.Available = 0 && not feedB.Exhausted then
            let! filled = feedB.Fill meter
            return if filled > 0 then CompareStep.Continue elif filled = 0 then CompareStep.Waiting else CompareStep.Changed
        elif feedA.Available = 0 && feedB.Available = 0 then
            pair <- pair + 1
            open' <- false
            return CompareStep.Continue
        elif feedA.Available = 0 || feedB.Available = 0 then return CompareStep.Finished pair
        else
            let n = min feedA.Available feedB.Available
            let mutable index = 0
            while index < n && feedA.Peek index = feedB.Peek index do
                index <- index + 1
            Meter.charge meter (index / 256)
            if index < n then return CompareStep.Finished pair
            else
                feedA.Consume n
                feedB.Consume n
                return CompareStep.Continue
    }

module internal InlineHighlights =
    [<Literal>]
    let private MaxTokens = 4_096

    [<Literal>]
    let private MaxTextUnits = 64 * 1024

    type private TokenTable = {
        Starts: int[]
        Lengths: int[]
        Count: int
        Overflow: bool
    }

    let private isBoundary (value: char) = Char.IsWhiteSpace value || Char.IsPunctuation value

    let private tokenCapacity (text: string) =
        min MaxTokens (max 1 text.Length)

    let private tokenize (text: string) =
        let starts = Array.zeroCreate<int> (tokenCapacity text)
        let lengths = Array.zeroCreate<int> starts.Length
        let mutable count = 0
        let mutable index = 0
        let mutable overflow = false

        let addToken start length =
            if count = MaxTokens then
                overflow <- true
            else
                starts[count] <- start
                lengths[count] <- length
                count <- count + 1

        while index < text.Length && not overflow do
            let start = index
            let value = text[index]
            if Char.IsWhiteSpace value then
                index <- index + 1
                while index < text.Length && Char.IsWhiteSpace text[index] do
                    index <- index + 1
                addToken start (index - start)
            elif Char.IsPunctuation value then
                index <- index + 1
                addToken start 1
            else
                index <- index + 1
                while index < text.Length && not (isBoundary text[index]) do
                    index <- index + 1
                addToken start (index - start)

        { Starts = starts; Lengths = lengths; Count = count; Overflow = overflow }

    let private tokenEqual (previous: string) (current: string) (previousTokens: TokenTable) previousIndex (currentTokens: TokenTable) currentIndex =
        let leftStart = previousTokens.Starts[previousIndex]
        let rightStart = currentTokens.Starts[currentIndex]
        let leftLength = previousTokens.Lengths[previousIndex]
        let rightLength = currentTokens.Lengths[currentIndex]
        if leftLength <> rightLength then false
        else
            let mutable index = 0
            let mutable equal = true
            while equal && index < leftLength do
                equal <- previous[leftStart + index] = current[rightStart + index]
                index <- index + 1
            equal

    let private appendSpan (target: ResizeArray<Highlight>) start length kind =
        if length > 0 then
            if target.Count > 0 then
                let previous = target[target.Count - 1]
                if previous.Kind = kind && previous.Start + previous.Length = start then
                    target[target.Count - 1] <- { previous with Length = previous.Length + length }
                else
                    target.Add { Start = start; Length = length; Kind = kind }
            else
                target.Add { Start = start; Length = length; Kind = kind }

    let private appendTokenSpan (target: ResizeArray<Highlight>) (tokens: TokenTable) index kind =
        appendSpan target tokens.Starts[index] tokens.Lengths[index] kind

    let private commonPrefix (previous: string) (current: string) =
        let limit = min previous.Length current.Length
        let mutable count = 0
        while count < limit && previous[count] = current[count] do
            count <- count + 1
        if count > 0 && count < previous.Length && Char.IsHighSurrogate previous[count - 1] && Char.IsLowSurrogate previous[count] then
            count - 1
        else
            count

    let private commonSuffix (previous: string) (current: string) prefix =
        let limit = min previous.Length current.Length
        let mutable count = 0
        while count < limit - prefix && previous[previous.Length - count - 1] = current[current.Length - count - 1] do
            count <- count + 1
        let previousStart = previous.Length - count
        let currentStart = current.Length - count
        if
            count > 0
            && previousStart > prefix
            && currentStart > prefix
            && Char.IsLowSurrogate previous[previousStart]
            && Char.IsHighSurrogate previous[previousStart - 1]
            && Char.IsLowSurrogate current[currentStart]
            && Char.IsHighSurrogate current[currentStart - 1]
        then
            count - 1
        else
            count

    let middleBounds previousLength currentLength prefix suffix =
        let suffix = min suffix (max 0 (min (previousLength - prefix) (currentLength - prefix)))
        let previousChanged = previousLength - prefix - suffix
        let currentChanged = currentLength - prefix - suffix
        let previousResult = ResizeArray<Highlight>(3)
        let currentResult = ResizeArray<Highlight>(3)
        appendSpan previousResult 0 prefix HighlightKind.UnchangedText
        appendSpan currentResult 0 prefix HighlightKind.UnchangedText
        appendSpan previousResult prefix previousChanged HighlightKind.ChangedText
        appendSpan currentResult prefix currentChanged HighlightKind.ChangedText
        appendSpan previousResult (prefix + previousChanged) suffix HighlightKind.UnchangedText
        appendSpan currentResult (prefix + currentChanged) suffix HighlightKind.UnchangedText
        previousResult.ToArray(), currentResult.ToArray()

    let private middleHighlights (previous: string) (current: string) =
        let prefix = commonPrefix previous current
        let suffix = commonSuffix previous current prefix
        middleBounds previous.Length current.Length prefix suffix

    let clipSlice (offset: int64) (sliceText: string) (highlights: Highlight[]) =
        let first = max 0 (int offset)
        let result = ResizeArray<Highlight>()
        for span in highlights do
            let mutable start = max 0 (span.Start - first)
            let mutable end' = min sliceText.Length (span.Start + span.Length - first)
            if start < end' then
                if start > 0 && start < sliceText.Length
                   && Char.IsLowSurrogate sliceText[start]
                   && Char.IsHighSurrogate sliceText[start - 1] then
                    start <- start - 1
                if end' < sliceText.Length && end' > 0
                   && Char.IsHighSurrogate sliceText[end' - 1]
                   && Char.IsLowSurrogate sliceText[end'] then
                    end' <- end' + 1
                appendSpan result start (end' - start) span.Kind
        result.ToArray()

    let clip (_wholeText: string) (offset: int64) (sliceText: string) (highlights: Highlight[]) =
        clipSlice offset sliceText highlights

    let middleSlice (total: float) (prefix: float) (suffix: float) (offset: int64) (sliceText: string) =
        let suffix = min suffix (max 0.0 (total - prefix))
        let changedEnd = max prefix (total - suffix)
        let sliceStart = float offset
        let sliceEnd = sliceStart + float sliceText.Length
        let result = ResizeArray<Highlight>(3)
        let appendInterval start finish kind =
            let clippedStart = max sliceStart start
            let clippedEnd = min sliceEnd finish
            if clippedStart < clippedEnd then
                appendSpan result (int (clippedStart - sliceStart)) (int (clippedEnd - clippedStart)) kind
        appendInterval 0.0 prefix HighlightKind.UnchangedText
        appendInterval prefix changedEnd HighlightKind.ChangedText
        appendInterval changedEnd total HighlightKind.UnchangedText
        result.ToArray()

    let private tokenHighlights (previous: string) (current: string) (meter: Meter) (previousTokens: TokenTable) (currentTokens: TokenTable) =
        let remaining = max 0 (meter.MaxUnits - meter.Units)
        if remaining = 0 || Meter.overBudget meter then
            None
        else
            let stepper =
                MyersStepper(
                    0,
                    previousTokens.Count,
                    0,
                    currentTokens.Count,
                    remaining,
                    meter,
                    fun previousIndex currentIndex -> tokenEqual previous current previousTokens previousIndex currentTokens currentIndex)
            let mutable result = None
            let mutable running = true
            while running do
                match stepper.Step() with
                | MyersStepResult.Complete operations ->
                    result <- Some operations
                    running <- false
                | MyersStepResult.StepLimitExceeded
                | MyersStepResult.NeedComparison _ ->
                    running <- false
                | MyersStepResult.Running ->
                    if Meter.overBudget meter then running <- false
            result

    let private highlightsFromOperations
        (previous: string)
        (current: string)
        (previousTokens: TokenTable)
        (currentTokens: TokenTable)
        (operations: MyersOperation[])
        =
        let previousResult = ResizeArray<Highlight>()
        let currentResult = ResizeArray<Highlight>()
        for operation in operations do
            match operation with
            | MyersOperation.Equal(previousIndex, currentIndex) ->
                appendTokenSpan previousResult previousTokens previousIndex HighlightKind.UnchangedText
                appendTokenSpan currentResult currentTokens currentIndex HighlightKind.UnchangedText
            | MyersOperation.Delete previousIndex ->
                appendTokenSpan previousResult previousTokens previousIndex HighlightKind.ChangedText
            | MyersOperation.Insert currentIndex ->
                appendTokenSpan currentResult currentTokens currentIndex HighlightKind.ChangedText
        previousResult.ToArray(), currentResult.ToArray()

    let compute (meter: Meter) (previous: string) (current: string) : Highlight[] * Highlight[] =
        if isNull previous then nullArg (nameof previous)
        if isNull current then nullArg (nameof current)
        elif previous.Length > MaxTextUnits || current.Length > MaxTextUnits || previous.Length + current.Length > MaxTextUnits then
            middleHighlights previous current
        else
            let previousTokens = tokenize previous
            let currentTokens = tokenize current
            if previousTokens.Overflow || currentTokens.Overflow then
                middleHighlights previous current
            else
                match tokenHighlights previous current meter previousTokens currentTokens with
                | Some operations -> highlightsFromOperations previous current previousTokens currentTokens operations
                | None -> middleHighlights previous current
