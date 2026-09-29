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
    reportEvidence: DiffSide -> string -> int64 -> unit
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

    member _.Spec = spec
    member _.Encoding = encoding
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
                    let count = min buffer.Length (int (min (int64 readLimit) remaining))
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
        if recordPeaks then ledger.RecordWindowLines(category, table.Count)
        observer scanner (float table.LineBase + float table.Count)
        coverage <- max coverage (float scanner.StartOffset + scanner.ValidatedBytes)
        match result.Error with
        | Some error -> reportEvidence side ("invalid " + Decoders.name encoding + " sequence: " + error.Reason) error.Offset
        | None -> ()
        if result.Status = EndOfInput then finished <- true
        result

    member _.SetCoverage(value: float) = coverage <- max coverage value

    /// False for helper sides whose window sizes do not count toward the session peaks.
    member _.RecordPeaks with get () = recordPeaks and set value = recordPeaks <- value

    /// Called after every consumed chunk with the scanner and the number of lines that ended before its position.
    member _.Observer with get () = observer and set value = observer <- value

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
