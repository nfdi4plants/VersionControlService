namespace VersionControlService.TextDiff

open System
open VersionControlService.Abstractions

[<RequireQualifiedAccess>]
type private Mode =
    | Common
    | Window
    | Done

[<RequireQualifiedAccess>]
type private WindowState =
    | Loading
    | Aligning
    | Comparing
    | Feeding

type private ReadStep =
    | ReadData of count: int * endOfSource: bool
    | ReadWaiting
    | ReadWindowFull
    | ReadFinished
    | ReadChanged

[<RequireQualifiedAccess>]
type private CompareStep =
    | Continue
    | Waiting
    | Finished of matched: int
    | Changed

module private Widths =
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

type private ScanSide(
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
        ledger.RecordWindowLines(category, table.Count)
        coverage <- max coverage (float scanner.StartOffset + scanner.ValidatedBytes)
        match result.Error with
        | Some error -> reportEvidence side ("invalid " + Decoders.name encoding + " sequence: " + error.Reason) error.Offset
        | None -> ()
        if result.Status = EndOfInput then finished <- true
        result

    member _.SetCoverage(value: float) = coverage <- max coverage value

    member _.Dispose() = table.Dispose()

/// Confirms a claimed run of equal lines against the source bytes when both sources share an encoding.
/// Consecutive pairs with equal endings and equal byte spans are compared as one byte range.
type private RunCompare(previous: ScanSide, current: ScanSide, previousIndex: int, currentIndex: int, count: int, bufferA: byte[], bufferB: byte[]) =
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
type private DecodeFeed(side: ScanSide, buffer: byte[]) =
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
type private DecodeCompare(previous: ScanSide, current: ScanSide, previousIndex: int, currentIndex: int, count: int, bufferA: byte[], bufferB: byte[]) =
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

[<RequireQualifiedAccess>]
type private PageBuild =
    | Built of page: DiffPage * fullCount: int * partialConsumed: int * rowsRemoved: int * fragmentsRemoved: int * nextRow: int64
    | TooLarge
    | Interrupted

type private FragmentBuild = { Part: DiffPart; Taken: int; Size: int }

/// Streams a diff of two byte sources. Phase one compares equal bytes with CommonRun and skips whole runs of
/// equal lines. Around every difference, a window of line records per side is aligned, and the resulting
/// operations feed a HunkBuilder that queues page content. Row text is read from the sources by offset when a
/// page is built, so no line text is kept.
type TextDiffSession internal (
    host: EngineHost,
    ledger: Ledger,
    config: SessionConfig,
    sizer: PartSizer,
    previousSpec: SourceSpec,
    currentSpec: SourceSpec,
    previousEncoding: TextEncoding,
    currentEncoding: TextEncoding,
    store: ITempStore
) =
    let bufferCap = 4 * 1024 * 1024
    let chunkLimit = max 1 (min CommonRun.MaxRunBytes config.CommonChunkBytes)
    let scanChunkLimit = max 1 (min chunkLimit config.WindowMaxBytes)
    let initialWindowLines = max 1 (min config.WindowMaxLines 512)
    let context = max 0 config.ContextLines
    let unitWidth = Widths.unitBytes previousEncoding
    let canStartCommon =
        previousEncoding = currentEncoding
        && previousSpec.Source.IsSome
        && currentSpec.Source.IsSome
        && previousSpec.BomLength = currentSpec.BomLength
    let previousLength = float previousSpec.ByteLength
    let currentLength = float currentSpec.ByteLength
    let totalBytes = previousSpec.ByteLength + currentSpec.ByteLength

    let mutable invalidDetail: DiffContentBlocked option = None
    let mutable failure: (string * string) option = None
    let mutable firstPageReturned = false
    let mutable closed = false
    let mutable closing = false
    let mutable busy = false
    let mutable requestSequence = 0L
    let mutable rowSequence = 0L
    let mutable mode = if canStartCommon then Mode.Common else Mode.Window
    let mutable windowState = WindowState.Loading
    let mutable waited = false
    let mutable bufA = Array.empty<byte>
    let mutable bufB = Array.empty<byte>
    let mutable bufferLease: IDisposable option = None

    // Equal-byte phase. The positions below belong to the previous side. The current side sits at the same
    // position plus delta, which is nonzero after an insertion or deletion.
    let mutable pos = float (max previousSpec.BomLength currentSpec.BomLength)
    let mutable delta = 0.0
    let mutable lineStart = pos
    let mutable commonStart = pos
    let mutable commonLines = 0.0
    let mutable pendingCR = false
    let mutable chunk = min 4_096 chunkLimit
    let mutable pendingWindow: ObservationWindow option = None
    let mutable backActive = false
    let mutable backEnd = 0.0
    let mutable backFound = 0
    let mutable backTarget = 0.0

    // Window phase.
    let mutable windowLimit = initialWindowLines
    let mutable aligner: WindowAligner option = None
    let mutable compareJob: (Meter -> Async<CompareStep>) option = None
    let mutable ops = Array.empty<DiffOperation>
    let mutable commitCount = 0
    let mutable opIndex = 0
    let mutable feedPrevious = 0
    let mutable feedCurrent = 0
    let mutable regionStarted = false
    let mutable pBase = 0.0
    let mutable cBase = 0.0
    let mutable partialAlign = false
    let mutable partialPrevious = -1
    let mutable partialCurrent = -1

    // Text decoding scratch for page building.
    let textUnits = Array.zeroCreate<uint16> 8_192
    let mutable textCount = 0

    let builder = HunkBuilder(config.ContextLines, config.PageMaxRows)
    let journal = Journal(store)

    let report (side: DiffSide) (kind: string) (offset: int64) =
        if invalidDetail.IsNone then
            let evidence =
                if kind = "nul" then "NUL character at byte " + string offset
                else kind + " at byte " + string offset
            invalidDetail <- Some { Side = side; Evidence = evidence }

    let previousSide =
        ScanSide(previousSpec, previousEncoding, AllocationCategory.PreviousWindows, DiffSide.Previous, ledger, config.HashMaskForTesting, report)

    let currentSide =
        ScanSide(currentSpec, currentEncoding, AllocationCategory.CurrentWindows, DiffSide.Current, ledger, config.HashMaskForTesting, report)

    do
        previousSide.BeginWindow(0L, windowLimit, config.WindowMaxBytes)
        currentSide.BeginWindow(0L, windowLimit, config.WindowMaxBytes)

    let progress () = {
        ValidatedBytes = int64 (min (float totalBytes) (previousSide.Coverage + currentSide.Coverage))
        TotalBytes = totalBytes
        ScanComplete = mode = Mode.Done
    }

    let identifier (kind: string) (sequence: int64) =
        let prefix = config.SessionId + ":" + kind + ":" + string sequence
        let hash = Hash.create ()
        for character in prefix do
            Hash.addCodeUnit hash (int character)
        prefix + ":" + Hash.toString hash

    let readIdentifier (expectedKind: string) (value: string) =
        if isNull value then None
        else
            let last = value.LastIndexOf ':'
            if last <= 0 then None
            else
                let withoutChecksum = value.Substring(0, last)
                let previousSeparator = withoutChecksum.LastIndexOf ':'
                if previousSeparator <= 0 then None
                else
                    let sequenceText = withoutChecksum.Substring(previousSeparator + 1)
                    let kindSeparator = withoutChecksum.LastIndexOf(':', previousSeparator - 1)
                    if kindSeparator <= 0 then None
                    else
                        let sessionId = withoutChecksum.Substring(0, kindSeparator)
                        let kind = withoutChecksum.Substring(kindSeparator + 1, previousSeparator - kindSeparator - 1)
                        let mutable sequence = 0L
                        if sessionId <> config.SessionId || kind <> expectedKind || not (Int64.TryParse(sequenceText, &sequence)) then None
                        elif identifier kind sequence <> value then None
                        else Some sequence

    let failContent () =
        match invalidDetail with
        | Some detail -> EngineResult.Failed(TextDiffFailureCodes.ContentNotText, detail.Evidence, Some detail)
        | None -> EngineResult.Failed(TextDiffFailureCodes.WorkerFailed, "The diff could not read its source.", None)

    let failClosed () = EngineResult.Failed(TextDiffFailureCodes.SessionClosed, "The diff session is closed.", None)

    let failMismatch () =
        EngineResult.Failed(TextDiffFailureCodes.ContinuationMismatch, "The cursor does not belong to the next request for this session.", None)

    let failWorker message = EngineResult.Failed(TextDiffFailureCodes.WorkerFailed, message, None)

    let failNotImplemented (operation: string) = EngineResult.Failed("not_implemented", operation + " is not implemented.", None)

    let extensionResult operation =
        if closed || closing then failClosed ()
        elif invalidDetail.IsSome then failContent ()
        else failNotImplemented operation

    let sourceChanged () =
        if failure.IsNone then
            failure <- Some(TextDiffFailureCodes.SourceChanged, "A source changed while the diff was being computed.")

    let ensureBuffers () =
        if bufA.Length = 0 then
            let largest = max previousSpec.ByteLength currentSpec.ByteLength
            let size = int (min (float bufferCap) (max 16.0 (float largest)))
            match ledger.TryLease(AllocationCategory.ChunkScratch, int64 size * 2L) with
            | Some lease ->
                bufferLease <- Some lease
                bufA <- Array.zeroCreate<byte> size
                bufB <- Array.zeroCreate<byte> size
                true
            | None -> false
        else true

    let releaseBuffers () =
        bufA <- Array.empty
        bufB <- Array.empty
        match bufferLease with
        | Some lease ->
            lease.Dispose()
            bufferLease <- None
        | None -> ()

    let releaseWindows () =
        previousSide.Table.ReleaseWindow()
        currentSide.Table.ReleaseWindow()

    let readFully (source: IByteSource) (position: float) (buffer: byte[]) (count: int) = async {
        let mutable read = 0
        let mutable status = 1
        while status = 1 && read < count do
            let! outcome = source.ReadAt (int64 (position + float read)) buffer read (count - read)
            match outcome with
            | ReadOutcome.Bytes got when got > 0 -> read <- read + got
            | ReadOutcome.Bytes _ -> status <- 0
            | ReadOutcome.NotYetAvailable -> status <- 0
            | ReadOutcome.EndOfSource -> status <- -1
        return status
    }

    // Equal-byte phase helpers.

    let setPendingEvidence (evidence: ScannerEvidence) =
        report DiffSide.Previous evidence.Kind evidence.Offset

    let finalizePending () =
        match pendingWindow with
        | Some window ->
            Scanner.finalizeWindow window setPendingEvidence
            pendingWindow <- None
        | None -> ()

    let mergeWindows (run: CommonRunResult) =
        for window in run.Windows do
            match pendingWindow with
            | Some previous when previous.Start = window.Start ->
                previous.Bytes <- previous.Bytes + window.Bytes
                previous.Scalars <- previous.Scalars + window.Scalars
                previous.Controls <- previous.Controls + window.Controls
                if previous.FirstControl < 0 then previous.FirstControl <- window.FirstControl
            | Some previous ->
                if previous.Bytes >= Scanner.SmallFinalWindowBytes then Scanner.finalizeWindow previous setPendingEvidence
                pendingWindow <- Some window
            | None -> pendingWindow <- Some window

    let reportRun (run: CommonRunResult) =
        if invalidDetail.IsNone then
            if run.Evidence.Length > 0 then
                let evidence = run.Evidence[0]
                report DiffSide.Previous evidence.Kind evidence.Offset
            else
                match run.Error with
                | Some error -> report DiffSide.Previous ("invalid " + Decoders.name previousEncoding + " sequence: " + error.Reason) error.Offset
                | None -> ()

    /// Restarts both scanners at a line start of the previous side and at the matching line start of the
    /// current side, then switches to window alignment.
    let handover (cursor: float) =
        let carried = pendingWindow
        pendingWindow <- None
        mode <- Mode.Window
        windowState <- WindowState.Loading
        windowLimit <- initialWindowLines
        partialPrevious <- -1
        partialCurrent <- -1
        previousSide.SetCursor(cursor, int64 builder.NextPrevious, windowLimit, config.WindowMaxBytes)
        currentSide.SetCursor(cursor + delta, int64 builder.NextCurrent, windowLimit, config.WindowMaxBytes)
        pBase <- cursor
        cBase <- cursor + delta
        match carried with
        | Some window ->
            let start = Scanner.windowStartOf cursor
            if window.Start = start then previousSide.Scanner.CurrentWindow <- { window with Bytes = int (cursor - start) }
            elif window.Start < start then Scanner.finalizeWindow window setPendingEvidence
        | None -> ()

    let exitCommon () =
        let lines = commonLines
        if lines <= float context then handover commonStart
        elif context = 0 then
            builder.EqualSkip lines
            handover lineStart
        else
            backActive <- true
            backEnd <- lineStart
            backFound <- 0
            backTarget <- lineStart

    let finishEqual () =
        finalizePending ()
        let trailing = if lineStart < float previousSpec.ByteLength then 1.0 else 0.0
        builder.EqualSkip(commonLines + trailing)
        builder.Finish()
        previousSide.SetCoverage(float previousSpec.ByteLength)
        currentSide.SetCoverage(float currentSpec.ByteLength)
        releaseWindows ()
        mode <- Mode.Done

    let backtrackStep (meter: Meter) = async {
        let width = float unitWidth
        let bomStart = float previousSpec.BomLength
        let block = max unitWidth ((min 4_096 (bufA.Length - unitWidth)) / unitWidth * unitWidth)
        let blockStart = max bomStart (backEnd - float block)
        let readStart = if blockStart > bomStart then blockStart - width else blockStart
        let count = int (backEnd - readStart)
        let! status = readFully previousSpec.Source.Value readStart bufA count
        if status < 0 then sourceChanged ()
        elif status = 0 then
            waited <- true
            Meter.charge meter 1
        else
            Meter.chargeBytes meter count
            Meter.charge meter 1
            let mutable p = backEnd - width
            while p >= blockStart && backFound < context do
                let isBoundary =
                    if p = bomStart then true
                    else
                        let before = Widths.unitAt previousEncoding bufA (int (p - readStart) - unitWidth)
                        if before = 0x0A then true
                        elif before = 0x0D then Widths.unitAt previousEncoding bufA (int (p - readStart)) <> 0x0A
                        else false
                if isBoundary then
                    backFound <- backFound + 1
                    backTarget <- p
                p <- p - width
            backEnd <- blockStart
            if backFound >= context then
                builder.EqualSkip commonLines
                builder.RewindEqual context
                backActive <- false
                handover backTarget
            elif blockStart <= bomStart then
                backActive <- false
                handover commonStart
    }

    let commonStep (meter: Meter) = async {
        if backActive then return! backtrackStep meter
        elif pos >= previousLength || pos + delta >= currentLength then
            if pos >= previousLength && pos + delta >= currentLength then
                if previousSpec.Source.Value.IsComplete() && currentSpec.Source.Value.IsComplete() then finishEqual ()
                else
                    waited <- true
                    Meter.charge meter 1
            else exitCommon ()
        else
            let remaining = min (previousLength - pos) (currentLength - pos - delta)
            let want = int (min (float (min chunk chunkLimit)) (min remaining (float bufA.Length)))
            let! first = previousSpec.Source.Value.ReadAt (int64 pos) bufA 0 want
            let! second = currentSpec.Source.Value.ReadAt (int64 (pos + delta)) bufB 0 want
            match first, second with
            | ReadOutcome.NotYetAvailable, _
            | _, ReadOutcome.NotYetAvailable ->
                waited <- true
                Meter.charge meter 1
            | ReadOutcome.EndOfSource, _
            | _, ReadOutcome.EndOfSource -> sourceChanged ()
            | ReadOutcome.Bytes a, ReadOutcome.Bytes b ->
                let got = min a b
                if got <= 0 then
                    waited <- true
                    Meter.charge meter 1
                else
                    let run = CommonRun.find previousEncoding (int64 pos) pendingCR bufA 0 bufB 0 got
                    mergeWindows run
                    reportRun run
                    Meter.charge meter 1
                    if run.Length > 0 then
                        Meter.chargeBytes meter run.Length
                        Meter.charge meter run.Lines
                        ledger.RecordCommonRunBytes(float run.Length)
                        pos <- pos + float run.Length
                        if run.Lines > 0 then lineStart <- float run.LastLineStart
                        commonLines <- commonLines + float run.Lines
                        pendingCR <- run.PendingCR
                        previousSide.SetCoverage pos
                        currentSide.SetCoverage(pos + delta)
                    if invalidDetail.IsSome then ()
                    elif run.Mismatch then exitCommon ()
                    elif run.Length > 0 then chunk <- min chunkLimit (chunk * 2)
                    elif float want < remaining && chunk < chunkLimit then chunk <- min chunkLimit (chunk * 2)
                    else exitCommon ()
    }

    // Window phase helpers.

    let isCommittable (operation: DiffOperation) =
        match operation.Kind with
        | OperationKind.Equal
        | OperationKind.EndingChanged
        | OperationKind.Unaligned -> true
        | _ -> false

    let beginFeeding () =
        windowState <- WindowState.Feeding
        opIndex <- 0
        feedPrevious <- 0
        feedCurrent <- 0
        regionStarted <- false

    /// Chooses how many operations the window commits. Trailing changes stay unsettled because the next window
    /// may match them differently.
    let decideCommit () =
        let previousTable = previousSide.Table
        let currentTable = currentSide.Table
        let bothFinished = previousSide.Finished && currentSide.Finished
        let emptyTable = previousTable.Count = 0 || currentTable.Count = 0
        if partialAlign then
            // Growing sources stall before the window fills. Only settled operations are committed and the
            // window keeps its size so the next bytes extend the same lines.
            partialAlign <- false
            let mutable last = -1
            for index = 0 to ops.Length - 1 do
                if isCommittable ops[index] then last <- index
            if last >= 0 then
                commitCount <- last + 1
                beginFeeding ()
            else
                ops <- Array.empty
                windowState <- WindowState.Loading
        elif bothFinished || emptyTable then
            commitCount <- ops.Length
            beginFeeding ()
        else
            let mutable last = -1
            let mutable anchored = false
            for index = 0 to ops.Length - 1 do
                if isCommittable ops[index] then last <- index
                if ops[index].Kind = OperationKind.Equal || ops[index].Kind = OperationKind.EndingChanged then anchored <- true
            let canGrow = windowLimit < config.WindowMaxLines && not previousSide.ByteFull && not currentSide.ByteFull
            // A window without any matching line may hold an insertion or deletion that is larger than the
            // window, so it grows before its unaligned lines are committed.
            if last >= 0 && (anchored || not canGrow) then
                commitCount <- last + 1
                beginFeeding ()
            elif canGrow then
                windowLimit <- min config.WindowMaxLines (windowLimit * 2)
                previousSide.SetLimits(windowLimit, config.WindowMaxBytes)
                currentSide.SetLimits(windowLimit, config.WindowMaxBytes)
                ops <- Array.empty
                windowState <- WindowState.Loading
            else
                // Forward resynchronization replaces this whole-window fallback.
                ops <- [| DiffOperations.make OperationKind.Unaligned 0 0 previousTable.Count currentTable.Count |]
                commitCount <- 1
                beginFeeding ()

    let startAlign () =
        let previousTable = previousSide.Table
        let currentTable = currentSide.Table
        if previousTable.Count = 0 || currentTable.Count = 0 then
            ops <-
                if previousTable.Count > 0 then [| DiffOperations.make OperationKind.Removed 0 -1 previousTable.Count 0 |]
                elif currentTable.Count > 0 then [| DiffOperations.make OperationKind.Added -1 0 0 currentTable.Count |]
                else Array.empty
            decideCommit ()
        else
            aligner <- Some(WindowAligner(previousTable, currentTable, config.MyersStepsPerGap, ledger))
            windowState <- WindowState.Aligning

    let loadSide (side: ScanSide) (buffer: byte[]) (meter: Meter) = async {
        let remainingLines = max 1 (windowLimit - side.Table.Count)
        let readMax = min scanChunkLimit (max 4_096 (remainingLines * 128))
        let! step = side.ReadInto(buffer, readMax)
        match step with
        | ReadData(count, endOfSource) ->
            side.Consume(buffer, count, endOfSource, meter) |> ignore
            return 1
        | ReadWaiting -> return 0
        | ReadChanged ->
            sourceChanged ()
            return 2
        | ReadWindowFull
        | ReadFinished -> return 2
    }

    let startPartialAlign () =
        partialAlign <- true
        partialPrevious <- previousSide.Table.Count
        partialCurrent <- currentSide.Table.Count
        startAlign ()

    let loadStep (meter: Meter) = async {
        Meter.charge meter 1
        let! previousResult = loadSide previousSide bufA meter
        let! currentResult = loadSide currentSide bufB meter
        let ready =
            (previousSide.WindowFull || previousSide.Finished) && (currentSide.WindowFull || currentSide.Finished)
        if failure.IsSome || invalidDetail.IsSome then ()
        elif ready then startAlign ()
        elif previousResult <> 1 && currentResult <> 1 then
            let previousCount = previousSide.Table.Count
            let currentCount = currentSide.Table.Count
            if previousCount > 0 && currentCount > 0 && (previousCount <> partialPrevious || currentCount <> partialCurrent) then
                startPartialAlign ()
            else waited <- true
    }

    let startCompare (previousIndex: int) (currentIndex: int) (count: int) =
        if previousEncoding = currentEncoding then
            let job = RunCompare(previousSide, currentSide, previousIndex, currentIndex, count, bufA, bufB)
            compareJob <- Some(fun meter -> job.Step meter)
        else
            let job = DecodeCompare(previousSide, currentSide, previousIndex, currentIndex, count, bufA, bufB)
            compareJob <- Some(fun meter -> job.Step meter)
        windowState <- WindowState.Comparing

    let alignStep (meter: Meter) =
        match aligner with
        | Some active ->
            match active.Step meter with
            | AlignStep.Running -> ()
            | AlignStep.NeedRun(previousIndex, currentIndex, count) -> startCompare previousIndex currentIndex count
            | AlignStep.Complete completed ->
                ops <- completed
                active.Dispose()
                aligner <- None
                decideCommit ()
        | None -> windowState <- WindowState.Loading

    let compareStep (meter: Meter) = async {
        Meter.charge meter 1
        match compareJob with
        | Some job ->
            let! step = job meter
            match step with
            | CompareStep.Continue -> ()
            | CompareStep.Waiting -> waited <- true
            | CompareStep.Finished matched ->
                compareJob <- None
                aligner.Value.ResolveRun matched
                windowState <- WindowState.Aligning
            | CompareStep.Changed -> sourceChanged ()
        | None -> windowState <- WindowState.Aligning
    }

    /// Re-enters the equal-byte phase from the line starts that a window left on each side. The cursors may
    /// sit at different offsets after an insertion or deletion.
    let reenterCommon (previousCursor: float) (currentCursor: float) =
        mode <- Mode.Common
        pos <- previousCursor
        delta <- currentCursor - previousCursor
        lineStart <- previousCursor
        commonStart <- previousCursor
        commonLines <- 0.0
        pendingCR <- false
        chunk <- min 4_096 chunkLimit
        pendingWindow <- None
        backActive <- false
        releaseWindows ()

    let finishWindow () =
        let previousTable = previousSide.Table
        let currentTable = currentSide.Table
        let mutable previousLines = 0
        let mutable currentLines = 0
        let mutable fedEqual = false
        for index = 0 to commitCount - 1 do
            let operation: DiffOperation = ops[index]
            previousLines <- previousLines + operation.PreviousCount
            currentLines <- currentLines + operation.CurrentCount
            if operation.Kind = OperationKind.Equal || operation.Kind = OperationKind.EndingChanged then fedEqual <- true
        let endsEqual = commitCount > 0 && ops[commitCount - 1].Kind = OperationKind.Equal
        let previousCursor = if previousLines = 0 then pBase else previousTable.Finish(previousLines - 1)
        let currentCursor = if currentLines = 0 then cBase else currentTable.Finish(currentLines - 1)
        let previousDone = previousSide.Finished && previousLines = previousTable.Count
        let currentDone = currentSide.Finished && currentLines = currentTable.Count
        let previousAtBoundary = previousLines = previousTable.Count && previousSide.Scanner.NextOffset = previousCursor
        let currentAtBoundary = currentLines = currentTable.Count && currentSide.Scanner.NextOffset = currentCursor
        let previousFirst = previousTable.LineBase + int64 previousLines
        let currentFirst = currentTable.LineBase + int64 currentLines
        ops <- Array.empty
        commitCount <- 0
        opIndex <- 0
        partialPrevious <- -1
        partialCurrent <- -1
        if fedEqual then windowLimit <- initialWindowLines
        windowState <- WindowState.Loading
        if previousDone && currentDone then
            builder.Finish()
            previousSide.SetCoverage(float previousSpec.ByteLength)
            currentSide.SetCoverage(float currentSpec.ByteLength)
            releaseWindows ()
            mode <- Mode.Done
        else
            if previousAtBoundary then previousSide.Advance(previousFirst, windowLimit, config.WindowMaxBytes)
            else previousSide.SetCursor(previousCursor, previousFirst, windowLimit, config.WindowMaxBytes)
            if currentAtBoundary then currentSide.Advance(currentFirst, windowLimit, config.WindowMaxBytes)
            else currentSide.SetCursor(currentCursor, currentFirst, windowLimit, config.WindowMaxBytes)
            pBase <- previousCursor
            cBase <- currentCursor
            if canStartCommon && endsEqual && builder.IsIdle && previousCursor < previousLength && currentCursor < currentLength then
                reenterCommon previousCursor currentCursor

    let feedStep (meter: Meter) =
        if opIndex >= commitCount then finishWindow ()
        else
            let operation: DiffOperation = ops[opIndex]
            let previousTable = previousSide.Table
            let currentTable = currentSide.Table
            let advanceOperation () =
                opIndex <- opIndex + 1
                feedPrevious <- 0
                feedCurrent <- 0
            match operation.Kind with
            | OperationKind.Equal ->
                let count = min 1_024 (operation.PreviousCount - feedPrevious)
                builder.Equal(previousTable, operation.PreviousIndex + feedPrevious, currentTable, operation.CurrentIndex + feedPrevious, count)
                feedPrevious <- feedPrevious + count
                Meter.charge meter (count / 8 + 1)
                if feedPrevious >= operation.PreviousCount then advanceOperation ()
            | OperationKind.EndingChanged
            | OperationKind.Replaced ->
                let count = min 256 (operation.PreviousCount - feedPrevious)
                let kind = if operation.Kind = OperationKind.Replaced then DiffRowKind.Replaced else DiffRowKind.EndingChanged
                builder.Paired(kind, previousTable, operation.PreviousIndex + feedPrevious, currentTable, operation.CurrentIndex + feedPrevious, count)
                feedPrevious <- feedPrevious + count
                Meter.charge meter (count / 2 + 1)
                if feedPrevious >= operation.PreviousCount then advanceOperation ()
            | OperationKind.Removed ->
                let count = min 256 (operation.PreviousCount - feedPrevious)
                builder.Removed(previousTable, operation.PreviousIndex + feedPrevious, count)
                feedPrevious <- feedPrevious + count
                Meter.charge meter (count / 2 + 1)
                if feedPrevious >= operation.PreviousCount then advanceOperation ()
            | OperationKind.Added ->
                let count = min 256 (operation.CurrentCount - feedCurrent)
                builder.Added(currentTable, operation.CurrentIndex + feedCurrent, count)
                feedCurrent <- feedCurrent + count
                Meter.charge meter (count / 2 + 1)
                if feedCurrent >= operation.CurrentCount then advanceOperation ()
            | OperationKind.Unaligned ->
                let laneChunk = max 1 (min 1_024 config.PageMaxRows)
                Meter.charge meter 1
                if not regionStarted then
                    builder.BeginRegion()
                    regionStarted <- true
                elif feedPrevious < operation.PreviousCount then
                    let count = min laneChunk (operation.PreviousCount - feedPrevious)
                    builder.RegionLines(true, previousTable, operation.PreviousIndex + feedPrevious, count)
                    feedPrevious <- feedPrevious + count
                    Meter.charge meter (count / 2)
                elif feedCurrent < operation.CurrentCount then
                    let count = min laneChunk (operation.CurrentCount - feedCurrent)
                    builder.RegionLines(false, currentTable, operation.CurrentIndex + feedCurrent, count)
                    feedCurrent <- feedCurrent + count
                    Meter.charge meter (count / 2)
                else
                    builder.EndRegion()
                    regionStarted <- false
                    advanceOperation ()

    /// True when the next microstep touches no source, so a run of them needs no asynchronous scheduling.
    let synchronousStep () =
        match mode with
        | Mode.Common -> false
        | Mode.Window -> windowState = WindowState.Aligning || windowState = WindowState.Feeding
        | Mode.Done -> true

    let synchronousMicrostep (meter: Meter) =
        match mode with
        | Mode.Window ->
            match windowState with
            | WindowState.Aligning -> alignStep meter
            | _ -> feedStep meter
        | _ -> Meter.charge meter 1

    /// Runs aligning and feeding microsteps in one plain loop, because they never wait on a source. The loop
    /// lives outside any async block since an async loop schedules one continuation per iteration.
    let runSynchronously (meter: Meter) =
        let mutable running = true
        while running do
            synchronousMicrostep meter
            running <-
                synchronousStep ()
                && failure.IsNone
                && invalidDetail.IsNone
                && builder.QueuedRows < config.PageMaxRows
                && builder.QueuedFragments < config.PageMaxFragments
                && not (mode = Mode.Done && builder.Finished)
                && not (builder.CompletedHunks > 0 && mode <> Mode.Done && not firstPageReturned)
                && not (Meter.overBudget meter)
                && not (Meter.quantumDue meter)

    let microstep (meter: Meter) = async {
        match mode with
        | Mode.Common -> do! commonStep meter
        | Mode.Window ->
            match windowState with
            | WindowState.Loading -> do! loadStep meter
            | WindowState.Aligning -> alignStep meter
            | WindowState.Comparing -> do! compareStep meter
            | WindowState.Feeding -> feedStep meter
        | Mode.Done -> Meter.charge meter 1
    }

    // Page building. Stage one reads row text and builds the parts without changing any state. Stage two records
    // the page and removes the consumed queue content.

    let rowBuffer = Array.zeroCreate<byte> 32_768

    let readLine (spec: SourceSpec) (encoding: TextEncoding) (line: LineRef) : Async<DiffLine option> = async {
        let contentBytes = line.Finish - line.Start - Widths.endingWidth encoding line.Ending
        let want = int (min 32_768.0 (max 0.0 contentBytes))
        let! status = if want = 0 then async.Return 1 else readFully spec.Source.Value line.Start rowBuffer want
        if status <> 1 then
            sourceChanged ()
            return None
        else
            textCount <- 0
            if want > 0 then
                let state = Decoders.createAt encoding (int64 line.Start)
                Decoders.decode state rowBuffer 0 want (fun _ _ value ->
                    if textCount < textUnits.Length then textUnits[textCount] <- uint16 value
                    textCount <- textCount + 1)
                |> ignore
            let mutable count = min textCount textUnits.Length
            if count > 0 && textUnits[count - 1] >= 0xD800us && textUnits[count - 1] <= 0xDBFFus then count <- count - 1
            let text = Native.utf16Decode textUnits count
            return
                Some {
                    Number = int64 line.Number
                    Ending = LineEndingCode.toLineEnding line.Ending
                    Slice = { OffsetUtf16 = 0L; TotalUtf16 = Some(int64 line.Length); Text = text; Highlights = Array.empty }
                }
    }

    let buildRow (id: string) (row: RowRef) : Async<DiffRow option> = async {
        match row.Kind with
        | DiffRowKind.Context
        | DiffRowKind.EndingChanged ->
            let! previous = readLine previousSpec previousEncoding row.Previous
            match previous with
            | None -> return None
            | Some previousLine ->
                let currentLine = {
                    previousLine with
                        Number = int64 row.Current.Number
                        Ending = LineEndingCode.toLineEnding row.Current.Ending
                        Slice = { previousLine.Slice with TotalUtf16 = Some(int64 row.Current.Length) }
                }
                return Some { Id = id; Kind = row.Kind; Previous = Some previousLine; Current = Some currentLine }
        | DiffRowKind.Removed ->
            let! previous = readLine previousSpec previousEncoding row.Previous
            return previous |> Option.map (fun line -> { Id = id; Kind = row.Kind; Previous = Some line; Current = None })
        | DiffRowKind.Added ->
            let! current = readLine currentSpec currentEncoding row.Current
            return current |> Option.map (fun line -> { Id = id; Kind = row.Kind; Previous = None; Current = Some line })
        | DiffRowKind.Replaced ->
            let! previous = readLine previousSpec previousEncoding row.Previous
            let! current = readLine currentSpec currentEncoding row.Current
            match previous, current with
            | Some previousLine, Some currentLine ->
                return Some { Id = id; Kind = row.Kind; Previous = Some previousLine; Current = Some currentLine }
            | _ -> return None
    }

    let countSide (rows: RowRef[]) (first: int) (count: int) (previousSide: bool) =
        let mutable total = 0
        for index = first to first + count - 1 do
            let kind = rows[index].Kind
            if (if previousSide then LineRefs.hasPrevious kind else LineRefs.hasCurrent kind) then total <- total + 1
        total

    let rowsPart (item: QueueItem) (start: int) (previousBefore: int) (currentBefore: int) (built: DiffRow[]) =
        let taken = built.Length
        DiffPart.Hunk {
            HunkId = identifier "h" (int64 item.Sequence)
            PreviousRange = {
                Start = int64 item.PreviousStart + int64 previousBefore
                Count = int64 (countSide item.Rows start taken true)
            }
            CurrentRange = {
                Start = int64 item.CurrentStart + int64 currentBefore
                Count = int64 (countSide item.Rows start taken false)
            }
            StartsHunk = item.StartsHunk && start = 0
            EndsHunk = item.EndsHunk && start + taken = item.Rows.Length
            Body = HunkBody.AlignedRows built
        }

    let lanePart (item: QueueItem) (isPrevious: bool) (start: int) (built: DiffLine[]) =
        let lines = if isPrevious then item.PreviousLines else item.CurrentLines
        let own = { Start = int64 lines[start].Number; Count = int64 built.Length }
        let other = { Start = int64 (if isPrevious then item.CurrentStart else item.PreviousStart); Count = 0L }
        DiffPart.Hunk {
            HunkId = identifier "h" (int64 item.Sequence)
            PreviousRange = if isPrevious then own else other
            CurrentRange = if isPrevious then other else own
            StartsHunk = item.StartsHunk && start = 0
            EndsHunk = item.EndsHunk && start + built.Length = lines.Length
            Body = if isPrevious then HunkBody.UnalignedSides(built, Array.empty) else HunkBody.UnalignedSides(Array.empty, built)
        }

    /// Reads up to available entries and keeps as many as fit in byteRoom. Sizes are estimated from a one entry
    /// fragment per entry and verified against the sizer for the whole fragment.
    let takeFragment
        (available: int)
        (byteRoom: int)
        (cancel: unit -> bool)
        (readEntry: int -> Async<obj option>)
        (makePart: obj[] -> DiffPart)
        : Async<FragmentBuild option> =
        async {
            let built = ResizeArray<obj>()
            let baseSize = sizer (makePart Array.empty)
            let mutable estimate = baseSize
            let mutable index = 0
            let mutable stop = false
            let mutable interrupted = false
            while not stop && index < available do
                if index &&& 255 = 255 && cancel () then
                    interrupted <- true
                    stop <- true
                else
                    let! entry = readEntry index
                    match entry with
                    | None ->
                        interrupted <- true
                        stop <- true
                    | Some value ->
                        let cost = max 0 (sizer (makePart [| value |]) - baseSize) + 1
                        if estimate + cost > byteRoom then stop <- true
                        else
                            built.Add value
                            estimate <- estimate + cost
                            index <- index + 1
            if interrupted then return None
            else
                let all = built.ToArray()
                let mutable taken = all.Length
                let mutable part = makePart all
                let mutable size = sizer part
                while size > byteRoom && taken > 0 do
                    taken <- taken - 1
                    part <- makePart (Array.sub all 0 taken)
                    size <- sizer part
                return Some { Part = part; Taken = taken; Size = size }
        }

    let buildPage (sequence: int64) (cancel: unit -> bool) : Async<PageBuild> = async {
        let items = Seq.toArray builder.Items
        let parts = ResizeArray<DiffPart>()
        let mutable usedBytes = 0
        let mutable fragments = 0
        let mutable rowsUsed = 0
        let mutable fullCount = 0
        let mutable partialConsumed = 0
        let mutable fullFragments = 0
        let mutable nextRow = rowSequence
        let mutable stop = false
        let mutable oversize = false
        let mutable interrupted = false
        let mutable index = 0
        while not stop && index < items.Length do
            let item = items[index]
            if cancel () then
                interrupted <- true
                stop <- true
            elif item.Kind = ItemKind.Gap then
                let part =
                    DiffPart.HiddenEqual {
                        GapId = identifier "g" (int64 item.Sequence)
                        PreviousRange = { Start = int64 item.PreviousStart; Count = int64 item.Count }
                        CurrentRange = { Start = int64 item.CurrentStart; Count = int64 item.Count }
                    }
                let size = sizer part
                if usedBytes + size > config.PageMaxBytes then
                    if parts.Count = 0 then oversize <- true
                    stop <- true
                else
                    parts.Add part
                    usedBytes <- usedBytes + size
                    fullCount <- fullCount + 1
                    index <- index + 1
            elif fragments >= config.PageMaxFragments || (item.Total > 0 && rowsUsed >= config.PageMaxRows) then stop <- true
            else
                let start = item.Consumed
                let available = min item.Remaining (config.PageMaxRows - rowsUsed)
                let byteRoom = config.PageMaxBytes - usedBytes
                let! fragment =
                    if item.Kind = ItemKind.Rows then
                        let previousBefore = countSide item.Rows 0 start true
                        let currentBefore = countSide item.Rows 0 start false
                        let firstId = nextRow
                        takeFragment
                            available
                            byteRoom
                            cancel
                            (fun offset -> async {
                                let! row = buildRow (identifier "r" (firstId + int64 offset)) item.Rows[start + offset]
                                return row |> Option.map box
                            })
                            (fun built -> rowsPart item start previousBefore currentBefore (Array.map unbox<DiffRow> built))
                    elif item.PreviousLines.Length > 0 then
                        takeFragment
                            available
                            byteRoom
                            cancel
                            (fun offset -> async {
                                let! line = readLine previousSpec previousEncoding item.PreviousLines[start + offset]
                                return line |> Option.map box
                            })
                            (fun built -> lanePart item true start (Array.map unbox<DiffLine> built))
                    else
                        takeFragment
                            available
                            byteRoom
                            cancel
                            (fun offset -> async {
                                let! line = readLine currentSpec currentEncoding item.CurrentLines[start + offset]
                                return line |> Option.map box
                            })
                            (fun built -> lanePart item false start (Array.map unbox<DiffLine> built))
                match fragment with
                | None ->
                    interrupted <- true
                    stop <- true
                | Some value ->
                    if value.Size > byteRoom || (item.Total > 0 && value.Taken = 0) then
                        if parts.Count = 0 then oversize <- true
                        stop <- true
                    else
                        parts.Add value.Part
                        usedBytes <- usedBytes + value.Size
                        fragments <- fragments + 1
                        rowsUsed <- rowsUsed + value.Taken
                        if item.Kind = ItemKind.Rows then nextRow <- nextRow + int64 value.Taken
                        if start + value.Taken >= item.Total then
                            fullCount <- fullCount + 1
                            fullFragments <- fullFragments + 1
                            index <- index + 1
                        else
                            partialConsumed <- value.Taken
                            stop <- true
        if interrupted then return PageBuild.Interrupted
        elif oversize then return PageBuild.TooLarge
        else
            let complete = builder.Finished && fullCount = items.Length
            let page = {
                PageId = identifier "p" sequence
                NextCursor = if complete then None else Some(identifier "c" (sequence + 1L))
                Parts = parts.ToArray()
                Progress = progress ()
                OutputComplete = complete
                Pending = None
            }
            return PageBuild.Built(page, fullCount, partialConsumed, rowsUsed, fullFragments, nextRow)
    }

    let recordResult (sequence: int64) (result: Resumable<DiffPage>) = async {
        do! journal.Append(sequence, result)
        requestSequence <- sequence + 1L
    }

    let producePage (sequence: int64) (cancel: unit -> bool) : Async<EngineResult<Resumable<DiffPage>> option> = async {
        let! built = buildPage sequence cancel
        match built with
        | PageBuild.Interrupted -> return None
        | PageBuild.TooLarge -> return Some(failWorker "A diff row exceeds the configured page byte limit.")
        | PageBuild.Built(page, fullCount, partialConsumed, rowsRemoved, fragmentsRemoved, nextRow) ->
            let value = Resumable.Ready page
            do! recordResult sequence value
            builder.CommitPage(fullCount, partialConsumed, rowsRemoved, fragmentsRemoved)
            rowSequence <- nextRow
            firstPageReturned <- true
            if page.OutputComplete then releaseBuffers ()
            return Some(EngineResult.Ok value)
    }

    let advance (sequence: int64) (cancel: unit -> bool) : Async<EngineResult<Resumable<DiffPage>>> = async {
        if not (ensureBuffers ()) then return failWorker "The diff scratch memory is not available."
        else
            let meter = Meter.create host.Clock config.Limits
            let mutable result: EngineResult<Resumable<DiffPage>> option = None
            let mutable first = true
            while result.IsNone do
                if cancel () || closing then result <- Some EngineResult.Canceled
                elif invalidDetail.IsSome then result <- Some(failContent ())
                elif failure.IsSome then
                    let code, message = failure.Value
                    result <- Some(EngineResult.Failed(code, message, None))
                else
                    let hardTrigger = builder.QueuedRows >= config.PageMaxRows || builder.QueuedFragments >= config.PageMaxFragments
                    let doneTrigger = mode = Mode.Done && builder.Finished
                    let softTrigger = builder.CompletedHunks > 0 && mode <> Mode.Done && not firstPageReturned
                    if hardTrigger || doneTrigger || softTrigger then
                        let! produced = producePage sequence cancel
                        match produced with
                        | Some value -> result <- Some value
                        | None -> if failure.IsNone then result <- Some EngineResult.Canceled
                    elif not first && Meter.overBudget meter then
                        if builder.QueueCount > 0 || builder.HasOpenRows then
                            builder.FlushOpen()
                            let! produced = producePage sequence cancel
                            match produced with
                            | Some value -> result <- Some value
                            | None -> if failure.IsNone then result <- Some EngineResult.Canceled
                        else
                            let value = Resumable.Scanning(progress (), identifier "c" (sequence + 1L), None)
                            do! recordResult sequence value
                            result <- Some(EngineResult.Ok value)
                    else
                        if Meter.quantumDue meter then
                            do! host.Yield()
                            Meter.beginNextQuantum meter
                        if synchronousStep () then runSynchronously meter
                        else do! microstep meter
                        if waited then
                            waited <- false
                            do! host.Yield()
                first <- false
            return result.Value
    }

    let acquire (cancel: unit -> bool) = async {
        let mutable acquired = false
        let mutable canceled = false
        while not acquired && not canceled && not closed do
            if cancel () || closing then canceled <- true
            elif not busy then
                busy <- true
                acquired <- true
            else
                do! host.Yield()
        return acquired
    }

    member _.Progress = progress ()

    member _.InitializeJournal() = journal.Initialize()

    member this.FirstPage(cancel: unit -> bool) = async {
        if closed then return failClosed ()
        elif invalidDetail.IsSome then return failContent ()
        else
            let! acquired = acquire cancel
            if not acquired then return EngineResult.Canceled
            else
                try
                    try
                        if cancel () then return EngineResult.Canceled
                        else
                            let! recorded = journal.Read 0L
                            match recorded with
                            | Some value -> return EngineResult.Ok value
                            | None when requestSequence <> 0L -> return failMismatch ()
                            | None -> return! advance 0L cancel
                    with error -> return failWorker error.Message
                finally busy <- false
    }

    member this.ReadPage (cursor: string) (cancel: unit -> bool) = async {
        if closed then return failClosed ()
        elif invalidDetail.IsSome then return failContent ()
        else
            let! acquired = acquire cancel
            if not acquired then return EngineResult.Canceled
            else
                try
                    try
                        if cancel () then return EngineResult.Canceled
                        else
                            match readIdentifier "c" cursor with
                            | None -> return failMismatch ()
                            | Some sequence ->
                                let! recorded = journal.Read sequence
                                match recorded with
                                | Some value -> return EngineResult.Ok value
                                | None when sequence <> requestSequence -> return failMismatch ()
                                | None -> return! advance sequence cancel
                    with error -> return failWorker error.Message
                finally busy <- false
    }

    member _.ReplayPage(pageId: string) = async {
        if closed then return failClosed ()
        elif invalidDetail.IsSome then return failContent ()
        else
            match readIdentifier "p" pageId with
            | None -> return failMismatch ()
            | Some sequence ->
                try
                    let! recorded = journal.Read sequence
                    match recorded with
                    | Some(Resumable.Ready page) when page.PageId = pageId -> return EngineResult.Ok { page with Pending = None }
                    | _ -> return failMismatch ()
                with error -> return failWorker error.Message
    }

    member _.Expand(_gapId: string) = async.Return(extensionResult "Gap expansion")

    member _.ReadLine(_side: DiffSide, _line: int64, _offsetUtf16: int64, _maxUtf16: int) =
        async.Return(extensionResult "Line slices")

    member _.PendingPreview() = extensionResult "Pending preview"

    member _.Resync() = extensionResult "Forward resynchronization"

    member _.Spill() = async.Return(extensionResult "Suspended-state spill")

    member _.Restore() = async.Return(extensionResult "Suspended-state restore")

    member _.Close() = async {
        if not closed then
            closing <- true
            while busy do
                do! host.Yield()
            if not closed then
                closed <- true
                match aligner with
                | Some active ->
                    active.Dispose()
                    aligner <- None
                | None -> ()
                previousSide.Dispose()
                currentSide.Dispose()
                releaseBuffers ()
                do! store.Dispose()
    }

module TextDiffSession =
    let create
        (host: EngineHost)
        (ledger: Ledger)
        (config: SessionConfig)
        (sizer: PartSizer)
        (previous: SourceSpec)
        (current: SourceSpec)
        =
        async {
            if String.IsNullOrWhiteSpace config.SessionId then invalidArg (nameof config.SessionId) "A session id is required."
            if
                config.ContextLines < 0
                || config.PageMaxRows <= 0
                || config.PageMaxBytes <= 0
                || config.PageMaxFragments <= 0
                || config.WindowMaxLines <= 0
                || config.WindowMaxBytes <= 0
                || config.CommonChunkBytes <= 0
                || config.CommonChunkBytes > CommonRun.MaxRunBytes
                || config.MyersStepsPerGap <= 0
            then
                invalidArg (nameof config) "The session limits must be positive and the context count cannot be negative."
            for source in [| previous; current |] do
                if source.BomLength < 0 || source.ByteLength < int64 source.BomLength then
                    invalidArg (nameof source) "The BOM length is outside the source."
                if source.Source.IsNone && (source.ByteLength <> 0L || source.BomLength <> 0) then
                    invalidArg (nameof source) "An absent source has no bytes."
            let parse (spec: SourceSpec) =
                if spec.Source.IsNone then TextEncoding.Utf8
                else
                    Decoders.tryParseName spec.Encoding
                    |> Option.defaultWith (fun () -> invalidArg (nameof spec.Encoding) "The source encoding is not supported.")
            let previousEncoding = parse previous
            let currentEncoding = parse current
            let! store = host.CreateTempStore config.SessionId
            let session = TextDiffSession(host, ledger, config, sizer, previous, current, previousEncoding, currentEncoding, store)
            do! session.InitializeJournal()
            return session
        }
