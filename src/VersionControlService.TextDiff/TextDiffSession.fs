namespace VersionControlService.TextDiff

open System
open System.Collections.Generic
open VersionControlService.Abstractions

[<RequireQualifiedAccess>]
type private Mode =
    | Common
    | Window
    | Resync
    | Done

[<RequireQualifiedAccess>]
type private WindowState =
    | Loading
    | Aligning
    | Comparing
    | Feeding

[<RequireQualifiedAccess>]
type private PageBuild =
    | Built of page: DiffPage * fullCount: int * partialConsumed: int * rowsRemoved: int * fragmentsRemoved: int * nextRow: int64
    | Suspended
    | TooLarge
    | Interrupted

type private FragmentBuild = { Part: DiffPart; Taken: int; Size: int }

type private SeekCursor = {
    SideIndex: int
    ByLine: bool
    StartLine: float
    EndLine: float
    TargetOffset: float
    State: ScannerState
    mutable Buffer: byte[]
    mutable Batch: LineBatch
    mutable BatchCapacity: int
    FoundLines: ResizeArray<ScannedLine>
    mutable LineNumber: float
    mutable Position: float
    mutable Complete: bool
    mutable Waiting: bool
}

type private PendingExpansion = {
    Attempt: int64
    GapSequence: int64
    Gap: EqualGap
    FromStart: bool
    Count: int
    TakeCount: int
    mutable RequestSequence: int64
    mutable Search: SeekCursor option
    mutable PreviousLines: ScannedLine[] option
    mutable CurrentLines: ScannedLine[] option
}

type private TextReadCursor = {
    Side: DiffSide
    Spec: SourceSpec
    Encoding: TextEncoding
    ContentEnd: float
    mutable Buffer: byte[]
    mutable Position: float
    mutable Decoder: DecoderState
    mutable PendingUnit: uint16 option
}

type private ReverseTextCursor = {
    Side: DiffSide
    Spec: SourceSpec
    Encoding: TextEncoding
    StartByte: float
    mutable EndByte: float
    mutable Buffer: byte[]
    mutable Units: uint16[]
    mutable BlockCount: int
    mutable BlockPosition: int
    mutable WindowStart: float
    mutable WindowEnd: float
    mutable ReadPosition: float
    mutable Decoder: DecoderState option
    mutable UnitCount: int
    mutable Loading: bool
    mutable Aligned: bool
    mutable Exhausted: bool
}

[<RequireQualifiedAccess>]
type private PairHighlightResult =
    | Spans of Highlight[] * Highlight[]
    | Middle of prefix: float * suffix: float

type private PendingLineRead = {
    Attempt: int64
    Side: DiffSide
    Line: int64
    OffsetUtf16: int64
    mutable SliceOffsetUtf16: int64
    MaxUtf16: int
    TakeMaxUtf16: int
    mutable RequestSequence: int64
    mutable LastUsed: int64
    mutable Search: SeekCursor option
    mutable Scanned: ScannedLine option
    PairedLine: int64 option
    mutable PairSearch: SeekCursor option
    mutable PairedScanned: ScannedLine option
    mutable PairAnalysis: PairLineAnalysis option
    mutable PairHighlights: PairHighlightResult option
    mutable Decoder: DecoderState option
    mutable BytePosition: float
    mutable Buffer: byte[]
    mutable Units: uint16[]
    mutable UnitCount: int
    mutable UnitPosition: float
    mutable PreviousUnit: uint16 option
    mutable Waiting: bool
}

and private PairLineAnalysis = {
    Previous: ScannedLine
    Current: ScannedLine
    PreviousCursor: TextReadCursor
    CurrentCursor: TextReadCursor
    mutable PreviousValues: uint16[] option
    mutable CurrentValues: uint16[] option
    mutable PreviousRead: int
    mutable CurrentRead: int
    mutable PreviousBlock: uint16[]
    mutable CurrentBlock: uint16[]
    mutable PreviousBlockCount: int
    mutable CurrentBlockCount: int
    mutable PreviousBlockPosition: int
    mutable CurrentBlockPosition: int
    mutable PreviousFinished: bool
    mutable CurrentFinished: bool
    PreviousReverse: ReverseTextCursor
    CurrentReverse: ReverseTextCursor
    mutable ReverseDone: bool
    mutable SuffixLength: float
    mutable LastSuffixUnit: uint16
    mutable HasLastSuffixUnit: bool
    mutable PrefixLength: float
    mutable PrefixOpen: bool
    mutable LastPrefixUnit: uint16
    mutable HasLastPrefixUnit: bool
    mutable Failed: bool
}

[<RequireQualifiedAccess>]
type private LongLineStep =
    | Complete of previous: DiffLine * current: DiffLine * prefix: float * suffix: float
    | Suspended
    | Canceled
    | Failed

type private PendingLongPair = {
    Item: QueueItem
    RowIndex: int
    Previous: LineRef
    Current: LineRef
    mutable PreviousLine: DiffLine
    mutable CurrentLine: DiffLine
    PreviousCursor: TextReadCursor
    CurrentCursor: TextReadCursor
    PreviousBlock: uint16[]
    CurrentBlock: uint16[]
    mutable PreviousBlockCount: int
    mutable CurrentBlockCount: int
    mutable PreviousBlockPosition: int
    mutable CurrentBlockPosition: int
    mutable PreviousBlockComplete: bool
    mutable CurrentBlockComplete: bool
    mutable PreviousFinished: bool
    mutable CurrentFinished: bool
    PreviousHistory: uint16[]
    CurrentHistory: uint16[]
    mutable HistoryCount: int
    mutable UnitOffset: float
    mutable PrefixLength: float
    mutable PrefixOpen: bool
    mutable Found: (DiffLine * DiffLine) option
    PreviousReverse: ReverseTextCursor
    CurrentReverse: ReverseTextCursor
    mutable ReverseDone: bool
    mutable SuffixLength: float
    mutable LastSuffixUnit: uint16
    mutable HasLastSuffixUnit: bool
    Reservation: int64
    mutable Reserved: bool
    mutable Failed: bool
}

type private PendingLineView = {
    SideIndex: int
    Number: int64
    StartOffset: float
    ContentEnd: float
    KnownEnd: float
    KnownUtf16: int64 option
    TotalUtf16: int64 option
    Ending: LineEnding
    Active: bool
    EndOfFile: bool
}

type private PendingPreviewCursor = {
    SideIndex: int
    Number: int64
    StartOffset: float
    Offset: int64
    mutable Decoder: DecoderState
    mutable BytePosition: float
    mutable DecodedUnits: float
    mutable Captured: int
    Units: uint16[]
    Buffer: byte[]
    Reservation: int64
}

module private LongLine =
    let sliceAt (line: DiffLine) (values: uint16[]) valueCount baseOffset startOffset =
        let mutable first = max 0 (min valueCount (int (startOffset - baseOffset)))
        let mutable length = min 8_192 (valueCount - first)
        if first > 0 && first < valueCount
           && values[first - 1] >= 0xD800us && values[first - 1] <= 0xDBFFus
           && values[first] >= 0xDC00us && values[first] <= 0xDFFFus then
            first <- first - 1
            length <- min 8_192 (valueCount - first)
        elif first < valueCount && values[first] >= 0xDC00us && values[first] <= 0xDFFFus then
            first <- first + 1
            length <- min 8_192 (valueCount - first)
        if length > 0 && values[first + length - 1] >= 0xD800us && values[first + length - 1] <= 0xDBFFus then length <- length - 1
        let selected = Array.zeroCreate<uint16> length
        if length > 0 then Array.blit values first selected 0 length
        let text = Native.utf16Decode selected length
        { line with Slice = { line.Slice with OffsetUtf16 = int64 (baseOffset + float first); Text = text } }

[<Struct>]
type private PairRange = {
    PreviousStart: float
    CurrentStart: float
    Count: float
}

type private PairingIndex(ledger: Ledger, createStore: unit -> Async<ITempStore>) =
    let cacheCapacity = 256
    let cacheReservation = int64 cacheCapacity * 3L * 8L
    let cacheEnabled = ledger.TryReserve(AllocationCategory.ResponseData, cacheReservation)
    let previousCache = if cacheEnabled then Array.zeroCreate<float> cacheCapacity else Array.empty
    let currentCache = if cacheEnabled then Array.zeroCreate<float> cacheCapacity else Array.empty
    let countCache = if cacheEnabled then Array.zeroCreate<float> cacheCapacity else Array.empty
    let mutable cacheStart = 0
    let mutable cacheStartIndex = 0
    let mutable cacheCount = 0
    let mutable rangeCount = 0
    let mutable lastPageSequence = -1L
    let mutable lastRange: PairRange option = None
    let mutable store: ITempStore option = None
    let scratch = Array.zeroCreate<byte> (Numbers.NumberBytes * 3)

    let ensureStore () = async {
        match store with
        | Some value -> return value
        | None ->
            let! value = createStore ()
            store <- Some value
            return value
    }

    let encodeRange (target: byte[]) offset (value: PairRange) =
        Numbers.writeNumber target offset value.PreviousStart
        Numbers.writeNumber target (offset + Numbers.NumberBytes) value.CurrentStart
        Numbers.writeNumber target (offset + Numbers.NumberBytes * 2) value.Count

    let decodeRange (source: byte[]) offset = {
        PreviousStart = Numbers.readNumber source offset
        CurrentStart = Numbers.readNumber source (offset + Numbers.NumberBytes)
        Count = Numbers.readNumber source (offset + Numbers.NumberBytes * 2)
    }

    let inRange start count value = value >= start && value < start + count

    let cacheRange index (value: PairRange) =
        if cacheEnabled then
            let slot =
                if cacheCount < cacheCapacity then
                    let next = (cacheStart + cacheCount) % cacheCapacity
                    cacheCount <- cacheCount + 1
                    next
                else
                    let next = cacheStart
                    cacheStart <- (cacheStart + 1) % cacheCapacity
                    cacheStartIndex <- cacheStartIndex + 1
                    next
            previousCache[slot] <- value.PreviousStart
            currentCache[slot] <- value.CurrentStart
            countCache[slot] <- value.Count

    let cachedRange index =
        let slot = (cacheStart + (index - cacheStartIndex)) % cacheCapacity
        {
            PreviousStart = previousCache[slot]
            CurrentStart = currentCache[slot]
            Count = countCache[slot]
        }

    let cachedFind side line =
        let mutable low = cacheStartIndex
        let mutable high = cacheStartIndex + cacheCount - 1
        let mutable result = None
        while result.IsNone && low <= high do
            let middle = low + (high - low) / 2
            let range = cachedRange middle
            let start = if side = DiffSide.Previous then range.PreviousStart else range.CurrentStart
            if inRange start range.Count line then
                result <- Some(if side = DiffSide.Previous then int64 (range.CurrentStart + line - range.PreviousStart) else int64 (range.PreviousStart + line - range.CurrentStart))
            elif line < start then high <- middle - 1
            else low <- middle + 1
        result

    let diskFind side line = async {
        let mutable low = 0
        let mutable high = rangeCount - 1
        let mutable result = None
        let mutable failed = false
        while result.IsNone && low <= high && not failed do
            let middle = low + (high - low) / 2
            let! source = ensureStore ()
            let! actual = source.ReadAt (int64 middle * int64 scratch.Length) scratch 0 scratch.Length
            if actual <> scratch.Length then failed <- true
            else
                let range = decodeRange scratch 0
                let start = if side = DiffSide.Previous then range.PreviousStart else range.CurrentStart
                if inRange start range.Count line then
                    result <- Some(if side = DiffSide.Previous then int64 (range.CurrentStart + line - range.PreviousStart) else int64 (range.PreviousStart + line - range.CurrentStart))
                elif line < start then high <- middle - 1
                else low <- middle + 1
        if failed then invalidOp "The line pairing index is truncated."
        return result
    }

    let contiguous (left: PairRange) (right: PairRange) =
        left.PreviousStart + left.Count = right.PreviousStart
        && left.CurrentStart + left.Count = right.CurrentStart

    member _.AppendPage(sequence: int64, ranges: PairRange[]) = async {
        if sequence > lastPageSequence then
            let mutable first = 0
            if ranges.Length > 0 then
                match lastRange with
                | Some previous when contiguous previous ranges[0] ->
                    let combined = { previous with Count = previous.Count + ranges[0].Count }
                    let! target = ensureStore ()
                    encodeRange scratch 0 combined
                    do! target.WriteAt (int64 (rangeCount - 1) * int64 scratch.Length) scratch 0 scratch.Length
                    lastRange <- Some combined
                    if cacheEnabled && rangeCount - 1 >= cacheStartIndex then
                        let slot = (cacheStart + (rangeCount - 1 - cacheStartIndex)) % cacheCapacity
                        previousCache[slot] <- combined.PreviousStart
                        currentCache[slot] <- combined.CurrentStart
                        countCache[slot] <- combined.Count
                    first <- 1
                | _ -> ()
            if first < ranges.Length then
                let appendCount = ranges.Length - first
                let values = Array.zeroCreate<float> (appendCount * 3)
                for index = 0 to appendCount - 1 do
                    let value = ranges[first + index]
                    values[index * 3] <- value.PreviousStart
                    values[index * 3 + 1] <- value.CurrentStart
                    values[index * 3 + 2] <- value.Count
                let bytes = Numbers.encodeAll values
                let! target = ensureStore ()
                do! target.WriteAt (int64 rangeCount * int64 scratch.Length) bytes 0 bytes.Length
                for index = 0 to appendCount - 1 do
                    let value = ranges[first + index]
                    cacheRange rangeCount value
                    rangeCount <- rangeCount + 1
                    lastRange <- Some value
            lastPageSequence <- sequence
    }

    member _.Find(side: DiffSide, line: int64) = async {
        let value = float line
        match cachedFind side value with
        | Some paired -> return Some paired
        | None when cacheEnabled && cacheStartIndex = 0 && cacheCount = rangeCount -> return None
        | None -> return! diskFind side value
    }

    member _.Dispose() = async {
        match store with
        | Some value ->
            store <- None
            do! value.Dispose()
        | None -> ()
        if cacheEnabled then ledger.Release(AllocationCategory.ResponseData, cacheReservation)
    }

/// Streams a diff of two byte sources. The engine compares equal bytes with CommonRun and skips whole runs of
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
    // One request at a time. Every member that reads or writes the journal, the pairing index or the
    // scanners holds this flag for its whole run, so those parts need no locks of their own. A host
    // that issues several requests to one session must wait for each reply before it sends the next.
    let mutable busy = false
    let mutable requestSequence = 0L
    let mutable rowSequence = 0L
    let mutable nextGapSequence = 0L
    let mutable nextExpandRequest = 0L
    let mutable nextExpandAttempt = 0L
    let mutable nextLineRequest = 0L
    let mutable nextLineAttempt = 0L
    let mutable lineReadUse = 0L
    let mutable pairedMismatch: (int64 * int64 * int64 option) option = None
    let pendingPreviewCursors: PendingPreviewCursor option[] = Array.create 2 None
    let pendingExpansions = Dictionary<int64, PendingExpansion>()
    let pendingLineReads = Dictionary<int64, PendingLineRead>()
    let mutable pendingLongPair: PendingLongPair option = None
    let mutable mode = if canStartCommon then Mode.Common else Mode.Window
    let mutable windowState = WindowState.Loading
    let mutable waited = false
    let mutable bufA = Array.empty<byte>
    let mutable bufB = Array.empty<byte>
    let mutable bufferLease: IDisposable option = None

    // Equal-byte phase.
    // The positions below belong to the previous side. The current side sits at the same
    // position plus delta, which is nonzero after an insertion or deletion.
    let mutable pos = float (max previousSpec.BomLength currentSpec.BomLength)
    let mutable delta = 0.0
    let mutable lineStart = pos
    let mutable commonStart = pos
    let mutable commonLines = 0.0
    let mutable pendingCR = false
    let mutable chunk = min 4_096 chunkLimit
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
    let mutable preserveWindowAfterCommit = false
    let mutable windowSelfUniqueLines = 0
    let mutable pBase = float previousSpec.BomLength
    let mutable cBase = float currentSpec.BomLength
    let mutable partialAlign = false
    let mutable partialPrevious = -1
    let mutable partialCurrent = -1
    let mutable resync: ResyncEngine option = None

    // Page building.
    // Text decoding scratch for page building.
    let textUnits = Array.zeroCreate<uint16> 8_192
    let mutable textCount = 0

    let builder = HunkBuilder(config.ContextLines, config.PageMaxRows)
    let journal = Journal(store, ledger, config.JournalCacheBytes, fun () -> host.CreateTempStore(config.SessionId + ":journal-index"))
    let pairings = PairingIndex(ledger, fun () -> host.CreateTempStore(config.SessionId + ":pairs"))

    let checkpoints =
        Checkpoints(config.CheckpointIntervalBytes, [| previousEncoding; currentEncoding |])

    let knownLineCounts = Array.create 2 None

    let setKnownLineCount sideIndex count =
        knownLineCounts[sideIndex] <-
            match knownLineCounts[sideIndex] with
            | Some previous -> Some(max previous count)
            | None -> Some count

    do
        if previousSpec.Source.IsNone then knownLineCounts[0] <- Some 0L
        if currentSpec.Source.IsNone then knownLineCounts[1] <- Some 0L

    let report (side: DiffSide) (kind: string) (offset: int64) (invalidSequenceOffset: int64 option) =
        if invalidDetail.IsNone then
            let evidence =
                if kind = "nul" then "NUL character at byte " + string offset
                else kind + " at byte " + string offset
            invalidDetail <- Some { Side = side; Evidence = evidence; InvalidSequenceOffset = invalidSequenceOffset }

    let reportTallyEvidence side kind offset = report side kind offset None

    let previousEvidence = ControlRatioTally(DiffSide.Previous, previousSpec.BomLength, previousSpec.ByteLength, reportTallyEvidence)
    let currentEvidence = ControlRatioTally(DiffSide.Current, currentSpec.BomLength, currentSpec.ByteLength, reportTallyEvidence)

    let previousSide =
        ScanSide(previousSpec, previousEncoding, AllocationCategory.PreviousWindows, DiffSide.Previous, ledger, previousEvidence, config.HashMaskForTesting, report, config.CheckpointIntervalBytes, setKnownLineCount 0, (fun state line -> checkpoints.Observe(0, state, line)), Some(fun offset line -> checkpoints.ObserveLine(0, offset, line)))

    let currentSide =
        ScanSide(currentSpec, currentEncoding, AllocationCategory.CurrentWindows, DiffSide.Current, ledger, currentEvidence, config.HashMaskForTesting, report, config.CheckpointIntervalBytes, setKnownLineCount 1, (fun state line -> checkpoints.Observe(1, state, line)), Some(fun offset line -> checkpoints.ObserveLine(1, offset, line)))

    do
        previousSide.BeginWindow(0L, windowLimit, config.WindowMaxBytes)
        currentSide.BeginWindow(0L, windowLimit, config.WindowMaxBytes)

    let modeIsDone () =
        match mode with
        | Mode.Done -> true
        | _ -> false

    let modeIsWindowOrResync () =
        match mode with
        | Mode.Window
        | Mode.Resync -> true
        | _ -> false

    let progress () = {
        ValidatedBytes = int64 (min (float totalBytes) (previousSide.Coverage + currentSide.Coverage))
        TotalBytes = totalBytes
        ScanComplete = modeIsDone ()
    }

    let identifier (kind: string) (sequence: int64) =
        let prefix = config.SessionId + ":" + kind + ":" + string sequence
        let hash = Hash.create ()
        for character in prefix do
            Hash.addCodeUnit hash (int character)
        prefix + ":" + Hash.toString hash

    let operationIdentifier (kind: string) (attempt: int64) (sequence: int64) (binding: string) =
        let prefix = config.SessionId + ":" + kind + ":" + string attempt + ":" + string sequence + ":" + binding
        let hash = Hash.create ()
        for character in prefix do Hash.addCodeUnit hash (int character)
        config.SessionId + ":" + kind + ":" + string attempt + ":" + string sequence + ":" + Hash.toString hash

    let readOperationIdentifier (expectedKind: string) (binding: string) (value: string) =
        if isNull value then None
        else
            let last = value.LastIndexOf ':'
            if last <= 0 then None
            else
                let beforeChecksum = value.Substring(0, last)
                let previousSeparator = beforeChecksum.LastIndexOf ':'
                if previousSeparator <= 0 then None
                else
                    let sequenceText = beforeChecksum.Substring(previousSeparator + 1)
                    let attemptSeparator = beforeChecksum.LastIndexOf(':', previousSeparator - 1)
                    if attemptSeparator <= 0 then None
                    else
                        let attemptText = beforeChecksum.Substring(attemptSeparator + 1, previousSeparator - attemptSeparator - 1)
                        let kindSeparator = beforeChecksum.LastIndexOf(':', attemptSeparator - 1)
                        if kindSeparator <= 0 then None
                        else
                            let sessionId = beforeChecksum.Substring(0, kindSeparator)
                            let kind = beforeChecksum.Substring(kindSeparator + 1, attemptSeparator - kindSeparator - 1)
                            let mutable attempt = 0L
                            let mutable sequence = 0L
                            if sessionId <> config.SessionId
                               || kind <> expectedKind
                               || not (Int64.TryParse(attemptText, &attempt))
                               || not (Int64.TryParse(sequenceText, &sequence))
                               || operationIdentifier kind attempt sequence binding <> value then None
                            else Some(attempt, sequence)

    let journalKey (kind: int64) (sequence: int64) = sequence * 5L + kind

    let rowIdentifierPrefix = config.SessionId + ":r:"
    let rowIdentifierSeed =
        let hash = Hash.create ()
        for character in rowIdentifierPrefix do
            Hash.addCodeUnit hash (int character)
        hash

    let rowIdentifier (sequence: int) =
        let sequenceText = string sequence
        let hash = { Lo = rowIdentifierSeed.Lo; Hi = rowIdentifierSeed.Hi }
        for character in sequenceText do
            Hash.addCodeUnit hash (int character)
        rowIdentifierPrefix + sequenceText + ":" + Hash.toString hash

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

    let sourceChanged () =
        if failure.IsNone then
            failure <- Some(TextDiffFailureCodes.SourceChanged, "A source changed while the diff was being computed.")

    let failCommit (error: exn) =
        failure <- Some(TextDiffFailureCodes.WorkerFailed, error.Message)

    let reportDecodeError sideIndex (error: DecodeError) =
        let sideState = if sideIndex = 0 then previousSide else currentSide
        if float error.Offset < sideState.Coverage then sourceChanged ()
        else
            let side = if sideIndex = 0 then DiffSide.Previous else DiffSide.Current
            let encoding = if sideIndex = 0 then previousEncoding else currentEncoding
            report side ("invalid " + Decoders.name encoding + " sequence: " + error.Reason) error.Offset (Some error.Offset)

    /// A worker runs one session, so the ledger always has room for the chunk scratch. A refusal is a bug.
    let ensureBuffers () =
        if bufA.Length = 0 then
            let largest = max previousSpec.ByteLength currentSpec.ByteLength
            let size = int (min (float bufferCap) (max 16.0 (float largest)))
            match ledger.TryLease(AllocationCategory.ChunkScratch, int64 size * 2L) with
            | Some lease ->
                bufferLease <- Some lease
                bufA <- Array.zeroCreate<byte> size
                bufB <- Array.zeroCreate<byte> size
            | None -> invalidOp "The ledger refused the chunk scratch of the session."

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

    let reportRun (run: CommonRunResult) =
        if invalidDetail.IsNone then
            match run.Error with
            | Some error -> reportDecodeError 0 error
            | None -> ()

    let countUtf16Units (source: IByteSource) (encoding: TextEncoding) (startOffset: int64) (endOffset: int64) (meter: Meter) = async {
        let buffer = Array.zeroCreate<byte> 65_536
        let mutable state = Decoders.createAt encoding startOffset
        let mutable position = float startOffset
        let finish = float endOffset
        let mutable units = 0.0
        let mutable complete = true
        while complete && position < finish && not (Meter.overBudget meter) do
            let count = int (min (float buffer.Length) (finish - position))
            let! outcome = source.ReadAt (int64 position) buffer 0 count
            match outcome with
            | ReadOutcome.Bytes actual when actual > 0 ->
                let next, result = Decoders.decode state buffer 0 actual (fun _ _ _ -> units <- units + 1.0)
                state <- next
                position <- float next.AbsoluteOffset
                Meter.chargeBytes meter actual
                match result with
                | Error error ->
                    reportDecodeError 0 error
                    complete <- false
                | Ok _ -> ()
            | _ -> complete <- false
        if position <> finish then return None else return Some(int64 units)
    }

    /// Restarts both scanners at a line start of the previous side and at the matching line start of the
    /// current side, then switches to window alignment.
    let handover (cursor: float) =
        mode <- Mode.Window
        windowState <- WindowState.Loading
        windowLimit <- initialWindowLines
        partialPrevious <- -1
        partialCurrent <- -1
        previousSide.SetCursor(cursor, int64 builder.NextPrevious, windowLimit, config.WindowMaxBytes)
        currentSide.SetCursor(cursor + delta, int64 builder.NextCurrent, windowLimit, config.WindowMaxBytes)
        pBase <- cursor
        cBase <- cursor + delta

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

    /// Records a checkpoint at the last line start when a side passed its interval boundary. The equal-byte phase
    /// has no scanner state, so the checkpoint starts a fresh scanner at that line.
    let observeCommon () =
        if checkpoints.IsDue(0, lineStart) then
            checkpoints.Observe(0, Scanner.create previousEncoding (int64 lineStart), builder.NextPrevious + commonLines)
        if checkpoints.IsDue(1, lineStart + delta) then
            checkpoints.Observe(1, Scanner.create currentEncoding (int64 (lineStart + delta)), builder.NextCurrent + commonLines)

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
            let checkpointRoom offset =
                let interval = config.CheckpointIntervalBytes
                let boundary = (Math.Floor(offset / interval) + 1.0) * interval
                let room = min (float bufA.Length) (max (float unitWidth) (boundary - offset))
                max unitWidth (int room / unitWidth * unitWidth)
            let checkpointBytes = min (checkpointRoom pos) (checkpointRoom (pos + delta))
            let want = int (min (float (min chunk chunkLimit)) (min remaining (float (min bufA.Length checkpointBytes))))
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
                    let oldPos = pos
                    let run =
                        CommonRun.findObserved
                            previousEncoding
                            oldPos
                            (oldPos + delta)
                            pendingCR
                            previousEvidence.HighWater
                            currentEvidence.HighWater
                            (fun start bytes controls scalars firstControl firstNul -> previousEvidence.ObserveCounts(start, bytes, controls, scalars, firstControl, firstNul))
                            (fun start bytes controls scalars firstControl firstNul -> currentEvidence.ObserveCounts(start, bytes, controls, scalars, firstControl, firstNul))
                            bufA
                            0
                            bufB
                            0
                            got
                    reportRun run
                    Meter.charge meter 1
                    if run.Length > 0 then
                        Meter.chargeBytes meter run.Length
                        Meter.charge meter run.Lines
                        pos <- pos + float run.Length
                        if run.Lines > 0 then lineStart <- float run.LastLineStart
                        commonLines <- commonLines + float run.Lines
                        pendingCR <- run.PendingCR
                        if not pendingCR then observeCommon ()
                        previousSide.SetCoverage pos
                        currentSide.SetCoverage(pos + delta)
                    if invalidDetail.IsSome then ()
                    elif run.Mismatch then
                        let differenceByte = run.DifferenceOffset |> Option.defaultValue run.Length
                        let! mismatchUnits = countUtf16Units previousSpec.Source.Value previousEncoding (int64 lineStart) (int64 (oldPos + float differenceByte)) meter
                        pairedMismatch <- Some(int64 (builder.NextPrevious + commonLines), int64 (builder.NextCurrent + commonLines), mismatchUnits)
                        exitCommon ()
                    elif run.Length > 0 then chunk <- min chunkLimit (chunk * 2)
                    elif float want < remaining && chunk < chunkLimit then chunk <- min chunkLimit (chunk * 2)
                    else exitCommon ()
    }

    // Window phase helpers.

    let beginFeeding () =
        windowState <- WindowState.Feeding
        opIndex <- 0
        feedPrevious <- 0
        feedCurrent <- 0
        regionStarted <- false

    /// Searches forward from the window start for the place where the two sources line up again. The window
    /// tables are released because the search scans the same lines again with its own tables.
    let createEngine (startOffsets: float[]) (startLines: float[]) =
        let coverage (side: int) (value: float) =
            if side = 0 then previousSide.SetCoverage value else currentSide.SetCoverage value
        ResyncEngine(
            config,
            ledger,
            [| previousEvidence; currentEvidence |],
            builder,
            [| previousSpec; currentSpec |],
            [| previousEncoding; currentEncoding |],
            report,
            sourceChanged,
            coverage,
            (fun side count -> setKnownLineCount side count),
            (fun side state line -> checkpoints.Observe(side, state, line)),
            startOffsets,
            startLines
        )

    let startResync () =
        let startLines = [| float previousSide.Table.LineBase; float currentSide.Table.LineBase |]
        resync <- Some(createEngine [| pBase; cBase |] startLines)
        ops <- Array.empty
        commitCount <- 0
        windowState <- WindowState.Loading
        releaseWindows ()
        mode <- Mode.Resync

    /// A window may still grow while its limit is below the largest size and neither side filled its bytes.
    let canGrowWindow () =
        windowLimit < config.WindowMaxLines && not previousSide.ByteFull && not currentSide.ByteFull

    /// Chooses how many operations the window commits. Trailing changes stay unsettled because the next window
    /// may match them differently.
    let decideCommit () =
        let previousTable = previousSide.Table
        let currentTable = currentSide.Table
        let bothFinished = previousSide.Finished && currentSide.Finished
        let emptyTable = previousTable.Count = 0 || currentTable.Count = 0
        let canGrow = canGrowWindow ()
        preserveWindowAfterCommit <- false
        let mutable lastAnchored = -1
        let mutable lastPrefixAnchor = -1
        let mutable lastEqual = -1
        let mutable lastConfirmedAnchor = -1
        let mutable anchoredLines = 0
        let mutable runLines = 0
        let mutable previousEnd = 0
        let mutable currentEnd = 0
        for index = 0 to ops.Length - 1 do
            let operation = ops[index]
            if operation.Kind = OperationKind.Equal || operation.Kind = OperationKind.EndingChanged then
                lastEqual <- index
                runLines <- runLines + operation.PreviousCount
                if operation.Anchored then
                    lastAnchored <- index
                    anchoredLines <- anchoredLines + operation.PreviousCount
                    if runLines >= config.ResyncConfirmLines then lastConfirmedAnchor <- index
                    if operation.PrefixAnchor then lastPrefixAnchor <- index
                    previousEnd <- operation.PreviousIndex + operation.PreviousCount
                    currentEnd <- operation.CurrentIndex + operation.CurrentCount
            else
                runLines <- 0
        // A window commits up to its last anchored equal run only when few lines follow that run. The lines
        // are counted on a side whose window stopped at its limit. A side that reached the end of its source
        // holds everything that is left, so its count says nothing about the anchor. When both windows
        // stopped at their limit, the side with fewer lines after the anchor is the one whose window ended
        // there. The other side then holds the lines of an insertion or deletion.
        let previousTail = previousTable.Count - previousEnd
        let currentTail = currentTable.Count - currentEnd
        let anchorTail =
            if previousSide.Finished && not currentSide.Finished then currentTail
            elif currentSide.Finished && not previousSide.Finished then previousTail
            else min previousTail currentTail
        // Many lines after the anchor on one side only can mean an insertion or deletion that the window does
        // not hold completely. The anchor may then be a line that occurs once in each window only by chance, such
        // as a repeated value inside inserted records, and such an anchor sits in a short run between changed
        // lines. The window settles on it in that case only when its equal run is long.
        let mutable anchorRunLines = 0
        let mutable runIndex = lastAnchored
        while runIndex >= 0 && (ops[runIndex].Kind = OperationKind.Equal || ops[runIndex].Kind = OperationKind.EndingChanged) do
            anchorRunLines <- anchorRunLines + ops[runIndex].PreviousCount
            runIndex <- runIndex - 1
        let anchorTailSmall = anchorTail <= 64 && (max previousTail currentTail <= 64 || anchorRunLines >= 64)
        let growWindow () =
            windowLimit <- min config.WindowMaxLines (windowLimit * 2)
            previousSide.SetLimits(windowLimit, config.WindowMaxBytes)
            currentSide.SetLimits(windowLimit, config.WindowMaxBytes)
            ops <- Array.empty
            windowState <- WindowState.Loading
        if partialAlign then
            // Growing sources stall before the window fills. Only settled operations are committed and the
            // window keeps its size so the next bytes extend the same lines. A partial alignment starts only
            // while a side can still grow, so the finished case is a guard.
            partialAlign <- false
            if lastAnchored >= 0 && anchorTailSmall then
                commitCount <- lastAnchored + 1
                beginFeeding ()
            elif bothFinished then
                commitCount <- ops.Length
                beginFeeding ()
            else
                ops <- Array.empty
                windowState <- WindowState.Loading
        elif bothFinished || emptyTable then
            commitCount <- ops.Length
            beginFeeding ()
        else
            if lastAnchored >= 0 && anchorTailSmall then
                commitCount <- lastAnchored + 1
                beginFeeding ()
            elif lastPrefixAnchor >= 0 then
                // Only the aligned start is certain. It is committed and the window keeps its size, so the
                // rest is aligned again with the following lines.
                commitCount <- lastPrefixAnchor + 1
                preserveWindowAfterCommit <- true
                beginFeeding ()
            elif canGrow then
                // The window may hold an insertion or deletion larger than itself, so it grows first.
                growWindow ()
            elif windowSelfUniqueLines = 0 && lastEqual >= 0 then
                // Every line occurs at least twice within its own window, as in a file of identical rows. The
                // window keeps its own alignment up to its last equal run there. A window that holds a line
                // unique within itself may be part of an insertion or deletion larger than the window, and the
                // forward search looks for the place where the sources meet again.
                commitCount <- lastEqual + 1
                beginFeeding ()
            elif lastConfirmedAnchor >= 0 || (lastAnchored >= 0 && anchoredLines >= 64) then
                // At its largest size the window settles on its last anchored run that a forward search would also
                // confirm (ResyncConfirmLines lines), or on its last anchor when the window holds many anchored
                // lines. A line that matches by chance inside an insertion larger than the window sits in a
                // one-line run in a window with almost no other anchored lines, so such a window still goes to
                // the forward search.
                commitCount <- (if lastConfirmedAnchor >= 0 then lastConfirmedAnchor else lastAnchored) + 1
                beginFeeding ()
            else
                // At its largest size without an anchor the diff continues with a forward search.
                startResync ()

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
            aligner <- Some(WindowAligner(previousTable, currentTable, config.MyersStepsPerGap, previousSpec.ByteLength = currentSpec.ByteLength, (not partialAlign) && not (canGrowWindow ()), ledger))
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
                windowSelfUniqueLines <- active.SelfUniqueLines
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
        backActive <- false
        releaseWindows ()

    let disposeResync () =
        match resync with
        | Some engine ->
            engine.Dispose()
            resync <- None
        | None -> ()

    /// Continues with windows at line starts that the forward search found. The sides may be at different
    /// offsets and line numbers.
    let resumeAt (previousOffset: float) (previousLine: float) (currentOffset: float) (currentLine: float) =
        disposeResync ()
        mode <- Mode.Window
        windowState <- WindowState.Loading
        windowLimit <- initialWindowLines
        partialAlign <- false
        partialPrevious <- -1
        partialCurrent <- -1
        ops <- Array.empty
        commitCount <- 0
        previousSide.SetCursor(previousOffset, int64 previousLine, windowLimit, config.WindowMaxBytes)
        currentSide.SetCursor(currentOffset, int64 currentLine, windowLimit, config.WindowMaxBytes)
        pBase <- previousOffset
        cBase <- currentOffset
        if canStartCommon && builder.IsIdle && previousOffset < previousLength && currentOffset < currentLength then
            reenterCommon previousOffset currentOffset

    let finishResync () =
        disposeResync ()
        builder.Finish()
        previousSide.SetCoverage(float previousSpec.ByteLength)
        currentSide.SetCoverage(float currentSpec.ByteLength)
        releaseWindows ()
        mode <- Mode.Done

    let resyncStep (meter: Meter) (bufferA: byte[]) (bufferB: byte[]) = async {
        let! step = resync.Value.Step(meter, bufferA, bufferB)
        match step with
        | ResyncStep.Running -> ()
        | ResyncStep.Waiting ->
            waited <- true
            Meter.charge meter 1
        | ResyncStep.Handover(previousOffset, previousLine, currentOffset, currentLine) ->
            resumeAt previousOffset previousLine currentOffset currentLine
        | ResyncStep.Finished -> finishResync ()
    }

    let finishWindow () =
        let previousTable = previousSide.Table
        let currentTable = currentSide.Table
        let mutable previousLines = 0
        let mutable currentLines = 0
        let mutable fedAnchor = false
        for index = 0 to commitCount - 1 do
            let operation: DiffOperation = ops[index]
            previousLines <- previousLines + operation.PreviousCount
            currentLines <- currentLines + operation.CurrentCount
            if operation.Anchored && (operation.Kind = OperationKind.Equal || operation.Kind = OperationKind.EndingChanged) then fedAnchor <- true
        let endsEqual = commitCount > 0 && ops[commitCount - 1].Kind = OperationKind.Equal && ops[commitCount - 1].ReenterAnchor
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
        if fedAnchor && not preserveWindowAfterCommit then windowLimit <- initialWindowLines
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
            if canStartCommon && endsEqual && builder.IsIdle && not preserveWindowAfterCommit && previousCursor < previousLength && currentCursor < currentLength then
                reenterCommon previousCursor currentCursor
            preserveWindowAfterCommit <- false

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
        | Mode.Resync -> false
        | Mode.Window ->
            match windowState with
            | WindowState.Aligning
            | WindowState.Feeding -> true
            | _ -> false
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
                && not (modeIsDone () && builder.Finished)
                && not (builder.CompletedHunks > 0 && not (modeIsDone ()) && not firstPageReturned)
                && not (Meter.overBudget meter)
                && not (Meter.quantumDue meter)

    // HDF5 superblocks can start at 512 times a power of two. The classifier samples only the first 64 KiB,
    // so the scan checks the later offsets when its coverage passes them. The signature is read directly from the
    // source, which covers a signature that straddles two chunks.
    let hdf5Signature = [| 0x89uy; 0x48uy; 0x44uy; 0x46uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]
    let nextHdf5 = [| 65_536.0; 65_536.0 |]
    let hdf5Buffer = Array.zeroCreate<byte> 8

    let hdf5Due () =
        invalidDetail.IsNone
        && (nextHdf5[0] + 8.0 <= min previousSide.Coverage previousLength
            || nextHdf5[1] + 8.0 <= min currentSide.Coverage currentLength)

    let probeHdf5 () = async {
        for side in 0..1 do
            let spec = if side = 0 then previousSpec else currentSpec
            let coverage = if side = 0 then previousSide.Coverage else currentSide.Coverage
            if spec.Source.IsSome then
                let mutable waiting = false
                while not waiting && invalidDetail.IsNone && nextHdf5[side] + 8.0 <= min coverage (float spec.ByteLength) do
                    let! status = readFully spec.Source.Value nextHdf5[side] hdf5Buffer 8
                    if status = 1 then
                        let mutable same = true
                        for index = 0 to 7 do
                            if Native.readByte hdf5Buffer index <> int hdf5Signature[index] then same <- false
                        if same then report (if side = 0 then DiffSide.Previous else DiffSide.Current) "HDF5 signature" (int64 nextHdf5[side]) None
                        nextHdf5[side] <- nextHdf5[side] * 2.0
                    else waiting <- true
    }

    let microstep (meter: Meter) = async {
        match mode with
        | Mode.Common -> do! commonStep meter
        | Mode.Resync -> do! resyncStep meter bufA bufB
        | Mode.Window ->
            match windowState with
            | WindowState.Loading -> do! loadStep meter
            | WindowState.Aligning -> alignStep meter
            | WindowState.Comparing -> do! compareStep meter
            | WindowState.Feeding -> feedStep meter
        | Mode.Done -> Meter.charge meter 1
    }

    // Page construction.
    // Stage one reads row text and builds the parts without changing any state. Stage two records
    // the page and removes the consumed queue content.

    let rowBuffer = Array.zeroCreate<byte> 32_768
    let pageReadBuffer = Array.zeroCreate<byte> (1024 * 1024)

    let makeDiffLine (line: LineRef) text = {
        Number = int64 line.Number
        Ending = LineEndingCode.toLineEnding line.Ending
        Slice = { OffsetUtf16 = 0L; TotalUtf16 = Some(int64 line.Length); Text = text; Highlights = Array.empty }
    }

    let decodeLine (encoding: TextEncoding) (line: LineRef) (bytes: byte[]) offset want =
        textCount <- 0
        if want > 0 then
            let state = Decoders.createAt encoding (int64 line.Start)
            Decoders.decode state bytes offset want (fun _ _ value ->
                if textCount < textUnits.Length then textUnits[textCount] <- uint16 value
                textCount <- textCount + 1)
            |> ignore
        let mutable count = min textCount textUnits.Length
        if count > 0 && textUnits[count - 1] >= 0xD800us && textUnits[count - 1] <= 0xDBFFus then count <- count - 1
        makeDiffLine line (Native.utf16Decode textUnits count)

    let lineContentBytes encoding (line: LineRef) =
        max 0.0 (line.Finish - line.Start - float (Widths.endingWidth encoding line.Ending))

    let createReverseCursorFromBounds side spec encoding startByte contentEnd = {
        Side = side
        Spec = spec
        Encoding = encoding
        StartByte = startByte
        EndByte = contentEnd
        Buffer = Array.zeroCreate<byte> 4_100
        Units = Array.zeroCreate<uint16> 4_100
        BlockCount = 0
        BlockPosition = -1
        WindowStart = 0.0
        WindowEnd = 0.0
        ReadPosition = 0.0
        Decoder = None
        UnitCount = 0
        Loading = false
        Aligned = false
        Exhausted = contentEnd <= startByte
    }

    let createForwardCursorFromBounds side spec encoding startByte contentEnd : TextReadCursor = {
        Side = side
        Spec = spec
        Encoding = encoding
        ContentEnd = contentEnd
        Buffer = Array.zeroCreate<byte> 4_096
        Position = startByte
        Decoder = Decoders.createAt encoding (int64 startByte)
        PendingUnit = None
    }

    let createForwardCursorFromRef side spec encoding (line: LineRef) =
        createForwardCursorFromBounds side spec encoding line.Start (line.Start + lineContentBytes encoding line)

    let createReverseCursorFromRef side spec encoding (line: LineRef) =
        createReverseCursorFromBounds side spec encoding line.Start (line.Start + lineContentBytes encoding line)

    let fillReverseWindow (cursor: ReverseTextCursor) (meter: Meter) (cancel: unit -> bool) (respectBudget: bool) = async {
        let budgetExpired () = respectBudget && Meter.overBudget meter
        if cursor.BlockPosition >= 0 then return true, false, false
        elif cursor.Exhausted then return true, false, false
        else
            if not cursor.Loading then
                let roughStart = max cursor.StartByte (cursor.EndByte - 4_096.0)
                let alignedStart =
                    match cursor.Encoding with
                    | TextEncoding.Utf16LE
                    | TextEncoding.Utf16BE -> cursor.StartByte + Math.Floor((roughStart - cursor.StartByte) / 2.0) * 2.0
                    | TextEncoding.Utf32LE
                    | TextEncoding.Utf32BE -> cursor.StartByte + Math.Floor((roughStart - cursor.StartByte) / 4.0) * 4.0
                    | _ -> roughStart
                cursor.WindowStart <- alignedStart
                cursor.WindowEnd <- cursor.EndByte
                cursor.ReadPosition <- alignedStart
                cursor.Decoder <- Some(Decoders.createAt cursor.Encoding (int64 alignedStart))
                cursor.UnitCount <- 0
                cursor.Loading <- true
            let mutable waiting = false
            let mutable failed = false
            if cursor.Loading && not cursor.Aligned then
                match cursor.Encoding with
                | TextEncoding.Utf8 ->
                    let mutable attempts = 0
                    while not cursor.Aligned && attempts < 4 && cursor.ReadPosition < cursor.WindowEnd && not waiting && not failed && not (cancel ()) && not (budgetExpired ()) do
                        match cursor.Spec.Source with
                        | None -> failed <- true
                        | Some source ->
                            let! outcome = source.ReadAt (int64 cursor.ReadPosition) cursor.Buffer 0 1
                            match outcome with
                            | ReadOutcome.Bytes 1 ->
                                Meter.chargeBytes meter 1
                                if (Native.readByte cursor.Buffer 0 &&& 0xC0) <> 0x80 || cursor.ReadPosition <= cursor.StartByte then
                                    cursor.Aligned <- true
                                else cursor.ReadPosition <- cursor.ReadPosition - 1.0
                                attempts <- attempts + 1
                            | ReadOutcome.NotYetAvailable -> waiting <- true
                            | _ -> sourceChanged (); failed <- true
                    if cursor.ReadPosition >= cursor.WindowEnd then cursor.Aligned <- true
                | TextEncoding.Utf16LE
                | TextEncoding.Utf16BE ->
                    if cursor.ReadPosition + 2.0 <= cursor.WindowEnd then
                        match cursor.Spec.Source with
                        | None -> failed <- true
                        | Some source ->
                            let probe = Array.zeroCreate<byte> 2
                            let! outcome = source.ReadAt (int64 cursor.ReadPosition) probe 0 2
                            match outcome with
                            | ReadOutcome.Bytes 2 ->
                                Meter.chargeBytes meter 2
                                let value = Widths.unitAt cursor.Encoding probe 0
                                if value >= 0xDC00 && value <= 0xDFFF && cursor.ReadPosition > cursor.StartByte then
                                    cursor.ReadPosition <- cursor.ReadPosition - 2.0
                                cursor.Aligned <- true
                            | ReadOutcome.NotYetAvailable -> waiting <- true
                            | _ -> sourceChanged (); failed <- true
                    else cursor.Aligned <- true
                | _ -> cursor.Aligned <- true
                if cursor.Aligned && not failed then
                    cursor.WindowStart <- cursor.ReadPosition
                    cursor.Decoder <- Some(Decoders.createAt cursor.Encoding (int64 cursor.ReadPosition))
            while cursor.Loading && cursor.ReadPosition < cursor.WindowEnd && not waiting && not failed && not (cancel ()) && not (budgetExpired ()) do
                let byteCount = int (min (float cursor.Buffer.Length) (cursor.WindowEnd - cursor.ReadPosition))
                match cursor.Spec.Source, cursor.Decoder with
                | None, _ -> failed <- true
                | _, None -> failed <- true
                | Some source, Some decoder ->
                    let! outcome = source.ReadAt (int64 cursor.ReadPosition) cursor.Buffer 0 byteCount
                    match outcome with
                    | ReadOutcome.Bytes actual when actual > 0 ->
                        let sink _ _ value =
                            if cursor.UnitCount < cursor.Units.Length then
                                cursor.Units[cursor.UnitCount] <- uint16 value
                                cursor.UnitCount <- cursor.UnitCount + 1
                            else failed <- true
                        let next, result = Decoders.decode decoder cursor.Buffer 0 actual sink
                        cursor.Decoder <- Some next
                        cursor.ReadPosition <- float next.AbsoluteOffset
                        Meter.chargeBytes meter actual
                        match result with
                        | Error error -> reportDecodeError (if cursor.Side = DiffSide.Previous then 0 else 1) error; failed <- true
                        | Ok _ -> ()
                    | ReadOutcome.NotYetAvailable -> waiting <- true
                    | ReadOutcome.EndOfSource -> sourceChanged (); failed <- true
                    | ReadOutcome.Bytes _ -> waiting <- true
            if failed then cursor.Loading <- false; return false, true, false
            elif waiting || cancel () || budgetExpired () then return false, false, true
            elif cursor.Loading && cursor.ReadPosition >= cursor.WindowEnd then
                cursor.EndByte <- cursor.WindowStart
                cursor.BlockCount <- cursor.UnitCount
                cursor.BlockPosition <- cursor.UnitCount - 1
                cursor.Exhausted <- cursor.EndByte <= cursor.StartByte
                cursor.Loading <- false
                cursor.Aligned <- false
                return true, false, false
            else return false, false, true
    }

    let readLine (spec: SourceSpec) (encoding: TextEncoding) (line: LineRef) : Async<DiffLine option> = async {
        let contentBytes = line.Finish - line.Start - Widths.endingWidth encoding line.Ending
        let want = int (min 32_768.0 (max 0.0 contentBytes))
        let! status = if want = 0 then async.Return 1 else readFully spec.Source.Value line.Start rowBuffer want
        if status <> 1 then
            sourceChanged ()
            return None
        else
            return Some(decodeLine encoding line rowBuffer 0 want)
    }

    let decodeUtf8BatchLines (lines: LineRef[]) firstIndex finishIndex (decoded: DiffLine[]) (text: string) =
        let mutable textOffset = 0
        for lineIndex = firstIndex to finishIndex - 1 do
            let line = lines[lineIndex]
            let lineLength = int line.Length
            let available = max 0 (min lineLength (text.Length - textOffset))
            let mutable count = min textUnits.Length available
            if count > 0 && Char.IsHighSurrogate(text[textOffset + count - 1]) then count <- count - 1
            let lineText = if count = 0 then "" else text.Substring(textOffset, count)
            decoded[lineIndex] <- makeDiffLine line lineText
            textOffset <- textOffset + lineLength + (if line.Ending = LineEndingCode.CRLF then 2 else if line.Ending = LineEndingCode.NoEnding then 0 else 1)

    let decodeBatchLines (encoding: TextEncoding) (lines: LineRef[]) firstIndex finishIndex (decoded: DiffLine[]) (batchStart: float) =
        for lineIndex = firstIndex to finishIndex - 1 do
            let line = lines[lineIndex]
            let offset = int (line.Start - batchStart)
            let want = int (min 32_768.0 (lineContentBytes encoding line))
            decoded[lineIndex] <- decodeLine encoding line pageReadBuffer offset want

    let findBatchEnd (encoding: TextEncoding) (lines: LineRef[]) index batchStart initialFinish =
        let mutable batchFinish = initialFinish
        let mutable stop = false
        let mutable finishIndex = index + 1
        while finishIndex < lines.Length && not stop do
            let next = lines[finishIndex]
            if next.Start = batchFinish
               && lineContentBytes encoding next <= 32_768.0
               && next.Finish - batchStart <= float pageReadBuffer.Length then
                batchFinish <- next.Finish
                finishIndex <- finishIndex + 1
            else stop <- true
        finishIndex, batchFinish

    let readLines (spec: SourceSpec) (encoding: TextEncoding) (lines: LineRef[]) (cancel: unit -> bool) : Async<DiffLine[] option> = async {
        let decoded = Array.zeroCreate<DiffLine> lines.Length
        let mutable index = 0
        let mutable failed = false
        let readIndividually () = async {
            let! line = readLine spec encoding lines[index]
            match line with
            | Some value -> decoded[index] <- value; index <- index + 1
            | None -> failed <- true
        }
        while index < lines.Length && not failed && not (cancel ()) do
            let first = lines[index]
            let firstContentBytes = lineContentBytes encoding first
            let firstSpan = first.Finish - first.Start
            if firstContentBytes > 32_768.0 || firstSpan > float pageReadBuffer.Length then
                do! readIndividually ()
            else
                let batchStart = first.Start
                let finishIndex, batchFinish = findBatchEnd encoding lines index batchStart first.Finish
                let byteCount = int (batchFinish - batchStart)
                let! status = if byteCount = 0 then async.Return 1 else readFully spec.Source.Value batchStart pageReadBuffer byteCount
                if status <> 1 then
                    sourceChanged ()
                    failed <- true
                else
                    let validUtf8 =
                        match encoding with
                        | TextEncoding.Utf8 -> Native.isUtf8 pageReadBuffer 0 byteCount
                        | _ -> false
                    if validUtf8 then
                        let text = Native.utf8Decode pageReadBuffer 0 byteCount
                        decodeUtf8BatchLines lines index finishIndex decoded text
                    else
                        decodeBatchLines encoding lines index finishIndex decoded batchStart
                    index <- finishIndex
        if failed || cancel () then return None else return Some decoded
    }

    let activePreviewView sideIndex number (state: ScannerState) (table: LineTable) (byteLength: int64) =
        let active =
            not state.IsComplete
            && (state.LineHasText || state.LineLengthUtf16 > 0.0 || state.PendingCR || state.PendingHigh <> 0 || state.PendingCount > 0)
        if not active then None
        else
            let knownEnd = min (float byteLength) (float state.StartOffset + state.ValidatedBytes)
            let width = float (Widths.unitBytes (if sideIndex = 0 then previousEncoding else currentEncoding))
            let contentEnd = if state.PendingCR then min knownEnd (state.PendingCREnd - width) else knownEnd
            Some {
                SideIndex = sideIndex
                Number = number
                StartOffset = state.LineStart
                ContentEnd = max state.LineStart contentEnd
                KnownEnd = knownEnd
                KnownUtf16 = Some(int64 state.LineLengthUtf16)
                TotalUtf16 = None
                Ending = LineEnding.NoEnding
                Active = true
                EndOfFile = false
            }

    let previewViewFromScan sideIndex (scan: ScanSide) =
        let lineNumber = scan.Table.LineBase + int64 scan.Table.Count
        activePreviewView sideIndex lineNumber scan.Scanner scan.Table scan.Spec.ByteLength

    let completedPreviewView sideIndex number (scan: ScanSide) =
        let table = scan.Table
        if table.Count = 0 || table.LineBase + int64 table.Count - 1L <> number then None
        else
            let index = table.Count - 1
            let endingCode = table.EndingCode index
            let ending = LineEndingCode.toLineEnding endingCode
            let encoding = if sideIndex = 0 then previousEncoding else currentEncoding
            let finish = table.Finish index
            let contentEnd = finish - Widths.endingWidth encoding endingCode
            Some {
                SideIndex = sideIndex
                Number = number
                StartOffset = table.Start index
                ContentEnd = max (table.Start index) contentEnd
                KnownEnd = finish
                KnownUtf16 = Some(int64 (table.Length index))
                TotalUtf16 = Some(int64 (table.Length index))
                Ending = ending
                Active = false
                EndOfFile = scan.Finished && ending = LineEnding.NoEnding
            }

    let commonPreviewView sideIndex startOffset endOffset number =
        let width = float (Widths.unitBytes (if sideIndex = 0 then previousEncoding else currentEncoding))
        let contentEnd = if pendingCR then endOffset - width else endOffset
        if endOffset <= startOffset && not pendingCR then None
        else
            Some {
                SideIndex = sideIndex
                Number = number
                StartOffset = startOffset
                ContentEnd = max startOffset contentEnd
                KnownEnd = endOffset
                KnownUtf16 = None
                TotalUtf16 = None
                Ending = LineEnding.NoEnding
                Active = true
                EndOfFile = false
            }

    let releasePendingPreviewCursor sideIndex =
        match pendingPreviewCursors[sideIndex] with
        | Some cursor ->
            if cursor.Reservation > 0L then ledger.Release(AllocationCategory.ResponseData, cursor.Reservation)
            pendingPreviewCursors[sideIndex] <- None
        | None -> ()

    let readPreviewSnippet (view: PendingLineView) (offset: int64) = async {
        let spec = if view.SideIndex = 0 then previousSpec else currentSpec
        let encoding = if view.SideIndex = 0 then previousEncoding else currentEncoding
        let cursor =
            match pendingPreviewCursors[view.SideIndex] with
            | Some current when offset <> 0L
                                && current.Number = view.Number
                                && current.StartOffset = view.StartOffset
                                && current.Offset = offset
                                && current.BytePosition <= min view.ContentEnd view.KnownEnd -> current
            | _ ->
                releasePendingPreviewCursor view.SideIndex
                let reservation = 5_120L
                let reserved = offset <> 0L && ledger.TryReserve(AllocationCategory.ResponseData, reservation)
                let created = {
                    SideIndex = view.SideIndex
                    Number = view.Number
                    StartOffset = view.StartOffset
                    Offset = offset
                    Decoder = Decoders.createAt encoding (int64 view.StartOffset)
                    BytePosition = view.StartOffset
                    DecodedUnits = 0.0
                    Captured = 0
                    Units = Array.zeroCreate<uint16> 2_048
                    Buffer = Array.zeroCreate<byte> 1_024
                    Reservation = if reserved then reservation else 0L
                }
                if reserved then pendingPreviewCursors[view.SideIndex] <- Some created
                created
        let mutable waiting = false
        let mutable failed = false
        let requestedOffset = float offset
        let requestedEnd = requestedOffset + 2_048.0
        while cursor.Captured < 2_048
              && cursor.DecodedUnits < requestedEnd
              && cursor.BytePosition < view.ContentEnd
              && cursor.BytePosition < view.KnownEnd
              && not waiting
              && not failed do
            let count = int (min (float cursor.Buffer.Length) (min (view.ContentEnd - cursor.BytePosition) (view.KnownEnd - cursor.BytePosition)))
            if count <= 0 then waiting <- true
            else
                match spec.Source with
                | None -> waiting <- true
                | Some source ->
                    let! outcome = source.ReadAt (int64 cursor.BytePosition) cursor.Buffer 0 count
                    match outcome with
                    | ReadOutcome.Bytes actual when actual > 0 ->
                        let sink _ _ value =
                            if cursor.DecodedUnits >= requestedOffset && cursor.DecodedUnits < requestedEnd && cursor.Captured < cursor.Units.Length then
                                cursor.Units[cursor.Captured] <- uint16 value
                                cursor.Captured <- cursor.Captured + 1
                            cursor.DecodedUnits <- cursor.DecodedUnits + 1.0
                        let next, result = Decoders.decode cursor.Decoder cursor.Buffer 0 actual sink
                        cursor.Decoder <- next
                        cursor.BytePosition <- float next.AbsoluteOffset
                        match result with
                        | Error error ->
                            reportDecodeError view.SideIndex error
                            failed <- true
                        | Ok _ -> ()
                    | ReadOutcome.NotYetAvailable -> waiting <- true
                    | ReadOutcome.EndOfSource -> sourceChanged (); failed <- true
                    | ReadOutcome.Bytes _ -> waiting <- true
        let captured =
            if cursor.Captured > 0 && cursor.Units[cursor.Captured - 1] >= 0xD800us && cursor.Units[cursor.Captured - 1] <= 0xDBFFus then
                cursor.Captured - 1
            else cursor.Captured
        let text = Native.utf16Decode cursor.Units captured
        let endState =
            match view.TotalUtf16 with
            | Some total when offset + int64 captured < total -> SnippetEnd.Truncated
            | Some _ -> if view.EndOfFile then SnippetEnd.EndOfFile else SnippetEnd.LineEnd
            | None when cursor.DecodedUnits > requestedOffset + float captured || cursor.BytePosition < min view.ContentEnd view.KnownEnd -> SnippetEnd.Truncated
            | None -> SnippetEnd.MoreTextPending
        return { Line = view.Number; OffsetUtf16 = offset; Text = text; End = endState }
    }

    let boundPendingPreview (value: PendingPreview option) =
        match value with
        | None -> None
        | Some preview ->
            let stringCost =
                [| preview.Previous; preview.Current |]
                |> Array.sumBy (function PendingSide.Snippet snippet -> int64 snippet.Text.Length * 2L | _ -> 0L)
            if stringCost + 64L > 16L * 1024L then None else Some preview

    let makePendingPreview (active: PendingLineView option[]) (completed: (int64 -> PendingLineView option)[]) (exhausted: int64 option[]) (mismatchLines: (int64 * int64 * int64 option) option) = async {
        let selected = Array.copy active
        for sideIndex = 0 to 1 do
            if selected[sideIndex].IsNone then
                let other = 1 - sideIndex
                match selected[other] with
                | Some peer ->
                    let! pairedNumber =
                        match mismatchLines with
                        | Some(previousNumber, currentNumber, _) when sideIndex = 0 && peer.Number = currentNumber -> async.Return(Some previousNumber)
                        | Some(previousNumber, currentNumber, _) when sideIndex = 1 && peer.Number = previousNumber -> async.Return(Some currentNumber)
                        | _ -> async.Return(Some peer.Number)
                    pairedNumber |> Option.iter (fun number -> selected[sideIndex] <- completed[sideIndex] number)
                | None -> ()
        if selected[0].IsNone && selected[1].IsNone then
            if exhausted[0].IsSome && exhausted[1].IsSome then return boundPendingPreview None
            else
                let side = function Some count -> PendingSide.Exhausted count | None -> PendingSide.NoActiveLine
                return boundPendingPreview (Some { Previous = side exhausted[0]; Current = side exhausted[1]; Mismatch = None })
        else
            let mutable knownMismatch = None
            let offsets = [| 0L; 0L |]
            match mismatchLines, selected[0], selected[1] with
            | Some(previousNumber, currentNumber, Some unitOffset), Some previousView, Some currentView
                when previousView.Number = previousNumber
                     && currentView.Number = currentNumber
                     && (previousView.KnownUtf16 |> Option.exists (fun length -> length > unitOffset)
                         || previousView.TotalUtf16 = Some unitOffset)
                     && (currentView.KnownUtf16 |> Option.exists (fun length -> length > unitOffset)
                         || currentView.TotalUtf16 = Some unitOffset) ->
                knownMismatch <- Some unitOffset
                offsets[0] <- max 0L (unitOffset - 256L)
                offsets[1] <- max 0L (unitOffset - 256L)
            | _ -> ()
            let readOptionalSnippet sideIndex view = async {
                match view with
                | Some value ->
                    let! snippet = readPreviewSnippet value offsets[sideIndex]
                    return Some snippet
                | None -> return None
            }
            let! previousSnippet = readOptionalSnippet 0 selected[0]
            let! currentSnippet = readOptionalSnippet 1 selected[1]
            let previousSide =
                match previousSnippet, exhausted[0] with
                | Some snippet, _ -> PendingSide.Snippet snippet
                | None, Some count -> PendingSide.Exhausted count
                | _ -> PendingSide.NoActiveLine
            let currentSideValue =
                match currentSnippet, exhausted[1] with
                | Some snippet, _ -> PendingSide.Snippet snippet
                | None, Some count -> PendingSide.Exhausted count
                | _ -> PendingSide.NoActiveLine
            let mutable mismatch = None
            match mismatchLines, selected[0], selected[1], previousSnippet, currentSnippet with
            | Some(previousNumber, currentNumber, Some unitOffset), Some previousView, Some currentView, Some previousText, Some currentText
                when previousView.Number = previousNumber
                     && currentView.Number = currentNumber
                     && knownMismatch.IsSome
                     && unitOffset >= previousText.OffsetUtf16
                     && unitOffset <= previousText.OffsetUtf16 + int64 previousText.Text.Length
                     && unitOffset >= currentText.OffsetUtf16
                     && unitOffset <= currentText.OffsetUtf16 + int64 currentText.Text.Length ->
                mismatch <- Some { PreviousOffsetUtf16 = unitOffset; CurrentOffsetUtf16 = unitOffset }
            | _ -> ()
            let preview = { Previous = previousSide; Current = currentSideValue; Mismatch = mismatch }
            if invalidDetail.IsSome || failure.IsSome then return boundPendingPreview None
            else return boundPendingPreview (Some preview)
    }

    let pagePreviewCandidates () =
        match mode, resync with
        | Mode.Resync, Some engine -> engine.PreviewSides
        | Mode.Window, _ -> [| [| previousSide; currentSide |] |]
        | _ -> [||]

    let pendingPagePreview () = async {
        let active = Array.create 2 None
        let complete = Array.create 2 (fun _ -> None)
        let ended = Array.create 2 None
        if mode = Mode.Common then
            let previousNumber = int64 (builder.NextPrevious + commonLines)
            let currentNumber = int64 (builder.NextCurrent + commonLines)
            active[0] <- commonPreviewView 0 lineStart pos previousNumber
            active[1] <- commonPreviewView 1 (lineStart + delta) (pos + delta) currentNumber
            if active[0].IsNone && previousSpec.Source.IsNone then ended[0] <- Some 0L
            if active[1].IsNone && currentSpec.Source.IsNone then ended[1] <- Some 0L
        else
            let candidateSets = pagePreviewCandidates ()
            for sideIndex = 0 to 1 do
                let scans = candidateSets |> Array.collect (fun set -> set) |> Array.filter (fun scan -> (if sideIndex = 0 then scan.Side = DiffSide.Previous else scan.Side = DiffSide.Current))
                let views = scans |> Array.choose (previewViewFromScan sideIndex)
                if views.Length > 0 then
                    active[sideIndex] <-
                        Some(
                            Array.minBy
                                (fun view -> view.Number, view.StartOffset, -view.KnownEnd)
                                views
                        )
                complete[sideIndex] <- fun number ->
                    scans
                    |> Array.choose (completedPreviewView sideIndex number)
                    |> Array.tryHead
                if scans |> Array.exists (fun scan -> scan.Spec.Source.IsNone) then ended[sideIndex] <- Some 0L
                else
                    let finished = scans |> Array.filter (fun scan -> scan.Finished)
                    if finished.Length > 0 then ended[sideIndex] <- Some(finished |> Array.map (fun scan -> scan.Table.LineBase + int64 scan.Table.Count) |> Array.max)
        return! makePendingPreview active complete ended pairedMismatch
    }

    let pendingSeekPreview (cursor: SeekCursor) = async {
        let active = Array.create 2 None
        let complete = Array.create 2 (fun _ -> None)
        let ended = Array.create 2 None
        let sideIndex = cursor.SideIndex
        let scans = [| previousSide; currentSide |]
        let scan = scans[sideIndex]
        let spec = if sideIndex = 0 then previousSpec else currentSpec
        active[sideIndex] <- activePreviewView sideIndex (int64 cursor.LineNumber) cursor.State scan.Table spec.ByteLength
        for index = 0 to 1 do
            if index <> sideIndex then
                complete[index] <- fun number -> scans[index] |> completedPreviewView index number
                active[index] <- previewViewFromScan index scans[index]
                if scans[index].Finished then ended[index] <- Some(scans[index].Table.LineBase + int64 scans[index].Table.Count)
                elif scans[index].Spec.Source.IsNone then ended[index] <- Some 0L
        return! makePendingPreview active complete ended pairedMismatch
    }

    let presentPageResult result = async {
        match result with
        | EngineResult.Ok(Resumable.Scanning(progress, continuation, _)) ->
            let! preview = pendingPagePreview ()
            if invalidDetail.IsSome then return failContent ()
            elif failure.IsSome then
                let code, message = failure.Value
                return EngineResult.Failed(code, message, None)
            else return EngineResult.Ok(Resumable.Scanning(progress, continuation, preview))
        | EngineResult.Ok(Resumable.Ready page) when page.NextCursor.IsSome ->
            let! preview = pendingPagePreview ()
            if invalidDetail.IsSome then return failContent ()
            elif failure.IsSome then
                let code, message = failure.Value
                return EngineResult.Failed(code, message, None)
            else
                let previewBytes =
                    match preview with
                    | Some value ->
                        [| value.Previous; value.Current |]
                        |> Array.sumBy (function PendingSide.Snippet snippet -> snippet.Text.Length * 3 | _ -> 0)
                    | None -> 0
                let partBytes = page.Parts |> Array.sumBy (fun part -> max 0 (sizer part))
                let pending = if partBytes + previewBytes + 512 <= config.PageMaxBytes then preview else None
                return EngineResult.Ok(Resumable.Ready { page with Pending = pending })
        | EngineResult.Ok(Resumable.Ready page) ->
            releasePendingPreviewCursor 0
            releasePendingPreviewCursor 1
            return EngineResult.Ok(Resumable.Ready { page with Pending = None })
        | other -> return other
    }

    let readPairBlock (cursor: TextReadCursor) (target: uint16[]) (meter: Meter) (cancel: unit -> bool) = async {
        if cancel () || Meter.overBudget meter then return 0, false, true, false
        elif cursor.Position >= cursor.ContentEnd then return 0, false, false, true
        else
            let byteCount = int (min (float cursor.Buffer.Length) (cursor.ContentEnd - cursor.Position))
            match cursor.Spec.Source with
            | None -> return 0, true, false, false
            | Some source ->
                let! outcome = source.ReadAt (int64 cursor.Position) cursor.Buffer 0 byteCount
                match outcome with
                | ReadOutcome.Bytes actual when actual > 0 ->
                    let mutable count = 0
                    let mutable failed = false
                    let sink _ _ value =
                        if count < target.Length then
                            target[count] <- uint16 value
                            count <- count + 1
                        else failed <- true
                    let next, result = Decoders.decode cursor.Decoder cursor.Buffer 0 actual sink
                    cursor.Decoder <- next
                    cursor.Position <- float next.AbsoluteOffset
                    Meter.chargeBytes meter actual
                    match result with
                    | Error error ->
                        reportDecodeError (if cursor.Side = DiffSide.Previous then 0 else 1) error
                        return count, true, false, false
                    | Ok _ -> return count, failed, false, cursor.Position >= cursor.ContentEnd
                | ReadOutcome.NotYetAvailable -> return 0, false, true, false
                | ReadOutcome.EndOfSource -> sourceChanged (); return 0, true, false, false
                | ReadOutcome.Bytes _ -> return 0, false, true, false
    }

    let readLongBlock (cursor: TextReadCursor) (target: uint16[]) initialCount (meter: Meter) (cancel: unit -> bool) = async {
        let wanted = min 4_096 target.Length
        let mutable count = initialCount
        let mutable failed = false
        let mutable blocked = false
        match cursor.PendingUnit with
        | Some value when count < wanted ->
            target[count] <- value
            count <- count + 1
            cursor.PendingUnit <- None
        | _ -> ()
        let byteWidth =
            match cursor.Encoding with
            | TextEncoding.Utf16LE
            | TextEncoding.Utf16BE -> 2
            | TextEncoding.Utf32LE
            | TextEncoding.Utf32BE -> 4
            | _ -> 1
        while count < wanted && not failed && not blocked && not (cancel ()) && not (Meter.overBudget meter) && cursor.Position < cursor.ContentEnd do
            let remainingUnits = wanted - count
            let byteCount = int (min (float cursor.Buffer.Length) (min (cursor.ContentEnd - cursor.Position) (float (remainingUnits * byteWidth))))
            if byteCount <= 0 then failed <- true
            else
                match cursor.Spec.Source with
                | None -> failed <- true
                | Some source ->
                    let! outcome = source.ReadAt (int64 cursor.Position) cursor.Buffer 0 byteCount
                    match outcome with
                    | ReadOutcome.Bytes actual when actual > 0 ->
                        let isUtf32 = byteWidth = 4
                        let mutable overflow: uint16 option = None
                        // A UTF-32 read holds up to one code point per unit asked for, so it can hold more than
                        // the block has room for. The first code point that does not fit is where the next read starts.
                        let mutable refusedAt = -1.0
                        let sink (start: int64) _ value =
                            if count < wanted then
                                target[count] <- uint16 value
                                count <- count + 1
                            elif not isUtf32 then
                                if overflow.IsNone then overflow <- Some(uint16 value)
                            elif refusedAt < 0.0 then
                                if overflow.IsNone && value >= 0xDC00 && value <= 0xDFFF then overflow <- Some(uint16 value)
                                else refusedAt <- float start
                        let next, decoded = Decoders.decode cursor.Decoder cursor.Buffer 0 actual sink
                        if refusedAt >= 0.0 then
                            Meter.chargeBytes meter (int (refusedAt - cursor.Position))
                            cursor.Position <- refusedAt
                            cursor.Decoder <- Decoders.createAt cursor.Encoding (int64 refusedAt)
                            cursor.PendingUnit <- overflow
                        else
                            cursor.Decoder <- next
                            cursor.Position <- float next.AbsoluteOffset
                            cursor.PendingUnit <- overflow
                            Meter.chargeBytes meter actual
                            match decoded with
                            | Error error ->
                                reportDecodeError (if cursor.Side = DiffSide.Previous then 0 else 1) error
                                failed <- true
                            | Ok _ -> ()
                    | ReadOutcome.NotYetAvailable -> blocked <- true
                    | ReadOutcome.EndOfSource -> sourceChanged (); failed <- true
                    | ReadOutcome.Bytes _ -> blocked <- true
        if not failed && not blocked && count < wanted && cursor.Position < cursor.ContentEnd && (cancel () || Meter.overBudget meter) then blocked <- true
        return count, failed, blocked, cursor.Position >= cursor.ContentEnd && cursor.PendingUnit.IsNone
    }

    let releasePendingLongPair () =
        match pendingLongPair with
        | Some pending ->
            if pending.Reserved then
                ledger.Release(AllocationCategory.AlignmentScratch, pending.Reservation)
                pending.Reserved <- false
            pending.PreviousCursor.Buffer <- Array.empty
            pending.CurrentCursor.Buffer <- Array.empty
            pending.PreviousReverse.Buffer <- Array.empty
            pending.CurrentReverse.Buffer <- Array.empty
            pendingLongPair <- None
        | None -> ()

    let createPendingLongPair item rowIndex previousRef currentRef previousLine currentLine =
        let reservation = 64L * 1024L
        if not (ledger.TryReserve(AllocationCategory.AlignmentScratch, reservation)) then None
        else
            try
                Some {
                    Item = item
                    RowIndex = rowIndex
                    Previous = previousRef
                    Current = currentRef
                    PreviousLine = previousLine
                    CurrentLine = currentLine
                    PreviousCursor = createForwardCursorFromRef DiffSide.Previous previousSpec previousEncoding previousRef
                    CurrentCursor = createForwardCursorFromRef DiffSide.Current currentSpec currentEncoding currentRef
                    PreviousBlock = Array.zeroCreate<uint16> 4_100
                    CurrentBlock = Array.zeroCreate<uint16> 4_100
                    PreviousBlockCount = 0
                    CurrentBlockCount = 0
                    PreviousBlockPosition = 0
                    CurrentBlockPosition = 0
                    PreviousBlockComplete = false
                    CurrentBlockComplete = false
                    PreviousFinished = false
                    CurrentFinished = false
                    PreviousHistory = Array.zeroCreate<uint16> 128
                    CurrentHistory = Array.zeroCreate<uint16> 128
                    HistoryCount = 0
                    UnitOffset = 0.0
                    PrefixLength = 0.0
                    PrefixOpen = true
                    Found = None
                    PreviousReverse = createReverseCursorFromRef DiffSide.Previous previousSpec previousEncoding previousRef
                    CurrentReverse = createReverseCursorFromRef DiffSide.Current currentSpec currentEncoding currentRef
                    ReverseDone = false
                    SuffixLength = 0.0
                    LastSuffixUnit = 0us
                    HasLastSuffixUnit = false
                    Reservation = reservation
                    Reserved = true
                    Failed = false
                }
            with error ->
                ledger.Release(AllocationCategory.AlignmentScratch, reservation)
                raise error

    let pendingLongPairFor item rowIndex previousRef currentRef previousLine currentLine =
        let create () =
            pendingLongPair <- createPendingLongPair item rowIndex previousRef currentRef previousLine currentLine
            pendingLongPair
        match pendingLongPair with
        | Some pending when obj.ReferenceEquals(pending.Item, item) && pending.RowIndex = rowIndex ->
            Some pending
        | Some _ ->
            releasePendingLongPair ()
            create ()
        | None -> create ()

    let keepLongPairTail (history: uint16[]) (historyCount: int) (block: uint16[]) (blockCount: int) =
        let result = Array.zeroCreate<uint16> 128
        let take = min 128 blockCount
        if take = 128 then Array.blit block (blockCount - take) result 0 take
        else
            let fromHistory = min historyCount (128 - take)
            if fromHistory > 0 then Array.blit history (historyCount - fromHistory) result 0 fromHistory
            if take > 0 then Array.blit block 0 result fromHistory take
        result, min 128 (min historyCount 128 + blockCount)

    let compareLongPairPrefix (pending: PendingLongPair) (meter: Meter) (cancel: unit -> bool) shared =
        let mutable compared = 0
        let mutable mismatch = false
        let mutable blocked = false
        let mutable canceled = false
        while compared < shared && not mismatch && not blocked && not canceled && not (Meter.overBudget meter) do
            if cancel () then canceled <- true
            else
                let segment = min 64 (shared - compared)
                let equal =
                    Native.equalUnitPrefix
                        pending.PreviousBlock
                        pending.PreviousBlockPosition
                        pending.CurrentBlock
                        pending.CurrentBlockPosition
                        segment
                Meter.chargeBytes meter (segment * 2)
                if equal > 0 then
                    pending.PreviousBlockPosition <- pending.PreviousBlockPosition + equal
                    pending.CurrentBlockPosition <- pending.CurrentBlockPosition + equal
                    pending.PrefixLength <- pending.PrefixLength + float equal
                    compared <- compared + equal
                if equal < segment then mismatch <- true
                elif compared < shared && cancel () then canceled <- true
        compared, mismatch, blocked, canceled

    let compareLongPairSuffix (pending: PendingLongPair) (meter: Meter) (cancel: unit -> bool) =
        let mutable mismatch = false
        let mutable blocked = false
        let mutable canceled = false
        while pending.PreviousReverse.BlockPosition >= 0
              && pending.CurrentReverse.BlockPosition >= 0
              && not mismatch
              && not blocked
              && not canceled
              && not (Meter.overBudget meter) do
            if cancel () then canceled <- true
            else
                let segment = min 64 (min (pending.PreviousReverse.BlockPosition + 1) (pending.CurrentReverse.BlockPosition + 1))
                let equal =
                    Native.equalUnitSuffix
                        pending.PreviousReverse.Units
                        (pending.PreviousReverse.BlockPosition + 1)
                        pending.CurrentReverse.Units
                        (pending.CurrentReverse.BlockPosition + 1)
                        segment
                Meter.chargeBytes meter (segment * 2)
                if equal > 0 then
                    pending.SuffixLength <- pending.SuffixLength + float equal
                    pending.LastSuffixUnit <- pending.PreviousReverse.Units[pending.PreviousReverse.BlockPosition - equal + 1]
                    pending.HasLastSuffixUnit <- true
                    pending.PreviousReverse.BlockPosition <- pending.PreviousReverse.BlockPosition - equal
                    pending.CurrentReverse.BlockPosition <- pending.CurrentReverse.BlockPosition - equal
                if equal < segment then
                    mismatch <- true
                    let previousUnit = pending.PreviousReverse.Units[pending.PreviousReverse.BlockPosition]
                    let currentUnit = pending.CurrentReverse.Units[pending.CurrentReverse.BlockPosition]
                    if pending.SuffixLength > 0.0
                       && pending.HasLastSuffixUnit
                       && pending.LastSuffixUnit >= 0xDC00us && pending.LastSuffixUnit <= 0xDFFFus
                       && previousUnit >= 0xD800us && previousUnit <= 0xDBFFus
                       && currentUnit >= 0xD800us && currentUnit <= 0xDBFFus then
                        pending.SuffixLength <- pending.SuffixLength - 1.0
                elif cancel () then canceled <- true
        mismatch, blocked, canceled

    let advanceLongPair (pending: PendingLongPair) (meter: Meter) (cancel: unit -> bool) = async {
        let mutable blocked = false
        let mutable canceled = false

        let makeFound rawPrefix previousAt currentAt =
            let leading = min pending.HistoryCount 128
            let previousValues = Array.zeroCreate<uint16> (leading + pending.PreviousBlockCount)
            let currentValues = Array.zeroCreate<uint16> (leading + pending.CurrentBlockCount)
            if leading > 0 then
                Array.blit pending.PreviousHistory (pending.HistoryCount - leading) previousValues 0 leading
                Array.blit pending.CurrentHistory (pending.HistoryCount - leading) currentValues 0 leading
            if pending.PreviousBlockCount > 0 then Array.blit pending.PreviousBlock 0 previousValues leading pending.PreviousBlockCount
            if pending.CurrentBlockCount > 0 then Array.blit pending.CurrentBlock 0 currentValues leading pending.CurrentBlockCount
            let mutable prefix = rawPrefix
            match previousAt, currentAt with
            | Some previousUnit, Some currentUnit ->
                let previousBefore =
                    if pending.PreviousBlockPosition > 0 then Some pending.PreviousBlock[pending.PreviousBlockPosition - 1]
                    elif pending.HistoryCount > 0 then Some pending.PreviousHistory[pending.HistoryCount - 1]
                    else None
                match previousBefore with
                | Some value when value >= 0xD800us && value <= 0xDBFFus
                                   && previousUnit >= 0xDC00us && previousUnit <= 0xDFFFus
                                   && currentUnit >= 0xDC00us && currentUnit <= 0xDFFFus ->
                    prefix <- prefix - 1.0
                | _ -> ()
            | _ -> ()
            let baseOffset = pending.UnitOffset - float leading
            let startOffset = rawPrefix - float leading
            let previousSlice = LongLine.sliceAt pending.PreviousLine previousValues previousValues.Length baseOffset startOffset
            let currentSlice = LongLine.sliceAt pending.CurrentLine currentValues currentValues.Length baseOffset startOffset
            pending.PrefixLength <- prefix
            pending.PrefixOpen <- false
            pending.Found <- Some(previousSlice, currentSlice)

        let fillPrevious () = async {
            if (pending.PreviousBlockPosition >= pending.PreviousBlockCount || not pending.PreviousBlockComplete)
               && not pending.PreviousFinished && not (Meter.overBudget meter) then
                if pending.PreviousBlockPosition >= pending.PreviousBlockCount then
                    pending.PreviousBlockPosition <- 0
                    pending.PreviousBlockCount <- 0
                    pending.PreviousBlockComplete <- false
                let! count, missing, waiting, ended = readLongBlock pending.PreviousCursor pending.PreviousBlock pending.PreviousBlockCount meter cancel
                pending.PreviousBlockCount <- count
                pending.PreviousBlockComplete <- not waiting && (count >= min 4_096 pending.PreviousBlock.Length || ended)
                pending.PreviousFinished <- ended
                pending.Failed <- pending.Failed || missing
                if waiting then blocked <- true
            return ()
        }

        let fillCurrent () = async {
            if (pending.CurrentBlockPosition >= pending.CurrentBlockCount || not pending.CurrentBlockComplete)
               && not pending.CurrentFinished && not (Meter.overBudget meter) then
                if pending.CurrentBlockPosition >= pending.CurrentBlockCount then
                    pending.CurrentBlockPosition <- 0
                    pending.CurrentBlockCount <- 0
                    pending.CurrentBlockComplete <- false
                let! count, missing, waiting, ended = readLongBlock pending.CurrentCursor pending.CurrentBlock pending.CurrentBlockCount meter cancel
                pending.CurrentBlockCount <- count
                pending.CurrentBlockComplete <- not waiting && (count >= min 4_096 pending.CurrentBlock.Length || ended)
                pending.CurrentFinished <- ended
                pending.Failed <- pending.Failed || missing
                if waiting then blocked <- true
            return ()
        }

        while pending.Found.IsNone && pending.PrefixOpen && not pending.Failed && not blocked && not canceled && not (Meter.overBudget meter) do
            do! fillPrevious ()
            do! fillCurrent ()
            if cancel () then canceled <- true
            elif pending.Failed || blocked || Meter.overBudget meter then ()
            else
                let previousAvailable = pending.PreviousBlockCount - pending.PreviousBlockPosition
                let currentAvailable = pending.CurrentBlockCount - pending.CurrentBlockPosition
                let shared = min previousAvailable currentAvailable
                let compared, mismatch, compareBlocked, compareCanceled = compareLongPairPrefix pending meter cancel shared
                blocked <- blocked || compareBlocked
                canceled <- canceled || compareCanceled
                if not canceled && not pending.Failed && not blocked then
                    if mismatch then
                        let previousUnit = pending.PreviousBlock[pending.PreviousBlockPosition]
                        let currentUnit = pending.CurrentBlock[pending.CurrentBlockPosition]
                        makeFound (pending.UnitOffset + float pending.PreviousBlockPosition) (Some previousUnit) (Some currentUnit)
                    elif compared = shared then
                        if pending.PreviousBlockCount <> pending.CurrentBlockCount then
                            makeFound (pending.UnitOffset + float pending.PreviousBlockPosition) None None
                        elif pending.PreviousBlockPosition >= pending.PreviousBlockCount
                             && pending.CurrentBlockPosition >= pending.CurrentBlockCount then
                            let nextPrevious, nextPreviousCount = keepLongPairTail pending.PreviousHistory pending.HistoryCount pending.PreviousBlock pending.PreviousBlockCount
                            let nextCurrent, nextCurrentCount = keepLongPairTail pending.CurrentHistory pending.HistoryCount pending.CurrentBlock pending.CurrentBlockCount
                            Array.blit nextPrevious 0 pending.PreviousHistory 0 nextPreviousCount
                            Array.blit nextCurrent 0 pending.CurrentHistory 0 nextCurrentCount
                            pending.HistoryCount <- min nextPreviousCount nextCurrentCount
                            let consumed = pending.PreviousBlockCount
                            pending.UnitOffset <- pending.UnitOffset + float consumed
                            pending.PrefixLength <- pending.UnitOffset
                            pending.PreviousBlockPosition <- 0
                            pending.PreviousBlockCount <- 0
                            pending.PreviousBlockComplete <- false
                            pending.CurrentBlockPosition <- 0
                            pending.CurrentBlockCount <- 0
                            pending.CurrentBlockComplete <- false
                            if consumed = 0 then
                                makeFound pending.UnitOffset None None
                        elif pending.PreviousFinished && pending.PreviousBlockPosition >= pending.PreviousBlockCount then
                            makeFound (pending.UnitOffset + float pending.PreviousBlockPosition) None None
                        elif pending.CurrentFinished && pending.CurrentBlockPosition >= pending.CurrentBlockCount then
                            makeFound (pending.UnitOffset + float pending.CurrentBlockPosition) None None

        while pending.Found.IsSome && not pending.ReverseDone && not pending.Failed && not blocked && not canceled && not (Meter.overBudget meter) do
            let! previousReady, previousFailed, previousWaiting = fillReverseWindow pending.PreviousReverse meter cancel true
            let! currentReady, currentFailed, currentWaiting = fillReverseWindow pending.CurrentReverse meter cancel true
            pending.Failed <- pending.Failed || previousFailed || currentFailed
            if previousWaiting || currentWaiting then blocked <- true
            elif cancel () then canceled <- true
            elif not pending.Failed && previousReady && currentReady then
                let mismatch, compareBlocked, compareCanceled = compareLongPairSuffix pending meter cancel
                blocked <- blocked || compareBlocked
                canceled <- canceled || compareCanceled
                if mismatch then pending.ReverseDone <- true
                if not mismatch && not canceled && not blocked && not (Meter.overBudget meter) then
                    if pending.PreviousReverse.BlockPosition < 0 then pending.PreviousReverse.BlockCount <- 0
                    if pending.CurrentReverse.BlockPosition < 0 then pending.CurrentReverse.BlockCount <- 0
                    if (pending.PreviousReverse.Exhausted && pending.PreviousReverse.BlockCount = 0)
                       || (pending.CurrentReverse.Exhausted && pending.CurrentReverse.BlockCount = 0) then
                        pending.ReverseDone <- true

        if cancel () || canceled then return LongLineStep.Canceled
        elif pending.Failed || failure.IsSome then return LongLineStep.Failed
        elif pending.Found.IsSome && pending.ReverseDone then
            let previousSlice, currentSlice = pending.Found.Value
            let suffix = pending.SuffixLength
            let changedEnd = max pending.PrefixLength (min (pending.Previous.Length - suffix) (pending.Current.Length - suffix))
            if previousSlice.Slice.OffsetUtf16 > int64 changedEnd then return LongLineStep.Failed
            else return LongLineStep.Complete(previousSlice, currentSlice, pending.PrefixLength, suffix)
        else return LongLineStep.Suspended
    }

    let wholeLineHighlights (meter: Meter) (cancel: unit -> bool) (previousRef: LineRef) (currentRef: LineRef) = async {
        let totalUnits = previousRef.Length + currentRef.Length
        if not (InlineHighlights.canUseWholeLine previousRef.Length currentRef.Length) then return None
        else
            let reservation = int64 totalUnits * 8L + 32_768L
            if not (ledger.TryReserve(AllocationCategory.AlignmentScratch, reservation)) then return None
            else
                try
                    let readLine side spec encoding (line: LineRef) = async {
                        let contentEnd = line.Start + lineContentBytes encoding line
                        let buffer = Array.zeroCreate<byte> 4_096
                        let units = Array.zeroCreate<uint16> (int line.Length)
                        let mutable count = 0
                        let mutable decoder = Decoders.createAt encoding (int64 line.Start)
                        let mutable waiting = false
                        let mutable failed = false
                        while not waiting && not failed && float decoder.AbsoluteOffset < contentEnd && not (cancel ()) do
                            let byteCount = int (min (float buffer.Length) (contentEnd - float decoder.AbsoluteOffset))
                            if byteCount <= 0 then failed <- true
                            else
                                match spec.Source with
                                | None -> failed <- true
                                | Some source ->
                                    let! outcome = source.ReadAt decoder.AbsoluteOffset buffer 0 byteCount
                                    match outcome with
                                    | ReadOutcome.Bytes actual when actual > 0 ->
                                        let sink _ _ value =
                                            if count < units.Length then
                                                units[count] <- uint16 value
                                                count <- count + 1
                                            else failed <- true
                                        let next, decoded = Decoders.decode decoder buffer 0 actual sink
                                        decoder <- next
                                        Meter.chargeBytes meter actual
                                        match decoded with
                                        | Error error -> reportDecodeError (if side = DiffSide.Previous then 0 else 1) error; failed <- true
                                        | Ok _ -> ()
                                    | ReadOutcome.NotYetAvailable -> waiting <- true
                                    | ReadOutcome.EndOfSource -> sourceChanged (); failed <- true
                                    | ReadOutcome.Bytes _ -> waiting <- true
                        if failed || waiting || cancel () || float decoder.AbsoluteOffset < contentEnd || count <> units.Length then return None
                        else return Some(Native.utf16Decode units count)
                    }
                    let! previousText = readLine DiffSide.Previous previousSpec previousEncoding previousRef
                    match previousText with
                    | None -> return None
                    | Some oldText ->
                        let! currentText = readLine DiffSide.Current currentSpec currentEncoding currentRef
                        match currentText with
                        | None -> return None
                        | Some newText ->
                            return Some(InlineHighlights.compute meter oldText newText)
                finally
                    ledger.Release(AllocationCategory.AlignmentScratch, reservation)
    }

    let countSide (rows: RowRef[]) (first: int) (count: int) (previousSide: bool) =
        let mutable total = 0
        for index = first to first + count - 1 do
            match rows[index].Kind with
            | DiffRowKind.Added when previousSide -> ()
            | DiffRowKind.Removed when not previousSide -> ()
            | _ -> total <- total + 1
        total

    let rowsPart hunkId (item: QueueItem) (start: int) (previousBefore: int) (currentBefore: int) (built: DiffRow[]) =
        let taken = built.Length
        DiffPart.Hunk {
            HunkId = hunkId
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

    let pairRanges (parts: DiffPart[]) =
        let ranges = ResizeArray<PairRange>()
        let mutable active = false
        let mutable previousStart = 0.0
        let mutable currentStart = 0.0
        let mutable count = 0.0
        let flush () =
            if active then
                ranges.Add { PreviousStart = previousStart; CurrentStart = currentStart; Count = count }
                active <- false
                count <- 0.0
        let addRow (row: DiffRow) =
            match row.Kind, row.Previous, row.Current with
            | DiffRowKind.Replaced, Some previous, Some current ->
                let previousNumber = float previous.Number
                let currentNumber = float current.Number
                if active && previousNumber = previousStart + count && currentNumber = currentStart + count then
                    count <- count + 1.0
                else
                    flush ()
                    active <- true
                    previousStart <- previousNumber
                    currentStart <- currentNumber
                    count <- 1.0
            | _ -> flush ()
        for part in parts do
            match part with
            | DiffPart.Hunk { Body = HunkBody.AlignedRows rows } ->
                for row in rows do addRow row
                flush ()
            | _ -> flush ()
        ranges.ToArray()

    let rememberPagePairs sequence (page: DiffPage) = pairings.AppendPage(sequence, pairRanges page.Parts)

    let makeRows (item: QueueItem) (start: int) (count: int) (firstId: int64) (previousValues: DiffLine[]) (currentValues: DiffLine[]) (cancel: unit -> bool) (meter: Meter) (pairWorkStarted: bool ref) =
        let fastIdStart =
            if firstId >= 0L && firstId + int64 (max 0 (count - 1)) <= int64 Int32.MaxValue then int firstId
            else -1
        let rows = Array.zeroCreate<DiffRow> count
        let mutable previousRefIndex = 0
        let mutable currentRefIndex = 0
        for index = 0 to count - 1 do
            let row = item.Rows[start + index]
            let id = if fastIdStart >= 0 then rowIdentifier (fastIdStart + index) else identifier "r" (firstId + int64 index)
            match row.Kind with
            | DiffRowKind.Context
            | DiffRowKind.EndingChanged ->
                let previousLine = previousValues[previousRefIndex]
                previousRefIndex <- previousRefIndex + 1
                let currentLine = {
                    previousLine with
                        Number = int64 row.Current.Number
                        Ending = LineEndingCode.toLineEnding row.Current.Ending
                        Slice = { previousLine.Slice with TotalUtf16 = Some(int64 row.Current.Length) }
                }
                rows[index] <- { Id = id; Kind = row.Kind; Previous = Some previousLine; Current = Some currentLine }
            | DiffRowKind.Removed ->
                let previousLine = previousValues[previousRefIndex]
                previousRefIndex <- previousRefIndex + 1
                rows[index] <- { Id = id; Kind = row.Kind; Previous = Some previousLine; Current = None }
            | DiffRowKind.Added ->
                let currentLine = currentValues[currentRefIndex]
                currentRefIndex <- currentRefIndex + 1
                rows[index] <- { Id = id; Kind = row.Kind; Previous = None; Current = Some currentLine }
            | DiffRowKind.Replaced ->
                let previousLine = previousValues[previousRefIndex]
                let currentLine = currentValues[currentRefIndex]
                previousRefIndex <- previousRefIndex + 1
                currentRefIndex <- currentRefIndex + 1
                rows[index] <- { Id = id; Kind = row.Kind; Previous = Some previousLine; Current = Some currentLine }
        let applyWholeHighlights index (previousHighlights: Highlight[]) (currentHighlights: Highlight[]) =
            let previousLine = rows[index].Previous.Value
            let currentLine = rows[index].Current.Value
            rows[index] <- {
                rows[index] with
                    Previous = Some { previousLine with Slice = { previousLine.Slice with Highlights = InlineHighlights.clipSlice previousLine.Slice.OffsetUtf16 previousLine.Slice.Text previousHighlights } }
                    Current = Some { currentLine with Slice = { currentLine.Slice with Highlights = InlineHighlights.clipSlice currentLine.Slice.OffsetUtf16 currentLine.Slice.Text currentHighlights } }
            }
        let applyMiddleHighlights index previousTotal currentTotal prefix suffix =
            let previousLine = rows[index].Previous.Value
            let currentLine = rows[index].Current.Value
            let previousHighlights = InlineHighlights.middleSlice previousTotal currentTotal prefix suffix previousLine.Slice.OffsetUtf16 previousLine.Slice.Text
            let currentHighlights = InlineHighlights.middleSlice currentTotal previousTotal prefix suffix currentLine.Slice.OffsetUtf16 currentLine.Slice.Text
            rows[index] <- {
                rows[index] with
                    Previous = Some { previousLine with Slice = { previousLine.Slice with Highlights = previousHighlights } }
                    Current = Some { currentLine with Slice = { currentLine.Slice with Highlights = currentHighlights } }
            }
        async {
            let mutable index = 0
            let mutable stopped = false
            let mutable interrupted = false
            while index < rows.Length && not stopped && not interrupted do
                if cancel () then interrupted <- true
                elif (match rows[index].Kind with | DiffRowKind.Replaced -> false | _ -> true) then index <- index + 1
                elif Meter.overBudget meter && pairWorkStarted.Value then stopped <- true
                else
                    let source = item.Rows[start + index]
                    let previousLine = rows[index].Previous.Value
                    let currentLine = rows[index].Current.Value
                    let firstPairWork = not pairWorkStarted.Value
                    pairWorkStarted.Value <- true
                    if source.Previous.Length <= 8_192.0 && source.Current.Length <= 8_192.0 then
                        let previousHighlights, currentHighlights = InlineHighlights.compute meter previousLine.Slice.Text currentLine.Slice.Text
                        applyWholeHighlights index previousHighlights currentHighlights
                        index <- index + 1
                    else
                        match pendingLongPairFor item (start + index) source.Previous source.Current previousLine currentLine with
                        | None ->
                            if cancel () then interrupted <- true else stopped <- true
                        | Some pending ->
                            // The first pair search of a page always advances, even when the earlier steps of the
                            // request used up the budget. It runs on a fresh meter and the request meter pays for it.
                            let pairMeter = if firstPairWork && Meter.overBudget meter then Meter.create host.Clock config.Limits else meter
                            let! changed = advanceLongPair pending pairMeter cancel
                            if not (obj.ReferenceEquals(pairMeter, meter)) then Meter.charge meter pairMeter.Units
                            match changed with
                            | LongLineStep.Canceled ->
                                releasePendingLongPair ()
                                interrupted <- true
                            | LongLineStep.Failed ->
                                releasePendingLongPair ()
                                stopped <- true
                            | LongLineStep.Suspended -> stopped <- true
                            | LongLineStep.Complete(previousSlice, currentSlice, prefix, suffix) ->
                                rows[index] <- { rows[index] with Previous = Some previousSlice; Current = Some currentSlice }
                                if not (InlineHighlights.canUseWholeLine source.Previous.Length source.Current.Length) then
                                    applyMiddleHighlights index source.Previous.Length source.Current.Length prefix suffix
                                    releasePendingLongPair ()
                                    index <- index + 1
                                else
                                    let! whole = wholeLineHighlights meter cancel source.Previous source.Current
                                    match whole with
                                    | Some(previousHighlights, currentHighlights) ->
                                        applyWholeHighlights index previousHighlights currentHighlights
                                        releasePendingLongPair ()
                                        index <- index + 1
                                    | None ->
                                        releasePendingLongPair ()
                                        if cancel () then interrupted <- true else stopped <- true
            if interrupted then return None
            elif index = rows.Length then return Some rows
            else return Some(Array.sub rows 0 index)
        }

    /// True when the suspended long pair is one of the rows start to start + rowCount - 1 of the item.
    let hasPageLongPair item start rowCount =
        match pendingLongPair with
        | Some pending ->
            obj.ReferenceEquals(pending.Item, item)
            && pending.RowIndex >= start
            && pending.RowIndex < start + rowCount
        | None -> false

    let readPageLines isPrevious spec encoding (item: QueueItem) start rowCount (lines: LineRef[]) cancel = async {
        match pendingLongPair with
        | Some pending when hasPageLongPair item start rowCount ->
            let index = RowLines.slotBefore item.Rows start pending.RowIndex isPrevious
            let beforeRefs = if index = 0 then Array.empty else Array.sub lines 0 index
            let afterRefs = if index + 1 = lines.Length then Array.empty else Array.sub lines (index + 1) (lines.Length - index - 1)
            let! before = readLines spec encoding beforeRefs cancel
            match before with
            | None -> return None
            | Some earlier ->
                let! after = readLines spec encoding afterRefs cancel
                match after with
                | None -> return None
                | Some later ->
                    let result = Array.zeroCreate<DiffLine> lines.Length
                    if earlier.Length > 0 then Array.blit earlier 0 result 0 earlier.Length
                    result[index] <- if isPrevious then pending.PreviousLine else pending.CurrentLine
                    if later.Length > 0 then Array.blit later 0 result (index + 1) later.Length
                    return Some result
        | _ -> return! readLines spec encoding lines cancel
    }

    let buildRows (item: QueueItem) (start: int) (count: int) (firstId: int64) (cancel: unit -> bool) (meter: Meter) (pairWorkStarted: bool ref) : Async<DiffRow[] option> = async {
        if cancel () then return None
        else
            let previousRefs, currentRefs = RowLines.collect item.Rows start count
            let! previousLines =
                if hasPageLongPair item start count then
                    readPageLines true previousSpec previousEncoding item start count previousRefs cancel
                else
                    readLines previousSpec previousEncoding previousRefs cancel
            match previousLines with
            | None -> return None
            | Some previousValues ->
                let! currentLines =
                    if hasPageLongPair item start count then
                        readPageLines false currentSpec currentEncoding item start count currentRefs cancel
                    else
                        readLines currentSpec currentEncoding currentRefs cancel
                match currentLines with
                | None -> return None
                | Some currentValues ->
                    return! makeRows item start count firstId previousValues currentValues cancel meter pairWorkStarted
    }

    let lanePart hunkId (item: QueueItem) (isPrevious: bool) (start: int) (built: DiffLine[]) =
        let lines = if isPrevious then item.PreviousLines else item.CurrentLines
        let own = { Start = int64 lines[start].Number; Count = int64 built.Length }
        let other = { Start = int64 (if isPrevious then item.CurrentStart else item.PreviousStart); Count = 0L }
        DiffPart.Hunk {
            HunkId = hunkId
            PreviousRange = if isPrevious then own else other
            CurrentRange = if isPrevious then other else own
            StartsHunk = item.StartsHunk && start = 0
            EndsHunk = item.EndsHunk && start + built.Length = lines.Length
            Body = if isPrevious then HunkBody.UnalignedSides(built, Array.empty) else HunkBody.UnalignedSides(Array.empty, built)
        }

    let takeBuiltFragment
        (available: int)
        (byteRoom: int)
        (cancel: unit -> bool)
        (values: 'T[])
        (makePart: 'T[] -> DiffPart)
        =
        let empty = Array.empty<'T>
        let one = Array.zeroCreate<'T> 1
        let baseSize = sizer (makePart empty)
        let mutable estimate = baseSize
        let mutable index = 0
        let mutable stop = false
        let mutable interrupted = false
        while not stop && index < available do
            if index &&& 255 = 255 && cancel () then
                interrupted <- true
                stop <- true
            else
                one[0] <- values[index]
                let cost = max 0 (sizer (makePart one) - baseSize) + 1
                if estimate + cost > byteRoom then stop <- true
                else
                    estimate <- estimate + cost
                    index <- index + 1
        if interrupted then None
        else
            let all = if index = values.Length then values else Array.sub values 0 index
            let mutable taken = all.Length
            let mutable part = makePart all
            let mutable size = sizer part
            while size > byteRoom && taken > 0 do
                taken <- taken - 1
                part <- makePart (Array.sub all 0 taken)
                size <- sizer part
            Some { Part = part; Taken = taken; Size = size }

    let estimateLineBytes (lineLength: float) =
        // The estimate uses the displayed slice, with two bytes reserved for each UTF-16 unit.
        let units = int (min 8_192.0 (max 0.0 lineLength))
        units * 2 + 96

    let estimateRowBytes (row: RowRef) =
        let previous = if LineRefs.hasPrevious row.Kind then estimateLineBytes row.Previous.Length else 0
        let current = if LineRefs.hasCurrent row.Kind then estimateLineBytes row.Current.Length else 0
        previous + current + 128

    let estimatePrefix available byteRoom cancel costAt =
        let mutable count = 0
        let mutable size = 512
        let mutable stop = false
        let mutable interrupted = false
        while not stop && count < available do
            if count &&& 255 = 255 && cancel () then
                interrupted <- true
                stop <- true
            else
                let cost = costAt count
                if size + cost > byteRoom then stop <- true
                else
                    size <- size + cost
                    count <- count + 1
        if interrupted then None else Some count

    let buildPage (sequence: int64) (cancel: unit -> bool) (meter: Meter) : Async<PageBuild> = async {
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
        let mutable suspended = false
        let mutable interrupted = false
        let pairWorkStarted = ref false
        let mutable index = 0
        while not stop && index < items.Length do
            let item = items[index]
            if cancel () then
                interrupted <- true
                stop <- true
            elif item.Kind = ItemKind.Gap then
                let gapId = identifier "g" nextGapSequence
                nextGapSequence <- nextGapSequence + 1L
                let part =
                    DiffPart.HiddenEqual {
                        GapId = gapId
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
            elif item.Kind = ItemKind.Rows && item.Total = 0 then
                let start = item.Consumed
                let previousBefore = countSide item.Rows 0 start true
                let currentBefore = countSide item.Rows 0 start false
                let part = rowsPart (identifier "h" (int64 item.Sequence)) item start previousBefore currentBefore Array.empty
                let size = sizer part
                if size > config.PageMaxBytes - usedBytes then
                    if parts.Count = 0 then oversize <- true
                    stop <- true
                else
                    parts.Add part
                    usedBytes <- usedBytes + size
                    fragments <- fragments + 1
                    fullCount <- fullCount + 1
                    fullFragments <- fullFragments + 1
                    index <- index + 1
            else
                let start = item.Consumed
                let requested = min item.Remaining (config.PageMaxRows - rowsUsed)
                let byteRoom = config.PageMaxBytes - usedBytes
                let available =
                    match item.Kind with
                    | ItemKind.Rows -> estimatePrefix requested byteRoom cancel (fun offset -> estimateRowBytes item.Rows[start + offset])
                    | ItemKind.Lane when item.PreviousLines.Length > 0 ->
                        estimatePrefix requested byteRoom cancel (fun offset -> estimateLineBytes item.PreviousLines[start + offset].Length + 128)
                    | ItemKind.Lane ->
                        estimatePrefix requested byteRoom cancel (fun offset -> estimateLineBytes item.CurrentLines[start + offset].Length + 128)
                    | ItemKind.Gap -> Some 0
                match available with
                | None ->
                    interrupted <- true
                    stop <- true
                | Some 0 ->
                    if parts.Count = 0 then oversize <- true
                    stop <- true
                | Some available ->
                    let mutable noRowsBuilt = false
                    let! fragment =
                        if item.Kind = ItemKind.Rows then async {
                            let previousBefore = countSide item.Rows 0 start true
                            let currentBefore = countSide item.Rows 0 start false
                            let firstId = nextRow
                            let hunkId = identifier "h" (int64 item.Sequence)
                            let! built = buildRows item start available firstId cancel meter pairWorkStarted
                            match built with
                            | None -> return None
                            | Some rows ->
                                noRowsBuilt <- rows.Length = 0
                                return takeBuiltFragment rows.Length byteRoom cancel rows (fun values -> rowsPart hunkId item start previousBefore currentBefore values)
                        }
                        elif item.PreviousLines.Length > 0 then async {
                            let lines = Array.sub item.PreviousLines start available
                            let hunkId = identifier "h" (int64 item.Sequence)
                            let! decoded = readLines previousSpec previousEncoding lines cancel
                            match decoded with
                            | None -> return None
                            | Some values -> return takeBuiltFragment values.Length byteRoom cancel values (lanePart hunkId item true start)
                        }
                        else
                            async {
                                let lines = Array.sub item.CurrentLines start available
                                let hunkId = identifier "h" (int64 item.Sequence)
                                let! decoded = readLines currentSpec currentEncoding lines cancel
                                match decoded with
                                | None -> return None
                                | Some values -> return takeBuiltFragment values.Length byteRoom cancel values (lanePart hunkId item false start)
                            }
                    match fragment with
                    | None ->
                        interrupted <- true
                        stop <- true
                    | Some _ when noRowsBuilt ->
                        suspended <- true
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
        elif suspended && parts.Count = 0 then return PageBuild.Suspended
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

    /// Appends the result for a sequence and advances the sequence. The commit runs to completion even when
    /// the caller is canceled, so a retry of the same cursor finds the record and never appends a second one.
    let recordResult (sequence: int64) (result: Resumable<DiffPage>) =
        Shield.run (
            async {
                do! journal.Append(journalKey 0L sequence, JournalValue.Page result)
                requestSequence <- sequence + 1L
            }
        )

    let producePage (sequence: int64) (cancel: unit -> bool) (meter: Meter) : Async<EngineResult<Resumable<DiffPage>> option> = async {
        let! built = buildPage sequence cancel meter
        match built with
        | PageBuild.Interrupted -> return None
        | PageBuild.Suspended ->
            let value = Resumable.Scanning(progress (), identifier "c" (sequence + 1L), None)
            do! recordResult sequence value
            return Some(EngineResult.Ok value)
        | PageBuild.TooLarge -> return Some(failWorker "A diff row exceeds the configured page byte limit.")
        | PageBuild.Built(page, fullCount, partialConsumed, rowsRemoved, fragmentsRemoved, nextRow) ->
            let value = Resumable.Ready page
            // The record and the consumption of the queued rows commit together. A cancellation that arrives
            // meanwhile takes effect after both are done.
            let mutable commitStarted = false
            try
                do!
                    Shield.run (
                        async {
                            commitStarted <- true
                            do! recordResult sequence value
                            match value with
                            | Resumable.Ready readyPage -> do! rememberPagePairs sequence readyPage
                            | Resumable.Scanning _ -> ()
                            for part in page.Parts do
                                match part with
                                | DiffPart.HiddenEqual gap ->
                                    match readIdentifier "g" gap.GapId with
                                    | Some gapSequence -> do! journal.Link(journalKey 3L gapSequence, journalKey 0L sequence)
                                    | None -> invalidOp "The generated gap id is invalid."
                                | _ -> ()
                            builder.CommitPage(fullCount, partialConsumed, rowsRemoved, fragmentsRemoved)
                            rowSequence <- nextRow
                            firstPageReturned <- true
                            if page.OutputComplete then releaseBuffers ()
                        }
                    )
                return Some(EngineResult.Ok value)
            with error ->
                if commitStarted then failCommit error
                return Some(failWorker error.Message)
    }

    let advance (sequence: int64) (cancel: unit -> bool) : Async<EngineResult<Resumable<DiffPage>>> = async {
        ensureBuffers ()
        let meter = Meter.create host.Clock config.Limits
        let mutable result: EngineResult<Resumable<DiffPage>> option = None
        let mutable first = true
        while result.IsNone do
            if hdf5Due () then do! probeHdf5 ()
            if cancel () || closing then result <- Some EngineResult.Canceled
            elif invalidDetail.IsSome then result <- Some(failContent ())
            elif failure.IsSome then
                let code, message = failure.Value
                result <- Some(EngineResult.Failed(code, message, None))
            else
                let hardTrigger = builder.QueuedRows >= config.PageMaxRows || builder.QueuedFragments >= config.PageMaxFragments
                let doneTrigger = modeIsDone () && builder.Finished
                let softTrigger = builder.CompletedHunks > 0 && not (modeIsDone ()) && not firstPageReturned
                if hardTrigger || doneTrigger || softTrigger then
                    let! produced = producePage sequence cancel meter
                    match produced with
                    | Some value -> result <- Some value
                    | None -> if failure.IsNone then result <- Some EngineResult.Canceled
                elif not first && Meter.overBudget meter then
                    if builder.QueueCount > 0 || builder.HasOpenRows then
                        builder.FlushOpen()
                        let! produced = producePage sequence cancel meter
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
        return! presentPageResult result.Value
    }

    let specAt sideIndex = if sideIndex = 0 then previousSpec else currentSpec
    let encodingAt sideIndex = if sideIndex = 0 then previousEncoding else currentEncoding
    let scanSideAt sideIndex = if sideIndex = 0 then previousSide else currentSide

    let beginSeek (sideIndex: int) (byLine: bool) (target: float) (startLine: float) (endLine: float) = async {
        let spec = specAt sideIndex
        if spec.Source.IsNone then return None
        else
            let hit = checkpoints.Find(sideIndex, byLine, target)
            let state, firstLine =
                match hit with
                | Some found -> found.State, found.Line
                | None -> Scanner.create (encodingAt sideIndex) (int64 spec.BomLength), 0.0
            let remaining = if byLine then endLine - firstLine + 1.0 else 1.0
            let batchCapacity = max 1 (int (max 1.0 (min 4_096.0 remaining)))
            let batch = LineBatch batchCapacity
            return Some {
                SideIndex = sideIndex
                ByLine = byLine
                StartLine = startLine
                EndLine = endLine
                TargetOffset = target
                State = state
                Buffer = Array.zeroCreate<byte> 65_536
                Batch = batch
                BatchCapacity = batchCapacity
                FoundLines = ResizeArray<ScannedLine>()
                LineNumber = firstLine
                Position = state.NextOffset
                Complete = false
                Waiting = false
        }
    }

    let releaseSeekBuffers (cursor: SeekCursor) =
        cursor.Buffer <- Array.empty
        cursor.Batch <- LineBatch 1

    let restoreSeekBuffers (cursor: SeekCursor) =
        if cursor.Buffer.Length = 0 then cursor.Buffer <- Array.zeroCreate<byte> 65_536
        if cursor.Batch.Capacity <> cursor.BatchCapacity then cursor.Batch <- LineBatch cursor.BatchCapacity

    let advanceSeek (cursor: SeekCursor) (meter: Meter) (cancel: unit -> bool) = async {
        let spec = specAt cursor.SideIndex
        match spec.Source with
        | None -> cursor.Complete <- true
        | Some source ->
            restoreSeekBuffers cursor
            cursor.Waiting <- false
            cursor.Complete <- cursor.Complete || cursor.State.IsComplete
            let mutable running = not cursor.Complete
            while running && not (Meter.overBudget meter) && not (cancel ()) do
                cursor.Batch.StopAtFull <- true

                let onLines (lines: LineBatch) =
                    for index = 0 to lines.Count - 1 do
                        let scanned = lines.Line index
                        let currentLine = cursor.LineNumber
                        let selected =
                            if cursor.ByLine then currentLine >= cursor.StartLine && currentLine <= cursor.EndLine
                            else float scanned.EndOffset > cursor.TargetOffset
                        if selected then cursor.FoundLines.Add scanned
                        cursor.LineNumber <- currentLine + 1.0
                        checkpoints.ObserveLine(cursor.SideIndex, float scanned.EndOffset, cursor.LineNumber) |> ignore
                        if (cursor.ByLine && currentLine >= cursor.EndLine) || (not cursor.ByLine && selected) then
                            cursor.Complete <- true
                            lines.StopRequested <- true
                let sideState = scanSideAt cursor.SideIndex
                let validatedBefore = sideState.Coverage

                if cursor.Position >= float spec.ByteLength then
                    if source.IsComplete() then
                        let result = Scanner.scanChunk cursor.State cursor.Buffer 0 0 true meter cursor.Batch onLines
                        cursor.Position <- cursor.State.NextOffset
                        if result.Status = DecodeFailure then cursor.Complete <- true
                        elif cursor.State.IsComplete && not cursor.Complete then cursor.Complete <- true
                    else cursor.Waiting <- true
                else
                    let boundary = (Math.Floor(cursor.Position / config.CheckpointIntervalBytes) + 1.0) * config.CheckpointIntervalBytes
                    let boundaryRoom = max 1.0 (boundary - cursor.Position)
                    let count = int (min (float cursor.Buffer.Length) (min (float spec.ByteLength - cursor.Position) boundaryRoom))
                    let! outcome = source.ReadAt (int64 cursor.Position) cursor.Buffer 0 count
                    match outcome with
                    | ReadOutcome.Bytes actual when actual > 0 ->
                        cursor.Waiting <- false
                        let atEnd = cursor.Position + float actual >= float spec.ByteLength && source.IsComplete()
                        let result = Scanner.scanChunk cursor.State cursor.Buffer 0 actual atEnd meter cursor.Batch onLines
                        cursor.Position <- cursor.State.NextOffset
                        sideState.SetCoverage(float cursor.State.StartOffset + cursor.State.ValidatedBytes)
                        checkpoints.Observe(cursor.SideIndex, cursor.State, cursor.LineNumber)
                        if result.Status = DecodeFailure then
                            match result.Error with
                            | Some error when float error.Offset >= validatedBefore ->
                                report (if cursor.SideIndex = 0 then DiffSide.Previous else DiffSide.Current) ("invalid " + Decoders.name (encodingAt cursor.SideIndex) + " sequence: " + error.Reason) error.Offset (Some error.Offset)
                            | Some _ -> sourceChanged ()
                            | None -> ()
                            cursor.Complete <- true
                        elif cursor.State.IsComplete && not cursor.Complete then cursor.Complete <- true
                    | ReadOutcome.NotYetAvailable -> cursor.Waiting <- true
                    | ReadOutcome.EndOfSource when source.IsComplete() -> sourceChanged (); cursor.Complete <- true
                    | ReadOutcome.EndOfSource -> cursor.Waiting <- true
                    | ReadOutcome.Bytes _ -> cursor.Waiting <- true
                    if hdf5Due () then do! probeHdf5 ()
                if cursor.State.IsComplete then
                    cursor.Complete <- true
                    if source.IsComplete() then setKnownLineCount cursor.SideIndex (int64 cursor.LineNumber)
                if cursor.Complete || cursor.Waiting || Meter.overBudget meter then running <- false
                if Meter.quantumDue meter && running then
                    do! host.Yield()
                    Meter.beginNextQuantum meter
            if cursor.Complete && cursor.FoundLines.Count = 0 && cursor.EndLine < cursor.LineNumber then ()
    }

    let expansionBinding gapId fromStart count =
        gapId + "|" + (if fromStart then "1" else "0") + "|" + string count

    let lineReadBinding side line offset maxUtf16 =
        (if side = DiffSide.Previous then "0" else "1")
        + "|" + string line + "|" + string offset + "|" + string maxUtf16

    let findGap gapId (parts: DiffPart[]) =
        parts
        |> Array.tryPick (function
            | DiffPart.HiddenEqual gap when gap.GapId = gapId -> Some gap
            | _ -> None)

    let readGap gapSequence gapId = async {
        let! result = journal.Read(journalKey 3L gapSequence)
        match result with
        | Some(JournalValue.Page(Resumable.Ready page)) -> return findGap gapId page.Parts
        | Some(JournalValue.Expansion(Resumable.Ready parts)) -> return findGap gapId parts
        | _ -> return None
    }

    let committedExpansion gapSequence = async {
        let! result = journal.Read(journalKey 4L gapSequence)
        match result with
        | Some(JournalValue.Expansion(Resumable.Ready parts)) -> return Some parts
        | _ -> return None
    }

    let lineRef (line: ScannedLine) = {
        Number = 0.0
        Start = float line.StartOffset
        Finish = float line.EndOffset
        Length = float line.Utf16Length
        Ending = LineEndingCode.ofLineEnding line.Ending
    }

    let makeExpansion (pending: PendingExpansion) = async {
        let previousScanned = pending.PreviousLines |> Option.defaultValue Array.empty
        let currentScanned = pending.CurrentLines |> Option.defaultValue Array.empty
        let previousRefs = previousScanned |> Array.map lineRef
        let currentRefs = currentScanned |> Array.map lineRef
        for index = 0 to previousRefs.Length - 1 do previousRefs[index] <- { previousRefs[index] with Number = float pending.Gap.PreviousRange.Start + float index + (if pending.FromStart then 0.0 else float (pending.Gap.PreviousRange.Count - int64 pending.TakeCount)) }
        for index = 0 to currentRefs.Length - 1 do currentRefs[index] <- { currentRefs[index] with Number = float pending.Gap.CurrentRange.Start + float index + (if pending.FromStart then 0.0 else float (pending.Gap.CurrentRange.Count - int64 pending.TakeCount)) }
        let! previousLines = readLines previousSpec previousEncoding previousRefs (fun () -> false)
        match previousLines with
        | None -> return None
        | Some previousValues ->
            let! currentLines = readLines currentSpec currentEncoding currentRefs (fun () -> false)
            match currentLines with
            | None -> return None
            | Some currentValues ->
                let rows = Array.zeroCreate<DiffRow> (min previousValues.Length currentValues.Length)
                let fastIdStart =
                    if rowSequence >= 0L && rowSequence <= int64 Int32.MaxValue - int64 (max 0 (rows.Length - 1)) then int rowSequence
                    else -1
                let rowId index =
                    if fastIdStart >= 0 then rowIdentifier (fastIdStart + index)
                    else identifier "r" (rowSequence + int64 index)
                for index = 0 to rows.Length - 1 do
                    let previous = previousValues[index]
                    let current = {
                        currentValues[index] with
                            Number = int64 currentRefs[index].Number
                    }
                    rows[index] <- {
                        Id = rowId index
                        Kind = DiffRowKind.Context
                        Previous = Some previous
                        Current = Some current
                    }
                let requestedRows = min rows.Length pending.TakeCount
                let residualId = identifier "g" nextGapSequence
                let makeParts taken =
                    let first = if pending.FromStart then 0 else rows.Length - taken
                    let visible =
                        if taken = 0 then Array.empty
                        else
                            Array.sub rows first taken
                            |> Array.mapi (fun index row -> { row with Id = rowId index })
                    let expanded = DiffPart.ExpandedContext(pending.Gap.GapId, visible)
                    let remaining = pending.Gap.PreviousRange.Count - int64 taken
                    let residual =
                        if remaining <= 0L then None
                        elif pending.FromStart then
                            Some(DiffPart.HiddenEqual {
                                GapId = residualId
                                PreviousRange = { pending.Gap.PreviousRange with Start = pending.Gap.PreviousRange.Start + int64 taken; Count = remaining }
                                CurrentRange = { pending.Gap.CurrentRange with Start = pending.Gap.CurrentRange.Start + int64 taken; Count = remaining }
                            })
                        else
                            Some(DiffPart.HiddenEqual {
                                GapId = residualId
                                PreviousRange = { pending.Gap.PreviousRange with Count = remaining }
                                CurrentRange = { pending.Gap.CurrentRange with Count = remaining }
                            })
                    match pending.FromStart, residual with
                    | true, Some gap -> [| expanded; gap |]
                    | false, Some gap -> [| gap; expanded |]
                    | _ -> [| expanded |]
                let mutable taken = requestedRows
                let mutable parts = makeParts taken
                let mutable size = parts |> Array.sumBy (fun part -> max 0 (sizer part))
                while taken > 0 && size > config.PageMaxBytes do
                    taken <- taken - 1
                    parts <- makeParts taken
                    size <- parts |> Array.sumBy (fun part -> max 0 (sizer part))
                if size > config.PageMaxBytes then return None
                else return Some(parts, taken)
    }

    let startExpansionSearch (pending: PendingExpansion) sideIndex = async {
        if pending.TakeCount = 0 then
            pending.PreviousLines <- Some Array.empty
            pending.CurrentLines <- Some Array.empty
            pending.Search <- None
        else
            let range = if sideIndex = 0 then pending.Gap.PreviousRange else pending.Gap.CurrentRange
            let first = if pending.FromStart then range.Start else range.Start + range.Count - int64 pending.TakeCount
            let last = first + int64 pending.TakeCount - 1L
            let! cursor = beginSeek sideIndex true (float first) (float first) (float last)
            pending.Search <- cursor
    }

    let advanceExpansion (pending: PendingExpansion) (meter: Meter) (cancel: unit -> bool) = async {
        let mutable continueWork = not (cancel ())
        while continueWork && not (Meter.overBudget meter) do
            match pending.Search with
            | None -> continueWork <- false
            | Some cursor ->
                do! advanceSeek cursor meter cancel
                if cursor.Complete then
                    let foundLines = cursor.FoundLines.ToArray()
                    releaseSeekBuffers cursor
                    if foundLines.Length <> pending.TakeCount then
                        sourceChanged ()
                        continueWork <- false
                    elif pending.PreviousLines.IsNone then
                        pending.PreviousLines <- Some foundLines
                        do! startExpansionSearch pending 1
                    else
                        pending.CurrentLines <- Some foundLines
                        pending.Search <- None
                elif cursor.Waiting || cancel () || Meter.overBudget meter then continueWork <- false
        return not continueWork || pending.Search.IsNone
    }

    let appendExpansion (pending: PendingExpansion) sequence parts usedRows =
        async {
            let mutable commitStarted = false
            try
                do!
                    Shield.run (
                        async {
                            let value = Resumable.Ready parts
                            let target = journalKey 1L sequence
                            commitStarted <- true
                            do! journal.Append(target, JournalValue.Expansion value)
                            do! journal.Link(journalKey 4L pending.GapSequence, target)
                            for part in parts do
                                match part with
                                | DiffPart.HiddenEqual gap ->
                                    match readIdentifier "g" gap.GapId with
                                    | Some gapSequence -> do! journal.Link(journalKey 3L gapSequence, target)
                                    | None -> invalidOp "The generated gap id is invalid."
                                | _ -> ()
                            nextGapSequence <- nextGapSequence + 1L
                            rowSequence <- rowSequence + int64 usedRows
                            pendingExpansions.Remove pending.GapSequence |> ignore
                        }
                    )
            with error ->
                if commitStarted then failCommit error
                return raise error
        }

    let createLineRead side line offset maxUtf16 attempt sequence pairedLine = async {
        let index = if side = DiffSide.Previous then 0 else 1
        let! search = beginSeek index true (float line) (float line) (float line)
        let! pairSearch =
            match pairedLine with
            | Some paired ->
                let peerIndex = if side = DiffSide.Previous then 1 else 0
                beginSeek peerIndex true (float paired) (float paired) (float paired)
            | None -> async.Return None
        return {
            Attempt = attempt
            Side = side
            Line = line
            OffsetUtf16 = offset
            SliceOffsetUtf16 = max 0L offset
            MaxUtf16 = maxUtf16
            TakeMaxUtf16 = min 8_192 (max 0 maxUtf16)
            RequestSequence = sequence
            LastUsed = 0L
            Search = search
            Scanned = None
            PairedLine = pairedLine
            PairSearch = pairSearch
            PairedScanned = None
            PairAnalysis = None
            PairHighlights = None
            Decoder = None
            BytePosition = 0.0
            Buffer = Array.zeroCreate<byte> 65_536
            Units = Array.zeroCreate<uint16> 8_192
            UnitCount = 0
            UnitPosition = 0.0
            PreviousUnit = None
            Waiting = false
        }
    }

    let scannedContentEnd encoding (scanned: ScannedLine) =
        float scanned.EndOffset - Widths.endingWidth encoding (LineEndingCode.ofLineEnding scanned.Ending)

    let createPairCursor side (scanned: ScannedLine) =
        let index = if side = DiffSide.Previous then 0 else 1
        let encoding = encodingAt index
        createForwardCursorFromBounds side (specAt index) encoding (float scanned.StartOffset) (scannedContentEnd encoding scanned)

    let createReverseCursor side (scanned: ScannedLine) =
        let index = if side = DiffSide.Previous then 0 else 1
        let encoding = encodingAt index
        createReverseCursorFromBounds side (specAt index) encoding (float scanned.StartOffset) (scannedContentEnd encoding scanned)

    let createPairAnalysis (previous: ScannedLine) (current: ScannedLine) =
        let collect = InlineHighlights.canUseWholeLine (float previous.Utf16Length) (float current.Utf16Length)
        {
            Previous = previous
            Current = current
            PreviousCursor = createPairCursor DiffSide.Previous previous
            CurrentCursor = createPairCursor DiffSide.Current current
            PreviousValues = if collect then Some(Array.zeroCreate<uint16> (int previous.Utf16Length)) else None
            CurrentValues = if collect then Some(Array.zeroCreate<uint16> (int current.Utf16Length)) else None
            PreviousRead = 0
            CurrentRead = 0
            PreviousBlock = Array.zeroCreate<uint16> 4_100
            CurrentBlock = Array.zeroCreate<uint16> 4_100
            PreviousBlockCount = 0
            CurrentBlockCount = 0
            PreviousBlockPosition = 0
            CurrentBlockPosition = 0
            PreviousFinished = false
            CurrentFinished = false
            PreviousReverse = createReverseCursor DiffSide.Previous previous
            CurrentReverse = createReverseCursor DiffSide.Current current
            ReverseDone = collect
            SuffixLength = 0
            LastSuffixUnit = 0us
            HasLastSuffixUnit = false
            PrefixLength = 0
            PrefixOpen = true
            LastPrefixUnit = 0us
            HasLastPrefixUnit = false
            Failed = false
        }

    let releasePendingLineRead (pending: PendingLineRead) =
        pending.PairAnalysis <- None
        for cursor in [| pending.Search; pending.PairSearch |] do
            match cursor with
            | Some value -> releaseSeekBuffers value
            | None -> ()
        pending.Buffer <- Array.empty
        pending.Units <- Array.empty

    let releasePendingLineRequest (pending: PendingLineRead) =
        match pending.PairAnalysis with
        | Some analysis ->
            analysis.PreviousCursor.Buffer <- Array.empty
            analysis.CurrentCursor.Buffer <- Array.empty
            analysis.PreviousReverse.Buffer <- Array.empty
            analysis.CurrentReverse.Buffer <- Array.empty
        | None -> ()
        for cursor in [| pending.Search; pending.PairSearch |] do
            match cursor with
            | Some value -> releaseSeekBuffers value
            | None -> ()
        pending.Buffer <- Array.empty

    let restorePendingLineRequest (pending: PendingLineRead) =
        if pending.Buffer.Length = 0 then pending.Buffer <- Array.zeroCreate<byte> 65_536
        for cursor in [| pending.Search; pending.PairSearch |] do
            match cursor with
            | Some value -> restoreSeekBuffers value
            | None -> ()
        match pending.PairAnalysis with
        | Some analysis ->
            if analysis.PreviousCursor.Buffer.Length = 0 then analysis.PreviousCursor.Buffer <- Array.zeroCreate<byte> 4_100
            if analysis.CurrentCursor.Buffer.Length = 0 then analysis.CurrentCursor.Buffer <- Array.zeroCreate<byte> 4_100
            if analysis.PreviousReverse.Buffer.Length = 0 then analysis.PreviousReverse.Buffer <- Array.zeroCreate<byte> 4_096
            if analysis.CurrentReverse.Buffer.Length = 0 then analysis.CurrentReverse.Buffer <- Array.zeroCreate<byte> 4_096
        | None -> ()

    let advancePairAnalysis (pending: PendingLineRead) (meter: Meter) (cancel: unit -> bool) = async {
        match pending.PairedLine with
        | None -> return true, false, false
        | Some _ when pending.TakeMaxUtf16 = 0 || (pending.Scanned |> Option.exists (fun scanned -> pending.OffsetUtf16 >= scanned.Utf16Length)) -> return true, false, false
        | Some _ when pending.PairHighlights.IsSome -> return true, false, false
        | Some _ ->
            let mutable failed = false
            let mutable waiting = false
            match pending.PairSearch with
            | Some cursor ->
                do! advanceSeek cursor meter cancel
                if cursor.Complete then
                    pending.PairSearch <- None
                    if cursor.FoundLines.Count = 0 then failed <- true
                    else pending.PairedScanned <- Some cursor.FoundLines[0]
                elif cursor.Waiting then waiting <- true
            | None -> ()
            if not failed && not waiting && pending.PairAnalysis.IsNone then
                match pending.Scanned, pending.PairedScanned with
                | Some own, Some peer ->
                    let previous, current =
                        if pending.Side = DiffSide.Previous then own, peer else peer, own
                    pending.PairAnalysis <- Some(createPairAnalysis previous current)
                | _ -> ()
            match pending.PairAnalysis with
            | None -> return false, failed, waiting
            | Some analysis when failed || waiting -> return false, failed, waiting
            | Some analysis ->
                let mutable progress = true
                let fillPrevious () = async {
                    if analysis.PreviousBlockPosition >= analysis.PreviousBlockCount && not analysis.PreviousFinished && not (Meter.overBudget meter) then
                        analysis.PreviousBlockPosition <- 0
                        let! count, missing, blocked, ended = readPairBlock analysis.PreviousCursor analysis.PreviousBlock meter cancel
                        analysis.PreviousBlockCount <- count
                        analysis.PreviousFinished <- ended
                        analysis.Failed <- analysis.Failed || missing
                        if blocked then waiting <- true
                        if count > 0 then
                            match analysis.PreviousValues with
                            | Some values -> Array.blit analysis.PreviousBlock 0 values analysis.PreviousRead count
                            | None -> ()
                            analysis.PreviousRead <- analysis.PreviousRead + count
                    return ()
                }
                let fillCurrent () = async {
                    if analysis.CurrentBlockPosition >= analysis.CurrentBlockCount && not analysis.CurrentFinished && not (Meter.overBudget meter) then
                        analysis.CurrentBlockPosition <- 0
                        let! count, missing, blocked, ended = readPairBlock analysis.CurrentCursor analysis.CurrentBlock meter cancel
                        analysis.CurrentBlockCount <- count
                        analysis.CurrentFinished <- ended
                        analysis.Failed <- analysis.Failed || missing
                        if blocked then waiting <- true
                        if count > 0 then
                            match analysis.CurrentValues with
                            | Some values -> Array.blit analysis.CurrentBlock 0 values analysis.CurrentRead count
                            | None -> ()
                            analysis.CurrentRead <- analysis.CurrentRead + count
                    return ()
                }
                do! fillPrevious ()
                do! fillCurrent ()
                if analysis.Failed then failed <- true
                let previousAvailable = analysis.PreviousBlockCount - analysis.PreviousBlockPosition
                let currentAvailable = analysis.CurrentBlockCount - analysis.CurrentBlockPosition
                let pairedAvailable = min previousAvailable currentAvailable
                let canComparePrefix = not waiting && not (cancel ()) && not (Meter.overBudget meter)
                if pairedAvailable > 0 && analysis.PrefixOpen && not canComparePrefix then waiting <- true
                if pairedAvailable > 0 && analysis.PrefixOpen && canComparePrefix then
                    let prefixLimit = max 0.0 (min (float analysis.Previous.Utf16Length) (float analysis.Current.Utf16Length) - analysis.PrefixLength)
                    let limit = min pairedAvailable (int (min (float Int32.MaxValue) prefixLimit))
                    let equal = Native.equalUnitPrefix analysis.PreviousBlock analysis.PreviousBlockPosition analysis.CurrentBlock analysis.CurrentBlockPosition limit
                    analysis.PrefixLength <- analysis.PrefixLength + float equal
                    if equal < limit then
                        let previousUnit = analysis.PreviousBlock[analysis.PreviousBlockPosition + equal]
                        let currentUnit = analysis.CurrentBlock[analysis.CurrentBlockPosition + equal]
                        let hasUnitBefore = equal > 0 || analysis.HasLastPrefixUnit
                        let unitBefore = if equal > 0 then analysis.PreviousBlock[analysis.PreviousBlockPosition + equal - 1] else analysis.LastPrefixUnit
                        if hasUnitBefore
                           && unitBefore >= 0xD800us && unitBefore <= 0xDBFFus
                           && previousUnit >= 0xDC00us && previousUnit <= 0xDFFFus
                           && currentUnit >= 0xDC00us && currentUnit <= 0xDFFFus then
                            analysis.PrefixLength <- analysis.PrefixLength - 1.0
                        analysis.PrefixOpen <- false
                    elif equal > 0 then
                        analysis.LastPrefixUnit <- analysis.PreviousBlock[analysis.PreviousBlockPosition + equal - 1]
                        analysis.HasLastPrefixUnit <- true
                    if analysis.PrefixLength >= min (float analysis.Previous.Utf16Length) (float analysis.Current.Utf16Length) then analysis.PrefixOpen <- false
                if pairedAvailable > 0 && (not analysis.PrefixOpen || canComparePrefix) then
                    analysis.PreviousBlockPosition <- analysis.PreviousBlockPosition + pairedAvailable
                    analysis.CurrentBlockPosition <- analysis.CurrentBlockPosition + pairedAvailable
                elif not analysis.PrefixOpen then
                    analysis.PreviousBlockPosition <- analysis.PreviousBlockPosition + previousAvailable
                    analysis.CurrentBlockPosition <- analysis.CurrentBlockPosition + currentAvailable
                if (analysis.PreviousFinished && analysis.PreviousBlockPosition >= analysis.PreviousBlockCount)
                   || (analysis.CurrentFinished && analysis.CurrentBlockPosition >= analysis.CurrentBlockCount) then
                    analysis.PrefixOpen <- false
                let complete =
                    analysis.PreviousFinished
                    && analysis.CurrentFinished
                    && analysis.PreviousBlockPosition >= analysis.PreviousBlockCount
                    && analysis.CurrentBlockPosition >= analysis.CurrentBlockCount
                if complete && not analysis.ReverseDone && not failed && not waiting && not (cancel ()) && not (Meter.overBudget meter) then
                    let! previousReady, previousFailed, previousWaiting = fillReverseWindow analysis.PreviousReverse meter cancel true
                    let! currentReady, currentFailed, currentWaiting = fillReverseWindow analysis.CurrentReverse meter cancel true
                    failed <- failed || previousFailed || currentFailed
                    waiting <- waiting || previousWaiting || currentWaiting
                    let previousReverse = analysis.PreviousReverse
                    let currentReverse = analysis.CurrentReverse
                    if not failed && not waiting && previousReady && currentReady then
                        if cancel () || Meter.overBudget meter then waiting <- true
                        else
                            let reverseSegment = min (previousReverse.BlockPosition + 1) (currentReverse.BlockPosition + 1)
                            let equal =
                                Native.equalUnitSuffix
                                    previousReverse.Units
                                    (previousReverse.BlockPosition + 1)
                                    currentReverse.Units
                                    (currentReverse.BlockPosition + 1)
                                    reverseSegment
                            if equal > 0 then
                                analysis.SuffixLength <- analysis.SuffixLength + float equal
                                analysis.LastSuffixUnit <- previousReverse.Units[previousReverse.BlockPosition - equal + 1]
                                analysis.HasLastSuffixUnit <- true
                                previousReverse.BlockPosition <- previousReverse.BlockPosition - equal
                                currentReverse.BlockPosition <- currentReverse.BlockPosition - equal
                            if equal < reverseSegment then
                                let previousUnit = previousReverse.Units[previousReverse.BlockPosition]
                                let currentUnit = currentReverse.Units[currentReverse.BlockPosition]
                                if analysis.SuffixLength > 0
                                   && analysis.HasLastSuffixUnit
                                   && analysis.LastSuffixUnit >= 0xDC00us && analysis.LastSuffixUnit <= 0xDFFFus
                                   && previousUnit >= 0xD800us && previousUnit <= 0xDBFFus
                                   && currentUnit >= 0xD800us && currentUnit <= 0xDBFFus then
                                    analysis.SuffixLength <- analysis.SuffixLength - 1.0
                                analysis.ReverseDone <- true
                            if previousReverse.BlockPosition < 0 then previousReverse.BlockCount <- 0
                            if currentReverse.BlockPosition < 0 then currentReverse.BlockCount <- 0
                            if previousReverse.Exhausted && previousReverse.BlockCount = 0 then analysis.ReverseDone <- true
                            if currentReverse.Exhausted && currentReverse.BlockCount = 0 then analysis.ReverseDone <- true
                    if failed then analysis.Failed <- true
                if complete && analysis.ReverseDone && not failed && not waiting && not (cancel ()) then
                    let highlights =
                        match analysis.PreviousValues, analysis.CurrentValues with
                        | Some previousValues, Some currentValues ->
                            let previousText = Native.utf16Decode previousValues analysis.PreviousRead
                            let currentText = Native.utf16Decode currentValues analysis.CurrentRead
                            let previousHighlights, currentHighlights = InlineHighlights.compute meter previousText currentText
                            PairHighlightResult.Spans(previousHighlights, currentHighlights)
                        | _ ->
                            PairHighlightResult.Middle(analysis.PrefixLength, analysis.SuffixLength)
                    pending.PairHighlights <- Some highlights
                    pending.PairAnalysis <- None
                    return true, false, false
                else return false, failed, waiting || cancel () || Meter.overBudget meter
    }

    let advanceLineRead (pending: PendingLineRead) (meter: Meter) (cancel: unit -> bool) = async {
        pending.Waiting <- false
        let mutable ready = false
        let mutable failed = false
        match pending.Search with
        | Some cursor ->
            do! advanceSeek cursor meter cancel
            if cursor.Complete then
                pending.Search <- None
                if cursor.FoundLines.Count = 0 then failed <- true
                else
                    let scanned = cursor.FoundLines[0]
                    pending.Scanned <- Some scanned
                    pending.Decoder <- Some(Decoders.createAt (encodingAt cursor.SideIndex) scanned.StartOffset)
                    pending.BytePosition <- float scanned.StartOffset
        | None -> ()
        if not failed && not (cancel ()) && not (Meter.overBudget meter) then
            match pending.Scanned, pending.Decoder with
            | Some scanned, Some initialDecoder ->
                let mutable decoder = initialDecoder
                let total = float scanned.Utf16Length
                let requestedOffset = float (max 0L pending.OffsetUtf16)
                let mutable requestedEnd = min total (requestedOffset + float pending.TakeMaxUtf16)
                let contentEnd = scannedContentEnd (encodingAt (if pending.Side = DiffSide.Previous then 0 else 1)) scanned
                if pending.TakeMaxUtf16 = 0 || requestedOffset >= total then ready <- true
                else
                    let mutable running = pending.UnitPosition < requestedEnd && pending.BytePosition < contentEnd
                    while running && not (Meter.overBudget meter) && not (cancel ()) do
                        let count = int (min (float pending.Buffer.Length) (contentEnd - pending.BytePosition))
                        if count <= 0 then running <- false
                        else
                            let! outcome = (specAt (if pending.Side = DiffSide.Previous then 0 else 1)).Source.Value.ReadAt (int64 pending.BytePosition) pending.Buffer 0 count
                            match outcome with
                            | ReadOutcome.Bytes actual when actual > 0 ->
                                let sink _ _ value =
                                    let unitIndex = pending.UnitPosition
                                    let currentUnit = uint16 value
                                    if unitIndex = requestedOffset then
                                        match pending.PreviousUnit with
                                        | Some previous when currentUnit >= 0xDC00us && currentUnit <= 0xDFFFus && previous >= 0xD800us && previous <= 0xDBFFus ->
                                            if pending.TakeMaxUtf16 < 2 then
                                                pending.UnitCount <- 0
                                                pending.SliceOffsetUtf16 <- pending.OffsetUtf16
                                                requestedEnd <- requestedOffset
                                            else
                                                pending.Units[0] <- previous
                                                pending.UnitCount <- 1
                                                pending.SliceOffsetUtf16 <- max 0L (pending.OffsetUtf16 - 1L)
                                                requestedEnd <- max requestedOffset (requestedEnd - 1.0)
                                        | _ -> pending.SliceOffsetUtf16 <- max 0L pending.OffsetUtf16
                                    if unitIndex >= requestedOffset && unitIndex < requestedEnd && pending.UnitCount < pending.TakeMaxUtf16 then
                                        pending.Units[pending.UnitCount] <- currentUnit
                                        pending.UnitCount <- pending.UnitCount + 1
                                    pending.UnitPosition <- unitIndex + 1.0
                                    pending.PreviousUnit <- Some currentUnit
                                let next, result = Decoders.decode decoder pending.Buffer 0 actual sink
                                decoder <- next
                                pending.Decoder <- Some next
                                pending.BytePosition <- float next.AbsoluteOffset
                                Meter.chargeBytes meter actual
                                match result with
                                | Error error -> reportDecodeError (if pending.Side = DiffSide.Previous then 0 else 1) error; failed <- true; running <- false
                                | Ok _ ->
                                    if pending.UnitPosition >= requestedEnd then running <- false
                            | ReadOutcome.NotYetAvailable -> pending.Waiting <- true; running <- false
                            | ReadOutcome.EndOfSource -> sourceChanged (); failed <- true; running <- false
                            | ReadOutcome.Bytes _ -> running <- false
                    if pending.UnitPosition >= requestedEnd || pending.BytePosition >= contentEnd then ready <- true
                    if not ready && not failed && Meter.overBudget meter then ()
            | _ -> ()
        if pending.Search.IsSome then ready <- false
        return ready, failed, pending.Waiting || (pending.Search |> Option.exists (fun cursor -> cursor.Waiting))
    }

    let lineReadResult (pending: PendingLineRead) =
        let scanned = pending.Scanned.Value
        let mutable count = pending.UnitCount
        if count > 0 && pending.Units[count - 1] >= 0xD800us && pending.Units[count - 1] <= 0xDBFFus then count <- count - 1
        let text = Native.utf16Decode pending.Units count
        let offset = pending.SliceOffsetUtf16
        let highlights =
            match pending.PairHighlights with
            | Some(PairHighlightResult.Spans(previous, current)) ->
                InlineHighlights.clipSlice offset text (if pending.Side = DiffSide.Previous then previous else current)
            | Some(PairHighlightResult.Middle(prefix, suffix)) ->
                let scannedTotal = float scanned.Utf16Length
                let pairedTotal =
                    match pending.PairedScanned with
                    | Some paired -> float paired.Utf16Length
                    | None -> scannedTotal
                InlineHighlights.middleSlice scannedTotal pairedTotal prefix suffix offset text
            | None -> Array.empty
        let line = {
            Number = pending.Line
            Ending = scanned.Ending
            Slice = {
                OffsetUtf16 = offset
                TotalUtf16 = Some scanned.Utf16Length
                Text = text
                Highlights = highlights
            }
        }
        line

    let allocateExpandRequest () =
        let sequence = nextExpandRequest
        nextExpandRequest <- nextExpandRequest + 1L
        sequence

    let allocateExpandAttempt () =
        let attempt = nextExpandAttempt
        nextExpandAttempt <- nextExpandAttempt + 1L
        attempt

    let allocateLineRequest () =
        let sequence = nextLineRequest
        nextLineRequest <- nextLineRequest + 1L
        sequence

    let allocateLineAttempt () =
        let attempt = nextLineAttempt
        nextLineAttempt <- nextLineAttempt + 1L
        attempt

    let touchLineRead (pending: PendingLineRead) =
        lineReadUse <- lineReadUse + 1L
        pending.LastUsed <- lineReadUse

    let lineReadContinuationWasIssued attempt sequence =
        attempt >= 0L
        && attempt < nextLineAttempt
        && sequence >= 0L
        && sequence < nextLineRequest

    let cacheLineRead (pending: PendingLineRead) =
        touchLineRead pending
        pendingLineReads[pending.Attempt] <- pending
        while pendingLineReads.Count > 8 do
            let expired = pendingLineReads.Values |> Seq.minBy (fun candidate -> candidate.LastUsed)
            releasePendingLineRead expired
            pendingLineReads.Remove expired.Attempt |> ignore

    let runExpansionRequestCore (pending: PendingExpansion) sequence (cancel: unit -> bool) = async {
        let meter = Meter.create host.Clock config.Limits
        let mutable finished = pending.Search.IsNone && pending.PreviousLines.IsSome && pending.CurrentLines.IsSome
        let mutable waiting = false
        while not finished && not waiting && not (Meter.overBudget meter) && not (cancel ()) && invalidDetail.IsNone && failure.IsNone do
            let! _ = advanceExpansion pending meter cancel
            finished <- pending.Search.IsNone && pending.PreviousLines.IsSome && pending.CurrentLines.IsSome
            waiting <- pending.Search |> Option.exists (fun cursor -> cursor.Waiting)
            if hdf5Due () then do! probeHdf5 ()
            if Meter.quantumDue meter && not finished && not (Meter.overBudget meter) then
                do! host.Yield()
                Meter.beginNextQuantum meter
        if cancel () then return EngineResult.Canceled
        elif invalidDetail.IsSome then return failContent ()
        elif failure.IsSome then
            let code, message = failure.Value
            return EngineResult.Failed(code, message, None)
        elif finished then
            let! built = makeExpansion pending
            match built with
            | None -> return failWorker "The expansion result exceeds the configured response limit."
            | Some(parts, usedRows) ->
                do! appendExpansion pending sequence parts usedRows
                return EngineResult.Ok(Resumable.Ready parts)
        else
            let nextSequence = allocateExpandRequest ()
            let continuation = operationIdentifier "x" pending.Attempt nextSequence (expansionBinding pending.Gap.GapId pending.FromStart pending.Count)
            let! preview =
                match pending.Search with
                | Some cursor -> pendingSeekPreview cursor
                | None -> async.Return None
            if invalidDetail.IsSome then return failContent ()
            elif failure.IsSome then
                let code, message = failure.Value
                return EngineResult.Failed(code, message, None)
            else
                let result = Resumable.Scanning(progress (), continuation, None)
                do! journal.Append(journalKey 1L sequence, JournalValue.Expansion result)
                pending.RequestSequence <- nextSequence
                return EngineResult.Ok(Resumable.Scanning(progress (), continuation, preview))
    }

    let runExpansionRequest (pending: PendingExpansion) sequence (cancel: unit -> bool) = async {
        try return! runExpansionRequestCore pending sequence cancel
        finally
            match pending.Search with
            | Some cursor -> releaseSeekBuffers cursor
            | None -> ()
    }

    let runLineReadRequestCore (pending: PendingLineRead) sequence (cancel: unit -> bool) = async {
        let meter = Meter.create host.Clock config.Limits
        let mutable finished = false
        let mutable failed = false
        let mutable waiting = false
        while not finished && not failed && not waiting && not (Meter.overBudget meter) && not (cancel ()) && invalidDetail.IsNone && failure.IsNone do
            let! ready, missing, isWaiting = advanceLineRead pending meter cancel
            if ready && not missing then
                let! paired, pairFailed, pairWaiting = advancePairAnalysis pending meter cancel
                finished <- paired
                failed <- missing || pairFailed
                waiting <- isWaiting || pairWaiting
            else
                finished <- ready
                failed <- missing
                waiting <- isWaiting
            if hdf5Due () then do! probeHdf5 ()
            if Meter.quantumDue meter && not finished && not failed && not (Meter.overBudget meter) then
                do! host.Yield()
                Meter.beginNextQuantum meter
        if cancel () then return EngineResult.Canceled
        elif invalidDetail.IsSome then return failContent ()
        elif failure.IsSome then
            let code, message = failure.Value
            return EngineResult.Failed(code, message, None)
        elif failed then return failMismatch ()
        elif finished then
            let mutable line = lineReadResult pending
            let mutable estimate = line.Slice.Text.Length * 3 + line.Slice.Highlights.Length * 64 + 512
            while estimate > 64 * 1024 && pending.UnitCount > 0 do
                pending.UnitCount <- max 0 (pending.UnitCount - 256)
                line <- lineReadResult pending
                estimate <- line.Slice.Text.Length * 3 + line.Slice.Highlights.Length * 64 + 512
            let result = Resumable.Ready line
            do! journal.Append(journalKey 2L sequence, JournalValue.Line result)
            releasePendingLineRead pending
            pendingLineReads.Remove pending.Attempt |> ignore
            return EngineResult.Ok result
        else
            let nextSequence = allocateLineRequest ()
            let continuation = operationIdentifier "l" pending.Attempt nextSequence (lineReadBinding pending.Side pending.Line pending.OffsetUtf16 pending.MaxUtf16)
            let! preview =
                match pending.Search with
                | Some cursor -> pendingSeekPreview cursor
                | None -> async.Return None
            if invalidDetail.IsSome then return failContent ()
            elif failure.IsSome then
                let code, message = failure.Value
                return EngineResult.Failed(code, message, None)
            else
                let result = Resumable.Scanning(progress (), continuation, None)
                do! journal.Append(journalKey 2L sequence, JournalValue.Line result)
                pending.RequestSequence <- nextSequence
                return EngineResult.Ok(Resumable.Scanning(progress (), continuation, preview))
    }

    let runLineReadRequest (pending: PendingLineRead) sequence (cancel: unit -> bool) = async {
        restorePendingLineRequest pending
        try
            return! runLineReadRequestCore pending sequence cancel
        finally
            releasePendingLineRequest pending
    }

    let sourceInfo sideIndex =
        let spec = specAt sideIndex
        let side = scanSideAt sideIndex
        let nextLine = if sideIndex = 0 then builder.NextPrevious else builder.NextCurrent
        let lineCount =
            match knownLineCounts[sideIndex] with
            | Some count -> Some count
            | None when modeIsDone () -> Some(int64 nextLine)
            | None when side.Finished -> Some(int64 (max nextLine (float side.Table.LineBase + float side.Table.Count)))
            | None -> None
        {
            ByteLength = spec.ByteLength
            LineCount = lineCount
            Encoding = Decoders.name (encodingAt sideIndex)
            HasBom = spec.BomLength > 0
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

    // Closed, failed and invalid sessions answer every request the same way. A request that arrives
    // while the session closes is rejected only when rejectClosing is set, otherwise acquire cancels it.
    let rejectedRequest (rejectClosing: bool) : EngineResult<_> option =
        if closed || (rejectClosing && closing) then Some(failClosed ())
        elif failure.IsSome then
            let code, message = failure.Value
            Some(EngineResult.Failed(code, message, None))
        elif invalidDetail.IsSome then Some(failContent ())
        else None

    // Runs one request under the busy flag. invalidRequest is a cheap argument check that answers with
    // a mismatch before the request waits for the session.
    let exclusive (rejectClosing: bool) (invalidRequest: bool) (cancel: unit -> bool) (body: unit -> Async<EngineResult<'T>>) = async {
        match rejectedRequest rejectClosing with
        | Some rejected -> return rejected
        | None ->
            if invalidRequest then return failMismatch ()
            else
                let! acquired = acquire cancel
                if not acquired then return EngineResult.Canceled
                else
                    try
                        try
                            if cancel () then return EngineResult.Canceled
                            else return! body ()
                        with error -> return failWorker error.Message
                    finally busy <- false
    }

    let pageAt (sequence: int64) (cancel: unit -> bool) = async {
        let! recorded = journal.Read(journalKey 0L sequence)
        match recorded with
        | Some(JournalValue.Page(Resumable.Ready page as value)) ->
            do! rememberPagePairs sequence page
            return! presentPageResult (EngineResult.Ok value)
        | Some(JournalValue.Page value) -> return! presentPageResult (EngineResult.Ok value)
        | Some _ -> return failMismatch ()
        | None when sequence <> requestSequence -> return failMismatch ()
        | None -> return! advance sequence cancel
    }

    member _.Progress = progress ()

    member _.SourceInfo = sourceInfo 0, sourceInfo 1

    member _.InitializeJournal() = journal.Initialize()

    member _.FirstPage(cancel: unit -> bool) = exclusive false false cancel (fun () -> pageAt 0L cancel)

    member _.ReadPage (cursor: string) (cancel: unit -> bool) =
        exclusive false false cancel (fun () -> async {
            match readIdentifier "c" cursor with
            | None -> return failMismatch ()
            | Some sequence -> return! pageAt sequence cancel
        })

    member _.ReplayPage(pageId: string) = async {
        match rejectedRequest true with
        | Some rejected -> return rejected
        | None ->
            match readIdentifier "p" pageId with
            | None -> return failMismatch ()
            | Some sequence ->
                let! acquired = acquire (fun () -> false)
                if not acquired then return failClosed ()
                else
                    try
                        try
                            let! recorded = journal.Read(journalKey 0L sequence)
                            match recorded with
                            | Some(JournalValue.Page(Resumable.Ready page)) when page.PageId = pageId ->
                                do! rememberPagePairs sequence page
                                return EngineResult.Ok { page with Pending = None }
                            | _ -> return failMismatch ()
                        with error -> return failWorker error.Message
                    finally busy <- false
    }

    member _.Expand(gapId: string, fromStart: bool, count: int, continuation: string option, cancel: unit -> bool) =
        exclusive true false cancel (fun () -> async {
            match readIdentifier "g" gapId with
            | None -> return failMismatch ()
            | Some gapSequence ->
                let binding = expansionBinding gapId fromStart count
                let parsed = continuation |> Option.map (readOperationIdentifier "x" binding)
                if parsed = Some None then return failMismatch ()
                else
                    let! committed = committedExpansion gapSequence
                    match committed with
                    | Some parts -> return EngineResult.Ok(Resumable.Ready parts)
                    | None ->
                      match continuation, parsed with
                      | Some _, Some(Some(attempt, sequence)) ->
                        match pendingExpansions.TryGetValue gapSequence with
                        | true, pending when attempt = pending.Attempt ->
                            let! recorded = journal.Read(journalKey 1L sequence)
                            match recorded with
                            | Some(JournalValue.Expansion(Resumable.Scanning(scanProgress, recordedContinuation, _))) ->
                                let! preview = pending.Search |> Option.map pendingSeekPreview |> Option.defaultValue (async.Return None)
                                if invalidDetail.IsSome then return failContent ()
                                else return EngineResult.Ok(Resumable.Scanning(scanProgress, recordedContinuation, preview))
                            | Some(JournalValue.Expansion result) -> return EngineResult.Ok result
                            | Some _ -> return failMismatch ()
                            | None when sequence <> pending.RequestSequence -> return failMismatch ()
                            | None -> return! runExpansionRequest pending sequence cancel
                        | _ -> return failMismatch ()
                      | None, _ ->
                        pendingExpansions.Remove gapSequence |> ignore
                        let! gap = readGap gapSequence gapId
                        match gap with
                        | None -> return failMismatch ()
                        | Some value ->
                            let take = min 100 (max 0 count |> min (int (min value.PreviousRange.Count 100L)))
                            let attempt = allocateExpandAttempt ()
                            let sequence = allocateExpandRequest ()
                            let pending = {
                                Attempt = attempt
                                GapSequence = gapSequence
                                Gap = value
                                FromStart = fromStart
                                Count = count
                                TakeCount = take
                                RequestSequence = sequence
                                Search = None
                                PreviousLines = None
                                CurrentLines = None
                            }
                            do! startExpansionSearch pending 0
                            pendingExpansions[gapSequence] <- pending
                            return! runExpansionRequest pending sequence cancel
                      | _ -> return failMismatch ()
        })

    member _.ReadLine(side: DiffSide, line: int64, offsetUtf16: int64, maxUtf16: int, continuation: string option, cancel: unit -> bool) =
        let invalidRequest = line < 0L || offsetUtf16 < 0L || (specAt (if side = DiffSide.Previous then 0 else 1)).Source.IsNone
        exclusive true invalidRequest cancel (fun () -> async {
            let binding = lineReadBinding side line offsetUtf16 maxUtf16
            match continuation with
            | Some token ->
                match readOperationIdentifier "l" binding token with
                | None -> return failMismatch ()
                | Some(attempt, sequence) when not (lineReadContinuationWasIssued attempt sequence) -> return failMismatch ()
                | Some(attempt, sequence) ->
                    let! recorded = journal.Read(journalKey 2L sequence)
                    match recorded with
                    | Some(JournalValue.Line(Resumable.Scanning(scanProgress, recordedContinuation, _) as recordedResult)) ->
                        match pendingLineReads.TryGetValue attempt with
                        | true, pending ->
                            touchLineRead pending
                            let! preview = pending.Search |> Option.map pendingSeekPreview |> Option.defaultValue (async.Return None)
                            if invalidDetail.IsSome then return failContent ()
                            else return EngineResult.Ok(Resumable.Scanning(scanProgress, recordedContinuation, preview))
                        | _ -> return EngineResult.Ok recordedResult
                    | Some(JournalValue.Line result) -> return EngineResult.Ok result
                    | Some _ -> return failMismatch ()
                    | None ->
                        match pendingLineReads.TryGetValue attempt with
                        | true, pending when pending.Side = side && pending.Line = line && pending.OffsetUtf16 = offsetUtf16 && pending.MaxUtf16 = maxUtf16 && pending.RequestSequence = sequence ->
                            touchLineRead pending
                            return! runLineReadRequest pending sequence cancel
                        | _ -> return failMismatch ()
            | None ->
                let attempt = allocateLineAttempt ()
                let sequence = allocateLineRequest ()
                let! pairedLine = pairings.Find(side, line)
                let! pending = createLineRead side line offsetUtf16 maxUtf16 attempt sequence pairedLine
                cacheLineRead pending
                let! result = runLineReadRequest pending sequence cancel
                match result with
                | EngineResult.Canceled
                | EngineResult.Failed _ -> releasePendingLineRead pending; pendingLineReads.Remove attempt |> ignore
                | _ -> ()
                return result
        })

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
                disposeResync ()
                previousSide.Dispose()
                currentSide.Dispose()
                for pending in pendingLineReads.Values do releasePendingLineRead pending
                pendingLineReads.Clear()
                releasePendingLongPair ()
                releaseBuffers ()
                releasePendingPreviewCursor 0
                releasePendingPreviewCursor 1
                do! pairings.Dispose()
                journal.Release()
                do! journal.Dispose()
                checkpoints.Dispose()
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
                || config.JournalCacheBytes < 0
                || config.ResyncSampleModulus <= 0
                || (config.ResyncSampleModulus &&& (config.ResyncSampleModulus - 1)) <> 0
                || config.ResyncIndexCapacity <= 0
                || config.ResyncIndexCapacity > 1_000_000
                || config.ResyncScanLines <= 0
                || config.ResyncScanBytes <= 0.0
                || config.ResyncChainLimit <= 0
                || config.ResyncProbeLimit <= 0
                || config.ResyncConfirmLines <= 0
                || config.CheckpointIntervalBytes <= 0.0
            then
                invalidArg (nameof config) "The session limits must be positive and the context count cannot be negative."
            if config.WindowMaxLines > SessionConfig.MaxWindowLines then
                invalidArg (nameof config) $"WindowMaxLines cannot exceed {SessionConfig.MaxWindowLines} lines, the largest window whose alignment fits the alignment scratch cap."
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
