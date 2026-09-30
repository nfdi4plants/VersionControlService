namespace VersionControlService.TextDiff

open System
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
    let mutable resync: ResyncEngine option = None

    // Answers that the aligner received from run comparisons. A restored aligner replays them instead of
    // comparing the same lines again.
    let replayLog = ResizeArray<int>()
    let mutable replayPosition = 0

    // Spilled state. The snapshot lives in its own temp store and is read back in bounded steps.
    let mutable spilled = false
    let mutable spillStore: ITempStore option = None
    let mutable restoreStage = 0
    /// True from the end of a restore until the end of the next request that runs on the restored state. A restore
    /// can use a whole request budget, so without this the session would be spilled again before it did any work.
    let mutable freshlyRestored = false
    let mutable restoreReader: SnapshotReader option = None
    let mutable restoreEngine: ResyncEngine option = None
    let mutable restoreSlots: ArraySlot[] = Array.empty
    let mutable restoreSlot = 0
    let restoreLines = Array.zeroCreate<float> 4
    let restoreCounts = Array.zeroCreate<int> 4
    let mutable restoreOps = Array.empty<int>
    let mutable restoreLog = Array.empty<int>
    let mutable restoreResync = false
    let mutable restoreAligning = false
    let mutable scratch: WorkerScratch option = None
    let mutable holder: IScratchHolder option = None

    // Text decoding scratch for page building.
    let textUnits = Array.zeroCreate<uint16> 8_192
    let mutable textCount = 0

    let builder = HunkBuilder(config.ContextLines, config.PageMaxRows)
    let journal = Journal(store, ledger, config.JournalCacheBytes)

    let checkpoints =
        Checkpoints(
            ledger,
            config.CheckpointIntervalBytes,
            config.CheckpointResidentBytes,
            [| previousEncoding; currentEncoding |],
            fun () -> host.CreateTempStore(config.SessionId + ":checkpoints")
        )

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
        previousSide.Observer <- fun state line -> checkpoints.Observe(0, state, line)
        currentSide.Observer <- fun state line -> checkpoints.Observe(1, state, line)
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

    /// Records a checkpoint at the last line start when a side passed its interval boundary. The equal-byte phase
    /// has no scanner state, so the checkpoint starts a fresh scanner at that line.
    let observeCommon () =
        if checkpoints.IsDue(0, lineStart) then
            checkpoints.Observe(0, Scanner.create previousEncoding (int64 lineStart) None, builder.NextPrevious + commonLines)
        if checkpoints.IsDue(1, lineStart + delta) then
            checkpoints.Observe(1, Scanner.create currentEncoding (int64 (lineStart + delta)) None, builder.NextCurrent + commonLines)

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
                        if not pendingCR then observeCommon ()
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

    /// Searches forward from the window start for the place where the two sources line up again. The window
    /// tables are released because the search scans the same lines again with its own tables.
    let createEngine (startOffsets: float[]) (startLines: float[]) =
        let coverage (side: int) (value: float) =
            if side = 0 then previousSide.SetCoverage value else currentSide.SetCoverage value
        ResyncEngine(
            config,
            ledger,
            builder,
            [| previousSpec; currentSpec |],
            [| previousEncoding; currentEncoding |],
            report,
            sourceChanged,
            coverage,
            (fun side state line -> checkpoints.Observe(side, state, line)),
            startOffsets,
            startLines
        )

    let startResync () =
        let startLines = [| float previousSide.Table.LineBase; float currentSide.Table.LineBase |]
        resync <- Some(createEngine [| pBase; cBase |] startLines)
        replayLog.Clear()
        replayPosition <- 0
        ops <- Array.empty
        commitCount <- 0
        windowState <- WindowState.Loading
        releaseWindows ()
        mode <- Mode.Resync

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
                // The window is at its largest size and no operation anchors it, so the diff continues with a
                // forward search for the next matching lines.
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
            replayLog.Clear()
            replayPosition <- 0
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
            | AlignStep.Waiting ->
                // The alignment scratch is held by other sessions. Ending the request budget suspends the job
                // with a Scanning result, and the next request tries the reservation again.
                Meter.charge meter 1
                meter.Units <- meter.MaxUnits
            | AlignStep.NeedRun(previousIndex, currentIndex, count) ->
                if replayPosition < replayLog.Count then
                    replayPosition <- replayPosition + 1
                    active.ResolveRun replayLog[replayPosition - 1]
                else startCompare previousIndex currentIndex count
            | AlignStep.Complete completed ->
                ops <- completed
                replayLog.Clear()
                replayPosition <- 0
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
                replayLog.Add matched
                replayPosition <- replayLog.Count
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
        | ResyncStep.Reserving ->
            // Other sessions hold the index memory. The request ends and the next one reserves again.
            Meter.charge meter 1
            meter.Units <- meter.MaxUnits
        | ResyncStep.Handover(previousOffset, previousLine, currentOffset, currentLine) ->
            resumeAt previousOffset previousLine currentOffset currentLine
        | ResyncStep.Finished -> finishResync ()
    }

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

    // Spill and restore of a suspended alignment step.

    let operationCode (kind: OperationKind) =
        match kind with
        | OperationKind.Equal -> 0
        | OperationKind.Added -> 1
        | OperationKind.Removed -> 2
        | OperationKind.Replaced -> 3
        | OperationKind.EndingChanged -> 4
        | OperationKind.Unaligned -> 5

    let operationKind (code: int) =
        match code with
        | 0 -> OperationKind.Equal
        | 1 -> OperationKind.Added
        | 2 -> OperationKind.Removed
        | 3 -> OperationKind.Replaced
        | 4 -> OperationKind.EndingChanged
        | _ -> OperationKind.Unaligned

    let tableSlots (table: LineTable) (count: int) : ArraySlot list = [
        ArraySlot.OfFloats(table.Starts, count)
        ArraySlot.OfFloats(table.Finishes, count)
        ArraySlot.OfFloats(table.Lengths, count)
        ArraySlot.OfInts(table.KeyLow, count)
        ArraySlot.OfInts(table.KeyHigh, count)
        ArraySlot.OfBytes(table.Endings, count)
    ]

    let ensureSpillStore () = async {
        match spillStore with
        | Some value -> return value
        | None ->
            let! created = host.CreateTempStore(config.SessionId + ":spill")
            spillStore <- Some created
            return created
    }

    let writeSnapshot () = async {
        let! target = ensureSpillStore ()
        let writer = SnapshotWriter target
        let header = HeaderBuilder()
        let inResync =
            match mode with
            | Mode.Resync -> true
            | _ -> false
        header.Bool inResync
        header.Bool(
            match windowState with
            | WindowState.Aligning
            | WindowState.Comparing -> true
            | _ -> false
        )
        header.Int(
            match windowState with
            | WindowState.Loading -> 0
            | WindowState.Feeding -> 3
            | _ -> 1
        )
        header.Int windowLimit
        header.Int commitCount
        header.Int opIndex
        header.Int feedPrevious
        header.Int feedCurrent
        header.Bool regionStarted
        header.Number pBase
        header.Number cBase
        header.Bool partialAlign
        header.Int partialPrevious
        header.Int partialCurrent
        header.Int ops.Length
        header.Int replayLog.Count
        if inResync then resync.Value.Export header
        else
            previousSide.Export header
            currentSide.Export header
        do! writer.Header(header.ToArray())
        if inResync then
            for slot in resync.Value.Slots do
                do! writer.Slot slot
        else
            for slot in tableSlots previousSide.Table previousSide.Table.Count do
                do! writer.Slot slot
            for slot in tableSlots currentSide.Table currentSide.Table.Count do
                do! writer.Slot slot
            let opValues = Array.zeroCreate<int> (ops.Length * 5)
            for index = 0 to ops.Length - 1 do
                let operation = ops[index]
                opValues[index * 5] <- operationCode operation.Kind
                opValues[index * 5 + 1] <- operation.PreviousIndex
                opValues[index * 5 + 2] <- operation.CurrentIndex
                opValues[index * 5 + 3] <- operation.PreviousCount
                opValues[index * 5 + 4] <- operation.CurrentCount
            do! writer.Slot(ArraySlot.OfInts(opValues, opValues.Length))
            let logValues = Array.zeroCreate<int> replayLog.Count
            for index = 0 to replayLog.Count - 1 do
                logValues[index] <- replayLog[index]
            do! writer.Slot(ArraySlot.OfInts(logValues, logValues.Length))
        let! _ = writer.Complete()
        ()
    }

    let dropRestore () =
        restoreReader <- None
        restoreSlots <- Array.empty
        restoreSlot <- 0
        restoreOps <- Array.empty
        restoreLog <- Array.empty
        match restoreEngine with
        | Some engine ->
            engine.Dispose()
            restoreEngine <- None
        | None -> ()
        restoreStage <- 0

    /// Writes the suspended step to the spill store and releases its scratch memory. A step that was only
    /// partly restored keeps its snapshot and gives back what the restore had taken.
    let spillNow () = async {
        if spilled then
            releaseWindows ()
            dropRestore ()
        elif modeIsWindowOrResync () then
            do! writeSnapshot ()
            releaseWindows ()
            match aligner with
            | Some active ->
                active.Dispose()
                aligner <- None
            | None -> ()
            compareJob <- None
            replayLog.Clear()
            replayPosition <- 0
            disposeResync ()
            dropRestore ()
            spilled <- true
        releaseBuffers ()
    }

    /// Reads the scalar state of a snapshot.
    let restoreHeader () = async {
        let scratchBytes = Array.zeroCreate<byte> 65_536
        let reader = SnapshotReader(spillStore.Value, scratchBytes)
        let! values = reader.Header()
        let header = HeaderReader values
        restoreResync <- header.Bool()
        restoreAligning <- header.Bool()
        let savedState = header.Int()
        windowState <- if savedState = 0 then WindowState.Loading elif savedState = 3 then WindowState.Feeding else WindowState.Aligning
        windowLimit <- header.Int()
        commitCount <- header.Int()
        opIndex <- header.Int()
        feedPrevious <- header.Int()
        feedCurrent <- header.Int()
        regionStarted <- header.Bool()
        pBase <- header.Number()
        cBase <- header.Number()
        partialAlign <- header.Bool()
        partialPrevious <- header.Int()
        partialCurrent <- header.Int()
        let opCount = header.Int()
        let logCount = header.Int()
        restoreOps <- Array.zeroCreate<int> (opCount * 5)
        restoreLog <- Array.zeroCreate<int> logCount
        if restoreResync then
            let engine = createEngine [| 0.0; 0.0 |] [| 0.0; 0.0 |]
            engine.Import header
            restoreEngine <- Some engine
        else
            let struct (previousFirst, previousCount) = previousSide.Import header
            let struct (currentFirst, currentCount) = currentSide.Import header
            restoreLines[0] <- float previousFirst
            restoreLines[1] <- float currentFirst
            restoreCounts[0] <- previousCount
            restoreCounts[1] <- currentCount
        restoreReader <- Some reader
    }

    /// Takes the memory that the snapshot needs. It returns false while other sessions hold it.
    let restoreMemory () =
        let acquired =
            if restoreResync then restoreEngine.Value.TryAllocate()
            else previousSide.Table.TryEnsure restoreCounts[0] && currentSide.Table.TryEnsure restoreCounts[1]
        if acquired then
            restoreSlots <-
                if restoreResync then List.toArray restoreEngine.Value.Slots
                else
                    Array.ofList (
                        tableSlots previousSide.Table restoreCounts[0]
                        @ tableSlots currentSide.Table restoreCounts[1]
                        @ [ ArraySlot.OfInts(restoreOps, restoreOps.Length); ArraySlot.OfInts(restoreLog, restoreLog.Length) ]
                    )
            restoreSlot <- 0
        acquired

    let finishRestore () =
        if restoreResync then
            resync <- restoreEngine
            restoreEngine <- None
            mode <- Mode.Resync
        else
            previousSide.Table.SetCount(restoreCounts[0], int64 restoreLines[0])
            currentSide.Table.SetCount(restoreCounts[1], int64 restoreLines[1])
            ops <-
                Array.init (restoreOps.Length / 5) (fun index ->
                    DiffOperations.make
                        (operationKind restoreOps[index * 5])
                        restoreOps[index * 5 + 1]
                        restoreOps[index * 5 + 2]
                        restoreOps[index * 5 + 3]
                        restoreOps[index * 5 + 4])
            replayLog.Clear()
            for value in restoreLog do
                replayLog.Add value
            replayPosition <- 0
            mode <- Mode.Window
            if restoreAligning then
                aligner <- Some(WindowAligner(previousSide.Table, currentSide.Table, config.MyersStepsPerGap, ledger))
                windowState <- WindowState.Aligning
        restoreReader <- None
        restoreSlots <- Array.empty
        restoreOps <- Array.empty
        restoreLog <- Array.empty
        restoreStage <- 0
        spilled <- false
        freshlyRestored <- true

    /// True when the next microstep touches no source, so a run of them needs no asynchronous scheduling.
    let synchronousStep () =
        if spilled then false
        else
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

    /// Restores a spilled step in bounded pieces: the scalar state, then the memory, then the arrays.
    let restoreStep (meter: Meter) = async {
        Meter.charge meter 1
        if restoreStage = 0 then
            do! restoreHeader ()
            restoreStage <- 1
        elif restoreStage = 1 then
            if restoreMemory () then restoreStage <- 2
            else
                // Other sessions hold the memory. The request ends and the next one tries again.
                meter.Units <- meter.MaxUnits
        elif restoreSlot < restoreSlots.Length then
            let slot = restoreSlots[restoreSlot]
            if slot.Filled >= slot.Count then restoreSlot <- restoreSlot + 1
            else
                let! bytes = restoreReader.Value.Fill(slot, 65_536)
                Meter.chargeBytes meter bytes
        else finishRestore ()
    }

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
                        if same then report (if side = 0 then DiffSide.Previous else DiffSide.Current) "HDF5 signature" (int64 nextHdf5[side])
                        nextHdf5[side] <- nextHdf5[side] * 2.0
                    else waiting <- true
    }

    let microstep (meter: Meter) = async {
        if spilled then do! restoreStep meter
        else
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

    // Page building. Stage one reads row text and builds the parts without changing any state. Stage two records
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

    let readLines (spec: SourceSpec) (encoding: TextEncoding) (lines: LineRef[]) : Async<DiffLine[] option> = async {
        let decoded = Array.zeroCreate<DiffLine> lines.Length
        let mutable index = 0
        let mutable failed = false
        let readIndividually () = async {
            let! line = readLine spec encoding lines[index]
            match line with
            | Some value -> decoded[index] <- value; index <- index + 1
            | None -> failed <- true
        }
        while index < lines.Length && not failed do
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
        if failed then return None else return Some decoded
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

    let collectRowRefs (item: QueueItem) (start: int) (count: int) =
        let previousCount = countSide item.Rows start count true
        let currentCount = countSide item.Rows start count false
        let previousRefs = Array.zeroCreate<LineRef> previousCount
        let currentRefs = Array.zeroCreate<LineRef> currentCount
        let mutable previousRefIndex = 0
        let mutable currentRefIndex = 0
        for index = start to start + count - 1 do
            let row = item.Rows[index]
            match row.Kind with
            | DiffRowKind.Added -> currentRefs[currentRefIndex] <- row.Current; currentRefIndex <- currentRefIndex + 1
            | DiffRowKind.Removed -> previousRefs[previousRefIndex] <- row.Previous; previousRefIndex <- previousRefIndex + 1
            | DiffRowKind.Replaced ->
                previousRefs[previousRefIndex] <- row.Previous
                currentRefs[currentRefIndex] <- row.Current
                previousRefIndex <- previousRefIndex + 1
                currentRefIndex <- currentRefIndex + 1
            | DiffRowKind.Context
            | DiffRowKind.EndingChanged -> previousRefs[previousRefIndex] <- row.Previous; previousRefIndex <- previousRefIndex + 1
        previousRefs, currentRefs

    let makeRows (item: QueueItem) (start: int) (count: int) (firstId: int64) (previousValues: DiffLine[]) (currentValues: DiffLine[]) =
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
        rows

    let buildRows (item: QueueItem) (start: int) (count: int) (firstId: int64) : Async<DiffRow[] option> = async {
        let previousRefs, currentRefs = collectRowRefs item start count
        let! previousLines = readLines previousSpec previousEncoding previousRefs
        match previousLines with
        | None -> return None
        | Some previousValues ->
            let! currentLines = readLines currentSpec currentEncoding currentRefs
            match currentLines with
            | None -> return None
            | Some currentValues -> return Some(makeRows item start count firstId previousValues currentValues)
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
                    if item.Kind = ItemKind.Rows then async {
                        let previousBefore = countSide item.Rows 0 start true
                        let currentBefore = countSide item.Rows 0 start false
                        let firstId = nextRow
                        let hunkId = identifier "h" (int64 item.Sequence)
                        let! built = buildRows item start available firstId
                        match built with
                        | None -> return None
                        | Some rows ->
                            return takeBuiltFragment available byteRoom cancel rows (fun values -> rowsPart hunkId item start previousBefore currentBefore values)
                    }
                    elif item.PreviousLines.Length > 0 then async {
                        let lines = Array.sub item.PreviousLines start available
                        let hunkId = identifier "h" (int64 item.Sequence)
                        let! decoded = readLines previousSpec previousEncoding lines
                        match decoded with
                        | None -> return None
                        | Some values -> return takeBuiltFragment available byteRoom cancel values (lanePart hunkId item true start)
                    }
                    else
                        async {
                            let lines = Array.sub item.CurrentLines start available
                            let hunkId = identifier "h" (int64 item.Sequence)
                            let! decoded = readLines currentSpec currentEncoding lines
                            match decoded with
                            | None -> return None
                            | Some values -> return takeBuiltFragment available byteRoom cancel values (lanePart hunkId item false start)
                        }
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

    /// Appends the result for a sequence and advances the sequence. The commit runs to completion even when
    /// the caller is canceled, so a retry of the same cursor finds the record and never appends a second one.
    let recordResult (sequence: int64) (result: Resumable<DiffPage>) =
        Shield.run (
            async {
                do! journal.Append(sequence, result)
                requestSequence <- sequence + 1L
            }
        )

    let producePage (sequence: int64) (cancel: unit -> bool) : Async<EngineResult<Resumable<DiffPage>> option> = async {
        let! built = buildPage sequence cancel
        match built with
        | PageBuild.Interrupted -> return None
        | PageBuild.TooLarge -> return Some(failWorker "A diff row exceeds the configured page byte limit.")
        | PageBuild.Built(page, fullCount, partialConsumed, rowsRemoved, fragmentsRemoved, nextRow) ->
            let value = Resumable.Ready page
            // The record and the consumption of the queued rows commit together. A cancellation that arrives
            // meanwhile takes effect after both are done.
            do!
                Shield.run (
                    async {
                        do! recordResult sequence value
                        builder.CommitPage(fullCount, partialConsumed, rowsRemoved, fragmentsRemoved)
                        rowSequence <- nextRow
                        firstPageReturned <- true
                        if page.OutputComplete then releaseBuffers ()
                    }
                )
            return Some(EngineResult.Ok value)
    }

    let advance (sequence: int64) (cancel: unit -> bool) : Async<EngineResult<Resumable<DiffPage>>> = async {
        let! admitted =
            match scratch, holder with
            | Some coordinator, Some self -> coordinator.BeginRequest self
            | _ -> async.Return true
        if not admitted || not (ensureBuffers ()) then
            // Another session is in the middle of a step or was waiting longer, or other sessions hold the
            // chunk scratch. The job suspends and the next request tries again.
            let value = Resumable.Scanning(progress (), identifier "c" (sequence + 1L), None)
            do! recordResult sequence value
            return EngineResult.Ok value
        else
            let meter = Meter.create host.Clock config.Limits
            let ranAfterRestore = freshlyRestored && not spilled
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
                        if checkpoints.HasPending then do! checkpoints.Flush()
                        if Meter.quantumDue meter then
                            do! host.Yield()
                            Meter.beginNextQuantum meter
                        if synchronousStep () then runSynchronously meter
                        else do! microstep meter
                        if waited then
                            waited <- false
                            do! host.Yield()
                first <- false
            if ranAfterRestore then freshlyRestored <- false
            return result.Value
    }

    /// Finds the line that a line number or a byte offset names. The scan resumes from the nearest checkpoint at
    /// or before the target and reads the source in 64 KiB pieces.
    let seekScan (sideIndex: int) (byLine: bool) (target: float) : Async<ScannedLine option> = async {
        let spec = if sideIndex = 0 then previousSpec else currentSpec
        let encoding = if sideIndex = 0 then previousEncoding else currentEncoding
        match spec.Source with
        | None -> return None
        | Some source ->
            let! hit = checkpoints.Find(sideIndex, byLine, target)
            let state, firstLine =
                match hit with
                | Some found -> found.State, found.Line
                | None -> Scanner.create encoding (int64 spec.BomLength) None, 0.0
            let buffer = Array.zeroCreate<byte> 65_536
            let batch = LineBatch(256)
            let meter = Meter.create host.Clock { config.Limits with MaxUnits = Int32.MaxValue; RequestMs = 1e15; QuantumMs = 1e15 }
            let mutable line = firstLine
            let mutable found: ScannedLine option = None
            let mutable position = state.NextOffset
            let mutable stop = false
            let onLines (lines: LineBatch) =
                for index = 0 to lines.Count - 1 do
                    if found.IsNone then
                        let scanned = lines.Line index
                        if (byLine && line = target) || (not byLine && float scanned.EndOffset > target) then found <- Some scanned
                        line <- line + 1.0
            let ignoreEvidence (_: ScannerEvidence) = ()
            while found.IsNone && not stop && not state.IsComplete do
                if position >= float spec.ByteLength then
                    let result = Scanner.scanChunk state buffer 0 0 true meter batch onLines ignoreEvidence
                    stop <- result.Status <> EndOfInput
                else
                    let count = int (min 65_536.0 (float spec.ByteLength - position))
                    let! outcome = source.ReadAt (int64 position) buffer 0 count
                    match outcome with
                    | ReadOutcome.Bytes actual when actual > 0 ->
                        meter.Units <- 0
                        let atEnd = position + float actual >= float spec.ByteLength
                        let result = Scanner.scanChunk state buffer 0 actual atEnd meter batch onLines ignoreEvidence
                        position <- position + float result.Consumed
                        if result.Status = DecodeFailure || result.Consumed < actual then stop <- true
                    | _ -> stop <- true
            return found
    }

    let seek (side: DiffSide) (byLine: bool) (target: float) : Async<EngineResult<ScannedLine option>> = async {
        if closed || closing then return failClosed ()
        elif target < 0.0 then return EngineResult.Ok None
        else
            try
                let! result = seekScan (if side = DiffSide.Previous then 0 else 1) byLine target
                return EngineResult.Ok result
            with error -> return failWorker error.Message
    }

    let holdsScratch () =
        bufA.Length > 0
        || aligner.IsSome
        || previousSide.Table.Capacity > 0
        || currentSide.Table.Capacity > 0
        || (match resync with
            | Some engine -> engine.HoldsScratch
            | None -> false)
        || (match restoreEngine with
            | Some engine -> engine.HoldsScratch
            | None -> false)

    /// True while an alignment step or a restore is in progress, and for the request that follows a restore.
    /// The intermediate alignment state is not part of a snapshot, so a spill in this state would throw the
    /// step's work away.
    let midStep () = aligner.IsSome || freshlyRestored || (spilled && restoreStage >= 1)

    /// Spills an idle session. A busy session keeps its state because its request is using it, and a session
    /// in the middle of a step keeps it until the step completes.
    let spillSession () : Async<EngineResult<unit>> = async {
        if closed || closing then return failClosed ()
        elif invalidDetail.IsSome then return failContent ()
        elif busy || midStep () then return EngineResult.Ok()
        else
            busy <- true
            try
                try
                    do! spillNow ()
                    return EngineResult.Ok()
                with error -> return failWorker error.Message
            finally busy <- false
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

    /// Writes the suspended step to the spill store and releases its scratch memory. The next request restores it.
    member _.Spill() = spillSession ()

    /// Returns the line with the given zero-based number, or None past the end of the source.
    member _.SeekLine(side: DiffSide, line: int64) = seek side true (float line)

    /// Returns the line that contains the given byte offset, or None past the end of the source.
    member _.SeekOffset(side: DiffSide, offset: int64) = seek side false (float offset)

    /// Registers the session with the coordinator of its worker.
    member internal this.Attach(coordinator: WorkerScratch) =
        let self = this :> IScratchHolder
        scratch <- Some coordinator
        holder <- Some self
        coordinator.Register self

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
                dropRestore ()
                match scratch, holder with
                | Some coordinator, Some self -> coordinator.Unregister self
                | _ -> ()
                previousSide.Dispose()
                currentSide.Dispose()
                releaseBuffers ()
                journal.Release()
                do! checkpoints.Dispose()
                do! store.Dispose()
                match spillStore with
                | Some value ->
                    spillStore <- None
                    do! value.Dispose()
                | None -> ()
    }

    interface IScratchHolder with
        member _.HoldsScratch = holdsScratch ()
        member _.IsBusy = busy
        member _.MustKeepScratch = midStep ()

        member _.Spill() =
            async {
                let! _ = spillSession ()
                ()
            }

module TextDiffSession =
    let private createWith
        (scratch: WorkerScratch option)
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
                || config.CheckpointResidentBytes < 0
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
            scratch |> Option.iter session.Attach
            return session
        }

    let create host ledger config sizer previous current = createWith None host ledger config sizer previous current

    /// Creates a session that shares scratch memory with the other sessions of a worker. A request on this
    /// session first spills the idle sessions that hold scratch memory.
    let createWithScratch (scratch: WorkerScratch) host ledger config sizer previous current =
        createWith (Some scratch) host ledger config sizer previous current
