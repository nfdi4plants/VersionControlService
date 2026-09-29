namespace VersionControlService.TextDiff

open System
open VersionControlService.Abstractions

[<RequireQualifiedAccess>]
type internal ResyncStep =
    | Running
    | Waiting
    | Reserving
    | Handover of previousOffset: float * previousLine: float * currentOffset: float * currentLine: float
    | Finished

module private ResyncStage =
    [<Literal>]
    let Discover = 0

    [<Literal>]
    let VerifyStart = 1

    [<Literal>]
    let VerifyLoad = 2

    [<Literal>]
    let VerifyForward = 3

    [<Literal>]
    let VerifyBackward = 4

    [<Literal>]
    let Emit = 5

    [<Literal>]
    let FollowDiscover = 0

    [<Literal>]
    let FollowHandover = 1

    [<Literal>]
    let FollowFinish = 2

    [<Literal>]
    let CandidateWidth = 8

/// Finds the next place where two sources line up again after a stretch of lines that a window could not
/// align. Both sides are scanned forward in step. A sampled subset of the lines goes into one hash index per
/// side, and each hit in the other side's index is confirmed against the source text. The lines before a
/// confirmed match are emitted as a pure insertion, a pure deletion or an unaligned region.
type internal ResyncEngine
    (
        config: SessionConfig,
        ledger: Ledger,
        builder: HunkBuilder,
        specs: SourceSpec[],
        encodings: TextEncoding[],
        report: DiffSide -> string -> int64 -> unit,
        changed: unit -> unit,
        coverage: int -> float -> unit,
        observe: int -> ScannerState -> float -> unit,
        startOffsets: float[],
        startLines: float[]
    ) =
    let categories = [| AllocationCategory.PreviousWindows; AllocationCategory.CurrentWindows |]
    let sides = [| DiffSide.Previous; DiffSide.Current |]

    let makeSides () =
        Array.init 2 (fun side ->
            let scan = ScanSide(specs[side], encodings[side], categories[side], sides[side], ledger, config.HashMaskForTesting, report)
            scan.RecordPeaks <- false
            scan)

    let disc = makeSides ()
    do
        for side in 0..1 do
            disc[side].Observer <- fun state line -> observe side state line
    let aux = makeSides ()
    let indexes = [| SampledIndex config.ResyncIndexCapacity; SampledIndex config.ResyncIndexCapacity |]
    let indexBytes = 2L * SampledIndex.ReservedBytes config.ResyncIndexCapacity

    let discCap = 4_096
    let auxCap = max 4_096 (config.ResyncConfirmLines * 4)
    let laneChunk = max 1 (min 1_024 config.PageMaxRows)
    let mask = config.ResyncSampleModulus - 1

    let importCounts = Array.zeroCreate<int> 2
    let mutable reserved = false
    let mutable stage = ResyncStage.Discover

    // Scan positions per side. Origin is where the cumulative counters start, range is where the lines that no
    // region has claimed yet start, and proc is the first line that the discovery has not processed.
    let originOffset = Array.copy startOffsets
    let originLine = Array.copy startLines
    let rangeOffset = Array.copy startOffsets
    let rangeLine = Array.copy startLines
    let procOffset = Array.copy startOffsets
    let procLine = Array.copy startLines
    let procIdx = Array.zeroCreate<int> 2
    let discReady = Array.zeroCreate<bool> 2
    let finishedSide = Array.zeroCreate<bool> 2

    // Candidates of the line that was just processed. Each one holds the own and other position and the earliest
    // position that a backward extension may reach.
    let candData = Array.zeroCreate<float> (max 1 config.ResyncProbeLimit * ResyncStage.CandidateWidth)
    let mutable candSide = 0
    let mutable candCount = 0
    let mutable candNext = -1
    // A side that hits a budget waits for the other side to reach the same line count before the region ends.
    let mutable cutFollow = 0
    let mutable cutCount = 0.0

    // Verification of one candidate.
    let candOffset = Array.zeroCreate<float> 2
    let candLine = Array.zeroCreate<float> 2
    let boundOffset = Array.zeroCreate<float> 2
    let boundLine = Array.zeroCreate<float> 2
    let targetLine = Array.zeroCreate<float> 2
    let auxLimit = Array.zeroCreate<int> 2
    let mutable forwardCount = 0
    let mutable backwardCount = 0
    let mutable rowP = 0
    let mutable rowC = 0
    let mutable job: (Meter -> Async<CompareStep>) option = None

    // Emission of the lines between the range start and an end position.
    let endOffset = Array.zeroCreate<float> 2
    let endLine = Array.zeroCreate<float> 2
    let emitOffset = Array.zeroCreate<float> 2
    let emitLine = Array.zeroCreate<float> 2
    let emitIdx = Array.zeroCreate<int> 2
    let emitLoaded = Array.zeroCreate<bool> 2
    let mutable emitFollow = 0
    let mutable emitPhase = 0
    let mutable emitKind = 0

    let tableHeld (side: ScanSide) = side.Table.Capacity > 0

    let dropTables () =
        for side in 0..1 do
            disc[side].Table.ReleaseWindow()
            aux[side].Table.ReleaseWindow()
            discReady[side] <- false
            procIdx[side] <- 0
            emitLoaded[side] <- false
        job <- None
        if stage = ResyncStage.VerifyLoad || stage = ResyncStage.VerifyForward || stage = ResyncStage.VerifyBackward then
            stage <- ResyncStage.VerifyStart

    let reserve () =
        if reserved then true
        elif ledger.TryReserve(AllocationCategory.SampledIndexes, indexBytes) then
            reserved <- true
            indexes[0].Allocate()
            indexes[1].Allocate()
            true
        else false

    let startSide (side: ScanSide) (offset: float) (line: float) (limit: int) =
        side.SetCursor(offset, int64 line, limit, Int32.MaxValue)

    /// Reads one chunk into a side. It returns 1 after progress, 0 when the source has no data yet, -1 when the
    /// ledger refuses the line table and 2 when nothing changed.
    let loadStep (side: ScanSide) (buffer: byte[]) (limit: int) (meter: Meter) = async {
        if not (side.Table.TryEnsure limit) then return -1
        else
            Meter.charge meter 1
            let remainingLines = max 1 (limit - side.Table.Count)
            let readMax = min buffer.Length (max 4_096 (remainingLines * 128))
            let! step = side.ReadInto(buffer, readMax)
            match step with
            | ReadData(count, endOfSource) ->
                side.Consume(buffer, count, endOfSource, meter) |> ignore
                return 1
            | ReadWaiting -> return 0
            | ReadChanged ->
                changed ()
                return 2
            | ReadWindowFull
            | ReadFinished -> return 2
    }

    let toStep (code: int) =
        if code = 0 then ResyncStep.Waiting
        elif code < 0 then ResyncStep.Reserving
        else ResyncStep.Running

    let beginEmit (offsets: float[]) (lines: float[]) (follow: int) =
        for side in 0..1 do
            endOffset[side] <- offsets[side]
            endLine[side] <- lines[side]
            emitOffset[side] <- rangeOffset[side]
            emitLine[side] <- rangeLine[side]
            emitLoaded[side] <- false
            emitIdx[side] <- 0
        let previousCount = endLine[0] - rangeLine[0]
        let currentCount = endLine[1] - rangeLine[1]
        emitKind <-
            if previousCount > 0.0 && currentCount > 0.0 then 3
            elif previousCount > 0.0 then 1
            elif currentCount > 0.0 then 2
            else 0
        emitFollow <- follow
        cutFollow <- 0
        emitPhase <- 0
        stage <- ResyncStage.Emit

    let emitProc (follow: int) = beginEmit procOffset procLine follow

    let hasRoom (side: int) = emitLine[side] < endLine[side]

    let emitLane (side: int) (buffer: byte[]) (meter: Meter) = async {
        let scan = aux[side]
        let table = scan.Table
        let remaining = endLine[side] - emitLine[side]
        if emitLoaded[side] && emitIdx[side] < table.Count then
            let count = min (int (min remaining (float laneChunk))) (table.Count - emitIdx[side])
            let index = emitIdx[side]
            if emitKind = 3 then builder.RegionLines((side = 0), table, index, count)
            elif emitKind = 1 then builder.Removed(table, index, count)
            else builder.Added(table, index, count)
            emitOffset[side] <- table.Finish(index + count - 1)
            emitLine[side] <- emitLine[side] + float count
            emitIdx[side] <- index + count
            Meter.charge meter (count / 2 + 1)
            return ResyncStep.Running
        elif not emitLoaded[side] then
            let limit = int (min (float auxCap) remaining)
            startSide scan emitOffset[side] emitLine[side] limit
            auxLimit[side] <- limit
            emitLoaded[side] <- true
            emitIdx[side] <- 0
            return ResyncStep.Running
        elif scan.WindowFull then
            emitLoaded[side] <- false
            return ResyncStep.Running
        elif scan.Finished then
            changed ()
            return ResyncStep.Running
        else
            let! code = loadStep scan buffer auxLimit[side] meter
            coverage side scan.Coverage
            return toStep code
    }

    let finishEmit () =
        emitPhase <- 0
        if emitFollow = ResyncStage.FollowDiscover then
            for side in 0..1 do
                rangeOffset[side] <- endOffset[side]
                rangeLine[side] <- endLine[side]
            indexes[0].Reset()
            indexes[1].Reset()
            candCount <- 0
            candNext <- -1
            stage <- ResyncStage.Discover
            ResyncStep.Running
        elif emitFollow = ResyncStage.FollowHandover then ResyncStep.Handover(endOffset[0], endLine[0], endOffset[1], endLine[1])
        else ResyncStep.Finished

    let emitStep (bufferA: byte[]) (bufferB: byte[]) (meter: Meter) = async {
        Meter.charge meter 1
        if emitPhase = 0 then
            if emitKind = 3 then builder.BeginRegion()
            emitPhase <- 1
            return ResyncStep.Running
        elif emitPhase = 1 then
            if emitKind = 2 || not (hasRoom 0) then
                emitPhase <- 2
                return ResyncStep.Running
            else return! emitLane 0 bufferA meter
        elif emitPhase = 2 then
            if emitKind = 1 || not (hasRoom 1) then
                emitPhase <- 3
                return ResyncStep.Running
            else return! emitLane 1 bufferB meter
        else
            if emitKind = 3 then builder.EndRegion()
            return finishEmit ()
    }

    let pickSide () =
        if finishedSide[0] then 1
        elif finishedSide[1] then 0
        elif procLine[1] - originLine[1] < procLine[0] - originLine[0] then 1
        else 0

    let addCandidate (own: int) (ownOffset: float) (ownBoundOffset: float) (ownBoundLine: float) (otherId: int) =
        let other = 1 - own
        let index = indexes[other]
        let baseIndex = candCount * ResyncStage.CandidateWidth
        candData[baseIndex] <- ownOffset
        candData[baseIndex + 1] <- procLine[own]
        candData[baseIndex + 2] <- ownBoundOffset
        candData[baseIndex + 3] <- ownBoundLine
        candData[baseIndex + 4] <- index.Offset otherId
        candData[baseIndex + 5] <- index.Line otherId
        if otherId > 0 then
            candData[baseIndex + 6] <- index.Offset(otherId - 1)
            candData[baseIndex + 7] <- index.Line(otherId - 1)
        else
            candData[baseIndex + 6] <- rangeOffset[other]
            candData[baseIndex + 7] <- rangeLine[other]
        candCount <- candCount + 1

    /// Ends the region when both sides scanned the same number of lines. The first side that hits a budget
    /// waits for the other side. When the other side hits its own budget first, the region ends at once.
    let cutAt (side: int) (follow: int) =
        let other = 1 - side
        let scanned = procLine[side] - originLine[side]
        if cutFollow = 0 && not finishedSide[other] && procLine[other] - originLine[other] < scanned then
            cutFollow <- follow
            cutCount <- scanned
        else
            emitProc follow

    /// Processes lines of one side that its discovery table holds. Sampled lines probe the other index and
    /// then enter their own.
    let processLines (side: int) (meter: Meter) =
        let table = disc[side].Table
        let other = 1 - side
        let own = indexes[side]
        let mutable running = true
        let mutable processed = 0
        while running && procIdx[side] < table.Count && processed < 256 do
            let index = procIdx[side]
            let scanned = procLine[side] - originLine[side]
            if cutFollow <> 0 && scanned >= cutCount then
                running <- false
            elif scanned >= float config.ResyncScanLines
                 || procOffset[side] - originOffset[side] >= config.ResyncScanBytes then
                cutAt side ResyncStage.FollowHandover
                running <- false
            else
                let hash = table.HashLow index
                let sampled = (hash &&& mask) = 0
                if sampled && own.IsFull then
                    cutAt side ResyncStage.FollowDiscover
                    running <- false
                else
                    if sampled then
                        let lineOffset = table.Start index
                        let ownBoundOffset = if own.Count > 0 then own.Offset(own.Count - 1) else rangeOffset[side]
                        let ownBoundLine = if own.Count > 0 then own.Line(own.Count - 1) else rangeLine[side]
                        candCount <- 0
                        let otherIndex = indexes[other]
                        let mutable id = otherIndex.Newest hash
                        let mutable probes = 0
                        while id >= 0 && probes < config.ResyncProbeLimit do
                            if otherIndex.Hash id = hash then addCandidate side lineOffset ownBoundOffset ownBoundLine id
                            probes <- probes + 1
                            id <- otherIndex.Older id
                        if own.CountSame(hash, config.ResyncProbeLimit) < config.ResyncChainLimit then
                            own.Add(lineOffset, procLine[side], hash)
                        if candCount > 0 then
                            candSide <- side
                            candNext <- candCount - 1
                    procOffset[side] <- table.Finish index
                    procLine[side] <- procLine[side] + 1.0
                    procIdx[side] <- index + 1
                    if sampled && candCount > 0 then
                        stage <- ResyncStage.VerifyStart
                        running <- false
            processed <- processed + 1
        Meter.charge meter (processed / 16 + 1)
        ResyncStep.Running

    let discoverStep (bufferA: byte[]) (bufferB: byte[]) (meter: Meter) = async {
        if finishedSide[0] && finishedSide[1] then
            emitProc ResyncStage.FollowFinish
            return ResyncStep.Running
        elif cutFollow <> 0
             && (finishedSide[0] || procLine[0] - originLine[0] >= cutCount)
             && (finishedSide[1] || procLine[1] - originLine[1] >= cutCount) then
            emitProc cutFollow
            return ResyncStep.Running
        else
            let side = pickSide ()
            let scan = disc[side]
            let table = scan.Table
            if not discReady[side] then
                startSide scan procOffset[side] procLine[side] discCap
                discReady[side] <- true
                procIdx[side] <- 0
                return ResyncStep.Running
            elif procIdx[side] < table.Count then return processLines side meter
            elif scan.Finished then
                finishedSide[side] <- true
                return ResyncStep.Running
            elif scan.WindowFull then
                scan.Advance(int64 table.LineBase + int64 table.Count, discCap, Int32.MaxValue)
                procIdx[side] <- 0
                return ResyncStep.Running
            else
                let! code = loadStep scan (if side = 0 then bufferA else bufferB) discCap meter
                coverage side scan.Coverage
                return toStep code
    }

    let rejectCandidate () =
        candNext <- candNext - 1
        job <- None
        stage <- if candNext >= 0 then ResyncStage.VerifyStart else ResyncStage.Discover
        if candNext < 0 then candCount <- 0

    let makeJob (previousIndex: int) (currentIndex: int) (count: int) (bufferA: byte[]) (bufferB: byte[]) =
        if encodings[0] = encodings[1] then
            let compare = RunCompare(aux[0], aux[1], previousIndex, currentIndex, count, bufferA, bufferB)
            job <- Some(fun meter -> compare.Step meter)
        else
            let compare = DecodeCompare(aux[0], aux[1], previousIndex, currentIndex, count, bufferA, bufferB)
            job <- Some(fun meter -> compare.Step meter)

    let verifyStart () =
        let baseIndex = candNext * ResyncStage.CandidateWidth
        let other = 1 - candSide
        candOffset[candSide] <- candData[baseIndex]
        candLine[candSide] <- candData[baseIndex + 1]
        boundOffset[candSide] <- candData[baseIndex + 2]
        boundLine[candSide] <- candData[baseIndex + 3]
        candOffset[other] <- candData[baseIndex + 4]
        candLine[other] <- candData[baseIndex + 5]
        boundOffset[other] <- candData[baseIndex + 6]
        boundLine[other] <- candData[baseIndex + 7]
        for side in 0..1 do
            targetLine[side] <- candLine[side] + float config.ResyncConfirmLines
            let limit = int (min (float auxCap) (targetLine[side] - boundLine[side]))
            auxLimit[side] <- limit
            startSide aux[side] boundOffset[side] boundLine[side] limit
        stage <- ResyncStage.VerifyLoad

    let loaded (side: int) =
        let scan = aux[side]
        float scan.Table.LineBase + float scan.Table.Count >= targetLine[side] || scan.Finished

    let sameKeys (previousIndex: int) (currentIndex: int) =
        let previousTable = aux[0].Table
        let currentTable = aux[1].Table
        previousTable.HashLow previousIndex = currentTable.HashLow currentIndex
        && previousTable.HashHigh previousIndex = currentTable.HashHigh currentIndex
        && previousTable.Length previousIndex = currentTable.Length currentIndex

    let verifyLoad (bufferA: byte[]) (bufferB: byte[]) (meter: Meter) = async {
        let side = if not (loaded 0) then 0 elif not (loaded 1) then 1 else -1
        if side >= 0 then
            let scan = aux[side]
            let table = scan.Table
            if scan.WindowFull && float table.LineBase + float table.Count < targetLine[side] then
                // The window is smaller than the distance to the candidate, so it slides toward the candidate.
                let keep = table.Count - auxCap / 2
                let newOffset = table.Start keep
                let newLine = float table.LineBase + float keep
                let limit = int (min (float auxCap) (targetLine[side] - newLine))
                auxLimit[side] <- limit
                startSide scan newOffset newLine limit
                return ResyncStep.Running
            else
                let! code = loadStep scan (if side = 0 then bufferA else bufferB) auxLimit[side] meter
                coverage side scan.Coverage
                return toStep code
        else
            rowP <- int (candLine[0] - float aux[0].Table.LineBase)
            rowC <- int (candLine[1] - float aux[1].Table.LineBase)
            let availableP = aux[0].Table.Count - rowP
            let availableC = aux[1].Table.Count - rowC
            let confirm = config.ResyncConfirmLines
            let enough = availableP >= confirm && availableC >= confirm || availableP = availableC
            forwardCount <- min confirm (min availableP availableC)
            let mutable equal = enough && forwardCount >= 1
            let mutable index = 0
            while equal && index < forwardCount do
                if not (sameKeys (rowP + index) (rowC + index)) then equal <- false
                index <- index + 1
            Meter.charge meter (index / 16 + 1)
            if equal then
                makeJob rowP rowC forwardCount bufferA bufferB
                stage <- ResyncStage.VerifyForward
            else rejectCandidate ()
            return ResyncStep.Running
    }

    /// Extends the confirmed match backward over equal lines, then compares the extension against the text.
    let beginBackward (bufferA: byte[]) (bufferB: byte[]) (meter: Meter) =
        let limit = min rowP rowC
        let mutable back = 0
        while back < limit && sameKeys (rowP - back - 1) (rowC - back - 1) do
            back <- back + 1
        Meter.charge meter (back / 16 + 1)
        backwardCount <- back
        if back = 0 then true
        else
            makeJob (rowP - back) (rowC - back) back bufferA bufferB
            stage <- ResyncStage.VerifyBackward
            false

    let acceptCandidate () =
        let offsets = [| aux[0].Table.Start(rowP - backwardCount); aux[1].Table.Start(rowC - backwardCount) |]
        let lines = [| candLine[0] - float backwardCount; candLine[1] - float backwardCount |]
        job <- None
        if lines[0] <= rangeLine[0] && lines[1] <= rangeLine[1] then rejectCandidate ()
        else beginEmit offsets lines ResyncStage.FollowHandover

    let verifyStep (bufferA: byte[]) (bufferB: byte[]) (meter: Meter) = async {
        Meter.charge meter 1
        match job with
        | None ->
            stage <- ResyncStage.VerifyStart
            return ResyncStep.Running
        | Some run ->
            let! step = run meter
            match step with
            | CompareStep.Continue -> return ResyncStep.Running
            | CompareStep.Waiting -> return ResyncStep.Waiting
            | CompareStep.Changed ->
                changed ()
                return ResyncStep.Running
            | CompareStep.Finished matched ->
                if stage = ResyncStage.VerifyForward then
                    if matched < forwardCount then rejectCandidate ()
                    elif beginBackward bufferA bufferB meter then acceptCandidate ()
                elif matched >= backwardCount then acceptCandidate ()
                else
                    backwardCount <- backwardCount - matched - 1
                    if backwardCount = 0 then acceptCandidate ()
                    else makeJob (rowP - backwardCount) (rowC - backwardCount) backwardCount bufferA bufferB
                return ResyncStep.Running
    }

    member _.HoldsScratch = reserved || tableHeld disc[0] || tableHeld disc[1] || tableHeld aux[0] || tableHeld aux[1]

    /// Runs one bounded unit of work. The buffers are the session's chunk scratch.
    member _.Step(meter: Meter, bufferA: byte[], bufferB: byte[]) : Async<ResyncStep> = async {
        if not (reserve ()) then return ResyncStep.Reserving
        else
            match stage with
            | ResyncStage.Discover -> return! discoverStep bufferA bufferB meter
            | ResyncStage.VerifyStart ->
                verifyStart ()
                return ResyncStep.Running
            | ResyncStage.VerifyLoad -> return! verifyLoad bufferA bufferB meter
            | ResyncStage.Emit -> return! emitStep bufferA bufferB meter
            | _ -> return! verifyStep bufferA bufferB meter
    }

    /// Frees every line table and the index reservation. The engine cannot run afterwards.
    member _.Release() =
        dropTables ()
        indexes[0].Release()
        indexes[1].Release()
        if reserved then
            ledger.Release(AllocationCategory.SampledIndexes, indexBytes)
            reserved <- false

    member this.Dispose() =
        this.Release()
        for side in 0..1 do
            disc[side].Dispose()
            aux[side].Dispose()

    /// Writes the scalar state. Verification restarts at its candidate after a restore and emission
    /// reloads its lines, so the line tables are not part of the state.
    member _.Export(header: HeaderBuilder) =
        let saved =
            if stage = ResyncStage.VerifyLoad || stage = ResyncStage.VerifyForward || stage = ResyncStage.VerifyBackward then
                ResyncStage.VerifyStart
            else stage
        header.Int saved
        header.Int candSide
        header.Int candCount
        header.Int candNext
        for index = 0 to candCount * ResyncStage.CandidateWidth - 1 do
            header.Number candData[index]
        for side in 0..1 do
            header.Number originOffset[side]
            header.Number originLine[side]
            header.Number rangeOffset[side]
            header.Number rangeLine[side]
            header.Number procOffset[side]
            header.Number procLine[side]
            header.Bool finishedSide[side]
            header.Int indexes[side].Count
            header.Number endOffset[side]
            header.Number endLine[side]
            header.Number emitOffset[side]
            header.Number emitLine[side]
        header.Int emitFollow
        header.Int emitPhase
        header.Int emitKind
        header.Int cutFollow
        header.Number cutCount

    /// Reads what Export wrote. It returns the entry counts of the two indexes.
    member _.Import(header: HeaderReader) =
        stage <- header.Int()
        candSide <- header.Int()
        candCount <- header.Int()
        candNext <- header.Int()
        for index = 0 to candCount * ResyncStage.CandidateWidth - 1 do
            candData[index] <- header.Number()
        let counts = Array.zeroCreate<int> 2
        for side in 0..1 do
            originOffset[side] <- header.Number()
            originLine[side] <- header.Number()
            rangeOffset[side] <- header.Number()
            rangeLine[side] <- header.Number()
            procOffset[side] <- header.Number()
            procLine[side] <- header.Number()
            finishedSide[side] <- header.Bool()
            counts[side] <- header.Int()
            endOffset[side] <- header.Number()
            endLine[side] <- header.Number()
            emitOffset[side] <- header.Number()
            emitLine[side] <- header.Number()
        emitFollow <- header.Int()
        emitPhase <- header.Int()
        emitKind <- header.Int()
        cutFollow <- header.Int()
        cutCount <- header.Number()
        importCounts[0] <- counts[0]
        importCounts[1] <- counts[1]

    /// Reserves the index memory and creates empty arrays for the imported entries. It returns false while
    /// other sessions hold the reservation.
    member _.TryAllocate() =
        if reserved then true
        elif ledger.TryReserve(AllocationCategory.SampledIndexes, indexBytes) then
            reserved <- true
            indexes[0].Prepare importCounts[0]
            indexes[1].Prepare importCounts[1]
            true
        else false

    /// The index arrays that the spilled state holds, in a fixed order.
    member _.Slots: ArraySlot list = indexes[0].Slots(indexes[0].Count) @ indexes[1].Slots(indexes[1].Count)

