namespace VersionControlService.TextDiff

open System
open System.Collections.Generic
open VersionControlService.Abstractions

type internal LineTable(category: AllocationCategory, ledger: Ledger, hashMask: (uint32 * uint32) option) =
    let mutable capacity = 0
    let mutable count = 0
    let mutable starts = Array.empty<float>
    let mutable finishes = Array.empty<float>
    let mutable keyLow = Array.empty<int>
    let mutable keyHigh = Array.empty<int>
    let mutable lengths = Array.empty<float>
    let mutable endings = Array.empty<byte>
    let mutable reserved = 0L
    let mutable lineBase = 0L

    /// Grows the arrays to hold `required` lines. It returns false when the ledger refuses the reservation.
    let tryEnsure required =
        if required <= capacity then true
        else
            let next = max required (max 16 (capacity * 2))
            let nextBytes = int64 next * 40L
            let extra = nextBytes - reserved
            if not (ledger.TryReserve(category, extra)) then false
            else
                let growFloat (source: float[]) =
                    let target = Array.zeroCreate<float> next
                    if count > 0 then Array.Copy(source, target, count)
                    target
                let growInt (source: int[]) =
                    let target = Array.zeroCreate<int> next
                    if count > 0 then Array.Copy(source, target, count)
                    target
                let growByte (source: byte[]) =
                    let target = Array.zeroCreate<byte> next
                    if count > 0 then Array.Copy(source, target, count)
                    target
                starts <- growFloat starts
                finishes <- growFloat finishes
                keyLow <- growInt keyLow
                keyHigh <- growInt keyHigh
                lengths <- growFloat lengths
                endings <- growByte endings
                capacity <- next
                reserved <- nextBytes
                true

    let ensure required =
        if not (tryEnsure required) then invalidOp "The diff window allocation limit was reached."

    member _.TryEnsure(required: int) = tryEnsure required

    /// Sets the line count after the arrays were filled from a spilled copy.
    member _.SetCount(value: int, firstLine: int64) =
        count <- value
        lineBase <- firstLine

    member _.Count = count
    member _.Capacity = capacity
    member _.LineBase = lineBase
    member _.BeginWindow(firstLine: int64) =
        count <- 0
        lineBase <- firstLine
    member _.Starts = starts
    member _.Finishes = finishes
    member _.KeyLow = keyLow
    member _.KeyHigh = keyHigh
    member _.Lengths = lengths
    member _.Endings = endings

    member _.AppendBatch(batch: LineBatch) =
        ensure (count + batch.Count)
        for index = 0 to batch.Count - 1 do
            Native.writeFloat starts count (Native.readFloat batch.StartOffsets index)
            Native.writeFloat finishes count (Native.readFloat batch.EndOffsets index)
            let low = uint32 (Native.readInt batch.KeysLo index)
            let high = uint32 (Native.readInt batch.KeysHi index)
            let low, high =
                match hashMask with
                | Some(lowMask, highMask) -> low &&& lowMask, high &&& highMask
                | None -> low, high
            Native.writeInt keyLow count (int low)
            Native.writeInt keyHigh count (int high)
            Native.writeFloat lengths count (Native.readFloat batch.Utf16Lengths index)
            Native.writeByte endings count (Native.readByte batch.Endings index)
            count <- count + 1

    member _.Start(index: int) = Native.readFloat starts index
    member _.Finish(index: int) = Native.readFloat finishes index
    member _.Length(index: int) = Native.readFloat lengths index
    member _.EndingCode(index: int) = Native.readByte endings index
    member _.HashLow(index: int) = Native.readInt keyLow index
    member _.HashHigh(index: int) = Native.readInt keyHigh index

    member _.Dispose() =
        if reserved > 0L then
            ledger.Release(category, reserved)
            reserved <- 0L
        capacity <- 0
        count <- 0
        starts <- Array.empty
        finishes <- Array.empty
        keyLow <- Array.empty
        keyHigh <- Array.empty
        lengths <- Array.empty
        endings <- Array.empty

    member this.ReleaseWindow() =
        this.Dispose()


[<RequireQualifiedAccess>]
type internal OperationKind =
    | Equal
    | Added
    | Removed
    | Replaced
    | EndingChanged
    | Unaligned

/// A run of lines. Equal, EndingChanged and Replaced runs pair PreviousCount and CurrentCount lines
/// one to one. Removed and Added runs carry only one side. Unaligned runs carry both sides unpaired.
[<Struct>]
type internal DiffOperation = {
    Kind: OperationKind
    PreviousIndex: int
    CurrentIndex: int
    PreviousCount: int
    CurrentCount: int
}

[<RequireQualifiedAccess>]
type internal AlignStep =
    | Running
    /// The step needs scratch memory that other sessions hold. The caller retries later.
    | Waiting
    | NeedRun of previousIndex: int * currentIndex: int * count: int
    | Complete of DiffOperation[]

module internal DiffOperations =
    let make kind previousIndex currentIndex previousCount currentCount = {
        Kind = kind
        PreviousIndex = previousIndex
        CurrentIndex = currentIndex
        PreviousCount = previousCount
        CurrentCount = currentCount
    }

    /// Adds an operation and merges it into the last one when both are contiguous runs of the same kind.
    let appendRaw (operations: ResizeArray<DiffOperation>) (operation: DiffOperation) =
        if operation.PreviousCount > 0 || operation.CurrentCount > 0 then
            let last = operations.Count - 1
            if last >= 0 then
                let tail = operations[last]
                let merged =
                    tail.Kind = operation.Kind
                    && (match operation.Kind with
                        | OperationKind.Equal ->
                            tail.PreviousIndex + tail.PreviousCount = operation.PreviousIndex
                            && tail.CurrentIndex + tail.CurrentCount = operation.CurrentIndex
                        | OperationKind.Removed -> tail.PreviousIndex + tail.PreviousCount = operation.PreviousIndex
                        | OperationKind.Added -> tail.CurrentIndex + tail.CurrentCount = operation.CurrentIndex
                        | _ -> false)
                if merged then
                    operations[last] <-
                        { tail with
                            PreviousCount = tail.PreviousCount + operation.PreviousCount
                            CurrentCount = tail.CurrentCount + operation.CurrentCount }
                else operations.Add operation
            else operations.Add operation

[<RequireQualifiedAccess>]
type private AlignPhase =
    | Prefix
    | PrefixConfirm
    | Suffix
    | SuffixConfirm
    | MiddleStart
    | BuildSlots
    | ProbeCurrent
    | FindCandidates
    | LongestChain
    | RecoverChain
    | AnchorConfirm
    | Gaps
    | Convert
    | Finished

/// Aligns the lines of two windows without ever holding line text. Line keys (two hash halves and the
/// UTF-16 length) select candidates, and the caller confirms every claimed run of equal lines against the
/// source bytes through NeedRun and ResolveRun. The work is split into steps that each do a bounded amount
/// of work and charge the meter.
type internal WindowAligner(previous: LineTable, current: LineTable, stepsPerGap: int, ledger: Ledger) =
    let stepChunk = 512
    let raw = ResizeArray<DiffOperation>()
    let mutable phase = AlignPhase.Prefix
    let previousCount = previous.Count
    let currentCount = current.Count

    let mutable prefixEnd = 0
    let mutable scanned = 0
    let mutable suffixLength = 0
    let mutable suffixPrevious = previousCount
    let mutable suffixCurrent = currentCount
    let mutable pendingLength = 0

    let mutable middlePreviousStart = 0
    let mutable middlePreviousEnd = 0
    let mutable middleCurrentStart = 0
    let mutable middleCurrentEnd = 0

    let mutable slotMask = 0
    let mutable slotPrevious = Array.empty<int>
    let mutable slotPreviousCount = Array.empty<byte>
    let mutable slotCurrent = Array.empty<int>
    let mutable slotCurrentCount = Array.empty<byte>
    let mutable reserved = 0L
    let mutable cursor = 0

    let candidatePrevious = ResizeArray<int>()
    let candidateCurrent = ResizeArray<int>()
    let tails = ResizeArray<int>()
    let tailCandidates = ResizeArray<int>()
    let prior = ResizeArray<int>()
    let chainReversed = ResizeArray<int>()
    let mutable recovery = -1

    let spanPrevious = ResizeArray<int>()
    let spanCurrent = ResizeArray<int>()
    let spanLength = ResizeArray<int>()
    let mutable anchorIndex = 0
    let mutable anchorPrevious = 0
    let mutable anchorCurrent = 0
    let mutable anchorLength = 0
    let mutable anchorPending = false
    let mutable chainSpanStart = -1

    let mutable gapIndex = 0
    let mutable gapStarted = false
    let mutable gapPreviousStart = 0
    let mutable gapCurrentStart = 0
    let mutable gapPreviousEnd = 0
    let mutable gapCurrentEnd = 0
    let mutable stepper: MyersStepper option = None
    let mutable pendingPair = struct (-1, -1)
    let mutable pendingResult: bool option = None
    let gapCache = Dictionary<float, bool>()

    let mutable convertIndex = 0
    let output = ResizeArray<DiffOperation>()
    let mutable equalOffset = 0
    let mutable groupOpen = false
    let mutable groupPreviousStart = 0
    let mutable groupPreviousCount = 0
    let mutable groupCurrentStart = 0
    let mutable groupCurrentCount = 0
    let mutable result = Array.empty<DiffOperation>

    let keysEqual (p: int) (c: int) =
        previous.HashLow p = current.HashLow c
        && previous.HashHigh p = current.HashHigh c
        && previous.Length p = current.Length c

    let slotFor (table: LineTable) index =
        let low = table.HashLow index
        let high = table.HashHigh index
        let mixed = Native.imul low 0x9E3779B1 ^^^ Native.imul high 0x85EBCA6B ^^^ int (table.Length index)
        (mixed ^^^ (mixed >>> 15)) &&& slotMask

    let releaseScratch () =
        if reserved > 0L then
            ledger.Release(AllocationCategory.AlignmentScratch, reserved)
            reserved <- 0L
        slotPrevious <- Array.empty
        slotPreviousCount <- Array.empty
        slotCurrent <- Array.empty
        slotCurrentCount <- Array.empty

    let emitBulk previousStart previousEnd currentStart currentEnd =
        DiffOperations.appendRaw raw (DiffOperations.make OperationKind.Removed previousStart -1 (previousEnd - previousStart) 0)
        DiffOperations.appendRaw raw (DiffOperations.make OperationKind.Added -1 currentStart 0 (currentEnd - currentStart))

    let beginGaps () =
        anchorIndex <- 0
        gapIndex <- 0
        gapPreviousStart <- middlePreviousStart
        gapCurrentStart <- middleCurrentStart
        phase <- AlignPhase.Gaps

    let beginConvert () =
        releaseScratch ()
        let suffixCount = previousCount - suffixPrevious
        if suffixCount > 0 then
            DiffOperations.appendRaw raw (DiffOperations.make OperationKind.Equal suffixPrevious suffixCurrent suffixCount suffixCount)
        convertIndex <- 0
        phase <- AlignPhase.Convert

    let finishMiddle () = beginConvert ()

    /// Returns false when the scratch reservation is not available yet, so the caller retries the step.
    let startMiddle () =
        middlePreviousStart <- prefixEnd
        middleCurrentStart <- prefixEnd
        middlePreviousEnd <- suffixPrevious
        middleCurrentEnd <- suffixCurrent
        let previousMiddle = middlePreviousEnd - middlePreviousStart
        let currentMiddle = middleCurrentEnd - middleCurrentStart
        if previousMiddle <= 0 && currentMiddle <= 0 then
            finishMiddle ()
            true
        elif previousMiddle <= 0 || currentMiddle <= 0 then
            emitBulk middlePreviousStart middlePreviousEnd middleCurrentStart middleCurrentEnd
            finishMiddle ()
            true
        else
            let mutable slots = 16
            while slots < previousMiddle * 2 do slots <- slots * 2
            let estimate = int64 slots * 10L + int64 (min previousMiddle currentMiddle) * 40L + 4096L
            if ledger.TryReserve(AllocationCategory.AlignmentScratch, estimate) then
                reserved <- estimate
                slotMask <- slots - 1
                slotPrevious <- Array.create slots -1
                slotPreviousCount <- Array.zeroCreate slots
                slotCurrent <- Array.zeroCreate slots
                slotCurrentCount <- Array.zeroCreate slots
                cursor <- middlePreviousStart
                phase <- AlignPhase.BuildSlots
                true
            else false

    let equalOracle (p: int) (c: int) =
        if not (keysEqual p c) then Some false
        else
            match gapCache.TryGetValue(float p * 4294967296.0 + float c) with
            | true, value -> Some value
            | _ -> None

    let finishGap () =
        gapStarted <- false
        if gapIndex < spanPrevious.Count then
            let p = spanPrevious[gapIndex]
            let c = spanCurrent[gapIndex]
            let n = spanLength[gapIndex]
            DiffOperations.appendRaw raw (DiffOperations.make OperationKind.Equal p c n n)
            gapPreviousStart <- p + n
            gapCurrentStart <- c + n
            gapIndex <- gapIndex + 1
        else beginConvert ()

    let startGap (meter: Meter) =
        gapCache.Clear()
        gapStarted <- true
        if gapIndex < spanPrevious.Count then
            gapPreviousEnd <- spanPrevious[gapIndex]
            gapCurrentEnd <- spanCurrent[gapIndex]
        else
            gapPreviousEnd <- middlePreviousEnd
            gapCurrentEnd <- middleCurrentEnd
        if gapPreviousEnd <= gapPreviousStart || gapCurrentEnd <= gapCurrentStart then
            emitBulk gapPreviousStart gapPreviousEnd gapCurrentStart gapCurrentEnd
            finishGap ()
        else
            stepper <-
                Some(
                    MyersStepper(
                        gapPreviousStart,
                        gapPreviousEnd - gapPreviousStart,
                        gapCurrentStart,
                        gapCurrentEnd - gapCurrentStart,
                        stepsPerGap,
                        meter,
                        equalOracle
                    )
                )

    member _.Dispose() = releaseScratch ()

    member _.ResolveRun(matched: int) =
        match phase with
        | AlignPhase.PrefixConfirm ->
            DiffOperations.appendRaw raw (DiffOperations.make OperationKind.Equal 0 0 matched matched)
            prefixEnd <- matched
            scanned <- 0
            phase <- AlignPhase.Suffix
        | AlignPhase.SuffixConfirm ->
            if matched >= pendingLength then
                phase <- AlignPhase.MiddleStart
            else
                let skip = matched + 1
                suffixPrevious <- suffixPrevious + skip
                suffixCurrent <- suffixCurrent + skip
                suffixLength <- suffixLength - skip
                pendingLength <- suffixLength
                if suffixLength <= 0 then
                    suffixPrevious <- previousCount
                    suffixCurrent <- currentCount
                    suffixLength <- 0
                    phase <- AlignPhase.MiddleStart
        | AlignPhase.AnchorConfirm ->
            if matched > 0 then
                spanPrevious.Add anchorPrevious
                spanCurrent.Add anchorCurrent
                spanLength.Add matched
            if matched < anchorLength then
                anchorPrevious <- anchorPrevious + matched + 1
                anchorCurrent <- anchorCurrent + matched + 1
                anchorLength <- anchorLength - matched - 1
            else anchorLength <- 0
            anchorPending <- false
        | AlignPhase.Gaps ->
            let struct (p, c) = pendingPair
            gapCache[float p * 4294967296.0 + float c] <- matched > 0
            pendingResult <- Some(matched > 0)
        | _ -> invalidOp "No confirmation was requested."

    member _.Step(meter: Meter) : AlignStep =
        Meter.charge meter 1
        match phase with
        | AlignPhase.Prefix ->
            let limit = min previousCount currentCount
            let mutable count = 0
            while scanned < limit && count < stepChunk && keysEqual scanned scanned do
                scanned <- scanned + 1
                count <- count + 1
            Meter.charge meter (count / 64)
            if scanned < limit && count >= stepChunk then AlignStep.Running
            elif scanned > 0 then
                phase <- AlignPhase.PrefixConfirm
                pendingLength <- scanned
                AlignStep.NeedRun(0, 0, scanned)
            else
                scanned <- 0
                phase <- AlignPhase.Suffix
                AlignStep.Running
        | AlignPhase.PrefixConfirm -> AlignStep.NeedRun(0, 0, pendingLength)
        | AlignPhase.Suffix ->
            let limit = min previousCount currentCount - prefixEnd
            let mutable count = 0
            while scanned < limit && count < stepChunk && keysEqual (previousCount - 1 - scanned) (currentCount - 1 - scanned) do
                scanned <- scanned + 1
                count <- count + 1
            Meter.charge meter (count / 64)
            if scanned < limit && count >= stepChunk then AlignStep.Running
            else
                suffixLength <- scanned
                suffixPrevious <- previousCount - scanned
                suffixCurrent <- currentCount - scanned
                pendingLength <- scanned
                phase <- if scanned > 0 then AlignPhase.SuffixConfirm else AlignPhase.MiddleStart
                AlignStep.Running
        | AlignPhase.SuffixConfirm -> AlignStep.NeedRun(suffixPrevious, suffixCurrent, pendingLength)
        | AlignPhase.MiddleStart -> if startMiddle () then AlignStep.Running else AlignStep.Waiting
        | AlignPhase.BuildSlots ->
            let mutable count = 0
            while cursor < middlePreviousEnd && count < stepChunk do
                let mutable slot = slotFor previous cursor
                let mutable placed = false
                while not placed do
                    let holder = Native.readInt slotPrevious slot
                    if holder < 0 then
                        Native.writeInt slotPrevious slot cursor
                        Native.writeByte slotPreviousCount slot 1
                        placed <- true
                    elif previous.HashLow holder = previous.HashLow cursor
                         && previous.HashHigh holder = previous.HashHigh cursor
                         && previous.Length holder = previous.Length cursor then
                        if Native.readByte slotPreviousCount slot < 2 then Native.writeByte slotPreviousCount slot 2
                        placed <- true
                    else slot <- (slot + 1) &&& slotMask
                cursor <- cursor + 1
                count <- count + 1
            Meter.charge meter (count / 8)
            if cursor >= middlePreviousEnd then
                cursor <- middleCurrentStart
                phase <- AlignPhase.ProbeCurrent
            AlignStep.Running
        | AlignPhase.ProbeCurrent ->
            let mutable count = 0
            while cursor < middleCurrentEnd && count < stepChunk do
                let mutable slot = slotFor current cursor
                let mutable searching = true
                while searching do
                    let holder = Native.readInt slotPrevious slot
                    if holder < 0 then searching <- false
                    elif keysEqual holder cursor then
                        let seen = Native.readByte slotCurrentCount slot
                        if seen = 0 then Native.writeInt slotCurrent slot cursor
                        if seen < 2 then Native.writeByte slotCurrentCount slot (seen + 1)
                        searching <- false
                    else slot <- (slot + 1) &&& slotMask
                cursor <- cursor + 1
                count <- count + 1
            Meter.charge meter (count / 8)
            if cursor >= middleCurrentEnd then
                cursor <- middlePreviousStart
                phase <- AlignPhase.FindCandidates
            AlignStep.Running
        | AlignPhase.FindCandidates ->
            let mutable count = 0
            while cursor < middlePreviousEnd && count < stepChunk do
                let mutable slot = slotFor previous cursor
                let mutable searching = true
                while searching do
                    let holder = Native.readInt slotPrevious slot
                    if holder < 0 then searching <- false
                    elif previous.HashLow holder = previous.HashLow cursor
                         && previous.HashHigh holder = previous.HashHigh cursor
                         && previous.Length holder = previous.Length cursor then
                        if Native.readByte slotPreviousCount slot = 1 && Native.readByte slotCurrentCount slot = 1 then
                            candidatePrevious.Add cursor
                            candidateCurrent.Add(Native.readInt slotCurrent slot)
                        searching <- false
                    else slot <- (slot + 1) &&& slotMask
                cursor <- cursor + 1
                count <- count + 1
            Meter.charge meter (count / 8)
            if cursor >= middlePreviousEnd then
                cursor <- 0
                phase <- AlignPhase.LongestChain
            AlignStep.Running
        | AlignPhase.LongestChain ->
            let mutable count = 0
            while cursor < candidatePrevious.Count && count < stepChunk do
                let value = candidateCurrent[cursor]
                let mutable low = 0
                let mutable high = tails.Count
                while low < high do
                    let middle = low + ((high - low) >>> 1)
                    if tails[middle] < value then low <- middle + 1 else high <- middle
                prior.Add(if low > 0 then tailCandidates[low - 1] else -1)
                if low = tails.Count then
                    tails.Add value
                    tailCandidates.Add cursor
                else
                    tails[low] <- value
                    tailCandidates[low] <- cursor
                cursor <- cursor + 1
                count <- count + 1
            Meter.charge meter (count / 4)
            if cursor >= candidatePrevious.Count then
                recovery <- if tails.Count > 0 then tailCandidates[tails.Count - 1] else -1
                phase <- AlignPhase.RecoverChain
            AlignStep.Running
        | AlignPhase.RecoverChain ->
            let mutable count = 0
            while recovery >= 0 && count < stepChunk do
                chainReversed.Add recovery
                recovery <- prior[recovery]
                count <- count + 1
            Meter.charge meter (count / 8)
            if recovery < 0 then
                anchorIndex <- chainReversed.Count - 1
                anchorPending <- false
                anchorLength <- 0
                phase <- AlignPhase.AnchorConfirm
            AlignStep.Running
        | AlignPhase.AnchorConfirm ->
            if anchorPending then AlignStep.NeedRun(anchorPrevious, anchorCurrent, anchorLength)
            elif anchorLength > 0 then
                anchorPending <- true
                AlignStep.NeedRun(anchorPrevious, anchorCurrent, anchorLength)
            elif anchorIndex < 0 then
                beginGaps ()
                AlignStep.Running
            else
                // Coalesce consecutive candidates into one span so one confirmation covers them.
                let first = chainReversed[anchorIndex]
                anchorPrevious <- candidatePrevious[first]
                anchorCurrent <- candidateCurrent[first]
                anchorLength <- 1
                anchorIndex <- anchorIndex - 1
                let mutable extending = true
                while extending && anchorIndex >= 0 do
                    let next = chainReversed[anchorIndex]
                    if candidatePrevious[next] = anchorPrevious + anchorLength && candidateCurrent[next] = anchorCurrent + anchorLength then
                        anchorLength <- anchorLength + 1
                        anchorIndex <- anchorIndex - 1
                    else extending <- false
                Meter.charge meter (anchorLength / 8)
                anchorPending <- true
                AlignStep.NeedRun(anchorPrevious, anchorCurrent, anchorLength)
        | AlignPhase.Gaps ->
            match stepper with
            | Some active ->
                active.UseMeter meter
                match pendingResult with
                | Some value ->
                    pendingResult <- None
                    active.ResolveComparison value
                | None -> ()
                match active.Step() with
                | MyersStepResult.Running -> AlignStep.Running
                | MyersStepResult.NeedComparison(p, c) ->
                    pendingPair <- struct (p, c)
                    AlignStep.NeedRun(p, c, 1)
                | MyersStepResult.Complete operations ->
                    for operation in operations do
                        match operation with
                        | MyersOperation.Equal(p, c) -> DiffOperations.appendRaw raw (DiffOperations.make OperationKind.Equal p c 1 1)
                        | MyersOperation.Delete p -> DiffOperations.appendRaw raw (DiffOperations.make OperationKind.Removed p -1 1 0)
                        | MyersOperation.Insert c -> DiffOperations.appendRaw raw (DiffOperations.make OperationKind.Added -1 c 0 1)
                    Meter.charge meter (operations.Length / 64)
                    stepper <- None
                    finishGap ()
                    AlignStep.Running
                | MyersStepResult.StepLimitExceeded ->
                    // The gap exceeded its step budget, so its lines are reported as one unaligned region.
                    raw.Add(
                        DiffOperations.make
                            OperationKind.Unaligned
                            gapPreviousStart
                            gapCurrentStart
                            (gapPreviousEnd - gapPreviousStart)
                            (gapCurrentEnd - gapCurrentStart)
                    )
                    stepper <- None
                    finishGap ()
                    AlignStep.Running
            | None ->
                startGap meter
                AlignStep.Running
        | AlignPhase.Convert ->
            let mutable budget = 4096
            let flushGroup () =
                if groupOpen then
                    let paired = min groupPreviousCount groupCurrentCount
                    if paired > 0 then
                        output.Add(DiffOperations.make OperationKind.Replaced groupPreviousStart groupCurrentStart paired paired)
                    if groupPreviousCount > paired then
                        output.Add(DiffOperations.make OperationKind.Removed (groupPreviousStart + paired) -1 (groupPreviousCount - paired) 0)
                    if groupCurrentCount > paired then
                        output.Add(DiffOperations.make OperationKind.Added -1 (groupCurrentStart + paired) 0 (groupCurrentCount - paired))
                    groupOpen <- false
                    groupPreviousCount <- 0
                    groupCurrentCount <- 0
            while budget > 0 && convertIndex < raw.Count do
                let operation = raw[convertIndex]
                match operation.Kind with
                | OperationKind.Equal ->
                    flushGroup ()
                    let n = operation.PreviousCount
                    let start = equalOffset
                    let endingsMatch index =
                        previous.EndingCode(operation.PreviousIndex + index) = current.EndingCode(operation.CurrentIndex + index)
                    let same = endingsMatch start
                    let mutable index = start
                    while index < n && budget > 0 && endingsMatch index = same do
                        index <- index + 1
                        budget <- budget - 1
                    let kind = if same then OperationKind.Equal else OperationKind.EndingChanged
                    output.Add(
                        DiffOperations.make kind (operation.PreviousIndex + start) (operation.CurrentIndex + start) (index - start) (index - start)
                    )
                    if index >= n then
                        equalOffset <- 0
                        convertIndex <- convertIndex + 1
                    else equalOffset <- index
                | OperationKind.Removed ->
                    if not groupOpen || groupPreviousCount = 0 then groupPreviousStart <- operation.PreviousIndex
                    groupOpen <- true
                    groupPreviousCount <- groupPreviousCount + operation.PreviousCount
                    convertIndex <- convertIndex + 1
                    budget <- budget - 1
                | OperationKind.Added ->
                    if not groupOpen || groupCurrentCount = 0 then groupCurrentStart <- operation.CurrentIndex
                    groupOpen <- true
                    groupCurrentCount <- groupCurrentCount + operation.CurrentCount
                    convertIndex <- convertIndex + 1
                    budget <- budget - 1
                | _ ->
                    flushGroup ()
                    output.Add operation
                    convertIndex <- convertIndex + 1
                    budget <- budget - 1
            Meter.charge meter ((4096 - budget) / 64)
            if convertIndex >= raw.Count then
                flushGroup ()
                result <- output.ToArray()
                phase <- AlignPhase.Finished
            AlignStep.Running
        | AlignPhase.Finished -> AlignStep.Complete result
