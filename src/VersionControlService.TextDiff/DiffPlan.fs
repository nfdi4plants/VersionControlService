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
    /// The equal run can settle a window. Its lines were confirmed as a unique match, or at least two
    /// anchored equal lines follow it directly, or it follows changed lines directly and is long or holds a
    /// line that occurs once in each window.
    Anchored: bool
    /// The run belongs to the equal lines at the window start that line up position for position.
    PrefixAnchor: bool
    /// The fast equal-byte scan may continue after this run.
    ReenterAnchor: bool
}

[<RequireQualifiedAccess>]
type internal AlignStep =
    | Running
    | NeedRun of previousIndex: int * currentIndex: int * count: int
    | Complete of DiffOperation[]

module internal DiffOperations =
    let make kind previousIndex currentIndex previousCount currentCount = {
        Kind = kind
        PreviousIndex = previousIndex
        CurrentIndex = currentIndex
        PreviousCount = previousCount
        CurrentCount = currentCount
        Anchored = false
        PrefixAnchor = false
        ReenterAnchor = false
    }

    let makeAnchored kind previousIndex currentIndex previousCount currentCount =
        { make kind previousIndex currentIndex previousCount currentCount with Anchored = true; ReenterAnchor = true }

    /// Adds an operation and merges it into the last one when both are contiguous runs of the same kind.
    let appendRaw (operations: ResizeArray<DiffOperation>) (operation: DiffOperation) =
        if operation.PreviousCount > 0 || operation.CurrentCount > 0 then
            let last = operations.Count - 1
            if last >= 0 then
                let tail = operations[last]
                let merged =
                    (match tail.Kind, operation.Kind with
                     | OperationKind.Equal, OperationKind.Equal
                     | OperationKind.Removed, OperationKind.Removed
                     | OperationKind.Added, OperationKind.Added -> true
                     | _ -> false)
                    && tail.Anchored = operation.Anchored
                    && tail.PrefixAnchor = operation.PrefixAnchor
                    && tail.ReenterAnchor = operation.ReenterAnchor
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
    | PositionalConfirm
    | BuildSlots
    | ProbeCurrent
    | FindCandidates
    | LongestChain
    | RecoverChain
    | AnchorConfirm
    | RestoreConfirm
    | Gaps
    | DirectConfirm
    | CountUnique
    | Convert
    | MarkRuns
    | Finished

/// Lines in an equal run that the aligner and the session both treat as a long run.
module internal AlignerLimits =
    [<Literal>]
    let LongRunLines = 64

/// Aligns the lines of two windows without ever holding line text. Line keys (two hash halves and the
/// UTF-16 length) select candidates, and the caller confirms every claimed run of equal lines against the
/// source bytes through NeedRun and ResolveRun. The work is split into steps that each do a bounded amount
/// of work and charge the meter. A window of a source that is still growing, or a window that can still grow,
/// passes false for longRunAnchors, because a long equal run in repetitive text can line up at a shifted
/// position until more lines arrive.
type internal WindowAligner(previous: LineTable, current: LineTable, stepsPerGap: int, sameSourceLength: bool, longRunAnchors: bool, directPass: bool, ledger: Ledger) =
    let stepChunk = 512
    let lookaheadLines = 2
    let longRunLines = AlignerLimits.LongRunLines
    let raw = ResizeArray<DiffOperation>()
    let mutable phase = AlignPhase.Prefix
    let previousCount = previous.Count
    let currentCount = current.Count

    let mutable prefixEnd = 0
    let mutable scanned = 0
    let mutable suffixLength = 0
    let mutable suffixPrevious = previousCount
    let mutable suffixCurrent = currentCount
    let mutable prefixScanLength = 0
    let mutable prefixConfirmedLength = 0
    let mutable pendingLength = 0

    let mutable middlePreviousStart = 0
    let mutable middlePreviousEnd = 0
    let mutable middleCurrentStart = 0
    let mutable middleCurrentEnd = 0
    let positionalOperations = ResizeArray<DiffOperation>()
    let mutable positionalPrevious = 0
    let mutable positionalCurrent = 0
    let mutable positionalRemaining = 0
    let mutable positionalMismatches = 0
    let mutable positionalAttempted = false
    let mutable positionalConfirmed = false

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

    // Spans that rule C rejected, in chain order. A gap that merged them and then exhausts its step budget gets
    // them back as anchors, which splits it into the smaller gaps it had without the rejection.
    let rejectedPrevious = ResizeArray<int>()
    let rejectedCurrent = ResizeArray<int>()
    let rejectedLength = ResizeArray<int>()
    let mutable restoreCursor = 0
    let mutable restoreEnd = 0
    let mutable restoreAt = 0
    let mutable restoreFirst = 0
    let mutable lookupProbes = 0

    let mutable directAttempted = false
    let mutable directActive = false
    let directOperations = ResizeArray<DiffOperation>()
    let mutable directConfirmIndex = 0
    let mutable gapIndex = 0
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

    // How often each line key occurs in each window, capped at two. The table is built once per window after
    // the gaps are aligned. A holder below previousCount is a previous line, a larger one is a current line.
    let mutable uniqueMask = 0
    let mutable uniqueHolder = Array.empty<int>
    let mutable uniquePreviousCount = Array.empty<byte>
    let mutable uniqueCurrentCount = Array.empty<byte>
    let mutable uniqueReserved = 0L
    let mutable uniqueCursor = 0
    let mutable selfUniqueLines = 0
    let mutable markIndex = 0
    let mutable markAfterChange = false

    let keysEqual (p: int) (c: int) =
        previous.HashLow p = current.HashLow c
        && previous.HashHigh p = current.HashHigh c
        && previous.Length p = current.Length c

    let holderMatches (holder: int) (table: LineTable) index =
        if holder < previousCount then
            previous.HashLow holder = table.HashLow index
            && previous.HashHigh holder = table.HashHigh index
            && previous.Length holder = table.Length index
        else
            let line = holder - previousCount
            current.HashLow line = table.HashLow index
            && current.HashHigh line = table.HashHigh index
            && current.Length line = table.Length index

    let uniqueSlot (table: LineTable) index =
        let low = table.HashLow index
        let high = table.HashHigh index
        let mixed = Native.imul low 0x9E3779B1 ^^^ Native.imul high 0x85EBCA6B ^^^ int (table.Length index)
        let mutable slot = (mixed ^^^ (mixed >>> 15)) &&& uniqueMask
        let mutable searching = true
        while searching do
            let holder = Native.readInt uniqueHolder slot
            if holder < 0 || holderMatches holder table index then searching <- false
            else slot <- (slot + 1) &&& uniqueMask
        slot

    /// A worker runs one session, so the ledger always has room for the alignment scratch. A refusal is a bug.
    let reserveUnique () =
        let mutable slots = 16
        while slots < (previousCount + currentCount) * 2 do slots <- slots * 2
        let estimate = int64 slots * 6L + 4096L
        if not (ledger.TryReserve(AllocationCategory.AlignmentScratch, estimate)) then
            invalidOp "The ledger refused the alignment scratch for the unique line count."
        uniqueReserved <- estimate
        uniqueMask <- slots - 1
        uniqueHolder <- Array.create slots -1
        uniquePreviousCount <- Array.zeroCreate slots
        uniqueCurrentCount <- Array.zeroCreate slots

    let releaseUnique () =
        if uniqueReserved > 0L then
            ledger.Release(AllocationCategory.AlignmentScratch, uniqueReserved)
            uniqueReserved <- 0L
        uniqueHolder <- Array.empty
        uniquePreviousCount <- Array.empty
        uniqueCurrentCount <- Array.empty

    let isUnique slot =
        Native.readByte uniquePreviousCount slot = 1 && Native.readByte uniqueCurrentCount slot = 1

    let countLine (table: LineTable) index (holder: int) (counts: byte[]) =
        let slot = uniqueSlot table index
        if Native.readInt uniqueHolder slot < 0 then Native.writeInt uniqueHolder slot holder
        let seen = Native.readByte counts slot
        if seen < 2 then Native.writeByte counts slot (seen + 1)
        if seen = 0 then selfUniqueLines <- selfUniqueLines + 1
        elif seen = 1 then selfUniqueLines <- selfUniqueLines - 1

    /// True when the key of a previous line occurs exactly once in each window.
    let uniqueInBoth (previousIndex: int) = isUnique (uniqueSlot previous previousIndex)

    let isEqualKind kind = kind = OperationKind.Equal || kind = OperationKind.EndingChanged

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

    /// Counts the line keys of both windows before the operations are converted.
    let beginConvert () =
        releaseScratch ()
        uniqueCursor <- 0
        selfUniqueLines <- 0
        phase <- AlignPhase.CountUnique

    let appendSuffix () =
        let suffixCount = previousCount - suffixPrevious
        let mutable uniqueSuffix = suffixCount >= lookaheadLines
        for offset = 0 to lookaheadLines - 1 do
            if uniqueSuffix && not (uniqueInBoth (previousCount - 1 - offset)) then
                uniqueSuffix <- false
        let suffixAnchored = suffixCount >= lookaheadLines && (uniqueSuffix || positionalConfirmed)
        if suffixCount > 0 then
            let suffix =
                if uniqueSuffix then DiffOperations.makeAnchored OperationKind.Equal suffixPrevious suffixCurrent suffixCount suffixCount
                elif suffixAnchored then { DiffOperations.make OperationKind.Equal suffixPrevious suffixCurrent suffixCount suffixCount with Anchored = true }
                else DiffOperations.make OperationKind.Equal suffixPrevious suffixCurrent suffixCount suffixCount
            DiffOperations.appendRaw raw suffix
        convertIndex <- 0
        phase <- AlignPhase.Convert

    let finishMiddle () = beginConvert ()

    /// A worker runs one session, so the ledger always has room for the alignment scratch. A refusal is a bug.
    let startMiddle () =
        middlePreviousStart <- prefixEnd
        middleCurrentStart <- prefixEnd
        middlePreviousEnd <- suffixPrevious
        middleCurrentEnd <- suffixCurrent
        let previousMiddle = middlePreviousEnd - middlePreviousStart
        let currentMiddle = middleCurrentEnd - middleCurrentStart
        if previousMiddle <= 0 && currentMiddle <= 0 then
            finishMiddle ()
        elif previousMiddle <= 0 || currentMiddle <= 0 then
            emitBulk middlePreviousStart middlePreviousEnd middleCurrentStart middleCurrentEnd
            finishMiddle ()
        else
            let mutable slots = 16
            while slots < previousMiddle * 2 do slots <- slots * 2
            let estimate = int64 slots * 10L + int64 (min previousMiddle currentMiddle) * 40L + 4096L
            if not (ledger.TryReserve(AllocationCategory.AlignmentScratch, estimate)) then
                invalidOp "The ledger refused the alignment scratch for the middle section."
            reserved <- estimate
            slotMask <- slots - 1
            slotPrevious <- Array.create slots -1
            slotPreviousCount <- Array.zeroCreate slots
            slotCurrent <- Array.zeroCreate slots
            slotCurrentCount <- Array.zeroCreate slots
            cursor <- middlePreviousStart
            phase <- AlignPhase.BuildSlots

    /// A window in which both sources end, whether it holds both whole files or the last part of larger ones, aligns
    /// its middle with one Myers pass first. Where a chance anchor would cost extra changed lines, the pass
    /// finds an alignment close to git's line diff. It has no indent heuristic and no slider compaction, so its
    /// hunks can sit differently from git's. It compares keys only, and DirectConfirm checks the equal runs
    /// against the source bytes afterwards, because a confirmation per line is slower than the pass itself. The pass
    /// gets twice the per-gap step budget, which is enough for every corpus pair it improves. When the pass
    /// completes, its result is the alignment. When it exceeds the budget, or two different lines turn out to
    /// share a key, the aligner drops its result and runs the anchor path as if the pass never started.
    let startDirect () =
        directAttempted <- true
        if suffixPrevious > prefixEnd && suffixCurrent > prefixEnd then
            middlePreviousStart <- prefixEnd
            middleCurrentStart <- prefixEnd
            middlePreviousEnd <- suffixPrevious
            middleCurrentEnd <- suffixCurrent
            directActive <- true
            beginGaps ()

    let tryStartPositionalConfirm () =
        let previousMiddle = suffixPrevious - prefixEnd
        let currentMiddle = suffixCurrent - prefixEnd
        positionalAttempted <- true
        let mutable candidate =
            sameSourceLength
            && previousMiddle > 0
            && previousMiddle = currentMiddle
            && previous.LineBase = current.LineBase
            && prefixEnd < previousCount
            && prefixEnd < currentCount
            && previous.Start(prefixEnd) = current.Start(prefixEnd)
        let mutable keyMismatches = 0
        let mutable offset = 0
        let mismatchOffsets = ResizeArray<int>()
        while candidate && offset < previousMiddle do
            if not (keysEqual (prefixEnd + offset) (prefixEnd + offset)) then
                keyMismatches <- keyMismatches + 1
                if keyMismatches > lookaheadLines * 4 then candidate <- false
                else mismatchOffsets.Add(prefixEnd + offset)
            offset <- offset + 1
        // Adjacent lines that swapped places or rotated show fewer changed lines as a move than as paired
        // replacements (2 against 4 for a swap), so the general alignment takes them.
        if candidate && mismatchOffsets.Count > 1 && mismatchOffsets[mismatchOffsets.Count - 1] - mismatchOffsets[0] + 1 = mismatchOffsets.Count then
            let mutable moved = false
            for first in mismatchOffsets do
                for second in mismatchOffsets do
                    if first <> second && keysEqual first second then moved <- true
            if moved then candidate <- false
        if candidate then
            positionalPrevious <- prefixEnd
            positionalCurrent <- prefixEnd
            positionalRemaining <- previousMiddle
            positionalMismatches <- 0
            positionalOperations.Clear()
            phase <- AlignPhase.PositionalConfirm
            true
        else false

    let equalOracle (p: int) (c: int) =
        if not (keysEqual p c) then Some false
        elif directActive then Some true
        else
            match gapCache.TryGetValue(float p * 4294967296.0 + float c) with
            | true, value -> Some value
            | _ -> None

    let finishGap () =
        if gapIndex < spanPrevious.Count then
            let p = spanPrevious[gapIndex]
            let c = spanCurrent[gapIndex]
            let n = spanLength[gapIndex]
            DiffOperations.appendRaw raw (DiffOperations.makeAnchored OperationKind.Equal p c n n)
            gapPreviousStart <- p + n
            gapCurrentStart <- c + n
            gapIndex <- gapIndex + 1
        else beginConvert ()

    let confirmAnchor () =
        anchorPending <- true
        AlignStep.NeedRun(anchorPrevious, anchorCurrent, anchorLength)

    /// Counts matching line pairs from (p, c) stepping by (dp, dc) inside the middle section, up to longRunLines.
    let matchedRun (p: int) (c: int) (dp: int) (dc: int) =
        let mutable k = 0
        while k < longRunLines
              && p + k * dp >= middlePreviousStart && p + k * dp < middlePreviousEnd
              && c + k * dc >= middleCurrentStart && c + k * dc < middleCurrentEnd
              && keysEqual (p + k * dp) (c + k * dc) do
            k <- k + 1
        lookupProbes <- lookupProbes + k
        k

    /// The first line of the current middle with the key of a previous line, or -1 when the key does not occur
    /// there. A collision chain longer than longRunLines answers -1, which keeps the candidate.
    let firstCurrent (p: int) =
        let mutable slot = slotFor previous p
        let mutable found = -1
        let mutable steps = 0
        let mutable searching = true
        while searching do
            let holder = Native.readInt slotPrevious slot
            if holder < 0 || steps >= longRunLines then searching <- false
            elif previous.HashLow holder = previous.HashLow p
                 && previous.HashHigh holder = previous.HashHigh p
                 && previous.Length holder = previous.Length p then
                if Native.readByte slotCurrentCount slot > 0 then found <- Native.readInt slotCurrent slot
                searching <- false
            else
                slot <- (slot + 1) &&& slotMask
                steps <- steps + 1
        lookupProbes <- lookupProbes + steps + 1
        found

    /// The first line of the previous middle with the key of a current line, or -1 when the key does not occur
    /// there. A collision chain longer than longRunLines answers -1, which keeps the candidate.
    let firstPrevious (c: int) =
        let mutable slot = slotFor current c
        let mutable found = -1
        let mutable steps = 0
        let mutable searching = true
        while searching do
            let holder = Native.readInt slotPrevious slot
            if holder < 0 || steps >= longRunLines then searching <- false
            elif keysEqual holder c then
                found <- holder
                searching <- false
            else
                slot <- (slot + 1) &&& slotMask
                steps <- steps + 1
        lookupProbes <- lookupProbes + steps + 1
        found

    /// A span of lines that occur once in each window is no anchor when it moved across a long equal run. The
    /// longRunLines lines before it on one side sit after it on the other side, either right after the span or
    /// where their first line first occurs. Anchored, the span would turn that run into removed and added lines.
    /// Left to the gap, the run aligns and the span becomes the moved line. Lines moved together are each
    /// rejected on their own, so the order of the chain does not matter.
    let movedAcrossRun () =
        let before = anchorPrevious - 1
        let after = anchorCurrent + anchorLength
        let previousBlock = anchorPrevious - longRunLines
        let currentBlock = anchorCurrent - longRunLines
        (matchedRun before (anchorCurrent - 1) (-1) (-1) < longRunLines
         && (matchedRun before after (-1) 1 >= longRunLines
             || (previousBlock >= middlePreviousStart
                 && (let f = firstCurrent previousBlock in f >= after && matchedRun previousBlock f 1 1 >= longRunLines))))
        || (matchedRun (anchorPrevious + anchorLength) after 1 1 < longRunLines
            && (matchedRun (anchorPrevious + anchorLength) (anchorCurrent - 1) 1 (-1) >= longRunLines
                || (currentBlock >= middleCurrentStart
                    && (let f = firstPrevious currentBlock in f >= anchorPrevious + anchorLength && matchedRun f currentBlock 1 1 >= longRunLines))))

    let markLookaheadAnchors () =
        let mutable followingPrevious = -1
        let mutable followingCurrent = -1
        let mutable followingEqualLines = 0
        for index = result.Length - 1 downto 0 do
            let operation = result[index]
            if operation.Kind = OperationKind.Equal || operation.Kind = OperationKind.EndingChanged then
                let previousEnd = operation.PreviousIndex + operation.PreviousCount
                let currentEnd = operation.CurrentIndex + operation.CurrentCount
                if followingEqualLines = 0 || previousEnd <> followingPrevious || currentEnd <> followingCurrent then
                    followingEqualLines <- 0
                if operation.Anchored then
                    followingEqualLines <- followingEqualLines + operation.PreviousCount
                elif operation.Kind = OperationKind.Equal && followingEqualLines >= lookaheadLines then
                    result[index] <- { operation with Anchored = true }
                    followingEqualLines <- followingEqualLines + operation.PreviousCount
                else
                    followingEqualLines <- 0
                followingPrevious <- operation.PreviousIndex
                followingCurrent <- operation.CurrentIndex
            else
                followingEqualLines <- 0
                followingPrevious <- -1
                followingCurrent <- -1

    let markAlignedPrefix () =
        let mutable openPrefix = true
        let mutable previousEnd = 0
        let mutable currentEnd = 0
        for index = 0 to result.Length - 1 do
            let operation = result[index]
            if openPrefix
               && operation.Anchored
               && (operation.Kind = OperationKind.Equal || operation.Kind = OperationKind.EndingChanged)
               && operation.PreviousIndex = previousEnd
               && operation.CurrentIndex = currentEnd
               && previousEnd = currentEnd then
                result[index] <- { operation with PrefixAnchor = true }
                previousEnd <- previousEnd + operation.PreviousCount
                currentEnd <- currentEnd + operation.CurrentCount
            else
                openPrefix <- false

    let emitUnaligned () =
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

    /// Starts confirming the rejected spans that lie inside the gap that just exhausted its budget. They form one
    /// contiguous run of the rejected list because both lists follow the same chain.
    let tryBeginRestore (meter: Meter) =
        restoreFirst <- -1
        restoreEnd <- -1
        for i = 0 to rejectedPrevious.Count - 1 do
            let n = rejectedLength[i]
            if rejectedPrevious[i] >= gapPreviousStart && rejectedPrevious[i] + n <= gapPreviousEnd
               && rejectedCurrent[i] >= gapCurrentStart && rejectedCurrent[i] + n <= gapCurrentEnd then
                if restoreFirst < 0 then restoreFirst <- i
                restoreEnd <- i + 1
        Meter.charge meter (rejectedPrevious.Count / 8)
        if restoreFirst < 0 then false
        else
            restoreCursor <- restoreFirst
            restoreAt <- gapIndex
            anchorLength <- 0
            anchorPending <- false
            stepper <- None
            phase <- AlignPhase.RestoreConfirm
            true

    let startGap (meter: Meter) =
        gapCache.Clear()
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
                        (if directActive then max 1 (stepsPerGap * 2) else stepsPerGap),
                        meter,
                        equalOracle
                    )
                )

    member _.Dispose() =
        releaseScratch ()
        releaseUnique ()

    /// The number of line keys that occur exactly once within the previous window, plus the number that occur
    /// exactly once within the current window. It is known once the alignment completes.
    member _.SelfUniqueLines = selfUniqueLines

    member _.ResolveRun(matched: int) =
        match phase with
        | AlignPhase.PrefixConfirm ->
            DiffOperations.appendRaw raw (DiffOperations.makeAnchored OperationKind.Equal 0 0 matched matched)
            prefixEnd <- matched
            prefixScanLength <- scanned
            prefixConfirmedLength <- matched
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
        | AlignPhase.PositionalConfirm ->
            let available = positionalRemaining
            let confirmed = min available matched
            if confirmed > 0 then
                positionalOperations.Add(
                    { DiffOperations.make OperationKind.Equal positionalPrevious positionalCurrent confirmed confirmed with Anchored = true }
                )
                positionalPrevious <- positionalPrevious + confirmed
                positionalCurrent <- positionalCurrent + confirmed
                positionalRemaining <- positionalRemaining - confirmed
            if confirmed < available then
                positionalMismatches <- positionalMismatches + 1
                positionalOperations.Add(DiffOperations.make OperationKind.Removed positionalPrevious -1 1 0)
                positionalOperations.Add(DiffOperations.make OperationKind.Added -1 positionalCurrent 0 1)
                positionalPrevious <- positionalPrevious + 1
                positionalCurrent <- positionalCurrent + 1
                positionalRemaining <- positionalRemaining - 1
            if positionalMismatches > lookaheadLines * 4 then
                positionalOperations.Clear()
                positionalConfirmed <- false
                phase <- AlignPhase.MiddleStart
            elif positionalRemaining = 0 then
                for operation in positionalOperations do DiffOperations.appendRaw raw operation
                positionalConfirmed <- true
                beginConvert ()
        | AlignPhase.AnchorConfirm
        | AlignPhase.RestoreConfirm ->
            if matched > 0 then
                if phase = AlignPhase.RestoreConfirm then
                    spanPrevious.Insert(restoreAt, anchorPrevious)
                    spanCurrent.Insert(restoreAt, anchorCurrent)
                    spanLength.Insert(restoreAt, matched)
                    restoreAt <- restoreAt + 1
                else
                    spanPrevious.Add anchorPrevious
                    spanCurrent.Add anchorCurrent
                    spanLength.Add matched
            if matched < anchorLength then
                anchorPrevious <- anchorPrevious + matched + 1
                anchorCurrent <- anchorCurrent + matched + 1
                anchorLength <- anchorLength - matched - 1
            else anchorLength <- 0
            anchorPending <- false
        | AlignPhase.DirectConfirm ->
            if matched >= directOperations[directConfirmIndex].PreviousCount then directConfirmIndex <- directConfirmIndex + 1
            else
                // Two different lines share a key.
                directOperations.Clear()
                directActive <- false
                phase <- AlignPhase.MiddleStart
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
            let limit = min previousCount currentCount - max prefixEnd (max prefixScanLength prefixConfirmedLength)
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
        | AlignPhase.MiddleStart ->
            if directPass && not directAttempted then
                startDirect ()
                AlignStep.Running
            elif not positionalAttempted && tryStartPositionalConfirm () then AlignStep.Running
            else
                startMiddle ()
                AlignStep.Running
        | AlignPhase.PositionalConfirm -> AlignStep.NeedRun(positionalPrevious, positionalCurrent, positionalRemaining)
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
            elif anchorLength > 0 then confirmAnchor ()
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
                lookupProbes <- 0
                let moved = movedAcrossRun ()
                Meter.charge meter (lookupProbes / 8)
                if moved then
                    rejectedPrevious.Add anchorPrevious
                    rejectedCurrent.Add anchorCurrent
                    rejectedLength.Add anchorLength
                    anchorLength <- 0
                    AlignStep.Running
                else confirmAnchor ()
        | AlignPhase.RestoreConfirm ->
            if anchorPending then AlignStep.NeedRun(anchorPrevious, anchorCurrent, anchorLength)
            elif anchorLength > 0 then confirmAnchor ()
            elif restoreCursor < restoreEnd then
                anchorPrevious <- rejectedPrevious[restoreCursor]
                anchorCurrent <- rejectedCurrent[restoreCursor]
                anchorLength <- rejectedLength[restoreCursor]
                restoreCursor <- restoreCursor + 1
                AlignStep.Running
            else
                let count = restoreEnd - restoreFirst
                rejectedPrevious.RemoveRange(restoreFirst, count)
                rejectedCurrent.RemoveRange(restoreFirst, count)
                rejectedLength.RemoveRange(restoreFirst, count)
                phase <- AlignPhase.Gaps
                // When no span confirmed, the gap would exhaust the same budget again.
                if restoreAt = gapIndex then emitUnaligned ()
                AlignStep.Running
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
                    let target = if directActive then directOperations else raw
                    for operation in operations do
                        match operation with
                        | MyersOperation.Equal(p, c) -> DiffOperations.appendRaw target (DiffOperations.make OperationKind.Equal p c 1 1)
                        | MyersOperation.Delete p -> DiffOperations.appendRaw target (DiffOperations.make OperationKind.Removed p -1 1 0)
                        | MyersOperation.Insert c -> DiffOperations.appendRaw target (DiffOperations.make OperationKind.Added -1 c 0 1)
                    Meter.charge meter (operations.Length / 64)
                    stepper <- None
                    if directActive then
                        directConfirmIndex <- 0
                        phase <- AlignPhase.DirectConfirm
                    else finishGap ()
                    AlignStep.Running
                | MyersStepResult.StepLimitExceeded ->
                    // The gap exceeded its step budget. Rejected spans inside it return as anchors. Without any, the aligner
                    // reports its lines as one unaligned region.
                    if directActive then
                        directActive <- false
                        stepper <- None
                        phase <- AlignPhase.MiddleStart
                    elif not (tryBeginRestore meter) then emitUnaligned ()
                    AlignStep.Running
            | None ->
                startGap meter
                AlignStep.Running
        | AlignPhase.DirectConfirm ->
            while directConfirmIndex < directOperations.Count && directOperations[directConfirmIndex].Kind <> OperationKind.Equal do
                directConfirmIndex <- directConfirmIndex + 1
            if directConfirmIndex < directOperations.Count then
                let operation = directOperations[directConfirmIndex]
                AlignStep.NeedRun(operation.PreviousIndex, operation.CurrentIndex, operation.PreviousCount)
            else
                for operation in directOperations do DiffOperations.appendRaw raw operation
                directActive <- false
                beginConvert ()
                AlignStep.Running
        | AlignPhase.CountUnique ->
            if uniqueReserved = 0L then reserveUnique ()
            let total = previousCount + currentCount
            let mutable count = 0
            while uniqueCursor < total && count < stepChunk do
                if uniqueCursor < previousCount then countLine previous uniqueCursor uniqueCursor uniquePreviousCount
                else countLine current (uniqueCursor - previousCount) uniqueCursor uniqueCurrentCount
                uniqueCursor <- uniqueCursor + 1
                count <- count + 1
            Meter.charge meter (count / 8)
            if uniqueCursor >= total then appendSuffix ()
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
                        { DiffOperations.make kind (operation.PreviousIndex + start) (operation.CurrentIndex + start) (index - start) (index - start) with Anchored = operation.Anchored; ReenterAnchor = operation.ReenterAnchor }
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
                markIndex <- 0
                markAfterChange <- false
                phase <- AlignPhase.MarkRuns
            AlignStep.Running
        | AlignPhase.MarkRuns ->
            // An equal run right after changed lines can settle the window when it holds a line that occurs once
            // in each window, or when it is long. A repeated line that matched by chance after a large insertion
            // is short and occurs more than once, so it stays unanchored.
            let mutable budget = 4096
            while budget > 0 && markIndex < result.Length do
                if isEqualKind result[markIndex].Kind then
                    let mutable runEnd = markIndex
                    let mutable lines = 0
                    while runEnd < result.Length && isEqualKind result[runEnd].Kind do
                        lines <- lines + result[runEnd].PreviousCount
                        runEnd <- runEnd + 1
                    if markAfterChange then
                        let mutable anchored = longRunAnchors && lines >= longRunLines
                        let mutable index = markIndex
                        while not anchored && index < runEnd do
                            let operation = result[index]
                            let mutable offset = 0
                            while not anchored && offset < operation.PreviousCount do
                                anchored <- uniqueInBoth (operation.PreviousIndex + offset)
                                offset <- offset + 1
                            index <- index + 1
                        if anchored then
                            for index = markIndex to runEnd - 1 do
                                result[index] <- { result[index] with Anchored = true }
                        budget <- budget - min lines longRunLines
                    budget <- budget - (runEnd - markIndex)
                    markIndex <- runEnd
                    markAfterChange <- false
                else
                    markAfterChange <- true
                    markIndex <- markIndex + 1
                    budget <- budget - 1
            Meter.charge meter ((4096 - budget) / 64)
            if markIndex >= result.Length then
                markLookaheadAnchors ()
                markAlignedPrefix ()
                releaseUnique ()
                phase <- AlignPhase.Finished
            AlignStep.Running
        | AlignPhase.Finished -> AlignStep.Complete result
