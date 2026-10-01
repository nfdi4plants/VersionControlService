namespace VersionControlService.TextDiff

open System.Collections.Generic
open VersionControlService.Abstractions

/// Position and size of one source line. Offsets and counts are floats so the transpiled code avoids 64-bit arithmetic.
[<Struct>]
type internal LineRef = {
    Number: float
    Start: float
    Finish: float
    Length: float
    Ending: int
}

/// One aligned row. Context, Replaced and EndingChanged rows carry both lines, Removed rows only the previous
/// line and Added rows only the current line.
[<Struct>]
type internal RowRef = {
    Kind: DiffRowKind
    Previous: LineRef
    Current: LineRef
}

[<RequireQualifiedAccess>]
type internal ItemKind =
    | Gap
    | Rows
    | Lane

/// A queued piece of page content. Gap items describe hidden equal lines, Rows items hold aligned rows of one
/// hunk fragment and Lane items hold the lines of one side of an unaligned region.
type internal QueueItem(kind: ItemKind) =
    member _.Kind = kind
    member val Sequence = 0 with get, set
    member val PreviousStart = 0.0 with get, set
    member val CurrentStart = 0.0 with get, set
    member val Count = 0.0 with get, set
    member val StartsHunk = false with get, set
    member val EndsHunk = false with get, set
    member val Rows = Array.empty<RowRef> with get, set
    member val PreviousLines = Array.empty<LineRef> with get, set
    member val CurrentLines = Array.empty<LineRef> with get, set
    /// Rows or lane lines already emitted by earlier pages.
    member val Consumed = 0 with get, set

    member this.Total =
        match kind with
        | ItemKind.Gap -> 0
        | ItemKind.Rows -> this.Rows.Length
        | ItemKind.Lane -> this.PreviousLines.Length + this.CurrentLines.Length

    member this.Remaining = this.Total - this.Consumed

module internal LineRefs =
    let inline ofTable (table: LineTable) (index: int) = {
        Number = float table.LineBase + float index
        Start = table.Start index
        Finish = table.Finish index
        Length = table.Length index
        Ending = int (table.EndingCode index)
    }

    let hasPrevious = function
        | DiffRowKind.Added -> false
        | _ -> true

    let hasCurrent = function
        | DiffRowKind.Removed -> false
        | _ -> true

/// The compact per-side line arrays that a window of rows reads. A row takes a slot on a side only when it reads
/// a line from that side. Context and ending-only rows read their line from the previous side and build the
/// current line from it, so they take no slot on the current side.
module internal RowLines =
    let count (rows: RowRef[]) (first: int) (rowCount: int) (previousSide: bool) =
        let mutable total = 0
        for index = first to first + rowCount - 1 do
            match rows[index].Kind with
            | DiffRowKind.Replaced -> total <- total + 1
            | DiffRowKind.Added -> if not previousSide then total <- total + 1
            | _ -> if previousSide then total <- total + 1
        total

    /// The slot of a row in the arrays that collect builds for one side.
    let slotBefore (rows: RowRef[]) (first: int) (rowIndex: int) (previousSide: bool) =
        count rows first (rowIndex - first) previousSide

    let collect (rows: RowRef[]) (first: int) (rowCount: int) =
        let previousRefs = Array.zeroCreate<LineRef> (count rows first rowCount true)
        let currentRefs = Array.zeroCreate<LineRef> (count rows first rowCount false)
        let mutable previousIndex = 0
        let mutable currentIndex = 0
        for index = first to first + rowCount - 1 do
            let row = rows[index]
            match row.Kind with
            | DiffRowKind.Added ->
                currentRefs[currentIndex] <- row.Current
                currentIndex <- currentIndex + 1
            | DiffRowKind.Replaced ->
                previousRefs[previousIndex] <- row.Previous
                currentRefs[currentIndex] <- row.Current
                previousIndex <- previousIndex + 1
                currentIndex <- currentIndex + 1
            | _ ->
                previousRefs[previousIndex] <- row.Previous
                previousIndex <- previousIndex + 1
        previousRefs, currentRefs

/// Groups a stream of aligned lines into gaps, hunks and unaligned regions. It stores line positions and never
/// line text. Hunks carry ContextLines lines of context on each side, merge when the equal lines between them
/// number at most twice that, and stay open until the equal run exceeds it. Rows accumulate in fragments of at
/// most pageMaxRows rows.
type internal HunkBuilder(contextLines: int, pageMaxRows: int) =
    let context = max 0 contextLines
    let capacity = 2 * context + 1
    let ringPrevious = Array.zeroCreate<LineRef> capacity
    let ringCurrent = Array.zeroCreate<LineRef> capacity
    let mutable ringHead = 0
    let mutable ringCount = 0
    let mutable nextPrevious = 0.0
    let mutable nextCurrent = 0.0
    let mutable shownPrevious = 0.0
    let mutable shownCurrent = 0.0
    let mutable isOpen = false
    let mutable hunkSequence = 0
    let mutable gapSequence = 0
    let mutable rows = ResizeArray<RowRef>()
    let mutable fragmentBasePrevious = 0.0
    let mutable fragmentBaseCurrent = 0.0
    let mutable fragmentStarts = true
    let mutable openEndPrevious = 0.0
    let mutable openEndCurrent = 0.0
    let queue = Queue<QueueItem>()
    let mutable queuedRows = 0
    let mutable queuedFragments = 0
    let mutable completedHunks = 0
    let mutable finished = false
    let mutable regionActive = false
    let mutable regionSequence = 0
    let mutable regionStarts = true
    let mutable regionPending: QueueItem option = None

    let ringIndex offset = (ringHead + offset) % capacity

    let ringDropFront count =
        if count >= ringCount then
            ringCount <- 0
            ringHead <- 0
        elif count > 0 then
            ringHead <- ringIndex count
            ringCount <- ringCount - count

    let ringPush (previous: LineRef) (current: LineRef) keep =
        if keep > 0 then
            if ringCount >= keep then ringDropFront (ringCount - keep + 1)
            let slot = ringIndex ringCount
            ringPrevious[slot] <- previous
            ringCurrent[slot] <- current
            ringCount <- ringCount + 1

    let enqueue (item: QueueItem) =
        queue.Enqueue item
        match item.Kind with
        | ItemKind.Gap -> ()
        | ItemKind.Rows ->
            queuedRows <- queuedRows + item.Rows.Length
            queuedFragments <- queuedFragments + 1
        | ItemKind.Lane ->
            queuedRows <- queuedRows + item.PreviousLines.Length + item.CurrentLines.Length
            queuedFragments <- queuedFragments + 1
        if item.EndsHunk then completedHunks <- completedHunks + 1

    let queueGap previousStart currentStart count =
        if count > 0.0 then
            let item = QueueItem(ItemKind.Gap)
            gapSequence <- gapSequence + 1
            item.Sequence <- gapSequence
            item.PreviousStart <- previousStart
            item.CurrentStart <- currentStart
            item.Count <- count
            enqueue item

    let flushFragment endsHunk =
        if rows.Count > 0 || (endsHunk && not fragmentStarts) then
            let item = QueueItem(ItemKind.Rows)
            item.Sequence <- hunkSequence
            item.StartsHunk <- fragmentStarts
            item.EndsHunk <- endsHunk
            item.PreviousStart <- fragmentBasePrevious
            item.CurrentStart <- fragmentBaseCurrent
            item.Rows <- rows.ToArray()
            enqueue item
            rows <- ResizeArray<RowRef>()
            fragmentStarts <- false
            fragmentBasePrevious <- openEndPrevious
            fragmentBaseCurrent <- openEndCurrent

    let addRow (row: RowRef) =
        if rows.Count >= pageMaxRows then flushFragment false
        rows.Add row
        if LineRefs.hasPrevious row.Kind then openEndPrevious <- row.Previous.Number + 1.0
        if LineRefs.hasCurrent row.Kind then openEndCurrent <- row.Current.Number + 1.0

    let addContext (previous: LineRef) (current: LineRef) =
        addRow { Kind = DiffRowKind.Context; Previous = previous; Current = current }

    let closeHunk trailing =
        for offset = 0 to trailing - 1 do
            let slot = ringIndex offset
            addContext ringPrevious[slot] ringCurrent[slot]
        ringDropFront trailing
        flushFragment true
        isOpen <- false
        shownPrevious <- openEndPrevious
        shownCurrent <- openEndCurrent

    let beginHunk () =
        if not isOpen then
            let hidden = nextPrevious - shownPrevious - float ringCount
            queueGap shownPrevious shownCurrent hidden
            isOpen <- true
            hunkSequence <- hunkSequence + 1
            fragmentStarts <- true
            rows <- ResizeArray<RowRef>()
            if ringCount > 0 then
                fragmentBasePrevious <- ringPrevious[ringHead].Number
                fragmentBaseCurrent <- ringCurrent[ringHead].Number
            else
                fragmentBasePrevious <- nextPrevious
                fragmentBaseCurrent <- nextCurrent
            openEndPrevious <- fragmentBasePrevious
            openEndCurrent <- fragmentBaseCurrent
            for offset = 0 to ringCount - 1 do
                let slot = ringIndex offset
                addContext ringPrevious[slot] ringCurrent[slot]
            ringDropFront ringCount

    let pendingAsContext () =
        for offset = 0 to ringCount - 1 do
            let slot = ringIndex offset
            addContext ringPrevious[slot] ringCurrent[slot]
        ringDropFront ringCount

    let equalLine (previous: LineRef) (current: LineRef) =
        if isOpen then
            if ringCount < 2 * context then ringPush previous current (2 * context)
            else
                closeHunk context
                ringPush previous current context
        else ringPush previous current context
        nextPrevious <- nextPrevious + 1.0
        nextCurrent <- nextCurrent + 1.0

    let changeRow kind (previous: LineRef) (current: LineRef) =
        beginHunk ()
        pendingAsContext ()
        addRow { Kind = kind; Previous = previous; Current = current }
        if LineRefs.hasPrevious kind then nextPrevious <- nextPrevious + 1.0
        if LineRefs.hasCurrent kind then nextCurrent <- nextCurrent + 1.0

    let closeOpenForRegion () =
        if isOpen then closeHunk (min context ringCount)

    let emptyLine = { Number = 0.0; Start = 0.0; Finish = 0.0; Length = 0.0; Ending = 0 }

    member _.NextPrevious = nextPrevious
    member _.NextCurrent = nextCurrent
    member _.IsIdle = not isOpen && not regionActive
    member _.Finished = finished
    member _.QueuedRows = queuedRows
    member _.QueuedFragments = queuedFragments
    member _.CompletedHunks = completedHunks
    member _.QueueCount = queue.Count
    member _.HasOpenRows = isOpen && rows.Count > 0

    member _.RingCount = ringCount

    /// Queue items in page order. The page builder reads them without removing them.
    member _.ItemAt(index: int) = Seq.item index queue

    member _.Items = queue

    /// Removes the first fullCount items and records that the following item has emitted consumed lines.
    member _.CommitPage(fullCount: int, partialConsumed: int, rowsRemoved: int, fragmentsRemoved: int) =
        for _ in 1..fullCount do
            let item = queue.Dequeue()
            if item.EndsHunk then completedHunks <- completedHunks - 1
        if partialConsumed > 0 then queue.Peek().Consumed <- queue.Peek().Consumed + partialConsumed
        queuedRows <- queuedRows - rowsRemoved
        queuedFragments <- queuedFragments - fragmentsRemoved

    /// Moves rows collected for the open hunk into the queue so a page can include them. The hunk stays open.
    member _.FlushOpen() = if isOpen then flushFragment false

    member _.EqualSkip(count: float) =
        if count > 0.0 then
            ringCount <- 0
            ringHead <- 0
            nextPrevious <- nextPrevious + count
            nextCurrent <- nextCurrent + count

    /// Moves the line counters back over equal lines that the caller re-reads from source.
    member _.RewindEqual(count: int) =
        nextPrevious <- nextPrevious - float count
        nextCurrent <- nextCurrent - float count

    member _.Equal(previous: LineTable, previousIndex: int, current: LineTable, currentIndex: int, count: int) =
        let mutable index = 0
        while index < count do
            if isOpen then
                equalLine (LineRefs.ofTable previous (previousIndex + index)) (LineRefs.ofTable current (currentIndex + index))
                index <- index + 1
            else
                let remaining = count - index
                let skip = max 0 (remaining - context)
                if skip > 0 then
                    ringCount <- 0
                    ringHead <- 0
                    nextPrevious <- nextPrevious + float skip
                    nextCurrent <- nextCurrent + float skip
                    index <- index + skip
                let stop = count
                while index < stop && not isOpen do
                    equalLine (LineRefs.ofTable previous (previousIndex + index)) (LineRefs.ofTable current (currentIndex + index))
                    index <- index + 1

    member _.Paired(kind: DiffRowKind, previous: LineTable, previousIndex: int, current: LineTable, currentIndex: int, count: int) =
        for index = 0 to count - 1 do
            changeRow kind (LineRefs.ofTable previous (previousIndex + index)) (LineRefs.ofTable current (currentIndex + index))

    member _.Removed(previous: LineTable, previousIndex: int, count: int) =
        for index = 0 to count - 1 do
            changeRow DiffRowKind.Removed (LineRefs.ofTable previous (previousIndex + index)) emptyLine

    member _.Added(current: LineTable, currentIndex: int, count: int) =
        for index = 0 to count - 1 do
            changeRow DiffRowKind.Added emptyLine (LineRefs.ofTable current (currentIndex + index))

    member _.BeginRegion() =
        closeOpenForRegion ()
        queueGap shownPrevious shownCurrent (nextPrevious - shownPrevious)
        ringCount <- 0
        ringHead <- 0
        regionActive <- true
        hunkSequence <- hunkSequence + 1
        regionSequence <- hunkSequence
        regionStarts <- true
        regionPending <- None

    /// Adds lines of one side to the open region. The previous side is fed first.
    member _.RegionLines(previousSide: bool, table: LineTable, index: int, count: int) =
        if count > 0 then
            match regionPending with
            | Some pending ->
                enqueue pending
                regionPending <- None
            | None -> ()
            let item = QueueItem(ItemKind.Lane)
            item.Sequence <- regionSequence
            item.StartsHunk <- regionStarts
            regionStarts <- false
            let lines = Array.init count (fun offset -> LineRefs.ofTable table (index + offset))
            if previousSide then
                item.PreviousLines <- lines
                item.PreviousStart <- lines[0].Number
                item.CurrentStart <- nextCurrent
                nextPrevious <- nextPrevious + float count
            else
                item.CurrentLines <- lines
                item.PreviousStart <- nextPrevious
                item.CurrentStart <- lines[0].Number
                nextCurrent <- nextCurrent + float count
            regionPending <- Some item

    member _.EndRegion() =
        match regionPending with
        | Some pending ->
            pending.EndsHunk <- true
            enqueue pending
            regionPending <- None
        | None -> ()
        regionActive <- false
        shownPrevious <- nextPrevious
        shownCurrent <- nextCurrent

    member _.Finish() =
        if not finished then
            if isOpen then closeHunk (min context ringCount)
            queueGap shownPrevious shownCurrent (nextPrevious - shownPrevious)
            shownPrevious <- nextPrevious
            shownCurrent <- nextCurrent
            ringCount <- 0
            finished <- true
