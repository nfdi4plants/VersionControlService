namespace VersionControlService.TextDiff

open System

/// A scanner position that a seek resumes from.
type internal CheckpointHit = {
    Offset: float
    Line: float
    InLine: float
    State: ScannerState
}

type internal CheckpointEntry = {
    Offset: float
    Line: float
    InLine: float
    mutable State: ScannerState option
    mutable Resident: bool
    mutable StoreOffset: float
}

/// Scanner checkpoints of both sides. Every side records its scanner state once per interval of source
/// bytes. A bounded number of records stays in memory and the rest is written to a temp store that is
/// created on the first spill. Records are ordered by offset, so a seek finds the nearest earlier one
/// with a binary search.
type internal Checkpoints(ledger: Ledger, intervalBytes: float, residentBytes: int, encodings: TextEncoding[], createStore: unit -> Async<ITempStore>) =
    let recordCost = 320
    let recordNumbers = 32

    let entries = [| ResizeArray<CheckpointEntry>(); ResizeArray<CheckpointEntry>() |]
    let nextDue = [| intervalBytes; intervalBytes |]
    let lastOffset = [| -1.0; -1.0 |]
    let pending = ResizeArray<CheckpointEntry>()
    let mutable residentUsed = 0
    let mutable store: ITempStore option = None

    let ensureStore () = async {
        match store with
        | Some value -> return value
        | None ->
            let! created = createStore ()
            store <- Some created
            return created
    }

    member _.Count(side: int) = entries[side].Count
    member _.HasPending = pending.Count > 0
    member _.ResidentBytes = residentUsed

    /// True when a side has passed its next interval boundary.
    member _.IsDue(side: int, offset: float) = offset >= nextDue[side]

    /// Records the scanner state when the scan position passes the next interval boundary. The line is the
    /// number of lines that ended before the scanner's next offset.
    member _.Observe(side: int, state: ScannerState, line: float) =
        if state.NextOffset >= nextDue[side] then
            let offset = state.NextOffset
            nextDue[side] <- (Math.Floor(offset / intervalBytes) + 1.0) * intervalBytes
            if offset > lastOffset[side] then
                lastOffset[side] <- offset
                let fits = residentUsed + recordCost <= residentBytes && ledger.TryReserve(AllocationCategory.ResidentCheckpoints, int64 recordCost)
                let entry = {
                    Offset = offset
                    Line = line
                    InLine = state.LineLengthUtf16
                    State = Some(Scanner.copyState state)
                    Resident = fits
                    StoreOffset = -1.0
                }
                if fits then residentUsed <- residentUsed + recordCost else pending.Add entry
                entries[side].Add entry

    /// Writes the records that did not fit in memory.
    member _.Flush() = async {
        if pending.Count > 0 then
            let! target = ensureStore ()
            let batch = pending.ToArray()
            pending.Clear()
            for entry in batch do
                let header = HeaderBuilder()
                ScannerStateCodec.write header entry.State.Value
                let numbers = Array.zeroCreate<float> recordNumbers
                let values = header.ToArray()
                Array.blit values 0 numbers 0 values.Length
                let bytes = Numbers.encodeAll numbers
                let! start = target.Append bytes 0 bytes.Length
                entry.StoreOffset <- float start
                entry.State <- None
    }

    /// Finds the last record of a side at or before a line number or a byte offset.
    member _.Find(side: int, byLine: bool, target: float) : Async<CheckpointHit option> = async {
        let list = entries[side]
        let key (entry: CheckpointEntry) = if byLine then entry.Line else entry.Offset
        let mutable low = 0
        let mutable high = list.Count
        while low < high do
            let middle = low + ((high - low) >>> 1)
            if key list[middle] <= target then low <- middle + 1 else high <- middle
        if low = 0 then return None
        else
            let entry = list[low - 1]
            match entry.State with
            | Some state ->
                return Some { Offset = entry.Offset; Line = entry.Line; InLine = entry.InLine; State = Scanner.copyState state }
            | None ->
                let! source = ensureStore ()
                let bytes = Array.zeroCreate<byte> (recordNumbers * Numbers.NumberBytes)
                let! actual = source.ReadAt (int64 entry.StoreOffset) bytes 0 bytes.Length
                if actual < bytes.Length then invalidOp "The checkpoint store is truncated."
                let reader = HeaderReader(Numbers.decodeAll bytes recordNumbers)
                let state = ScannerStateCodec.read encodings[side] reader
                return Some { Offset = entry.Offset; Line = entry.Line; InLine = entry.InLine; State = state }
    }

    member _.Dispose() = async {
        if residentUsed > 0 then
            ledger.Release(AllocationCategory.ResidentCheckpoints, int64 residentUsed)
            residentUsed <- 0
        entries[0].Clear()
        entries[1].Clear()
        pending.Clear()
        match store with
        | Some value ->
            store <- None
            do! value.Dispose()
        | None -> ()
    }
