namespace VersionControlService.TextDiff

open System

/// A scanner position that a seek resumes from.
type internal CheckpointHit = {
    Line: float
    State: ScannerState
}

/// Scanner checkpoints of both sides. Every side records its scanner state once per interval of source
/// bytes. Records are ordered by line, so a seek finds the nearest earlier one with a binary search.
type internal Checkpoints(intervalBytes: float, encodings: TextEncoding[]) =
    let entries = [| ResizeArray<CheckpointHit>(); ResizeArray<CheckpointHit>() |]
    let nextDue = [| intervalBytes; intervalBytes |]
    let lastOffset = [| -1.0; -1.0 |]

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
                entries[side].Add {
                    Line = line
                    State = Scanner.copyState state
                }

    member _.ObserveLine(side: int, offset: float, line: float) =
        if offset >= nextDue[side] then
            entries[side].Add {
                Line = line
                State = Scanner.create encodings[side] (int64 offset)
            }
            nextDue[side] <- (Math.Floor(offset / intervalBytes) + 1.0) * intervalBytes
        nextDue[side]

    /// Finds the last record of a side at or before a line number.
    member _.Find(side: int, line: float) : CheckpointHit option =
        let list = entries[side]
        let mutable low = 0
        let mutable high = list.Count
        while low < high do
            let middle = low + ((high - low) >>> 1)
            if list[middle].Line <= line then low <- middle + 1 else high <- middle
        if low = 0 then None
        else
            let entry = list[low - 1]
            Some { entry with State = Scanner.copyState entry.State }

    member _.Dispose() =
        entries[0].Clear()
        entries[1].Clear()
