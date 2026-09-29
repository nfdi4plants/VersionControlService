namespace VersionControlService.TextDiff

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
type AllocationCategory =
    | PreviousWindows
    | CurrentWindows
    | SampledIndexes
    | ChunkScratch
    | AlignmentScratch
    | ResponseData
    | ResidentCheckpoints
    | RetainedBlobs

type Ledger() =
    let used = Array.zeroCreate<int64> 8
    let peakWindowLines = Array.zeroCreate<int> 2
    let mutable commonRunBytes = 0.0
    let caps = [| 32L; 32L; 54L; 16L; 8L; 8L; 1L; 2L |] |> Array.map (fun value -> value * 1024L * 1024L)

    let index = function
        | AllocationCategory.PreviousWindows -> 0
        | AllocationCategory.CurrentWindows -> 1
        | AllocationCategory.SampledIndexes -> 2
        | AllocationCategory.ChunkScratch -> 3
        | AllocationCategory.AlignmentScratch -> 4
        | AllocationCategory.ResponseData -> 5
        | AllocationCategory.ResidentCheckpoints -> 6
        | AllocationCategory.RetainedBlobs -> 7

    let windowIndex = function
        | AllocationCategory.PreviousWindows -> 0
        | AllocationCategory.CurrentWindows -> 1
        | category -> invalidArg (nameof category) "Window line counts require a window allocation category."

    member _.TryReserve(category: AllocationCategory, bytes: int64) =
        if bytes < 0L then invalidArg (nameof bytes) "The reservation cannot be negative."
        let slot = index category
        if bytes > caps[slot] - used[slot] then false
        else
            used[slot] <- used[slot] + bytes
            true

    member _.Release(category: AllocationCategory, bytes: int64) =
        if bytes < 0L then invalidArg (nameof bytes) "The release cannot be negative."
        let slot = index category
        if bytes > used[slot] then invalidArg (nameof bytes) "The release exceeds the reserved bytes."
        used[slot] <- used[slot] - bytes

    member _.Used(category: AllocationCategory) = used[index category]

    member _.RecordWindowLines(category: AllocationCategory, count: int) =
        if count < 0 then invalidArg (nameof count) "The window line count cannot be negative."
        let slot = windowIndex category
        peakWindowLines[slot] <- max peakWindowLines[slot] count

    member _.PeakWindowLines(category: AllocationCategory) = peakWindowLines[windowIndex category]

    /// Counts the bytes per side that the equal-byte phase consumed, so tests can tell it apart from window work.
    member _.RecordCommonRunBytes(count: float) =
        if count < 0.0 then invalidArg (nameof count) "The common run byte count cannot be negative."
        commonRunBytes <- commonRunBytes + count

    member _.CommonRunBytes = commonRunBytes

    member this.TryLease(category: AllocationCategory, bytes: int64) =
        if this.TryReserve(category, bytes) then Some(new AllocationLease(this, category, bytes) :> IDisposable)
        else None

and AllocationLease internal (ledger: Ledger, category: AllocationCategory, bytes: int64) =
    let mutable released = false

    interface IDisposable with
        member _.Dispose() =
            if not released then
                released <- true
                ledger.Release(category, bytes)

module Ledger =
    let tryReserve (category: AllocationCategory) (bytes: int64) (ledger: Ledger) = ledger.TryReserve(category, bytes)
    let release (category: AllocationCategory) (bytes: int64) (ledger: Ledger) = ledger.Release(category, bytes)
    let used (category: AllocationCategory) (ledger: Ledger) = ledger.Used(category)
    let lease (category: AllocationCategory) (bytes: int64) (ledger: Ledger) = ledger.TryLease(category, bytes)

type ChunkBufferLease internal (buffer: byte[], release: byte[] -> unit) =
    let mutable disposed = false
    member _.Buffer = buffer
    interface IDisposable with
        member _.Dispose() =
            if not disposed then
                disposed <- true
                release buffer

module private ChunkBuffers =
    [<Literal>]
    let Size = 8 * 1024 * 1024

type ChunkBufferPool(ledger: Ledger) =
    let gate = obj()
    let available = Stack<byte[]>()
    let mutable disposed = false

    member _.Rent() =
        lock gate (fun () ->
            if disposed then invalidOp "The chunk buffer pool has been disposed."
            let buffer =
                if available.Count > 0 then available.Pop()
                elif ledger.TryReserve(AllocationCategory.ChunkScratch, int64 ChunkBuffers.Size) then
                    try Array.zeroCreate<byte> ChunkBuffers.Size
                    with error ->
                        ledger.Release(AllocationCategory.ChunkScratch, int64 ChunkBuffers.Size)
                        raise error
                else null
            if isNull buffer then None
            else
                let release returned =
                    lock gate (fun () ->
                        if disposed then ledger.Release(AllocationCategory.ChunkScratch, int64 ChunkBuffers.Size)
                        else available.Push(returned))
                Some(new ChunkBufferLease(buffer, release)))

    member _.Dispose() =
        lock gate (fun () ->
            if not disposed then
                disposed <- true
                while available.Count > 0 do
                    available.Pop() |> ignore
                    ledger.Release(AllocationCategory.ChunkScratch, int64 ChunkBuffers.Size))

    interface IDisposable with
        member this.Dispose() = this.Dispose()
