namespace VersionControlService.TextDiff

open System

[<RequireQualifiedAccess>]
type AllocationCategory =
    | PreviousWindows
    | CurrentWindows
    | SampledIndexes
    | ChunkScratch
    | AlignmentScratch
    | ResponseData
    | RetainedBlobs

type Ledger() =
    let used = Array.zeroCreate<int64> 7
    let caps = [| 32L; 32L; 54L; 16L; 8L; 8L; 2L |] |> Array.map (fun value -> value * 1024L * 1024L)

    let index = function
        | AllocationCategory.PreviousWindows -> 0
        | AllocationCategory.CurrentWindows -> 1
        | AllocationCategory.SampledIndexes -> 2
        | AllocationCategory.ChunkScratch -> 3
        | AllocationCategory.AlignmentScratch -> 4
        | AllocationCategory.ResponseData -> 5
        | AllocationCategory.RetainedBlobs -> 6

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
