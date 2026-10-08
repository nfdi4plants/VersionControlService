/// The decisions of the worker about where a diff session keeps its data and when it refuses to write more.
/// Sizes are floats inside and become int64 only at the contract boundary.
module VersionControlService.Git.TextDiff.TextDiffStorage

open VersionControlService.Abstractions

/// The largest response envelope of a page, an Open or an expansion.
[<Literal>]
let PageEnvelopeLimit = 524288

/// The most that one committed Git blob may take in memory, whatever the budget is. It is the output limit of the
/// short command that reads the blob.
let MemoryBlobLimit = float TextDiffSupervisor.BlobOutputLimit

/// One page request can also write up to 24,000 bytes of pairs (24 bytes per range, at most 1,000 ranges) and,
/// while the journal index is below about 4 KiB, up to about 8 KiB of index growth beyond one doubling.
[<Literal>]
let PairsMargin = 32768.0

/// Room above the envelope that a fresh session needs on a drive before the worker trusts it with disk.
[<Literal>]
let private SafetyMargin = 65536.0

[<Literal>]
let private BytesPerMegabyte = 1048576.0

[<RequireQualifiedAccess>]
type StorageChoice =
    | Disk
    | Memory of budgetBytes: float

/// The largest committed blob that a memory session reads into memory. A larger blob blocks the Open.
let memoryBlobLimit (budgetBytes: float) : float =
    min MemoryBlobLimit (min (budgetBytes / 2.0) (budgetBytes - float PageEnvelopeLimit - SafetyMargin))

/// Chooses the storage of a session at its first Open. Unknown free space chooses disk, because the free space
/// check of a running session never refuses on unknown space either.
let chooseStorage (policy: DiffStoragePolicy) (freeBytes: float option) (committedBlobBytes: float option) : StorageChoice =
    match policy with
    | DiffStoragePolicy.MemoryOnly budget -> StorageChoice.Memory(float budget)
    | DiffStoragePolicy.PreferDisk(minimumFreeBytes, budget) ->
        match freeBytes with
        | None -> StorageChoice.Disk
        | Some free ->
            let needed = float minimumFreeBytes + float PageEnvelopeLimit + SafetyMargin

            let neededWithBlob =
                match committedBlobBytes with
                | Some blob -> needed + blob
                | None -> needed

            if free < neededWithBlob then StorageChoice.Memory(float budget) else StorageChoice.Disk

let private megabytes (bytes: float) : string = $"%.1f{bytes / BytesPerMegabyte}"

/// Answers a message when a memory session cannot take the next write request. A background read stops at three
/// quarters of the budget, which leaves the rest for reads that a user waits for.
let memoryRefusal
    (storedBytes: float)
    (indexLength: float)
    (blobBytes: float)
    (budgetBytes: float)
    (background: bool)
    : string option =
    let limit = if background then budgetBytes * 3.0 / 4.0 else budgetBytes
    let extra = indexLength + float PageEnvelopeLimit + PairsMargin

    if storedBytes + extra + blobBytes > limit then
        let held = megabytes (storedBytes + blobBytes)
        let next = megabytes extra

        if background then
            Some
                $"A background read stops at three quarters of the memory budget of {megabytes budgetBytes} MB. The diff session holds {held} MB and the next request needs up to {next} MB more."
        else
            Some
                $"The diff session reached its memory budget of {megabytes budgetBytes} MB. It holds {held} MB and the next request needs up to {next} MB more."
    else
        None

/// Answers a message when the temp drive cannot take the next write request and still keep the minimum free.
/// Unknown free space never refuses. The spool remainder is what the open sides of the session still receive.
let diskRefusal (freeBytes: float option) (indexLength: float) (spoolRemaining: float) (minimumFreeBytes: float) : string option =
    match freeBytes with
    | None -> None
    | Some free ->
        let after = free - indexLength - float PageEnvelopeLimit - PairsMargin - spoolRemaining

        if after < minimumFreeBytes then
            let next = megabytes (indexLength + float PageEnvelopeLimit + PairsMargin)

            let need =
                if spoolRemaining > 0.0 then
                    $"The diff session still receives {megabytes spoolRemaining} MB of blob data and the next request needs up to {next} MB"
                else
                    $"The next request needs up to {next} MB"

            Some
                $"The temp drive has {megabytes free} MB free. {need}, so the drive would fall below its minimum of {megabytes minimumFreeBytes} MB free."
        else
            None
