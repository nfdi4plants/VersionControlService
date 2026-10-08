namespace VersionControlService.TextDiff

open System

/// The temp stores of one session, kept in memory. All stores of the group share one logical byte counter, so the
/// host can read how much the session has stored. The cap is a bug detector. A host keeps sessions well below it
/// with its write check, and a write that would grow the stored bytes past it throws an InvalidOperationException.
[<Sealed>]
type MemoryStoreGroup(capBytes: int64) =
    // Offsets, lengths and chunk indexes stay in int, which is cheap in Fable. The cap bounds every size.
    static let maxCapBytes = 1L <<< 30

    do
        if capBytes < 0L || capBytes > maxCapBytes then
            raise (ArgumentOutOfRangeException(nameof capBytes, "The cap must be between 0 and 1 GiB."))

    let mutable stored = 0

    /// The sum of the logical lengths of the live stores of this group.
    member _.StoredBytes: int64 = int64 stored

    /// The largest number of bytes the group accepts.
    member _.CapBytes: int64 = capBytes

    /// Creates an empty store. The name appears in the cap error message.
    member this.Create(name: string) : ITempStore = MemoryChunkStore(this, name) :> ITempStore

    member internal _.Grow(name: string, by: int64) =
        if by > capBytes - int64 stored then
            invalidOp (
                "The memory store group would exceed its cap of " + string capBytes + " bytes while growing '" + name + "'."
            )
        stored <- stored + int by

    member internal _.Release(by: int) = stored <- stored - by

/// A store that keeps its bytes in 64 KiB chunks. A chunk is allocated when a write first reaches it, and a chunk
/// that was never written reads as zeros.
and [<Sealed>] internal MemoryChunkStore(group: MemoryStoreGroup, name: string) =
    static let chunkSize = 64 * 1024

    let mutable chunks: byte[][] = Array.zeroCreate 4
    let mutable length = 0
    let mutable disposed = false

    let validateRange (buffer: byte[]) offset count =
        if disposed then invalidOp "The temporary store has been disposed."
        if isNull buffer then nullArg (nameof buffer)
        if offset < 0 || count < 0 || offset > buffer.Length - count then
            invalidArg (nameof offset) "The buffer range is invalid."

    let chunkFor (index: int) =
        if index >= chunks.Length then
            let grown: byte[][] = Array.zeroCreate (max (index + 1) (chunks.Length * 2))
            Array.blit chunks 0 grown 0 chunks.Length
            chunks <- grown
        if isNull chunks[index] then chunks[index] <- Array.zeroCreate chunkSize
        chunks[index]

    let write (position: int64) (buffer: byte[]) offset count =
        let ending = position + int64 count
        let growth = if ending > int64 length then ending - int64 length else 0L
        if growth > 0L then group.Grow(name, growth)
        let mutable at = int position
        let mutable source = offset
        let mutable remaining = count
        while remaining > 0 do
            let chunk = chunkFor (at / chunkSize)
            let inChunk = at % chunkSize
            let take = min remaining (chunkSize - inChunk)
            Array.Copy(buffer, source, chunk, inChunk, take)
            at <- at + take
            source <- source + take
            remaining <- remaining - take
        if growth > 0L then length <- int ending

    interface ITempStore with
        member _.Append buffer offset count = async {
            validateRange buffer offset count
            let start = length
            write (int64 start) buffer offset count
            return int64 start
        }

        member _.WriteAt position buffer offset count = async {
            validateRange buffer offset count
            if position < 0L then invalidArg (nameof position) "The write position cannot be negative."
            if position > Int64.MaxValue - int64 count then invalidArg (nameof position) "The write position is too large."
            write position buffer offset count
        }

        member _.ReadAt position buffer offset count = async {
            validateRange buffer offset count
            if position < 0L then invalidArg (nameof position) "The read position cannot be negative."
            if position >= int64 length then return 0
            else
                let copied = min count (length - int position)
                let mutable at = int position
                let mutable target = offset
                let mutable remaining = copied
                while remaining > 0 do
                    let index = at / chunkSize
                    let inChunk = at % chunkSize
                    let take = min remaining (chunkSize - inChunk)
                    if index < chunks.Length && not (isNull chunks[index]) then
                        Array.Copy(chunks[index], inChunk, buffer, target, take)
                    else
                        Array.fill buffer target take 0uy
                    at <- at + take
                    target <- target + take
                    remaining <- remaining - take
                return copied
        }

        member _.Length() = int64 length

        member _.Dispose() = async {
            if not disposed then
                group.Release length
                length <- 0
                chunks <- Array.empty
                disposed <- true
        }
