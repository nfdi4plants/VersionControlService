namespace VersionControlService.TextDiff

open System

/// Hash index over the sampled lines of one side. Each entry records the byte offset and line number of a
/// line together with the low word of its hash. Entries live in fixed-size chunks that are allocated on first
/// use, and buckets chain entries newest first through an index array.
type SampledIndex(capacity: int) =
    static let chunkBits = 12
    static let chunkSize = 1 <<< 12

    static let headBitsFor (capacity: int) =
        let mutable bits = 4
        while bits < 20 && (1 <<< bits) < capacity do
            bits <- bits + 1
        bits

    let headBits = headBitsFor capacity
    let mutable heads = Array.empty<int>
    let offsets = ResizeArray<float[]>()
    let lines = ResizeArray<float[]>()
    let hashes = ResizeArray<int[]>()
    let nexts = ResizeArray<int[]>()
    let mutable count = 0

    let bucketOf (hash: int) =
        int (uint32 (Native.imul hash -1640531535) >>> (32 - headBits))

    let chunkLength (chunk: int) = min chunkSize (capacity - chunk * chunkSize)

    let chunkCount (entries: int) = (entries + chunkSize - 1) >>> chunkBits

    let ensureChunk (chunk: int) =
        while offsets.Count <= chunk do
            let length = chunkLength offsets.Count
            offsets.Add(Array.zeroCreate<float> length)
            lines.Add(Array.zeroCreate<float> length)
            hashes.Add(Array.zeroCreate<int> length)
            nexts.Add(Array.zeroCreate<int> length)

    /// Ledger bytes that an index of this capacity may use, including its bucket heads.
    static member ReservedBytes(capacity: int) =
        int64 capacity * 24L + int64 (1 <<< headBitsFor capacity) * 4L

    member _.Count = count
    member _.Capacity = capacity
    member _.IsFull = count >= capacity
    member _.IsAllocated = heads.Length > 0

    /// Creates the bucket heads. Calling it again keeps the current content.
    member _.Allocate() =
        if heads.Length = 0 then heads <- Array.zeroCreate<int> (1 <<< headBits)

    member _.Reset() =
        count <- 0
        if heads.Length > 0 then Array.fill heads 0 heads.Length 0

    member _.Release() =
        heads <- Array.empty
        offsets.Clear()
        lines.Clear()
        hashes.Clear()
        nexts.Clear()
        count <- 0

    member _.Offset(id: int) = Native.readFloat offsets[id >>> chunkBits] (id &&& (chunkSize - 1))
    member _.Line(id: int) = Native.readFloat lines[id >>> chunkBits] (id &&& (chunkSize - 1))
    member _.Hash(id: int) = Native.readInt hashes[id >>> chunkBits] (id &&& (chunkSize - 1))

    /// The next older entry of the same bucket, or -1.
    member _.Older(id: int) = Native.readInt nexts[id >>> chunkBits] (id &&& (chunkSize - 1)) - 1

    /// The newest entry of the bucket that holds a hash, or -1.
    member _.Newest(hash: int) = Native.readInt heads (bucketOf hash) - 1

    member _.Add(offset: float, line: float, hash: int) =
        let id = count
        let chunk = id >>> chunkBits
        ensureChunk chunk
        let slot = id &&& (chunkSize - 1)
        Native.writeFloat offsets[chunk] slot offset
        Native.writeFloat lines[chunk] slot line
        Native.writeInt hashes[chunk] slot hash
        let bucket = bucketOf hash
        Native.writeInt nexts[chunk] slot (Native.readInt heads bucket)
        Native.writeInt heads bucket (id + 1)
        count <- count + 1

    /// Adds an entry unless the hash already has `chainLimit` entries or the bucket already holds `probeLimit`
    /// entries. A bucket never grows past the probe limit, so a walk of that length visits every entry of the
    /// bucket and the count of equal hashes is exact. It returns whether the entry was added.
    member this.TryAdd(offset: float, line: float, hash: int, probeLimit: int, chainLimit: int) =
        let mutable same = 0
        let mutable visited = 0
        let mutable id = this.Newest hash
        while id >= 0 && visited < probeLimit do
            if this.Hash id = hash then same <- same + 1
            visited <- visited + 1
            id <- this.Older id
        if visited >= probeLimit || same >= chainLimit then false
        else
            this.Add(offset, line, hash)
            true

    /// Prepares empty arrays for a restore of `entries` entries.
    member this.Prepare(entries: int) =
        this.Allocate()
        if entries > 0 then ensureChunk (chunkCount entries - 1)
        count <- entries

    /// The arrays of a spilled index in the order that Prepare filled them.
    member internal _.Slots(entries: int) : ArraySlot list =
        [
            yield ArraySlot.OfInts(heads, heads.Length)
            for chunk = 0 to chunkCount entries - 1 do
                let length = min chunkSize (entries - chunk * chunkSize)
                yield ArraySlot.OfFloats(offsets[chunk], length)
                yield ArraySlot.OfFloats(lines[chunk], length)
                yield ArraySlot.OfInts(hashes[chunk], length)
                yield ArraySlot.OfInts(nexts[chunk], length)
        ]
