namespace VersionControlService.TextDiff

open System
open System.Collections.Generic
open System.Text
open VersionControlService.Abstractions

/// Collects a record in a byte[] that is a typed array on both runtimes. A ResizeArray would convert to a
/// plain JavaScript array, which file writes reject.
type private JournalWriter() =
    let mutable bytes = Array.zeroCreate<byte> 256
    let mutable count = 0

    member _.Position = count

    member _.WriteByte(value: int) =
        if count = bytes.Length then
            let grown = Array.zeroCreate<byte> (bytes.Length * 2)
            for index = 0 to count - 1 do
                Native.writeByte grown index (Native.readByte bytes index)
            bytes <- grown
        Native.writeByte bytes count value
        count <- count + 1

    member this.WriteBool(value: bool) = this.WriteByte(if value then 1 else 0)

    member this.WriteInt32(value: int) =
        for index = 0 to 3 do
            this.WriteByte((value >>> (index * 8)) &&& 0xFF)

    member this.WriteInt64(value: int64) =
        let bits = uint64 value
        for index = 0 to 7 do
            this.WriteByte(int ((bits >>> (index * 8)) &&& 0xFFUL))

    member this.WriteString(value: string) =
        let encoded = Encoding.UTF8.GetBytes(value)
        this.WriteInt32 encoded.Length
        for item in encoded do this.WriteByte(int item)

    member this.WriteOptionString(value: string option) =
        match value with
        | Some text -> this.WriteBool true; this.WriteString text
        | None -> this.WriteBool false

    member _.ToArray() =
        let result = Array.zeroCreate<byte> count
        for index = 0 to count - 1 do
            Native.writeByte result index (Native.readByte bytes index)
        result

type private JournalReader(bytes: byte[]) =
    let mutable position = 0

    let require count =
        if count < 0 || position > bytes.Length - count then invalidOp "The journal record is truncated."

    member _.ReadByte() =
        require 1
        let value = Native.readByte bytes position
        position <- position + 1
        value

    member this.ReadBool() = this.ReadByte() <> 0

    member this.ReadInt32() =
        let mutable value = 0
        for index = 0 to 3 do value <- value ||| (this.ReadByte() <<< (index * 8))
        value

    member this.ReadInt64() =
        let mutable value = 0UL
        for index = 0 to 7 do value <- value ||| (uint64 (this.ReadByte()) <<< (index * 8))
        int64 value

    member this.ReadString() =
        let count = this.ReadInt32()
        if count < 0 then invalidOp "The journal string length is invalid."
        require count
        let value = Encoding.UTF8.GetString(bytes, position, count)
        position <- position + count
        value

    member this.ReadOptionString() =
        if this.ReadBool() then Some(this.ReadString()) else None

    member _.AtEnd = position = bytes.Length

module private JournalCodec =
    let private writeRange (writer: JournalWriter) (value: LineRange) =
        writer.WriteInt64 value.Start
        writer.WriteInt64 value.Count

    let private readRange (reader: JournalReader) = {
        Start = reader.ReadInt64()
        Count = reader.ReadInt64()
    }

    let private writeProgress (writer: JournalWriter) (value: ScanProgress) =
        writer.WriteInt64 value.ValidatedBytes
        writer.WriteInt64 value.TotalBytes
        writer.WriteBool value.ScanComplete

    let private readProgress (reader: JournalReader) = {
        ValidatedBytes = reader.ReadInt64()
        TotalBytes = reader.ReadInt64()
        ScanComplete = reader.ReadBool()
    }

    let private writeEnding (writer: JournalWriter) = function
        | LineEnding.NoEnding -> writer.WriteByte 0
        | LineEnding.LF -> writer.WriteByte 1
        | LineEnding.CRLF -> writer.WriteByte 2
        | LineEnding.CR -> writer.WriteByte 3

    let private readEnding (reader: JournalReader) =
        match reader.ReadByte() with
        | 1 -> LineEnding.LF
        | 2 -> LineEnding.CRLF
        | 3 -> LineEnding.CR
        | _ -> LineEnding.NoEnding

    let private writeLine (writer: JournalWriter) (value: DiffLine) =
        writer.WriteInt64 value.Number
        writeEnding writer value.Ending
        writer.WriteInt64 value.Slice.OffsetUtf16
        writer.WriteBool value.Slice.TotalUtf16.IsSome
        value.Slice.TotalUtf16 |> Option.iter writer.WriteInt64
        writer.WriteString value.Slice.Text
        writer.WriteInt32 value.Slice.Highlights.Length
        for highlight in value.Slice.Highlights do
            writer.WriteInt32 highlight.Start
            writer.WriteInt32 highlight.Length
            writer.WriteByte(match highlight.Kind with | HighlightKind.UnchangedText -> 0 | HighlightKind.ChangedText -> 1)

    let private readLine (reader: JournalReader) =
        let number = reader.ReadInt64()
        let ending = readEnding reader
        let offset = reader.ReadInt64()
        let total = if reader.ReadBool() then Some(reader.ReadInt64()) else None
        let text = reader.ReadString()
        let count = reader.ReadInt32()
        if count < 0 || count > 100_000 then invalidOp "The journal highlight count is invalid."
        let highlights = Array.zeroCreate<Highlight> count
        for index = 0 to count - 1 do
            let start = reader.ReadInt32()
            let length = reader.ReadInt32()
            let kind = if reader.ReadByte() = 0 then HighlightKind.UnchangedText else HighlightKind.ChangedText
            highlights[index] <- { Start = start; Length = length; Kind = kind }
        {
            Number = number
            Ending = ending
            Slice = { OffsetUtf16 = offset; TotalUtf16 = total; Text = text; Highlights = highlights }
        }

    let private writeOptionalLine (writer: JournalWriter) = function
        | Some value -> writer.WriteBool true; writeLine writer value
        | None -> writer.WriteBool false

    let private readOptionalLine (reader: JournalReader) =
        if reader.ReadBool() then Some(readLine reader) else None

    let private writeRow (writer: JournalWriter) (value: DiffRow) =
        writer.WriteString value.Id
        writer.WriteByte(
            match value.Kind with
            | DiffRowKind.Context -> 0
            | DiffRowKind.Added -> 1
            | DiffRowKind.Removed -> 2
            | DiffRowKind.Replaced -> 3
            | DiffRowKind.EndingChanged -> 4)
        writeOptionalLine writer value.Previous
        writeOptionalLine writer value.Current

    let private readRow (reader: JournalReader) =
        let id = reader.ReadString()
        let kind =
            match reader.ReadByte() with
            | 1 -> DiffRowKind.Added
            | 2 -> DiffRowKind.Removed
            | 3 -> DiffRowKind.Replaced
            | 4 -> DiffRowKind.EndingChanged
            | _ -> DiffRowKind.Context
        { Id = id; Kind = kind; Previous = readOptionalLine reader; Current = readOptionalLine reader }

    let private writeRows (writer: JournalWriter) (values: DiffRow[]) =
        writer.WriteInt32 values.Length
        for value in values do writeRow writer value

    let private readRows (reader: JournalReader) =
        let count = reader.ReadInt32()
        if count < 0 || count > 1_000_000 then invalidOp "The journal row count is invalid."
        Array.init count (fun _ -> readRow reader)

    let private writePart (writer: JournalWriter) = function
        | DiffPart.Hunk fragment ->
            writer.WriteByte 0
            writer.WriteString fragment.HunkId
            writeRange writer fragment.PreviousRange
            writeRange writer fragment.CurrentRange
            writer.WriteBool fragment.StartsHunk
            writer.WriteBool fragment.EndsHunk
            match fragment.Body with
            | HunkBody.AlignedRows rows -> writer.WriteByte 0; writeRows writer rows
            | HunkBody.UnalignedSides(previous, current) ->
                writer.WriteByte 1
                writer.WriteInt32 previous.Length
                for line in previous do writeLine writer line
                writer.WriteInt32 current.Length
                for line in current do writeLine writer line
        | DiffPart.HiddenEqual gap ->
            writer.WriteByte 1
            writer.WriteString gap.GapId
            writeRange writer gap.PreviousRange
            writeRange writer gap.CurrentRange
        | DiffPart.ExpandedContext(gapId, rows) ->
            writer.WriteByte 2
            writer.WriteString gapId
            writeRows writer rows

    let private readPart (reader: JournalReader) =
        match reader.ReadByte() with
        | 1 ->
            let gap = {
                GapId = reader.ReadString()
                PreviousRange = readRange reader
                CurrentRange = readRange reader
            }
            DiffPart.HiddenEqual gap
        | 2 -> DiffPart.ExpandedContext(reader.ReadString(), readRows reader)
        | _ ->
            let hunkId = reader.ReadString()
            let previousRange = readRange reader
            let currentRange = readRange reader
            let starts = reader.ReadBool()
            let ends = reader.ReadBool()
            let body =
                if reader.ReadByte() = 0 then HunkBody.AlignedRows(readRows reader)
                else
                    let previousCount = reader.ReadInt32()
                    if previousCount < 0 || previousCount > 1_000_000 then invalidOp "The journal lane count is invalid."
                    let previous = Array.init previousCount (fun _ -> readLine reader)
                    let currentCount = reader.ReadInt32()
                    if currentCount < 0 || currentCount > 1_000_000 then invalidOp "The journal lane count is invalid."
                    let current = Array.init currentCount (fun _ -> readLine reader)
                    HunkBody.UnalignedSides(previous, current)
            DiffPart.Hunk {
                HunkId = hunkId
                PreviousRange = previousRange
                CurrentRange = currentRange
                StartsHunk = starts
                EndsHunk = ends
                Body = body
            }

    let private writePending (writer: JournalWriter) (value: PendingPreview option) =
        match value with
        | None -> writer.WriteBool false
        | Some preview ->
            writer.WriteBool true
            let writeSide = function
                | PendingSide.NoActiveLine -> writer.WriteByte 0
                | PendingSide.Exhausted count -> writer.WriteByte 1; writer.WriteInt64 count
                | PendingSide.Snippet snippet ->
                    writer.WriteByte 2
                    writer.WriteInt64 snippet.Line
                    writer.WriteInt64 snippet.OffsetUtf16
                    writer.WriteString snippet.Text
                    writer.WriteByte(match snippet.End with | SnippetEnd.Truncated -> 0 | SnippetEnd.MoreTextPending -> 1 | SnippetEnd.LineEnd -> 2 | SnippetEnd.EndOfFile -> 3)
            writeSide preview.Previous
            writeSide preview.Current
            writer.WriteBool preview.Mismatch.IsSome
            preview.Mismatch |> Option.iter (fun mismatch -> writer.WriteInt64 mismatch.PreviousOffsetUtf16; writer.WriteInt64 mismatch.CurrentOffsetUtf16)

    let private readPending (reader: JournalReader) =
        if not (reader.ReadBool()) then None
        else
            let readSide () =
                match reader.ReadByte() with
                | 1 -> PendingSide.Exhausted(reader.ReadInt64())
                | 2 ->
                    let line = reader.ReadInt64()
                    let offset = reader.ReadInt64()
                    let text = reader.ReadString()
                    let ending =
                        match reader.ReadByte() with
                        | 1 -> SnippetEnd.MoreTextPending
                        | 2 -> SnippetEnd.LineEnd
                        | 3 -> SnippetEnd.EndOfFile
                        | _ -> SnippetEnd.Truncated
                    PendingSide.Snippet { Line = line; OffsetUtf16 = offset; Text = text; End = ending }
                | _ -> PendingSide.NoActiveLine
            let previous = readSide ()
            let current = readSide ()
            let mismatch =
                if reader.ReadBool() then Some { PreviousOffsetUtf16 = reader.ReadInt64(); CurrentOffsetUtf16 = reader.ReadInt64() }
                else None
            Some { Previous = previous; Current = current; Mismatch = mismatch }

    let encode (value: Resumable<DiffPage>) =
        let writer = JournalWriter()
        writer.WriteByte 1
        match value with
        | Resumable.Ready page ->
            writer.WriteByte 0
            writer.WriteString page.PageId
            writer.WriteOptionString page.NextCursor
            writer.WriteInt32 page.Parts.Length
            for part in page.Parts do writePart writer part
            writeProgress writer page.Progress
            writer.WriteBool page.OutputComplete
            writePending writer page.Pending
        | Resumable.Scanning(progress, continuation, pending) ->
            writer.WriteByte 1
            writeProgress writer progress
            writer.WriteString continuation
            writePending writer pending
        writer.ToArray()

    let decode (bytes: byte[]) =
        let reader = JournalReader bytes
        if reader.ReadByte() <> 1 then invalidOp "The journal record version is unsupported."
        let value =
            match reader.ReadByte() with
            | 1 -> Resumable.Scanning(readProgress reader, reader.ReadString(), readPending reader)
            | _ ->
                let pageId = reader.ReadString()
                let cursor = reader.ReadOptionString()
                let count = reader.ReadInt32()
                if count < 0 || count > 100_000 then invalidOp "The journal part count is invalid."
                let parts = Array.init count (fun _ -> readPart reader)
                let progress = readProgress reader
                let outputComplete = reader.ReadBool()
                let pending = readPending reader
                Resumable.Ready {
                    PageId = pageId
                    NextCursor = cursor
                    Parts = parts
                    Progress = progress
                    OutputComplete = outputComplete
                    Pending = pending
                }
        if not reader.AtEnd then invalidOp "The journal record has trailing data."
        value

/// Frames an encoded value with its checksum. The byte loops live outside the async members because
/// an async loop pays one scheduling step per iteration on the JavaScript runtime.
module internal JournalRecord =
    let encode (value: Resumable<DiffPage>) : byte[] =
        let payload = JournalCodec.encode value
        let hash = Hash.create ()
        for byteIndex = 0 to payload.Length - 1 do Hash.addByte hash payload[byteIndex]
        let writer = JournalWriter()
        writer.WriteByte 0x54
        writer.WriteInt32 payload.Length
        for byteValue in payload do writer.WriteByte(int byteValue)
        writer.WriteInt32(int hash.Lo)
        writer.WriteInt32(int hash.Hi)
        writer.ToArray()

    let decode (record: byte[]) : Resumable<DiffPage> =
        let reader = JournalReader record
        if reader.ReadByte() <> 0x54 then invalidOp "The journal record header is invalid."
        let payloadLength = reader.ReadInt32()
        if payloadLength < 0 || payloadLength > record.Length - 9 then invalidOp "The journal payload length is invalid."
        let payload = Array.zeroCreate<byte> payloadLength
        for byteIndex = 0 to payloadLength - 1 do payload[byteIndex] <- byte (reader.ReadByte())
        let storedLo = uint32 (reader.ReadInt32())
        let storedHi = uint32 (reader.ReadInt32())
        if not reader.AtEnd then invalidOp "The journal record length is invalid."
        let actual = Hash.create ()
        for byteValue in payload do Hash.addByte actual byteValue
        if actual.Lo <> storedLo || actual.Hi <> storedHi then invalidOp "The journal record checksum is invalid."
        JournalCodec.decode payload

#if FABLE_COMPILER
module private Deferred =
    /// Runs the action after the current call stack has unwound.
    [<Fable.Core.Emit("Promise.resolve().then($0)")>]
    let later (action: unit -> unit) : unit = Fable.Core.Util.jsNative
#endif

/// Runs work to completion even when the surrounding computation is canceled. The work starts with the
/// default token, so a cancellation that arrives while it runs takes effect after it has finished.
module internal Shield =
#if FABLE_COMPILER
    /// The JavaScript build resumes the caller from a fresh call stack. Otherwise every shielded call would
    /// keep the rest of the caller's computation nested inside its own stack frames.
    let run (work: Async<'T>) : Async<'T> =
        Async.FromContinuations(fun (resolve, reject, _) ->
            Async.StartWithContinuations(
                work,
                (fun value -> Deferred.later (fun () -> resolve value)),
                (fun error -> Deferred.later (fun () -> reject error)),
                ignore))
#else
    let run (work: Async<'T>) : Async<'T> =
        Async.FromContinuations(fun (resolve, reject, _) -> Async.StartWithContinuations(work, resolve, reject, ignore))
#endif

/// Admits one holder at a time. The holder runs inside a shielded computation, so no cancellation can
/// leave the gate held.
type private AsyncGate() =
    let waiters = Queue<unit -> unit>()
    let mutable held = false

    let enter =
        Async.FromContinuations(fun (resolve, _, _) ->
            let proceed =
                lock waiters (fun () ->
                    if held then
                        waiters.Enqueue(fun () -> resolve ())
                        false
                    else
                        held <- true
                        true)
            if proceed then resolve ())

    let leave () =
        let next =
            lock waiters (fun () ->
                if waiters.Count > 0 then Some(waiters.Dequeue())
                else
                    held <- false
                    None)
        match next with
        | Some resume -> resume ()
        | None -> ()

    member _.Run(work: Async<'T>) : Async<'T> =
        Shield.run (
            async {
                do! enter
                try
                    return! work
                finally
                    leave ()
            }
        )

/// A cached record keeps the encoded bytes. Every hit decodes a fresh value, so callers never share arrays
/// with the cache or with each other.
type private CachedRecord = {
    Record: byte[]
    Cost: int
    mutable LastUse: int64
}

/// Keeps the most recently used journal records in memory. The bytes are charged to the ledger's response
/// data category and to a per-session cap, and the least recently used records leave first. A record that
/// does not fit is not cached.
type private JournalCache(ledger: Ledger, capBytes: int) =
    let entries = Dictionary<int64, CachedRecord>()
    let mutable used = 0
    let mutable tick = 0L

    let evictOldest () =
        let mutable oldest = -1L
        let mutable oldestUse = Int64.MaxValue
        for pair in entries do
            if pair.Value.LastUse < oldestUse then
                oldestUse <- pair.Value.LastUse
                oldest <- pair.Key
        if oldestUse <> Int64.MaxValue then
            let removed = entries[oldest]
            entries.Remove oldest |> ignore
            used <- used - removed.Cost
            ledger.Release(AllocationCategory.ResponseData, int64 removed.Cost)
            true
        else false

    member _.Used = used

    member _.TryGet(sequence: int64) =
        match entries.TryGetValue sequence with
        | true, entry ->
            tick <- tick + 1L
            entry.LastUse <- tick
            Some entry.Record
        | _ -> None

    member _.Add(sequence: int64, record: byte[]) =
        let cost = record.Length
        if cost <= capBytes then
            match entries.TryGetValue sequence with
            | true, existing ->
                entries.Remove sequence |> ignore
                used <- used - existing.Cost
                ledger.Release(AllocationCategory.ResponseData, int64 existing.Cost)
            | _ -> ()
            let mutable room = true
            while room && used + cost > capBytes do
                room <- evictOldest ()
            let mutable reserved = false
            let mutable retry = true
            while retry do
                if ledger.TryReserve(AllocationCategory.ResponseData, int64 cost) then
                    reserved <- true
                    retry <- false
                else retry <- evictOldest ()
            if reserved then
                tick <- tick + 1L
                entries[sequence] <- { Record = record; Cost = cost; LastUse = tick }
                used <- used + cost

    member _.Clear() =
        if used > 0 then ledger.Release(AllocationCategory.ResponseData, int64 used)
        used <- 0
        entries.Clear()

type internal Journal(store: ITempStore, ledger: Ledger, cacheBytes: int) =
    let cache = JournalCache(ledger, cacheBytes)
    let gate = AsyncGate()
    let mutable indexCapacity = 64L
    let mutable dataStart = indexCapacity * 16L
    let mutable count = 0L
    let mutable initialized = false

    let readFully position buffer count = async {
        let! actual = store.ReadAt position buffer 0 count
        if actual <> count then invalidOp "The journal index or record is truncated."
    }

    let readEntry sequence = async {
        if sequence < 0L || sequence >= count || sequence >= indexCapacity then return None
        else
            let bytes = Array.zeroCreate<byte> 16
            let! actual = store.ReadAt (sequence * 16L) bytes 0 bytes.Length
            if actual < bytes.Length then return None
            else
                let reader = JournalReader bytes
                let offset = reader.ReadInt64()
                let length = reader.ReadInt64()
                if length <= 0L then return None else return Some(offset, length)
    }

    let writeEntry sequence offset length = async {
        let writer = JournalWriter()
        writer.WriteInt64 offset
        writer.WriteInt64 length
        let bytes = writer.ToArray()
        do! store.WriteAt (sequence * 16L) bytes 0 bytes.Length
    }

    let growIndex neededSequence = async {
        if neededSequence >= indexCapacity then
            let oldCapacity = indexCapacity
            let mutable nextCapacity = oldCapacity
            while neededSequence >= nextCapacity do nextCapacity <- nextCapacity * 2L
            let oldStart = dataStart
            let newStart = nextCapacity * 16L
            let shift = newStart - oldStart
            let oldEnd = store.Length()
            let block = Array.zeroCreate<byte> 65_536
            let mutable remaining = oldEnd - oldStart
            while remaining > 0L do
                let amount = int (min (int64 block.Length) remaining)
                let source = oldStart + remaining - int64 amount
                do! readFully source block amount
                do! store.WriteAt (source + shift) block 0 amount
                remaining <- remaining - int64 amount
            for sequence = 0L to count - 1L do
                let entry = Array.zeroCreate<byte> 16
                let! actual = store.ReadAt (sequence * 16L) entry 0 entry.Length
                if actual = entry.Length then
                    let reader = JournalReader entry
                    let offset = reader.ReadInt64()
                    let length = reader.ReadInt64()
                    if length > 0L then do! writeEntry sequence (offset + shift) length
            indexCapacity <- nextCapacity
            dataStart <- newStart
            let reserve = Array.zeroCreate<byte> 1
            do! store.WriteAt (dataStart - 1L) reserve 0 1
    }

    member _.Initialize() = async {
        if not initialized then
            let reservation = Array.zeroCreate<byte> 1
            do! store.WriteAt (dataStart - 1L) reservation 0 1
            initialized <- true
    }

    /// Records a result. The whole commit runs to completion once it has started, and lookups wait for it,
    /// so an index relocation never interleaves with a lookup.
    member _.Append(sequence: int64, value: Resumable<DiffPage>) =
        gate.Run(
            async {
                if sequence < 0L then invalidArg (nameof sequence) "The journal sequence cannot be negative."
                if not initialized then invalidOp "The journal has not been initialized."
                do! growIndex sequence
                let record = JournalRecord.encode value
                let! offset = store.Append record 0 record.Length
                do! writeEntry sequence offset (int64 record.Length)
                count <- max count (sequence + 1L)
                cache.Add(sequence, record)
            }
        )

    /// Returns a freshly decoded copy of a recorded result.
    member _.Read(sequence: int64) =
        gate.Run(
            async {
                match cache.TryGet sequence with
                | Some record -> return Some(JournalRecord.decode record)
                | None ->
                    let! entry = readEntry sequence
                    match entry with
                    | None -> return None
                    | Some(offset, length) ->
                        if length > int64 Int32.MaxValue then invalidOp "The journal record is too large."
                        let record = Array.zeroCreate<byte> (int length)
                        do! readFully offset record record.Length
                        let value = JournalRecord.decode record
                        cache.Add(sequence, record)
                        return Some value
            }
        )

    /// Bytes of cached records, which count against the ledger.
    member _.CachedBytes = cache.Used

    /// Drops the cached records and returns their bytes to the ledger.
    member _.Release() = cache.Clear()
