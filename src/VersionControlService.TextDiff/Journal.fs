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

    let ensure additional =
        let required = count + additional
        if required > bytes.Length then
            let mutable capacity = bytes.Length
            while capacity < required do capacity <- max (capacity * 2) required
            let grown = Array.zeroCreate<byte> capacity
            Native.copyBytes bytes 0 grown 0 count
            bytes <- grown

    member _.Position = count

    member _.WriteByte(value: int) =
        ensure 1
        Native.writeByte bytes count value
        count <- count + 1

    member this.WriteBool(value: bool) = this.WriteByte(if value then 1 else 0)

    member this.WriteInt32(value: int) =
        ensure 4
        Native.writeByte bytes count (value &&& 0xFF)
        Native.writeByte bytes (count + 1) ((value >>> 8) &&& 0xFF)
        Native.writeByte bytes (count + 2) ((value >>> 16) &&& 0xFF)
        Native.writeByte bytes (count + 3) ((value >>> 24) &&& 0xFF)
        count <- count + 4

    member this.WriteInt64(value: int64) =
        ensure 8
        Native.writeInt64 bytes count value
        count <- count + 8

    member _.WriteBytes(value: byte[]) =
        ensure value.Length
        Native.copyBytes value 0 bytes count value.Length
        count <- count + value.Length

    member this.WriteString(value: string) =
        ensure (4 + value.Length * 3)
        let written = Native.utf8EncodeInto value bytes (count + 4)
        this.WriteInt32 written
        count <- count + written

    member this.WriteOptionString(value: string option) =
        match value with
        | Some text -> this.WriteBool true; this.WriteString text
        | None -> this.WriteBool false

    member _.ToArray() =
        let result = Array.zeroCreate<byte> count
        Native.copyBytes bytes 0 result 0 count
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

    member _.ReadBytes(count: int) =
        require count
        let value = Array.zeroCreate<byte> count
        Native.copyBytes bytes position value 0 count
        position <- position + count
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

[<RequireQualifiedAccess>]
type internal JournalValue =
    | Page of Resumable<DiffPage>
    | Expansion of Resumable<DiffPart[]>
    | Line of Resumable<DiffLine>

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

    /// Pages carry a pending-preview flag that is always false, because replays never show a preview.
    let private readPending (reader: JournalReader) : PendingPreview option =
        reader.ReadBool() |> ignore
        None

    let encode (value: JournalValue) =
        let writer = JournalWriter()
        writer.WriteByte 1
        match value with
        | JournalValue.Page result ->
            writer.WriteByte 0
            match result with
            | Resumable.Ready page ->
                writer.WriteByte 0
                writer.WriteString page.PageId
                writer.WriteOptionString page.NextCursor
                writer.WriteInt32 page.Parts.Length
                for part in page.Parts do writePart writer part
                writeProgress writer page.Progress
                writer.WriteBool page.OutputComplete
                writer.WriteBool false
            | Resumable.Scanning(progress, continuation, _) ->
                writer.WriteByte 1
                writeProgress writer progress
                writer.WriteString continuation
                writer.WriteBool false
        | JournalValue.Expansion result ->
            writer.WriteByte 1
            match result with
            | Resumable.Ready parts ->
                writer.WriteByte 0
                writer.WriteInt32 parts.Length
                for part in parts do writePart writer part
            | Resumable.Scanning(progress, continuation, _) ->
                writer.WriteByte 1
                writeProgress writer progress
                writer.WriteString continuation
        | JournalValue.Line result ->
            writer.WriteByte 2
            match result with
            | Resumable.Ready line -> writer.WriteByte 0; writeLine writer line
            | Resumable.Scanning(progress, continuation, _) ->
                writer.WriteByte 1
                writeProgress writer progress
                writer.WriteString continuation
        writer.ToArray()

    let decode (bytes: byte[]) =
        let reader = JournalReader bytes
        if reader.ReadByte() <> 1 then invalidOp "The journal record version is unsupported."
        let value =
            match reader.ReadByte() with
            | 0 ->
                let result =
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
                JournalValue.Page result
            | 1 ->
                let result =
                    match reader.ReadByte() with
                    | 1 -> Resumable.Scanning(readProgress reader, reader.ReadString(), None)
                    | _ ->
                        let count = reader.ReadInt32()
                        if count < 0 || count > 100_000 then invalidOp "The journal part count is invalid."
                        Resumable.Ready(Array.init count (fun _ -> readPart reader))
                JournalValue.Expansion result
            | _ ->
                let result =
                    match reader.ReadByte() with
                    | 1 -> Resumable.Scanning(readProgress reader, reader.ReadString(), None)
                    | _ -> Resumable.Ready(readLine reader)
                JournalValue.Line result
        if not reader.AtEnd then invalidOp "The journal record has trailing data."
        value

/// Frames an encoded value with its checksum. The byte loops live outside the async members because
/// an async loop pays one scheduling step per iteration on the JavaScript runtime.
module internal JournalRecord =
    let encode (value: JournalValue) : byte[] =
        let payload = JournalCodec.encode value
        let hash = Hash.create ()
        for byteIndex = 0 to payload.Length - 1 do Hash.addByte hash payload[byteIndex]
        let writer = JournalWriter()
        writer.WriteByte 0x54
        writer.WriteInt32 payload.Length
        writer.WriteBytes payload
        writer.WriteInt32(int hash.Lo)
        writer.WriteInt32(int hash.Hi)
        writer.ToArray()

    let decode (record: byte[]) : JournalValue =
        let reader = JournalReader record
        if reader.ReadByte() <> 0x54 then invalidOp "The journal record header is invalid."
        let payloadLength = reader.ReadInt32()
        if payloadLength < 0 || payloadLength > record.Length - 9 then invalidOp "The journal payload length is invalid."
        let payload = reader.ReadBytes payloadLength
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

    member _.Remove(sequence: int64) =
        match entries.TryGetValue sequence with
        | true, entry ->
            entries.Remove sequence |> ignore
            used <- used - entry.Cost
            ledger.Release(AllocationCategory.ResponseData, int64 entry.Cost)
        | _ -> ()

    member _.Clear() =
        if used > 0 then ledger.Release(AllocationCategory.ResponseData, int64 used)
        used <- 0
        entries.Clear()

type internal Journal(store: ITempStore, ledger: Ledger, cacheBytes: int, createIndexStore: unit -> Async<ITempStore>) =
    let cache = JournalCache(ledger, cacheBytes)
    let mutable indexStore: ITempStore option = None
    let indexEntryBytes = 24L
    let mutable count = 0L
    let mutable initialized = false

    let getIndexStore () = async {
        match indexStore with
        | Some value -> return value
        | None ->
            let! created = createIndexStore ()
            indexStore <- Some created
            return created
    }

    let readFully position buffer count = async {
        let! actual = store.ReadAt position buffer 0 count
        if actual <> count then invalidOp "The journal record is truncated."
    }

    let readEntry sequence = async {
        let! index = getIndexStore ()
        let indexCapacity = index.Length() / indexEntryBytes
        if sequence < 0L || sequence >= count || sequence >= indexCapacity then return None
        else
            let bytes = Array.zeroCreate<byte> (int indexEntryBytes)
            let! actual = index.ReadAt (sequence * indexEntryBytes) bytes 0 bytes.Length
            if actual < bytes.Length then return None
            else
                let reader = JournalReader bytes
                let key = reader.ReadInt64()
                let offset = reader.ReadInt64()
                let length = reader.ReadInt64()
                if key <> sequence || length <= 0L then return None else return Some(offset, length)
    }

    let writeEntry sequence offset length = async {
        let! index = getIndexStore ()
        let writer = JournalWriter()
        writer.WriteInt64 sequence
        writer.WriteInt64 offset
        writer.WriteInt64 length
        let bytes = writer.ToArray()
        do! index.WriteAt (sequence * indexEntryBytes) bytes 0 bytes.Length
    }

    let appendZeroSlots slotCount = async {
        let! index = getIndexStore ()
        let slotsPerChunk = 2_730
        let zeroSlots = Array.zeroCreate<byte> (slotsPerChunk * int indexEntryBytes)
        let mutable remaining = slotCount
        while remaining > 0L do
            let slots = int (min (int64 slotsPerChunk) remaining)
            let! _ = index.Append zeroSlots 0 (slots * int indexEntryBytes)
            remaining <- remaining - int64 slots
    }

    let growIndex neededSequence = async {
        let! index = getIndexStore ()
        let currentCapacity = index.Length() / indexEntryBytes
        if neededSequence >= currentCapacity then
            let mutable nextCapacity = max 64L currentCapacity
            while neededSequence >= nextCapacity do nextCapacity <- nextCapacity * 2L
            do! appendZeroSlots (nextCapacity - currentCapacity)
    }

    member _.Initialize() = async {
        if not initialized then
            do! growIndex 0L
            initialized <- true
    }

    /// Records a result.
    member _.Append(sequence: int64, value: JournalValue) = async {
        if sequence < 0L then invalidArg (nameof sequence) "The journal sequence cannot be negative."
        if not initialized then invalidOp "The journal has not been initialized."
        do! growIndex sequence
        let record = JournalRecord.encode value
        let! offset = store.Append record 0 record.Length
        do! writeEntry sequence offset (int64 record.Length)
        count <- max count (sequence + 1L)
        cache.Add(sequence, record)
    }

    /// Returns a freshly decoded copy of a recorded result.
    member _.Read(sequence: int64) = async {
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

    member _.Link(aliasSequence: int64, targetSequence: int64) = async {
        if aliasSequence < 0L || targetSequence < 0L then invalidArg (nameof aliasSequence) "The journal key cannot be negative."
        if not initialized then invalidOp "The journal has not been initialized."
        do! growIndex aliasSequence
        let! entry = readEntry targetSequence
        match entry with
        | Some(offset, length) ->
            do! writeEntry aliasSequence offset length
            count <- max count (aliasSequence + 1L)
            cache.Remove aliasSequence
        | None -> invalidOp "The journal target does not exist."
    }

    /// Drops the cached records and returns their bytes to the ledger.
    member _.Release() = cache.Clear()

    member _.Dispose() = async {
        match indexStore with
        | Some value ->
            do! value.Dispose()
            indexStore <- None
        | None -> ()
    }
