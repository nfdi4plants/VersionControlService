namespace VersionControlService.TextDiff

open System
open VersionControlService.Abstractions

/// Little-endian number coding for spilled state. Numbers are floats that are integral and at least -2^31, so
/// offsets, counts and signed hash words use the same coding on both runtimes without 64-bit integers.
module internal Numbers =
    [<Literal>]
    let Bias = 2147483648.0

    [<Literal>]
    let Radix = 4294967296.0

    [<Literal>]
    let NumberBytes = 8

    let writeInt32 (bytes: byte[]) (index: int) (value: int) =
        Native.writeByte bytes index (value &&& 0xFF)
        Native.writeByte bytes (index + 1) ((value >>> 8) &&& 0xFF)
        Native.writeByte bytes (index + 2) ((value >>> 16) &&& 0xFF)
        Native.writeByte bytes (index + 3) ((value >>> 24) &&& 0xFF)

    let readInt32 (bytes: byte[]) (index: int) =
        Native.readByte bytes index
        ||| (Native.readByte bytes (index + 1) <<< 8)
        ||| (Native.readByte bytes (index + 2) <<< 16)
        ||| (Native.readByte bytes (index + 3) <<< 24)

    let writeNumber (bytes: byte[]) (index: int) (value: float) =
        let biased = value + Bias
        if biased < 0.0 then invalidArg (nameof value) "The number is below the supported range."
        let high = Math.Floor(biased / Radix)
        let low = biased - high * Radix
        writeInt32 bytes index (int (uint32 low))
        writeInt32 bytes (index + 4) (int (uint32 high))

    let readNumber (bytes: byte[]) (index: int) =
        float (uint32 (readInt32 bytes index)) + float (uint32 (readInt32 bytes (index + 4))) * Radix - Bias

    let encodeAll (values: float[]) : byte[] =
        let bytes = Array.zeroCreate<byte> (values.Length * NumberBytes)
        for index = 0 to values.Length - 1 do
            writeNumber bytes (index * NumberBytes) values[index]
        bytes

    let decodeAll (bytes: byte[]) (count: int) : float[] =
        let values = Array.zeroCreate<float> count
        for index = 0 to count - 1 do
            values[index] <- readNumber bytes (index * NumberBytes)
        values

/// Collects the scalar values of a spilled state in a fixed order.
type internal HeaderBuilder() =
    let values = ResizeArray<float>()
    member _.Number(value: float) = values.Add value
    member _.Int(value: int) = values.Add(float value)
    member _.Bool(value: bool) = values.Add(if value then 1.0 else 0.0)
    member _.Count = values.Count
    member _.ToArray() = values.ToArray()

/// Reads back the scalar values in the order that HeaderBuilder wrote them.
type internal HeaderReader(values: float[]) =
    let mutable position = 0

    member _.Number() =
        if position >= values.Length then invalidOp "The spilled state ends early."
        let value = values[position]
        position <- position + 1
        value

    member this.Int() = int (this.Number())
    member this.Bool() = this.Number() <> 0.0
    member _.AtEnd = position >= values.Length

/// One typed array of a spilled state. Only the array that matches the kind is set.
type internal ArraySlot(floats: float[], ints: int[], bytes: byte[], count: int) =
    let mutable filled = 0
    member _.Floats = floats
    member _.Ints = ints
    member _.Bytes = bytes
    member _.Count = count
    member _.Filled with get () = filled and set value = filled <- value
    member _.Width = if not (isNull (box floats)) then Numbers.NumberBytes elif not (isNull (box ints)) then 4 else 1

    static member OfFloats(values: float[], count: int) = ArraySlot(values, Unchecked.defaultof<int[]>, Unchecked.defaultof<byte[]>, count)
    static member OfInts(values: int[], count: int) = ArraySlot(Unchecked.defaultof<float[]>, values, Unchecked.defaultof<byte[]>, count)
    static member OfBytes(values: byte[], count: int) = ArraySlot(Unchecked.defaultof<float[]>, Unchecked.defaultof<int[]>, values, count)

/// Writes a spilled state to a temp store from position zero. The block buffer is transient, so a spill
/// itself needs only this one buffer while it frees the state it writes.
type internal SnapshotWriter(store: ITempStore) =
    let buffer = Array.zeroCreate<byte> 65_536
    let mutable used = 0
    let mutable position = 0L

    let flush () = async {
        if used > 0 then
            do! store.WriteAt position buffer 0 used
            position <- position + int64 used
            used <- 0
    }

    member _.Slot(slot: ArraySlot) = async {
        let width = slot.Width
        let mutable written = 0
        while written < slot.Count do
            let room = (buffer.Length - used) / width
            if room = 0 then do! flush ()
            else
                let amount = min room (slot.Count - written)
                if width = Numbers.NumberBytes then
                    for index = 0 to amount - 1 do
                        Numbers.writeNumber buffer (used + index * width) (Native.readFloat slot.Floats (written + index))
                elif width = 4 then
                    for index = 0 to amount - 1 do
                        Numbers.writeInt32 buffer (used + index * width) (Native.readInt slot.Ints (written + index))
                else
                    for index = 0 to amount - 1 do
                        Native.writeByte buffer (used + index) (Native.readByte slot.Bytes (written + index))
                used <- used + amount * width
                written <- written + amount
    }

    member this.Header(values: float[]) = async {
        do! this.Slot(ArraySlot.OfFloats([| float values.Length |], 1))
        do! this.Slot(ArraySlot.OfFloats(values, values.Length))
    }

    member _.Complete() = async {
        do! flush ()
        return position
    }

/// Reads a spilled state back in bounded steps. Each call moves at most the requested number of bytes.
type internal SnapshotReader(store: ITempStore, scratch: byte[]) =
    let mutable position = 0L

    let readBlock (bytes: int) = async {
        let! actual = store.ReadAt position scratch 0 bytes
        if actual < bytes then invalidOp "The spilled state is truncated."
        position <- position + int64 bytes
    }

    member _.ScratchBytes = scratch.Length

    member this.ReadNumbers(count: int) = async {
        let values = Array.zeroCreate<float> count
        let perBlock = max 1 (scratch.Length / Numbers.NumberBytes)
        let mutable done' = 0
        while done' < count do
            let amount = min perBlock (count - done')
            do! readBlock (amount * Numbers.NumberBytes)
            for index = 0 to amount - 1 do
                values[done' + index] <- Numbers.readNumber scratch (index * Numbers.NumberBytes)
            done' <- done' + amount
        return values
    }

    member this.Header() = async {
        let! count = this.ReadNumbers 1
        if count[0] < 0.0 || count[0] > 1_000_000.0 then invalidOp "The spilled state header is invalid."
        return! this.ReadNumbers(int count[0])
    }

    /// Fills part of a slot and returns the number of bytes read.
    member _.Fill(slot: ArraySlot, maxBytes: int) = async {
        let width = slot.Width
        let amount = min (slot.Count - slot.Filled) (max 1 (min maxBytes scratch.Length / width))
        if amount <= 0 then return 0
        else
            do! readBlock (amount * width)
            if width = Numbers.NumberBytes then
                for index = 0 to amount - 1 do
                    Native.writeFloat slot.Floats (slot.Filled + index) (Numbers.readNumber scratch (index * width))
            elif width = 4 then
                for index = 0 to amount - 1 do
                    Native.writeInt slot.Ints (slot.Filled + index) (Numbers.readInt32 scratch (index * width))
            else
                for index = 0 to amount - 1 do
                    Native.writeByte slot.Bytes (slot.Filled + index) (Native.readByte scratch index)
            slot.Filled <- slot.Filled + amount
            return amount * width
    }

/// Copies the fields of a scanner state that has no retained text to and from scalar values.
module internal ScannerStateCodec =
    let write (header: HeaderBuilder) (state: ScannerState) =
        if state.RetainLimit.IsSome then invalidArg (nameof state) "A scanner with retained text cannot be spilled."
        header.Number(float state.StartOffset)
        header.Number state.NextOffset
        header.Number state.ValidatedBytes
        header.Int state.PendingValue
        header.Int state.PendingCount
        header.Int state.ExpectedCount
        header.Number state.PendingStart
        header.Int state.PendingHigh
        header.Number state.PendingHighStart
        header.Number state.PendingHighEnd
        header.Number state.LineStart
        header.Number state.LineLengthUtf16
        header.Bool state.LineHasText
        header.Int state.KeyLo
        header.Int state.KeyHi
        header.Bool state.PendingCR
        header.Number state.PendingCREnd
        header.Bool state.IsComplete
        let writeWindow (window: ObservationWindow) =
            header.Number window.Start
            header.Int window.Bytes
            header.Int window.Scalars
            header.Int window.Controls
            header.Int window.FirstControl
        writeWindow state.CurrentWindow
        match state.PreviousWindow with
        | Some window ->
            header.Bool true
            writeWindow window
        | None -> header.Bool false

    let read (encoding: TextEncoding) (header: HeaderReader) : ScannerState =
        let startOffset = header.Number()
        let state = Scanner.create encoding (int64 startOffset) None
        state.NextOffset <- header.Number()
        state.ValidatedBytes <- header.Number()
        state.PendingValue <- header.Int()
        state.PendingCount <- header.Int()
        state.ExpectedCount <- header.Int()
        state.PendingStart <- header.Number()
        state.PendingHigh <- header.Int()
        state.PendingHighStart <- header.Number()
        state.PendingHighEnd <- header.Number()
        state.LineStart <- header.Number()
        state.LineLengthUtf16 <- header.Number()
        state.LineHasText <- header.Bool()
        state.KeyLo <- header.Int()
        state.KeyHi <- header.Int()
        state.PendingCR <- header.Bool()
        state.PendingCREnd <- header.Number()
        state.IsComplete <- header.Bool()
        let readWindow () : ObservationWindow =
            let start = header.Number()
            let bytes = header.Int()
            let scalars = header.Int()
            let controls = header.Int()
            let first = header.Int()
            { Start = start; Bytes = bytes; Scalars = scalars; Controls = controls; FirstControl = first }
        state.CurrentWindow <- readWindow ()
        state.PreviousWindow <- if header.Bool() then Some(readWindow ()) else None
        state
