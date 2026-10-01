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

/// Copies the fields of a scanner state to and from scalar values.
module internal ScannerStateCodec =
    let write (header: HeaderBuilder) (state: ScannerState) =
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

    let read (encoding: TextEncoding) (header: HeaderReader) : ScannerState =
        let startOffset = header.Number()
        let state = Scanner.create encoding (int64 startOffset)
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
        state
