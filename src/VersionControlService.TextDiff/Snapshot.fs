namespace VersionControlService.TextDiff

open System
open VersionControlService.Abstractions

/// Little-endian number coding for the pairing index records. Numbers are non-negative integral floats, so line
/// numbers and counts use the same coding on both runtimes without 64-bit integers.
module internal Numbers =
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
        if value < 0.0 then invalidArg (nameof value) "The number is below the supported range."
        let high = Math.Floor(value / Radix)
        let low = value - high * Radix
        writeInt32 bytes index (int (uint32 low))
        writeInt32 bytes (index + 4) (int (uint32 high))

    let readNumber (bytes: byte[]) (index: int) =
        float (uint32 (readInt32 bytes index)) + float (uint32 (readInt32 bytes (index + 4))) * Radix

    let encodeAll (values: float[]) : byte[] =
        let bytes = Array.zeroCreate<byte> (values.Length * NumberBytes)
        for index = 0 to values.Length - 1 do
            writeNumber bytes (index * NumberBytes) values[index]
        bytes
