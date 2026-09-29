namespace VersionControlService.TextDiff

open System

#if FABLE_COMPILER
open Fable.Core
#else
open System.Runtime.InteropServices
#endif

/// Array and byte primitives for the scanner loops. The JavaScript build reads and writes typed arrays
/// directly, because Fable otherwise routes every element access through a bounds-checked helper,
/// and it uses Buffer and TextDecoder for bulk work. The .NET build uses the matching managed APIs.
module Native =
#if FABLE_COMPILER
    /// Multiplies two 32-bit integers and keeps the low 32 bits. Plain `*` would round through a double.
    [<Emit("Math.imul($0, $1)")>]
    let imul (left: int) (right: int) : int = jsNative

    [<Emit("$0[$1]")>]
    let readByte (bytes: byte[]) (index: int) : int = jsNative

    [<Emit("$0[$1] = $2")>]
    let writeByte (bytes: byte[]) (index: int) (value: int) : unit = jsNative

    [<Emit("$0[$1]")>]
    let readInt (values: int[]) (index: int) : int = jsNative

    [<Emit("$0[$1] = $2")>]
    let writeInt (values: int[]) (index: int) (value: int) : unit = jsNative

    [<Emit("$0[$1]")>]
    let readFloat (values: float[]) (index: int) : float = jsNative

    [<Emit("$0[$1] = $2")>]
    let writeFloat (values: float[]) (index: int) (value: float) : unit = jsNative

    [<Emit("$0[$1]")>]
    let readUnit (units: uint16[]) (index: int) : int = jsNative

    [<Emit("$0[$1] = $2")>]
    let writeUnit (units: uint16[]) (index: int) (value: int) : unit = jsNative

    // Buffer's indexOf is a native byte search. TypedArray.prototype.indexOf is about four times slower.
    [<Emit("(ArrayBuffer.isView($0) ? Buffer.prototype.indexOf.call($0, $1, $2) : $0.indexOf($1, $2))")>]
    let private indexOfFrom (bytes: byte[]) (value: int) (start: int) : int = jsNative

    /// True when the array is a typed array, which file writes require.
    [<Emit("ArrayBuffer.isView($0)")>]
    let isTypedBytes (bytes: byte[]) : bool = jsNative

    // Fable builds byte[] as a Uint8Array, but Seq.toArray and array literals can produce a plain
    // array. Views work on typed arrays only, so a plain array is copied into one first.
    [<Emit("(ArrayBuffer.isView($0) ? $0.subarray($1, $2) : Uint8Array.from($0.slice($1, $2)))")>]
    let private subarray (bytes: byte[]) (start: int) (finish: int) : byte[] = jsNative

    [<Emit("(ArrayBuffer.isView($0) ? Buffer.from($0.buffer, $0.byteOffset + $1, $2) : Buffer.from($0.slice($1, $1 + $2)))")>]
    let private bufferView (bytes: byte[]) (offset: int) (count: int) : byte[] = jsNative

    /// Returns bytes[offset .. offset + count - 1] as a Uint8Array view that shares memory with `bytes`.
    let view (bytes: byte[]) (offset: int) (count: int) = subarray bytes offset (offset + count)

    /// Returns the index of the first `value` in bytes[start .. finish - 1], or -1.
    let indexOfByte (bytes: byte[]) (value: int) (start: int) (finish: int) =
        if start >= finish then -1
        elif finish = bytes.Length then indexOfFrom bytes value start
        else
            // A view keeps the native search from running past `finish`.
            let found = indexOfFrom (subarray bytes start finish) value 0
            if found < 0 then -1 else found + start

    [<Emit("$0.equals($1)")>]
    let private buffersEqual (left: byte[]) (right: byte[]) : bool = jsNative

    let bytesEqual (left: byte[]) (leftOffset: int) (right: byte[]) (rightOffset: int) (count: int) =
        buffersEqual (bufferView left leftOffset count) (bufferView right rightOffset count)

    [<Import("isUtf8", "node:buffer")>]
    let private nodeIsUtf8 (view: byte[]) : bool = jsNative

    /// Strict UTF-8 validation with the same rules as the strict decoder.
    let isUtf8 (bytes: byte[]) (offset: int) (count: int) = nodeIsUtf8 (subarray bytes offset (offset + count))

    [<Emit("new TextDecoder('utf-16le', { ignoreBOM: true })")>]
    let private createUtf16Decoder () : obj = jsNative

    // ignoreBOM keeps a leading U+FEFF in the text. The default decoder would drop it.
    let private utf16Decoder = createUtf16Decoder ()

    [<Emit("$0.decode($1.subarray(0, $2))")>]
    let private decodeUnits (decoder: obj) (units: uint16[]) (count: int) : string = jsNative

    /// Builds a string from the first `count` UTF-16 code units.
    let utf16Decode (units: uint16[]) (count: int) =
        if count = 0 then "" else decodeUnits utf16Decoder units count
#else
    let inline imul (left: int) (right: int) = left * right

    /// True when the array is a typed array, which file writes require. Every .NET byte array qualifies.
    let inline isTypedBytes (_bytes: byte[]) = true

    let inline readByte (bytes: byte[]) (index: int) = int bytes[index]

    let inline writeByte (bytes: byte[]) (index: int) (value: int) = bytes[index] <- byte value

    let inline readInt (values: int[]) (index: int) = values[index]

    let inline writeInt (values: int[]) (index: int) (value: int) = values[index] <- value

    let inline readFloat (values: float[]) (index: int) = values[index]

    let inline writeFloat (values: float[]) (index: int) (value: float) = values[index] <- value

    let inline readUnit (units: uint16[]) (index: int) = int units[index]

    let inline writeUnit (units: uint16[]) (index: int) (value: int) = units[index] <- uint16 value

    /// Returns bytes[offset .. offset + count - 1]. The .NET build copies unless the range is the whole array.
    let view (bytes: byte[]) (offset: int) (count: int) =
        if offset = 0 && count = bytes.Length then bytes else Array.sub bytes offset count

    /// Returns the index of the first `value` in bytes[start .. finish - 1], or -1.
    let indexOfByte (bytes: byte[]) (value: int) (start: int) (finish: int) =
        if start >= finish then -1
        else Array.IndexOf(bytes, byte value, start, finish - start)

    let bytesEqual (left: byte[]) (leftOffset: int) (right: byte[]) (rightOffset: int) (count: int) =
        MemoryExtensions.SequenceEqual(ReadOnlySpan<byte>(left, leftOffset, count), ReadOnlySpan<byte>(right, rightOffset, count))

    /// Strict UTF-8 validation with the same rules as the strict decoder.
    let isUtf8 (bytes: byte[]) (offset: int) (count: int) =
        System.Text.Unicode.Utf8.IsValid(ReadOnlySpan<byte>(bytes, offset, count))

    /// Builds a string from the first `count` UTF-16 code units.
    let utf16Decode (units: uint16[]) (count: int) =
        String(MemoryMarshal.Cast<uint16, char>(ReadOnlySpan<uint16>(units, 0, count)))
#endif
