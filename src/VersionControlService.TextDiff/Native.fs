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

    [<Emit("$2.set($0.subarray($1, $1 + $4), $3)")>]
    let copyBytes (source: byte[]) (sourceOffset: int) (target: byte[]) (targetOffset: int) (count: int) : unit = jsNative

    /// The JavaScript DataView class, which writes multi-byte numbers at a byte offset.
    [<Global>]
    type private DataView(buffer: JS.ArrayBuffer, byteOffset: int, byteLength: int) =
        member _.setBigInt64(offset: int, value: int64, littleEndian: bool) : unit = jsNative

    /// Writes the eight bytes of a 64-bit integer in little-endian order. Fable 5 holds int64 as a BigInt.
    let writeInt64 (bytes: byte[]) (index: int) (value: int64) : unit =
        DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength).setBigInt64(index, value, true)

    [<Emit("$0[$1]")>]
    let readInt (values: int[]) (index: int) : int = jsNative

    [<Emit("$0[$1] = $2")>]
    let writeInt (values: int[]) (index: int) (value: int) : unit = jsNative

    [<Emit("$0[$1]")>]
    let readFloat (values: float[]) (index: int) : float = jsNative

    [<Emit("$0[$1] = $2")>]
    let writeFloat (values: float[]) (index: int) (value: float) : unit = jsNative

    [<Emit("(($left, $leftStart, $right, $rightStart, $count) => { let matched = 0; while (matched < $count && $left[$leftStart + matched] === $right[$rightStart + matched]) matched++; return matched; })($0, $1, $2, $3, $4)")>]
    let equalUnitPrefix (left: uint16[]) (leftStart: int) (right: uint16[]) (rightStart: int) (count: int) : int = jsNative

    [<Emit("(($left, $leftStart, $right, $rightStart, $count) => { let matched = 0; while (matched < $count && $left[$leftStart + matched] === $right[$rightStart + matched]) matched++; return matched; })($0, $1, $2, $3, $4)")>]
    let equalIntPrefix (left: int[]) (leftStart: int) (right: int[]) (rightStart: int) (count: int) : int = jsNative

    [<Emit("(($left, $leftEnd, $right, $rightEnd, $count) => { let matched = 0; while (matched < $count && $left[$leftEnd - matched - 1] === $right[$rightEnd - matched - 1]) matched++; return matched; })($0, $1, $2, $3, $4)")>]
    let equalUnitSuffix (left: uint16[]) (leftEnd: int) (right: uint16[]) (rightEnd: int) (count: int) : int = jsNative

    [<Emit("(($left, $leftStart, $right, $rightStart, $count) => { let matched = 0; while (matched < $count && $left[$leftStart + matched] === $right[$rightStart + matched]) matched++; return matched; })($0, $1, $2, $3, $4)")>]
    let equalBytePrefix (left: byte[]) (leftStart: int) (right: byte[]) (rightStart: int) (count: int) : int = jsNative

    // Buffer's indexOf is a native byte search. TypedArray.prototype.indexOf is about four times slower.
    [<Emit("(ArrayBuffer.isView($0) ? Buffer.prototype.indexOf.call($0, $1, $2) : $0.indexOf($1, $2))")>]
    let private indexOfFrom (bytes: byte[]) (value: int) (start: int) : int = jsNative

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

    [<Emit("new TextDecoder('utf-8', { fatal: true, ignoreBOM: true })")>]
    let private createUtf8Decoder () : obj = jsNative

    [<Emit("new TextEncoder()")>]
    let private createUtf8Encoder () : obj = jsNative

    // ignoreBOM keeps a leading U+FEFF in the text. The default decoder would drop it.
    let private utf16Decoder = createUtf16Decoder ()
    let private utf8Decoder = createUtf8Decoder ()
    let private utf8Encoder = createUtf8Encoder ()

    [<Emit("$0.decode($1.subarray(0, $2))")>]
    let private decodeUnits (decoder: obj) (units: uint16[]) (count: int) : string = jsNative

    [<Emit("$0.decode($1.subarray($2, $2 + $3))")>]
    let private decodeUtf8 (decoder: obj) (bytes: byte[]) (offset: int) (count: int) : string = jsNative

    [<Emit("$0.encodeInto($1, $2.subarray($3)).written")>]
    let private encodeUtf8Into (encoder: obj) (value: string) (bytes: byte[]) (offset: int) : int = jsNative

    /// Builds a string from the first `count` UTF-16 code units.
    let utf16Decode (units: uint16[]) (count: int) =
        if count = 0 then "" else decodeUnits utf16Decoder units count

    /// Decodes one validated UTF-8 byte range while preserving a leading BOM.
    let utf8Decode (bytes: byte[]) (offset: int) (count: int) =
        if count = 0 then "" else decodeUtf8 utf8Decoder bytes offset count

    let utf8EncodeInto (value: string) (bytes: byte[]) (offset: int) =
        if value.Length = 0 then 0 else encodeUtf8Into utf8Encoder value bytes offset
#else
    let inline imul (left: int) (right: int) = left * right

    let inline readByte (bytes: byte[]) (index: int) = int bytes[index]

    let inline writeByte (bytes: byte[]) (index: int) (value: int) = bytes[index] <- byte value

    let writeInt64 (bytes: byte[]) (index: int) (value: int64) =
        System.Buffers.Binary.BinaryPrimitives.WriteInt64LittleEndian(Span<byte>(bytes, index, 8), value)

    let inline copyBytes (source: byte[]) (sourceOffset: int) (target: byte[]) (targetOffset: int) (count: int) =
        Array.Copy(source, sourceOffset, target, targetOffset, count)

    let inline readInt (values: int[]) (index: int) = values[index]

    let inline writeInt (values: int[]) (index: int) (value: int) = values[index] <- value

    let inline readFloat (values: float[]) (index: int) = values[index]

    let inline writeFloat (values: float[]) (index: int) (value: float) = values[index] <- value

    let equalUnitPrefix (left: uint16[]) (leftStart: int) (right: uint16[]) (rightStart: int) (count: int) =
        let mutable matched = 0
        while matched < count && left[leftStart + matched] = right[rightStart + matched] do
            matched <- matched + 1
        matched

    let equalIntPrefix (left: int[]) (leftStart: int) (right: int[]) (rightStart: int) (count: int) =
        let mutable matched = 0
        while matched < count && left[leftStart + matched] = right[rightStart + matched] do
            matched <- matched + 1
        matched

    let equalUnitSuffix (left: uint16[]) (leftEnd: int) (right: uint16[]) (rightEnd: int) (count: int) =
        let mutable matched = 0
        while matched < count && left[leftEnd - matched - 1] = right[rightEnd - matched - 1] do
            matched <- matched + 1
        matched

    let equalBytePrefix (left: byte[]) (leftStart: int) (right: byte[]) (rightStart: int) (count: int) =
        let mutable matched = 0
        while matched < count && left[leftStart + matched] = right[rightStart + matched] do
            matched <- matched + 1
        matched

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

    let utf8Decode (bytes: byte[]) (offset: int) (count: int) =
        System.Text.Encoding.UTF8.GetString(bytes, offset, count)

    let utf8EncodeInto (value: string) (bytes: byte[]) (offset: int) =
        System.Text.Encoding.UTF8.GetBytes(value, 0, value.Length, bytes, offset)
#endif
