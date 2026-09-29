namespace VersionControlService.TextDiff

open System

type Hash64 = {
    mutable Lo: uint32
    mutable Hi: uint32
}

module Hash =
    /// Low word of the FNV-1a 64 offset basis, as a signed 32-bit value.
    [<Literal>]
    let OffsetLo = 0x84222325

    /// High word of the FNV-1a 64 offset basis, as a signed 32-bit value.
    [<Literal>]
    let OffsetHi = 0xCBF29CE4

    /// High word after multiplying by the FNV prime 2^40 + 435, where `mixed` is the low word already
    /// combined with the next byte. The carry is the high half of mixed * 435. It is built from two
    /// 16-bit halves so that every product stays below 2^53 in JavaScript.
    let inline mixHi (hi: int) (mixed: int) =
        let bits = uint32 mixed
        let carry = int (((bits >>> 16) * 435u + (((bits &&& 0xFFFFu) * 435u) >>> 16)) >>> 16)
        Native.imul hi 435 + (mixed <<< 8) + carry

    /// Low word after multiplying by the FNV prime, where `mixed` is the low word combined with the next byte.
    let inline mixLo (mixed: int) = Native.imul mixed 435

    let create () = { Lo = uint32 OffsetLo; Hi = uint32 OffsetHi }

    let inline reset (hash: Hash64) =
        hash.Lo <- uint32 OffsetLo
        hash.Hi <- uint32 OffsetHi

    let inline addByte (hash: Hash64) (value: byte) =
        let mixed = int hash.Lo ^^^ int value
        hash.Hi <- uint32 (mixHi (int hash.Hi) mixed)
        hash.Lo <- uint32 (mixLo mixed)

    /// Calls `emit` with each byte of the UTF-8 encoding of a scalar value. It is not inline because
    /// Fable declares the lambda's parameter once per inlined call and emits duplicate declarations.
    let forEachUtf8Byte (scalar: int) (emit: int -> unit) =
        if scalar <= 0x7F then
            emit scalar
        elif scalar <= 0x7FF then
            emit (0xC0 ||| (scalar >>> 6))
            emit (0x80 ||| (scalar &&& 0x3F))
        elif scalar <= 0xFFFF then
            emit (0xE0 ||| (scalar >>> 12))
            emit (0x80 ||| ((scalar >>> 6) &&& 0x3F))
            emit (0x80 ||| (scalar &&& 0x3F))
        else
            emit (0xF0 ||| (scalar >>> 18))
            emit (0x80 ||| ((scalar >>> 12) &&& 0x3F))
            emit (0x80 ||| ((scalar >>> 6) &&& 0x3F))
            emit (0x80 ||| (scalar &&& 0x3F))

    let addScalar (hash: Hash64) (scalar: int) =
        forEachUtf8Byte scalar (fun value -> addByte hash (byte value))

    let addCodeUnit (hash: Hash64) (scalar: int) = addScalar hash scalar

    let toString (hash: Hash64) =
        let chars = Array.zeroCreate<char> 16
        let digits = "0123456789abcdef"
        let writeWord (value: uint32) offset =
            for index = 0 to 7 do
                let shift = (7 - index) * 4
                chars[offset + index] <- digits[int ((value >>> shift) &&& 0xFu)]
        writeWord hash.Hi 0
        writeWord hash.Lo 8
        String(chars)
