namespace VersionControlService.TextDiff

open System

type Hash64 = {
    mutable Lo: uint32
    mutable Hi: uint32
}

module Hash =
    let create () = { Lo = 0x84222325u; Hi = 0xCBF29CE4u }

    let inline reset (hash: Hash64) =
        hash.Lo <- 0x84222325u
        hash.Hi <- 0xCBF29CE4u

    let inline addByte (hash: Hash64) (value: byte) =
        let modulus = 4294967296.0
        let mixed = hash.Lo ^^^ uint32 value
        let product = float mixed * 435.0
        let high =
            float hash.Hi * 435.0
            + float (mixed <<< 8)
            + floor (product / modulus)
        hash.Lo <- uint32 (product % modulus)
        hash.Hi <- uint32 (high % modulus)

    let inline addScalar (hash: Hash64) (scalar: int) =
        if scalar <= 0x7F then
            addByte hash (byte scalar)
        elif scalar <= 0x7FF then
            addByte hash (byte (0xC0 ||| (scalar >>> 6)))
            addByte hash (byte (0x80 ||| (scalar &&& 0x3F)))
        elif scalar <= 0xFFFF then
            addByte hash (byte (0xE0 ||| (scalar >>> 12)))
            addByte hash (byte (0x80 ||| ((scalar >>> 6) &&& 0x3F)))
            addByte hash (byte (0x80 ||| (scalar &&& 0x3F)))
        else
            addByte hash (byte (0xF0 ||| (scalar >>> 18)))
            addByte hash (byte (0x80 ||| ((scalar >>> 12) &&& 0x3F)))
            addByte hash (byte (0x80 ||| ((scalar >>> 6) &&& 0x3F)))
            addByte hash (byte (0x80 ||| (scalar &&& 0x3F)))

    let inline addCodeUnit (hash: Hash64) (scalar: int) = addScalar hash scalar

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
