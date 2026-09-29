module VersionControlService.Tests.TextDiffLfsPointerTests

open System.Text
open Vitest
open VersionControlService.Git.TextDiff.TextDiffLfsPointer

let private oid = String.replicate 32 "ab"
let private otherHash = String.replicate 64 "c"

let private bytes (text: string) = Encoding.UTF8.GetBytes text

let private parse (text: string) = tryParseStrict (bytes text)

let private lines (items: string list) = items |> List.map (fun line -> line + "\n") |> String.concat ""

let private version = "version https://git-lfs.github.com/spec/v1"

let private expectRejected (text: string) =
    Vitest.expect(parse text).toBe None

Vitest.describe (
    "Strict LFS pointer parsing",
    fun () ->
        Vitest.test (
            "accepts a pointer with LF line endings",
            fun () ->
                let parsed = parse (lines [ version; $"oid sha256:{oid}"; "size 12345" ])
                Vitest.expect(parsed).toEqual (Some { Oid = oid; Size = 12345L })
        )

        Vitest.test (
            "accepts a pointer with CRLF line endings",
            fun () ->
                let parsed = parse (version + "\r\n" + $"oid sha256:{oid}\r\n" + "size 0\r\n")
                Vitest.expect(parsed).toEqual (Some { Oid = oid; Size = 0L })
        )

        Vitest.test (
            "accepts extensions in ascending order with git-lfs name characters",
            fun () ->
                let parsed =
                    parse (
                        lines [
                            version
                            $"ext-0-foo sha256:{otherHash}"
                            $"ext-1-Bar_baz-2 sha256:{otherHash}"
                            $"oid sha256:{oid}"
                            "size 9223372036854775807"
                        ]
                    )

                Vitest.expect(parsed).toEqual (Some { Oid = oid; Size = 9223372036854775807L })
        )

        Vitest.test (
            "rejects content longer than 1024 bytes",
            fun () ->
                let pointer = lines [ version; $"oid sha256:{oid}"; "size 1" ]
                let padded = pointer + String.replicate (1025 - pointer.Length) "\n"
                Vitest.expect(padded.Length).toBe 1025
                expectRejected padded
        )

        Vitest.test (
            "rejects mixed line endings and an unterminated final line",
            fun () ->
                expectRejected (version + "\r\n" + $"oid sha256:{oid}\n" + "size 1\r\n")
                expectRejected (version + "\n" + $"oid sha256:{oid}\n" + "size 1")
        )

        Vitest.test (
            "rejects a different version line",
            fun () ->
                expectRejected (lines [ "version https://hawser.github.com/spec/v1"; $"oid sha256:{oid}"; "size 1" ])
                expectRejected (lines [ version + " "; $"oid sha256:{oid}"; "size 1" ])
        )

        Vitest.test (
            "rejects extensions out of order, duplicated or with invalid names",
            fun () ->
                let withExtensions extensions =
                    lines ([ version ] @ extensions @ [ $"oid sha256:{oid}"; "size 1" ])

                expectRejected (withExtensions [ $"ext-1-a sha256:{otherHash}"; $"ext-0-b sha256:{otherHash}" ])
                expectRejected (withExtensions [ $"ext-0-a sha256:{otherHash}"; $"ext-0-b sha256:{otherHash}" ])
                expectRejected (withExtensions [ $"ext-0-a.b sha256:{otherHash}" ])
                expectRejected (withExtensions [ $"ext-x-a sha256:{otherHash}" ])
                expectRejected (withExtensions [ $"ext-0-a sha256:{otherHash.ToUpperInvariant()}" ])
        )

        Vitest.test (
            "rejects an oid that is not 64 lowercase hex characters",
            fun () ->
                expectRejected (lines [ version; $"oid sha256:{oid.ToUpperInvariant()}"; "size 1" ])
                expectRejected (lines [ version; $"oid sha256:{oid.Substring 1}"; "size 1" ])
                expectRejected (lines [ version; $"oid sha1:{oid}"; "size 1" ])
        )

        Vitest.test (
            "rejects sizes with a sign, a leading zero, other characters or beyond int64",
            fun () ->
                for size in [ "+1"; "-1"; "01"; "1.0"; ""; " 1"; "9223372036854775808" ] do
                    expectRejected (lines [ version; $"oid sha256:{oid}"; $"size {size}" ])
        )

        Vitest.test (
            "rejects extra keys, duplicates, blank lines and a wrong key order",
            fun () ->
                expectRejected (lines [ version; $"oid sha256:{oid}"; "size 1"; "extra value" ])
                expectRejected (lines [ version; $"oid sha256:{oid}"; $"oid sha256:{oid}"; "size 1" ])
                expectRejected (lines [ version; $"oid sha256:{oid}"; "size 1"; "size 1" ])
                expectRejected (lines [ version; ""; $"oid sha256:{oid}"; "size 1" ])
                expectRejected (lines [ version; "size 1"; $"oid sha256:{oid}" ])
        )
)
