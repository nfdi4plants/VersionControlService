namespace VersionControlService.TextDiff.Tests

open Expecto
open System
open System.Text
open VersionControlService.TextDiff

module FnvReferenceTests =
    let private reference (bytes: byte[]) =
        let mutable value = 0xCBF29CE484222325UL
        for item in bytes do
            value <- (value ^^^ uint64 item) * 0x100000001B3UL
        value.ToString("x16")

    let private production (bytes: byte[]) =
        let hash = Hash.create ()
        for item in bytes do Hash.addByte hash item
        Hash.toString hash

    let randomReferenceTests =
        testCase "two-word FNV matches a uint64 reference for deterministic inputs" <| fun () ->
            let random = Random 38_417

            for _ = 1 to 1_000 do
                let data = Array.zeroCreate<byte> (random.Next(0, 2049))
                random.NextBytes data
                Expect.equal (production data) (reference data) "The two-word hash matches uint64 arithmetic."
