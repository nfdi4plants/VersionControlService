/// A strict reader for Git LFS pointer files. It accepts only the exact layout git-lfs writes, so a text
/// file that merely looks like a pointer is diffed as text.
module VersionControlService.Git.TextDiff.TextDiffLfsPointer

open System

type LfsPointer = { Oid: string; Size: int64 }

[<Literal>]
let MaximumPointerBytes = 1024

[<Literal>]
let private VersionLine = "version https://git-lfs.github.com/spec/v1"

let private isDigit (character: char) = character >= '0' && character <= '9'

let private isLowerHex (character: char) = isDigit character || (character >= 'a' && character <= 'f')

let private isExtensionNameCharacter (character: char) =
    isDigit character
    || (character >= 'a' && character <= 'z')
    || (character >= 'A' && character <= 'Z')
    || character = '-'
    || character = '_'

let private tryParseSha256 (value: string) =
    let prefix = "sha256:"

    if value.StartsWith(prefix, StringComparison.Ordinal) then
        let hash = value.Substring prefix.Length

        if hash.Length = 64 && hash |> Seq.forall isLowerHex then Some hash else None
    else
        None

/// Parses the extension number and checks the rest of an `ext-<number>-<name> sha256:<hash>` line.
let private tryParseExtension (line: string) : int option =
    let prefix = "ext-"

    if not (line.StartsWith(prefix, StringComparison.Ordinal)) then
        None
    else
        let body = line.Substring prefix.Length
        let numberEnd = body.IndexOf '-'
        let separator = body.IndexOf ' '

        if numberEnd <= 0 || numberEnd > 9 || separator < 0 || separator < numberEnd then
            None
        else
            let number = body.Substring(0, numberEnd)
            let name = body.Substring(numberEnd + 1, separator - numberEnd - 1)
            let value = body.Substring(separator + 1)

            if number |> Seq.forall isDigit
               && name.Length > 0
               && name |> Seq.forall isExtensionNameCharacter
               && (tryParseSha256 value).IsSome then
                Some(Int32.Parse number)
            else
                None

let private tryParseSize (line: string) : int64 option =
    let prefix = "size "

    if not (line.StartsWith(prefix, StringComparison.Ordinal)) then
        None
    else
        let digits = line.Substring prefix.Length

        // Int64.TryParse accepts signs and white space, so the digits and leading zero checks stay.
        if digits.Length > 0 && digits |> Seq.forall isDigit && (digits = "0" || digits[0] <> '0') then
            match Int64.TryParse digits with
            | true, size -> Some size
            | _ -> None
        else
            None

/// Splits terminated lines. Either every line ends with LF, or every line ends with CRLF.
let private trySplitLines (text: string) : string[] option =
    if not (text.EndsWith "\n") then
        None
    else
        let parts = text.Substring(0, text.Length - 1).Split('\n')
        let crlf = parts[0].EndsWith "\r"

        if crlf then
            if parts |> Array.forall (fun part -> part.EndsWith "\r") then
                Some(parts |> Array.map (fun part -> part.Substring(0, part.Length - 1)))
            else
                None
        else
            Some parts

/// Returns the pointer when the bytes are exactly a Git LFS pointer file, and None otherwise.
let tryParseStrict (bytes: byte[]) : LfsPointer option =
    if bytes.Length = 0 || bytes.Length > MaximumPointerBytes || bytes |> Array.exists (fun value -> value >= 128uy) then
        None
    else
        let text = Text.Encoding.UTF8.GetString bytes

        match trySplitLines text with
        | None -> None
        | Some lines when lines.Length < 3 || lines[0] <> VersionLine -> None
        | Some lines ->
            let mutable index = 1
            let mutable previousExtension = -1
            let mutable valid = true

            while valid && index < lines.Length && lines[index].StartsWith("ext-", StringComparison.Ordinal) do
                match tryParseExtension lines[index] with
                | Some number when number > previousExtension ->
                    previousExtension <- number
                    index <- index + 1
                | _ -> valid <- false

            if not valid || index + 2 <> lines.Length then
                None
            else
                let oidLine = lines[index]

                let oid =
                    if oidLine.StartsWith("oid ", StringComparison.Ordinal) then
                        tryParseSha256 (oidLine.Substring 4)
                    else
                        None

                match oid, tryParseSize lines[index + 1] with
                | Some oid, Some size -> Some { Oid = oid; Size = size }
                | _ -> None
