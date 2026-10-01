/// Plain-data messages exchanged between the text diff pool and its workers. Every message is a
/// JSON-safe object with the protocol version in `v` and the message type in `t`. Unions travel as
/// `{ tag, ...fields }`, options as null and int64 values as decimal strings.
module VersionControlService.Git.TextDiff.TextDiffProtocol

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions

module NodeProcess = VersionControlService.Runtime.Node.Process
module Supervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor

[<Literal>]
let ProtocolVersion = 1

/// The workspace and window that own a diff session. Handles of one owner are never usable by another.
/// The worker reads LFS objects from the media directory.
type TextDiffOwner = {
    WorkspaceRoot: string
    LfsMediaDirectory: string
    WindowOwner: string
}

/// A request the pool sends to a worker, with the contract record it carries.
[<RequireQualifiedAccess>]
type RequestBody =
    | Open of OpenDiffRequest * TextDiffOwner
    | ReadPage of ReadPageRequest
    | ReplayPage of ReplayPageRequest
    | Expand of ExpandRequest
    | ReadLine of ReadLineRequest
    | SourceInfo of SourceInfoRequest
    | Close of DiffHandle

/// The successful value of a request, tagged by the call that produced it.
[<RequireQualifiedAccess>]
type ResultPayload =
    | Open of Resumable<OpenDiffResult>
    | ReadPage of Resumable<DiffPage>
    | ReplayPage of DiffPage
    | Expand of Resumable<DiffPart[]>
    | ReadLine of Resumable<DiffLine>
    | SourceInfo of DiffSourceInfo * DiffSourceInfo
    | Close

/// The answer to a spawn request from a worker.
[<RequireQualifiedAccess>]
type SpawnOutcome =
    | Short of Supervisor.ShortResult
    | Blob of NodeProcess.ChildExit
    | Failed of message: string

[<RequireQualifiedAccess>]
type TextDiffMessage =
    | Init of workerId: string * epoch: int * tempDirectory: string
    | InitAck of workerId: string * epoch: int
    | WorkerFailure of reason: string
    | Shutdown
    | Cancel of requestId: string * generation: int
    | Request of requestId: string * generation: int * body: RequestBody
    | Progress of requestId: string * generation: int * validatedBytes: int64 * totalBytes: int64
    | Result of requestId: string * generation: int * payload: ResultPayload
    | Error of requestId: string * generation: int * failure: OperationFailure
    | SpawnShort of callId: int * owner: Supervisor.ChildOwner * cwd: string * arguments: string[]
    | SpawnBlob of callId: int * owner: Supervisor.ChildOwner * cwd: string * oid: string * spoolPath: string
    | SpawnResult of callId: int * outcome: SpawnOutcome
    | ReleaseRequest of owner: Supervisor.ChildOwner
    | ReleaseSession of workerId: string * sessionId: string
    | SessionExpired of generation: int

[<Emit("Buffer.byteLength(JSON.stringify($0), 'utf8')")>]
let private jsonByteLength (_message: obj) : int = jsNative

/// UTF-8 byte length of the JSON text of a message.
let envelopeByteLength (message: obj) : int = jsonByteLength message


[<Emit("$0 !== null && typeof $0 === 'object' && !Array.isArray($0)")>]
let private isPlainObject (_value: obj) : bool = jsNative

[<Emit("Array.isArray($0)")>]
let private isArray (_value: obj) : bool = jsNative

[<Emit("typeof $0 === 'string'")>]
let private isString (_value: obj) : bool = jsNative

[<Emit("typeof $0 === 'boolean'")>]
let private isBoolean (_value: obj) : bool = jsNative

[<Emit("typeof $0 === 'number' && Number.isInteger($0) && $0 >= -2147483648 && $0 <= 2147483647")>]
let private isInt32 (_value: obj) : bool = jsNative

[<Emit("$0 == null")>]
let private isNullish (_value: obj) : bool = jsNative

[<Emit("$0[$1]")>]
let private readField (_value: obj) (_name: string) : obj = jsNative

[<Emit("BigInt.asIntN(64, BigInt($0))")>]
let private parseBigInt (_digits: string) : int64 = jsNative

[<Emit("Buffer.from($0).toString('base64')")>]
let private toBase64 (_bytes: byte[]) : string = jsNative

[<Emit("new Uint8Array(Buffer.from($0, 'base64'))")>]
let private fromBase64 (_text: string) : byte[] = jsNative

[<Emit("/^-?(0|[1-9][0-9]*)$/.test($0)")>]
let private isDecimalInteger (_text: string) : bool = jsNative

[<Emit("/^[A-Za-z0-9+/]*={0,2}$/.test($0) && $0.length % 4 === 0")>]
let private isBase64 (_text: string) : bool = jsNative

type private Decoder<'T> = obj -> Result<'T, string>

type private DecodeBuilder() =
    member _.Bind(result: Result<'T, string>, next: 'T -> Result<'U, string>) = Result.bind next result
    member _.Return(value: 'T) : Result<'T, string> = Ok value
    member _.ReturnFrom(result: Result<'T, string>) = result

let private decode = DecodeBuilder()

let private decodeString: Decoder<string> =
    fun value -> if isString value then Ok(unbox<string> value) else Error "expected a string"

let private decodeInt: Decoder<int> =
    fun value -> if isInt32 value then Ok(unbox<int> value) else Error "expected a 32-bit integer"

let private decodeBool: Decoder<bool> =
    fun value -> if isBoolean value then Ok(unbox<bool> value) else Error "expected a boolean"

// The digits go straight to BigInt, so values above 2^53 never pass through a JS number.
let private decodeInt64: Decoder<int64> =
    fun value ->
        if not (isString value) then
            Error "expected an int64 decimal string"
        else
            let text = unbox<string> value

            if not (isDecimalInteger text) || text.Length > 20 then
                Error "expected an int64 decimal string"
            else
                let parsed = parseBigInt text

                if string parsed = text then Ok parsed else Error "int64 value out of range"

let private decodeBase64: Decoder<byte[]> =
    fun value ->
        match decodeString value with
        | Ok text when isBase64 text -> Ok(fromBase64 text)
        | _ -> Error "expected base64 text"

let private decodeArray (item: Decoder<'T>) : Decoder<'T[]> =
    fun value ->
        if not (isArray value) then
            Error "expected an array"
        else
            let values = unbox<obj[]> value
            let results = ResizeArray<'T>(values.Length)
            let mutable failure = None
            let mutable index = 0

            while failure.IsNone && index < values.Length do
                match item values[index] with
                | Ok decoded -> results.Add decoded
                | Error message -> failure <- Some $"[{index}]: {message}"

                index <- index + 1

            match failure with
            | Some message -> Error message
            | None -> Ok(results.ToArray())

let private decodeOption (item: Decoder<'T>) : Decoder<'T option> =
    fun value -> if isNullish value then Ok None else item value |> Result.map Some

let private decodeObject (body: obj -> Result<'T, string>) : Decoder<'T> =
    fun value -> if isPlainObject value then body value else Error "expected an object"

let private field (name: string) (decoder: Decoder<'T>) (value: obj) : Result<'T, string> =
    decoder (readField value name) |> Result.mapError (fun message -> $"{name}: {message}")

let private decodeTagged (cases: string -> (obj -> Result<'T, string>) option) : Decoder<'T> =
    decodeObject (fun value ->
        match field "tag" decodeString value with
        | Error message -> Error message
        | Ok tag ->
            match cases tag with
            | Some body -> body value
            | None -> Error $"unknown tag '{tag}'")


let private encodeInt64 (value: int64) : obj = box (string value)

let private encodeOption (encoder: 'T -> obj) (value: 'T option) : obj =
    match value with
    | Some inner -> encoder inner
    | None -> null

let private encodeArray (encoder: 'T -> obj) (values: 'T[]) : obj = box (values |> Array.map encoder)

let private tagged (tag: string) (fields: (string * obj) list) : obj = createObj (("tag" ==> tag) :: fields)


let private encodeSide (side: DiffSide) =
    match side with
    | DiffSide.Previous -> tagged "Previous" []
    | DiffSide.Current -> tagged "Current" []

let private decodeSide: Decoder<DiffSide> =
    decodeTagged (function
        | "Previous" -> Some(fun _ -> Ok DiffSide.Previous)
        | "Current" -> Some(fun _ -> Ok DiffSide.Current)
        | _ -> None)

let private decodeRepositoryPath: Decoder<RepositoryPath> =
    fun value -> decodeString value |> Result.bind RepositoryPath.tryCreate

let private decodeRevisionId: Decoder<RevisionId> =
    fun value -> decodeString value |> Result.bind RevisionId.tryCreate

let private encodeHandle (handle: DiffHandle) = createObj [ "id" ==> handle.Id; "version" ==> handle.Version ]

let private decodeHandle: Decoder<DiffHandle> =
    decodeObject (fun value -> decode {
        let! id = field "id" decodeString value
        let! version = field "version" decodeString value
        return { DiffHandle.Id = id; Version = version }
    })

let private encodeRange (range: LineRange) =
    createObj [ "start" ==> encodeInt64 range.Start; "count" ==> encodeInt64 range.Count ]

let private decodeRange: Decoder<LineRange> =
    decodeObject (fun value -> decode {
        let! start = field "start" decodeInt64 value
        let! count = field "count" decodeInt64 value
        return { LineRange.Start = start; Count = count }
    })

let private encodeEnding (ending: LineEnding) =
    match ending with
    | LineEnding.NoEnding -> tagged "NoEnding" []
    | LineEnding.LF -> tagged "LF" []
    | LineEnding.CRLF -> tagged "CRLF" []
    | LineEnding.CR -> tagged "CR" []

let private decodeEnding: Decoder<LineEnding> =
    decodeTagged (function
        | "NoEnding" -> Some(fun _ -> Ok LineEnding.NoEnding)
        | "LF" -> Some(fun _ -> Ok LineEnding.LF)
        | "CRLF" -> Some(fun _ -> Ok LineEnding.CRLF)
        | "CR" -> Some(fun _ -> Ok LineEnding.CR)
        | _ -> None)

let private encodeHighlight (highlight: Highlight) =
    let kind =
        match highlight.Kind with
        | HighlightKind.UnchangedText -> tagged "UnchangedText" []
        | HighlightKind.ChangedText -> tagged "ChangedText" []

    createObj [ "start" ==> highlight.Start; "length" ==> highlight.Length; "kind" ==> kind ]

let private decodeHighlightKind: Decoder<HighlightKind> =
    decodeTagged (function
        | "UnchangedText" -> Some(fun _ -> Ok HighlightKind.UnchangedText)
        | "ChangedText" -> Some(fun _ -> Ok HighlightKind.ChangedText)
        | _ -> None)

let private decodeHighlight: Decoder<Highlight> =
    decodeObject (fun value -> decode {
        let! start = field "start" decodeInt value
        let! length = field "length" decodeInt value
        let! kind = field "kind" decodeHighlightKind value
        return { Highlight.Start = start; Length = length; Kind = kind }
    })

let private encodeLine (line: DiffLine) =
    createObj [
        "number" ==> encodeInt64 line.Number
        "ending" ==> encodeEnding line.Ending
        "slice"
        ==> createObj [
            "offsetUtf16" ==> encodeInt64 line.Slice.OffsetUtf16
            "totalUtf16" ==> encodeOption encodeInt64 line.Slice.TotalUtf16
            "text" ==> line.Slice.Text
            "highlights" ==> encodeArray encodeHighlight line.Slice.Highlights
        ]
    ]

let private decodeSlice: Decoder<LineSlice> =
    decodeObject (fun value -> decode {
        let! offset = field "offsetUtf16" decodeInt64 value
        let! total = field "totalUtf16" (decodeOption decodeInt64) value
        let! text = field "text" decodeString value
        let! highlights = field "highlights" (decodeArray decodeHighlight) value
        return { OffsetUtf16 = offset; TotalUtf16 = total; Text = text; Highlights = highlights }
    })

let private decodeLine: Decoder<DiffLine> =
    decodeObject (fun value -> decode {
        let! number = field "number" decodeInt64 value
        let! ending = field "ending" decodeEnding value
        let! slice = field "slice" decodeSlice value
        return { Number = number; Ending = ending; Slice = slice }
    })

let private encodeRowKind (kind: DiffRowKind) =
    match kind with
    | DiffRowKind.Context -> tagged "Context" []
    | DiffRowKind.Added -> tagged "Added" []
    | DiffRowKind.Removed -> tagged "Removed" []
    | DiffRowKind.Replaced -> tagged "Replaced" []
    | DiffRowKind.EndingChanged -> tagged "EndingChanged" []

let private decodeRowKind: Decoder<DiffRowKind> =
    decodeTagged (function
        | "Context" -> Some(fun _ -> Ok DiffRowKind.Context)
        | "Added" -> Some(fun _ -> Ok DiffRowKind.Added)
        | "Removed" -> Some(fun _ -> Ok DiffRowKind.Removed)
        | "Replaced" -> Some(fun _ -> Ok DiffRowKind.Replaced)
        | "EndingChanged" -> Some(fun _ -> Ok DiffRowKind.EndingChanged)
        | _ -> None)

let private encodeRow (row: DiffRow) =
    createObj [
        "id" ==> row.Id
        "kind" ==> encodeRowKind row.Kind
        "previous" ==> encodeOption encodeLine row.Previous
        "current" ==> encodeOption encodeLine row.Current
    ]

let private decodeRow: Decoder<DiffRow> =
    decodeObject (fun value -> decode {
        let! id = field "id" decodeString value
        let! kind = field "kind" decodeRowKind value
        let! previous = field "previous" (decodeOption decodeLine) value
        let! current = field "current" (decodeOption decodeLine) value
        return { Id = id; Kind = kind; Previous = previous; Current = current }
    })

let private encodeHunkBody (body: HunkBody) =
    match body with
    | HunkBody.AlignedRows rows -> tagged "AlignedRows" [ "rows" ==> encodeArray encodeRow rows ]
    | HunkBody.UnalignedSides(previous, current) ->
        tagged "UnalignedSides" [ "previous" ==> encodeArray encodeLine previous; "current" ==> encodeArray encodeLine current ]

let private decodeHunkBody: Decoder<HunkBody> =
    decodeTagged (function
        | "AlignedRows" -> Some(fun value -> field "rows" (decodeArray decodeRow) value |> Result.map HunkBody.AlignedRows)
        | "UnalignedSides" ->
            Some(fun value -> decode {
                let! previous = field "previous" (decodeArray decodeLine) value
                let! current = field "current" (decodeArray decodeLine) value
                return HunkBody.UnalignedSides(previous, current)
            })
        | _ -> None)

let private encodeGap (gap: EqualGap) =
    createObj [
        "gapId" ==> gap.GapId
        "previousRange" ==> encodeRange gap.PreviousRange
        "currentRange" ==> encodeRange gap.CurrentRange
    ]

let private decodeGap: Decoder<EqualGap> =
    decodeObject (fun value -> decode {
        let! gapId = field "gapId" decodeString value
        let! previousRange = field "previousRange" decodeRange value
        let! currentRange = field "currentRange" decodeRange value
        return { GapId = gapId; PreviousRange = previousRange; CurrentRange = currentRange }
    })

let private encodePart (part: DiffPart) =
    match part with
    | DiffPart.Hunk fragment ->
        tagged "Hunk" [
            "fragment"
            ==> createObj [
                "hunkId" ==> fragment.HunkId
                "previousRange" ==> encodeRange fragment.PreviousRange
                "currentRange" ==> encodeRange fragment.CurrentRange
                "startsHunk" ==> fragment.StartsHunk
                "endsHunk" ==> fragment.EndsHunk
                "body" ==> encodeHunkBody fragment.Body
            ]
        ]
    | DiffPart.HiddenEqual gap -> tagged "HiddenEqual" [ "gap" ==> encodeGap gap ]
    | DiffPart.ExpandedContext(gapId, rows) -> tagged "ExpandedContext" [ "gapId" ==> gapId; "rows" ==> encodeArray encodeRow rows ]

let partByteLength (part: DiffPart) = jsonByteLength (encodePart part)

let private decodeFragment: Decoder<HunkFragment> =
    decodeObject (fun value -> decode {
        let! hunkId = field "hunkId" decodeString value
        let! previousRange = field "previousRange" decodeRange value
        let! currentRange = field "currentRange" decodeRange value
        let! startsHunk = field "startsHunk" decodeBool value
        let! endsHunk = field "endsHunk" decodeBool value
        let! body = field "body" decodeHunkBody value

        return {
            HunkId = hunkId
            PreviousRange = previousRange
            CurrentRange = currentRange
            StartsHunk = startsHunk
            EndsHunk = endsHunk
            Body = body
        }
    })

let private decodePart: Decoder<DiffPart> =
    decodeTagged (function
        | "Hunk" -> Some(fun value -> field "fragment" decodeFragment value |> Result.map DiffPart.Hunk)
        | "HiddenEqual" -> Some(fun value -> field "gap" decodeGap value |> Result.map DiffPart.HiddenEqual)
        | "ExpandedContext" ->
            Some(fun value -> decode {
                let! gapId = field "gapId" decodeString value
                let! rows = field "rows" (decodeArray decodeRow) value
                return DiffPart.ExpandedContext(gapId, rows)
            })
        | _ -> None)

let private encodeProgress (progress: ScanProgress) =
    createObj [
        "validatedBytes" ==> encodeInt64 progress.ValidatedBytes
        "totalBytes" ==> encodeInt64 progress.TotalBytes
        "scanComplete" ==> progress.ScanComplete
    ]

let private decodeProgress: Decoder<ScanProgress> =
    decodeObject (fun value -> decode {
        let! validated = field "validatedBytes" decodeInt64 value
        let! total = field "totalBytes" decodeInt64 value
        let! complete = field "scanComplete" decodeBool value
        return { ValidatedBytes = validated; TotalBytes = total; ScanComplete = complete }
    })

let private encodeSnippetEnd (snippetEnd: SnippetEnd) =
    match snippetEnd with
    | SnippetEnd.Truncated -> tagged "Truncated" []
    | SnippetEnd.MoreTextPending -> tagged "MoreTextPending" []
    | SnippetEnd.LineEnd -> tagged "LineEnd" []
    | SnippetEnd.EndOfFile -> tagged "EndOfFile" []

let private decodeSnippetEnd: Decoder<SnippetEnd> =
    decodeTagged (function
        | "Truncated" -> Some(fun _ -> Ok SnippetEnd.Truncated)
        | "MoreTextPending" -> Some(fun _ -> Ok SnippetEnd.MoreTextPending)
        | "LineEnd" -> Some(fun _ -> Ok SnippetEnd.LineEnd)
        | "EndOfFile" -> Some(fun _ -> Ok SnippetEnd.EndOfFile)
        | _ -> None)

let private encodePendingSide (side: PendingSide) =
    match side with
    | PendingSide.NoActiveLine -> tagged "NoActiveLine" []
    | PendingSide.Snippet snippet ->
        tagged "Snippet" [
            "snippet"
            ==> createObj [
                "line" ==> encodeInt64 snippet.Line
                "offsetUtf16" ==> encodeInt64 snippet.OffsetUtf16
                "text" ==> snippet.Text
                "end" ==> encodeSnippetEnd snippet.End
            ]
        ]
    | PendingSide.Exhausted lineCount -> tagged "Exhausted" [ "lineCount" ==> encodeInt64 lineCount ]

let private decodeSnippet: Decoder<PendingSnippet> =
    decodeObject (fun value -> decode {
        let! line = field "line" decodeInt64 value
        let! offset = field "offsetUtf16" decodeInt64 value
        let! text = field "text" decodeString value
        let! snippetEnd = field "end" decodeSnippetEnd value
        return { Line = line; OffsetUtf16 = offset; Text = text; End = snippetEnd }
    })

let private decodePendingSide: Decoder<PendingSide> =
    decodeTagged (function
        | "NoActiveLine" -> Some(fun _ -> Ok PendingSide.NoActiveLine)
        | "Snippet" -> Some(fun value -> field "snippet" decodeSnippet value |> Result.map PendingSide.Snippet)
        | "Exhausted" -> Some(fun value -> field "lineCount" decodeInt64 value |> Result.map PendingSide.Exhausted)
        | _ -> None)

let private encodePending (pending: PendingPreview) =
    createObj [
        "previous" ==> encodePendingSide pending.Previous
        "current" ==> encodePendingSide pending.Current
        "mismatch"
        ==> encodeOption
                (fun (marker: MismatchMarker) ->
                    createObj [
                        "previousOffsetUtf16" ==> encodeInt64 marker.PreviousOffsetUtf16
                        "currentOffsetUtf16" ==> encodeInt64 marker.CurrentOffsetUtf16
                    ])
                pending.Mismatch
    ]

let private decodeMismatch: Decoder<MismatchMarker> =
    decodeObject (fun value -> decode {
        let! previous = field "previousOffsetUtf16" decodeInt64 value
        let! current = field "currentOffsetUtf16" decodeInt64 value
        return { PreviousOffsetUtf16 = previous; CurrentOffsetUtf16 = current }
    })

let private decodePending: Decoder<PendingPreview> =
    decodeObject (fun value -> decode {
        let! previous = field "previous" decodePendingSide value
        let! current = field "current" decodePendingSide value
        let! mismatch = field "mismatch" (decodeOption decodeMismatch) value
        return { Previous = previous; Current = current; Mismatch = mismatch }
    })

let private encodeResumable (encoder: 'T -> obj) (resumable: Resumable<'T>) =
    match resumable with
    | Resumable.Ready value -> tagged "Ready" [ "value" ==> encoder value ]
    | Resumable.Scanning(progress, continuation, pending) ->
        tagged "Scanning" [
            "progress" ==> encodeProgress progress
            "continuation" ==> continuation
            "pending" ==> encodeOption encodePending pending
        ]

let private decodeResumable (decoder: Decoder<'T>) : Decoder<Resumable<'T>> =
    decodeTagged (function
        | "Ready" -> Some(fun value -> field "value" decoder value |> Result.map Resumable.Ready)
        | "Scanning" ->
            Some(fun value -> decode {
                let! progress = field "progress" decodeProgress value
                let! continuation = field "continuation" decodeString value
                let! pending = field "pending" (decodeOption decodePending) value
                return Resumable.Scanning(progress, continuation, pending)
            })
        | _ -> None)

let private encodePage (page: DiffPage) =
    createObj [
        "pageId" ==> page.PageId
        "nextCursor" ==> encodeOption box page.NextCursor
        "parts" ==> encodeArray encodePart page.Parts
        "progress" ==> encodeProgress page.Progress
        "outputComplete" ==> page.OutputComplete
        "pending" ==> encodeOption encodePending page.Pending
    ]

let private decodePage: Decoder<DiffPage> =
    decodeObject (fun value -> decode {
        let! pageId = field "pageId" decodeString value
        let! nextCursor = field "nextCursor" (decodeOption decodeString) value
        let! parts = field "parts" (decodeArray decodePart) value
        let! progress = field "progress" decodeProgress value
        let! outputComplete = field "outputComplete" decodeBool value
        let! pending = field "pending" (decodeOption decodePending) value

        return {
            PageId = pageId
            NextCursor = nextCursor
            Parts = parts
            Progress = progress
            OutputComplete = outputComplete
            Pending = pending
        }
    })

let private encodeSourceInfo (info: DiffSourceInfo) =
    createObj [
        "path" ==> RepositoryPath.value info.Path
        "revision" ==> encodeOption (RevisionId.value >> box) info.Revision
        "isAbsent" ==> info.IsAbsent
        "byteLength" ==> encodeInt64 info.ByteLength
        "lineCount" ==> encodeOption encodeInt64 info.LineCount
        "encoding" ==> encodeOption box info.Encoding
        "encodingWasChosen" ==> info.EncodingWasChosen
        "hasBom" ==> info.HasBom
    ]

let private decodeSourceInfo: Decoder<DiffSourceInfo> =
    decodeObject (fun value -> decode {
        let! path = field "path" decodeRepositoryPath value
        let! revision = field "revision" (decodeOption decodeRevisionId) value
        let! isAbsent = field "isAbsent" decodeBool value
        let! byteLength = field "byteLength" decodeInt64 value
        let! lineCount = field "lineCount" (decodeOption decodeInt64) value
        let! encoding = field "encoding" (decodeOption decodeString) value
        let! encodingWasChosen = field "encodingWasChosen" decodeBool value
        let! hasBom = field "hasBom" decodeBool value

        return {
            Path = path
            Revision = revision
            IsAbsent = isAbsent
            ByteLength = byteLength
            LineCount = lineCount
            Encoding = encoding
            EncodingWasChosen = encodingWasChosen
            HasBom = hasBom
        }
    })

let private encodeToken (token: PreparationToken) = createObj [ "id" ==> token.Id ]

let private decodeToken: Decoder<PreparationToken> =
    decodeObject (fun value -> field "id" decodeString value |> Result.map (fun id -> { PreparationToken.Id = id }))

let private encodeCandidate (candidate: EncodingCandidate) =
    createObj [ "encoding" ==> candidate.Encoding; "preview" ==> candidate.Preview ]

let private decodeCandidate: Decoder<EncodingCandidate> =
    decodeObject (fun value -> decode {
        let! encoding = field "encoding" decodeString value
        let! preview = field "preview" decodeString value
        return { EncodingCandidate.Encoding = encoding; Preview = preview }
    })

let private encodeBlocker (blocker: DiffBlocker) =
    match blocker with
    | DiffBlocker.Binary(side, evidence) -> tagged "Binary" [ "side" ==> encodeSide side; "evidence" ==> evidence ]
    | DiffBlocker.LocalContentUnavailable(side, objectId) ->
        tagged "LocalContentUnavailable" [ "side" ==> encodeSide side; "objectId" ==> encodeOption box objectId ]
    | DiffBlocker.EncodingRequired(side, token, candidates) ->
        tagged "EncodingRequired" [
            "side" ==> encodeSide side
            "token" ==> encodeToken token
            "candidates" ==> encodeArray encodeCandidate candidates
        ]
    | DiffBlocker.NotRegularFile side -> tagged "NotRegularFile" [ "side" ==> encodeSide side ]
    | DiffBlocker.ProviderUnsupported -> tagged "ProviderUnsupported" []

let private decodeBlocker: Decoder<DiffBlocker> =
    decodeTagged (function
        | "Binary" ->
            Some(fun value -> decode {
                let! side = field "side" decodeSide value
                let! evidence = field "evidence" decodeString value
                return DiffBlocker.Binary(side, evidence)
            })
        | "LocalContentUnavailable" ->
            Some(fun value -> decode {
                let! side = field "side" decodeSide value
                let! objectId = field "objectId" (decodeOption decodeString) value
                return DiffBlocker.LocalContentUnavailable(side, objectId)
            })
        | "EncodingRequired" ->
            Some(fun value -> decode {
                let! side = field "side" decodeSide value
                let! token = field "token" decodeToken value
                let! candidates = field "candidates" (decodeArray decodeCandidate) value
                return DiffBlocker.EncodingRequired(side, token, candidates)
            })
        | "NotRegularFile" -> Some(fun value -> field "side" decodeSide value |> Result.map DiffBlocker.NotRegularFile)
        | "ProviderUnsupported" -> Some(fun _ -> Ok DiffBlocker.ProviderUnsupported)
        | _ -> None)

let private encodeOpenResult (result: OpenDiffResult) =
    match result with
    | OpenDiffResult.NotDiffable blocker -> tagged "NotDiffable" [ "blocker" ==> encodeBlocker blocker ]
    | OpenDiffResult.Opened(handle, previous, current, first) ->
        tagged "Opened" [
            "handle" ==> encodeHandle handle
            "previous" ==> encodeSourceInfo previous
            "current" ==> encodeSourceInfo current
            "first" ==> encodeResumable encodePage first
        ]

let private decodeOpenResult: Decoder<OpenDiffResult> =
    decodeTagged (function
        | "NotDiffable" -> Some(fun value -> field "blocker" decodeBlocker value |> Result.map OpenDiffResult.NotDiffable)
        | "Opened" ->
            Some(fun value -> decode {
                let! handle = field "handle" decodeHandle value
                let! previous = field "previous" decodeSourceInfo value
                let! current = field "current" decodeSourceInfo value
                let! first = field "first" (decodeResumable decodePage) value
                return OpenDiffResult.Opened(handle, previous, current, first)
            })
        | _ -> None)


let private categoryName (category: FailureCategory) =
    match category with
    | Validation -> "Validation"
    | NotFound -> "NotFound"
    | Concurrency -> "Concurrency"
    | Authentication -> "Authentication"
    | Authorization -> "Authorization"
    | DependencyMissing -> "DependencyMissing"
    | Network -> "Network"
    | Timeout -> "Timeout"
    | Canceled -> "Canceled"
    | Conflict -> "Conflict"
    | Unsupported -> "Unsupported"
    | ProviderError -> "ProviderError"

let private decodeCategory: Decoder<FailureCategory> =
    decodeTagged (function
        | "Validation" -> Some(fun _ -> Ok Validation)
        | "NotFound" -> Some(fun _ -> Ok NotFound)
        | "Concurrency" -> Some(fun _ -> Ok Concurrency)
        | "Authentication" -> Some(fun _ -> Ok Authentication)
        | "Authorization" -> Some(fun _ -> Ok Authorization)
        | "DependencyMissing" -> Some(fun _ -> Ok DependencyMissing)
        | "Network" -> Some(fun _ -> Ok Network)
        | "Timeout" -> Some(fun _ -> Ok Timeout)
        | "Canceled" -> Some(fun _ -> Ok Canceled)
        | "Conflict" -> Some(fun _ -> Ok Conflict)
        | "Unsupported" -> Some(fun _ -> Ok Unsupported)
        | "ProviderError" -> Some(fun _ -> Ok ProviderError)
        | _ -> None)

let private encodeFailure (failure: OperationFailure) =
    createObj [
        "category" ==> tagged (categoryName failure.Category) []
        "code" ==> failure.Code
        "message" ==> failure.Message
        "stateChanged" ==> failure.StateChanged
        "retryable" ==> failure.Retryable
        "affectedPaths" ==> failure.AffectedPaths
        "recoveryAction"
        ==> encodeOption
                (fun (action: RecoveryAction) ->
                    createObj [ "code" ==> action.Code; "instructions" ==> encodeOption box action.Instructions ])
                failure.RecoveryAction
        "details" ==> failure.Details
        "revisionEvidence"
        ==> encodeArray
                (fun (name: string, revision: RevisionId) -> createObj [ "name" ==> name; "revision" ==> RevisionId.value revision ])
                failure.RevisionEvidence
        "diffDetail"
        ==> encodeOption
                (fun (detail: DiffContentBlocked) -> createObj [ "side" ==> encodeSide detail.Side; "evidence" ==> detail.Evidence ])
                failure.DiffDetail
    ]

let private decodeFailure: Decoder<OperationFailure> =
    decodeObject (fun value -> decode {
        let! category = field "category" decodeCategory value
        let! code = field "code" decodeString value
        let! message = field "message" decodeString value
        let! stateChanged = field "stateChanged" decodeBool value
        let! retryable = field "retryable" decodeBool value
        let! affectedPaths = field "affectedPaths" (decodeArray decodeString) value

        let! recoveryAction =
            field
                "recoveryAction"
                (decodeOption (
                    decodeObject (fun action -> decode {
                        let! actionCode = field "code" decodeString action
                        let! instructions = field "instructions" (decodeOption decodeString) action
                        return { RecoveryAction.Code = actionCode; Instructions = instructions }
                    })
                ))
                value

        let! details = field "details" (decodeArray decodeString) value

        let! revisionEvidence =
            field
                "revisionEvidence"
                (decodeArray (
                    decodeObject (fun evidence -> decode {
                        let! name = field "name" decodeString evidence
                        let! revision = field "revision" decodeRevisionId evidence
                        return name, revision
                    })
                ))
                value

        let! diffDetail =
            field
                "diffDetail"
                (decodeOption (
                    decodeObject (fun detail -> decode {
                        let! side = field "side" decodeSide detail
                        let! evidence = field "evidence" decodeString detail
                        return { DiffContentBlocked.Side = side; Evidence = evidence }
                    })
                ))
                value

        return {
            Category = category
            Code = code
            Message = message
            StateChanged = stateChanged
            Retryable = retryable
            AffectedPaths = affectedPaths
            RecoveryAction = recoveryAction
            Details = details
            RevisionEvidence = revisionEvidence
            DiffDetail = diffDetail
        }
    })


let private encodeOpenRequest (request: OpenDiffRequest) =
    createObj [
        "path" ==> RepositoryPath.value request.Path
        "previousPath" ==> encodeOption (RepositoryPath.value >> box) request.PreviousPath
        "preparation" ==> encodeOption encodeToken request.Preparation
        "previousEncoding" ==> encodeOption box request.PreviousEncoding
        "currentEncoding" ==> encodeOption box request.CurrentEncoding
        "contextLines" ==> request.ContextLines
        "continuation" ==> encodeOption box request.Continuation
    ]

let private decodeOpenRequest: Decoder<OpenDiffRequest> =
    decodeObject (fun value -> decode {
        let! path = field "path" decodeRepositoryPath value
        let! previousPath = field "previousPath" (decodeOption decodeRepositoryPath) value
        let! preparation = field "preparation" (decodeOption decodeToken) value
        let! previousEncoding = field "previousEncoding" (decodeOption decodeString) value
        let! currentEncoding = field "currentEncoding" (decodeOption decodeString) value
        let! contextLines = field "contextLines" decodeInt value
        let! continuation = field "continuation" (decodeOption decodeString) value

        return {
            Path = path
            PreviousPath = previousPath
            Preparation = preparation
            PreviousEncoding = previousEncoding
            CurrentEncoding = currentEncoding
            ContextLines = contextLines
            Continuation = continuation
        }
    })

let private encodeOwner (owner: TextDiffOwner) =
    createObj [
        "workspaceRoot" ==> owner.WorkspaceRoot
        "lfsMediaDirectory" ==> owner.LfsMediaDirectory
        "windowOwner" ==> owner.WindowOwner
    ]

let private decodeOwner: Decoder<TextDiffOwner> =
    decodeObject (fun value -> decode {
        let! workspaceRoot = field "workspaceRoot" decodeString value
        let! lfsMediaDirectory = field "lfsMediaDirectory" decodeString value
        let! windowOwner = field "windowOwner" decodeString value

        return {
            WorkspaceRoot = workspaceRoot
            LfsMediaDirectory = lfsMediaDirectory
            WindowOwner = windowOwner
        }
    })

let private encodeRequestBody (body: RequestBody) : string * DiffHandle option * (string * obj) list =
    match body with
    | RequestBody.Open(request, owner) -> "open", None, [ "request" ==> encodeOpenRequest request; "owner" ==> encodeOwner owner ]
    | RequestBody.ReadPage request ->
        "readPage", Some request.Handle, [ "request" ==> createObj [ "handle" ==> encodeHandle request.Handle; "cursor" ==> request.Cursor ] ]
    | RequestBody.ReplayPage request ->
        "replayPage", Some request.Handle, [ "request" ==> createObj [ "handle" ==> encodeHandle request.Handle; "pageId" ==> request.PageId ] ]
    | RequestBody.Expand request ->
        "expand",
        Some request.Handle,
        [
            "request"
            ==> createObj [
                "handle" ==> encodeHandle request.Handle
                "gapId" ==> request.GapId
                "fromStart" ==> request.FromStart
                "count" ==> request.Count
                "continuation" ==> encodeOption box request.Continuation
            ]
        ]
    | RequestBody.ReadLine request ->
        "readLine",
        Some request.Handle,
        [
            "request"
            ==> createObj [
                "handle" ==> encodeHandle request.Handle
                "side" ==> encodeSide request.Side
                "line" ==> encodeInt64 request.Line
                "offsetUtf16" ==> encodeInt64 request.OffsetUtf16
                "maxUtf16" ==> request.MaxUtf16
                "continuation" ==> encodeOption box request.Continuation
            ]
        ]
    | RequestBody.SourceInfo request -> "sourceInfo", Some request.Handle, [ "request" ==> createObj [ "handle" ==> encodeHandle request.Handle ] ]
    | RequestBody.Close handle -> "close", Some handle, []

let private requestField (value: obj) (body: Decoder<'T>) = field "request" (decodeObject body) value

let private decodeRequestBody (messageType: string) (value: obj) : Result<RequestBody, string> option =
    let sameHandle (handle: DiffHandle) (request: 'T) (build: 'T -> RequestBody) =
        decode {
            let! envelopeHandle = field "handle" decodeHandle value

            if envelopeHandle = handle then
                return build request
            else
                return! Error "handle: does not match the request handle"
        }

    match messageType with
    | "open" ->
        Some(decode {
            let! request = field "request" decodeOpenRequest value
            let! owner = field "owner" decodeOwner value
            return RequestBody.Open(request, owner)
        })
    | "readPage" ->
        Some(decode {
            let! request =
                requestField value (fun request -> decode {
                    let! handle = field "handle" decodeHandle request
                    let! cursor = field "cursor" decodeString request
                    return { ReadPageRequest.Handle = handle; Cursor = cursor }
                })

            return! sameHandle request.Handle request RequestBody.ReadPage
        })
    | "replayPage" ->
        Some(decode {
            let! request =
                requestField value (fun request -> decode {
                    let! handle = field "handle" decodeHandle request
                    let! pageId = field "pageId" decodeString request
                    return { ReplayPageRequest.Handle = handle; PageId = pageId }
                })

            return! sameHandle request.Handle request RequestBody.ReplayPage
        })
    | "expand" ->
        Some(decode {
            let! request =
                requestField value (fun request -> decode {
                    let! handle = field "handle" decodeHandle request
                    let! gapId = field "gapId" decodeString request
                    let! fromStart = field "fromStart" decodeBool request
                    let! count = field "count" decodeInt request
                    let! continuation = field "continuation" (decodeOption decodeString) request

                    return {
                        ExpandRequest.Handle = handle
                        GapId = gapId
                        FromStart = fromStart
                        Count = count
                        Continuation = continuation
                    }
                })

            return! sameHandle request.Handle request RequestBody.Expand
        })
    | "readLine" ->
        Some(decode {
            let! request =
                requestField value (fun request -> decode {
                    let! handle = field "handle" decodeHandle request
                    let! side = field "side" decodeSide request
                    let! line = field "line" decodeInt64 request
                    let! offset = field "offsetUtf16" decodeInt64 request
                    let! maxUtf16 = field "maxUtf16" decodeInt request
                    let! continuation = field "continuation" (decodeOption decodeString) request

                    return {
                        ReadLineRequest.Handle = handle
                        Side = side
                        Line = line
                        OffsetUtf16 = offset
                        MaxUtf16 = maxUtf16
                        Continuation = continuation
                    }
                })

            return! sameHandle request.Handle request RequestBody.ReadLine
        })
    | "sourceInfo" ->
        Some(decode {
            let! request = requestField value (fun request -> field "handle" decodeHandle request |> Result.map (fun handle -> { SourceInfoRequest.Handle = handle }))
            return! sameHandle request.Handle request RequestBody.SourceInfo
        })
    | "close" -> Some(field "handle" decodeHandle value |> Result.map RequestBody.Close)
    | _ -> None


let private encodePayload (payload: ResultPayload) =
    match payload with
    | ResultPayload.Open result -> tagged "Open" [ "value" ==> encodeResumable encodeOpenResult result ]
    | ResultPayload.ReadPage page -> tagged "ReadPage" [ "value" ==> encodeResumable encodePage page ]
    | ResultPayload.ReplayPage page -> tagged "ReplayPage" [ "value" ==> encodePage page ]
    | ResultPayload.Expand parts -> tagged "Expand" [ "value" ==> encodeResumable (encodeArray encodePart) parts ]
    | ResultPayload.ReadLine line -> tagged "ReadLine" [ "value" ==> encodeResumable encodeLine line ]
    | ResultPayload.SourceInfo(previous, current) ->
        tagged "SourceInfo" [ "previous" ==> encodeSourceInfo previous; "current" ==> encodeSourceInfo current ]
    | ResultPayload.Close -> tagged "Close" []

let private decodePayload: Decoder<ResultPayload> =
    decodeTagged (function
        | "Open" -> Some(fun value -> field "value" (decodeResumable decodeOpenResult) value |> Result.map ResultPayload.Open)
        | "ReadPage" -> Some(fun value -> field "value" (decodeResumable decodePage) value |> Result.map ResultPayload.ReadPage)
        | "ReplayPage" -> Some(fun value -> field "value" decodePage value |> Result.map ResultPayload.ReplayPage)
        | "Expand" -> Some(fun value -> field "value" (decodeResumable (decodeArray decodePart)) value |> Result.map ResultPayload.Expand)
        | "ReadLine" -> Some(fun value -> field "value" (decodeResumable decodeLine) value |> Result.map ResultPayload.ReadLine)
        | "SourceInfo" ->
            Some(fun value -> decode {
                let! previous = field "previous" decodeSourceInfo value
                let! current = field "current" decodeSourceInfo value
                return ResultPayload.SourceInfo(previous, current)
            })
        | "Close" -> Some(fun _ -> Ok ResultPayload.Close)
        | _ -> None)


let private encodeChildOwner (owner: Supervisor.ChildOwner) =
    createObj [ "workerId" ==> owner.WorkerId; "sessionId" ==> owner.SessionId; "requestId" ==> owner.RequestId ]

let private decodeChildOwner: Decoder<Supervisor.ChildOwner> =
    decodeObject (fun value -> decode {
        let! workerId = field "workerId" decodeString value
        let! sessionId = field "sessionId" decodeString value
        let! requestId = field "requestId" decodeString value
        return { Supervisor.ChildOwner.WorkerId = workerId; SessionId = sessionId; RequestId = requestId }
    })

let private encodeOutcome (outcome: SpawnOutcome) =
    match outcome with
    | SpawnOutcome.Short result ->
        tagged "Short" [
            "exitCode" ==> encodeOption box result.ExitCode
            "stdout" ==> toBase64 result.Stdout
            "stderr" ==> result.Stderr
            "error" ==> encodeOption box result.Error
        ]
    | SpawnOutcome.Blob exit ->
        tagged "Blob" [
            "exitCode" ==> encodeOption box exit.ExitCode
            "signal" ==> encodeOption box exit.Signal
            "stderr" ==> exit.Stderr
            "stderrTruncated" ==> exit.StderrTruncated
            "spawnError" ==> encodeOption box exit.SpawnError
        ]
    | SpawnOutcome.Failed message -> tagged "Failed" [ "message" ==> message ]

let private decodeOutcome: Decoder<SpawnOutcome> =
    decodeTagged (function
        | "Short" ->
            Some(fun value -> decode {
                let! exitCode = field "exitCode" (decodeOption decodeInt) value
                let! stdout = field "stdout" decodeBase64 value
                let! stderr = field "stderr" decodeString value
                let! error = field "error" (decodeOption decodeString) value

                return
                    SpawnOutcome.Short {
                        Supervisor.ShortResult.ExitCode = exitCode
                        Stdout = stdout
                        Stderr = stderr
                        Error = error
                    }
            })
        | "Blob" ->
            Some(fun value -> decode {
                let! exitCode = field "exitCode" (decodeOption decodeInt) value
                let! signal = field "signal" (decodeOption decodeString) value
                let! stderr = field "stderr" decodeString value
                let! stderrTruncated = field "stderrTruncated" decodeBool value
                let! spawnError = field "spawnError" (decodeOption decodeString) value

                return
                    SpawnOutcome.Blob {
                        NodeProcess.ChildExit.ExitCode = exitCode
                        Signal = signal
                        Stderr = stderr
                        StderrTruncated = stderrTruncated
                        SpawnError = spawnError
                    }
            })
        | "Failed" -> Some(fun value -> field "message" decodeString value |> Result.map SpawnOutcome.Failed)
        | _ -> None)


let private envelope (messageType: string) (fields: (string * obj) list) : obj =
    createObj ([ "v" ==> ProtocolVersion; "t" ==> messageType ] @ fields)

let encode (message: TextDiffMessage) : obj =
    match message with
    | TextDiffMessage.Init(workerId, epoch, tempDirectory) ->
        envelope "init" [ "workerId" ==> workerId; "epoch" ==> epoch; "tempDirectory" ==> tempDirectory ]
    | TextDiffMessage.InitAck(workerId, epoch) -> envelope "initAck" [ "workerId" ==> workerId; "epoch" ==> epoch ]
    | TextDiffMessage.WorkerFailure reason -> envelope "workerFailure" [ "reason" ==> reason ]
    | TextDiffMessage.Shutdown -> envelope "shutdown" []
    | TextDiffMessage.Cancel(requestId, generation) -> envelope "cancel" [ "requestId" ==> requestId; "generation" ==> generation ]
    | TextDiffMessage.Request(requestId, generation, body) ->
        let messageType, handle, fields = encodeRequestBody body

        let handleField =
            match handle with
            | Some value -> [ "handle" ==> encodeHandle value ]
            | None -> []

        envelope messageType ([ "requestId" ==> requestId; "generation" ==> generation ] @ handleField @ fields)
    | TextDiffMessage.Progress(requestId, generation, validated, total) ->
        envelope "progress" [
            "requestId" ==> requestId
            "generation" ==> generation
            "validatedBytes" ==> encodeInt64 validated
            "totalBytes" ==> encodeInt64 total
        ]
    | TextDiffMessage.Result(requestId, generation, payload) ->
        envelope "result" [ "requestId" ==> requestId; "generation" ==> generation; "payload" ==> encodePayload payload ]
    | TextDiffMessage.Error(requestId, generation, failure) ->
        envelope "error" [ "requestId" ==> requestId; "generation" ==> generation; "failure" ==> encodeFailure failure ]
    | TextDiffMessage.SpawnShort(callId, owner, cwd, arguments) ->
        envelope "spawnShort" [ "callId" ==> callId; "owner" ==> encodeChildOwner owner; "cwd" ==> cwd; "args" ==> arguments ]
    | TextDiffMessage.SpawnBlob(callId, owner, cwd, oid, spoolPath) ->
        envelope "spawnBlob" [
            "callId" ==> callId
            "owner" ==> encodeChildOwner owner
            "cwd" ==> cwd
            "oid" ==> oid
            "spoolPath" ==> spoolPath
        ]
    | TextDiffMessage.SpawnResult(callId, outcome) -> envelope "spawnResult" [ "callId" ==> callId; "outcome" ==> encodeOutcome outcome ]
    | TextDiffMessage.ReleaseRequest owner -> envelope "releaseRequest" [ "owner" ==> encodeChildOwner owner ]
    | TextDiffMessage.ReleaseSession(workerId, sessionId) ->
        envelope "releaseSession" [ "workerId" ==> workerId; "sessionId" ==> sessionId ]
    | TextDiffMessage.SessionExpired generation -> envelope "sessionExpired" [ "generation" ==> generation ]

let private decodeEnvelope (messageType: string) (value: obj) : Result<TextDiffMessage, string> =
    let requestKey () = decode {
        let! requestId = field "requestId" decodeString value
        let! generation = field "generation" decodeInt value
        return requestId, generation
    }

    match messageType with
    | "init" ->
        decode {
            let! workerId = field "workerId" decodeString value
            let! epoch = field "epoch" decodeInt value
            let! tempDirectory = field "tempDirectory" decodeString value
            return TextDiffMessage.Init(workerId, epoch, tempDirectory)
        }
    | "initAck" ->
        decode {
            let! workerId = field "workerId" decodeString value
            let! epoch = field "epoch" decodeInt value
            return TextDiffMessage.InitAck(workerId, epoch)
        }
    | "workerFailure" -> field "reason" decodeString value |> Result.map TextDiffMessage.WorkerFailure
    | "shutdown" -> Ok TextDiffMessage.Shutdown
    | "cancel" -> requestKey () |> Result.map TextDiffMessage.Cancel
    | "progress" ->
        decode {
            let! requestId, generation = requestKey ()
            let! validated = field "validatedBytes" decodeInt64 value
            let! total = field "totalBytes" decodeInt64 value
            return TextDiffMessage.Progress(requestId, generation, validated, total)
        }
    | "result" ->
        decode {
            let! requestId, generation = requestKey ()
            let! payload = field "payload" decodePayload value
            return TextDiffMessage.Result(requestId, generation, payload)
        }
    | "error" ->
        decode {
            let! requestId, generation = requestKey ()
            let! failure = field "failure" decodeFailure value
            return TextDiffMessage.Error(requestId, generation, failure)
        }
    | "spawnShort" ->
        decode {
            let! callId = field "callId" decodeInt value
            let! owner = field "owner" decodeChildOwner value
            let! cwd = field "cwd" decodeString value
            let! arguments = field "args" (decodeArray decodeString) value
            return TextDiffMessage.SpawnShort(callId, owner, cwd, arguments)
        }
    | "spawnBlob" ->
        decode {
            let! callId = field "callId" decodeInt value
            let! owner = field "owner" decodeChildOwner value
            let! cwd = field "cwd" decodeString value
            let! oid = field "oid" decodeString value
            let! spoolPath = field "spoolPath" decodeString value
            return TextDiffMessage.SpawnBlob(callId, owner, cwd, oid, spoolPath)
        }
    | "spawnResult" ->
        decode {
            let! callId = field "callId" decodeInt value
            let! outcome = field "outcome" decodeOutcome value
            return TextDiffMessage.SpawnResult(callId, outcome)
        }
    | "releaseRequest" -> field "owner" decodeChildOwner value |> Result.map TextDiffMessage.ReleaseRequest
    | "releaseSession" ->
        decode {
            let! workerId = field "workerId" decodeString value
            let! sessionId = field "sessionId" decodeString value
            return TextDiffMessage.ReleaseSession(workerId, sessionId)
        }
    | "sessionExpired" -> field "generation" decodeInt value |> Result.map TextDiffMessage.SessionExpired
    | other ->
        match decodeRequestBody other value with
        | Some body ->
            decode {
                let! requestId, generation = requestKey ()
                let! decodedBody = body
                return TextDiffMessage.Request(requestId, generation, decodedBody)
            }
        | None -> Error $"t: unknown message type '{other}'"

/// Decodes a message. Every malformed input returns Error with a description of the first problem found.
let decodeMessage (value: obj) : Result<TextDiffMessage, string> =
    if not (isPlainObject value) then
        Error "The message is not an object."
    else
        match field "v" decodeInt value with
        | Error message -> Error message
        | Ok version when version <> ProtocolVersion -> Error $"v: unsupported protocol version {version}"
        | Ok _ ->
            match field "t" decodeString value with
            | Error message -> Error message
            | Ok messageType -> decodeEnvelope messageType value

/// Reads the request id and generation of a message that failed to decode, when both are present and valid.
let tryRequestKey (value: obj) : (string * int) option =
    if isPlainObject value then
        match field "requestId" decodeString value, field "generation" decodeInt value with
        | Ok requestId, Ok generation -> Some(requestId, generation)
        | Ok requestId, Error _ -> Some(requestId, 0)
        | _ -> None
    else
        None
