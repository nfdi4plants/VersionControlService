module VersionControlService.Tests.TextDiffProtocolTests

open System
open System.Text
open Fable.Core
open Vitest
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffProtocol

module Supervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor
module NodeProcess = VersionControlService.Runtime.Node.Process

[<Emit("JSON.parse(JSON.stringify($0))")>]
let private throughJson (_message: obj) : obj = jsNative

[<Emit("JSON.parse($0)")>]
let private parseJson (_text: string) : obj = jsNative

let private path value =
    match RepositoryPath.tryCreate value with
    | Ok created -> created
    | Error message -> failwith message

let private revision value =
    match RevisionId.tryCreate value with
    | Ok created -> created
    | Error message -> failwith message

let private roundTrip (message: TextDiffMessage) = decodeMessage (throughJson (encode message))

let private expectRoundTrip (message: TextDiffMessage) =
    let decoded = roundTrip message

    if decoded <> Ok message then
        failwith $"The message did not survive the round trip: %A{message} became %A{decoded}"

let private expectRejected (raw: string) =
    match decodeMessage (parseJson raw) with
    | Ok decoded -> failwith $"The malformed message was accepted: {raw} became %A{decoded}"
    | Error _ -> ()

let private handle: DiffHandle = { Id = "h-1"; Version = "v-1" }
let private large = 9007199254740993L

let private line number ending (highlights: Highlight[]) : DiffLine = {
    Number = number
    Ending = ending
    Slice = { OffsetUtf16 = large; TotalUtf16 = Some Int64.MaxValue; Text = "a\u00e4\u20ac\U0001D11E"; Highlights = highlights }
}

let private highlights: Highlight[] = [|
    { Start = 0; Length = 1; Kind = HighlightKind.UnchangedText }
    { Start = 1; Length = 3; Kind = HighlightKind.ChangedText }
|]

let private lines = [|
    line 0L LineEnding.NoEnding highlights
    line 1L LineEnding.LF [||]
    line Int64.MaxValue LineEnding.CRLF [||]
    { line 3L LineEnding.CR [||] with Slice = { OffsetUtf16 = 0L; TotalUtf16 = None; Text = ""; Highlights = [||] } }
|]

let private rows: DiffRow[] = [|
    { Id = "r0"; Kind = DiffRowKind.Context; Previous = Some lines[0]; Current = Some lines[1] }
    { Id = "r1"; Kind = DiffRowKind.Added; Previous = None; Current = Some lines[2] }
    { Id = "r2"; Kind = DiffRowKind.Removed; Previous = Some lines[3]; Current = None }
    { Id = "r3"; Kind = DiffRowKind.Replaced; Previous = Some lines[1]; Current = Some lines[2] }
    { Id = "r4"; Kind = DiffRowKind.EndingChanged; Previous = Some lines[1]; Current = Some lines[3] }
|]

let private range start count : LineRange = { Start = start; Count = count }

let private parts: DiffPart[] = [|
    DiffPart.Hunk {
        HunkId = "hunk-1"
        PreviousRange = range 0L 5L
        CurrentRange = range large 5L
        StartsHunk = true
        EndsHunk = false
        Body = HunkBody.AlignedRows rows
    }
    DiffPart.Hunk {
        HunkId = "hunk-2"
        PreviousRange = range 5L 2L
        CurrentRange = range 5L 1L
        StartsHunk = false
        EndsHunk = true
        Body = HunkBody.UnalignedSides(lines, [| lines[0] |])
    }
    DiffPart.HiddenEqual { GapId = "gap-1"; PreviousRange = range 7L Int64.MaxValue; CurrentRange = range 6L large }
    DiffPart.ExpandedContext("gap-1", rows)
|]

let private progress: ScanProgress = { ValidatedBytes = large; TotalBytes = Int64.MaxValue; ScanComplete = false }

let private snippet snippetEnd : PendingSnippet = { Line = large; OffsetUtf16 = 4L; Text = "pending \u00fc"; End = snippetEnd }

let private pendingPreviews: PendingPreview[] = [|
    { Previous = PendingSide.NoActiveLine; Current = PendingSide.Exhausted large; Mismatch = None }
    {
        Previous = PendingSide.Snippet(snippet SnippetEnd.Truncated)
        Current = PendingSide.Snippet(snippet SnippetEnd.MoreTextPending)
        Mismatch = Some { PreviousOffsetUtf16 = 1L; CurrentOffsetUtf16 = large }
    }
    { Previous = PendingSide.Snippet(snippet SnippetEnd.LineEnd); Current = PendingSide.Snippet(snippet SnippetEnd.EndOfFile); Mismatch = None }
|]

let private page: DiffPage = {
    PageId = "page-1"
    NextCursor = Some "cursor-2"
    Parts = parts
    Progress = progress
    OutputComplete = false
    Pending = Some pendingPreviews[1]
}

let private finalPage = { page with NextCursor = None; OutputComplete = true; Pending = None; Parts = [||] }

let private sourceInfo: DiffSourceInfo = {
    Path = path "folder/\u00e4 file.txt"
    Revision = Some(revision "0123456789abcdef0123456789abcdef01234567")
    IsAbsent = false
    ByteLength = large
    LineCount = Some Int64.MaxValue
    Encoding = Some "utf-8"
    EncodingWasChosen = true
    HasBom = true
}

let private absentInfo = {
    sourceInfo with
        Revision = None
        IsAbsent = true
        ByteLength = 0L
        LineCount = None
        Encoding = None
        EncodingWasChosen = false
        HasBom = false
}

let private token: PreparationToken = { Id = "token-1" }

let private blockers = [|
    DiffBlocker.Binary(DiffSide.Previous, "NUL byte at 12")
    DiffBlocker.LocalContentUnavailable(DiffSide.Current, Some "abc")
    DiffBlocker.LocalContentUnavailable(DiffSide.Previous, None)
    DiffBlocker.EncodingRequired(DiffSide.Current, token, [| { Encoding = "windows-1252"; Preview = "caf\u00e9" }; { Encoding = "latin1"; Preview = "" } |])
    DiffBlocker.NotRegularFile DiffSide.Current
    DiffBlocker.ProviderUnsupported
    DiffBlocker.BlobTooLargeForMemory(DiffSide.Previous, large, Int64.MaxValue)
|]

let private openRequest: OpenDiffRequest = {
    Path = path "a.txt"
    PreviousPath = Some(path "old/a.txt")
    Preparation = Some token
    PreviousEncoding = Some "utf-8"
    CurrentEncoding = Some "utf-16le"
    ContextLines = 3
    Continuation = Some "continue-1"
    Storage = DiffStoragePolicy.PreferDisk(large, Int64.MaxValue)
}

let private owner: TextDiffOwner = {
    WorkspaceRoot = "C:\\work\\repo"
    LfsMediaDirectory = "C:\\work\\repo\\.git\\lfs\\objects"
    WindowOwner = "window-1"
}

let private childOwner: Supervisor.ChildOwner = { WorkerId = "worker-0-1"; SessionId = "3"; RequestId = "r7" }

let private failure category =
    OperationFailure.create category "code" "message"

let private categories = [|
    Validation
    NotFound
    Concurrency
    Authentication
    Authorization
    DependencyMissing
    Network
    Timeout
    Canceled
    Conflict
    Unsupported
    ProviderError
|]

let private fullFailure = {
    failure ProviderError with
        Code = TextDiffFailureCodes.ContentNotText
        StateChanged = true
        Retryable = true
        AffectedPaths = [| "a.txt"; "b/\u00e4.txt" |]
        RecoveryAction = Some { Code = "reopen"; Instructions = Some "Open the diff again." }
        Details = [| "detail 1"; "detail 2" |]
        RevisionEvidence = [| "expected_target", revision "abc"; "observed_target", revision "def" |]
        DiffDetail = Some { Side = DiffSide.Current; Evidence = "invalid UTF-8 at byte 9"; InvalidSequenceOffset = Some 2147483657L }
}

Vitest.describe (
    "Text diff protocol",
    fun () ->
        Vitest.test (
            "round-trips control messages",
            fun () ->
                expectRoundTrip (TextDiffMessage.Init("worker-0-1", 1, "C:\\temp\\text-diff\\1-abcdef01\\worker-0-1"))
                expectRoundTrip (TextDiffMessage.InitAck("worker-0-1", 1))
                expectRoundTrip (TextDiffMessage.WorkerFailure "out of memory")
                expectRoundTrip TextDiffMessage.Shutdown
                expectRoundTrip (TextDiffMessage.Cancel("r1", 4))
        )

        Vitest.test (
            "round-trips every request",
            fun () ->
                expectRoundTrip (TextDiffMessage.Request("r1", 1, RequestBody.Open(openRequest, owner)))

                expectRoundTrip (
                    TextDiffMessage.Request(
                        "r2",
                        1,
                        RequestBody.Open(
                            {
                                openRequest with
                                    PreviousPath = None
                                    Preparation = None
                                    PreviousEncoding = None
                                    CurrentEncoding = None
                                    Continuation = None
                                    Storage = DiffStoragePolicy.MemoryOnly large
                            },
                            owner
                        )
                    )
                )

                expectRoundTrip (TextDiffMessage.Request("r3", 2, RequestBody.ReadPage { Handle = handle; Cursor = "cursor"; Background = false }))
                expectRoundTrip (TextDiffMessage.Request("r3", 2, RequestBody.ReadPage { Handle = handle; Cursor = "cursor"; Background = true }))
                expectRoundTrip (TextDiffMessage.Request("r4", 2, RequestBody.ReplayPage { Handle = handle; PageId = "page-1" }))

                for continuation in [ Some "c"; None ] do
                    expectRoundTrip (
                        TextDiffMessage.Request(
                            "r5",
                            2,
                            RequestBody.Expand { Handle = handle; GapId = "gap-1"; FromStart = true; Count = 100; Continuation = continuation }
                        )
                    )

                for side in [ DiffSide.Previous; DiffSide.Current ] do
                    expectRoundTrip (
                        TextDiffMessage.Request(
                            "r6",
                            2,
                            RequestBody.ReadLine {
                                Handle = handle
                                Side = side
                                Line = large
                                OffsetUtf16 = Int64.MaxValue
                                MaxUtf16 = 8192
                                Continuation = Some "c"
                            }
                        )
                    )

                expectRoundTrip (TextDiffMessage.Request("r7", 2, RequestBody.SourceInfo { Handle = handle }))
                expectRoundTrip (TextDiffMessage.Request("r8", 2, RequestBody.Close(Some handle)))
                expectRoundTrip (TextDiffMessage.Request("r9", 2, RequestBody.Close None))
        )

        Vitest.test (
            "round-trips every result payload and nested contract type",
            fun () ->
                let result payload = TextDiffMessage.Result("r1", 3, payload)

                expectRoundTrip (
                    result (ResultPayload.Open(Resumable.Ready(OpenDiffResult.Opened(handle, sourceInfo, absentInfo, Resumable.Ready page, DiffStorage.OnDisk))))
                )

                expectRoundTrip (
                    result (
                        ResultPayload.Open(
                            Resumable.Ready(OpenDiffResult.Opened(handle, sourceInfo, absentInfo, Resumable.Ready page, DiffStorage.InMemory large))
                        )
                    )
                )

                for pending in pendingPreviews do
                    expectRoundTrip (
                        result (
                            ResultPayload.Open(
                                Resumable.Ready(OpenDiffResult.Opened(handle, absentInfo, sourceInfo, Resumable.Scanning(progress, "c", Some pending), DiffStorage.OnDisk))
                            )
                        )
                    )

                expectRoundTrip (result (ResultPayload.Open(Resumable.Scanning(progress, "open-continuation", None))))

                for blocker in blockers do
                    expectRoundTrip (result (ResultPayload.Open(Resumable.Ready(OpenDiffResult.NotDiffable blocker))))

                expectRoundTrip (result (ResultPayload.ReadPage(Resumable.Ready page)))
                expectRoundTrip (result (ResultPayload.ReadPage(Resumable.Ready finalPage)))
                expectRoundTrip (result (ResultPayload.ReadPage(Resumable.Scanning(progress, "cursor-3", Some pendingPreviews[0]))))
                expectRoundTrip (result (ResultPayload.ReplayPage page))
                expectRoundTrip (result (ResultPayload.Expand(Resumable.Ready parts)))
                expectRoundTrip (result (ResultPayload.Expand(Resumable.Ready [||])))
                expectRoundTrip (result (ResultPayload.Expand(Resumable.Scanning(progress, "expand", None))))

                for diffLine in lines do
                    expectRoundTrip (result (ResultPayload.ReadLine(Resumable.Ready diffLine)))

                expectRoundTrip (result (ResultPayload.ReadLine(Resumable.Scanning(progress, "line", None))))
                expectRoundTrip (result (ResultPayload.SourceInfo(sourceInfo, absentInfo)))
                expectRoundTrip (result ResultPayload.Close)
                expectRoundTrip (TextDiffMessage.Progress("r1", 3, large, Int64.MaxValue))
        )

        Vitest.test (
            "round-trips failures with every category and the diff detail",
            fun () ->
                for category in categories do
                    expectRoundTrip (TextDiffMessage.Error("r1", 1, failure category))

                expectRoundTrip (TextDiffMessage.Error("r1", 1, fullFailure))

                expectRoundTrip (
                    TextDiffMessage.Error("r1", 1, { fullFailure with DiffDetail = Some { Side = DiffSide.Previous; Evidence = ""; InvalidSequenceOffset = None } })
                )

                expectRoundTrip (
                    TextDiffMessage.Error("r1", 1, { fullFailure with RecoveryAction = Some { Code = "retry"; Instructions = None } })
                )
        )

        Vitest.test (
            "round-trips Git child messages",
            fun () ->
                expectRoundTrip (TextDiffMessage.SpawnShort(1, childOwner, "C:\\repo", [| "cat-file"; "-s"; "abc" |]))
                expectRoundTrip (TextDiffMessage.SpawnBlob(2, childOwner, "C:\\repo", "abc", "C:\\temp\\blob.spool"))

                let bytes = Encoding.UTF8.GetBytes("line one\n\u00e4\u0000\u00ff")

                expectRoundTrip (
                    TextDiffMessage.SpawnResult(
                        1,
                        SpawnOutcome.Short {
                            Supervisor.ShortResult.ExitCode = Some 0
                            Stdout = bytes
                            Stderr = "warning"
                            Error = None
                        }
                    )
                )

                expectRoundTrip (
                    TextDiffMessage.SpawnResult(
                        1,
                        SpawnOutcome.Short {
                            Supervisor.ShortResult.ExitCode = None
                            Stdout = [||]
                            Stderr = ""
                            Error = Some "stdout limit"
                        }
                    )
                )

                expectRoundTrip (
                    TextDiffMessage.SpawnResult(
                        2,
                        SpawnOutcome.Blob {
                            NodeProcess.ChildExit.ExitCode = None
                            Signal = Some "SIGKILL"
                            Stderr = "fatal"
                            StderrTruncated = true
                            SpawnError = Some "ENOENT"
                        }
                    )
                )

                expectRoundTrip (TextDiffMessage.SpawnResult(3, SpawnOutcome.Failed "not allowed"))
                expectRoundTrip (TextDiffMessage.ReleaseRequest childOwner)
                expectRoundTrip (TextDiffMessage.ReleaseSession("worker-0-1", "3"))
        )

        Vitest.test (
            "keeps int64 values above 2^53 exact",
            fun () ->
                let encoded = throughJson (encode (TextDiffMessage.Progress("r1", 1, large, Int64.MinValue)))

                match decodeMessage encoded with
                | Ok(TextDiffMessage.Progress(_, _, validated, total)) ->
                    Vitest.expect(string validated).toBe "9007199254740993"
                    Vitest.expect((validated = large)).toBe true
                    Vitest.expect((total = Int64.MinValue)).toBe true
                | other -> failwith $"Unexpected decode result %A{other}"
        )

        Vitest.test (
            "rejects malformed messages",
            fun () ->
                Vitest.expect(decodeMessage null |> Result.isError).toBe true
                Vitest.expect(decodeMessage (box "text") |> Result.isError).toBe true
                expectRejected "[]"
                expectRejected """{"t":"shutdown"}"""
                expectRejected """{"v":4,"t":"shutdown"}"""
                expectRejected """{"v":3,"t":"unknown"}"""
                expectRejected """{"v":3,"t":"cancel","requestId":"r1"}"""
                expectRejected """{"v":3,"t":"cancel","requestId":"r1","generation":1.5}"""
                expectRejected """{"v":3,"t":"progress","requestId":"r1","generation":1,"validatedBytes":5,"totalBytes":"5"}"""
                expectRejected """{"v":3,"t":"progress","requestId":"r1","generation":1,"validatedBytes":"9223372036854775808","totalBytes":"5"}"""
                expectRejected """{"v":3,"t":"progress","requestId":"r1","generation":1,"validatedBytes":"1e3","totalBytes":"5"}"""
                expectRejected """{"v":3,"t":"result","requestId":"r1","generation":1,"payload":{"tag":"Missing"}}"""
                expectRejected """{"v":3,"t":"spawnResult","callId":1,"outcome":{"tag":"Short","exitCode":0,"stdout":"not base64!","stderr":"","error":null}}"""

                expectRejected
                    """{"v":3,"t":"open","requestId":"r1","generation":1,"request":{"path":"../escape","previousPath":null,"preparation":null,"previousEncoding":null,"currentEncoding":null,"contextLines":3,"continuation":null,"storage":{"tag":"PreferDisk","minimumFreeBytes":"0","memoryBudgetBytes":"67108864"}},"owner":{"workspaceRoot":"r","lfsMediaDirectory":"m","windowOwner":"k"}}"""
        )

        Vitest.test (
            "measures the envelope in UTF-8 bytes",
            fun () ->
                let reason = "\u00e4\u20ac\U0001D11E"
                let message = encode (TextDiffMessage.WorkerFailure reason)
                let expected = Encoding.UTF8.GetBytes("""{"v":3,"t":"workerFailure","reason":""}""" + reason).Length
                Vitest.expect(envelopeByteLength message).toBe expected
                Vitest.expect(expected).toBe (39 + 9)
        )
)
