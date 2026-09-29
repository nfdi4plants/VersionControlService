namespace VersionControlService.TextDiff

open VersionControlService.Abstractions

type SourceSpec = {
    Source: IByteSource option
    Encoding: string
    BomLength: int
    ByteLength: int64
}

type SessionConfig = {
    SessionId: string
    ContextLines: int
    PageMaxRows: int
    PageMaxBytes: int
    PageMaxFragments: int
    WindowMaxLines: int
    WindowMaxBytes: int
    CommonChunkBytes: int
    MyersStepsPerGap: int
    HashMaskForTesting: (uint32 * uint32) option
    Limits: Limits
}

type PartSizer = DiffPart -> int

[<RequireQualifiedAccess>]
type EngineResult<'T> =
    | Ok of 'T
    | Failed of code: string * message: string * detail: DiffContentBlocked option
    | Canceled

module SessionConfig =
    let defaults sessionId = {
        SessionId = sessionId
        ContextLines = 3
        PageMaxRows = 1_000
        PageMaxBytes = 512 * 1024
        PageMaxFragments = 32
        WindowMaxLines = 65_536
        WindowMaxBytes = 32 * 1024 * 1024
        CommonChunkBytes = 8 * 1024 * 1024
        MyersStepsPerGap = 1_000_000
        HashMaskForTesting = None
        Limits = Limits.defaults
    }
