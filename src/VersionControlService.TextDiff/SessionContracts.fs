namespace VersionControlService.TextDiff

open VersionControlService.Abstractions

type SourceSpec = {
    Source: IByteSource option
    Encoding: string
    BomLength: int
    ByteLength: int64
}

type SessionSourceInfo = {
    ByteLength: int64
    LineCount: int64 option
    Encoding: string
    HasBom: bool
}

type SessionConfig = {
    SessionId: string
    ContextLines: int
    PageMaxRows: int
    PageMaxBytes: int
    PageMaxFragments: int
    /// Lines per side that one alignment window may hold. A session accepts at most SessionConfig.MaxWindowLines,
    /// because a larger window needs more alignment scratch than the worker ledger allows.
    WindowMaxLines: int
    WindowMaxBytes: int
    CommonChunkBytes: int
    MyersStepsPerGap: int
    HashMaskForTesting: (uint32 * uint32) option
    /// A line joins the resync index when its low hash word is a multiple of this power of two.
    ResyncSampleModulus: int
    /// Entries per side in the resync index.
    ResyncIndexCapacity: int
    /// Lines per side that one forward search may scan. A search that reaches the limit without a match turns
    /// exactly the scanned range into an unaligned region, and the next search starts with a fresh count.
    /// The default is 1,000,000 lines, so an insertion or deletion of up to about that size realigns.
    ResyncScanLines: int
    /// Source bytes per side that one forward search may scan, with the same effect as ResyncScanLines.
    /// The default is 256 MiB.
    ResyncScanBytes: float
    /// Entries with the same hash that one index keeps.
    ResyncChainLimit: int
    /// Index entries that one line may inspect.
    ResyncProbeLimit: int
    /// Consecutive equal lines that confirm a resync candidate.
    ResyncConfirmLines: int
    /// Source bytes between two checkpoints of one side.
    CheckpointIntervalBytes: float
    /// Bytes of checkpoint records that stay in memory per session.
    CheckpointResidentBytes: int
    /// Bytes of recent journal records that one session keeps in memory.
    JournalCacheBytes: int
    Limits: Limits
}

type PartSizer = DiffPart -> int

[<RequireQualifiedAccess>]
type EngineResult<'T> =
    | Ok of 'T
    | Failed of code: string * message: string * detail: DiffContentBlocked option
    | Canceled

module SessionConfig =
    /// The largest WindowMaxLines that a session accepts. The alignment of a window of this size still fits the
    /// 8 MiB alignment scratch cap of the worker ledger.
    [<Literal>]
    let MaxWindowLines = 131_072

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
        ResyncSampleModulus = 64
        ResyncIndexCapacity = 1_000_000
        ResyncScanLines = 1_000_000
        ResyncScanBytes = 268_435_456.0
        ResyncChainLimit = 8
        ResyncProbeLimit = 32
        ResyncConfirmLines = 8
        CheckpointIntervalBytes = 67_108_864.0
        CheckpointResidentBytes = 1_048_576
        JournalCacheBytes = 1_048_576
        Limits = Limits.defaults
    }
