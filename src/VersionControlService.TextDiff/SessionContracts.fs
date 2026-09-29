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
    /// A line joins the resync index when its low hash word is a multiple of this power of two.
    ResyncSampleModulus: int
    /// Entries per side in the resync index.
    ResyncIndexCapacity: int
    /// Lines per side that one resync region may scan.
    ResyncScanLines: int
    /// Source bytes per side that one resync region may scan.
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
        ResyncScanLines = 64_000_000
        ResyncScanBytes = 17_179_869_184.0
        ResyncChainLimit = 8
        ResyncProbeLimit = 32
        ResyncConfirmLines = 8
        CheckpointIntervalBytes = 67_108_864.0
        CheckpointResidentBytes = 1_048_576
        JournalCacheBytes = 1_048_576
        Limits = Limits.defaults
    }
