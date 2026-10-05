namespace VersionControlService.Abstractions

/// Opaque handle for a diff session pinned to its source identities.
type DiffHandle = { Id: string; Version: string }

/// A zero-based range of source lines.
type LineRange = { Start: int64; Count: int64 }

/// The line terminator found after a line.
[<RequireQualifiedAccess>]
type LineEnding =
    | NoEnding
    | LF
    | CRLF
    | CR

/// Describes whether a highlighted span is unchanged or changed text.
[<RequireQualifiedAccess>]
type HighlightKind =
    | UnchangedText
    | ChangedText

/// A UTF-16 span inside the text of a line slice.
type Highlight = { Start: int; Length: int; Kind: HighlightKind }

/// A UTF-16 slice of one line. TotalUtf16 remains absent until the line length is known.
type LineSlice = { OffsetUtf16: int64; TotalUtf16: int64 option; Text: string; Highlights: Highlight[] }

/// A whole source line with the currently available slice of its text.
type DiffLine = { Number: int64; Ending: LineEnding; Slice: LineSlice }

/// Describes the relationship represented by a diff row.
[<RequireQualifiedAccess>]
type DiffRowKind =
    | Context
    | Added
    | Removed
    | Replaced
    | EndingChanged

/// One diff row with a stable identifier and optional lines from each side.
type DiffRow = { Id: string; Kind: DiffRowKind; Previous: DiffLine option; Current: DiffLine option }

/// Aligned rows or separate line sequences when alignment is unavailable.
[<RequireQualifiedAccess>]
type HunkBody =
    | AlignedRows of DiffRow[]
    | UnalignedSides of previous: DiffLine[] * current: DiffLine[]

/// A hunk fragment with source ranges and markers for its boundaries.
type HunkFragment = {
    HunkId: string
    PreviousRange: LineRange
    CurrentRange: LineRange
    StartsHunk: bool
    EndsHunk: bool
    Body: HunkBody
}

/// A hidden range whose lines and endings have been verified equal.
type EqualGap = { GapId: string; PreviousRange: LineRange; CurrentRange: LineRange }

/// One visible component of a diff page, including verified equal context.
[<RequireQualifiedAccess>]
type DiffPart =
    | Hunk of HunkFragment
    | HiddenEqual of EqualGap
    | ExpandedContext of gapId: string * DiffRow[]

/// Unique validated byte coverage across both sources and the scan completion state.
type ScanProgress = { ValidatedBytes: int64; TotalBytes: int64; ScanComplete: bool }

/// Describes what follows the final character in a pending snippet.
[<RequireQualifiedAccess>]
type SnippetEnd =
    | Truncated
    | MoreTextPending
    | LineEnd
    | EndOfFile

/// The available portion of one line while scanning continues.
type PendingSnippet = { Line: int64; OffsetUtf16: int64; Text: string; End: SnippetEnd }

/// State for one side at the pending scan position, including proven exhaustion.
[<RequireQualifiedAccess>]
type PendingSide =
    | NoActiveLine
    | Snippet of PendingSnippet
    | Exhausted of lineCount: int64

/// UTF-16 offsets of an advisory mismatch between the two sources.
type MismatchMarker = { PreviousOffsetUtf16: int64; CurrentOffsetUtf16: int64 }

/// Advisory snippets for lines still being scanned. They never form a diff row.
type PendingPreview = { Previous: PendingSide; Current: PendingSide; Mismatch: MismatchMarker option }

/// A result that is ready or has a continuation while scanning proceeds.
[<RequireQualifiedAccess>]
type Resumable<'T> =
    | Ready of 'T
    | Scanning of ScanProgress * continuation: string * pending: PendingPreview option

/// A page of diff parts with its cursor and progress. A pending preview appears only when another page is available.
type DiffPage = {
    PageId: string
    NextCursor: string option
    Parts: DiffPart[]
    Progress: ScanProgress
    OutputComplete: bool
    Pending: PendingPreview option
}

/// Text metadata for one source in an opened diff.
type DiffSourceInfo = {
    Path: RepositoryPath
    Revision: RevisionId option
    IsAbsent: bool
    ByteLength: int64
    LineCount: int64 option
    Encoding: string option
    EncodingWasChosen: bool
    HasBom: bool
}

/// An encoding candidate with a preview of at most 2 KiB of decoded text.
type EncodingCandidate = { Encoding: string; Preview: string }

/// Binds the selected path, optional previous path, pinned commit, and source identities.
type PreparationToken = { Id: string }

/// Input for opening a pinned diff session with optional encoding choices and continuation.
type OpenDiffRequest = {
    Path: RepositoryPath
    PreviousPath: RepositoryPath option
    Preparation: PreparationToken option
    PreviousEncoding: string option
    CurrentEncoding: string option
    ContextLines: int
    Continuation: string option
}

/// Reason the provider cannot produce a diff for the requested sources.
[<RequireQualifiedAccess>]
type DiffBlocker =
    | Binary of DiffSide * evidence: string
    | LocalContentUnavailable of DiffSide * objectId: string option
    | EncodingRequired of DiffSide * PreparationToken * EncodingCandidate[]
    | NotRegularFile of DiffSide
    | ProviderUnsupported

/// The result of opening a diff, with either a blocker or a new handle and initial page.
[<RequireQualifiedAccess>]
type OpenDiffResult =
    | NotDiffable of DiffBlocker
    | Opened of DiffHandle * previous: DiffSourceInfo * current: DiffSourceInfo * first: Resumable<DiffPage>

/// Request the page that follows a cursor in an opened diff.
type ReadPageRequest = { Handle: DiffHandle; Cursor: string }

/// Request a previously returned page again by its identifier.
type ReplayPageRequest = { Handle: DiffHandle; PageId: string }

/// Request part of a verified equal range. The service clamps Count to 100 lines.
type ExpandRequest = {
    Handle: DiffHandle
    GapId: string
    FromStart: bool
    Count: int
    Continuation: string option
}

/// Request a UTF-16 slice of one whole source line. The service clamps MaxUtf16 to 8,192.
type ReadLineRequest = {
    Handle: DiffHandle
    Side: DiffSide
    Line: int64
    OffsetUtf16: int64
    MaxUtf16: int
    Continuation: string option
}

/// Request the pinned metadata for both sources in an opened diff.
type SourceInfoRequest = { Handle: DiffHandle }

/// Reads and expands paged diffs with resumable work.
/// A handle belongs to the workspace and to the owner that the hosting provider derives from the calling
/// operation (for example, the window that made the call). A call with a handle of another owner answers
/// `TextDiffFailureCodes.SessionClosed` ("diff_session_closed"), and `Close` of another owner's handle changes nothing.
type TextDiffService = {
    Open: OpenDiffRequest -> OperationContext -> Async<OperationResult<Resumable<OpenDiffResult>>>
    ReadPage: ReadPageRequest -> OperationContext -> Async<OperationResult<Resumable<DiffPage>>>
    ReplayPage: ReplayPageRequest -> OperationContext -> Async<OperationResult<DiffPage>>
    Expand: ExpandRequest -> OperationContext -> Async<OperationResult<Resumable<DiffPart[]>>>
    ReadLine: ReadLineRequest -> OperationContext -> Async<OperationResult<Resumable<DiffLine>>>
    GetSourceInfo: SourceInfoRequest -> OperationContext -> Async<OperationResult<DiffSourceInfo * DiffSourceInfo>>
    Close: DiffHandle -> OperationContext -> Async<OperationResult<unit>>
}

/// Stable failure codes returned by paged text diff operations.
module TextDiffFailureCodes =

    /// The selected source content could not be read as text.
    [<Literal>]
    let ContentNotText = "diff_content_not_text"

    /// The bytes of a side that classification read as UTF-8 contain a sequence that is invalid in UTF-8 but
    /// valid in Windows-1252, and the caller did not choose the encoding. `OperationFailure.DiffDetail` names
    /// the side and the evidence. When `Open` finds the sequence before the first page, it returns
    /// `DiffBlocker.EncodingRequired` for that side.
    [<Literal>]
    let EncodingMismatch = "diff_encoding_mismatch"

    /// A source could not be read because of a file system or process error, for example a working file that
    /// cannot be opened or a Git child that cannot be started. Any call that reads a source can return it.
    [<Literal>]
    let ReadFailed = "diff_read_failed"

    /// `Open` received an encoding name in `PreviousEncoding` or `CurrentEncoding` that the engine does not know.
    [<Literal>]
    let UnsupportedEncoding = "unsupported_encoding"

    /// A pinned source identity changed while its diff was open.
    [<Literal>]
    let SourceChanged = "source_changed"

    /// The diff handle has already been closed.
    [<Literal>]
    let SessionClosed = "diff_session_closed"

    /// The worker could not complete a diff operation.
    [<Literal>]
    let WorkerFailed = "diff_worker_failed"

    /// The preparation token does not match the requested sources.
    [<Literal>]
    let PreparationMismatch = "preparation_mismatch"

    /// The continuation was produced for a request with different fields.
    [<Literal>]
    let ContinuationMismatch = "continuation_mismatch"

/// Materialization state of one lazily-hydrated object.
type ObjectState = {
    Path: RepositoryPath
    IsMaterialized: bool
    /// Whether the object bytes are available in provider-local storage.
    IsLocallyAvailable: bool
    SizeBytes: float option
    ObjectId: string option
}

/// Optional object-materialization extension (Git LFS hydration, lakeFS object download).
type ObjectMaterializationService = {
    ListObjects: OperationContext -> Async<OperationResult<ObjectState[]>>
    Materialize: RepositoryPath -> OperationContext -> Async<OperationResult<unit>>
    /// When Dematerialize replaces a materialized file with the provider's reference, it also removes the
    /// provider-local copy of the object. A provider does this only after checking that its remote has the object,
    /// and it keeps the copy when the local storage is shared with other workspaces.
    Dematerialize: RepositoryPath -> OperationContext -> Async<OperationResult<unit>>
}

type StoragePolicySettings = {
    /// Threshold in megabytes above which new objects use large-object storage, when supported.
    AutoPolicyThresholdMb: int option
    /// Whether large objects are materialized during clone/update by default.
    MaterializeLargeObjects: bool
}

/// Optional large-object storage-policy extension over literal repository paths.
type StoragePolicyService = {
    /// True marks the path as a large object. False records an explicit opt-out that
    /// outranks the automatic policy of later revisions until the path is marked again.
    /// A provider that changes the policy reports the path in AffectedPaths, and the next
    /// revision that includes the path stores it under the new policy.
    SetPathPolicy: RepositoryPath -> bool -> OperationContext -> Async<OperationResult<unit>>
    GetSettings: OperationContext -> Async<OperationResult<StoragePolicySettings>>
    SetSettings: StoragePolicySettings -> OperationContext -> Async<OperationResult<unit>>
}

/// How one selected regular file is stored in the revision being created. Only providers
/// with a large-object representation (Git with Git LFS) act on it. Providers without one
/// accept the strategy and never call it.
[<RequireQualifiedAccess>]
type RevisionPathPolicy =
    /// The provider decides from its automatic threshold and its own rules.
    | Automatic
    /// The plain content, even above the threshold and even when a provider rule says large object.
    | Inline
    /// Large-object storage, even below the threshold.
    | LargeObject

/// What the strategy sees for each selected regular file. SizeInBytes is the size of the
/// content that would be committed, so for a file that is a recognized large-object
/// reference it is the declared payload size, not the length of the reference.
type RevisionPathPolicyRequest = {
    Path: RepositoryPath
    SizeInBytes: float
}

/// Immutable configuration handed to a factory at creation. The function must be fast,
/// deterministic and free of side effects. It must not touch the file system or the network.
type RevisionPolicyStrategy = {
    ResolvePathPolicy: RevisionPathPolicyRequest -> RevisionPathPolicy
}

module RevisionPolicyStrategy =

    let automatic: RevisionPolicyStrategy = {
        ResolvePathPolicy = fun _ -> RevisionPathPolicy.Automatic
    }

/// Optional local storage maintenance extension.
type StorageMaintenanceService = {
    Prune: OperationContext -> Async<OperationResult<string>>
    Deduplicate: OperationContext -> Async<OperationResult<string>>
}

/// Optional repository browser extension.
type RepositoryBrowserService = {
    GetRepositoryWebUrl: OperationContext -> Async<OperationResult<string option>>
}
