namespace VersionControlService.Abstractions

/// Provider-issued opaque conflict-session handle. Its Version rotates after every
/// successful nonterminal resolution and is distinct from WorkspaceVersion; the two
/// tokens have different lifetimes and are never collapsed into one field.
type ConflictSessionHandle = {
    SessionId: string
    Version: string
}

type ConflictPreview =
    | TextPreview of content: string
    | UnsupportedPreview of reason: string option

/// The stored object behind a conflict candidate, for content a provider keeps outside the ordinary file history (a Git LFS object, say). Consumers render it without a download.
type ConflictCandidateObject = {
    SizeBytes: float option
    /// Content identity, such as an LFS object id. Not a revision.
    ObjectId: string option
    /// Whether the object's bytes are in provider-local storage, observed when the summary was read.
    IsLocallyAvailable: bool
}

type ConflictCandidate = {
    CandidateId: string
    Label: string
    Revision: RevisionId option
    Preview: ConflictPreview option
    /// Some when the candidate's content is a separately stored object. None when the provider advertises no such object for it.
    Object: ConflictCandidateObject option
}

type ConflictItem = {
    Path: RepositoryPath
    Candidates: ConflictCandidate[]
    /// Provider-owned combined representation, e.g. Git conflict-marker text.
    CombinedPreview: ConflictPreview option
    /// True when the provider accepts caller-supplied resolved content. False for items whose candidates include a stored object, which are resolved by picking a candidate.
    SupportsResolvedContent: bool
}

type ConflictSessionSummary = {
    Handle: ConflictSessionHandle
    Items: ConflictItem[]
}

type ConflictResolution =
    | PickCandidate of candidateId: string
    | SupplyResolvedContent of content: string

type ResolveConflictRequest = {
    Handle: ConflictSessionHandle
    /// Independently observed workspace version; separate lifetime from the handle.
    ExpectedWorkspaceVersion: string
    Path: RepositoryPath
    Resolution: ConflictResolution
}

/// Every successful nonterminal resolution returns the refreshed handle so replaying
/// an earlier choice is deterministically stale.
type ConflictResolutionOutcome = {
    RefreshedHandle: ConflictSessionHandle
    RemainingItems: ConflictItem[]
}

type FinalizeConflictRequest = {
    Handle: ConflictSessionHandle
    ExpectedWorkspaceVersion: string
    Message: string option
}

type CancelConflictRequest = {
    Handle: ConflictSessionHandle
    ExpectedWorkspaceVersion: string
}

/// Stable recovery action code for stale/foreign/closed conflict-session handles.
module ConflictRecovery =

    [<Literal>]
    let RefreshConflictSession = "refresh_conflict_session"

/// Provider-managed conflict sessions. Stale, foreign, or closed handles are rejected
/// before provider state changes with category Concurrency, code "precondition_failed",
/// and recovery "refresh_conflict_session". Finalize/cancel closes the handle.
type ConflictResolutionService = {
    GetActiveSession: OperationContext -> Async<OperationResult<ConflictSessionSummary option>>
    Resolve: ResolveConflictRequest -> OperationContext -> Async<OperationResult<ConflictResolutionOutcome>>
    /// Finalizes only when all conflicts are resolved; verifies the provider
    /// destination after its own pre-check and closes the handle.
    Finalize: FinalizeConflictRequest -> OperationContext -> Async<OperationResult<RevisionId option>>
    Cancel: CancelConflictRequest -> OperationContext -> Async<OperationResult<unit>>
}
