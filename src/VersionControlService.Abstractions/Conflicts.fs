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

type ConflictCandidate = {
    CandidateId: string
    Label: string
    Revision: RevisionId option
    Preview: ConflictPreview option
}

type ConflictItem = {
    Path: RepositoryPath
    Candidates: ConflictCandidate[]
    /// True when the provider accepts caller-supplied resolved content for this item.
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
