namespace VersionControlService.Abstractions

open System

/// Library-owned cancellation surface so contracts stay portable across .NET and Fable.
type OperationCancellation = {
    /// True once cancellation has been requested.
    IsCancellationRequested: unit -> bool
    /// Registers a callback invoked when cancellation is requested. It runs immediately when
    /// already canceled. Implementations must make IsCancellationRequested return true before
    /// they invoke callbacks: providers read the flag inside abort handlers to tell a
    /// cancellation from a transport failure.
    Register: (unit -> unit) -> unit
}

module OperationCancellation =

    /// Host-controlled cancellation source; providers observe it through OperationCancellation.
    type Source() =
        let mutable canceled = false
        let callbacks = ResizeArray<unit -> unit>()

        member _.IsCancellationRequested = canceled

        member _.Cancel() =
            if not canceled then
                canceled <- true

                for callback in callbacks do
                    callback ()

        member _.Cancellation: OperationCancellation = {
            IsCancellationRequested = fun () -> canceled
            Register =
                fun callback ->
                    if canceled then callback () else callbacks.Add callback
        }

    /// A cancellation that is never requested.
    let none: OperationCancellation = {
        IsCancellationRequested = fun () -> false
        Register = ignore
    }

/// Progress report with a stable phase code and a sanitized display message.
/// Raw subprocess output is never forwarded without redaction.
type OperationProgress = {
    /// Stable machine-readable phase code such as "transfer" or "commit".
    PhaseCode: string
    /// Optional item or repository path the progress refers to.
    Item: string option
    Completed: float option
    Total: float option
    /// Sanitized human-readable message.
    DisplayMessage: string option
}

/// Context passed to every asynchronous provider operation.
type OperationContext = {
    OperationId: string
    Cancellation: OperationCancellation
    ReportProgress: OperationProgress -> unit
}

module OperationContext =

    /// The context catches and drops any exception the progress callback throws, so a host
    /// that reports to something already gone (a closed window, say) cannot fail the operation.
    let create (operationId: string) (cancellation: OperationCancellation) (reportProgress: OperationProgress -> unit) = {
        OperationId = operationId
        Cancellation = cancellation
        ReportProgress =
            fun progress ->
                try
                    reportProgress progress
                with _ ->
                    ()
    }

    /// Context without cancellation or progress observation.
    let detached (operationId: string) = {
        OperationId = operationId
        Cancellation = OperationCancellation.none
        ReportProgress = ignore
    }

/// Stable failure category. Provider-specific detail lives in the extensible Code string.
type FailureCategory =
    | Validation
    | NotFound
    | Concurrency
    | Authentication
    | Authorization
    | DependencyMissing
    | Network
    | Timeout
    | Canceled
    | Conflict
    | Unsupported
    | ProviderError

/// Machine-actionable recovery hint, e.g. code "refresh_conflict_session".
type RecoveryAction = {
    Code: string
    Instructions: string option
}

/// Structured failure. Message and Details must already be redacted.
type OperationFailure = {
    Category: FailureCategory
    /// Stable provider-extensible failure code such as "precondition_failed".
    Code: string
    /// Redacted human-readable message.
    Message: string
    /// Whether provider-visible state may have changed before the failure.
    StateChanged: bool
    /// Whether retrying the same request may succeed without further changes.
    Retryable: bool
    /// Repository-relative paths the failure is attributed to.
    AffectedPaths: string[]
    RecoveryAction: RecoveryAction option
    /// Redacted provider detail lines for diagnostics.
    Details: string[]
    /// Structured revision evidence for concurrency/race outcomes, e.g.
    /// ("expected_target", rev) and ("observed_target", rev).
    RevisionEvidence: (string * RevisionId)[]
}

type OperationWarning = {
    Code: string
    Message: string
}

/// Whether an operation changed provider state or was semantically unnecessary.
type OperationEffect =
    | Performed
    | NoOp of reason: string option

/// Whether the operation's result is visible on the configured publication target.
type PublicationState =
    /// The operation has no publication semantics.
    | PublicationNotApplicable
    /// Revisions exist locally (or on a provider-owned workspace branch) but are
    /// not yet visible on the logical target; publish can be retried.
    | LocalOnly
    /// The result is visible on the logical target.
    | Published

/// Successful (or partially successful) operation payload.
type OperationOutcome<'T> = {
    Value: 'T
    Effect: OperationEffect
    Warnings: OperationWarning[]
    /// Repository-relative paths the operation visibly changed.
    AffectedPaths: string[]
    ResultingRevision: RevisionId option
    /// Opaque optimistic-concurrency token observed after the operation.
    ResultingWorkspaceVersion: string option
    Publication: PublicationState
}

/// Distinguishes no change, success, partial success, and failure without message parsing.
type OperationResult<'T> =
    | Succeeded of OperationOutcome<'T>
    | PartiallySucceeded of OperationOutcome<'T> * OperationFailure
    | Failed of OperationFailure

/// Removes credential material from provider text before it enters messages,
/// details, progress output, or serialized data.
module Redaction =

    open System.Text.RegularExpressions

    let private replacements = [
        // URL credentials: https://user:secret@host or https://token@host
        Regex(@"://[^/@\s]+@", RegexOptions.None), "://[REDACTED]@"
        // Authorization / token headers keep the field name, lose the value.
        Regex(@"(authorization\s*:)[^\r\n]+", RegexOptions.IgnoreCase), "$1 [REDACTED]"
        Regex(@"(private-token\s*:)[^\r\n]+", RegexOptions.IgnoreCase), "$1 [REDACTED]"
        Regex(@"(x-access-token\s*:)[^\r\n]+", RegexOptions.IgnoreCase), "$1 [REDACTED]"
    ]

    /// Redacts likely credential material; clean text passes through unchanged.
    let redact (text: string) : string =
        if isNull text then
            text
        else
            replacements
            |> List.fold (fun (current: string) (pattern: Regex, replacement) -> pattern.Replace(current, replacement)) text

module OperationOutcome =

    let performed (value: 'T) : OperationOutcome<'T> = {
        Value = value
        Effect = Performed
        Warnings = [||]
        AffectedPaths = [||]
        ResultingRevision = None
        ResultingWorkspaceVersion = None
        Publication = PublicationNotApplicable
    }

    let noOp (reason: string option) (value: 'T) : OperationOutcome<'T> = {
        Value = value
        Effect = NoOp reason
        Warnings = [||]
        AffectedPaths = [||]
        ResultingRevision = None
        ResultingWorkspaceVersion = None
        Publication = PublicationNotApplicable
    }

module OperationFailure =

    let create (category: FailureCategory) (code: string) (message: string) : OperationFailure = {
        Category = category
        Code = code
        Message = message
        StateChanged = false
        Retryable = false
        AffectedPaths = [||]
        RecoveryAction = None
        Details = [||]
        RevisionEvidence = [||]
    }

    /// Creates a failure whose message is passed through the redaction guard.
    let createRedacted (category: FailureCategory) (code: string) (message: string) : OperationFailure =
        create category code (Redaction.redact message)

    /// Attaches diagnostic detail lines, redacting each line.
    let withDetails (details: string[]) (failure: OperationFailure) : OperationFailure = {
        failure with
            Details = details |> Array.map Redaction.redact
    }

module OperationResult =

    let succeeded (value: 'T) : OperationResult<'T> =
        Succeeded(OperationOutcome.performed value)

    let noOp (reason: string option) (value: 'T) : OperationResult<'T> =
        Succeeded(OperationOutcome.noOp reason value)

    let failed (failure: OperationFailure) : OperationResult<'T> = Failed failure

    /// Partial success always describes changed state and carries a recovery action,
    /// so consumers can never observe an ambiguous partially-completed operation.
    let partiallySucceeded
        (outcome: OperationOutcome<'T>)
        (failure: OperationFailure)
        (recovery: RecoveryAction)
        : OperationResult<'T> =
        PartiallySucceeded(
            outcome,
            {
                failure with
                    StateChanged = true
                    RecoveryAction = Some recovery
            }
        )

    let validationFailed (code: string) (message: string) : OperationResult<'T> =
        Failed(OperationFailure.create Validation code message)

    /// Structured canceled failure; StateChanged must be set by the caller when cleanup could not restore state.
    let canceled (message: string) : OperationResult<'T> =
        Failed(OperationFailure.create Canceled "operation_canceled" message)
