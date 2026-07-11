namespace VersionControlService.Abstractions

open System

/// Library-owned cancellation surface so contracts stay portable across .NET and Fable.
type OperationCancellation = {
    /// True once cancellation has been requested.
    IsCancellationRequested: unit -> bool
    /// Registers a callback invoked when cancellation is requested; invoked immediately when already canceled.
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
    Completed: int option
    Total: int option
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

    let create (operationId: string) (cancellation: OperationCancellation) (reportProgress: OperationProgress -> unit) = {
        OperationId = operationId
        Cancellation = cancellation
        ReportProgress = reportProgress
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
}

type OperationWarning = {
    Code: string
    Message: string
}

/// Whether an operation changed provider state or was semantically unnecessary.
type OperationEffect =
    | Performed
    | NoOp of reason: string option

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
}

/// Distinguishes no change, success, partial success, and failure without message parsing.
type OperationResult<'T> =
    | Succeeded of OperationOutcome<'T>
    | PartiallySucceeded of OperationOutcome<'T> * OperationFailure
    | Failed of OperationFailure

module OperationOutcome =

    let performed (value: 'T) : OperationOutcome<'T> = {
        Value = value
        Effect = Performed
        Warnings = [||]
        AffectedPaths = [||]
        ResultingRevision = None
        ResultingWorkspaceVersion = None
    }

    let noOp (reason: string option) (value: 'T) : OperationOutcome<'T> = {
        Value = value
        Effect = NoOp reason
        Warnings = [||]
        AffectedPaths = [||]
        ResultingRevision = None
        ResultingWorkspaceVersion = None
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
    }

module OperationResult =

    let succeeded (value: 'T) : OperationResult<'T> =
        Succeeded(OperationOutcome.performed value)

    let noOp (reason: string option) (value: 'T) : OperationResult<'T> =
        Succeeded(OperationOutcome.noOp reason value)

    let failed (failure: OperationFailure) : OperationResult<'T> = Failed failure

    let validationFailed (code: string) (message: string) : OperationResult<'T> =
        Failed(OperationFailure.create Validation code message)

    /// Structured canceled failure; StateChanged must be set by the caller when cleanup could not restore state.
    let canceled (message: string) : OperationResult<'T> =
        Failed(OperationFailure.create Canceled "operation_canceled" message)
