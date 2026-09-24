namespace VersionControlService.Abstractions

/// Relationship between the workspace revisions and the configured target.
type RevisionRelationship =
    | UpToDate
    | LocalAhead
    | TargetAhead
    | Diverged
    | NoTarget
    | UnknownRelationship

/// Neutral synchronization state: explicit revisions plus optional relationship data.
/// Counts are optional because object-oriented providers cannot always compute them.
type SynchronizationState = {
    BaseRevision: RevisionId option
    WorkspaceRevision: RevisionId option
    TargetRevision: RevisionId option
    /// Configured publication/update target, when the workspace has one.
    TargetRef: LogicalRef option
    LocalRevisionCount: int option
    TargetRevisionCount: int option
    /// Advisory cache from the most recent Refresh/PreviewUpdate; may be None or
    /// stale. Never a substitute for running PreviewUpdate.
    RemoteChangedPaths: RepositoryPath[] option
    Relationship: RevisionRelationship
}

/// Authoritative result for preview UI before an Update.
type UpdatePreview = {
    /// Paths changed on the target side.
    ChangedPaths: RepositoryPath[]
    /// Target-side changes overlapping local dirty paths.
    OverlappingPaths: RepositoryPath[]
    /// Paths the update is expected to leave in conflict, when the provider can predict them.
    /// None means the provider does not predict conflicts.
    PredictedConflictPaths: RepositoryPath[] option
    HasDataLossRisk: bool
    WouldCreateConflictSession: bool
}

type UpdateRequest = {
    ExpectedWorkspaceVersion: string
}

type PublishRequest = {
    ExpectedWorkspaceVersion: string
    /// Consumer-observed target revision for the provider's client-side
    /// check-then-act-then-verify sequence. Never a server-side precondition.
    ExpectedTargetRevision: RevisionId option
}

/// One synchronization. The refresh observes the target, and the operation then
/// performs two mutations at most: an update when the workspace is behind and a
/// publish of the local revisions. The consumer decides only where the provider
/// cannot: it accepts a conflict session, saves or discards local changes an update
/// would touch, retries a preview that could not be computed, and binds a
/// publication target the provider reports as missing.
type SynchronizeRequest = {
    ExpectedWorkspaceVersion: string
    /// The target revision the consumer observed when it made its decision. A target
    /// that has moved since fails with precondition_failed before any mutation.
    /// Required when AcceptUpdateRisks is set, so an acceptance never applies to a
    /// target the user did not see.
    ExpectedTargetRevision: RevisionId option
    /// The consumer showed the update preview and the user accepted that the update
    /// opens a conflict session.
    AcceptUpdateRisks: bool
    /// When false, the operation updates the workspace and leaves local revisions unpublished.
    PublishLocalRevisions: bool
}

/// Synchronization expressed as intent; providers own the mechanics.
type SynchronizationService = {
    /// Observe target state without changing workspace content.
    Refresh: OperationContext -> Async<OperationResult<SynchronizationState>>
    /// Return changed/overlapping paths, data-loss risk, and whether a conflict session would be created.
    PreviewUpdate: OperationContext -> Async<OperationResult<UpdatePreview>>
    /// Incorporate the target into the provider workspace.
    Update: UpdateRequest -> OperationContext -> Async<OperationResult<SynchronizationState>>
    /// Make workspace revisions visible on the configured target.
    Publish: PublishRequest -> OperationContext -> Async<OperationResult<SynchronizationState>>
    /// The refresh, the update the workspace needs and the publish of its revisions
    /// run as one operation under the provider's mutation lock. See
    /// Synchronization.compose for the contract.
    Synchronize: SynchronizeRequest -> OperationContext -> Async<OperationResult<SynchronizationState>>
}

/// Codes the composition produces. Providers keep their own codes for their steps.
module SynchronizationCodes =
    /// The update would change files that carry local changes. AffectedPaths names them.
    [<Literal>]
    let UpdateWouldOverwriteLocalChanges = "update_would_overwrite_local_changes"

    /// The update would open a conflict session without touching local changes.
    [<Literal>]
    let UpdateWouldCreateConflictSession = "update_would_create_conflict_session"

    /// AcceptUpdateRisks was set without ExpectedTargetRevision.
    [<Literal>]
    let AcceptanceTargetRequired = "acceptance_target_required"

    /// A conflict session is open. Resolve or cancel it first.
    [<Literal>]
    let ConflictSessionActive = "conflict_session_active"

    /// Local changes on affected paths must be saved or discarded before updating.
    [<Literal>]
    let ResolveLocalChangesRecovery = "resolve_local_changes"

    /// The consumer's expected target revision no longer matches the refreshed target.
    [<Literal>]
    let PreconditionFailed = "precondition_failed"

    /// Recovery: show the preview data of the failure, then retry with AcceptUpdateRisks
    /// and the observed target revision.
    [<Literal>]
    let AcceptUpdateRisksRecovery = "accept_update_risks"

    /// Recovery: the update was applied and the publish did not happen. Synchronize again.
    [<Literal>]
    let RetryPublishRecovery = "retry_publish"

/// The provider primitives the composition runs. They run inside the provider's
/// mutation lock with the workspace version validated once, so none of them validates
/// it again. Refresh observes the target once and every later step consumes that
/// observation instead of reading the target again, so the update applies exactly the
/// revision the preview described and the decision evidence names that revision.
type SynchronizationSteps = {
    /// True when a conflict session is open. The composition refuses before the refresh. A
    /// Failed result (for example a provider's operation_in_progress) is returned as it is.
    HasActiveConflictSession: OperationContext -> Async<OperationResult<bool>>
    /// Observes the target and returns the pinned state.
    Refresh: OperationContext -> Async<OperationResult<SynchronizationState>>
    /// Previews an update to the pinned target without reading the target again.
    PreviewUpdate: SynchronizationState -> OperationContext -> Async<OperationResult<UpdatePreview>>
    /// Applies exactly the pinned target revision. NoOp when there is nothing to apply.
    Update: SynchronizationState -> OperationContext -> Async<OperationResult<SynchronizationState>>
    /// Publishes the workspace. The provider derives the expected publication target
    /// from the pinned state when its publication target is the synchronization
    /// target, and checks nothing otherwise.
    Publish: SynchronizationState -> OperationContext -> Async<OperationResult<SynchronizationState>>
}

module Synchronization =
    let private withObservedEvidence (state: SynchronizationState) (failure: OperationFailure) =
        match state.TargetRevision with
        | Some revision ->
            { failure with
                RevisionEvidence = Array.append failure.RevisionEvidence [| "observed_target", revision |] }
        | None -> failure

    let private mergeAffectedPaths (first: string[]) (second: string[]) =
        Array.append first second |> Array.distinct

    let compose
        (steps: SynchronizationSteps)
        (request: SynchronizeRequest)
        (context: OperationContext)
        : Async<OperationResult<SynchronizationState>> =
        async {
            if request.AcceptUpdateRisks && request.ExpectedTargetRevision.IsNone then
                return
                    Failed(
                        OperationFailure.create
                            Validation
                            SynchronizationCodes.AcceptanceTargetRequired
                            "An accepted update needs the target revision the preview was computed for."
                    )
            else
                let! activeConflictResult = steps.HasActiveConflictSession context

                match activeConflictResult with
                | Failed failure
                | PartiallySucceeded(_, failure) -> return Failed failure
                | Succeeded active when active.Value ->
                    return
                        Failed(
                            OperationFailure.create
                                Conflict
                                SynchronizationCodes.ConflictSessionActive
                                "Resolve or cancel the active conflict session first."
                        )
                | Succeeded _ ->
                    let! refreshResult = steps.Refresh context

                    match refreshResult with
                    | Failed failure
                    | PartiallySucceeded(_, failure) -> return Failed failure
                    | Succeeded refreshOutcome ->
                        let state0 = refreshOutcome.Value
                        let refreshWarnings = refreshOutcome.Warnings

                        let expectedTargetFailure =
                            match request.ExpectedTargetRevision with
                            | Some expected when state0.TargetRevision <> Some expected ->
                                Some {
                                    OperationFailure.create
                                        Concurrency
                                        SynchronizationCodes.PreconditionFailed
                                        "The target advanced past the revision the decision was made for." with
                                            RevisionEvidence = [|
                                                "expected_target", expected

                                                yield!
                                                    state0.TargetRevision
                                                    |> Option.map (fun observed -> "observed_target", observed)
                                                    |> Option.toList
                                            |]
                                }
                            | _ -> None

                        match expectedTargetFailure with
                        | Some failure -> return Failed failure
                        | None when context.Cancellation.IsCancellationRequested() ->
                            return OperationResult.canceled "The synchronization was canceled."
                        | None ->
                            let needsUpdate =
                                match state0.Relationship with
                                | UpToDate
                                | LocalAhead
                                | NoTarget -> false
                                | TargetAhead
                                | Diverged
                                | UnknownRelationship -> true

                            let! updateDecision =
                                if needsUpdate then
                                    async {
                                        let! previewResult = steps.PreviewUpdate state0 context

                                        match previewResult with
                                        | Failed failure
                                        | PartiallySucceeded(_, failure) ->
                                            return Error(withObservedEvidence state0 failure)
                                        | Succeeded previewOutcome when previewOutcome.Value.HasDataLossRisk ->
                                            return
                                                Error {
                                                    OperationFailure.create
                                                        Conflict
                                                        SynchronizationCodes.UpdateWouldOverwriteLocalChanges
                                                        "Updating from the target would change files with local changes." with
                                                            StateChanged = false
                                                            AffectedPaths =
                                                                previewOutcome.Value.OverlappingPaths
                                                                |> Array.map RepositoryPath.value
                                                            RevisionEvidence =
                                                                state0.TargetRevision
                                                                |> Option.map (fun revision -> [| "observed_target", revision |])
                                                                |> Option.defaultValue [||]
                                                            RecoveryAction =
                                                                Some {
                                                                    Code = SynchronizationCodes.ResolveLocalChangesRecovery
                                                                    Instructions =
                                                                        Some
                                                                            "Save or discard the local changes on the affected paths, then synchronize again."
                                                                }
                                                }
                                        | Succeeded previewOutcome when
                                            previewOutcome.Value.WouldCreateConflictSession
                                            && not request.AcceptUpdateRisks ->
                                            return
                                                Error {
                                                    OperationFailure.create
                                                        Conflict
                                                        SynchronizationCodes.UpdateWouldCreateConflictSession
                                                        "Updating from the target needs conflict resolution." with
                                                            // Committed conflicts count too, so the consumer can
                                                            // name every path the merge will stop on.
                                                            AffectedPaths =
                                                                Array.append
                                                                    previewOutcome.Value.OverlappingPaths
                                                                    (previewOutcome.Value.PredictedConflictPaths
                                                                     |> Option.defaultValue [||])
                                                                |> Array.map RepositoryPath.value
                                                                |> Array.distinct
                                                            RevisionEvidence =
                                                                state0.TargetRevision
                                                                |> Option.map (fun revision -> [| "observed_target", revision |])
                                                                |> Option.defaultValue [||]
                                                            RecoveryAction =
                                                                Some {
                                                                    Code = SynchronizationCodes.AcceptUpdateRisksRecovery
                                                                    Instructions =
                                                                        Some
                                                                            "Show the affected paths, then synchronize again with AcceptUpdateRisks and the observed target revision."
                                                                }
                                                }
                                        | Succeeded _ -> return Ok()
                                    }
                                else
                                    async { return Ok() }

                            match updateDecision with
                            | Error failure -> return Failed failure
                            | Ok() when needsUpdate && context.Cancellation.IsCancellationRequested() ->
                                return OperationResult.canceled "The synchronization was canceled before the update."
                            | Ok() ->
                                let finish
                                    (state1: SynchronizationState)
                                    (updateOutcome: OperationOutcome<SynchronizationState> option)
                                    =
                                    async {
                                        let updated = updateOutcome |> Option.exists (fun outcome -> outcome.Effect = Performed)
                                        let updateWarnings =
                                            updateOutcome
                                            |> Option.map (fun outcome -> outcome.Warnings)
                                            |> Option.defaultValue [||]
                                        let updateAffectedPaths =
                                            updateOutcome
                                            |> Option.map (fun outcome -> outcome.AffectedPaths)
                                            |> Option.defaultValue [||]

                                        let publicationForUpdateOnly =
                                            match state1.Relationship with
                                            | LocalAhead
                                            | Diverged -> LocalOnly
                                            | _ -> PublicationNotApplicable

                                        if not request.PublishLocalRevisions then
                                            match updateOutcome with
                                            | Some updateOutcome when updated ->
                                                return
                                                    Succeeded {
                                                        updateOutcome with
                                                            Warnings = Array.append refreshWarnings updateWarnings
                                                            Publication = publicationForUpdateOnly
                                                    }
                                            | _ ->
                                                return
                                                    Succeeded {
                                                        OperationOutcome.noOp
                                                            (Some "The workspace already has every target revision.")
                                                            state1 with
                                                                Warnings = refreshWarnings
                                                                Publication = publicationForUpdateOnly
                                                    }
                                        elif context.Cancellation.IsCancellationRequested() then
                                            if updated then
                                                let failure =
                                                    OperationFailure.create
                                                        Canceled
                                                        "operation_canceled"
                                                        "The synchronization was canceled before the publish."

                                                let recovery = {
                                                    Code = SynchronizationCodes.RetryPublishRecovery
                                                    Instructions = Some "The update was applied. Synchronize again to publish."
                                                }

                                                let updateOutcome = updateOutcome |> Option.get

                                                return
                                                    PartiallySucceeded(
                                                        {
                                                            updateOutcome with
                                                                Value = state1
                                                                Effect = Performed
                                                                Publication = publicationForUpdateOnly
                                                                Warnings = Array.append refreshWarnings updateWarnings
                                                        },
                                                        { failure with
                                                            StateChanged = true
                                                            RecoveryAction = Some recovery }
                                                    )
                                            else
                                                return OperationResult.canceled "The synchronization was canceled before the publish."
                                        else
                                            let! publishResult = steps.Publish state1 context

                                            match publishResult with
                                            | Succeeded publishOutcome ->
                                                return
                                                    Succeeded {
                                                        publishOutcome with
                                                            Effect =
                                                                if updated || publishOutcome.Effect = Performed then
                                                                    Performed
                                                                else
                                                                    NoOp(Some "The workspace and the target are already synchronized.")
                                                            Warnings =
                                                                Array.append
                                                                    refreshWarnings
                                                                    (Array.append updateWarnings publishOutcome.Warnings)
                                                            AffectedPaths =
                                                                mergeAffectedPaths updateAffectedPaths publishOutcome.AffectedPaths
                                                    }
                                            | PartiallySucceeded(publishOutcome, failure) ->
                                                return
                                                    PartiallySucceeded(
                                                        { publishOutcome with
                                                            Effect = Performed
                                                            Warnings =
                                                                Array.append
                                                                    refreshWarnings
                                                                    (Array.append updateWarnings publishOutcome.Warnings)
                                                            AffectedPaths =
                                                                mergeAffectedPaths updateAffectedPaths publishOutcome.AffectedPaths },
                                                        failure
                                                    )
                                            | Failed failure when updated && failure.StateChanged ->
                                                let recoveryAction =
                                                    match failure.RecoveryAction with
                                                    | Some recovery -> Some recovery
                                                    | None ->
                                                        Some {
                                                            Code = SynchronizationCodes.RetryPublishRecovery
                                                            Instructions = Some "The update was applied. Synchronize again to publish."
                                                        }

                                                return Failed { failure with RecoveryAction = recoveryAction }
                                            | Failed failure when updated ->
                                                let recovery =
                                                    failure.RecoveryAction
                                                    |> Option.defaultValue {
                                                        Code = SynchronizationCodes.RetryPublishRecovery
                                                        Instructions = Some "The update was applied. Synchronize again to publish."
                                                    }

                                                let updateOutcome = updateOutcome |> Option.get

                                                return
                                                    PartiallySucceeded(
                                                        {
                                                            updateOutcome with
                                                                Value = state1
                                                                Effect = Performed
                                                                Publication = publicationForUpdateOnly
                                                                Warnings = Array.append refreshWarnings updateWarnings
                                                        },
                                                        { failure with
                                                            StateChanged = true
                                                            RecoveryAction = Some recovery }
                                                    )
                                            | Failed failure -> return Failed failure

                                    }

                                let! updateResult =
                                    if needsUpdate then
                                        async { return Some(steps.Update state0 context) }
                                    else
                                        async { return None }

                                match updateResult with
                                | Some updateOperation ->
                                    let! result = updateOperation

                                    match result with
                                    | Failed failure -> return Failed failure
                                    | PartiallySucceeded(outcome, failure) ->
                                        return
                                            PartiallySucceeded(
                                                { outcome with
                                                    Warnings = Array.append refreshWarnings outcome.Warnings },
                                                failure
                                            )
                                    | Succeeded updateOutcome ->
                                        return! finish updateOutcome.Value (Some updateOutcome)
                                | None ->
                                    return! finish state0 None
                }
