module VersionControlService.Abstractions.Tests.SynchronizationComposeTests

open Expecto
open VersionControlService.Abstractions

let private revision value =
    match RevisionId.tryCreate value with
    | Ok value -> value
    | Error message -> failtest message

let private path value =
    match RepositoryPath.tryCreate value with
    | Ok value -> value
    | Error message -> failtest message

let private state relationship target = {
    BaseRevision = None
    WorkspaceRevision = None
    TargetRevision = target
    TargetRef = None
    LocalRevisionCount = None
    TargetRevisionCount = None
    RemoteChangedPaths = None
    Relationship = relationship
}

let private request accept publish target = {
    ExpectedWorkspaceVersion = "workspace-version"
    ExpectedTargetRevision = target
    AcceptUpdateRisks = accept
    PublishLocalRevisions = publish
}

let private context (source: OperationCancellation.Source option) =
    match source with
    | Some source -> OperationContext.create "compose-test" source.Cancellation ignore
    | None -> OperationContext.detached "compose-test"

let private warning code = {
    Code = code
    Message = code
}

let private preview risk conflict paths = {
    ChangedPaths = [||]
    OverlappingPaths = paths |> Array.map path
    PredictedConflictPaths = None
    HasDataLossRisk = risk
    WouldCreateConflictSession = conflict
}

let private failure category code = OperationFailure.create category code code

let private performed value warnings affected publication = {
    OperationOutcome.performed value with
        Warnings = warnings
        AffectedPaths = affected
        Publication = publication
}

let private noOp value = OperationResult.noOp (Some "no-op") value

let private recordingSteps
    (log: ResizeArray<string>)
    (activeResult: OperationResult<bool>)
    (refreshResult: OperationResult<SynchronizationState>)
    (previewResult: SynchronizationState -> OperationResult<UpdatePreview>)
    (updateResult: SynchronizationState -> OperationResult<SynchronizationState>)
    (publishResult: SynchronizationState -> OperationResult<SynchronizationState>)
    =
    let updatedState = ref None
    let publishedState = ref None

    let steps: SynchronizationSteps = {
        HasActiveConflictSession =
            fun _ ->
                log.Add "active"
                async { return activeResult }
        Refresh =
            fun _ ->
                log.Add "refresh"
                async { return refreshResult }
        PreviewUpdate =
            fun syncState _ ->
                log.Add "preview"
                async { return previewResult syncState }
        Update =
            fun syncState _ ->
                log.Add "update"
                updatedState.Value <- Some syncState
                async { return updateResult syncState }
        Publish =
            fun syncState _ ->
                log.Add "publish"
                publishedState.Value <- Some syncState
                async { return publishResult syncState }
    }

    steps, updatedState, publishedState

let private run steps request source =
    Synchronization.compose steps request (context source)
    |> Async.RunSynchronously

let private expectFailure result =
    match result with
    | Failed failure -> failure
    | PartiallySucceeded(_, failure) -> failure
    | Succeeded _ -> failtest "Expected a failure."

let private expectSucceeded result =
    match result with
    | Succeeded outcome -> outcome
    | PartiallySucceeded _
    | Failed _ -> failtest "Expected a succeeded result."

let private expectPartial result =
    match result with
    | PartiallySucceeded(outcome, failure) -> outcome, failure
    | Succeeded _
    | Failed _ -> failtest "Expected a partial result."

let private assertSequence log expected =
    Expect.equal (Seq.toArray log) expected "The composition called the expected steps."

[<Tests>]
let synchronizationComposeTests =
    testList "Synchronization.compose" [
        testCase "acceptance without a target fails before any step"
        <| fun () ->
            let log = ResizeArray<string>()
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded (state UpToDate None))
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value -> OperationResult.succeeded value)
                    (fun value -> OperationResult.succeeded value)

            let failure = expectFailure (run steps (request true false None) None)
            Expect.equal failure.Code SynchronizationCodes.AcceptanceTargetRequired "Acceptance target is required."
            assertSequence log [||]

        testCase "an active conflict stops before refresh"
        <| fun () ->
            let log = ResizeArray<string>()
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded true)
                    (OperationResult.succeeded (state UpToDate None))
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value -> OperationResult.succeeded value)
                    (fun value -> OperationResult.succeeded value)

            let failure = expectFailure (run steps (request false false None) None)
            Expect.equal failure.Code SynchronizationCodes.ConflictSessionActive "Active conflict code."
            assertSequence log [| "active" |]

        testCase "active conflict step failures are returned as failed"
        <| fun () ->
            for activeResult in [
                Failed(failure ProviderError "active_failed")
                OperationResult.partiallySucceeded
                    (OperationOutcome.performed true)
                    (failure ProviderError "active_partial")
                    { Code = "retry_active"; Instructions = None }
            ] do
                let log = ResizeArray<string>()
                let steps, _, _ =
                    recordingSteps
                        log
                        activeResult
                        (OperationResult.succeeded (state UpToDate None))
                        (fun _ -> OperationResult.succeeded (preview false false [||]))
                        (fun value -> OperationResult.succeeded value)
                        (fun value -> OperationResult.succeeded value)

                let observedFailure = expectFailure (run steps (request false false None) None)
                Expect.equal observedFailure.Category ProviderError "Active-step category is preserved."
                assertSequence log [| "active" |]

        testCase "a failed refresh is returned without later steps"
        <| fun () ->
            let log = ResizeArray<string>()
            let refreshFailure = failure Network "refresh_failed"
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (Failed refreshFailure)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value -> OperationResult.succeeded value)
                    (fun value -> OperationResult.succeeded value)

            let observed = expectFailure (run steps (request false false None) None)
            Expect.equal observed.Code refreshFailure.Code "Refresh failure is preserved."
            assertSequence log [| "active"; "refresh" |]

        testCase "a stale expected target fails with both revision evidence entries"
        <| fun () ->
            let log = ResizeArray<string>()
            let observed = revision "observed"
            let expected = revision "expected"
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded (state TargetAhead (Some observed)))
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value -> OperationResult.succeeded value)
                    (fun value -> OperationResult.succeeded value)

            let observedFailure = expectFailure (run steps (request false false (Some expected)) None)
            Expect.equal observedFailure.Code "precondition_failed" "Stale target code."
            Expect.isFalse observedFailure.StateChanged "No mutation happened."
            Expect.equal
                (observedFailure.RevisionEvidence |> Array.map (fun (role, value) -> role, RevisionId.value value))
                [| "expected_target", "expected"; "observed_target", "observed" |]
                "Target evidence is ordered."
            assertSequence log [| "active"; "refresh" |]

        testCase "up-to-date, local-ahead, and no-target states skip preview and update"
        <| fun () ->
            for relationship, target in [ UpToDate, Some(revision "target"); LocalAhead, Some(revision "target"); NoTarget, None ] do
                let log = ResizeArray<string>()
                let steps, _, _ =
                    recordingSteps
                        log
                        (OperationResult.succeeded false)
                        (OperationResult.succeeded (state relationship target))
                        (fun _ -> OperationResult.succeeded (preview false false [||]))
                        (fun value -> OperationResult.succeeded value)
                        (fun value -> OperationResult.succeeded value)

                let outcome = expectSucceeded (run steps (request false false None) None)
                Expect.equal outcome.Effect (NoOp(Some "The workspace already has every target revision.")) "No update effect."
                assertSequence log [| "active"; "refresh" |]

        testCase "diverged and unknown relationships reach preview and update"
        <| fun () ->
            for relationship in [ Diverged; UnknownRelationship ] do
                let log = ResizeArray<string>()
                let target = revision "target"
                let steps, _, _ =
                    recordingSteps
                        log
                        (OperationResult.succeeded false)
                        (OperationResult.succeeded (state relationship (Some target)))
                        (fun _ -> OperationResult.succeeded (preview false false [||]))
                        (fun value -> OperationResult.succeeded value)
                        (fun value -> OperationResult.succeeded value)

                let outcome = expectSucceeded (run steps (request false false None) None)
                Expect.equal outcome.Effect Performed "A state needing update reaches update."
                assertSequence log [| "active"; "refresh"; "preview"; "update" |]

        testCase "data loss and conflict-only previews produce distinct decisions"
        <| fun () ->
            for hasDataLoss, expectedCode in [ true, SynchronizationCodes.UpdateWouldOverwriteLocalChanges; false, SynchronizationCodes.UpdateWouldCreateConflictSession ] do
                let log = ResizeArray<string>()
                let target = revision "target"
                let steps, _, _ =
                    recordingSteps
                        log
                        (OperationResult.succeeded false)
                        (OperationResult.succeeded (state TargetAhead (Some target)))
                        (fun _ -> OperationResult.succeeded (preview hasDataLoss true [| "overlap.txt" |]))
                        (fun value -> OperationResult.succeeded value)
                        (fun value -> OperationResult.succeeded value)

                let observedFailure = expectFailure (run steps (request false false None) None)
                let expectedRecovery =
                    if hasDataLoss then
                        SynchronizationCodes.ResolveLocalChangesRecovery
                    else
                        SynchronizationCodes.AcceptUpdateRisksRecovery

                Expect.equal observedFailure.Code expectedCode "Decision code is stable."
                Expect.equal observedFailure.Category Conflict "Decision is a conflict."
                Expect.isFalse observedFailure.StateChanged "The preview failure did not change state."
                Expect.equal observedFailure.AffectedPaths [| "overlap.txt" |] "Overlapping path is carried."
                Expect.equal (observedFailure.RevisionEvidence |> Array.map fst) [| "observed_target" |] "Observed target evidence."
                Expect.equal (observedFailure.RecoveryAction |> Option.map _.Code) (Some expectedRecovery) "Decision recovery."
                assertSequence log [| "active"; "refresh"; "preview" |]

        testCase "a conflict refusal names predicted conflict paths after the overlapping ones"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = revision "target"

            let predictedPreview = {
                preview false true [| "overlap.txt" |] with
                    PredictedConflictPaths = Some [| path "committed.txt"; path "overlap.txt" |]
            }

            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded (state TargetAhead (Some target)))
                    (fun _ -> OperationResult.succeeded predictedPreview)
                    (fun value -> OperationResult.succeeded value)
                    (fun value -> OperationResult.succeeded value)

            let observedFailure = expectFailure (run steps (request false false None) None)
            Expect.equal observedFailure.Code SynchronizationCodes.UpdateWouldCreateConflictSession "Conflict decision."
            Expect.equal observedFailure.AffectedPaths [| "overlap.txt"; "committed.txt" |] "Both path sets, each once."

        testCase "data loss still fails when accepted and does not update"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = revision "target"
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded (state TargetAhead (Some target)))
                    (fun _ -> OperationResult.succeeded (preview true true [| "overlap.txt" |]))
                    (fun _ -> failwith "Data-loss acceptance must not update.")
                    (fun value -> OperationResult.succeeded value)

            let observedFailure = expectFailure (run steps (request true false (Some target)) None)
            Expect.equal observedFailure.Code SynchronizationCodes.UpdateWouldOverwriteLocalChanges "Data-loss code."
            Expect.equal observedFailure.RecoveryAction.Value.Code SynchronizationCodes.ResolveLocalChangesRecovery "Local-change recovery."
            assertSequence log [| "active"; "refresh"; "preview" |]

        testCase "a clean preview updates the same pinned state"
        <| fun () ->
            let log = ResizeArray<string>()
            let refreshed = state TargetAhead (Some(revision "target"))
            let seenPreview = ref None
            let seenUpdate = ref None
            let steps: SynchronizationSteps = {
                HasActiveConflictSession = fun _ -> async { log.Add "active"; return OperationResult.succeeded false }
                Refresh = fun _ -> async { log.Add "refresh"; return OperationResult.succeeded refreshed }
                PreviewUpdate = fun value _ -> async { log.Add "preview"; seenPreview.Value <- Some value; return OperationResult.succeeded (preview false false [||]) }
                Update = fun value _ -> async { log.Add "update"; seenUpdate.Value <- Some value; return OperationResult.succeeded value }
                Publish = fun value _ -> async { log.Add "publish"; return OperationResult.succeeded value }
            }

            let outcome = expectSucceeded (run steps (request false false None) None)
            Expect.isTrue (System.Object.ReferenceEquals(refreshed, seenPreview.Value.Value)) "Preview received the refreshed state."
            Expect.isTrue (System.Object.ReferenceEquals(refreshed, seenUpdate.Value.Value)) "Update received the refreshed state."
            Expect.equal outcome.Value refreshed "The update state is returned."
            assertSequence log [| "active"; "refresh"; "preview"; "update" |]

        testCase "a preview failure carries observed target evidence"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = revision "target"
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded (state TargetAhead (Some target)))
                    (fun _ -> Failed(failure ProviderError "preview_failed"))
                    (fun value -> OperationResult.succeeded value)
                    (fun value -> OperationResult.succeeded value)

            let observedFailure = expectFailure (run steps (request true false (Some target)) None)
            Expect.equal (observedFailure.RevisionEvidence |> Array.map fst) [| "observed_target" |] "Preview evidence is appended."
            assertSequence log [| "active"; "refresh"; "preview" |]

        testCase "accepted conflict-only preview reaches update"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = revision "target"
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded (state TargetAhead (Some target)))
                    (fun _ -> OperationResult.succeeded (preview false true [| "overlap.txt" |]))
                    (fun value -> OperationResult.succeeded value)
                    (fun value -> OperationResult.succeeded value)

            let outcome = expectSucceeded (run steps (request true false (Some target)) None)
            Expect.equal outcome.Effect Performed "Accepted update runs."
            assertSequence log [| "active"; "refresh"; "preview"; "update" |]

        testCase "a partial update is returned without publish"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = state TargetAhead (Some(revision "target"))
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (Succeeded(performed target [| warning "refresh" |] [||] PublicationNotApplicable))
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value ->
                        OperationResult.partiallySucceeded
                            (performed value [| warning "update" |] [||] PublicationNotApplicable)
                            (failure Conflict "conflicts_detected")
                            { Code = "resolve"; Instructions = None })
                    (fun _ -> failwith "Publish must not run.")

            let outcome, observedFailure = expectPartial (run steps (request true true target.TargetRevision) None)
            Expect.equal observedFailure.Code "conflicts_detected" "Update failure is preserved."
            Expect.equal (outcome.Warnings |> Array.map _.Code) [| "refresh"; "update" |] "Refresh warnings reach partial update."
            assertSequence log [| "active"; "refresh"; "preview"; "update" |]

        testCase "cancellation before update returns canceled without update"
        <| fun () ->
            let cancellation = OperationCancellation.Source()
            cancellation.Cancel()
            let log = ResizeArray<string>()
            let target = revision "target"
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded (state TargetAhead (Some target)))
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun _ -> failwith "Update must not run.")
                    (fun value -> OperationResult.succeeded value)

            let observedFailure = expectFailure (run steps (request true false (Some target)) (Some cancellation))
            Expect.equal observedFailure.Category Canceled "Cancellation category."
            Expect.isFalse observedFailure.StateChanged "No update happened."
            assertSequence log [| "active"; "refresh" |]

        testCase "cancellation after refresh with nothing to do skips publication"
        <| fun () ->
            let cancellation = OperationCancellation.Source()
            let log = ResizeArray<string>()
            let steps: SynchronizationSteps = {
                HasActiveConflictSession = fun _ -> async { log.Add "active"; return OperationResult.succeeded false }
                Refresh = fun _ ->
                    async {
                        log.Add "refresh"
                        cancellation.Cancel()
                        return OperationResult.succeeded (state UpToDate None)
                    }
                PreviewUpdate = fun _ _ -> async { return failwith "No-op cancellation must skip preview." }
                Update = fun _ _ -> async { return failwith "No-op cancellation must skip update." }
                Publish = fun _ _ -> async { return failwith "No-op cancellation must skip publish." }
            }

            let observedFailure = expectFailure (run steps (request false false None) (Some cancellation))
            Expect.equal observedFailure.Category Canceled "Cancellation category."
            Expect.isFalse observedFailure.StateChanged "No state changed."
            assertSequence log [| "active"; "refresh" |]

        testCase "publish disabled returns the update and never publishes"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = state TargetAhead (Some(revision "target"))
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded target)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value -> Succeeded(performed value [| warning "update" |] [||] PublicationNotApplicable))
                    (fun _ -> failwith "Publish must not run.")

            let outcome = expectSucceeded (run steps (request true false target.TargetRevision) None)
            Expect.equal outcome.Effect Performed "Update is performed."
            Expect.equal (outcome.Warnings |> Array.map _.Code) [| "update" |] "Update warning is preserved."
            assertSequence log [| "active"; "refresh"; "preview"; "update" |]

        testCase "publish disabled with no update is a no-op"
        <| fun () ->
            let log = ResizeArray<string>()
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (Succeeded(performed (state UpToDate None) [| warning "refresh" |] [||] PublicationNotApplicable))
                    (fun _ -> failwith "Preview must not run.")
                    (fun _ -> failwith "Update must not run.")
                    (fun _ -> failwith "Publish must not run.")

            let outcome = expectSucceeded (run steps (request false false None) None)
            Expect.isTrue (match outcome.Effect with NoOp _ -> true | Performed -> false) "No update is a no-op."
            Expect.equal (outcome.Warnings |> Array.map _.Code) [| "refresh" |] "Refresh warning reaches the no-op."
            Expect.equal outcome.Publication PublicationNotApplicable "Up-to-date no-op has no publication."
            assertSequence log [| "active"; "refresh" |]

        testCase "update-only local-ahead no-op reports local-only publication"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = state LocalAhead (Some(revision "target"))
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded target)
                    (fun _ -> failwith "Preview must not run.")
                    (fun _ -> failwith "Update must not run.")
                    (fun _ -> failwith "Publish must not run.")

            let outcome = expectSucceeded (run steps (request false false None) None)
            Expect.equal outcome.Publication LocalOnly "Local-ahead update-only state is local-only."
            assertSequence log [| "active"; "refresh" |]

        testCase "publish merges warnings and affected paths"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = state TargetAhead (Some(revision "target"))
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (Succeeded(performed target [| warning "refresh" |] [||] PublicationNotApplicable))
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value -> Succeeded(performed value [| warning "update" |] [| "same.txt"; "update.txt" |] PublicationNotApplicable))
                    (fun value -> Succeeded(performed value [| warning "publish" |] [| "same.txt"; "publish.txt" |] Published))

            let outcome = expectSucceeded (run steps (request true true target.TargetRevision) None)
            Expect.equal outcome.Effect Performed "Synchronization was performed."
            Expect.equal outcome.Publication Published "Publication comes from publish."
            Expect.equal (outcome.Warnings |> Array.map _.Code) [| "refresh"; "update"; "publish" |] "Warnings are ordered."
            Expect.equal outcome.AffectedPaths [| "same.txt"; "update.txt"; "publish.txt" |] "Affected paths are distinct and ordered."
            assertSequence log [| "active"; "refresh"; "preview"; "update"; "publish" |]

        testCase "publish no-op with no update remains a no-op"
        <| fun () ->
            let log = ResizeArray<string>()
            let state0 = state UpToDate None
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded state0)
                    (fun _ -> failwith "Preview must not run.")
                    (fun _ -> failwith "Update must not run.")
                    (fun value -> OperationResult.noOp (Some "provider no-op") value)

            let outcome = expectSucceeded (run steps (request false true None) None)
            Expect.equal outcome.Effect (NoOp(Some "The workspace and the target are already synchronized.")) "No-op reason is composed."
            assertSequence log [| "active"; "refresh"; "publish" |]

        testCase "a publish failure after update becomes local-only partial with retry recovery"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = state TargetAhead (Some(revision "target"))
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded target)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun _ -> Succeeded(performed (state LocalAhead target.TargetRevision) [||] [| "updated.txt" |] PublicationNotApplicable))
                    (fun _ -> Failed(failure Network "publish_failed"))

            let outcome, observedFailure = expectPartial (run steps (request true true target.TargetRevision) None)
            Expect.equal outcome.Publication LocalOnly "A merge that keeps local revisions is local-only."
            Expect.isTrue observedFailure.StateChanged "The update changed state."
            Expect.equal (observedFailure.RecoveryAction |> Option.map _.Code) (Some SynchronizationCodes.RetryPublishRecovery) "Retry recovery."
            assertSequence log [| "active"; "refresh"; "preview"; "update"; "publish" |]

        testCase "a publish failure with its own recovery keeps that recovery"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = state TargetAhead (Some(revision "target"))
            let ownRecovery = { Code = "provider_recovery"; Instructions = None }
            let publishFailure = { failure Network "publish_failed" with RecoveryAction = Some ownRecovery }
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded target)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value -> Succeeded(performed value [||] [||] PublicationNotApplicable))
                    (fun _ -> Failed publishFailure)

            let _, observedFailure = expectPartial (run steps (request true true target.TargetRevision) None)
            Expect.equal (observedFailure.RecoveryAction |> Option.map _.Code) (Some "provider_recovery") "Provider recovery wins."

        testCase "a publish failure that reports changed state stays failed"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = state TargetAhead (Some(revision "target"))
            let publishFailure = { failure Network "publish_failed" with StateChanged = true }
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded target)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value -> Succeeded(performed value [||] [||] PublicationNotApplicable))
                    (fun _ -> Failed publishFailure)

            match run steps (request true true target.TargetRevision) None with
            | Failed observed ->
                Expect.isTrue observed.StateChanged "Provider failure is still state-changing."
                Expect.equal
                    (observed.RecoveryAction |> Option.map _.Code)
                    (Some SynchronizationCodes.RetryPublishRecovery)
                    "Retry recovery is added."
            | Succeeded _
            | PartiallySucceeded _ -> failtest "Expected failed publish."

        testCase "a publish failure without an update is returned unchanged"
        <| fun () ->
            let log = ResizeArray<string>()
            let publishFailure = { failure Network "publish_failed" with StateChanged = true }
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded (state UpToDate None))
                    (fun _ -> failwith "Preview must not run.")
                    (fun _ -> failwith "Update must not run.")
                    (fun _ -> Failed publishFailure)

            match run steps (request false true None) None with
            | Failed observed -> Expect.equal observed.Code "publish_failed" "Failure is returned unchanged."
            | Succeeded _
            | PartiallySucceeded _ -> failtest "Expected failed publish."

        testCase "a partial publish keeps its failure and merges update warnings"
        <| fun () ->
            let log = ResizeArray<string>()
            let target = state TargetAhead (Some(revision "target"))
            let publishFailure = failure Network "publish_partial"
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded target)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun value -> Succeeded(performed value [| warning "update" |] [| "update.txt" |] PublicationNotApplicable))
                    (fun value -> OperationResult.partiallySucceeded (performed value [| warning "publish" |] [| "publish.txt" |] LocalOnly) publishFailure { Code = "publish-recovery"; Instructions = None })

            let outcome, observedFailure = expectPartial (run steps (request true true target.TargetRevision) None)
            Expect.equal observedFailure.Code "publish_partial" "Publish failure is preserved."
            Expect.equal (outcome.Warnings |> Array.map _.Code) [| "update"; "publish" |] "Partial warnings are merged."
            Expect.equal outcome.AffectedPaths [| "update.txt"; "publish.txt" |] "Partial paths are merged."

        testCase "cancellation before publish returns local-only partial"
        <| fun () ->
            let cancellation = OperationCancellation.Source()
            let log = ResizeArray<string>()
            let target = state TargetAhead (Some(revision "target"))
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded target)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun _ ->
                        cancellation.Cancel()
                        Succeeded(performed (state LocalAhead target.TargetRevision) [||] [||] PublicationNotApplicable))
                    (fun _ -> failwith "Publish must not run.")

            let outcome, observedFailure = expectPartial (run steps (request true true target.TargetRevision) (Some cancellation))
            Expect.equal outcome.Publication LocalOnly "A canceled publish after a merge leaves the local revisions unpublished."
            Expect.equal observedFailure.Category Canceled "Cancellation category."
            assertSequence log [| "active"; "refresh"; "preview"; "update" |]

        testCase "canceled publish after a fast-forward update is publication not applicable"
        <| fun () ->
            let cancellation = OperationCancellation.Source()
            let log = ResizeArray<string>()
            let refreshed = state TargetAhead (Some(revision "target"))
            let updated = state UpToDate (Some(revision "target"))
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded refreshed)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun _ ->
                        cancellation.Cancel()
                        Succeeded(performed updated [||] [||] PublicationNotApplicable))
                    (fun _ -> failwith "Publish must not run.")

            let outcome, observedFailure =
                expectPartial (run steps (request true true refreshed.TargetRevision) (Some cancellation))

            Expect.equal outcome.Publication PublicationNotApplicable "A synchronized fast-forward has no local publication state."
            Expect.equal observedFailure.Category Canceled "Cancellation category."

        testCase "a failed publish after a merge with local revisions is local-only"
        <| fun () ->
            let log = ResizeArray<string>()
            let refreshed = state TargetAhead (Some(revision "target"))
            let updated = state LocalAhead (Some(revision "target"))
            let steps, _, _ =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded refreshed)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun _ -> Succeeded(performed updated [||] [||] PublicationNotApplicable))
                    (fun _ -> Failed(failure Network "publish_failed"))

            let outcome, observedFailure =
                expectPartial (run steps (request true true refreshed.TargetRevision) None)

            Expect.equal outcome.Publication LocalOnly "A merged local revision remains unpublished."
            Expect.equal observedFailure.Code "publish_failed" "The publish failure is preserved."

        testCase "a failed publish after a fast-forward update is publication not applicable"
        <| fun () ->
            let refreshed = state TargetAhead (Some(revision "target"))
            let updated = state UpToDate (Some(revision "target"))
            let steps, _, _ =
                recordingSteps
                    (ResizeArray<string>())
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded refreshed)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun _ -> Succeeded(performed updated [||] [||] PublicationNotApplicable))
                    (fun _ -> Failed { failure Network "publish_failed" with StateChanged = false })

            let outcome, observedFailure =
                expectPartial (run steps (request true true refreshed.TargetRevision) None)

            Expect.equal outcome.Publication PublicationNotApplicable "A fast-forward has no local publication state."
            Expect.equal observedFailure.Code "publish_failed" "The publish failure is preserved."
            Expect.equal
                (observedFailure.RecoveryAction |> Option.map _.Code)
                (Some SynchronizationCodes.RetryPublishRecovery)
                "Retry recovery."

        testCase "a failed publish after a fast-forward keeps the provider's recovery"
        <| fun () ->
            let refreshed = state TargetAhead (Some(revision "target"))
            let updated = state UpToDate (Some(revision "target"))
            let ownRecovery = { Code = "provider_recovery"; Instructions = None }
            let publishFailure = { failure Network "publish_failed" with RecoveryAction = Some ownRecovery }
            let steps, _, _ =
                recordingSteps
                    (ResizeArray<string>())
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded refreshed)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun _ -> Succeeded(performed updated [||] [||] PublicationNotApplicable))
                    (fun _ -> Failed publishFailure)

            let _, observedFailure = expectPartial (run steps (request true true refreshed.TargetRevision) None)

            Expect.equal (observedFailure.RecoveryAction |> Option.map _.Code) (Some "provider_recovery") "Provider recovery is preserved."

        testCase "publish receives the state returned by update"
        <| fun () ->
            let log = ResizeArray<string>()
            let refreshed = state TargetAhead (Some(revision "target"))
            let updated = state UpToDate (Some(revision "target"))
            let steps, _, publishedState =
                recordingSteps
                    log
                    (OperationResult.succeeded false)
                    (OperationResult.succeeded refreshed)
                    (fun _ -> OperationResult.succeeded (preview false false [||]))
                    (fun _ -> Succeeded(performed updated [||] [||] PublicationNotApplicable))
                    (fun value -> Succeeded(performed value [||] [||] Published))

            ignore (expectSucceeded (run steps (request true true refreshed.TargetRevision) None))
            Expect.isTrue (System.Object.ReferenceEquals(updated, publishedState.Value.Value)) "Publish receives the updated state."
            assertSequence log [| "active"; "refresh"; "preview"; "update"; "publish" |]
    ]
