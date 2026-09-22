module VersionControlService.Tests.Contracts.SynchronizationProviderSuite

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

/// Synchronization profile: refresh/preview/update/publish, up-to-date no-ops,
/// divergence, structural stale-target reporting, recoverable publish failure, retry.
let register (harness: ProviderTestHarness) : string * (unit -> int) =
    let profileName = $"{harness.Name} / synchronization profile"
    let mutable testCount = 0

    Vitest.describe (
        profileName,
        fun () ->
            let profileTest (name: string) (body: unit -> JS.Promise<unit>) =
                testCount <- testCount + 1
                Vitest.test (name, suiteTestOptions, body)

            profileTest "refresh observes the target without changing workspace content"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                do!
                    harness.AdvanceTarget workspace [|
                        {
                            Path = "remote-only.txt"
                            Content = Some "remote content\n"
                        }
                    |]

                let! refreshResult = run ((syncService workspace.Session).Refresh(ctx "refresh"))
                let state = expectValue "refresh" refreshResult

                Vitest.expect(state.Relationship).toEqual (TargetAhead)
                Vitest.expect(state.TargetRevision = state.BaseRevision).toBe (false)

                match state.RemoteChangedPaths with
                | Some changed -> Vitest.expect(pathValues changed |> Array.contains "remote-only.txt").toBe (true)
                | None -> ()

                // The workspace content itself is untouched by a refresh.
                let! localFile = workspace.ReadFile "remote-only.txt"
                Vitest.expect(localFile).toEqual (None)
            }

            profileTest "preview reports changed and overlapping paths without mutating"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                do!
                    harness.AdvanceTarget workspace [|
                        {
                            Path = "base.txt"
                            Content = Some "remote base change\n"
                        }
                        {
                            Path = "remote-new.txt"
                            Content = Some "remote new file\n"
                        }
                    |]

                do! workspace.WriteFile "base.txt" "local base change\n"

                let! previewResult = run ((syncService workspace.Session).PreviewUpdate(ctx "preview"))
                let preview = expectValue "preview update" previewResult

                Vitest.expect(pathValues preview.ChangedPaths).toEqual ([| "base.txt"; "remote-new.txt" |])
                Vitest.expect(pathValues preview.OverlappingPaths).toEqual ([| "base.txt" |])
                Vitest.expect(preview.HasDataLossRisk).toBe (true)
                Vitest.expect(preview.WouldCreateConflictSession).toBe (true)

                // Preview is read-only.
                let! localBase = workspace.ReadFile "base.txt"
                Vitest.expect(localBase).toEqual (Some "local base change\n")
            }

            profileTest "an up-to-date update is a NoOp"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! status = getStatus workspace

                let! updateResult =
                    run (
                        (syncService workspace.Session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "noop-update")
                    )

                expectNoOp "up-to-date update" updateResult |> ignore
            }

            profileTest "update incorporates non-conflicting target changes"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                do!
                    harness.AdvanceTarget workspace [|
                        {
                            Path = "incoming.txt"
                            Content = Some "incoming content\n"
                        }
                    |]

                let! status = getStatus workspace

                let! updateResult =
                    run (
                        (syncService workspace.Session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "update")
                    )

                let state = expectValue "update" updateResult
                Vitest.expect(state.Relationship).toEqual (UpToDate)

                let! incoming = workspace.ReadFile "incoming.txt"
                Vitest.expect(incoming).toEqual (Some "incoming content\n")
            }

            profileTest "publish makes local revisions visible on the target and repeats as NoOp"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "published.txt" "published content\n"

                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "add published file"
                                Paths = [| mkPath "published.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "publish-revision")
                    )

                expectPerformed "revision before publish" revisionResult |> ignore

                let! afterRevision = getStatus workspace

                let! publishResult =
                    run (
                        (syncService workspace.Session).Publish
                            {
                                ExpectedWorkspaceVersion = afterRevision.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish")
                    )

                let publishOutcome = expectPerformed "publish" publishResult
                Vitest.expect(publishOutcome.Publication).toEqual (Published)

                // Another workspace bound to the same target can update and see the file.
                let! linked = harness.CreateLinkedWorkspace workspace
                let! linkedStatus = getStatus linked

                let! linkedUpdate =
                    run (
                        (syncService linked.Session).Update
                            { ExpectedWorkspaceVersion = linkedStatus.WorkspaceVersion }
                            (ctx "linked-update")
                    )

                expectOutcome "linked update" linkedUpdate |> ignore

                let! linkedContent = linked.ReadFile "published.txt"
                Vitest.expect(linkedContent).toEqual (Some "published content\n")

                // Publishing again with nothing new is a NoOp.
                let! repeatStatus = getStatus workspace

                let! repeatPublish =
                    run (
                        (syncService workspace.Session).Publish
                            {
                                ExpectedWorkspaceVersion = repeatStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "repeat-publish")
                    )

                expectNoOp "repeat publish" repeatPublish |> ignore
            }

            profileTest "a failed publish is recoverable and retry succeeds"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "retry.txt" "retry content\n"

                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "revision awaiting publish"
                                Paths = [| mkPath "retry.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "retry-revision")
                    )

                let revision = expectPerformed "revision before broken publish" revisionResult

                do! harness.BreakPublish workspace
                let! brokenStatus = getStatus workspace

                let! brokenPublish =
                    run (
                        (syncService workspace.Session).Publish
                            {
                                ExpectedWorkspaceVersion = brokenStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "broken-publish")
                    )

                let failure = expectFailure "broken publish" brokenPublish
                Vitest.expect(failure.Retryable).toBe (true)
                assertRedacted failure.Message

                // The local revision survives the failed publish.
                let! afterFailure = getStatus workspace

                match afterFailure.Synchronization with
                | Some sync -> Vitest.expect(sync.WorkspaceRevision = revision.ResultingRevision).toBe (true)
                | None -> failwith "Expected synchronization state."

                do! harness.RestorePublish workspace
                let! restoredStatus = getStatus workspace

                let! retryPublish =
                    run (
                        (syncService workspace.Session).Publish
                            {
                                ExpectedWorkspaceVersion = restoredStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "retry-publish")
                    )

                let retried = expectPerformed "retried publish" retryPublish
                Vitest.expect(retried.Publication).toEqual (Published)
            }

            profileTest "a stale expected target is reported structurally with revision evidence"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                // Capture the target revision the consumer believes is current.
                let! refreshResult = run ((syncService workspace.Session).Refresh(ctx "stale-refresh"))
                let observedState = expectValue "refresh before staleness" refreshResult

                let staleTarget =
                    match observedState.TargetRevision with
                    | Some revision -> revision
                    | None -> failwith "Expected a target revision."

                // Another client advances the target afterwards.
                do!
                    harness.AdvanceTarget workspace [|
                        {
                            Path = "raced.txt"
                            Content = Some "raced content\n"
                        }
                    |]

                do! workspace.WriteFile "local.txt" "local content\n"
                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "local revision"
                                Paths = [| mkPath "local.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "stale-revision")
                    )

                expectPerformed "local revision" revisionResult |> ignore
                let! prePublish = getStatus workspace

                let! publishResult =
                    run (
                        (syncService workspace.Session).Publish
                            {
                                ExpectedWorkspaceVersion = prePublish.WorkspaceVersion
                                ExpectedTargetRevision = Some staleTarget
                            }
                            (ctx "stale-publish")
                    )

                let failure = carriedFailure "stale publish" publishResult
                Vitest.expect(failure.Category).toEqual (Concurrency)
                Vitest.expect(failure.Code).toBe (ConformanceCodes.PreconditionFailed)

                let evidenceRoles = failure.RevisionEvidence |> Array.map fst
                Vitest.expect(evidenceRoles.Length >= 2).toBe (true)
            }

            profileTest "synchronize with nothing to do is a NoOp and leaves the target alone"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! beforeResult = run ((syncService workspace.Session).Refresh(ctx "sync-noop-before"))
                let before = expectValue "initial refresh" beforeResult
                let! status = getStatus workspace

                let! result =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "synchronize-noop")
                    )

                expectNoOp "synchronize no-op" result |> ignore
                let! afterResult = run ((syncService workspace.Session).Refresh(ctx "sync-noop-after"))
                let after = expectValue "final refresh" afterResult
                Vitest.expect(after.TargetRevision).toEqual (before.TargetRevision)
            }

            profileTest "synchronize updates a workspace behind its target and publishes local revisions in one call"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                do!
                    harness.AdvanceTarget workspace [|
                        { Path = "remote.txt"; Content = Some "remote content\n" }
                    |]

                do! workspace.WriteFile "local.txt" "local content\n"
                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "save local content"
                                Paths = [| mkPath "local.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "synchronize-local-revision")
                    )

                expectPerformed "local revision" revisionResult |> ignore
                let! afterRevision = getStatus workspace

                let! result =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = afterRevision.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "synchronize-update-publish")
                    )

                let outcome = expectPerformed "synchronize update and publish" result
                Vitest.expect(outcome.Publication).toEqual (Published)
                let! remoteFile = workspace.ReadFile "remote.txt"
                let! localFile = workspace.ReadFile "local.txt"
                Vitest.expect(remoteFile).toEqual (Some "remote content\n")
                Vitest.expect(localFile).toEqual (Some "local content\n")

                let! linked = harness.CreateLinkedWorkspace workspace
                let! linkedLocal = linked.ReadFile "local.txt"
                Vitest.expect(linkedLocal).toEqual (Some "local content\n")
                let! finalStatus = getStatus workspace

                match finalStatus.Synchronization with
                | Some synchronization -> Vitest.expect(synchronization.Relationship).toEqual (UpToDate)
                | None -> failwith "Expected synchronization state."
            }

            profileTest "synchronize stops with an acceptance failure and applies the accepted update afterwards"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do!
                    harness.AdvanceTarget workspace [|
                        { Path = "base.txt"; Content = Some "remote base\n" }
                    |]
                do! workspace.WriteFile "base.txt" "local edit\n"
                let! status = getStatus workspace

                let! firstResult =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "synchronize-risk")
                    )

                let firstFailure = expectFailure "synchronize risk" firstResult
                Vitest.expect(firstFailure.Code).toBe (SynchronizationCodes.UpdateWouldOverwriteLocalChanges)
                Vitest.expect(firstFailure.Category).toEqual (Conflict)
                Vitest.expect(firstFailure.AffectedPaths).toEqual ([| "base.txt" |])
                Vitest.expect(firstFailure.RevisionEvidence |> Array.map fst).toEqual ([| "observed_target" |])
                Vitest.expect(firstFailure.RecoveryAction |> Option.map _.Code).toEqual (Some SynchronizationCodes.AcceptUpdateRisksRecovery)
                let! untouched = workspace.ReadFile "base.txt"
                Vitest.expect(untouched).toEqual (Some "local edit\n")

                let observedTarget =
                    firstFailure.RevisionEvidence
                    |> Array.tryFind (fun (role, _) -> role = "observed_target")
                    |> Option.map snd

                let! acceptedStatus = getStatus workspace
                let! acceptedResult =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = acceptedStatus.WorkspaceVersion
                                ExpectedTargetRevision = observedTarget
                                AcceptUpdateRisks = true
                                PublishLocalRevisions = true
                            }
                            (ctx "synchronize-risk-accepted")
                    )

                let acceptedFailure = carriedFailure "accepted synchronize" acceptedResult
                Vitest.expect([| "conflicts_detected"; "update_rejected" |] |> Array.contains acceptedFailure.Code).toBe (true)
                let! afterAccepted = workspace.ReadFile "base.txt"
                let! acceptedWorkspaceStatus = getStatus workspace
                let activeConflict = acceptedWorkspaceStatus.ActiveConflictSession.IsSome
                Vitest.expect(afterAccepted = Some "local edit\n" || activeConflict).toBe (true)
                // Git reports update_rejected for the dirty-file rejection. lakeFS opens a conflict session.
            }

            profileTest "synchronize refuses an accepted target that moved"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do!
                    harness.AdvanceTarget workspace [|
                        { Path = "base.txt"; Content = Some "first remote change\n" }
                    |]
                do! workspace.WriteFile "base.txt" "local edit\n"
                let! status = getStatus workspace

                let! firstResult =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "synchronize-moved-risk")
                    )

                let firstFailure = expectFailure "synchronize moved risk" firstResult
                let oldTarget = firstFailure.RevisionEvidence |> Array.find (fun (role, _) -> role = "observed_target") |> snd
                do!
                    harness.AdvanceTarget workspace [|
                        { Path = "second.txt"; Content = Some "second remote change\n" }
                    |]
                let! movedStatus = getStatus workspace

                let! result =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = movedStatus.WorkspaceVersion
                                ExpectedTargetRevision = Some oldTarget
                                AcceptUpdateRisks = true
                                PublishLocalRevisions = true
                            }
                            (ctx "synchronize-moved-retry")
                    )

                let failure = expectFailure "synchronize moved target" result
                Vitest.expect(failure.Code).toBe (ConformanceCodes.PreconditionFailed)
                Vitest.expect(failure.Category).toEqual (Concurrency)
                Vitest.expect(failure.StateChanged).toBe (false)
                let! local = workspace.ReadFile "base.txt"
                Vitest.expect(local).toEqual (Some "local edit\n")
            }

            profileTest "synchronize refuses an acceptance without an observed target"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! status = getStatus workspace
                let! result =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = true
                                PublishLocalRevisions = false
                            }
                            (ctx "synchronize-missing-acceptance-target")
                    )

                let failure = expectFailure "missing acceptance target" result
                Vitest.expect(failure.Code).toBe (SynchronizationCodes.AcceptanceTargetRequired)
            }

            profileTest "synchronize without publishing leaves local revisions on the workspace"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "local-only.txt" "local only\n"
                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "local-only revision"
                                Paths = [| mkPath "local-only.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "synchronize-local-only-revision")
                    )

                expectPerformed "local-only revision" revisionResult |> ignore
                do!
                    harness.AdvanceTarget workspace [|
                        { Path = "remote-only.txt"; Content = Some "remote only\n" }
                    |]
                let! beforeSync = getStatus workspace

                let! result =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeSync.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = false
                            }
                            (ctx "synchronize-local-only")
                    )

                expectPerformed "synchronize without publish" result |> ignore
                let! remoteFile = workspace.ReadFile "remote-only.txt"
                Vitest.expect(remoteFile).toEqual (Some "remote only\n")
                let! linked = harness.CreateLinkedWorkspace workspace
                let! localFile = linked.ReadFile "local-only.txt"
                Vitest.expect(localFile).toEqual (None)
            }

            profileTest "synchronize with a stale workspace version fails before mutation"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! result =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = "stale"
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "synchronize-stale-workspace")
                    )

                let failure = expectFailure "stale workspace synchronize" result
                Vitest.expect(failure.Code).toBe (ConformanceCodes.PreconditionFailed)
                Vitest.expect(failure.Category).toEqual (Concurrency)
                Vitest.expect(failure.StateChanged).toBe (false)
            }

            profileTest "synchronize refuses while a conflict session is open"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do!
                    harness.AdvanceTarget workspace [|
                        { Path = "base.txt"; Content = Some "remote conflict\n" }
                    |]
                do! workspace.WriteFile "base.txt" "local conflict\n"
                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "conflicting local revision"
                                Paths = [| mkPath "base.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "synchronize-conflict-revision")
                    )

                expectPerformed "conflicting local revision" revisionResult |> ignore
                let! updateStatus = getStatus workspace
                let! updateResult =
                    run (
                        (syncService workspace.Session).Update
                            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                            (ctx "synchronize-open-conflict")
                    )

                let updateFailure = carriedFailure "conflicting update" updateResult
                Vitest.expect(updateFailure.Code).toBe (ConformanceCodes.ConflictsDetected)
                let! conflictStatus = getStatus workspace
                let! synchronizeResult =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = conflictStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = false
                            }
                            (ctx "synchronize-active-conflict")
                    )

                let failure = expectFailure "active conflict synchronize" synchronizeResult
                Vitest.expect(failure.Code).toBe (SynchronizationCodes.ConflictSessionActive)
                Vitest.expect(failure.StateChanged).toBe (false)
            }
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
