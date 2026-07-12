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
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
