module VersionControlService.Tests.Contracts.ConsumerWorkflowSuite

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

/// Consumer workflow profile: selected save/update/branch/discard/conflict
/// scenarios expressed without any provider-kind conditional.
let register (harness: ProviderTestHarness) : string * (unit -> int) =
    let profileName = $"{harness.Name} / consumer workflow profile"
    let mutable testCount = 0

    Vitest.describe (
        profileName,
        fun () ->
            let profileTest (name: string) (body: unit -> JS.Promise<unit>) =
                testCount <- testCount + 1
                Vitest.test (name, suiteTestOptions, body)

            profileTest "selected save preserves unselected changes and publishes on demand"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "assay.txt" "assay data\n"
                do! workspace.WriteFile "scratch.txt" "scratch data\n"

                let! status = getStatus workspace

                let! saveResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "save selected assay"
                                Paths = [| mkPath "assay.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "consumer-save")
                    )

                let saved = expectPerformed "selected save" saveResult

                let! afterSave = getStatus workspace
                Vitest.expect(saved.ResultingWorkspaceVersion).toEqual (Some afterSave.WorkspaceVersion)
                Vitest.expect(changePaths afterSave).toEqual ([| "scratch.txt" |])

                let! publishResult =
                    run (
                        (syncService workspace.Session).Publish
                            {
                                ExpectedWorkspaceVersion = afterSave.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "consumer-publish")
                    )

                let published = expectPerformed "publish after save" publishResult
                Vitest.expect(published.Publication).toEqual (Published)

                // The unselected scratch file never left the workspace.
                let! linked = harness.CreateLinkedWorkspace workspace
                let! linkedStatus = getStatus linked

                let! linkedUpdate =
                    run (
                        (syncService linked.Session).Update
                            { ExpectedWorkspaceVersion = linkedStatus.WorkspaceVersion }
                            (ctx "consumer-linked-update")
                    )

                expectOutcome "linked update" linkedUpdate |> ignore

                let! assay = linked.ReadFile "assay.txt"
                let! scratch = linked.ReadFile "scratch.txt"
                Vitest.expect(assay).toEqual (Some "assay data\n")
                Vitest.expect(scratch).toEqual (None)
            }

            profileTest "a pending publish survives failure and retries"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "pending.txt" "pending data\n"

                let! status = getStatus workspace

                let! saveResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "save before broken publish"
                                Paths = [| mkPath "pending.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "consumer-pending-save")
                    )

                let saved = expectPerformed "save" saveResult
                Vitest.expect(saved.Publication).toEqual (LocalOnly)

                do! harness.BreakPublish workspace
                let! preBroken = getStatus workspace

                let! brokenPublish =
                    run (
                        (syncService workspace.Session).Publish
                            {
                                ExpectedWorkspaceVersion = preBroken.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "consumer-broken-publish")
                    )

                let failure = expectFailure "broken publish" brokenPublish
                Vitest.expect(failure.Retryable).toBe (true)

                do! harness.RestorePublish workspace
                let! preRetry = getStatus workspace

                let! retryPublish =
                    run (
                        (syncService workspace.Session).Publish
                            {
                                ExpectedWorkspaceVersion = preRetry.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "consumer-retry-publish")
                    )

                let retried = expectPerformed "retried publish" retryPublish
                Vitest.expect(retried.Publication).toEqual (Published)
            }

            profileTest "update flows into conflict resolution with version picking"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                do!
                    harness.AdvanceTarget workspace [|
                        {
                            Path = "base.txt"
                            Content = Some "collaborator version\n"
                        }
                    |]

                do! workspace.WriteFile "base.txt" "my version\n"

                // Save the local change first, as in a consumer save-then-update flow.
                let! saveStatus = getStatus workspace

                let! saveResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "save my conflicting version"
                                Paths = [| mkPath "base.txt" |]
                                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                            }
                            (ctx "consumer-conflict-save")
                    )

                expectPerformed "conflicting save" saveResult |> ignore

                // Preview before updating.
                let! previewResult = run ((syncService workspace.Session).PreviewUpdate(ctx "consumer-preview"))
                let preview = expectValue "preview" previewResult
                Vitest.expect(preview.WouldCreateConflictSession).toBe (true)

                let! status = getStatus workspace

                let! updateResult =
                    run (
                        (syncService workspace.Session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "consumer-conflict-update")
                    )

                let failure = carriedFailure "conflicting update" updateResult
                Vitest.expect(failure.Code).toBe (ConformanceCodes.ConflictsDetected)

                let conflicts = conflictService workspace.Session
                let! sessionResult = run (conflicts.GetActiveSession(ctx "consumer-conflict-session"))

                let summary =
                    match expectValue "conflict session" sessionResult with
                    | Some summary -> summary
                    | None -> failwith "Expected an active conflict session."

                let! preResolve = getStatus workspace

                let! resolveResult =
                    run (
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = preResolve.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (ctx "consumer-pick")
                    )

                let resolution = expectValue "version pick" resolveResult
                let! preFinalize = getStatus workspace

                let! finalizeResult =
                    run (
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = preFinalize.WorkspaceVersion
                                Message = Some "resolve update conflict"
                            }
                            (ctx "consumer-finalize")
                    )

                expectValue "finalize" finalizeResult |> ignore

                let! resolved = workspace.ReadFile "base.txt"
                Vitest.expect(resolved).toEqual (Some "collaborator version\n")
            }

            profileTest "create-and-switch branch then selected save"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! status = getStatus workspace

                let! createResult =
                    run (
                        workspace.Session.Core.CreateRef
                            {
                                Name = "experiment-branch"
                                BaseRef = None
                                SwitchTo = true
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "consumer-branch")
                    )

                expectValue "create-and-switch" createResult |> ignore

                do! workspace.WriteFile "experiment.txt" "experiment data\n"
                let! branchStatus = getStatus workspace

                Vitest
                    .expect(branchStatus.CurrentRef |> Option.map _.Name)
                    .toEqual (Some "experiment-branch")

                let! saveResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "save on experiment branch"
                                Paths = [| mkPath "experiment.txt" |]
                                ExpectedWorkspaceVersion = branchStatus.WorkspaceVersion
                            }
                            (ctx "consumer-branch-save")
                    )

                expectPerformed "save on branch" saveResult |> ignore
            }

            profileTest "discard restores exactly the selected paths"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "base.txt" "broken edit\n"
                do! workspace.WriteFile "keep.txt" "keep this edit\n"

                let! status = getStatus workspace

                let! discardResult =
                    run (
                        workspace.Session.Core.RestorePaths
                            {
                                Paths = [| mkPath "base.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "consumer-discard")
                    )

                expectValue "discard" discardResult |> ignore

                let! baseContent = workspace.ReadFile "base.txt"
                let! keptContent = workspace.ReadFile "keep.txt"
                Vitest.expect(baseContent).toEqual (Some "base content\n")
                Vitest.expect(keptContent).toEqual (Some "keep this edit\n")
            }

            profileTest "supplied resolved content completes a text conflict"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                do!
                    harness.AdvanceTarget workspace [|
                        {
                            Path = "base.txt"
                            Content = Some "their text\n"
                        }
                    |]

                do! workspace.WriteFile "base.txt" "our text\n"
                let! saveStatus = getStatus workspace

                let! saveResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "save our text"
                                Paths = [| mkPath "base.txt" |]
                                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                            }
                            (ctx "consumer-text-save")
                    )

                expectPerformed "text save" saveResult |> ignore
                let! status = getStatus workspace

                let! updateResult =
                    run (
                        (syncService workspace.Session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "consumer-text-update")
                    )

                carriedFailure "conflicting update" updateResult |> ignore

                let conflicts = conflictService workspace.Session
                let! sessionResult = run (conflicts.GetActiveSession(ctx "consumer-text-session"))

                let summary =
                    match expectValue "conflict session" sessionResult with
                    | Some summary -> summary
                    | None -> failwith "Expected an active conflict session."

                Vitest.expect(summary.Items[0].SupportsResolvedContent).toBe (true)

                let! preResolve = getStatus workspace

                let! resolveResult =
                    run (
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = preResolve.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = SupplyResolvedContent "our text\ntheir text\n"
                            }
                            (ctx "consumer-supply")
                    )

                let resolution = expectValue "supplied content" resolveResult
                let! preFinalize = getStatus workspace

                let! finalizeResult =
                    run (
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = preFinalize.WorkspaceVersion
                                Message = Some "merge both texts"
                            }
                            (ctx "consumer-text-finalize")
                    )

                expectValue "finalize" finalizeResult |> ignore

                let! merged = workspace.ReadFile "base.txt"
                Vitest.expect(merged).toEqual (Some "our text\ntheir text\n")
            }

            profileTest "optional object controls follow service presence"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                // Consumers enable object controls only when the services exist.
                let objectControlsVisible =
                    workspace.Session.ObjectMaterialization.IsSome
                    || workspace.Session.StoragePolicy.IsSome

                let expectedVisible =
                    harness.ExpectedServices |> List.contains "materialization"
                    || harness.ExpectedServices |> List.contains "storage-policy"

                Vitest.expect(objectControlsVisible).toBe (expectedVisible)
            }

            profileTest "a save is one revision and one synchronize"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "saved.txt" "saved content\n"
                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "save once"
                                Paths = [| mkPath "saved.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "consumer-save-once")
                    )

                expectPerformed "save revision" revisionResult |> ignore
                let! afterSave = getStatus workspace
                let! synchronizeResult =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = afterSave.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "consumer-save-synchronize")
                    )

                let firstOutcome = expectPerformed "save synchronize" synchronizeResult
                Vitest.expect(firstOutcome.Publication).toEqual (Published)
                let! linked = harness.CreateLinkedWorkspace workspace
                let! saved = linked.ReadFile "saved.txt"
                Vitest.expect(saved).toEqual (Some "saved content\n")

                do!
                    harness.AdvanceTarget workspace [|
                        { Path = "remote.txt"; Content = Some "remote content\n" }
                    |]
                let! beforeUpdate = getStatus workspace
                let! updateResult =
                    run (
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "consumer-save-follow-up")
                    )

                expectPerformed "follow-up synchronize" updateResult |> ignore
                let! remote = workspace.ReadFile "remote.txt"
                Vitest.expect(remote).toEqual (Some "remote content\n")
                let! finalStatus = getStatus workspace

                match finalStatus.Synchronization with
                | Some synchronization -> Vitest.expect(synchronization.Relationship).toEqual (UpToDate)
                | None -> failwith "Expected synchronization state."
            }
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
