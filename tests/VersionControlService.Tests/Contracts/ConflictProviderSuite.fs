module VersionControlService.Tests.Contracts.ConflictProviderSuite

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

let private expectHandleRejection (operationName: string) (result: OperationResult<'T>) =
    let failure = expectFailure operationName result
    Vitest.expect(failure.Category).toEqual (Concurrency)
    Vitest.expect(failure.Code).toBe (ConformanceCodes.PreconditionFailed)

    Vitest
        .expect(failure.RecoveryAction |> Option.map _.Code)
        .toEqual (Some ConflictRecovery.RefreshConflictSession)

/// Opens a real conflict: the target and the workspace change base.txt differently
/// (the workspace side saved as a revision, matching a consumer save-then-update
/// flow), then Update reports conflicts and opens a provider-managed session.
let private openConflict (harness: ProviderTestHarness) = promise {
    let! workspace = harness.CreateWorkspace()

    do!
        harness.AdvanceTarget workspace [|
            {
                Path = "base.txt"
                Content = Some "target version\n"
            }
        |]

    do! workspace.WriteFile "base.txt" "workspace version\n"
    let! saveStatus = getStatus workspace

    let! saveResult =
        run (
            workspace.Session.Core.CreateRevision
                {
                    Message = "workspace conflicting change"
                    Paths = [| mkPath "base.txt" |]
                    ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                }
                (ctx "conflict-save")
        )

    expectPerformed "workspace conflicting revision" saveResult |> ignore
    let! status = getStatus workspace

    let! updateResult =
        run (
            (syncService workspace.Session).Update
                { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                (ctx "conflict-update")
        )

    let failure = carriedFailure "conflicting update" updateResult
    Vitest.expect(failure.Code).toBe (ConformanceCodes.ConflictsDetected)

    let! sessionResult = run ((conflictService workspace.Session).GetActiveSession(ctx "conflict-session"))

    match expectValue "active conflict session" sessionResult with
    | Some summary -> return workspace, summary
    | None -> return failwith "Expected an active conflict session after a conflicting update."
}

/// Conflict-session profile: real conflicts, candidate selection, resolved content,
/// rotating opaque handles, terminal closure, mutation-free stale rejection, and the
/// deterministic finalize race.
let register (harness: ProviderTestHarness) : string * (unit -> int) =
    let profileName = $"{harness.Name} / conflict-session profile"
    let mutable testCount = 0

    Vitest.describe (
        profileName,
        fun () ->
            let profileTest (name: string) (body: unit -> JS.Promise<unit>) =
                testCount <- testCount + 1
                Vitest.test (name, suiteTestOptions, body)

            profileTest "a conflicting update opens a provider-managed conflict session"
            <| fun () -> promise {
                let! _, summary = openConflict harness

                Vitest.expect(summary.Items.Length).toBe (1)

                let item = summary.Items[0]
                Vitest.expect(RepositoryPath.value item.Path).toBe ("base.txt")
                Vitest.expect(item.Candidates.Length >= 2).toBe (true)
                Vitest.expect(item.SupportsResolvedContent).toBe (true)
                Vitest.expect(item.Candidates |> Array.forall (fun candidate -> Option.isNone candidate.Object)).toBe (true)

                let candidateIds = item.Candidates |> Array.map _.CandidateId
                Vitest.expect(candidateIds |> Array.contains "workspace").toBe (true)
                Vitest.expect(candidateIds |> Array.contains "target").toBe (true)
            }

            profileTest "candidate selection resolves an item and rotates the handle"
            <| fun () -> promise {
                let! workspace, summary = openConflict harness
                let! status = getStatus workspace

                let! resolveResult =
                    run (
                        (conflictService workspace.Session).Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (ctx "resolve-pick")
                    )

                let resolution = expectValue "candidate resolution" resolveResult
                Vitest.expect(resolution.RefreshedHandle.SessionId).toBe (summary.Handle.SessionId)
                Vitest.expect(resolution.RefreshedHandle.Version = summary.Handle.Version).toBe (false)
                Vitest.expect(resolution.RemainingItems.Length).toBe (0)
            }

            profileTest "supplied resolved content is accepted for text conflicts"
            <| fun () -> promise {
                let! workspace, summary = openConflict harness
                let! status = getStatus workspace

                let! resolveResult =
                    run (
                        (conflictService workspace.Session).Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = SupplyResolvedContent "hand-merged content\n"
                            }
                            (ctx "resolve-content")
                    )

                let resolution = expectValue "supplied-content resolution" resolveResult
                let! afterResolve = getStatus workspace

                let! finalizeResult =
                    run (
                        (conflictService workspace.Session).Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = afterResolve.WorkspaceVersion
                                Message = Some "merge with supplied content"
                            }
                            (ctx "finalize-content")
                    )

                expectValue "finalize" finalizeResult |> ignore

                let! merged = workspace.ReadFile "base.txt"
                Vitest.expect(merged).toEqual (Some "hand-merged content\n")
            }

            profileTest "stale, foreign, or closed handles are rejected without mutation"
            <| fun () -> promise {
                let! workspace, summary = openConflict harness
                let! status = getStatus workspace
                let conflicts = conflictService workspace.Session

                // Rotate the handle with a real resolution, keeping the old handle.
                let! resolveResult =
                    run (
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "workspace"
                            }
                            (ctx "rotate")
                    )

                expectValue "rotation resolution" resolveResult |> ignore
                let! afterRotate = getStatus workspace

                // Replaying the stale handle is rejected before mutation.
                let! staleResult =
                    run (
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = afterRotate.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (ctx "stale-replay")
                    )

                expectHandleRejection "stale handle replay" staleResult

                // A foreign handle is rejected the same way.
                let! foreignResult =
                    run (
                        conflicts.Finalize
                            {
                                Handle = {
                                    SessionId = "foreign-session"
                                    Version = "1"
                                }
                                ExpectedWorkspaceVersion = afterRotate.WorkspaceVersion
                                Message = None
                            }
                            (ctx "foreign-finalize")
                    )

                expectHandleRejection "foreign handle finalize" foreignResult

                // No mutation happened: the workspace version is unchanged.
                let! finalStatus = getStatus workspace
                Vitest.expect(finalStatus.WorkspaceVersion).toBe (afterRotate.WorkspaceVersion)
            }

            profileTest "refresh and deliberate retry recover a live session after a stale rejection"
            <| fun () -> promise {
                let! workspace, summary = openConflict harness
                let! status = getStatus workspace
                let conflicts = conflictService workspace.Session

                // A stale rejection (wrong workspace version) leaves the session live.
                let! staleResult =
                    run (
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = "not-the-current-version"
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (ctx "stale-version")
                    )

                expectHandleRejection "stale workspace version" staleResult

                // Refresh supplies the live handle; the deliberate retry succeeds.
                let! refreshedSession = run (conflicts.GetActiveSession(ctx "refresh-session"))

                match expectValue "refreshed session" refreshedSession with
                | Some liveSummary ->
                    let! retryResult =
                        run (
                            conflicts.Resolve
                                {
                                    Handle = liveSummary.Handle
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                    Path = mkPath "base.txt"
                                    Resolution = PickCandidate "target"
                                }
                                (ctx "deliberate-retry")
                        )

                    expectValue "deliberate retry" retryResult |> ignore
                | None -> failwith "Expected the conflict session to stay live after a stale rejection."
            }

            profileTest "finalize verifies the destination and closes the handle"
            <| fun () -> promise {
                let! workspace, summary = openConflict harness
                let! status = getStatus workspace
                let conflicts = conflictService workspace.Session

                let! resolveResult =
                    run (
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (ctx "resolve-before-finalize")
                    )

                let resolution = expectValue "resolution before finalize" resolveResult
                let! afterResolve = getStatus workspace

                let! finalizeResult =
                    run (
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = afterResolve.WorkspaceVersion
                                Message = Some "finalize conflict"
                            }
                            (ctx "finalize")
                    )

                let mergedRevision = expectValue "finalize" finalizeResult
                Vitest.expect(mergedRevision.IsSome).toBe (true)

                let! closedSession = run (conflicts.GetActiveSession(ctx "closed-session"))
                Vitest.expect((expectValue "closed session" closedSession).IsNone).toBe (true)

                // The terminal handle is closed: replay is rejected without mutation.
                let! afterFinalize = getStatus workspace

                let! replayResult =
                    run (
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = afterFinalize.WorkspaceVersion
                                Message = None
                            }
                            (ctx "replay-finalize")
                    )

                expectHandleRejection "closed handle replay" replayResult

                // The picked target content is materialized.
                let! content = workspace.ReadFile "base.txt"
                Vitest.expect(content).toEqual (Some "target version\n")
            }

            profileTest "cancel closes the session and restores workspace consistency"
            <| fun () -> promise {
                let! workspace, summary = openConflict harness
                let! status = getStatus workspace
                let conflicts = conflictService workspace.Session

                let! cancelResult =
                    run (
                        conflicts.Cancel
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "cancel")
                    )

                expectValue "cancel" cancelResult |> ignore

                let! closedSession = run (conflicts.GetActiveSession(ctx "after-cancel"))
                Vitest.expect((expectValue "session after cancel" closedSession).IsNone).toBe (true)

                // The pre-update workspace content is restored.
                let! content = workspace.ReadFile "base.txt"
                Vitest.expect(content).toEqual (Some "workspace version\n")
            }

            profileTest "a deterministic finalize race reports expected and observed revisions"
            <| fun () -> promise {
                let! workspace, summary = openConflict harness
                let! status = getStatus workspace
                let conflicts = conflictService workspace.Session

                let! resolveResult =
                    run (
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (ctx "race-resolve")
                    )

                let resolution = expectValue "resolution before race" resolveResult
                let! afterResolve = getStatus workspace

                // Arm the destination advance between the finalize pre-check and its verification.
                do!
                    harness.ArmDestinationRace workspace [|
                        {
                            Path = "raced.txt"
                            Content = Some "raced content\n"
                        }
                    |]

                let! finalizeResult =
                    run (
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = afterResolve.WorkspaceVersion
                                Message = Some "finalize during race"
                            }
                            (ctx "race-finalize")
                    )

                let failure = carriedFailure "raced finalize" finalizeResult
                Vitest.expect(failure.Category).toEqual (Concurrency)
                Vitest.expect(failure.RevisionEvidence.Length >= 2).toBe (true)
                Vitest.expect(failure.RecoveryAction.IsSome).toBe (true)
            }
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
