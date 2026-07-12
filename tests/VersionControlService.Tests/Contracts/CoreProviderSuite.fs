module VersionControlService.Tests.Contracts.CoreProviderSuite

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

/// Core profile: literal selected revisions, unrelated-change preservation, exact
/// restore, refs, status versions, validation, no exception leakage, and byte-exact
/// path behavior including materialization collisions.
let register (harness: ProviderTestHarness) : string * (unit -> int) =
    let profileName = $"{harness.Name} / core profile"
    let mutable testCount = 0

    Vitest.describe (
        profileName,
        fun () ->
            let profileTest (name: string) (body: unit -> JS.Promise<unit>) =
                testCount <- testCount + 1
                Vitest.test (name, suiteTestOptions, body)

            profileTest "creates revisions from exact literal selected paths and preserves unselected changes"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "a[1].txt" "bracket-changed\n"
                do! workspace.WriteFile "a1.txt" "plain-changed\n"

                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "select the bracket file only"
                                Paths = [| mkPath "a[1].txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "literal-revision")
                    )

                let outcome = expectPerformed "literal selected revision" revisionResult
                Vitest.expect(Array.sort outcome.AffectedPaths).toEqual ([| "a[1].txt" |])

                let! afterStatus = getStatus workspace
                Vitest.expect(changePaths afterStatus).toEqual ([| "a1.txt" |])

                // Wildcard characters are literal file names, never patterns.
                let! wildcardResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "wildcard is not a pathspec"
                                Paths = [| mkPath "*.txt" |]
                                ExpectedWorkspaceVersion = afterStatus.WorkspaceVersion
                            }
                            (ctx "wildcard-revision")
                    )

                expectFailure "wildcard selected revision" wildcardResult |> ignore

                let! finalStatus = getStatus workspace
                Vitest.expect(changePaths finalStatus).toEqual ([| "a1.txt" |])
                Vitest.expect(finalStatus.WorkspaceVersion).toBe (afterStatus.WorkspaceVersion)
            }

            profileTest "a failed selected revision preserves unrelated workspace state"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "b.txt" "unrelated dirty change\n"

                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "missing path must fail"
                                Paths = [| mkPath "missing.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "failed-revision")
                    )

                let failure = expectFailure "selected revision with a missing path" revisionResult
                Vitest.expect(failure.StateChanged).toBe (false)

                let! afterStatus = getStatus workspace
                Vitest.expect(changePaths afterStatus).toEqual ([| "b.txt" |])
                Vitest.expect(afterStatus.WorkspaceVersion).toBe (status.WorkspaceVersion)

                let! content = workspace.ReadFile "b.txt"
                Vitest.expect(content).toEqual (Some "unrelated dirty change\n")
            }

            profileTest "restores exactly the selected paths"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                do! workspace.WriteFile "base.txt" "modified base\n"
                do! workspace.WriteFile "extra.txt" "extra content\n"

                let! status = getStatus workspace

                let! restoreResult =
                    run (
                        workspace.Session.Core.RestorePaths
                            {
                                Paths = [| mkPath "base.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "restore")
                    )

                expectValue "restore selected path" restoreResult |> ignore

                let! baseContent = workspace.ReadFile "base.txt"
                Vitest.expect(baseContent).toEqual (Some "base content\n")

                let! extraContent = workspace.ReadFile "extra.txt"
                Vitest.expect(extraContent).toEqual (Some "extra content\n")

                let! afterStatus = getStatus workspace
                Vitest.expect(changePaths afterStatus).toEqual ([| "extra.txt" |])
            }

            profileTest "selecting unchanged paths is a NoOp"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! status = getStatus workspace

                let! revisionResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "nothing changed"
                                Paths = [| mkPath "base.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "noop-revision")
                    )

                expectNoOp "unchanged selected revision" revisionResult |> ignore
            }

            profileTest "creates refs with explicit create-only and create-and-switch behavior"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! status = getStatus workspace

                let currentName =
                    match status.CurrentRef with
                    | Some currentRef -> currentRef.Name
                    | None -> failwith "Expected a current ref."

                let! createOnlyResult =
                    run (
                        workspace.Session.Core.CreateRef
                            {
                                Name = "feature-create-only"
                                BaseRef = None
                                SwitchTo = false
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "create-only")
                    )

                expectValue "create-only ref" createOnlyResult |> ignore

                let! afterCreateOnly = getStatus workspace

                Vitest
                    .expect(afterCreateOnly.CurrentRef |> Option.map _.Name)
                    .toEqual (Some currentName)

                let! createSwitchResult =
                    run (
                        workspace.Session.Core.CreateRef
                            {
                                Name = "feature-create-switch"
                                BaseRef = None
                                SwitchTo = true
                                ExpectedWorkspaceVersion = afterCreateOnly.WorkspaceVersion
                            }
                            (ctx "create-switch")
                    )

                expectValue "create-and-switch ref" createSwitchResult |> ignore

                let! afterSwitch = getStatus workspace

                Vitest
                    .expect(afterSwitch.CurrentRef |> Option.map _.Name)
                    .toEqual (Some "feature-create-switch")

                let! refsResult = run (workspace.Session.Core.ListRefs(ctx "list-refs"))
                let refs = expectValue "list refs" refsResult
                let refNames = refs |> Array.map _.Name

                Vitest.expect(refNames |> Array.contains "feature-create-only").toBe (true)
                Vitest.expect(refNames |> Array.contains "feature-create-switch").toBe (true)
            }

            profileTest "switch preflight reports destructive risk"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                let! riskyRef =
                    harness.SeedRevisionOnNewRef workspace "risky-ref" [| "base.txt", "different remote content\n" |]

                let! safeRef = harness.SeedRevisionOnNewRef workspace "safe-ref" [| "other.txt", "other content\n" |]

                do! workspace.WriteFile "base.txt" "dirty local content\n"
                let! status = getStatus workspace

                let! riskyPreflight =
                    run (
                        workspace.Session.Core.PreflightSwitchRef
                            {
                                TargetRef = riskyRef
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "risky-preflight")
                    )

                let risky = expectValue "risky preflight" riskyPreflight
                Vitest.expect(risky.IsSafe).toBe (false)
                Vitest.expect(pathValues risky.PathsAtRisk |> Array.contains "base.txt").toBe (true)

                let! safePreflight =
                    run (
                        workspace.Session.Core.PreflightSwitchRef
                            {
                                TargetRef = safeRef
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "safe-preflight")
                    )

                let safe = expectValue "safe preflight" safePreflight
                Vitest.expect(safe.IsSafe).toBe (true)
            }

            profileTest "status versions are stable across pure reads and change after mutation"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                let! firstStatus = getStatus workspace
                let! secondStatus = getStatus workspace
                Vitest.expect(secondStatus.WorkspaceVersion).toBe (firstStatus.WorkspaceVersion)

                do! workspace.WriteFile "token-change.txt" "content\n"

                let! thirdStatus = getStatus workspace
                Vitest.expect(thirdStatus.WorkspaceVersion = firstStatus.WorkspaceVersion).toBe (false)
            }

            profileTest "operations return structured failures instead of throwing"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! status = getStatus workspace

                let! emptySelection =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "no paths"
                                Paths = [||]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "empty-selection")
                    )

                let emptyFailure = expectFailure "empty selection" emptySelection
                Vitest.expect(emptyFailure.Category).toEqual (Validation)

                let bogusRef =
                    match ProviderRef.tryCreate "bogus:does-not-exist" with
                    | Ok reference -> reference
                    | Error message -> failwith message

                let! switchResult =
                    run (
                        workspace.Session.Core.SwitchRef
                            {
                                TargetRef = bogusRef
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "bogus-switch")
                    )

                let switchFailure = expectFailure "switch to a bogus ref" switchResult
                Vitest.expect(switchFailure.Category = NotFound || switchFailure.Category = Validation).toBe (true)

                // A stale mutation is rejected with the stable concurrency code.
                do! workspace.WriteFile "stale-probe.txt" "content\n"

                let! staleResult =
                    run (
                        workspace.Session.Core.CreateRevision
                            {
                                Message = "stale token"
                                Paths = [| mkPath "stale-probe.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "stale-mutation")
                    )

                let staleFailure = expectFailure "stale mutation" staleResult
                Vitest.expect(staleFailure.Category).toEqual (Concurrency)
                Vitest.expect(staleFailure.Code).toBe (ConformanceCodes.PreconditionFailed)
            }

            profileTest "NFC and NFD spellings are distinct provider keys"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                let nfcName = "café.txt"
                let nfdName = "café.txt"

                let! unicodeRef =
                    harness.SeedRevisionOnNewRef
                        workspace
                        "unicode-ref"
                        [| nfcName, "nfc content\n"; nfdName, "nfd content\n" |]

                let! status = getStatus workspace

                let! switchResult =
                    run (
                        workspace.Session.Core.SwitchRef
                            {
                                TargetRef = unicodeRef
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "unicode-switch")
                    )

                if workspace.LocalFileSystemAliasesNormalization then
                    // The provider keys stay distinct, so an aliasing materializer must
                    // fail with a structured collision instead of silently overwriting.
                    let failure = carriedFailure "unicode switch" switchResult
                    Vitest.expect(failure.Code).toBe (ConformanceCodes.PathCollision)
                    Vitest.expect(failure.AffectedPaths.Length >= 2).toBe (true)
                else
                    expectValue "unicode switch" switchResult |> ignore
                    let! nfcContent = workspace.ReadFile nfcName
                    let! nfdContent = workspace.ReadFile nfdName
                    Vitest.expect(nfcContent).toEqual (Some "nfc content\n")
                    Vitest.expect(nfdContent).toEqual (Some "nfd content\n")
            }

            profileTest "materialization collisions and unrepresentable names fail structurally"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()

                let! caseRef =
                    harness.SeedRevisionOnNewRef
                        workspace
                        "case-ref"
                        [| "Case.txt", "upper content\n"; "case.txt", "lower content\n" |]

                let! status = getStatus workspace

                let! caseSwitch =
                    run (
                        workspace.Session.Core.SwitchRef
                            {
                                TargetRef = caseRef
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "case-switch")
                    )

                if workspace.LocalFileSystemAliasesCase then
                    let failure = carriedFailure "case-collision switch" caseSwitch
                    Vitest.expect(failure.Code).toBe (ConformanceCodes.PathCollision)
                    Vitest.expect(pathValues (failure.AffectedPaths |> Array.map mkPath)).toEqual ([| "Case.txt"; "case.txt" |])
                else
                    expectValue "case switch" caseSwitch |> ignore

                if workspace.LocalFileSystemWindowsRules then
                    let! invalidRef =
                        harness.SeedRevisionOnNewRef workspace "invalid-ref" [| "bad<name>.txt", "content\n" |]

                    let! freshStatus = getStatus workspace

                    let! invalidSwitch =
                        run (
                            workspace.Session.Core.SwitchRef
                                {
                                    TargetRef = invalidRef
                                    ExpectedWorkspaceVersion = freshStatus.WorkspaceVersion
                                }
                                (ctx "invalid-switch")
                        )

                    let failure = carriedFailure "windows-invalid switch" invalidSwitch
                    Vitest.expect(failure.Code).toBe (ConformanceCodes.UnrepresentablePath)
                    Vitest.expect(failure.AffectedPaths |> Array.contains "bad<name>.txt").toBe (true)
            }
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
