module VersionControlService.Tests.ConformanceSuiteDiscoveryTests

open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

// Register every shared suite against the in-memory fake harness under the
// distinct "fake / ..." describe prefix. The real Git and lakeFS registrations
// use their own reserved names in their own test files.
let private fakeHarness = FakeHarness.create ()

let private registrations = [|
    CoreProviderSuite.register fakeHarness
    SynchronizationProviderSuite.register fakeHarness
    ConflictProviderSuite.register fakeHarness
    ProvisioningProviderSuite.register fakeHarness
    OperationalProviderSuite.register fakeHarness
    ExtensionProviderSuites.register fakeHarness
    ConsumerWorkflowSuite.register fakeHarness
|]

Vitest.describe (
    "conformance suite discovery",
    fun () ->
        Vitest.test (
            "exactly seven profiles register tests against the fake harness",
            fun () ->
                Vitest.expect(registrations.Length).toBe (7)

                let expectedProfiles = [|
                    "fake / core profile"
                    "fake / synchronization profile"
                    "fake / conflict-session profile"
                    "fake / provisioning profile"
                    "fake / operational profile"
                    "fake / extension suites"
                    "fake / consumer workflow profile"
                |]

                let registeredProfiles = registrations |> Array.map fst
                Vitest.expect(registeredProfiles).toEqual (expectedProfiles)
                Vitest.expect(registeredProfiles |> Array.exists (fun name -> name.Contains "selectable")).toBe (false)

                // The getters are evaluated now, after Vitest's collection phase
                // has run every describe callback.
                for profileName, getTestCount in registrations do
                    if getTestCount () < 1 then
                        failwith $"Profile '{profileName}' registered no tests."
        )

        Vitest.test (
            "fake synchronize applies the previewed target when the target moves during update",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! workspace = fakeHarness.CreateWorkspace()

                try
                    do!
                        fakeHarness.AdvanceTarget workspace [|
                            { Path = "one.txt"; Content = Some "one\n" }
                        |]

                    do!
                        fakeHarness.ArmDestinationRace workspace [|
                            { Path = "two.txt"; Content = Some "two\n" }
                        |]

                    let! statusResult = workspace.Session.Core.GetStatus(ctx "fake-synchronize-race-status") |> run
                    let status = expectValue "fake synchronize race status" statusResult
                    let! result =
                        (syncService workspace.Session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = false
                            }
                            (ctx "fake-synchronize-race")
                        |> run

                    expectPerformed "fake synchronize race" result |> ignore
                    let! first = workspace.ReadFile "one.txt"
                    let! second = workspace.ReadFile "two.txt"
                    Vitest.expect(first).toEqual (Some "one\n")
                    Vitest.expect(second).toEqual (None)

                    let! refreshResult = (syncService workspace.Session).Refresh(ctx "fake-synchronize-race-refresh") |> run
                    let refreshed = expectValue "fake synchronize race refresh" refreshResult
                    Vitest.expect(refreshed.Relationship).toEqual (TargetAhead)

                    do! fakeHarness.Cleanup()
                with error ->
                    do! fakeHarness.Cleanup()
                    return raise error
            }
        )
)
