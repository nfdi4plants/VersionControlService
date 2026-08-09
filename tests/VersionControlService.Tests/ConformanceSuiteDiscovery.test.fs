module VersionControlService.Tests.ConformanceSuiteDiscoveryTests

open VersionControlService.Tests.Contracts
open VersionControlService.Tests.Contracts.ProviderHarness
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
)
