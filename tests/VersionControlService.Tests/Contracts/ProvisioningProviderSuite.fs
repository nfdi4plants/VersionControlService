module VersionControlService.Tests.Contracts.ProvisioningProviderSuite

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

/// Provisioning profile: initialize, clone, bind/re-target, nonempty-target behavior,
/// access-intent verification, and dependency diagnostics.
let register (harness: ProviderTestHarness) : string * (unit -> int) =
    let profileName = $"{harness.Name} / provisioning profile"
    let mutable testCount = 0

    Vitest.describe (
        profileName,
        fun () ->
            let profileTest (name: string) (body: unit -> JS.Promise<unit>) =
                testCount <- testCount + 1
                Vitest.test (name, suiteTestOptions, body)

            profileTest "initializes a new workspace and opens a session"
            <| fun () -> promise {
                let! targetPath = harness.CreateLocalPath()

                let! initializeResult =
                    run (
                        harness.Factory.Initialize
                            {
                                TargetPath = targetPath
                                Location = None
                            }
                            (ctx "initialize")
                    )

                let binding = expectValue "initialize" initializeResult
                Vitest.expect(binding.SchemaVersion).toBe (WorkspaceBinding.CurrentSchemaVersion)

                let! openResult = run (harness.Factory.Open binding (ctx "open-initialized"))
                let session = expectValue "open initialized workspace" openResult

                let! status = sessionStatus session
                Vitest.expect(status.WorkspaceVersion.Length > 0).toBe (true)
            }

            profileTest "clones an existing repository location"
            <| fun () -> promise {
                let! anchor = harness.CreateWorkspace()
                let! clonePath = harness.CreateLocalPath()

                let! cloneResult =
                    run (
                        harness.Factory.Clone
                            {
                                Location = anchor.Binding.Location
                                TargetPath = clonePath
                                TargetRef = None
                                MaterializeAllObjects = true
                            }
                            (ctx "clone")
                    )

                let binding = expectValue "clone" cloneResult
                let! openResult = run (harness.Factory.Open binding (ctx "open-clone"))
                let session = expectValue "open cloned workspace" openResult

                let! status = sessionStatus session
                Vitest.expect(status.Changes.Length).toBe (0)

                match status.Synchronization with
                | Some sync -> Vitest.expect(sync.Relationship).toEqual (UpToDate)
                | None -> failwith "Expected synchronization state in a cloned workspace."
            }

            profileTest "bind re-targets an existing workspace without cloning"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let! otherWorkspace = harness.CreateWorkspace()

                // Give the other target a distinguishable head.
                do!
                    harness.AdvanceTarget otherWorkspace [|
                        {
                            Path = "other-target.txt"
                            Content = Some "other target content\n"
                        }
                    |]

                let! bindResult =
                    run (
                        harness.Factory.Bind
                            {
                                WorkspaceRoot = workspace.Binding.WorkspaceRoot
                                Location = otherWorkspace.Binding.Location
                            }
                            (ctx "bind")
                    )

                let rebound = expectValue "bind" bindResult
                Vitest.expect(rebound.Location.ProviderLocation).toBe (otherWorkspace.Binding.Location.ProviderLocation)

                let! openResult = run (harness.Factory.Open rebound (ctx "open-rebound"))
                let session = expectValue "open rebound workspace" openResult

                let! refreshResult = run ((syncService session).Refresh(ctx "rebound-refresh"))
                let state = expectValue "rebound refresh" refreshResult

                // The rebound session now observes the other target.
                let! otherStatus = getStatus otherWorkspace

                match otherStatus.Synchronization with
                | Some otherSync -> Vitest.expect(state.TargetRevision = otherSync.TargetRevision).toBe (true)
                | None -> failwith "Expected synchronization state on the other workspace."
            }

            profileTest "cloning into a nonempty target fails structurally"
            <| fun () -> promise {
                let! anchor = harness.CreateWorkspace()

                let! cloneResult =
                    run (
                        harness.Factory.Clone
                            {
                                Location = anchor.Binding.Location
                                TargetPath = anchor.Binding.WorkspaceRoot
                                TargetRef = None
                                MaterializeAllObjects = true
                            }
                            (ctx "clone-nonempty")
                    )

                let failure = expectFailure "clone into nonempty target" cloneResult
                Vitest.expect(failure.Category).toEqual (Validation)
                Vitest.expect(failure.Code).toBe (ConformanceCodes.TargetNotEmpty)
            }

            profileTest "access intents are verified against locations"
            <| fun () -> promise {
                let! goodLocation = harness.CreateLocation()

                let intents = [|
                    ReadIntent
                    WriteObjectIntent
                    CreateRevisionIntent
                    PublishIntent
                |]

                let! goodResult =
                    run (
                        harness.Factory.VerifyLocation
                            {
                                Location = goodLocation
                                Intents = intents
                            }
                            (ctx "verify-good")
                    )

                let goodReport = expectValue "verify authorized location" goodResult
                Vitest.expect(goodReport.DeniedIntents.Length).toBe (0)
                Vitest.expect(goodReport.GrantedIntents.Length).toBe (intents.Length)

                let! unauthorizedLocation = harness.CreateUnauthorizedLocation()

                let! unauthorizedResult =
                    run (
                        harness.Factory.VerifyLocation
                            {
                                Location = unauthorizedLocation
                                Intents = intents
                            }
                            (ctx "verify-unauthorized")
                    )

                // Either a structured failure or a report with denied intents is valid;
                // silently granting everything is not.
                match unauthorizedResult with
                | Succeeded outcome -> Vitest.expect(outcome.Value.DeniedIntents.Length > 0).toBe (true)
                | PartiallySucceeded(_, failure)
                | Failed failure ->
                    Vitest
                        .expect(failure.Category = Authorization || failure.Category = Authentication)
                        .toBe (true)
            }

            profileTest "dependency diagnostics report components with remediation"
            <| fun () -> promise {
                let! dependenciesResult = run (harness.Factory.CheckDependencies(ctx "dependencies"))
                let dependencies = expectValue "check dependencies" dependenciesResult

                for dependency in dependencies do
                    Vitest.expect(dependency.Component.Length > 0).toBe (true)

                    if not dependency.Installed || not dependency.Compatible then
                        Vitest.expect(dependency.Remediation.IsSome).toBe (true)
            }
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
