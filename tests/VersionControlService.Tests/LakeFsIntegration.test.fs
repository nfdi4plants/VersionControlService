module VersionControlService.Tests.LakeFsIntegrationTests

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.LakeFs
open VersionControlService.Tests.LakeFsProviderContractTests
open Vitest

let private expectValue operation = function
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure)
    | Failed failure -> failwith $"{operation} failed ({failure.Category}/{failure.Code}): {failure.Message}"

Vitest.describe (
    "lakeFS integration",
    fun () ->
        Vitest.test (
            "lakeFS creates a unique owned server-visible workspace branch",
            TestOptions(timeout = 120000),
            fun () -> promise {
                if not (integrationEnabled ()) then
                    return failwith "lakeFS integration skipped: Docker not available"

                let harness = createLakeFsHarness ()

                try
                    let! first = harness.CreateWorkspace()
                    let! second = harness.CreateLinkedWorkspace first

                    let firstIndex = LakeFsWorkspaceIndex.load first.Binding.WorkspaceRoot
                    let secondIndex = LakeFsWorkspaceIndex.load second.Binding.WorkspaceRoot

                    let firstWorkspaceIndex =
                        match firstIndex with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected the first workspace index to be persisted."

                    let secondWorkspaceIndex =
                        match secondIndex with
                        | LakeFsWorkspaceIndex.Loaded index -> index
                        | _ -> failwith "Expected the second workspace index to be persisted."

                    Vitest.expect(firstWorkspaceIndex.WorkspaceBranch).not.toBe (secondWorkspaceIndex.WorkspaceBranch)
                    Vitest.expect(firstWorkspaceIndex.OwnershipToken.Length).toBeGreaterThan (20)
                    Vitest.expect(secondWorkspaceIndex.OwnershipToken.Length).toBeGreaterThan (20)

                    let firstLeaf =
                        first.Binding.WorkspaceRoot.Replace('\\', '/').TrimEnd('/').Split('/')
                        |> Array.last
                        |> _.ToLowerInvariant()

                    Vitest.expect(firstWorkspaceIndex.WorkspaceBranch).toContain (firstLeaf)

                    let! refsResult =
                        first.Session.Core.ListRefs(OperationContext.detached "integration-list-refs")
                        |> Async.StartAsPromise

                    let logicalRefs = expectValue "list refs" refsResult

                    Vitest.expect(
                        logicalRefs
                        |> Array.exists (fun reference ->
                            reference.Name = firstWorkspaceIndex.WorkspaceBranch
                            || reference.Name = secondWorkspaceIndex.WorkspaceBranch)
                    ).toBe (false)

                    let parsedLocation =
                        LakeFsTypes.LakeFsLocation.tryParse first.Binding.Location.ProviderLocation
                        |> Result.defaultWith failwith

                    let! branches =
                        LakeFsApi.listBranches
                            (connection ())
                            parsedLocation.Repository
                            (OperationContext.detached "integration-list-server-branches")
                        |> Async.StartAsPromise

                    let serverBranches = branches |> Result.defaultWith (fun failure -> failwith failure.Message)
                    Vitest.expect(serverBranches |> Array.map _.Id).toContain (firstWorkspaceIndex.WorkspaceBranch)
                    Vitest.expect(serverBranches |> Array.map _.Id).toContain (secondWorkspaceIndex.WorkspaceBranch)
                    do! harness.Cleanup()
                with error ->
                    do! harness.Cleanup()
                    return raise error
            }
        )
)
