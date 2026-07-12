module VersionControlService.Tests.Contracts.ExtensionProviderSuites

open Fable.Core
open VersionControlService.Abstractions
open VersionControlService.Tests.Contracts.ProviderHarness
open VersionControlService.Tests.Contracts.ProviderHarness.SuiteHelpers
open Vitest

/// Optional extension suites: each runs only when the harness advertises the service.
/// Absent services are absent — never stubs.
let register (harness: ProviderTestHarness) : string * (unit -> int) =
    let profileName = $"{harness.Name} / extension suites"
    let mutable testCount = 0

    Vitest.describe (
        profileName,
        fun () ->
            let profileTest (name: string) (body: unit -> JS.Promise<unit>) =
                testCount <- testCount + 1
                Vitest.test (name, suiteTestOptions, body)

            profileTest "advertised optional services match the harness expectation"
            <| fun () -> promise {
                let! workspace = harness.CreateWorkspace()
                let discovered = discoverFeatures workspace.Session
                Vitest.expect(List.toArray discovered).toEqual (List.toArray (List.sort harness.ExpectedServices))
            }

            if harness.ExpectedServices |> List.contains "text-diff" then
                profileTest "text diff returns content data instead of exceptions"
                <| fun () -> promise {
                    let! workspace = harness.CreateWorkspace()
                    do! workspace.WriteFile "base.txt" "modified base content\n"

                    let textDiff =
                        match workspace.Session.TextDiff with
                        | Some service -> service
                        | None -> failwith "The harness advertises text-diff but the session lacks it."

                    let! diffResult = run (textDiff.GetDiff (mkPath "base.txt") (ctx "text-diff"))

                    match expectValue "text diff" diffResult with
                    | TextContent content -> Vitest.expect(content.Length > 0).toBe (true)
                    | UnsupportedContent _ -> failwith "Expected text content for a text file."
                }

            if harness.ExpectedServices |> List.contains "materialization" then
                profileTest "object materialization lists and hydrates objects"
                <| fun () -> promise {
                    let! workspace = harness.CreateWorkspace()

                    let materialization =
                        match workspace.Session.ObjectMaterialization with
                        | Some service -> service
                        | None -> failwith "The harness advertises materialization but the session lacks it."

                    let! listResult = run (materialization.ListObjects(ctx "list-objects"))
                    expectValue "list objects" listResult |> ignore
                }

            if harness.ExpectedServices |> List.contains "storage-policy" then
                profileTest "storage policy settings round-trip"
                <| fun () -> promise {
                    let! workspace = harness.CreateWorkspace()

                    let storagePolicy =
                        match workspace.Session.StoragePolicy with
                        | Some service -> service
                        | None -> failwith "The harness advertises storage-policy but the session lacks it."

                    let! settingsResult = run (storagePolicy.GetSettings(ctx "get-settings"))
                    expectValue "get storage settings" settingsResult |> ignore
                }

            if harness.ExpectedServices |> List.contains "maintenance" then
                profileTest "maintenance operations return structured results"
                <| fun () -> promise {
                    let! workspace = harness.CreateWorkspace()

                    let maintenance =
                        match workspace.Session.Maintenance with
                        | Some service -> service
                        | None -> failwith "The harness advertises maintenance but the session lacks it."

                    let! pruneResult = run (maintenance.Prune(ctx "prune"))

                    // Success or a classified failure are both fine; hidden errors are not.
                    match pruneResult with
                    | Succeeded _
                    | PartiallySucceeded _ -> ()
                    | Failed failure -> Vitest.expect(failure.Code.Length > 0).toBe (true)
                }

            if harness.ExpectedServices |> List.contains "browser" then
                profileTest "repository browser URLs are data, never exceptions"
                <| fun () -> promise {
                    let! workspace = harness.CreateWorkspace()

                    let browser =
                        match workspace.Session.RepositoryBrowser with
                        | Some service -> service
                        | None -> failwith "The harness advertises browser but the session lacks it."

                    let! urlResult = run (browser.GetRepositoryWebUrl(ctx "web-url"))
                    expectValue "repository web url" urlResult |> ignore
                }
    )

    // Vitest defers describe callbacks to the collection phase, so expose the
    // registered-test count as a getter evaluated at test time.
    profileName, (fun () -> testCount)
