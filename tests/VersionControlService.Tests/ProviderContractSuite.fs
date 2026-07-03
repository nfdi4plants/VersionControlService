/// Provider-neutral contract tests. Every Swate-selectable provider must pass this
/// suite through its own harness. Rules for this file:
/// - Only call VersionControlProvider members and harness helpers.
/// - Never invoke git/lakectl or assert on provider-specific terminology.
/// - Only assert behavior the design guarantees for all selectable providers.
module VersionControlService.Tests.ProviderContractSuite

open System
open Fable.Core
open VersionControlService.Contracts.VersionControl
open Vitest

/// A provider workspace prepared for testing plus neutral file access.
/// Paths passed to WriteFile/ReadFile are relative to the workspace root
/// and use forward slashes.
type ProviderWorkspace = {
    Provider: VersionControlProvider
    WorkspacePath: string
    WriteFile: string -> string -> JS.Promise<unit>
    ReadFile: string -> JS.Promise<string>
    Dispose: unit -> JS.Promise<unit>
}

type ProviderContractHarness = {
    ProviderName: string
    CreateWorkspace: unit -> JS.Promise<ProviderWorkspace>
}

let private contractTestOptions = TestOptions(timeout = 120000)

let private expectOk (operationName: string) (result: VersionControlResult<'T>) : 'T =
    match result with
    | Ok outcome -> outcome.Value
    | Error failure -> failwith $"{operationName} failed ({failure.Kind}): {failure.Message}"

let private expectError (operationName: string) (result: VersionControlResult<'T>) : VersionControlFailure =
    match result with
    | Ok _ -> failwith $"Expected {operationName} to fail."
    | Error failure -> failure

let private normalizeNewlines (content: string) = content.Replace("\r\n", "\n")

let private withWorkspace
    (harness: ProviderContractHarness)
    (testBody: ProviderWorkspace -> JS.Promise<unit>)
    : JS.Promise<unit> =
    promise {
        let! workspace = harness.CreateWorkspace()

        try
            do! testBody workspace
            do! workspace.Dispose()
        with error ->
            do! workspace.Dispose()
            return raise error
    }

let private commitFile
    (workspace: ProviderWorkspace)
    (relativePath: string)
    (content: string)
    (message: string)
    : JS.Promise<unit> =
    promise {
        do! workspace.WriteFile relativePath content

        let! commitResult =
            workspace.Provider.Commit
                workspace.WorkspacePath
                {
                    Message = message
                    Paths = [| relativePath |]
                }

        expectOk $"commit {relativePath}" commitResult |> ignore
    }

/// Registers the provider contract suite for one harness.
let registerProviderContractTests (harness: ProviderContractHarness) =
    Vitest.describe (
        $"{harness.ProviderName} provider contract",
        fun () ->
            Vitest.test (
                "initialized workspace reports a clean status",
                contractTestOptions,
                fun () -> promise {
                    do!
                        withWorkspace harness (fun workspace -> promise {
                            let! statusResult = workspace.Provider.GetStatus workspace.WorkspacePath
                            let status = expectOk "status of fresh workspace" statusResult

                            Vitest.expect(status.IsClean).toBe (true)
                            Vitest.expect(status.Files.Length).toBe (0)
                            Vitest.expect(status.IsMergeInProgress).toBe (false)
                            Vitest.expect(status.Conflicted.Length).toBe (0)
                        })
                }
            )

            Vitest.test (
                "a new file appears in status and disappears after commit",
                contractTestOptions,
                fun () -> promise {
                    do!
                        withWorkspace harness (fun workspace -> promise {
                            do! workspace.WriteFile "notes.txt" "hello\n"

                            let! dirtyStatusResult = workspace.Provider.GetStatus workspace.WorkspacePath
                            let dirtyStatus = expectOk "status with new file" dirtyStatusResult

                            Vitest.expect(dirtyStatus.IsClean).toBe (false)

                            let entry =
                                dirtyStatus.Files
                                |> Array.tryFind (fun file -> file.Path = "notes.txt")

                            match entry with
                            | Some file -> Vitest.expect(file.CanCommit).toBe (true)
                            | None -> failwith "Expected notes.txt in status file list."

                            let! commitResult =
                                workspace.Provider.Commit
                                    workspace.WorkspacePath
                                    {
                                        Message = "contract: add notes"
                                        Paths = [| "notes.txt" |]
                                    }

                            let commitOutcome =
                                match commitResult with
                                | Ok outcome -> outcome
                                | Error failure ->
                                    failwith $"commit failed ({failure.Kind}): {failure.Message}"

                            Vitest.expect(String.IsNullOrWhiteSpace commitOutcome.Value).toBe (false)

                            match commitOutcome.Effect with
                            | VersionControlEffect.Performed _ -> Vitest.expect(true).toBe (true)
                            | VersionControlEffect.NoOp _ ->
                                failwith "Commit of real changes must not report NoOp."

                            let! cleanStatusResult = workspace.Provider.GetStatus workspace.WorkspacePath
                            let cleanStatus = expectOk "status after commit" cleanStatusResult

                            Vitest.expect(cleanStatus.IsClean).toBe (true)
                        })
                }
            )

            Vitest.test (
                "selected-path commit leaves other changes uncommitted",
                contractTestOptions,
                fun () -> promise {
                    do!
                        withWorkspace harness (fun workspace -> promise {
                            Vitest
                                .expect(workspace.Provider.Capabilities.SupportsSelectedPathCommit)
                                .toBe (true)

                            do! workspace.WriteFile "a.txt" "a1\n"
                            do! workspace.WriteFile "b.txt" "b1\n"

                            let! baseCommitResult =
                                workspace.Provider.Commit
                                    workspace.WorkspacePath
                                    {
                                        Message = "contract: base"
                                        Paths = [| "a.txt"; "b.txt" |]
                                    }

                            expectOk "base commit" baseCommitResult |> ignore

                            do! workspace.WriteFile "a.txt" "a2\n"
                            do! workspace.WriteFile "b.txt" "b2\n"

                            let! selectedCommitResult =
                                workspace.Provider.Commit
                                    workspace.WorkspacePath
                                    {
                                        Message = "contract: only a"
                                        Paths = [| "a.txt" |]
                                    }

                            expectOk "selected commit" selectedCommitResult |> ignore

                            let! statusResult = workspace.Provider.GetStatus workspace.WorkspacePath
                            let status = expectOk "status after selected commit" statusResult

                            Vitest.expect(status.IsClean).toBe (false)

                            let changedPaths = status.Files |> Array.map _.Path

                            Vitest.expect(changedPaths |> Array.contains "b.txt").toBe (true)
                            Vitest.expect(changedPaths |> Array.contains "a.txt").toBe (false)
                        })
                }
            )

            Vitest.test (
                "commit rejects an empty message and an empty path list",
                contractTestOptions,
                fun () -> promise {
                    do!
                        withWorkspace harness (fun workspace -> promise {
                            do! workspace.WriteFile "c.txt" "c\n"

                            let! emptyMessageResult =
                                workspace.Provider.Commit
                                    workspace.WorkspacePath
                                    {
                                        Message = "   "
                                        Paths = [| "c.txt" |]
                                    }

                            expectError "commit with empty message" emptyMessageResult |> ignore

                            let! emptyPathsResult =
                                workspace.Provider.Commit
                                    workspace.WorkspacePath
                                    {
                                        Message = "contract: no paths"
                                        Paths = [||]
                                    }

                            expectError "commit with empty path list" emptyPathsResult |> ignore
                        })
                }
            )

            Vitest.test (
                "branches can be created, listed, and checked out through provider refs",
                contractTestOptions,
                fun () -> promise {
                    do!
                        withWorkspace harness (fun workspace -> promise {
                            do! commitFile workspace "base.txt" "base\n" "contract: branch base"

                            let branchName = "contract-branch-a"

                            let! createResult =
                                workspace.Provider.CreateBranch
                                    workspace.WorkspacePath
                                    {
                                        Name = branchName
                                        BaseProviderRef = None
                                    }

                            expectOk "create branch" createResult |> ignore

                            let! branchesResult = workspace.Provider.GetBranches workspace.WorkspacePath
                            let branches = expectOk "list branches" branchesResult

                            let created =
                                branches
                                |> Array.tryFind (fun branch -> branch.RefName = branchName)

                            match created with
                            | None -> failwith "Expected the created branch in the branch list."
                            | Some branch ->
                                Vitest.expect(branch.CanCheckout).toBe (true)

                                let! checkoutResult =
                                    workspace.Provider.CheckoutBranch
                                        workspace.WorkspacePath
                                        { ProviderRef = branch.ProviderRef }

                                expectOk "checkout created branch" checkoutResult |> ignore

                                let! statusResult = workspace.Provider.GetStatus workspace.WorkspacePath
                                let status = expectOk "status after checkout" statusResult

                                Vitest.expect(status.Current).toEqual (Some branchName)
                        })
                }
            )

            Vitest.test (
                "discard restores the last committed content",
                contractTestOptions,
                fun () -> promise {
                    do!
                        withWorkspace harness (fun workspace -> promise {
                            do! commitFile workspace "keep.txt" "v1\n" "contract: keep v1"
                            do! workspace.WriteFile "keep.txt" "v2\n"

                            let! discardResult =
                                workspace.Provider.Discard
                                    workspace.WorkspacePath
                                    { Paths = [| "keep.txt" |] }

                            expectOk "discard keep.txt" discardResult |> ignore

                            let! restored = workspace.ReadFile "keep.txt"
                            Vitest.expect(normalizeNewlines restored).toBe ("v1\n")

                            let! statusResult = workspace.Provider.GetStatus workspace.WorkspacePath
                            let status = expectOk "status after discard" statusResult
                            Vitest.expect(status.IsClean).toBe (true)
                        })
                }
            )

            Vitest.test (
                "diff summary honors SupportsDiffLineCounts",
                contractTestOptions,
                fun () -> promise {
                    do!
                        withWorkspace harness (fun workspace -> promise {
                            do! commitFile workspace "diffed.txt" "one\n" "contract: diff base"
                            do! workspace.WriteFile "diffed.txt" "one\ntwo\n"

                            let! summaryResult = workspace.Provider.GetDiffSummary workspace.WorkspacePath
                            let summary = expectOk "diff summary" summaryResult

                            Vitest.expect(summary.Changed >= 1).toBe (true)

                            if workspace.Provider.Capabilities.SupportsDiffLineCounts then
                                Vitest.expect(summary.Insertions.IsSome).toBe (true)
                                Vitest.expect(summary.Deletions.IsSome).toBe (true)
                            else
                                Vitest.expect(summary.Insertions.IsNone).toBe (true)
                                Vitest.expect(summary.Deletions.IsNone).toBe (true)
                        })
                }
            )

            Vitest.test (
                "unsupported merge resolution kinds fail with Unsupported, not silent success",
                contractTestOptions,
                fun () -> promise {
                    do!
                        withWorkspace harness (fun workspace -> promise {
                            if not workspace.Provider.Capabilities.SupportsVersionPickMergeResolution then
                                let! resolutionResult =
                                    workspace.Provider.ConfirmMergeResolution
                                        workspace.WorkspacePath
                                        {
                                            Path = "any.txt"
                                            Resolution = VersionControlMergeResolution.TakeSourceVersion
                                            AutoCommit = false
                                        }

                                let failure = expectError "version-pick resolution" resolutionResult
                                Vitest.expect(failure.Kind).toEqual (VersionControlFailureKind.Unsupported)

                            if not workspace.Provider.Capabilities.SupportsContentMergeResolution then
                                let! resolutionResult =
                                    workspace.Provider.ConfirmMergeResolution
                                        workspace.WorkspacePath
                                        {
                                            Path = "any.txt"
                                            Resolution = VersionControlMergeResolution.Content("x", "y")
                                            AutoCommit = false
                                        }

                                let failure = expectError "content resolution" resolutionResult
                                Vitest.expect(failure.Kind).toEqual (VersionControlFailureKind.Unsupported)
                        })
                }
            )

            Vitest.test (
                "verify remote access fails cleanly when no remote is configured",
                contractTestOptions,
                fun () -> promise {
                    do!
                        withWorkspace harness (fun workspace -> promise {
                            let! result =
                                workspace.Provider.VerifyRemoteAccess
                                    workspace.WorkspacePath
                                    { Remote = None; Branch = None }

                            let failure = expectError "verify remote access without remote" result
                            Vitest.expect(String.IsNullOrWhiteSpace failure.Message).toBe (false)
                        })
                }
            )
    )
