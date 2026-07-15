module VersionControlService.Tests.GitDependencyTests

open Fable.Core
open VersionControlService.Abstractions
open Vitest

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module NodeProcess = VersionControlService.Runtime.Node.Process

let private context (name: string) = OperationContext.detached name

let private processOutput exitCode stdout stderr : NodeProcess.ProcessOutput = {
    ExitCode = exitCode
    StdOut = stdout
    StdErr = stderr
}

let private expectValue (operationName: string) (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure) ->
        failwith $"{operationName} unexpectedly returned partial success ({failure.Code})."
    | Failed failure ->
        failwith $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private expectFailure (operationName: string) (result: OperationResult<'T>) : OperationFailure =
    match result with
    | Failed failure -> failure
    | Succeeded _
    | PartiallySucceeded _ -> failwith $"Expected {operationName} to fail."

let private dependencyHooks (gitAvailable: bool) (lfsAvailable: bool) (repairConfiguration: bool) =
    let mutable configurationInstalled = false
    let commands = ResizeArray<string[]>()

    let hooks: GitWorkspaceSession.GitSessionHooks = {
        RunProcess =
            Some(fun request _ ->
                async {
                    commands.Add request.Arguments

                    let output =
                        match request.Arguments with
                        | [| "--version" |] when gitAvailable -> processOutput 0 "git version 2.45.1\n" ""
                        | [| "--version" |] -> processOutput 1 "" "git was not found"
                        | [| "lfs"; "version" |] when lfsAvailable -> processOutput 0 "git-lfs/3.6.1 (GitHub; windows amd64; go 1.23.0)\n" ""
                        | [| "lfs"; "version" |] -> processOutput 1 "" "git: 'lfs' is not a git command"
                        | [| "config"; "--global"; "--get"; "filter.lfs.process" |]
                            when gitAvailable && configurationInstalled ->
                            processOutput 0 "git-lfs filter-process\n" ""
                        | [| "config"; "--global"; "--get"; "filter.lfs.process" |] ->
                            processOutput 1 "" ""
                        | [| "lfs"; "install"; "--skip-repo" |] when lfsAvailable ->
                            if repairConfiguration then
                                configurationInstalled <- true

                            processOutput 0 "Git LFS initialized.\n" ""
                        | [| "lfs"; "install"; "--skip-repo" |] ->
                            processOutput 1 "" "git: 'lfs' is not a git command"
                        | _ -> processOutput 1 "" "Unexpected Git command"

                    return OperationResult.succeeded output
                })
        Barrier = None
    }

    hooks, commands

Vitest.describe (
    "Git dependency remediation",
    fun () ->
        Vitest.test (
            "Git dependency remediation is truthful",
            fun () -> promise {
                let missingHooks, _ = dependencyHooks false false false
                let missingFactory = GitWorkspaceSession.createFactory missingHooks
                let! missingResult = Async.StartAsPromise(missingFactory.CheckDependencies(context "missing-dependencies"))
                let missing = expectValue "missing dependencies" missingResult

                for dependencyComponent in [| "git"; "git-lfs"; "git-lfs-configuration" |] do
                    let status = missing |> Array.find (fun entry -> entry.Component = dependencyComponent)
                    Vitest.expect(status.Installed).toBe (false)
                    Vitest.expect(status.Compatible).toBe (false)
                    Vitest.expect(status.Remediation.IsSome).toBe (true)

                let noRepairHooks, _ = dependencyHooks true true false
                let noRepairFactory = GitWorkspaceSession.createFactory noRepairHooks
                let! noRepairResult =
                    Async.StartAsPromise(noRepairFactory.InstallDependency "git-lfs-configuration" (context "repair-not-applied"))

                let noRepairFailure = expectFailure "unhealthy LFS configuration remediation" noRepairResult
                Vitest.expect(noRepairFailure.Category).toEqual (DependencyMissing)
                Vitest.expect(noRepairFailure.Code).toBe ("lfs_configuration_unhealthy")

                let! unknownResult =
                    Async.StartAsPromise(noRepairFactory.InstallDependency "other" (context "unknown-dependency"))

                let unknownFailure = expectFailure "unknown dependency remediation" unknownResult
                Vitest.expect(unknownFailure.Category).toEqual (Unsupported)
                Vitest.expect(unknownFailure.Message.Contains("git-lfs-configuration")).toBe (true)

                let! gitInstallResult =
                    Async.StartAsPromise(noRepairFactory.InstallDependency "git" (context "git-install"))

                let gitInstallFailure = expectFailure "Git executable remediation" gitInstallResult
                Vitest.expect(gitInstallFailure.Category).toEqual (Unsupported)
                Vitest.expect(gitInstallFailure.Message.Contains("Install Git")).toBe (true)

                let! lfsInstallResult =
                    Async.StartAsPromise(noRepairFactory.InstallDependency "git-lfs" (context "git-lfs-install"))

                let lfsInstallFailure = expectFailure "Git LFS executable remediation" lfsInstallResult
                Vitest.expect(lfsInstallFailure.Category).toEqual (Unsupported)
                Vitest.expect(lfsInstallFailure.Message.Contains("Install Git LFS")).toBe (true)

                let repairedHooks, commands = dependencyHooks true true true
                let repairedFactory = GitWorkspaceSession.createFactory repairedHooks
                let! repairedResult =
                    Async.StartAsPromise(repairedFactory.InstallDependency "git-lfs-configuration" (context "repair-applied"))

                let repaired = expectValue "healthy LFS configuration remediation" repairedResult
                Vitest.expect(repaired.Component).toBe ("git-lfs-configuration")
                Vitest.expect(repaired.Installed).toBe (true)
                Vitest.expect(repaired.Compatible).toBe (true)
                Vitest.expect(repaired.Remediation.IsNone).toBe (true)
                Vitest.expect(
                    commands
                    |> Seq.exists (fun command -> command = [| "lfs"; "install"; "--skip-repo" |])
                ).toBe (true)
                Vitest.expect(
                    commands
                    |> Seq.filter (fun command -> command = [| "config"; "--global"; "--get"; "filter.lfs.process" |])
                    |> Seq.length
                ).toBeGreaterThanOrEqual (1)
                let configurationCheckIndex =
                    commands
                    |> Seq.findIndex (fun command -> command = [| "config"; "--global"; "--get"; "filter.lfs.process" |])

                let installationIndex =
                    commands
                    |> Seq.findIndex (fun command -> command = [| "lfs"; "install"; "--skip-repo" |])

                Vitest.expect(configurationCheckIndex > installationIndex).toBe (true)
            }
        )
)
