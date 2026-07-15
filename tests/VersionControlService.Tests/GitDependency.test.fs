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

let private dependencyHooks
    (gitAvailable: bool)
    (lfsVersion: string option)
    (initialFilterProcess: string option)
    (filterProcessAfterInstall: string option)
    =
    let mutable filterProcess = initialFilterProcess
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
                        | [| "lfs"; "version" |] when lfsVersion.IsSome -> processOutput 0 (lfsVersion |> Option.get) ""
                        | [| "lfs"; "version" |] -> processOutput 1 "" "git: 'lfs' is not a git command"
                        | [| "config"; "--global"; "--get"; "filter.lfs.process" |]
                            when gitAvailable && filterProcess.IsSome ->
                            processOutput 0 (filterProcess |> Option.get) ""
                        | [| "config"; "--global"; "--get"; "filter.lfs.process" |] ->
                            processOutput 1 "" ""
                        | [| "lfs"; "install"; "--skip-repo" |] when lfsVersion.IsSome ->
                            filterProcess <- filterProcessAfterInstall

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
                let missingHooks, _ = dependencyHooks false None None None
                let missingFactory = GitWorkspaceSession.createFactory missingHooks
                let! missingResult = Async.StartAsPromise(missingFactory.CheckDependencies(context "missing-dependencies"))
                let missing = expectValue "missing dependencies" missingResult

                for dependencyComponent in [| "git"; "git-lfs"; "git-lfs-configuration" |] do
                    let status = missing |> Array.find (fun entry -> entry.Component = dependencyComponent)
                    Vitest.expect(status.Installed).toBe (false)
                    Vitest.expect(status.Compatible).toBe (false)
                    Vitest.expect(status.Remediation.IsSome).toBe (true)

                let missingLfsHooks, missingLfsCommands = dependencyHooks true None None None
                let missingLfsFactory = GitWorkspaceSession.createFactory missingLfsHooks
                let! missingLfsResult =
                    Async.StartAsPromise(
                        missingLfsFactory.InstallDependency "git-lfs-configuration" (context "missing-lfs-remediation")
                    )

                let missingLfsFailure = expectFailure "missing Git LFS configuration remediation" missingLfsResult
                Vitest.expect(missingLfsFailure.Category).toEqual (Unsupported)
                Vitest.expect(missingLfsFailure.Code).toBe ("manual_install_required")
                Vitest.expect(missingLfsFailure.Message.Contains("Install Git LFS")).toBe (true)
                Vitest.expect(
                    missingLfsCommands
                    |> Seq.exists (fun command -> command = [| "lfs"; "install"; "--skip-repo" |])
                ).toBe (false)

                let incompatibleLfsHooks, incompatibleLfsCommands =
                    dependencyHooks true (Some "git-lfs/3.6.1 (GitHub; windows amd64; go 1.23.0)\n") None None

                let incompatibleLfsFactory = GitWorkspaceSession.createFactory incompatibleLfsHooks
                let! incompatibleLfsResult =
                    Async.StartAsPromise(
                        incompatibleLfsFactory.InstallDependency "git-lfs-configuration" (context "incompatible-lfs-remediation")
                    )

                let incompatibleLfsFailure =
                    expectFailure "incompatible Git LFS configuration remediation" incompatibleLfsResult

                Vitest.expect(incompatibleLfsFailure.Category).toEqual (Unsupported)
                Vitest.expect(incompatibleLfsFailure.Code).toBe ("manual_install_required")
                Vitest.expect(incompatibleLfsFailure.Message.Contains("Install Git LFS")).toBe (true)
                Vitest.expect(
                    incompatibleLfsCommands
                    |> Seq.exists (fun command -> command = [| "lfs"; "install"; "--skip-repo" |])
                ).toBe (false)

                let noRepairHooks, _ =
                    dependencyHooks true (Some "git-lfs/3.7.0 (GitHub; windows amd64; go 1.23.0)\n") None None
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

                let repairedHooks, commands =
                    dependencyHooks
                        true
                        (Some "git-lfs/3.7.0 (GitHub; windows amd64; go 1.23.0)\n")
                        None
                        (Some "git-lfs filter-process\n")
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
                let installationIndex =
                    commands
                    |> Seq.findIndex (fun command -> command = [| "lfs"; "install"; "--skip-repo" |])

                Vitest.expect(
                    commands
                    |> Seq.skip (installationIndex + 1)
                    |> Seq.exists (fun command -> command = [| "config"; "--global"; "--get"; "filter.lfs.process" |])
                ).toBe (true)

                let staleFilter = Some "git-lfs filter-process --skip\n"
                let staleHooks, _ =
                    dependencyHooks
                        true
                        (Some "git-lfs/3.7.0 (GitHub; windows amd64; go 1.23.0)\n")
                        staleFilter
                        staleFilter

                let staleFactory = GitWorkspaceSession.createFactory staleHooks
                let! staleDependenciesResult = Async.StartAsPromise(staleFactory.CheckDependencies(context "stale-filter"))
                let staleDependencies = expectValue "stale filter dependencies" staleDependenciesResult
                let staleFilterStatus =
                    staleDependencies |> Array.find (fun status -> status.Component = "git-lfs-configuration")

                Vitest.expect(staleFilterStatus.Installed).toBe (false)
                Vitest.expect(staleFilterStatus.Compatible).toBe (false)
                Vitest.expect(staleFilterStatus.Remediation.IsSome).toBe (true)

                let! staleInstallResult =
                    Async.StartAsPromise(staleFactory.InstallDependency "git-lfs-configuration" (context "stale-filter-install"))

                let staleInstallFailure = expectFailure "stale filter post-install recheck" staleInstallResult
                Vitest.expect(staleInstallFailure.Category).toEqual (DependencyMissing)
                Vitest.expect(staleInstallFailure.Code).toBe ("lfs_configuration_unhealthy")

            }
        )

        Vitest.test (
            "Git dependency cancellation is preserved",
            fun () -> promise {
                let cancellation = OperationCancellation.Source()
                cancellation.Cancel()

                let canceledContext =
                    OperationContext.create "canceled-dependencies" cancellation.Cancellation ignore

                let canceledFactory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none
                let! canceledCheckResult = Async.StartAsPromise(canceledFactory.CheckDependencies canceledContext)
                let canceledCheckFailure = expectFailure "canceled dependency check" canceledCheckResult
                Vitest.expect(canceledCheckFailure.Category).toEqual (Canceled)
                Vitest.expect(canceledCheckFailure.Code).toBe ("operation_canceled")

                let! canceledInstallResult =
                    Async.StartAsPromise(canceledFactory.InstallDependency "git-lfs-configuration" canceledContext)

                let canceledInstallFailure = expectFailure "canceled dependency installation" canceledInstallResult
                Vitest.expect(canceledInstallFailure.Category).toEqual (Canceled)
                Vitest.expect(canceledInstallFailure.Code).toBe ("operation_canceled")
            }
        )
)
