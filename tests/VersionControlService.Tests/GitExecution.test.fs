module VersionControlService.Tests.GitExecutionTests

open Fable.Core
open VersionControlService.Abstractions
open Vitest

module GitAuthAdapter = VersionControlService.Git.GitAuthAdapter
module GitCommandResolver = VersionControlService.Git.GitCommandResolver
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module GitExecution = VersionControlService.Git.GitExecution
module GitInternals = VersionControlService.Git.GitInternals
module GitLfsAdapter = VersionControlService.Git.GitLfsAdapter
module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module NodeProcess = VersionControlService.Runtime.Node.Process
module TextDiffSupervisor = VersionControlService.Git.TextDiff.TextDiffSupervisor

[<Import("mkdtempSync", "node:fs")>]
let private mkdtempSync (prefix: string) : string = jsNative

[<Import("chmodSync", "node:fs")>]
let private chmodSync (path: string) (mode: int) : unit = jsNative

[<Import("symlinkSync", "node:fs")>]
let private symlinkSync (target: string) (path: string) : unit = jsNative

[<Import("tmpdir", "node:os")>]
let private systemTempDirectory () : string = jsNative

[<Emit("process.platform")>]
let private processPlatform () : string = jsNative

[<Emit("process.execPath")>]
let private nodeExecutable () : string = jsNative

[<Import("delimiter", "node:path")>]
let private pathDelimiter: string = jsNative

[<Emit("Object.assign({}, process.env)")>]
let private processEnvironment () : obj = jsNative

[<Emit("$0.PATH || $0.Path || ''")>]
let private environmentPath (_environment: obj) : string = jsNative

[<Emit("process.env.PATH || process.env.Path || ''")>]
let private currentProcessPath () : string = jsNative

let private firstPathEntry (path: string) = path.Split(pathDelimiter.[0]).[0]

let private emptyOutput: NodeProcess.ProcessOutput = {
    ExitCode = 0
    StdOut = ""
    StdErr = ""
}

/// A folder whose name has a space, with a runnable `git` stand-in inside. macOS and Linux probe
/// `git lfs --version` with the selected executable, and the stand-in answers that probe.
let private createGitInstallation (root: string) (folderName: string) : string =
    let folder = NodePath.join [| root; folderName |]
    NodeFileSystem.mkdirSync folder (NodeFileSystem.MkdirOptions(recursive = true))
    let executable = NodePath.join [| folder; "git" |]
    NodeFileSystem.writeFileSync executable "#!/bin/sh\nexit 0\n" NodeFileSystem.TextEncoding.Utf8
    chmodSync executable 493
    executable

let private removeTree (path: string) =
    NodeFileSystem.rmAsync
        path
        (NodeFileSystem.RmOptions(recursive = true, force = true, maxRetries = 4, retryDelay = 40))

/// Creates `fileName` with the given mode in the folder `folderName` below `root`.
let private createFile (root: string) (folderName: string) (fileName: string) (mode: int) : string =
    let folder = NodePath.join [| root; folderName |]
    NodeFileSystem.mkdirSync folder (NodeFileSystem.MkdirOptions(recursive = true))
    let file = NodePath.join [| folder; fileName |]
    NodeFileSystem.writeFileSync file "#!/bin/sh\nexit 0\n" NodeFileSystem.TextEncoding.Utf8
    chmodSync file mode
    file

let private withTempRoot (action: string -> JS.Promise<unit>) : JS.Promise<unit> = promise {
    let root =
        mkdtempSync (NodePath.join [| systemTempDirectory (); "vcs-git-lookup-" |])

    try
        do! action root
        do! removeTree root
    with error ->
        do! removeTree root
        return raise error
}

/// Answers the queries of a healthy install (git and LFS versions, the filter setting) and records every request.
let private healthyHooks (requests: ResizeArray<NodeProcess.ProcessRequest>) : GitWorkspaceSession.GitSessionHooks = {
    RunBytesProcess = None
    RunProcess =
        Some(fun request _ -> async {
            requests.Add request

            let output =
                match request.Arguments with
                | [| "--version" |] -> {
                    emptyOutput with
                        StdOut = "git version 2.45.1\n"
                  }
                | [| "lfs"; "version" |] -> {
                    emptyOutput with
                        StdOut = "git-lfs/3.7.0 (GitHub; linux amd64; go 1.24.4)\n"
                  }
                | _ -> {
                    emptyOutput with
                        StdOut = "git-lfs filter-process\n"
                  }

            return OperationResult.succeeded output
        })
    Barrier = None
}

/// Answers the queries of a git and a Git LFS that run, but whose filter setting is missing from every configuration.
let private unconfiguredLfsHooks: GitWorkspaceSession.GitSessionHooks = {
    RunBytesProcess = None
    RunProcess =
        Some(fun request _ -> async {
            let output =
                match request.Arguments with
                | [| "--version" |] -> {
                    emptyOutput with
                        StdOut = "git version 2.45.1\n"
                  }
                | [| "lfs"; "version" |] -> {
                    emptyOutput with
                        StdOut = "git-lfs/3.7.0 (GitHub; linux amd64; go 1.24.4)\n"
                  }
                | _ -> { emptyOutput with ExitCode = 1 }

            return OperationResult.succeeded output
        })
    Barrier = None
}

/// Answers every request with `output`, as a file that is not git would.
let private notGitHooks (output: NodeProcess.ProcessOutput) : GitWorkspaceSession.GitSessionHooks = {
    RunBytesProcess = None
    RunProcess = Some(fun _ _ -> async { return OperationResult.succeeded output })
    Barrier = None
}

let private checkDependencies (hooks: GitWorkspaceSession.GitSessionHooks) : JS.Promise<DependencyStatus[]> = promise {
    let factory = GitWorkspaceSession.createFactory hooks

    let! result =
        factory.CheckDependencies(OperationContext.detached "git-executable-probe")
        |> Async.StartAsPromise

    match result with
    | Succeeded outcome -> return outcome.Value
    | PartiallySucceeded(_, failure)
    | Failed failure -> return failwith $"The dependency check failed ({failure.Code}): {failure.Message}"
}

/// Runs `action` with the paths of two installations. The provider is restored afterwards.
let private withGitInstallations (action: string -> string -> JS.Promise<unit>) : JS.Promise<unit> = promise {
    let root =
        mkdtempSync (NodePath.join [| systemTempDirectory (); "vcs-git-execution-" |])

    let first = createGitInstallation root "first git tools"
    let second = createGitInstallation root "second git tools"

    try
        try
            do! action first second
        finally
            GitExecution.resetGitExecutableProvider ()

        do! removeTree root
    with error ->
        do! removeTree root
        return raise error
}

/// Runs `action` with node selected as the git executable, so a start of "git" runs node and the git
/// arguments are node arguments. The provider is restored afterwards.
let private withNodeAsGit (action: string -> JS.Promise<unit>) : JS.Promise<unit> = promise {
    let node = nodeExecutable ()
    GitExecution.setGitExecutableProvider (fun () -> Some node)

    try
        do! action node
    finally
        GitExecution.resetGitExecutableProvider ()
}

Vitest.describe (
    "Git executable selection",
    fun () ->
        Vitest.test (
            "uses git from PATH without a provider",
            fun () ->
                GitExecution.resetGitExecutableProvider ()

                let expectedPath =
                    environmentPath (GitCommandResolver.ensureGitToolPath "git" (processEnvironment ()))

                Vitest.expect(GitExecution.gitExecutable ()).toBe "git"
                Vitest.expect(environmentPath (GitExecution.resolvedEnvironment ())).toBe expectedPath

                try
                    GitExecution.setGitExecutableProvider (fun () -> Some "  ")
                    Vitest.expect(GitExecution.gitExecutable ()).toBe "git"
                    GitExecution.setGitExecutableProvider (fun () -> None)
                    Vitest.expect(GitExecution.gitExecutable ()).toBe "git"
                finally
                    GitExecution.resetGitExecutableProvider ()
        )

        Vitest.test (
            "a provider path with a space is the executable and leads the child PATH",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    let processPathBefore = currentProcessPath ()
                    GitExecution.setGitExecutableProvider (fun () -> Some first)

                    Vitest.expect(GitExecution.gitExecutable ()).toBe first

                    let childPath = environmentPath (GitExecution.resolvedEnvironment ())
                    Vitest.expect(firstPathEntry childPath).toBe (NodePath.dirname first)
                    Vitest.expect(currentProcessPath ()).toBe processPathBefore
                })
        )

        Vitest.test (
            "a changed selection applies to the next call without installing the provider again",
            fun () ->
                withGitInstallations (fun first second -> promise {
                    let selected = ref first
                    GitExecution.setGitExecutableProvider (fun () -> Some selected.Value)

                    Vitest.expect(GitExecution.gitExecutable ()).toBe first

                    Vitest
                        .expect(firstPathEntry (environmentPath (GitExecution.resolvedEnvironment ())))
                        .toBe (NodePath.dirname first)

                    selected.Value <- second

                    Vitest.expect(GitExecution.gitExecutable ()).toBe second

                    Vitest
                        .expect(firstPathEntry (environmentPath (GitExecution.resolvedEnvironment ())))
                        .toBe (NodePath.dirname second)
                })
        )

        Vitest.test (
            "resetGitExecutableProvider restores automatic discovery",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    let expectedPath =
                        environmentPath (GitCommandResolver.ensureGitToolPath "git" (processEnvironment ()))

                    GitExecution.setGitExecutableProvider (fun () -> Some first)
                    Vitest.expect(GitExecution.gitExecutable ()).toBe first

                    GitExecution.resetGitExecutableProvider ()

                    Vitest.expect(GitExecution.gitExecutable ()).toBe "git"
                    Vitest.expect(environmentPath (GitExecution.resolvedEnvironment ())).toBe expectedPath
                })
        )

        Vitest.test (
            "simple-git accepts the selected path with a space",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    GitExecution.setGitExecutableProvider (fun () -> Some first)

                    let invocation = GitExecution.resolveInvocation ()

                    let git =
                        GitInternals.createOptions
                            invocation
                            (NodePath.dirname first)
                            GitInternals.standardTimeout
                            None
                        |> GitInternals.createGit invocation

                    Vitest.expect(isNull (box git)).toBe false
                })
        )

        Vitest.test (
            "session requests carry the selected executable and its PATH",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    let requests = ResizeArray<NodeProcess.ProcessRequest>()

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess =
                            Some(fun request _ -> async {
                                requests.Add request
                                return OperationResult.succeeded emptyOutput
                            })
                        Barrier = None
                    }

                    let factory = GitWorkspaceSession.createFactory hooks

                    let checkDependencies () =
                        factory.CheckDependencies(OperationContext.detached "git-executable-session")
                        |> Async.StartAsPromise

                    GitExecution.setGitExecutableProvider (fun () -> Some first)
                    let! _ = checkDependencies ()

                    Vitest.expect(requests.Count).toBeGreaterThan 0

                    for request in requests do
                        let pathOverride =
                            request.Environment
                            |> Array.tryPick (fun (name, value) -> if name = "PATH" then Some value else None)

                        Vitest.expect(request.Command).toBe first
                        Vitest.expect(pathOverride |> Option.map firstPathEntry).toEqual (Some(NodePath.dirname first))

                    GitExecution.resetGitExecutableProvider ()
                    requests.Clear()
                    let! _ = checkDependencies ()

                    Vitest.expect(requests.Count).toBeGreaterThan 0

                    for request in requests do
                        Vitest.expect(request.Command).toBe "git"
                })
        )
)

Vitest.describe (
    "Git executable lookup on PATH",
    fun () ->
        Vitest.test (
            "finds git in the first PATH entry that holds it",
            fun () ->
                withTempRoot (fun root -> promise {
                    let gitFileName = if processPlatform () = "win32" then "git.exe" else "git"
                    let emptyFolder = NodePath.join [| root; "empty" |]
                    NodeFileSystem.mkdirSync emptyFolder (NodeFileSystem.MkdirOptions(recursive = true))

                    let first = createFile root "first" gitFileName 493
                    let second = createFile root "second" gitFileName 493

                    let pathValue =
                        String.concat pathDelimiter [
                            emptyFolder
                            NodePath.dirname first
                            NodePath.dirname second
                        ]

                    Vitest.expect(GitCommandResolver.findGitOnPath (processPlatform ()) pathValue).toEqual (Some first)
                })
        )

        Vitest.test (
            "tries .com before .exe in each Windows PATH entry",
            fun () ->
                withTempRoot (fun root -> promise {
                    let exeOnly = createFile root "exe only" "git.exe" 493
                    let comAndExe = createFile root "com and exe" "git.com" 493
                    createFile root "com and exe" "git.exe" 493 |> ignore

                    let pathValue entries = String.concat ";" entries

                    Vitest
                        .expect(
                            GitCommandResolver.findGitOnPath
                                "win32"
                                (pathValue [ NodePath.dirname exeOnly; NodePath.dirname comAndExe ])
                        )
                        .toEqual (Some exeOnly)

                    Vitest
                        .expect(
                            GitCommandResolver.findGitOnPath
                                "win32"
                                (pathValue [ NodePath.dirname comAndExe; NodePath.dirname exeOnly ])
                        )
                        .toEqual (Some comAndExe)
                })
        )

        Vitest.test (
            "gives None when no PATH entry holds a runnable git",
            fun () ->
                withTempRoot (fun root -> promise {
                    let emptyFolder = NodePath.join [| root; "empty" |]
                    NodeFileSystem.mkdirSync emptyFolder (NodeFileSystem.MkdirOptions(recursive = true))

                    // A file named git without the executable bit on macOS and Linux, and without
                    // an extension on Windows, is not something spawn would start.
                    let plain = createFile root "plain" "git" 420
                    let missing = NodePath.join [| root; "missing" |]

                    let pathValue =
                        String.concat pathDelimiter [ emptyFolder; NodePath.dirname plain; missing ]

                    Vitest.expect(GitCommandResolver.findGitOnPath (processPlatform ()) pathValue).toEqual None
                })
        )
)

Vitest.describe (
    "Git dependency check executable",
    fun () ->
        Vitest.test (
            "an explicit selection is the path every component reports",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    let requests = ResizeArray<NodeProcess.ProcessRequest>()
                    GitExecution.setGitExecutableProvider (fun () -> Some first)

                    let! statuses = checkDependencies (healthyHooks requests)

                    Vitest.expect(statuses.Length).toBe 3

                    for status in statuses do
                        Vitest.expect(status.Installed).toBe true
                        Vitest.expect(status.ProbeExecutablePath).toEqual (Some first)
                })
        )

        Vitest.test (
            "automatic discovery reports the git found on the PATH of the child environment",
            fun () -> promise {
                GitExecution.resetGitExecutableProvider ()

                let expected =
                    GitCommandResolver.findGitInEnvironment (GitExecution.resolvedEnvironment ())

                let! statuses = checkDependencies (healthyHooks (ResizeArray()))

                Vitest.expect(statuses.Length).toBe 3

                for status in statuses do
                    Vitest.expect(status.ProbeExecutablePath).toEqual expected

                match expected with
                | Some path -> Vitest.expect(NodePath.isAbsolute path).toBe true
                | None -> ()
            }
        )

        for (reason, selectionFor) in
            [
                "a path that does not exist", (fun first -> NodePath.join [| NodePath.dirname first; "missing git" |])
                "a relative path", (fun _ -> "relative-tools/git")
                "a folder", NodePath.dirname
                "a path with a NUL character", (fun first -> first + "\u0000")
            ] do
            Vitest.test (
                $"an explicit selection with {reason} makes every component not installed",
                fun () ->
                    withGitInstallations (fun first _ -> promise {
                        let requests = ResizeArray<NodeProcess.ProcessRequest>()
                        let selection = selectionFor first
                        GitExecution.setGitExecutableProvider (fun () -> Some selection)

                        let! statuses = checkDependencies (healthyHooks requests)

                        Vitest.expect(statuses.Length).toBe 3

                        for status in statuses do
                            Vitest.expect(status.Installed).toBe false
                            Vitest.expect(status.Compatible).toBe false
                            Vitest.expect(status.Remediation.IsSome).toBe true
                            Vitest.expect(status.Remediation.Value.Contains selection).toBe true
                            Vitest.expect(status.ProbeExecutablePath).toEqual (Some selection)

                        // No fallback to the git on PATH after an explicit selection fails.
                        Vitest.expect(requests.Count).toBe 0
                    })
            )

        Vitest.test (
            "a selected file that fails to run as git gets a remediation naming the path",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    GitExecution.setGitExecutableProvider (fun () -> Some first)

                    let! statuses =
                        checkDependencies (
                            notGitHooks {
                                emptyOutput with
                                    ExitCode = 1
                                    StdErr = "not a git"
                            }
                        )

                    let git = statuses |> Array.find (fun status -> status.Component = "git")

                    Vitest.expect(git.Installed).toBe false
                    Vitest.expect(git.Compatible).toBe false
                    Vitest.expect(git.Remediation.IsSome).toBe true
                    Vitest.expect(git.Remediation.Value.Contains first).toBe true
                })
        )

        Vitest.test (
            "a selected file whose output holds no git version is not installed and gets a remediation naming the path",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    GitExecution.setGitExecutableProvider (fun () -> Some first)

                    let! statuses = checkDependencies (notGitHooks { emptyOutput with StdOut = "hello\n" })
                    let git = statuses |> Array.find (fun status -> status.Component = "git")

                    Vitest.expect(git.Installed).toBe false
                    Vitest.expect(git.Compatible).toBe false
                    Vitest.expect(git.Remediation.IsSome).toBe true
                    Vitest.expect(git.Remediation.Value.Contains first).toBe true
                })
        )

        Vitest.test (
            "the remediation of a missing Git LFS configuration quotes the selected path in its command",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    GitExecution.setGitExecutableProvider (fun () -> Some first)

                    let! statuses = checkDependencies unconfiguredLfsHooks

                    let configuration =
                        statuses
                        |> Array.find (fun status -> status.Component = "git-lfs-configuration")

                    Vitest.expect(configuration.Installed).toBe false
                    Vitest.expect(configuration.Remediation.IsSome).toBe true
                    Vitest.expect(configuration.Remediation.Value.Contains("\"" + first + "\" lfs install")).toBe true
                })
        )

        Vitest.test (
            "the remediation of a missing Git LFS configuration runs git from PATH with automatic discovery",
            fun () -> promise {
                GitExecution.resetGitExecutableProvider ()

                let! statuses = checkDependencies unconfiguredLfsHooks

                let configuration =
                    statuses
                    |> Array.find (fun status -> status.Component = "git-lfs-configuration")

                Vitest.expect(configuration.Installed).toBe false
                Vitest.expect(configuration.Remediation.IsSome).toBe true
                Vitest.expect(configuration.Remediation.Value.Contains "`git lfs install --skip-repo`").toBe true
            }
        )

        // Node prints "v22.19.0" for --version and git-lfs sits beside git in a Git installation, so
        // both are easy to pick by mistake. Their numbers must not pass for a Git version.
        for (program, versionOutput) in
            [
                "Node.js", "v22.19.0\n"
                "Git LFS", "git-lfs/3.7.0 (GitHub; windows amd64; go 1.24.4)\n"
            ] do
            Vitest.test (
                $"a selected {program} version output is not Git",
                fun () ->
                    withGitInstallations (fun first _ -> promise {
                        GitExecution.setGitExecutableProvider (fun () -> Some first)

                        let! statuses =
                            checkDependencies (
                                notGitHooks {
                                    emptyOutput with
                                        StdOut = versionOutput
                                }
                            )

                        let git = statuses |> Array.find (fun status -> status.Component = "git")

                        Vitest.expect(git.Installed).toBe false
                        Vitest.expect(git.Compatible).toBe false
                        Vitest.expect(git.Version).toEqual None
                        Vitest.expect(git.Remediation.IsSome).toBe true
                        Vitest.expect(git.Remediation.Value.Contains first).toBe true
                    })
            )

            Vitest.test (
                $"automatic discovery treats a {program} version output as not Git",
                fun () -> promise {
                    GitExecution.resetGitExecutableProvider ()

                    let! statuses =
                        checkDependencies (
                            notGitHooks {
                                emptyOutput with
                                    StdOut = versionOutput
                            }
                        )

                    let git = statuses |> Array.find (fun status -> status.Component = "git")

                    Vitest.expect(git.Installed).toBe false
                    Vitest.expect(git.Compatible).toBe false
                    Vitest.expect(git.Version).toEqual None
                    Vitest.expect(git.Remediation.IsSome).toBe true
                }
            )

        Vitest.test (
            "a Windows git version line is Git and compatible",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    GitExecution.setGitExecutableProvider (fun () -> Some first)

                    let! statuses =
                        checkDependencies (
                            notGitHooks {
                                emptyOutput with
                                    StdOut = "git version 2.51.0.windows.1\n"
                            }
                        )

                    let git = statuses |> Array.find (fun status -> status.Component = "git")

                    Vitest.expect(git.Installed).toBe true
                    Vitest.expect(git.Compatible).toBe true
                    Vitest.expect(git.Version).toEqual (Some "git version 2.51.0.windows.1")
                    Vitest.expect(git.Remediation).toEqual None
                })
        )

        Vitest.test (
            "a symbolic link to a file is a usable selection",
            fun () ->
                withGitInstallations (fun first _ -> promise {
                    let link =
                        NodePath.join [|
                            NodePath.dirname (NodePath.dirname first)
                            "linked git"
                        |]

                    // Windows creates links only for privileged users, and then there is nothing to check.
                    let linked =
                        try
                            symlinkSync first link
                            true
                        with _ ->
                            false

                    if linked then
                        GitExecution.setGitExecutableProvider (fun () -> Some link)

                        let! statuses = checkDependencies (healthyHooks (ResizeArray()))

                        for status in statuses do
                            Vitest.expect(status.Installed).toBe true
                            Vitest.expect(status.ProbeExecutablePath).toEqual (Some link)
                })
        )

        Vitest.test (
            "one resolution serves every command and result of a check",
            fun () ->
                withGitInstallations (fun first second -> promise {
                    let requests = ResizeArray<NodeProcess.ProcessRequest>()
                    let providerCalls = ref 0

                    GitExecution.setGitExecutableProvider (fun () ->
                        providerCalls.Value <- providerCalls.Value + 1
                        Some(if providerCalls.Value % 2 = 1 then first else second)
                    )

                    let! statuses = checkDependencies (healthyHooks requests)

                    Vitest.expect(requests.Count).toBeGreaterThan 1
                    Vitest.expect(providerCalls.Value).toBe 1

                    for request in requests do
                        Vitest.expect(request.Command).toBe first

                    for status in statuses do
                        Vitest.expect(status.ProbeExecutablePath).toEqual (Some first)
                })
        )
)

// Node stands in for git in these tests, so the output of the child process proves which executable ran.
Vitest.describe (
    "Git executable at every start",
    fun () ->
        Vitest.test (
            "the Git LFS adapter spawns the selected executable",
            fun () ->
                withNodeAsGit (fun _ -> promise {
                    let request: GitLfsAdapter.GitSpawnRequest = {
                        WorkingDirectory = None
                        Arguments = [| "-e"; "console.log('adapter-marker')" |]
                        Environment = None
                        StandardInput = None
                        CancelCheck = None
                        TimeoutMs = None
                        Output = None
                    }

                    let! result = GitLfsAdapter.runGitCaptured request

                    Vitest.expect(result.ExitCode).toBe 0
                    Vitest.expect(result.StdoutText.Trim()).toBe "adapter-marker"
                })
        )

        Vitest.test (
            "a simple-git instance spawns the selected executable",
            fun () ->
                withNodeAsGit (fun _ -> promise {
                    let invocation = GitExecution.resolveInvocation ()

                    let git =
                        GitInternals.createOptions
                            invocation
                            (systemTempDirectory ())
                            GitInternals.standardTimeout
                            None
                        |> GitInternals.createGit invocation

                    let! output = git.raw [| "-e"; "console.log('simple-git-marker')" |]

                    Vitest.expect(output.Trim()).toBe "simple-git-marker"
                })
        )

        // The supervisor puts git options such as --literal-pathspecs before every command and accepts
        // only a fixed set of commands. Node rejects that first option and says so in its own words, with
        // its own exit code 9 for an invalid argument. No script can run, so the rejection is the proof.
        Vitest.test (
            "the text diff supervisor spawns the selected executable for a short command",
            fun () ->
                withTempRoot (fun root ->
                    withNodeAsGit (fun _ -> promise {
                        let! supervisor =
                            TextDiffSupervisor.create {
                                TextDiffSupervisor.TextDiffSupervisorOptions.defaults with
                                    TempRoot = root
                            }

                        let owner: TextDiffSupervisor.ChildOwner = {
                            WorkerId = "worker-a"
                            SessionId = "session-a"
                            RequestId = "request-a"
                        }

                        let! result = supervisor.RunShort(owner, root, [| "symbolic-ref"; "-q"; "HEAD" |])
                        do! supervisor.Dispose()

                        Vitest.expect(result.ExitCode).toEqual (Some 9)
                        Vitest.expect(result.Stderr.Contains "bad option: --literal-pathspecs").toBe true
                    })
                )
        )

        Vitest.test (
            "the non-interactive environment of an explicit executable leads with its directory and stays the same when completed again",
            fun () ->
                withNodeAsGit (fun node -> promise {
                    let invocation = GitExecution.resolveInvocation ()
                    let environment = GitAuthAdapter.createNonInteractiveEnv invocation
                    let path = environmentPath environment

                    // The fallback entries go in front of the inherited ones, and the directory goes in front of both.
                    let expectedPath =
                        processEnvironment ()
                        |> GitCommandResolver.ensureGitToolPath node
                        |> GitCommandResolver.prependExecutableDirectory (NodePath.dirname node)
                        |> environmentPath

                    Vitest.expect(firstPathEntry path).toBe (NodePath.dirname node)
                    Vitest.expect(path).toBe expectedPath
                    Vitest.expect(environmentPath (GitExecution.completeEnvironment invocation environment)).toBe path
                })
        )

        // The script prints LC_ALL, the first PATH entry, how often the directory of node appears in PATH
        // and whether the directory of the stand-in does. A start has to get its own directory exactly once.
        for (kind, buildAuthentication) in
            [
                "command",
                (fun () ->
                    GitCredentialStrategy.buildScopedCommandAuthentication "origin" "https://example.org/repo.git" None
                )
                "header",
                (fun () -> GitCredentialStrategy.buildScopedHeaderAuthentication "https://example.org/repo.git" None)
            ] do
            Vitest.test (
                $"the stored {kind} authentication is completed once by the start, for the executable of that start",
                fun () ->
                    withGitInstallations (fun standIn _ -> promise {
                        // Built while another executable is selected, so a completed environment would carry its directory.
                        GitExecution.setGitExecutableProvider (fun () -> Some standIn)
                        let authentication = buildAuthentication ()

                        Vitest.expect(environmentPath authentication.Environment).toBe (currentProcessPath ())

                        let node = nodeExecutable ()
                        GitExecution.setGitExecutableProvider (fun () -> Some node)

                        let script =
                            "const [d,o]=process.argv.slice(1);const p=process.env.PATH.split(require('path').delimiter);console.log([process.env.LC_ALL,p[0],p.filter(e=>e===d).length,p.includes(o)].join('|'))"

                        let request: GitLfsAdapter.GitSpawnRequest = {
                            WorkingDirectory = None
                            Arguments = [|
                                "-e"
                                script
                                NodePath.dirname node
                                NodePath.dirname standIn
                            |]
                            Environment = Some authentication.Environment
                            StandardInput = None
                            CancelCheck = None
                            TimeoutMs = None
                            Output = None
                        }

                        let! result = GitLfsAdapter.runGitCaptured request

                        Vitest.expect(result.ExitCode).toBe 0
                        Vitest.expect(result.StdoutText.Trim()).toBe $"C|{NodePath.dirname node}|1|false"
                    })
            )
)
