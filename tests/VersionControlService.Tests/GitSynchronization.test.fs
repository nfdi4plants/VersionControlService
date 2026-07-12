module VersionControlService.Tests.GitSynchronizationTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Tests.NodePath
open Vitest

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module NodeProcess = VersionControlService.Runtime.Node.Process

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-git-sync-" |]
    fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>

let private removeDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private writeUtf8FileAsync (path: string) (content: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?writeFile (path, content, "utf8") |> unbox<JS.Promise<obj>>
    return ()
}

let private tryReadUtf8FileAsync (path: string) : JS.Promise<string option> = promise {
    try
        let! content = fsPromisesDynamic?readFile (path, "utf8") |> unbox<JS.Promise<string>>
        return Some content
    with _ ->
        return None
}

let private runGitIn (cwd: string) (arguments: string[]) : JS.Promise<string> = promise {
    let request = {
        NodeProcess.ProcessRequest.create "git" arguments with
            WorkingDirectory = Some cwd
    }

    let! result = Async.StartAsPromise(NodeProcess.run request (OperationContext.detached "git-sync-fixture"))

    match result with
    | Succeeded outcome when outcome.Value.ExitCode = 0 -> return outcome.Value.StdOut
    | Succeeded outcome ->
        let command = String.concat " " arguments
        return failwith $"fixture git {command} failed: {outcome.Value.StdErr}"
    | PartiallySucceeded _
    | Failed _ -> return failwith "fixture git invocation failed"
}

let private gitProviderId =
    match ProviderId.tryCreate "git" with
    | Ok providerId -> providerId
    | Error message -> failwith message

let private mkPath (value: string) =
    match RepositoryPath.tryCreate value with
    | Ok path -> path
    | Error message -> failwith message

let private ctx (name: string) = OperationContext.detached name

let private expectValue (operationName: string) (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure) ->
        failwith $"{operationName} unexpectedly returned partial success ({failure.Code})."
    | Failed failure -> failwith $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

/// Isolated workspace with a local bare origin: (workspaceRoot, barePath, session).
let private createSyncFixture (hooks: GitWorkspaceSession.GitSessionHooks) = promise {
    let! root = createTempDirectoryAsync ()
    let barePath = join [| root; "origin.git" |]
    let workPath = join [| root; "work" |]

    let! _ = runGitIn root [| "init"; "--bare"; "-b"; "main"; barePath |]
    let! _ = runGitIn root [| "init"; "-b"; "main"; workPath |]
    let! _ = runGitIn workPath [| "config"; "user.name"; "VCS Sync Tests" |]
    let! _ = runGitIn workPath [| "config"; "user.email"; "sync@example.org" |]
    let! _ = runGitIn workPath [| "config"; "core.autocrlf"; "false" |]
    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "base content\n"
    let! _ = runGitIn workPath [| "add"; "-A" |]
    let! _ = runGitIn workPath [| "commit"; "-m"; "init: base" |]
    let! _ = runGitIn workPath [| "remote"; "add"; "origin"; barePath |]
    let! _ = runGitIn workPath [| "push"; "-u"; "origin"; "main" |]

    let binding: WorkspaceBinding = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = gitProviderId
        WorkspaceRoot = workPath
        ProviderStateRef = None
        Location = {
            ProviderId = gitProviderId
            DisplayName = None
            ProviderLocation = barePath
            ConnectionProfileId = None
        }
        ConnectionProfileId = None
    }

    let session = GitWorkspaceSession.createSession hooks binding
    return root, workPath, barePath, session
}

/// Commits target-side mutations through a scratch clone, like another client.
let private advanceTarget (root: string) (barePath: string) (mutations: (string * string) list) = promise {
    let clonePath = join [| root; $"advance-{DateTime.Now.Ticks}" |]

    let! _ =
        runGitIn root [| "clone"; "-c"; "core.autocrlf=false"; barePath; clonePath |]

    let! _ = runGitIn clonePath [| "config"; "user.name"; "External Client" |]
    let! _ = runGitIn clonePath [| "config"; "user.email"; "external@example.org" |]

    for path, content in mutations do
        do! writeUtf8FileAsync (join [| clonePath; path |]) content

    let! _ = runGitIn clonePath [| "add"; "-A" |]
    let! _ = runGitIn clonePath [| "commit"; "-m"; "external: advance" |]
    let! _ = runGitIn clonePath [| "push"; "origin"; "HEAD" |]
    return ()
}

let private syncService (session: WorkspaceSession) =
    match session.Synchronization with
    | Some service -> service
    | None -> failwith "Expected the Git synchronization service."

/// A process runner that fakes `git --version` and delegates everything else.
let private versionFakingRunner (fakeVersion: string) : GitWorkspaceSession.GitSessionHooks = {
    RunProcess =
        Some(fun request processContext ->
            async {
                if request.Arguments.Length = 1 && request.Arguments[0] = "--version" then
                    return
                        OperationResult.succeeded {
                            NodeProcess.ExitCode = 0
                            StdOut = $"git version {fakeVersion}\n"
                            StdErr = ""
                        }
                else
                    return! NodeProcess.run request processContext
            })
    Barrier = None
}

Vitest.describe (
    "GitWorkspaceSession v2 synchronization",
    fun () ->
        Vitest.test (
            "v2 preview includes dirty workspace and requires Git 2.38",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session =
                    createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    // Preview accounts for dirty workspace changes, not only
                    // merge-tree between commits.
                    do!
                        advanceTarget root barePath [
                            "base.txt", "target base change\n"
                            "remote-new.txt", "target new file\n"
                        ]

                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "local dirty change\n"

                    let! previewResult = Async.StartAsPromise((syncService session).PreviewUpdate(ctx "sync-preview"))
                    let preview = expectValue "preview update" previewResult

                    let changed =
                        preview.ChangedPaths |> Array.map RepositoryPath.value |> Array.sort

                    Vitest.expect(changed).toEqual ([| "base.txt"; "remote-new.txt" |])

                    let overlapping = preview.OverlappingPaths |> Array.map RepositoryPath.value

                    Vitest.expect(overlapping).toEqual ([| "base.txt" |])
                    Vitest.expect(preview.HasDataLossRisk).toBe (true)
                    Vitest.expect(preview.WouldCreateConflictSession).toBe (true)

                    // The dirty file itself is untouched by the preview.
                    let! localContent = tryReadUtf8FileAsync (join [| workPath; "base.txt" |])
                    Vitest.expect(localContent).toEqual (Some "local dirty change\n")

                    // Dependency diagnostics reject Git 2.37: pull preview uses
                    // `merge-tree --write-tree`, documented from Git 2.38.
                    let oldFactory = GitWorkspaceSession.createFactory (versionFakingRunner "2.37.2")
                    let! oldResult = Async.StartAsPromise(oldFactory.CheckDependencies(ctx "deps-237"))
                    let oldDependencies = expectValue "dependencies for 2.37" oldResult
                    let oldGit = oldDependencies |> Array.find (fun entry -> entry.Component = "git")

                    Vitest.expect(oldGit.Installed).toBe (true)
                    Vitest.expect(oldGit.Compatible).toBe (false)
                    Vitest.expect(oldGit.Remediation.IsSome).toBe (true)

                    let newFactory = GitWorkspaceSession.createFactory (versionFakingRunner "2.38.0")
                    let! newResult = Async.StartAsPromise(newFactory.CheckDependencies(ctx "deps-238"))
                    let newDependencies = expectValue "dependencies for 2.38" newResult
                    let newGit = newDependencies |> Array.find (fun entry -> entry.Component = "git")

                    Vitest.expect(newGit.Compatible).toBe (true)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
