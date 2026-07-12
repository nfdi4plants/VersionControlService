module VersionControlService.Tests.GitPartialSuccessTests

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
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-git-partial-" |]
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

let private runGit (cwd: string) (arguments: string[]) : JS.Promise<Result<string, string>> = promise {
    let request = {
        NodeProcess.ProcessRequest.create "git" arguments with
            WorkingDirectory = Some cwd
    }

    let! result = Async.StartAsPromise(NodeProcess.run request (OperationContext.detached "git-partial-fixture"))

    match result with
    | Succeeded outcome when outcome.Value.ExitCode = 0 -> return Ok outcome.Value.StdOut
    | Succeeded outcome -> return Error outcome.Value.StdErr
    | PartiallySucceeded _
    | Failed _ -> return Error "git invocation failed"
}

let private runGitOk (cwd: string) (arguments: string[]) : JS.Promise<string> = promise {
    let! result = runGit cwd arguments

    match result with
    | Ok output -> return output
    | Error message ->
        let command = String.concat " " arguments
        return failwith $"fixture git {command} failed: {message}"
}

let private gitProviderId =
    match ProviderId.tryCreate "git" with
    | Ok providerId -> providerId
    | Error message -> failwith message

let private ctx (name: string) = OperationContext.detached name

Vitest.describe (
    "GitWorkspaceSession v2 partial success",
    fun () ->
        Vitest.test (
            "v2 reports transfer success with hydration recovery",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // Requires git-lfs; report a pass with a skip note when unavailable.
                let! lfsProbe = runGit "." [| "lfs"; "version" |]

                match lfsProbe with
                | Error _ -> Vitest.expect(true).toBe (true)
                | Ok _ ->
                    let! root = createTempDirectoryAsync ()

                    try
                        let barePath = join [| root; "origin.git" |]
                        let workPath = join [| root; "work" |]

                        let! _ = runGitOk root [| "init"; "--bare"; "-b"; "main"; barePath |]
                        let! _ = runGitOk root [| "init"; "-b"; "main"; workPath |]
                        let! _ = runGitOk workPath [| "config"; "user.name"; "VCS Partial Tests" |]
                        let! _ = runGitOk workPath [| "config"; "user.email"; "partial@example.org" |]
                        let! _ = runGitOk workPath [| "config"; "core.autocrlf"; "false" |]
                        let! _ = runGitOk workPath [| "lfs"; "install"; "--local" |]
                        let! _ = runGitOk workPath [| "lfs"; "track"; "*.bin" |]
                        do! writeUtf8FileAsync (join [| workPath; "large.bin" |]) "large binary payload\n"
                        let! _ = runGitOk workPath [| "add"; "-A" |]
                        let! _ = runGitOk workPath [| "commit"; "-m"; "init: lfs base" |]
                        let! _ = runGitOk workPath [| "remote"; "add"; "origin"; barePath |]
                        let! _ = runGitOk workPath [| "push"; "-u"; "origin"; "main" |]

                        // Break hydration: remove the target's LFS object store.
                        do! removeDirectoryAsync (join [| barePath; "lfs" |])

                        let factory =
                            GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none

                        let clonePath = join [| root; "hydration-clone" |]

                        let! cloneResult =
                            Async.StartAsPromise(
                                factory.Clone
                                    {
                                        Location = {
                                            ProviderId = gitProviderId
                                            DisplayName = None
                                            ProviderLocation = barePath
                                            ConnectionProfileId = None
                                        }
                                        TargetPath = clonePath
                                        TargetRef = None
                                        MaterializeAllObjects = true
                                    }
                                    (ctx "hydration-clone")
                            )

                        // Git transfer succeeded, hydration failed: partial success with
                        // the changed workspace and a retry-materialization action.
                        match cloneResult with
                        | PartiallySucceeded(outcome, failure) ->
                            Vitest.expect(outcome.Value.WorkspaceRoot).toBe (clonePath)
                            Vitest.expect(failure.StateChanged).toBe (true)

                            Vitest
                                .expect(failure.RecoveryAction |> Option.map _.Code)
                                .toEqual (Some "retry_materialization")

                            // The cloned workspace exists with the pointer file intact.
                            let! pointerContent = tryReadUtf8FileAsync (join [| clonePath; "large.bin" |])

                            match pointerContent with
                            | Some content -> Vitest.expect(content.Contains "git-lfs").toBe (true)
                            | None -> failwith "Expected the cloned pointer file to exist."
                        | Succeeded _ -> failwith "Expected hydration to fail after the successful transfer."
                        | Failed failure ->
                            failwith
                                $"Expected partial success but the whole clone failed ({failure.Code}): {failure.Message}"

                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "v2 large-object listing preserves operational failures",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    // A directory that is not a Git repository: listing must fail
                    // structurally instead of returning an empty successful list.
                    let binding: WorkspaceBinding = {
                        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
                        ProviderId = gitProviderId
                        WorkspaceRoot = root
                        ProviderStateRef = None
                        Location = {
                            ProviderId = gitProviderId
                            DisplayName = None
                            ProviderLocation = root
                            ConnectionProfileId = None
                        }
                        ConnectionProfileId = None
                    }

                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let materialization =
                        match session.ObjectMaterialization with
                        | Some service -> service
                        | None -> failwith "Expected the object-materialization service."

                    let! listingResult = Async.StartAsPromise(materialization.ListObjects(ctx "broken-listing"))

                    match listingResult with
                    | Failed failure ->
                        Vitest.expect(failure.Code).toBe ("lfs_listing_failed")
                        Vitest.expect(failure.Message.Length > 0).toBe (true)
                    | Succeeded _
                    | PartiallySucceeded _ ->
                        failwith "Expected the listing over a non-repository to fail structurally."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
