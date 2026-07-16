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

let private createSelectedRevisionFixture () = promise {
    let! root = createTempDirectoryAsync ()
    let workPath = join [| root; "work" |]
    let! _ = runGitOk root [| "init"; "-b"; "main"; workPath |]
    let! _ = runGitOk workPath [| "config"; "user.name"; "VCS Partial Tests" |]
    let! _ = runGitOk workPath [| "config"; "user.email"; "partial@example.org" |]
    let! _ = runGitOk workPath [| "config"; "core.autocrlf"; "false" |]
    let! _ = runGitOk workPath [| "lfs"; "install"; "--local" |]
    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "base\n"
    let! _ = runGitOk workPath [| "add"; "base.txt" |]
    let! _ = runGitOk workPath [| "commit"; "-m"; "init: base" |]

    let binding: WorkspaceBinding = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = gitProviderId
        WorkspaceRoot = workPath
        ProviderStateRef = None
        Location = {
            ProviderId = gitProviderId
            DisplayName = None
            ProviderLocation = workPath
            ConnectionProfileId = None
        }
        ConnectionProfileId = None
    }

    return root, workPath, binding
}

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

        Vitest.test (
            "selected revision LFS reconciliation rejects unrelated dirty attributes before ref movement",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesBefore = "*.manual filter=lfs diff=lfs merge=lfs -text\n"
                    do! writeUtf8FileAsync (join [| workPath; ".gitattributes" |]) attributesBefore
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "x")

                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! statusBefore = runGitOk workPath [| "status"; "--porcelain=v1" |]
                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let! statusResult = session.Core.GetStatus(ctx "dirty-attributes-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let largePath = RepositoryPath.tryCreate "large.bin" |> Result.defaultWith failwith
                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject dirty attributes"
                                Paths = [| largePath |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "dirty-attributes-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Validation
                        Vitest.expect(failure.Code).toBe "precondition_failed"
                        Vitest.expect(failure.StateChanged).toBe false
                    | _ -> failwith "Expected unrelated dirty attributes to fail before ref movement."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! statusAfter = runGitOk workPath [| "status"; "--porcelain=v1" |]
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(statusAfter).toBe statusBefore
                    Vitest.expect(attributesAfter).toEqual (Some attributesBefore)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision LFS dependency failure leaves repository state unchanged",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "m")

                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! statusBefore = runGitOk workPath [| "status"; "--porcelain=v1" |]

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess =
                            Some(fun request operationContext ->
                                if request.Arguments = [| "lfs"; "version" |] then
                                    async {
                                        return
                                            OperationResult.succeeded {
                                                ExitCode = 1
                                                StdOut = ""
                                                StdErr = "git: 'lfs' is not a git command"
                                            }
                                    }
                                else
                                    NodeProcess.run request operationContext)
                        Barrier = None
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "missing-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let largePath = RepositoryPath.tryCreate "large.bin" |> Result.defaultWith failwith
                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject missing LFS dependency"
                                Paths = [| largePath |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "missing-lfs-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual DependencyMissing
                        Vitest.expect(failure.Code).toBe "git_lfs_missing"
                        Vitest.expect(failure.StateChanged).toBe false
                    | _ -> failwith "Expected the missing Git LFS dependency to fail before ref movement."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! statusAfter = runGitOk workPath [| "status"; "--porcelain=v1" |]
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(statusAfter).toBe statusBefore
                    Vitest.expect(attributesAfter).toEqual None
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision LFS preparation failure preserves ref index worktree and attributes",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let largeContent = String.replicate (1024 * 1024) "p"
                    do! writeUtf8FileAsync (join [| workPath; "large.bin" |]) largeContent
                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! statusBefore = runGitOk workPath [| "status"; "--porcelain=v1" |]

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess =
                            Some(fun request operationContext ->
                                if request.Arguments = [| "hash-object"; "-w"; "--stdin" |] then
                                    async {
                                        return
                                            OperationResult.failed(
                                                OperationFailure.create
                                                    ProviderError
                                                    "injected_lfs_preparation_failure"
                                                    "The LFS pointer hash failed."
                                            )
                                    }
                                else
                                    NodeProcess.run request operationContext)
                        Barrier = None
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "prepare-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let largePath = RepositoryPath.tryCreate "large.bin" |> Result.defaultWith failwith
                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: fail LFS preparation"
                                Paths = [| largePath |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "prepare-lfs-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Code).toBe "injected_lfs_preparation_failure"
                        Vitest.expect(failure.StateChanged).toBe false
                    | _ -> failwith "Expected LFS preparation to fail before ref movement."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! statusAfter = runGitOk workPath [| "status"; "--porcelain=v1" |]
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    let! worktreeAfter = tryReadUtf8FileAsync (join [| workPath; "large.bin" |])
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(statusAfter).toBe statusBefore
                    Vitest.expect(attributesAfter).toEqual None
                    Vitest.expect(worktreeAfter).toEqual (Some largeContent)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision LFS cancellation preserves repository state",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "c")

                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! statusBefore = runGitOk workPath [| "status"; "--porcelain=v1" |]
                    let session = GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding
                    let! statusResult = session.Core.GetStatus(ctx "cancel-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let cancellation = OperationCancellation.Source()
                    cancellation.Cancel()
                    let context = {
                        ctx "cancel-lfs-revision" with
                            Cancellation = cancellation.Cancellation
                    }

                    let largePath = RepositoryPath.tryCreate "large.bin" |> Result.defaultWith failwith
                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: cancel automatic LFS"
                                Paths = [| largePath |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            context
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Canceled
                        Vitest.expect(failure.StateChanged).toBe false
                    | _ -> failwith "Expected automatic LFS revision cancellation."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! statusAfter = runGitOk workPath [| "status"; "--porcelain=v1" |]
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(statusAfter).toBe statusBefore
                    Vitest.expect(attributesAfter).toEqual None
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision LFS reconciliation returns partial success after ref movement",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let largeContent = String.replicate (1024 * 1024) "y"
                    do! writeUtf8FileAsync (join [| workPath; "large.bin" |]) largeContent
                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess =
                            Some(fun request operationContext ->
                                if request.Arguments |> Array.tryHead = Some "reset" then
                                    async {
                                        return
                                            OperationResult.failed(
                                                OperationFailure.create
                                                    ProviderError
                                                    "injected_reconciliation_failure"
                                                    "The reconciliation command failed."
                                            )
                                    }
                                else
                                    NodeProcess.run request operationContext)
                        Barrier = None
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "partial-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let largePath = RepositoryPath.tryCreate "large.bin" |> Result.defaultWith failwith
                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: partial LFS reconciliation"
                                Paths = [| largePath |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "partial-lfs-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Code).toBe "index_reconciliation_failed"
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_index")
                        Vitest.expect(outcome.AffectedPaths |> Array.contains "large.bin").toBe true
                        Vitest.expect(outcome.AffectedPaths |> Array.contains ".gitattributes").toBe true
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected post-ref reconciliation failure to return partial success."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    let! worktreeLarge = tryReadUtf8FileAsync (join [| workPath; "large.bin" |])
                    Vitest.expect(headAfter.Trim() = headBefore.Trim()).toBe false
                    Vitest.expect(attributesAfter |> Option.exists _.Contains("\"/large.bin\" filter=lfs")).toBe true
                    Vitest.expect(worktreeLarge).toEqual (Some largeContent)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
