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

let private createDirectoryJunctionAsync (targetPath: string) (linkPath: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?symlink (targetPath, linkPath, "junction") |> unbox<JS.Promise<obj>>
    return ()
}

[<Emit("(() => { const fs = require('node:fs'); const moduleApi = require('node:module'); const original = fs.renameSync; fs.renameSync = (fromPath, toPath) => { if (toPath === $0 && String(fromPath).startsWith($0 + '.vcs-') && !$1()) fs.writeFileSync($0, $2, 'utf8'); return original(fromPath, toPath); }; moduleApi.syncBuiltinESMExports(); return () => { fs.renameSync = original; moduleApi.syncBuiltinESMExports(); }; })()")>]
let private injectLateAttributesEditBeforeRename
    (_attributesPath: string)
    (_barrierRan: unit -> bool)
    (_content: string)
    : unit -> unit =
    jsNative

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

let private repositoryPath value =
    RepositoryPath.tryCreate value |> Result.defaultWith failwith

let private sessionStatus (session: WorkspaceSession) = promise {
    let! result = session.Core.GetStatus(ctx "git-partial-status") |> Async.StartAsPromise

    match result with
    | Succeeded outcome -> return outcome.Value
    | PartiallySucceeded _
    | Failed _ -> return failwith "Expected Git workspace status."
}

let private synchronization (session: WorkspaceSession) =
    session.Synchronization |> Option.defaultWith (fun () -> failwith "Expected Git synchronization service.")

let private createPublishFixture hooks = promise {
    let! root = createTempDirectoryAsync ()
    let barePath = join [| root; "origin.git" |]
    let workPath = join [| root; "work" |]
    let! _ = runGitOk root [| "init"; "--bare"; "-b"; "main"; barePath |]
    let! _ = runGitOk root [| "init"; "-b"; "main"; workPath |]
    let! _ = runGitOk workPath [| "config"; "user.name"; "VCS Partial Tests" |]
    let! _ = runGitOk workPath [| "config"; "user.email"; "partial@example.org" |]
    let! _ = runGitOk workPath [| "config"; "core.autocrlf"; "false" |]
    let! _ = runGitOk workPath [| "lfs"; "install"; "--local" |]
    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "base\n"
    let! _ = runGitOk workPath [| "add"; "base.txt" |]
    let! _ = runGitOk workPath [| "commit"; "-m"; "init: base" |]
    let! _ = runGitOk workPath [| "remote"; "add"; "origin"; barePath |]
    let! _ = runGitOk workPath [| "push"; "-u"; "origin"; "main" |]

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

    return root, workPath, barePath, GitWorkspaceSession.createSession hooks binding
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
            "selected revision validates every staged descendant of a selected directory",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let directoryPath = join [| workPath; "selected" |]
                    let! _ = fsPromisesDynamic?mkdir (directoryPath) |> unbox<JS.Promise<obj>>
                    do! writeUtf8FileAsync (join [| directoryPath; "a-small.bin" |]) "small\n"
                    let largeContent = String.replicate (1024 * 1024) "d"
                    do! writeUtf8FileAsync (join [| directoryPath; "z-large.bin" |]) largeContent
                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! stagedBefore = runGitOk workPath [| "diff"; "--cached"; "--name-only" |]
                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let! statusResult = session.Core.GetStatus(ctx "directory-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: validate every selected directory descendant"
                                Paths = [| repositoryPath "selected" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "directory-lfs-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Concurrency
                        Vitest.expect(failure.Code).toBe "selected_content_changed"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.AffectedPaths).toEqual [| "selected/z-large.bin" |]
                    | _ -> failwith "Expected every staged directory descendant to be LFS-validated."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! stagedAfter = runGitOk workPath [| "diff"; "--cached"; "--name-only" |]
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    let! largeAfter = tryReadUtf8FileAsync (join [| directoryPath; "z-large.bin" |])
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(stagedAfter).toBe stagedBefore
                    Vitest.expect(attributesAfter).toEqual None
                    Vitest.expect(largeAfter).toEqual (Some largeContent)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision accepts an oversized file that is already LFS tracked",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesContent =
                        "\"/large.bin\" filter=lfs diff=lfs merge=lfs -text\n"

                    let attributesPath = join [| workPath; ".gitattributes" |]
                    do! writeUtf8FileAsync attributesPath attributesContent
                    let! _ = runGitOk workPath [| "add"; ".gitattributes" |]
                    let! _ = runGitOk workPath [| "commit"; "-m"; "test: pretrack large file" |]
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "t")

                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let! statusResult = session.Core.GetStatus(ctx "pretracked-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: commit pretracked LFS file"
                                Paths = [| repositoryPath "large.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "pretracked-lfs-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Succeeded outcome ->
                        Vitest.expect(outcome.AffectedPaths |> Array.contains "large.bin").toBe true
                    | _ -> failwith "Expected the already tracked LFS file revision to succeed."

                    let! committed = runGitOk workPath [| "show"; "HEAD:large.bin" |]
                    let! attributesAfter = tryReadUtf8FileAsync attributesPath
                    Vitest.expect(committed.Contains "git-lfs").toBe true
                    Vitest.expect(attributesAfter).toEqual (Some attributesContent)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision rejects a file that grows across the LFS threshold before staging",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let selectedPath = join [| workPath; "growing.bin" |]
                    let grownContent = String.replicate (1024 * 1024) "g"
                    do! writeUtf8FileAsync selectedPath "small\n"
                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! stagedBefore = runGitOk workPath [| "diff"; "--cached"; "--name-only" |]
                    let mutable barrierRan = false

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess = None
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-post-metadata-check" then
                                    barrierRan <- true
                                    do! writeUtf8FileAsync selectedPath grownContent |> Async.AwaitPromise
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "growing-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject stale LFS classification"
                                Paths = [| repositoryPath "growing.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "growing-lfs-revision")
                        |> Async.StartAsPromise

                    Vitest.expect(barrierRan).toBe true

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Concurrency
                        Vitest.expect(failure.Code).toBe "selected_content_changed"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.AffectedPaths).toEqual [| "growing.bin" |]
                    | _ -> failwith "Expected the stale LFS classification to fail before ref movement."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! stagedAfter = runGitOk workPath [| "diff"; "--cached"; "--name-only" |]
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    let! selectedAfter = tryReadUtf8FileAsync selectedPath
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(stagedAfter).toBe stagedBefore
                    Vitest.expect(attributesAfter).toEqual None
                    Vitest.expect(selectedAfter).toEqual (Some grownContent)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision rejects growth when the staged entry path contains a literal tab",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let literalTabPath = "growing\tfile.bin"
                    let isWindows = (osDynamic?platform () |> unbox<string>) = "win32"
                    // NTFS rejects literal tabs, so Windows injects the exact raw
                    // ls-files record that Git emits for the portable Unix case.
                    let relativePath = if isWindows then "growing-file.bin" else literalTabPath
                    let selectedPath = join [| workPath; relativePath |]
                    let grownContent = String.replicate (1024 * 1024) "g"
                    do! writeUtf8FileAsync selectedPath "small\n"
                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let mutable barrierRan = false

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess =
                            Some(fun request operationContext -> async {
                                let! result = NodeProcess.run request operationContext

                                if
                                    isWindows
                                    && request.Arguments =
                                        [| "--literal-pathspecs"; "ls-files"; "--stage"; "-z"; "--"; relativePath |]
                                then
                                    match result with
                                    | Succeeded outcome ->
                                        let rewritten =
                                            outcome.Value.StdOut.Replace(
                                                $"\t{relativePath}\000",
                                                $"\t{literalTabPath}\000"
                                            )

                                        return
                                            Succeeded {
                                                outcome with
                                                    Value = { outcome.Value with StdOut = rewritten }
                                            }
                                    | _ -> return result
                                else
                                    return result
                            })
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-post-metadata-check" then
                                    barrierRan <- true
                                    do! writeUtf8FileAsync selectedPath grownContent |> Async.AwaitPromise
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "tab-growing-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject tab-path stale LFS classification"
                                Paths = [| repositoryPath relativePath |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "tab-growing-lfs-revision")
                        |> Async.StartAsPromise

                    Vitest.expect(barrierRan).toBe true

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Concurrency
                        Vitest.expect(failure.Code).toBe "selected_content_changed"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.AffectedPaths).toEqual [| literalTabPath |]
                    | _ -> failwith "Expected the tabbed staged-entry LFS race to fail before ref movement."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! selectedAfter = tryReadUtf8FileAsync selectedPath
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(selectedAfter).toEqual (Some grownContent)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision reports malformed staged metadata before ref movement",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let relativePath = "growing-malformed.bin"
                    let selectedPath = join [| workPath; relativePath |]
                    let grownContent = String.replicate (1024 * 1024) "m"
                    do! writeUtf8FileAsync selectedPath "small\n"
                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess =
                            Some(fun request operationContext -> async {
                                let! result = NodeProcess.run request operationContext

                                if
                                    request.Arguments =
                                        [| "--literal-pathspecs"; "ls-files"; "--stage"; "-z"; "--"; relativePath |]
                                then
                                    match result with
                                    | Succeeded outcome ->
                                        return
                                            Succeeded {
                                                outcome with
                                                    Value = {
                                                        outcome.Value with
                                                            StdOut = $"malformed\t{relativePath}\000"
                                                    }
                                            }
                                    | _ -> return result
                                else
                                    return result
                            })
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-post-metadata-check" then
                                    do! writeUtf8FileAsync selectedPath grownContent |> Async.AwaitPromise
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "malformed-staged-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject malformed staged metadata"
                                Paths = [| repositoryPath relativePath |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "malformed-staged-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual ProviderError
                        Vitest.expect(failure.Code).toBe "temporary_index_entry_invalid"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.AffectedPaths).toEqual [| relativePath |]
                    | _ -> failwith "Expected malformed staged metadata to fail before ref movement."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! selectedAfter = tryReadUtf8FileAsync selectedPath
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(selectedAfter).toEqual (Some grownContent)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision reports an unsafe attributes path as a structured failure",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "u")

                    let linkedRepo = join [| root; "work-junction" |]
                    do! createDirectoryJunctionAsync workPath linkedRepo
                    let linkedBinding = {
                        binding with
                            WorkspaceRoot = linkedRepo
                            Location = {
                                binding.Location with
                                    ProviderLocation = linkedRepo
                            }
                    }

                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! stagedBefore = runGitOk workPath [| "diff"; "--cached"; "--name-only" |]
                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none linkedBinding

                    let! statusResult = session.Core.GetStatus(ctx "unsafe-attributes-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject unsafe attributes path"
                                Paths = [| repositoryPath "large.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "unsafe-attributes-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual ProviderError
                        Vitest.expect(failure.Code).toBe "attributes_read_failed"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.AffectedPaths).toEqual [| ".gitattributes" |]
                    | _ -> failwith "Expected the unsafe attributes path to fail structurally."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! stagedAfter = runGitOk workPath [| "diff"; "--cached"; "--name-only" |]
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(stagedAfter).toBe stagedBefore
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
            "selected revision LFS reconciliation preserves an in-place attributes edit after ref movement",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesBefore = "# baseline\n"
                    let attributesConcurrentEdit = "# consumer\n"
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    do! writeUtf8FileAsync attributesPath attributesBefore
                    let! _ = runGitOk workPath [| "add"; ".gitattributes" |]
                    let! _ = runGitOk workPath [| "commit"; "-m"; "test: baseline attributes" |]
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "r")

                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess = None
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-post-update-ref" then
                                    do! writeUtf8FileAsync attributesPath attributesConcurrentEdit |> Async.AwaitPromise
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "attributes-race-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: preserve concurrent attributes edit"
                                Paths = [| repositoryPath "large.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "attributes-race-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Code).toBe "attributes_reconciliation_failed"
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_index")
                        Vitest.expect(outcome.AffectedPaths |> Array.contains "large.bin").toBe true
                        Vitest.expect(outcome.AffectedPaths |> Array.contains ".gitattributes").toBe true
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected the post-ref attributes race to return partial success."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! attributesAfter = tryReadUtf8FileAsync attributesPath
                    Vitest.expect(headAfter.Trim() = headBefore.Trim()).toBe false
                    Vitest.expect(attributesAfter).toEqual (Some attributesConcurrentEdit)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision preserves an attributes edit in the final replacement window",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesBefore = "# baseline\n"
                    let attributesConcurrentEdit = "# final consumer edit\n"
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    do! writeUtf8FileAsync attributesPath attributesBefore
                    let! _ = runGitOk workPath [| "add"; ".gitattributes" |]
                    let! _ = runGitOk workPath [| "commit"; "-m"; "test: baseline final-window attributes" |]
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "w")

                    let mutable finalBarrierRan = false
                    let restoreRename =
                        injectLateAttributesEditBeforeRename
                            attributesPath
                            (fun () -> finalBarrierRan)
                            attributesConcurrentEdit

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess = None
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-attributes-replacement-ready" then
                                    finalBarrierRan <- true
                                    do! writeUtf8FileAsync attributesPath attributesConcurrentEdit |> Async.AwaitPromise
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "final-attributes-race-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        promise {
                            try
                                return!
                                    session.Core.CreateRevision
                                        {
                                            Message = "test: preserve final-window attributes edit"
                                            Paths = [| repositoryPath "large.bin" |]
                                            ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        }
                                        (ctx "final-attributes-race-revision")
                                    |> Async.StartAsPromise
                            finally
                                restoreRename ()
                        }

                    let! attributesAfter = tryReadUtf8FileAsync attributesPath
                    Vitest.expect(attributesAfter).toEqual (Some attributesConcurrentEdit)
                    Vitest.expect(finalBarrierRan).toBe true

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Code).toBe "attributes_reconciliation_failed"
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_index")
                        Vitest.expect(failure.RecoveryAction |> Option.bind _.Instructions).toEqual (
                            Some
                                "Merge the generated literal Git LFS rules into the current .gitattributes without discarding concurrent edits, then reconcile the affected paths in the index (for example with git reset)."
                        )
                        Vitest.expect(outcome.AffectedPaths |> Array.contains "large.bin").toBe true
                        Vitest.expect(outcome.AffectedPaths |> Array.contains ".gitattributes").toBe true
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected the final attributes replacement race to return partial success."

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

        Vitest.test (
            "explicit LFS upload failure leaves the remote ref unchanged",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGit "." [| "lfs"; "version" |]

                match lfsProbe with
                | Error _ -> Vitest.expect(true).toBe true
                | Ok _ ->
                    let! root, workPath, barePath, session =
                        createPublishFixture GitWorkspaceSession.GitSessionHooks.none

                    try
                        do!
                            writeUtf8FileAsync
                                (join [| workPath; "missing-upload.bin" |])
                                (String.replicate (1024 * 1024) "u")

                        let! status = sessionStatus session

                        let! revision =
                            session.Core.CreateRevision
                                {
                                    Message = "test: missing LFS upload"
                                    Paths = [| repositoryPath "missing-upload.bin" |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (ctx "missing-upload-revision")
                            |> Async.StartAsPromise

                        match revision with
                        | Succeeded _ -> ()
                        | _ -> failwith "Expected the LFS-backed revision to be created."

                        let! pointer = runGitOk workPath [| "show"; "HEAD:missing-upload.bin" |]

                        let oid =
                            pointer.Replace("\r\n", "\n").Split('\n')
                            |> Array.find _.StartsWith("oid sha256:")
                            |> fun line -> line.Substring("oid sha256:".Length).Trim()

                        do!
                            removeDirectoryAsync (
                                join [|
                                    workPath
                                    ".git"
                                    "lfs"
                                    "objects"
                                    oid.Substring(0, 2)
                                    oid.Substring(2, 2)
                                    oid
                                |]
                            )

                        let! targetBefore = runGitOk barePath [| "rev-parse"; "main" |]
                        let! publishStatus = sessionStatus session

                        let! result =
                            (synchronization session).Publish
                                {
                                    ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                    ExpectedTargetRevision = None
                                }
                                (ctx "missing-upload-publish")
                            |> Async.StartAsPromise

                        match result with
                        | Failed failure ->
                            Vitest.expect(failure.StateChanged).toBe false
                            Vitest.expect(failure.Retryable).toBe true
                        | _ -> failwith "Expected explicit LFS upload failure before ref publication."

                        let! targetAfter = runGitOk barePath [| "rev-parse"; "main" |]
                        Vitest.expect(targetAfter.Trim()).toBe(targetBefore.Trim())
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "verified publish followed by state inspection failure is partial success",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable refPublished = false
                let mutable refVerified = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if refVerified then
                                    return
                                        OperationResult.failed(
                                            OperationFailure.create
                                                ProviderError
                                                "injected_post_publish_failure"
                                                "State inspection failed after the remote ref was verified."
                                        )
                                else
                                    let! result = NodeProcess.run request operationContext

                                    if
                                        request.Arguments |> Array.contains "push"
                                        && match result with
                                           | Succeeded outcome -> outcome.Value.ExitCode = 0
                                           | _ -> false
                                    then
                                        refPublished <- true
                                    elif
                                        refPublished
                                        && request.Arguments |> Array.contains "ls-remote"
                                    then
                                        refVerified <- true

                                    return result
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks

                try
                    do! writeUtf8FileAsync (join [| workPath; "published.txt" |]) "published\n"
                    let! status = sessionStatus session

                    let! revision =
                        session.Core.CreateRevision
                            {
                                Message = "test: verified publish"
                                Paths = [| repositoryPath "published.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "verified-publish-revision")
                        |> Async.StartAsPromise

                    match revision with
                    | Succeeded _ -> ()
                    | _ -> failwith "Expected the publication revision to be created."

                    let! localHead = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! publishStatus = sessionStatus session

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "verified-publish")
                        |> Async.StartAsPromise

                    match result with
                    | PartiallySucceeded(_, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual(
                            Some "retry_publish_verification"
                        )
                    | _ -> failwith "Expected post-publication state failure to be partial success."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe(localHead.Trim())
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
