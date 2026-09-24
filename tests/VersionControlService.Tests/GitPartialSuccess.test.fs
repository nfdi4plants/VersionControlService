module VersionControlService.Tests.GitPartialSuccessTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Tests.NodePath
open Vitest

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module GitExecution = VersionControlService.Git.GitExecution
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem

[<Emit("process.execPath")>]
let private nodeExecutablePath: string = jsNative

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

[<Import("vi", "vitest")>]
let private vitestTimers: obj = jsNative

let private createTempDirectoryAsync () : JS.Promise<string> = promise {
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-git-partial-" |]
    let! created = fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>
    // GitHub's Windows runners hand out TEMP as an 8.3 short path (RUNNER~1). Git reports the
    // long form, so resolve the directory once here and every path comparison agrees.
    return! fsPromisesDynamic?realpath (created) |> unbox<JS.Promise<string>>
}

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

let private renameAsync (sourcePath: string) (destinationPath: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?rename (sourcePath, destinationPath) |> unbox<JS.Promise<obj>>
    return ()
}

[<Emit("(() => { const fs = require('node:fs'); const moduleApi = require('node:module'); const original = fs.lstatSync; fs.lstatSync = path => { const stats = original(path); if (path === $0 && $1()) return new Proxy(stats, { get(target, property, receiver) { return property === 'isSymbolicLink' ? (() => true) : Reflect.get(target, property, receiver); } }); return stats; }; moduleApi.syncBuiltinESMExports(); return () => { fs.lstatSync = original; moduleApi.syncBuiltinESMExports(); }; })()")>]
let private injectAttributesSymlinkIdentity
    (_attributesPath: string)
    (_enabled: unit -> bool)
    : unit -> unit =
    jsNative

[<Emit("(() => { const fs = require('node:fs'); const moduleApi = require('node:module'); const originalOpen = fs.openSync; const originalFsync = fs.fsyncSync; let targetDescriptor; fs.openSync = (path, ...args) => { const descriptor = originalOpen(path, ...args); if (String(path).startsWith($0 + '.vcs-') && String(path).endsWith('.tmp')) targetDescriptor = descriptor; return descriptor; }; fs.fsyncSync = descriptor => { if (descriptor === targetDescriptor) { targetDescriptor = undefined; const error = new Error('injected attributes temp fsync failure'); error.code = 'EIO'; throw error; } return originalFsync(descriptor); }; moduleApi.syncBuiltinESMExports(); return () => { fs.openSync = originalOpen; fs.fsyncSync = originalFsync; moduleApi.syncBuiltinESMExports(); }; })()")>]
let private injectAttributesTempFsyncFailure (_attributesPath: string) : unit -> unit = jsNative

[<Emit("(() => { const fs = require('node:fs'); const moduleApi = require('node:module'); const original = fs.linkSync; fs.linkSync = (existingPath, newPath) => { if (newPath === $0) { $1(); const error = new Error('injected hard-link restriction'); error.code = 'EPERM'; throw error; } return original(existingPath, newPath); }; moduleApi.syncBuiltinESMExports(); return () => { fs.linkSync = original; moduleApi.syncBuiltinESMExports(); }; })()")>]
let private injectAttributesLinkFailure
    (_attributesPath: string)
    (_onAttempt: unit -> unit)
    : unit -> unit =
    jsNative

[<Emit("(async () => { const fsp = require('node:fs/promises'); const probe = await fsp.open($0, 'r'); const prototype = Object.getPrototypeOf(probe); await probe.close(); const original = prototype.sync; let injected = false; prototype.sync = function() { if (!injected) { injected = true; const error = new Error('injected exclusive attributes fsync failure'); error.code = 'EIO'; return Promise.reject(error); } return original.call(this); }; return () => { prototype.sync = original; }; })()")>]
let private injectNextFileHandleSyncFailure (_probePath: string) : JS.Promise<unit -> unit> = jsNative

[<Emit("(async () => { const fs = require('node:fs'); const fsp = require('node:fs/promises'); const probe = await fsp.open($0, 'r'); const prototype = Object.getPrototypeOf(probe); await probe.close(); const original = prototype.sync; let injected = false; prototype.sync = function() { if (!injected) { injected = true; fs.writeFileSync($1, $2, 'utf8'); $3(); const error = new Error('injected exclusive attributes fsync failure after consumer edit'); error.code = 'EIO'; return Promise.reject(error); } return original.call(this); }; return () => { prototype.sync = original; }; })()")>]
let private injectNextFileHandleSyncFailureAfterAttributesEdit
    (_probePath: string)
    (_attributesPath: string)
    (_consumerContent: string)
    (_onInjection: unit -> unit)
    : JS.Promise<unit -> unit> =
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

let private pathExistsAsync (path: string) : JS.Promise<bool> = promise {
    try
        let! _ = fsPromisesDynamic?access (path) |> unbox<JS.Promise<obj>>
        return true
    with _ ->
        return false
}

[<Emit("require('node:crypto').createHash('sha256').update($0, 'utf8').digest('hex')")>]
let private sha256Utf8 (_value: string) : string = jsNative

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

let private isLfsPullRequest (request: NodeProcess.ProcessRequest) =
    request.Arguments
    |> Array.windowed 2
    |> Array.exists (fun pair -> pair.[0] = "lfs" && pair.[1] = "pull")

let private isGitCloneRequest (request: NodeProcess.ProcessRequest) =
    request.Arguments |> Array.contains "clone"

let private createLfsCloneFixture () = promise {
    let! root = createTempDirectoryAsync ()
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
    return root, barePath
}

let private createTextPublishRevision
    (session: WorkspaceSession)
    (workPath: string)
    (path: string)
    (operationName: string)
    =
    promise {
        do! writeUtf8FileAsync (join [| workPath; path |]) "published\n"
        let! status = sessionStatus session

        let! revision =
            session.Core.CreateRevision
                {
                    Message = $"test: {operationName}"
                    Paths = [| repositoryPath path |]
                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                }
                (ctx $"{operationName}-revision")
            |> Async.StartAsPromise

        match revision with
        | Succeeded _ -> ()
        | _ -> failwith "Expected the publication revision to be created."

        let! localHead = runGitOk workPath [| "rev-parse"; "HEAD" |]
        let! publishStatus = sessionStatus session
        return localHead.Trim(), publishStatus
    }

Vitest.describe (
    "Git workspace partial success",
    fun () ->
        Vitest.test (
            "CreateRevision refuses an existing index lock before writing objects",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "changed before lock\n"
                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let session = GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding
                    let! statusResult = session.Core.GetStatus(ctx "index-lock-revision-status") |> Async.StartAsPromise

                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected status before the locked revision."

                    let! objectsBefore = runGitOk workPath [| "count-objects"; "-v" |]
                    do! writeUtf8FileAsync (join [| workPath; ".git"; "index.lock" |]) "stale lock\n"

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject revision with index lock"
                                Paths = [| repositoryPath "base.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "index-lock-create-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Concurrency
                        Vitest.expect(failure.Code).toBe "index_locked"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.Retryable).toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "remove_index_lock")
                        Vitest.expect(
                            failure.Details |> Array.exists (fun detail -> detail.StartsWith "Lock file age:")
                        ).toBe true
                    | _ -> failwith "Expected the index lock to reject CreateRevision."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! objectsAfter = runGitOk workPath [| "count-objects"; "-v" |]
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(objectsAfter).toBe objectsBefore
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "direct git runs carry the resolved environment overrides",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, _, binding = createSelectedRevisionFixture ()
                let requests = ResizeArray<NodeProcess.ProcessRequest>()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                requests.Add request
                                return! NodeProcess.run request operationContext
                            })
                    Barrier = None
                }

                try
                    let session = GitWorkspaceSession.createSession hooks binding
                    let! _ = sessionStatus session
                    let expectedPathOverrides =
                        GitExecution.environmentOverrides ()
                        |> Array.filter (fun (name, _) -> name = "PATH")

                    Vitest.expect(requests.Count > 0).toBe true

                    for request in requests do
                        let actualPathOverrides =
                            request.Environment
                            |> Array.filter (fun (name, _) -> name = "PATH")

                        Vitest.expect(actualPathOverrides).toEqual(expectedPathOverrides)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "index lock diagnostics keep lock paths with apostrophes and linked-worktree lock paths",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()
                let mutable injectedLockPath: string option = None

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request operationContext ->
                                async {
                                    match injectedLockPath with
                                    | Some lockPath when Array.contains "commit-tree" request.Arguments ->
                                        let output: NodeProcess.ProcessOutput = {
                                            ExitCode = 128
                                            StdOut = ""
                                            StdErr = $"fatal: Unable to create '{lockPath}': File exists.\n"
                                        }

                                        return OperationResult.succeeded output
                                    | _ -> return! NodeProcess.run request operationContext
                                })
                }

                try
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "changed before the locked revision\n"
                    let session = GitWorkspaceSession.createSession hooks binding
                    let! status = sessionStatus session

                    for lockPath in
                        [|
                            @"C:\Users\carol\repo's files\.git\index.lock"
                            @"C:\Users\carol\repo\.git\worktrees\wt\index.lock"
                        |] do
                        injectedLockPath <- Some lockPath

                        let! revisionResult =
                            session.Core.CreateRevision
                                {
                                    Message = "test: parse the index lock path"
                                    Paths = [| repositoryPath "base.txt" |]
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                }
                                (ctx "index-lock-path-parsing")
                            |> Async.StartAsPromise

                        match revisionResult with
                        | Failed failure ->
                            Vitest.expect(failure.Code).toBe "index_locked"
                            Vitest.expect(failure.Details |> Array.contains $"Lock file: {lockPath}").toBe true
                        | _ -> failwith $"Expected the lock diagnostic for {lockPath} to fail CreateRevision."

                    injectedLockPath <- None
                    do! removeDirectoryAsync root
                with error ->
                    injectedLockPath <- None
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a post-commit index lock preserves index_locked details and requests index reconciliation",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()
                let lockPath = join [| workPath; ".git"; "index.lock" |]
                let mutable refUpdated = false
                let mutable lockCreated = false

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request operationContext ->
                                async {
                                    if Array.contains "update-ref" request.Arguments then
                                        let! result = NodeProcess.run request operationContext

                                        match result with
                                        | Succeeded outcome when outcome.Value.ExitCode = 0 -> refUpdated <- true
                                        | _ -> ()

                                        return result
                                    elif refUpdated && Array.contains "reset" request.Arguments then
                                        lockCreated <- true
                                        do!
                                            writeUtf8FileAsync lockPath "lock created during index reconciliation\n"
                                            |> Async.AwaitPromise

                                        let output: NodeProcess.ProcessOutput = {
                                            ExitCode = 128
                                            StdOut = ""
                                            StdErr = $"fatal: Unable to create '{lockPath}': File exists.\n"
                                        }

                                        return OperationResult.succeeded output
                                    else
                                        return! NodeProcess.run request operationContext
                                })
                }

                try
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "committed before index lock\n"
                    let session = GitWorkspaceSession.createSession hooks binding
                    let! beforeStatus = sessionStatus session
                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: report post-commit index lock"
                                Paths = [| repositoryPath "base.txt" |]
                                ExpectedWorkspaceVersion = beforeStatus.WorkspaceVersion
                            }
                            (ctx "post-commit-index-lock")
                        |> Async.StartAsPromise

                    Vitest.expect(refUpdated).toBe true
                    Vitest.expect(lockCreated).toBe true

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.Category).toEqual Concurrency
                        Vitest.expect(failure.Code).toBe "index_locked"
                        Vitest.expect(failure.Details |> Array.contains $"Lock file: {lockPath}").toBe true
                        Vitest.expect(
                            failure.Details |> Array.exists (fun detail -> detail.StartsWith "Lock file age:")
                        ).toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_index")
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected a partial revision after the committed ref could not reconcile the index."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "tracking then untracking a literal path removes exactly its rule",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()
                let relativePath = "assets/my-file name.bin"

                try
                    let assetsPath = join [| workPath; "assets" |]
                    let! _ = fsPromisesDynamic?mkdir assetsPath |> unbox<JS.Promise<obj>>
                    do! writeUtf8FileAsync (join [| workPath; relativePath |]) "literal content\n"

                    let unrelatedRule = "*.dat filter=lfs diff=lfs merge=lfs -text"
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    do! writeUtf8FileAsync attributesPath (unrelatedRule + "\n")

                    let storagePolicy =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding
                        |> fun session -> session.StoragePolicy |> Option.defaultWith (fun () -> failwith "Expected storage policy.")

                    let! tracked =
                        storagePolicy.SetPathPolicy
                            (repositoryPath relativePath)
                            true
                            (ctx "literal-track")
                        |> Async.StartAsPromise

                    match tracked with
                    | Succeeded _ -> ()
                    | _ -> failwith "Literal tracking failed."

                    let! _ = runGitOk workPath [| "add"; ".gitattributes" |]
                    let! untracked =
                        storagePolicy.SetPathPolicy
                            (repositoryPath relativePath)
                            false
                            (ctx "literal-untrack")
                        |> Async.StartAsPromise

                    match untracked with
                    | Succeeded _ -> ()
                    | _ -> failwith "Literal untracking failed."

                    let! content = fsPromisesDynamic?readFile (attributesPath, "utf8") |> unbox<JS.Promise<string>>
                    let trackingRule = "\"/assets/my-file name.bin\" filter=lfs diff=lfs merge=lfs -text"

                    Vitest.expect(content.Contains unrelatedRule).toBe true
                    Vitest.expect(content.Contains trackingRule).toBe false
                    Vitest.expect(content.EndsWith("\n")).toBe true

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "reports transfer success with hydration recovery",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // Requires git-lfs; report a pass with a skip note when unavailable.
                let! lfsProbe = runGit "." [| "lfs"; "version" |]

                match lfsProbe with
                | Error _ -> Vitest.expect(true).toBe (true)
                | Ok _ ->
                    let! root, barePath = createLfsCloneFixture ()

                    try
                        let hooks = {
                            GitWorkspaceSession.GitSessionHooks.none with
                                RunProcess =
                                    Some(fun request context ->
                                        if isLfsPullRequest request then
                                            async.Return(
                                                OperationResult.succeeded {
                                                    ExitCode = 2
                                                    StdOut = ""
                                                    StdErr = "injected lfs pull failure"
                                                }
                                            )
                                        else
                                            NodeProcess.run request context)
                        }

                        let factory = GitWorkspaceSession.createFactory hooks

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
            "canceled LFS clone removes only a target it created",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGit "." [| "lfs"; "version" |]

                match lfsProbe with
                | Error _ -> Vitest.expect(true).toBe (true)
                | Ok _ ->
                    let! root, barePath = createLfsCloneFixture ()

                    try
                        let blockedTransfer =
                            NodeProcess.ProcessRequest.create
                                nodeExecutablePath
                                [| "-e"; "console.log('lfs-transfer-started'); setInterval(()=>{}, 1000)" |]

                        let hooks = {
                            GitWorkspaceSession.GitSessionHooks.none with
                                RunProcess =
                                    Some(fun request context ->
                                        if isLfsPullRequest request then
                                            NodeProcess.run blockedTransfer context
                                        else
                                            NodeProcess.run request context)
                        }

                        let factory = GitWorkspaceSession.createFactory hooks

                        let cloneTo targetPath operationId = promise {
                            let source = OperationCancellation.Source()

                            let context =
                                OperationContext.create operationId source.Cancellation (fun progress ->
                                    if progress.DisplayMessage = Some "lfs-transfer-started" then
                                        source.Cancel())

                            return!
                                Async.StartAsPromise(
                                    factory.Clone
                                        {
                                            Location = {
                                                ProviderId = gitProviderId
                                                DisplayName = None
                                                ProviderLocation = barePath
                                                ConnectionProfileId = None
                                            }
                                            TargetPath = targetPath
                                            TargetRef = None
                                            MaterializeAllObjects = true
                                        }
                                        context
                                )
                        }

                        let expectCanceled result =
                            match result with
                            | Failed failure ->
                                Vitest.expect(failure.Category).toEqual (Canceled)
                                Vitest.expect(failure.Code).toBe ("operation_canceled")
                                Vitest.expect(failure.StateChanged).toBe (false)
                            | Succeeded _
                            | PartiallySucceeded _ -> failwith "Expected the clone to be canceled."

                        let missingTarget = join [| root; "cancel-missing-clone" |]
                        let! missingResult = cloneTo missingTarget "cancel-missing-clone"
                        expectCanceled missingResult
                        let! missingExists = pathExistsAsync missingTarget
                        Vitest.expect(missingExists).toBe (false)

                        let emptyTarget = join [| root; "cancel-empty-clone" |]
                        NodeFileSystem.mkdirSync emptyTarget (NodeFileSystem.MkdirOptions(recursive = false))
                        let! emptyResult = cloneTo emptyTarget "cancel-empty-clone"
                        expectCanceled emptyResult
                        let! emptyExists = pathExistsAsync emptyTarget
                        Vitest.expect(emptyExists).toBe (true)
                        Vitest.expect(NodeFileSystem.readdirSync emptyTarget).toHaveLength (0)

                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "cancellation during git clone removes a created target",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGit "." [| "lfs"; "version" |]

                match lfsProbe with
                | Error _ -> Vitest.expect(true).toBe (true)
                | Ok _ ->
                    let! root, barePath = createLfsCloneFixture ()

                    try
                        let source = OperationCancellation.Source()

                        let hooks = {
                            GitWorkspaceSession.GitSessionHooks.none with
                                RunProcess =
                                    Some(fun request context ->
                                        if isGitCloneRequest request then
                                            let targetPath = request.Arguments |> Array.last

                                            let script =
                                                "const fs=require('node:fs');const path=require('node:path');" +
                                                "const fd=fs.openSync(path.join(process.argv[1],'clone-residue.txt'),'w');fs.writeSync(fd,'residue');" +
                                                "console.log('clone-started');setInterval(()=>{},1000)"

                                            // The child runs inside the target and keeps a file open there, as a
                                            // killed git does on Windows until the rollback removes the target.
                                            let blockedClone = {
                                                NodeProcess.ProcessRequest.create
                                                    nodeExecutablePath
                                                    [| "-e"; script; targetPath |] with
                                                    WorkingDirectory = Some targetPath
                                            }

                                            NodeProcess.run blockedClone context
                                        else
                                            NodeProcess.run request context)
                        }

                        let context =
                            OperationContext.create "cancel-during-clone" source.Cancellation (fun progress ->
                                if progress.DisplayMessage = Some "clone-started" then
                                    source.Cancel())

                        let factory = GitWorkspaceSession.createFactory hooks
                        let targetPath = join [| root; "cancel-during-clone" |]

                        let! result =
                            Async.StartAsPromise(
                                factory.Clone
                                    {
                                        Location = {
                                            ProviderId = gitProviderId
                                            DisplayName = None
                                            ProviderLocation = barePath
                                            ConnectionProfileId = None
                                        }
                                        TargetPath = targetPath
                                        TargetRef = None
                                        MaterializeAllObjects = true
                                    }
                                    context
                            )

                        match result with
                        | Failed failure ->
                            Vitest.expect(failure.Category).toEqual (Canceled)
                            Vitest.expect(failure.Code).toBe ("operation_canceled")
                            Vitest.expect(failure.StateChanged).toBe (false)
                        | Succeeded _
                        | PartiallySucceeded _ -> failwith "Expected the clone to be canceled."

                        let! targetExists = pathExistsAsync targetPath
                        Vitest.expect(targetExists).toBe (false)
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "cancellation between clone and LFS pull removes the created target",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGit "." [| "lfs"; "version" |]

                match lfsProbe with
                | Error _ -> Vitest.expect(true).toBe (true)
                | Ok _ ->
                    let! root, barePath = createLfsCloneFixture ()

                    try
                        let source = OperationCancellation.Source()
                        let mutable lfsPullSeen = false

                        let hooks = {
                            GitWorkspaceSession.GitSessionHooks.none with
                                RunProcess =
                                    Some(fun request context ->
                                        if isLfsPullRequest request then
                                            lfsPullSeen <- true
                                            NodeProcess.run request context
                                        elif isGitCloneRequest request then
                                            async {
                                                let! result = NodeProcess.run request context
                                                source.Cancel()
                                                return result
                                            }
                                        else
                                            NodeProcess.run request context)
                        }

                        let targetPath = join [| root; "cancel-between-commands" |]
                        let context = OperationContext.create "cancel-between-commands" source.Cancellation ignore
                        let factory = GitWorkspaceSession.createFactory hooks

                        let! result =
                            Async.StartAsPromise(
                                factory.Clone
                                    {
                                        Location = {
                                            ProviderId = gitProviderId
                                            DisplayName = None
                                            ProviderLocation = barePath
                                            ConnectionProfileId = None
                                        }
                                        TargetPath = targetPath
                                        TargetRef = None
                                        MaterializeAllObjects = true
                                    }
                                    context
                            )

                        match result with
                        | Failed failure ->
                            Vitest.expect(failure.Category).toEqual (Canceled)
                            Vitest.expect(failure.Code).toBe ("operation_canceled")
                            Vitest.expect(failure.StateChanged).toBe (false)
                        | Succeeded _
                        | PartiallySucceeded _ -> failwith "Expected the clone to be canceled."

                        Vitest.expect(lfsPullSeen).toBe (false)
                        let! targetExists = pathExistsAsync targetPath
                        Vitest.expect(targetExists).toBe (false)
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "cancellation after LFS pull exits preserves clone success",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGit "." [| "lfs"; "version" |]

                match lfsProbe with
                | Error _ -> Vitest.expect(true).toBe (true)
                | Ok _ ->
                    let! root, barePath = createLfsCloneFixture ()

                    try
                        let source = OperationCancellation.Source()
                        let mutable lfsPullSeen = false

                        let hooks = {
                            GitWorkspaceSession.GitSessionHooks.none with
                                RunProcess =
                                    Some(fun request context ->
                                        if isLfsPullRequest request then
                                            async {
                                                let! result = NodeProcess.run request context
                                                lfsPullSeen <- true
                                                source.Cancel()
                                                return result
                                            }
                                        else
                                            NodeProcess.run request context)
                        }

                        let targetPath = join [| root; "late-cancel-clone" |]
                        let context = OperationContext.create "late-cancel-clone" source.Cancellation ignore
                        let factory = GitWorkspaceSession.createFactory hooks

                        let! result =
                            Async.StartAsPromise(
                                factory.Clone
                                    {
                                        Location = {
                                            ProviderId = gitProviderId
                                            DisplayName = None
                                            ProviderLocation = barePath
                                            ConnectionProfileId = None
                                        }
                                        TargetPath = targetPath
                                        TargetRef = None
                                        MaterializeAllObjects = true
                                    }
                                    context
                            )

                        match result with
                        | Succeeded outcome ->
                            Vitest.expect(outcome.Value.WorkspaceRoot).toBe (targetPath)
                        | Failed failure ->
                            failwith $"Expected clone success after late cancellation ({failure.Code})."
                        | PartiallySucceeded _ -> failwith "Expected clone success after late cancellation."

                        Vitest.expect(lfsPullSeen).toBe (true)
                        let! targetExists = pathExistsAsync targetPath
                        Vitest.expect(targetExists).toBe (true)
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "large-object listing preserves operational failures",
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
            "object materialization preserves dirty consumer content on rejection",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGit "." [| "lfs"; "version" |]

                match lfsProbe with
                | Error _ -> Vitest.expect(true).toBe true
                | Ok _ ->
                    let! root, workPath, _, session =
                        createPublishFixture GitWorkspaceSession.GitSessionHooks.none

                    try
                        let objectPath = join [| workPath; "guarded.bin" |]
                        let! _ = runGitOk workPath [| "lfs"; "track"; "guarded.bin" |]
                        do! writeUtf8FileAsync objectPath "tracked object content\n"
                        let! _ = runGitOk workPath [| "add"; ".gitattributes"; "guarded.bin" |]
                        let! _ = runGitOk workPath [| "commit"; "-m"; "test: tracked object" |]
                        let! _ = runGitOk workPath [| "push"; "origin"; "main" |]

                        let materialization =
                            session.ObjectMaterialization
                            |> Option.defaultWith (fun () -> failwith "Expected Git object materialization service.")

                        let consumerEdit = "dirty consumer content\n"
                        do! writeUtf8FileAsync objectPath consumerEdit

                        let! dematerializeResult =
                            materialization.Dematerialize
                                (repositoryPath "guarded.bin")
                                (ctx "dirty-dematerialize")
                            |> Async.StartAsPromise

                        match dematerializeResult with
                        | Failed failure -> Vitest.expect(failure.StateChanged).toBe false
                        | Succeeded _
                        | PartiallySucceeded _ -> failwith "Dirty object dematerialization must fail."

                        let! afterDematerialize = tryReadUtf8FileAsync objectPath
                        Vitest.expect(afterDematerialize).toEqual (Some consumerEdit)

                        let! _ = runGitOk workPath [| "checkout"; "--"; "guarded.bin" |]

                        let! cleanDematerialize =
                            materialization.Dematerialize
                                (repositoryPath "guarded.bin")
                                (ctx "clean-dematerialize")
                            |> Async.StartAsPromise

                        match cleanDematerialize with
                        | Succeeded _ -> ()
                        | PartiallySucceeded(_, failure)
                        | Failed failure -> failwith $"Clean dematerialization failed ({failure.Code})."

                        let dirtyPointerEdit = "dirty pointer replacement\n"
                        do! writeUtf8FileAsync objectPath dirtyPointerEdit

                        let! materializeResult =
                            materialization.Materialize
                                (repositoryPath "guarded.bin")
                                (ctx "dirty-materialize")
                            |> Async.StartAsPromise

                        match materializeResult with
                        | Failed failure -> Vitest.expect(failure.StateChanged).toBe false
                        | Succeeded _
                        | PartiallySucceeded _ -> failwith "Dirty object materialization must fail."

                        let! afterMaterialize = tryReadUtf8FileAsync objectPath
                        Vitest.expect(afterMaterialize).toEqual (Some dirtyPointerEdit)
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
                    let! statusBefore = runGitOk workPath [| "status"; "--porcelain" |]
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
                    let! statusAfter = runGitOk workPath [| "status"; "--porcelain" |]
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
                    let! statusBefore = runGitOk workPath [| "status"; "--porcelain" |]

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
                    let! statusAfter = runGitOk workPath [| "status"; "--porcelain" |]
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
            "selected revision LFS tracks every staged descendant of a selected directory",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let directoryPath = join [| workPath; "selected" |]
                    let! _ = fsPromisesDynamic?mkdir (directoryPath) |> unbox<JS.Promise<obj>>
                    do! writeUtf8FileAsync (join [| directoryPath; "a-small.bin" |]) "small\n"
                    let largeContent = String.replicate (1024 * 1024) "d"
                    do! writeUtf8FileAsync (join [| directoryPath; "z-large.bin" |]) largeContent
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
                    | Succeeded _ -> ()
                    | _ -> failwith "Expected every staged directory descendant to be LFS-tracked."

                    let! largeCommitted = runGitOk workPath [| "show"; "HEAD:selected/z-large.bin" |]
                    let! committedAttributes = runGitOk workPath [| "show"; "HEAD:.gitattributes" |]
                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    Vitest.expect(largeCommitted.Contains "git-lfs").toBe true
                    Vitest.expect(committedAttributes.Contains "\"/selected/z-large.bin\" filter=lfs").toBe true
                    Vitest.expect(attributesAfter |> Option.exists _.Contains("\"/selected/z-large.bin\" filter=lfs")).toBe true
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision LFS skips ignored directory descendants without creating orphan objects",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let directoryPath = join [| workPath; "selected" |]
                    let! _ = fsPromisesDynamic?mkdir (directoryPath) |> unbox<JS.Promise<obj>>
                    do! writeUtf8FileAsync (join [| workPath; ".gitignore" |]) "selected/ignored.bin\n"
                    do! writeUtf8FileAsync (join [| directoryPath; "tracked.bin" |]) "tracked before growth\n"
                    let! _ = runGitOk workPath [| "add"; ".gitignore"; "selected/tracked.bin" |]
                    let! _ = runGitOk workPath [| "commit"; "-m"; "test: tracked selected directory fixture" |]

                    let trackedContent = String.replicate (1024 * 1024) "t"
                    let ignoredContent = String.replicate (1024 * 1024) "i"
                    do! writeUtf8FileAsync (join [| directoryPath; "tracked.bin" |]) trackedContent
                    do! writeUtf8FileAsync (join [| directoryPath; "ignored.bin" |]) ignoredContent

                    let ignoredOid = sha256Utf8 ignoredContent
                    let ignoredObjectPath =
                        join
                            [|
                                workPath
                                ".git"
                                "lfs"
                                "objects"
                                ignoredOid.Substring(0, 2)
                                ignoredOid.Substring(2, 2)
                                ignoredOid
                            |]

                    let! ignoredObjectBefore = pathExistsAsync ignoredObjectPath
                    Vitest.expect(ignoredObjectBefore).toBe false
                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let! statusResult = session.Core.GetStatus(ctx "ignored-directory-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: skip ignored selected directory descendants"
                                Paths = [| repositoryPath "selected" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "ignored-directory-lfs-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Succeeded _ -> ()
                    | Failed failure ->
                        let! ignoredObjectAfterFailure = pathExistsAsync ignoredObjectPath
                        failwith
                            $"Expected ignored descendants to be skipped; got {failure.Code}; orphaned object: {ignoredObjectAfterFailure}."
                    | PartiallySucceeded(_, failure) ->
                        failwith $"Expected ignored descendants to be skipped; got partial success {failure.Code}."

                    let! trackedCommitted = runGitOk workPath [| "show"; "HEAD:selected/tracked.bin" |]
                    let! ignoredCommitted = runGit workPath [| "show"; "HEAD:selected/ignored.bin" |]
                    let! committedAttributes = runGitOk workPath [| "show"; "HEAD:.gitattributes" |]
                    let! ignoredObjectAfter = pathExistsAsync ignoredObjectPath
                    Vitest.expect(trackedCommitted.Contains "git-lfs").toBe true
                    Vitest.expect(ignoredCommitted |> Result.isError).toBe true
                    Vitest.expect(committedAttributes.Contains "\"/selected/tracked.bin\" filter=lfs").toBe true
                    Vitest.expect(committedAttributes.Contains "ignored.bin").toBe false
                    Vitest.expect(ignoredObjectAfter).toBe false
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision LFS ignores working-tree-only attribute coverage",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesContent = "*.bin filter=lfs diff=lfs merge=lfs -text\n"
                    do! writeUtf8FileAsync (join [| workPath; ".gitattributes" |]) attributesContent
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "w")

                    let! headBefore = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let! statusResult = session.Core.GetStatus(ctx "working-attributes-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: reject working-tree-only LFS coverage"
                                Paths = [| repositoryPath "large.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "working-attributes-lfs-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Validation
                        Vitest.expect(failure.Code).toBe "precondition_failed"
                        Vitest.expect(failure.StateChanged).toBe false
                    | _ -> failwith "Expected working-tree-only attributes to be excluded from commit coverage."

                    let! headAfter = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! committedAttributes = runGit workPath [| "show"; "HEAD:.gitattributes" |]
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(committedAttributes |> Result.isError).toBe true
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision LFS surfaces a candidate stat failure before staging validation",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let selectedPath = join [| workPath; "stat-failure.bin" |]
                    do! writeUtf8FileAsync selectedPath (String.replicate (1024 * 1024) "s")
                    let mutable barrierRan = false

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess = None
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-lfs-candidates-listed" then
                                    barrierRan <- true
                                    let! _ =
                                        fsPromisesDynamic?rm (selectedPath, createObj [ "force" ==> true ])
                                        |> unbox<JS.Promise<obj>>
                                        |> Async.AwaitPromise

                                    ()
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "stat-failure-lfs-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: surface selected LFS stat failure"
                                Paths = [| repositoryPath "stat-failure.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "stat-failure-lfs-revision")
                        |> Async.StartAsPromise

                    Vitest.expect(barrierRan).toBe true

                    match revisionResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual ProviderError
                        Vitest.expect(failure.Code).toBe "lfs_file_stat_failed"
                        Vitest.expect(failure.AffectedPaths).toEqual [| "stat-failure.bin" |]
                    | _ -> failwith "Expected the selected LFS candidate stat failure to surface."

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
                    let! statusBefore = runGitOk workPath [| "status"; "--porcelain" |]

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
                    let! statusAfter = runGitOk workPath [| "status"; "--porcelain" |]
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
                    let! statusBefore = runGitOk workPath [| "status"; "--porcelain" |]
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
                    let! statusAfter = runGitOk workPath [| "status"; "--porcelain" |]
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
            "selected revision removes its attributes temp file when fsync fails",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "f")

                    let restoreFsync = injectAttributesTempFsyncFailure attributesPath
                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let! statusResult = session.Core.GetStatus(ctx "attributes-temp-fsync-status") |> Async.StartAsPromise
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
                                            Message = "test: clean attributes temp after fsync failure"
                                            Paths = [| repositoryPath "large.bin" |]
                                            ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        }
                                        (ctx "attributes-temp-fsync-revision")
                                    |> Async.StartAsPromise
                            finally
                                restoreFsync ()
                        }

                    let! worktreeNames = fsPromisesDynamic?readdir workPath |> unbox<JS.Promise<string[]>>
                    let attributesTemps =
                        worktreeNames
                        |> Array.filter (fun name ->
                            name.StartsWith(".gitattributes.vcs-", StringComparison.Ordinal)
                            && name.EndsWith(".tmp", StringComparison.Ordinal))

                    Vitest.expect(attributesTemps).toEqual [||]
                    let! attributesAfter = tryReadUtf8FileAsync attributesPath
                    Vitest.expect(attributesAfter).toEqual None

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Code).toBe "attributes_reconciliation_failed"
                        Vitest.expect(outcome.AffectedPaths |> Array.contains ".gitattributes").toBe true
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected the temp fsync failure to return partial success."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision falls back to exclusive attributes creation when hard links are restricted",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "h")

                    let mutable linkAttempted = false
                    let restoreLink =
                        injectAttributesLinkFailure attributesPath (fun () -> linkAttempted <- true)

                    let expectedAttributes =
                        "\"/large.bin\" filter=lfs diff=lfs merge=lfs -text\n"

                    let mutable exclusiveInstalled = false
                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess = None
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-attributes-installed" then
                                    let! installed = tryReadUtf8FileAsync attributesPath |> Async.AwaitPromise
                                    exclusiveInstalled <- installed = Some expectedAttributes
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding

                    let! statusResult = session.Core.GetStatus(ctx "attributes-link-fallback-status") |> Async.StartAsPromise
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
                                            Message = "test: fall back from restricted hard links"
                                            Paths = [| repositoryPath "large.bin" |]
                                            ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        }
                                        (ctx "attributes-link-fallback-revision")
                                    |> Async.StartAsPromise
                            finally
                                restoreLink ()
                        }

                    Vitest.expect(linkAttempted).toBe true
                    Vitest.expect(exclusiveInstalled).toBe true
                    let! attributesAfter = tryReadUtf8FileAsync attributesPath
                    Vitest.expect(attributesAfter).toEqual (Some expectedAttributes)

                    let! worktreeNames = fsPromisesDynamic?readdir workPath |> unbox<JS.Promise<string[]>>
                    let attributesTemps =
                        worktreeNames
                        |> Array.filter (fun name -> name.StartsWith(".gitattributes.vcs-", StringComparison.Ordinal))

                    Vitest.expect(attributesTemps).toEqual [||]

                    match revisionResult with
                    | Succeeded outcome ->
                        Vitest.expect(outcome.AffectedPaths |> Array.contains ".gitattributes").toBe true
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected exclusive attributes creation to succeed."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision leaves its published exclusive attributes file when fallback fsync fails",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "e")

                    let mutable linkAttempted = false
                    let restoreLink =
                        injectAttributesLinkFailure attributesPath (fun () -> linkAttempted <- true)

                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let! statusResult = session.Core.GetStatus(ctx "attributes-fallback-fsync-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! restoreSync =
                        injectNextFileHandleSyncFailure (join [| workPath; "base.txt" |])

                    let! revisionResult =
                        promise {
                            try
                                return!
                                    session.Core.CreateRevision
                                        {
                                            Message = "test: clean exclusive attributes after fsync failure"
                                            Paths = [| repositoryPath "large.bin" |]
                                            ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        }
                                        (ctx "attributes-fallback-fsync-revision")
                                    |> Async.StartAsPromise
                            finally
                                restoreSync ()
                                restoreLink ()
                        }

                    Vitest.expect(linkAttempted).toBe true
                    let! attributesAfter = tryReadUtf8FileAsync attributesPath
                    Vitest.expect(attributesAfter).toEqual (
                        Some "\"/large.bin\" filter=lfs diff=lfs merge=lfs -text\n"
                    )

                    let! worktreeNames = fsPromisesDynamic?readdir workPath |> unbox<JS.Promise<string[]>>
                    let attributesTemps =
                        worktreeNames
                        |> Array.filter (fun name -> name.StartsWith(".gitattributes.vcs-", StringComparison.Ordinal))

                    Vitest.expect(attributesTemps).toEqual [||]

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Code).toBe "attributes_reconciliation_failed"
                        Vitest.expect(outcome.AffectedPaths |> Array.contains ".gitattributes").toBe true
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected fallback fsync failure to return partial success."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision preserves consumer bytes when exclusive attributes fsync fails",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    let consumerContent = "# consumer owns these published bytes\n"
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "u")

                    let mutable linkAttempted = false
                    let restoreLink =
                        injectAttributesLinkFailure attributesPath (fun () -> linkAttempted <- true)

                    let session =
                        GitWorkspaceSession.createSession GitWorkspaceSession.GitSessionHooks.none binding

                    let! statusResult = session.Core.GetStatus(ctx "attributes-consumer-fsync-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let mutable consumerEditInjected = false
                    let! restoreSync =
                        injectNextFileHandleSyncFailureAfterAttributesEdit
                            (join [| workPath; "base.txt" |])
                            attributesPath
                            consumerContent
                            (fun () -> consumerEditInjected <- true)

                    let! revisionResult =
                        promise {
                            try
                                return!
                                    session.Core.CreateRevision
                                        {
                                            Message = "test: preserve consumer attributes after fsync failure"
                                            Paths = [| repositoryPath "large.bin" |]
                                            ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        }
                                        (ctx "attributes-consumer-fsync-revision")
                                    |> Async.StartAsPromise
                            finally
                                restoreSync ()
                                restoreLink ()
                        }

                    Vitest.expect(linkAttempted).toBe true
                    Vitest.expect(consumerEditInjected).toBe true
                    let! attributesAfter = tryReadUtf8FileAsync attributesPath
                    Vitest.expect(attributesAfter).toEqual (Some consumerContent)

                    let! worktreeNames = fsPromisesDynamic?readdir workPath |> unbox<JS.Promise<string[]>>
                    let attributesTemps =
                        worktreeNames
                        |> Array.filter (fun name -> name.StartsWith(".gitattributes.vcs-", StringComparison.Ordinal))

                    Vitest.expect(attributesTemps).toEqual [||]

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
                    | _ -> failwith "Expected consumer edit after exclusive publication to return partial success."

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
            "selected revision does not replace a different attributes inode after preparation",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesBefore = "# baseline\n"
                    let replacementContent = "# replacement consumer file\n"
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    let originalPath = join [| workPath; ".gitattributes.original" |]
                    do! writeUtf8FileAsync attributesPath attributesBefore
                    let! _ = runGitOk workPath [| "add"; ".gitattributes" |]
                    let! _ = runGitOk workPath [| "commit"; "-m"; "test: baseline attributes inode" |]
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "i")

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess = None
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-attributes-replacement-ready" then
                                    do! renameAsync attributesPath originalPath |> Async.AwaitPromise
                                    do! writeUtf8FileAsync attributesPath replacementContent |> Async.AwaitPromise
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "attributes-inode-swap-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: preserve replacement attributes inode"
                                Paths = [| repositoryPath "large.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "attributes-inode-swap-revision")
                        |> Async.StartAsPromise

                    let! attributesAfter = tryReadUtf8FileAsync attributesPath
                    let! originalAfter = tryReadUtf8FileAsync originalPath
                    Vitest.expect(attributesAfter).toEqual (Some replacementContent)
                    Vitest.expect(originalAfter).toEqual (Some attributesBefore)

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Code).toBe "attributes_reconciliation_failed"
                        Vitest.expect(outcome.AffectedPaths |> Array.contains ".gitattributes").toBe true
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected the attributes inode swap to return partial success."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision does not replace an attributes path observed as a symlink after preparation",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesBefore = "# baseline\n"
                    let targetContent = "# consumer symlink target\n"
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    let originalPath = join [| workPath; ".gitattributes.original" |]
                    do! writeUtf8FileAsync attributesPath attributesBefore
                    let! _ = runGitOk workPath [| "add"; ".gitattributes" |]
                    let! _ = runGitOk workPath [| "commit"; "-m"; "test: baseline attributes symlink" |]
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "s")

                    let mutable symlinkSwapObserved = false
                    let restoreLstat =
                        injectAttributesSymlinkIdentity attributesPath (fun () -> symlinkSwapObserved)

                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess = None
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-attributes-replacement-ready" then
                                    do! renameAsync attributesPath originalPath |> Async.AwaitPromise
                                    do! writeUtf8FileAsync attributesPath targetContent |> Async.AwaitPromise
                                    symlinkSwapObserved <- true
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "attributes-symlink-swap-status") |> Async.StartAsPromise
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
                                            Message = "test: preserve attributes symlink"
                                            Paths = [| repositoryPath "large.bin" |]
                                            ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        }
                                        (ctx "attributes-symlink-swap-revision")
                                    |> Async.StartAsPromise
                            finally
                                restoreLstat ()
                        }

                    let! targetAfter = tryReadUtf8FileAsync attributesPath
                    let! originalAfter = tryReadUtf8FileAsync originalPath
                    Vitest.expect(symlinkSwapObserved).toBe true
                    Vitest.expect(targetAfter).toEqual (Some targetContent)
                    Vitest.expect(originalAfter).toEqual (Some attributesBefore)

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Code).toBe "attributes_reconciliation_failed"
                        Vitest.expect(outcome.AffectedPaths |> Array.contains ".gitattributes").toBe true
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected the attributes symlink swap to return partial success."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected revision preserves an attributes edit before append",
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
                        session.Core.CreateRevision
                            {
                                Message = "test: preserve pre-append attributes edit"
                                Paths = [| repositoryPath "large.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "final-attributes-race-revision")
                        |> Async.StartAsPromise

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
            "selected revision preserves newer attributes bytes after generated content is appended",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    let attributesBefore = "# baseline\n"
                    let generatedRule = "\"/large.bin\" filter=lfs diff=lfs merge=lfs -text\n"
                    let newestInstalledContent = "# newest installed path edit\n"
                    let attributesPath = join [| workPath; ".gitattributes" |]
                    do! writeUtf8FileAsync attributesPath attributesBefore
                    let! _ = runGitOk workPath [| "add"; ".gitattributes" |]
                    let! _ = runGitOk workPath [| "commit"; "-m"; "test: baseline installed attributes" |]
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "large.bin" |])
                            (String.replicate (1024 * 1024) "n")

                    let mutable installedBarrierRan = false
                    let mutable appendInstalled = false
                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        RunBytesProcess = None
                        RunProcess = None
                        Barrier =
                            Some(fun _ point _ -> async {
                                if point = "selected-revision-attributes-installed" then
                                    installedBarrierRan <- true
                                    let! installed = tryReadUtf8FileAsync attributesPath |> Async.AwaitPromise
                                    appendInstalled <- installed = Some(attributesBefore + generatedRule)
                                    do! writeUtf8FileAsync attributesPath newestInstalledContent |> Async.AwaitPromise
                            })
                    }

                    let session = GitWorkspaceSession.createSession hooks binding
                    let! statusResult = session.Core.GetStatus(ctx "installed-attributes-race-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: preserve newest installed attributes"
                                Paths = [| repositoryPath "large.bin" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "installed-attributes-race-revision")
                        |> Async.StartAsPromise

                    let! attributesAfter = tryReadUtf8FileAsync attributesPath
                    Vitest.expect(attributesAfter).toEqual (Some newestInstalledContent)
                    Vitest.expect(installedBarrierRan).toBe true
                    Vitest.expect(appendInstalled).toBe true

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Code).toBe "attributes_reconciliation_failed"
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_index")
                        Vitest.expect(outcome.AffectedPaths |> Array.contains "large.bin").toBe true
                        Vitest.expect(outcome.AffectedPaths |> Array.contains ".gitattributes").toBe true
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected the installed attributes race to return partial success."

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
                        Vitest.expect(failure.RecoveryAction |> Option.bind _.Instructions).toEqual (
                            Some "Reconcile the affected paths in the index (for example with git reset)."
                        )
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
            "selected revision without generated attributes gives index-only recovery",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, binding = createSelectedRevisionFixture ()

                try
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "changed\n"

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
                    let! statusResult = session.Core.GetStatus(ctx "index-only-recovery-status") |> Async.StartAsPromise
                    let status =
                        match statusResult with
                        | Succeeded outcome -> outcome.Value
                        | _ -> failwith "Expected selected-revision status."

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: index-only recovery"
                                Paths = [| repositoryPath "base.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "index-only-recovery-revision")
                        |> Async.StartAsPromise

                    match revisionResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Code).toBe "index_reconciliation_failed"
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "reconcile_index")
                        Vitest.expect(failure.RecoveryAction |> Option.bind _.Instructions).toEqual (
                            Some "Reconcile the affected paths in the index (for example with git reset)."
                        )
                        Vitest.expect(outcome.AffectedPaths).toEqual [| "base.txt" |]
                        Vitest.expect(outcome.ResultingRevision.IsSome).toBe true
                    | _ -> failwith "Expected index reconciliation to return partial success."

                    let! attributesAfter = tryReadUtf8FileAsync (join [| workPath; ".gitattributes" |])
                    Vitest.expect(attributesAfter).toEqual None
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

                        // Leave the pointer itself in the working tree. With the full file still
                        // there, a racy index refresh re-runs the clean filter and quietly recreates
                        // the object this test is about to delete.
                        do! writeUtf8FileAsync (join [| workPath; "missing-upload.bin" |]) pointer

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
            "rename publication reports both removed source and added destination paths",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session =
                    createPublishFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    let! _ = runGitOk workPath [| "config"; "diff.renames"; "true" |]
                    do!
                        renameAsync
                            (join [| workPath; "base.txt" |])
                            (join [| workPath; "renamed.txt" |])

                    let! status = sessionStatus session

                    let! revision =
                        session.Core.CreateRevision
                            {
                                Message = "test: publish rename"
                                Paths = [| repositoryPath "base.txt"; repositoryPath "renamed.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "publish-rename-revision")
                        |> Async.StartAsPromise

                    match revision with
                    | Succeeded _ -> ()
                    | _ -> failwith "Expected the rename revision to be created."

                    let! localHead = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! publishStatus = sessionStatus session

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-rename")
                        |> Async.StartAsPromise

                    match result with
                    | Succeeded outcome ->
                        Vitest.expect(outcome.Publication).toEqual Published
                        Vitest.expect(outcome.AffectedPaths).toEqual [| "base.txt"; "renamed.txt" |]
                        Vitest.expect(outcome.ResultingRevision |> Option.map RevisionId.value).toEqual (
                            Some(localHead.Trim())
                        )
                    | PartiallySucceeded(_, failure)
                    | Failed failure -> failwith $"Expected rename publication success, got {failure.Code}."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe(localHead.Trim())
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "push execution failure after remote advancement reports partial success",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable pushAdvancedRemote = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if request.Arguments |> Array.contains "push" then
                                    let! pushResult = NodeProcess.run request operationContext

                                    match pushResult with
                                    | Succeeded outcome when outcome.Value.ExitCode = 0 ->
                                        pushAdvancedRemote <- true

                                        return
                                            OperationResult.failed {
                                                OperationFailure.createRedacted
                                                    Network
                                                    "injected_push_response_loss"
                                                    "The push response was lost after https://secret@example.invalid accepted it." with
                                                    Retryable = true
                                                    Details = [| "authorization: secret" |] |> Array.map Redaction.redact
                                            }
                                    | _ -> return pushResult
                                else
                                    return! NodeProcess.run request operationContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks

                try
                    do! writeUtf8FileAsync (join [| workPath; "response-loss.txt" |]) "published\n"
                    let! status = sessionStatus session

                    let! revision =
                        session.Core.CreateRevision
                            {
                                Message = "test: publish before response loss"
                                Paths = [| repositoryPath "response-loss.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "push-response-loss-revision")
                        |> Async.StartAsPromise

                    match revision with
                    | Succeeded _ -> ()
                    | _ -> failwith "Expected the publication revision to be created."

                    let! previousTarget = runGitOk barePath [| "rev-parse"; "main" |]
                    let! localHead = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! publishStatus = sessionStatus session

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "push-response-loss")
                        |> Async.StartAsPromise

                    Vitest.expect(pushAdvancedRemote).toBe true

                    match result with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(outcome.Publication).toEqual Published
                        Vitest.expect(outcome.AffectedPaths).toEqual [| "response-loss.txt" |]
                        Vitest.expect(failure.AffectedPaths).toEqual [| "response-loss.txt" |]
                        Vitest.expect(outcome.ResultingRevision |> Option.map RevisionId.value).toEqual (
                            Some(localHead.Trim())
                        )
                        Vitest.expect(failure.Category).toEqual Network
                        Vitest.expect(failure.Code).toBe "injected_push_response_loss"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Retryable).toBe true
                        Vitest.expect(failure.Message.Contains "secret").toBe false
                        Vitest.expect(failure.Details |> Array.exists _.Contains("secret")).toBe false
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (
                            Some "retry_publish_verification"
                        )
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("previous_target", previousTarget.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("published_revision", localHead.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("observed_target", localHead.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                    | Failed failure ->
                        failwith $"Remote advancement was concealed as Failed ({failure.Code})."
                    | Succeeded _ -> failwith "Expected the lost push response to remain visible as a warning."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe(localHead.Trim())
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "new branch ambiguity reports every newly visible path",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if request.Arguments |> Array.contains "push" then
                                    let! pushResult = NodeProcess.run request operationContext

                                    match pushResult with
                                    | Succeeded outcome when outcome.Value.ExitCode = 0 ->
                                        return
                                            OperationResult.failed {
                                                OperationFailure.createRedacted
                                                    Network
                                                    "injected_new_branch_response_loss"
                                                    "The new branch response was lost after acceptance." with
                                                    Retryable = true
                                            }
                                    | _ -> return pushResult
                                else
                                    return! NodeProcess.run request operationContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks

                try
                    let! _ = runGitOk barePath [| "update-ref"; "-d"; "refs/heads/main" |]

                    let! localHead, publishStatus =
                        createTextPublishRevision session workPath "new-branch.txt" "new-branch-response-loss"

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "new-branch-response-loss")
                        |> Async.StartAsPromise

                    match result with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(outcome.Publication).toEqual Published
                        Vitest.expect(outcome.AffectedPaths).toEqual [| "base.txt"; "new-branch.txt" |]
                        Vitest.expect(failure.AffectedPaths).toEqual [| "base.txt"; "new-branch.txt" |]
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("published_revision", localHead |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("observed_target", localHead |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                    | _ -> failwith "Expected accepted new-branch ambiguity to report partial success."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "push nonzero exit after remote advancement reports partial success",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable pushAdvancedRemote = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if request.Arguments |> Array.contains "push" then
                                    let! pushResult = NodeProcess.run request operationContext

                                    match pushResult with
                                    | Succeeded outcome when outcome.Value.ExitCode = 0 ->
                                        pushAdvancedRemote <- true

                                        return
                                            OperationResult.succeeded {
                                                ExitCode = 1
                                                StdOut = ""
                                                StdErr = "transport closed after the remote accepted the ref"
                                            }
                                    | _ -> return pushResult
                                else
                                    return! NodeProcess.run request operationContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks

                try
                    do! writeUtf8FileAsync (join [| workPath; "nonzero-response.txt" |]) "published\n"
                    let! status = sessionStatus session

                    let! revision =
                        session.Core.CreateRevision
                            {
                                Message = "test: publish before nonzero response"
                                Paths = [| repositoryPath "nonzero-response.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "push-nonzero-revision")
                        |> Async.StartAsPromise

                    match revision with
                    | Succeeded _ -> ()
                    | _ -> failwith "Expected the publication revision to be created."

                    let! previousTarget = runGitOk barePath [| "rev-parse"; "main" |]
                    let! localHead = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! publishStatus = sessionStatus session

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "push-nonzero")
                        |> Async.StartAsPromise

                    Vitest.expect(pushAdvancedRemote).toBe true

                    match result with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(outcome.Publication).toEqual Published
                        Vitest.expect(outcome.AffectedPaths).toEqual [| "nonzero-response.txt" |]
                        Vitest.expect(failure.AffectedPaths).toEqual [| "nonzero-response.txt" |]
                        Vitest.expect(outcome.ResultingRevision |> Option.map RevisionId.value).toEqual (
                            Some(localHead.Trim())
                        )
                        Vitest.expect(failure.Category).toEqual Network
                        Vitest.expect(failure.Code).toBe "target_unreachable"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Retryable).toBe true
                        Vitest.expect(failure.Message.Contains "transport closed").toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (
                            Some "retry_publish_verification"
                        )
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("previous_target", previousTarget.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("published_revision", localHead.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("observed_target", localHead.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                    | Failed failure ->
                        failwith $"Remote advancement was concealed as Failed ({failure.Code})."
                    | Succeeded _ -> failwith "Expected the nonzero push response to remain visible as a warning."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe(localHead.Trim())
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "raced third revision reports changed-state concurrency evidence",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable bareTarget = ""
                let mutable armRace = false
                let mutable racedRevision = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if request.Arguments |> Array.contains "push" && armRace then
                                    armRace <- false
                                    let! parent = runGitOk bareTarget [| "rev-parse"; "main" |] |> Async.AwaitPromise
                                    let! tree = runGitOk bareTarget [| "rev-parse"; "main^{tree}" |] |> Async.AwaitPromise
                                    let! _ = runGitOk bareTarget [| "config"; "user.name"; "VCS Race Tests" |] |> Async.AwaitPromise
                                    let! _ = runGitOk bareTarget [| "config"; "user.email"; "race@example.org" |] |> Async.AwaitPromise
                                    let! concurrent =
                                        runGitOk
                                            bareTarget
                                            [| "commit-tree"; tree.Trim(); "-p"; parent.Trim(); "-m"; "race: third revision" |]
                                        |> Async.AwaitPromise

                                    racedRevision <- concurrent.Trim()
                                    let! _ =
                                        runGitOk
                                            bareTarget
                                            [| "update-ref"; "refs/heads/main"; racedRevision; parent.Trim() |]
                                        |> Async.AwaitPromise

                                    ()

                                return! NodeProcess.run request operationContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks
                bareTarget <- barePath

                try
                    let! previousTarget = runGitOk barePath [| "rev-parse"; "main" |]
                    let! _, publishStatus =
                        createTextPublishRevision session workPath "raced-third.txt" "raced-third-revision"

                    armRace <- true

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "raced-third-publish")
                        |> Async.StartAsPromise

                    match result with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Concurrency
                        Vitest.expect(failure.Code).toBe "precondition_failed"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Retryable).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("expected_target", previousTarget.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("observed_target", racedRevision |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                    | _ -> failwith "Expected a raced third revision to return changed-state concurrency."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe racedRevision
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "clean push whose exact ref remains unchanged fails without claiming mutation",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable pushReportedSuccess = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if request.Arguments |> Array.contains "push" then
                                    pushReportedSuccess <- true

                                    return
                                        OperationResult.succeeded {
                                            ExitCode = 0
                                            StdOut = ""
                                            StdErr = ""
                                        }
                                else
                                    return! NodeProcess.run request operationContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks

                try
                    let! previousTarget = runGitOk barePath [| "rev-parse"; "main" |]
                    let! _, publishStatus =
                        createTextPublishRevision
                            session
                            workPath
                            "clean-push-not-observed.txt"
                            "clean-push-not-observed"

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "clean-push-not-observed")
                        |> Async.StartAsPromise

                    Vitest.expect(pushReportedSuccess).toBe true

                    match result with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual ProviderError
                        Vitest.expect(failure.Code).toBe "publish_not_observed"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.Retryable).toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (
                            Some "retry_publish_verification"
                        )
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("expected_target", previousTarget.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("observed_target", previousTarget.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                    | PartiallySucceeded(outcome, _) ->
                        failwith $"An unchanged remote ref falsely reported {outcome.Publication}."
                    | Succeeded _ -> failwith "An unchanged remote ref cannot report a successful publication."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe(previousTarget.Trim())
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "clean push whose exact ref races to a third revision reports concurrency",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable bareTarget = ""
                let mutable pushReportedSuccess = false
                let mutable racedRevision = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                let! result = NodeProcess.run request operationContext

                                if
                                    request.Arguments |> Array.contains "push"
                                    && match result with
                                       | Succeeded outcome -> outcome.Value.ExitCode = 0
                                       | _ -> false
                                then
                                    pushReportedSuccess <- true
                                    let! parent = runGitOk bareTarget [| "rev-parse"; "main" |] |> Async.AwaitPromise
                                    let! tree = runGitOk bareTarget [| "rev-parse"; "main^{tree}" |] |> Async.AwaitPromise
                                    let! _ = runGitOk bareTarget [| "config"; "user.name"; "VCS Race Tests" |] |> Async.AwaitPromise
                                    let! _ = runGitOk bareTarget [| "config"; "user.email"; "race@example.org" |] |> Async.AwaitPromise
                                    let! concurrent =
                                        runGitOk
                                            bareTarget
                                            [| "commit-tree"; tree.Trim(); "-p"; parent.Trim(); "-m"; "race: after clean push" |]
                                        |> Async.AwaitPromise

                                    racedRevision <- concurrent.Trim()
                                    let! _ =
                                        runGitOk
                                            bareTarget
                                            [| "update-ref"; "refs/heads/main"; racedRevision; parent.Trim() |]
                                        |> Async.AwaitPromise

                                    ()

                                return result
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks
                bareTarget <- barePath

                try
                    let! previousTarget = runGitOk barePath [| "rev-parse"; "main" |]
                    let! _, publishStatus =
                        createTextPublishRevision
                            session
                            workPath
                            "clean-push-raced.txt"
                            "clean-push-raced"

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "clean-push-raced")
                        |> Async.StartAsPromise

                    Vitest.expect(pushReportedSuccess).toBe true

                    match result with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Concurrency
                        Vitest.expect(failure.Code).toBe "precondition_failed"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Retryable).toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (
                            Some "retry_publish_verification"
                        )
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("expected_target", previousTarget.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("observed_target", racedRevision |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                    | PartiallySucceeded(outcome, _) ->
                        failwith $"A raced remote ref falsely reported {outcome.Publication}."
                    | Succeeded _ -> failwith "A raced remote ref cannot report a successful publication."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe racedRevision
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "path evidence failure stops publication before the core push",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable failPathEvidence = false
                let mutable pathEvidenceAttempted = false
                let mutable corePushRan = false
                let mutable evidencePrevious = ""
                let mutable evidenceIntended = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if
                                    failPathEvidence
                                    && request.Arguments |> Array.contains "diff"
                                    && request.Arguments |> Array.contains "--name-only"
                                    && request.Arguments |> Array.contains evidencePrevious
                                    && request.Arguments |> Array.contains evidenceIntended
                                then
                                    pathEvidenceAttempted <- true

                                    return
                                        OperationResult.failed {
                                            OperationFailure.createRedacted
                                                ProviderError
                                                "injected_publish_evidence_failed"
                                                "Exact publication path evidence could not be read." with
                                                Retryable = true
                                        }
                                elif request.Arguments |> Array.contains "push" then
                                    corePushRan <- true
                                    let! result = NodeProcess.run request operationContext

                                    match result with
                                    | Succeeded outcome when outcome.Value.ExitCode = 0 ->
                                        return
                                            OperationResult.failed {
                                                OperationFailure.createRedacted
                                                    Network
                                                    "injected_push_response_loss"
                                                    "The response was lost after publication." with
                                                    Retryable = true
                                            }
                                    | _ -> return result
                                else
                                    return! NodeProcess.run request operationContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks

                try
                    let! previousTarget = runGitOk barePath [| "rev-parse"; "main" |]
                    let! localHead, publishStatus =
                        createTextPublishRevision
                            session
                            workPath
                            "unavailable-path-evidence.txt"
                            "unavailable-path-evidence"

                    evidencePrevious <- previousTarget.Trim()
                    evidenceIntended <- localHead.Trim()
                    failPathEvidence <- true

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "unavailable-path-evidence")
                        |> Async.StartAsPromise

                    Vitest.expect(pathEvidenceAttempted).toBe true
                    Vitest.expect(corePushRan).toBe false

                    match result with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual ProviderError
                        Vitest.expect(failure.Code).toBe "injected_publish_evidence_failed"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.Retryable).toBe true
                    | PartiallySucceeded(outcome, _) ->
                        failwith $"Missing path evidence was discovered only after {outcome.Publication}."
                    | Succeeded _ -> failwith "Missing path evidence cannot report success."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe(previousTarget.Trim())
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "successful push with inconclusive verification does not claim local-only state",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable pushCompleted = false
                let mutable verificationAttempted = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if
                                    pushCompleted
                                    && request.Arguments |> Array.contains "ls-remote"
                                then
                                    verificationAttempted <- true

                                    return
                                        OperationResult.failed {
                                            OperationFailure.createRedacted
                                                Network
                                                "injected_verification_unreachable"
                                                "Verification failed at https://secret@example.invalid." with
                                                Retryable = true
                                        }
                                else
                                    let! result = NodeProcess.run request operationContext

                                    if
                                        request.Arguments |> Array.contains "push"
                                        && match result with
                                           | Succeeded outcome -> outcome.Value.ExitCode = 0
                                           | _ -> false
                                    then
                                        pushCompleted <- true

                                    return result
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks

                try
                    let! previousTarget = runGitOk barePath [| "rev-parse"; "main" |]
                    let! localHead, publishStatus =
                        createTextPublishRevision
                            session
                            workPath
                            "inconclusive-verification.txt"
                            "inconclusive-verification"

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "inconclusive-verification-publish")
                        |> Async.StartAsPromise

                    Vitest.expect(pushCompleted).toBe true
                    Vitest.expect(verificationAttempted).toBe true

                    match result with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Network
                        Vitest.expect(failure.Code).toBe "injected_verification_unreachable"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Retryable).toBe true
                        Vitest.expect(failure.Message.Contains "secret").toBe false
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (
                            Some "retry_publish_verification"
                        )
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("expected_target", previousTarget.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.exists (fun (label, _) -> label = "published_revision")
                        ).toBe false
                    | PartiallySucceeded(outcome, _) ->
                        failwith $"Inconclusive verification falsely claimed {outcome.Publication}."
                    | Succeeded _ -> failwith "Inconclusive verification cannot report success."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe localHead
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "detached exact verification deadline cancels a blocked process and stays inconclusive",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let callerCancellation = OperationCancellation.Source()
                let mutable pushAdvancedRemote = false
                let mutable corePushCount = 0
                let mutable verificationStartedUncanceled = false
                let mutable verificationCancellationObserved = false
                let mutable verificationSafetyReleased = false
                let mutable signalFakeTimersEnabled: unit -> unit = ignore
                let mutable signalVerificationStarted: unit -> unit = ignore

                let fakeTimersEnabled =
                    JS.Constructors.Promise.Create(fun resolve _ ->
                        signalFakeTimersEnabled <- fun () -> resolve ())

                let verificationStarted =
                    JS.Constructors.Promise.Create(fun resolve _ ->
                        signalVerificationStarted <- fun () -> resolve ())

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if request.Arguments |> Array.contains "push" then
                                    corePushCount <- corePushCount + 1
                                    let! pushResult = NodeProcess.run request operationContext

                                    match pushResult with
                                    | Succeeded outcome when outcome.Value.ExitCode = 0 ->
                                        pushAdvancedRemote <- true
                                        callerCancellation.Cancel()
                                        vitestTimers?useFakeTimers ()
                                        signalFakeTimersEnabled ()

                                        return
                                            OperationResult.failed {
                                                OperationFailure.createRedacted
                                                    Network
                                                    "injected_push_response_loss"
                                                    "The response was lost after the remote accepted the ref." with
                                                    Retryable = true
                                            }
                                    | _ -> return pushResult
                                elif
                                    pushAdvancedRemote
                                    && request.Arguments |> Array.contains "ls-remote"
                                then
                                    verificationStartedUncanceled <-
                                        not (operationContext.Cancellation.IsCancellationRequested())

                                    signalVerificationStarted ()

                                    return!
                                        Async.FromContinuations(fun (succeed, _, _) ->
                                            let mutable completed = false

                                            operationContext.Cancellation.Register(fun () ->
                                                if not completed then
                                                    completed <- true
                                                    verificationCancellationObserved <- true

                                                    succeed (
                                                        OperationResult.canceled
                                                            "The detached verification process was canceled."
                                                    ))

                                            JS.setTimeout
                                                (fun () ->
                                                    if not completed then
                                                        completed <- true
                                                        verificationSafetyReleased <- true

                                                        succeed (
                                                            OperationResult.failed {
                                                                OperationFailure.createRedacted
                                                                    Network
                                                                    "injected_verification_safety_release"
                                                                    "The test released an unbounded verifier." with
                                                                    Retryable = true
                                                            }
                                                        ))
                                                60_000
                                            |> ignore)
                                else
                                    return! NodeProcess.run request operationContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks

                try
                    let! localHead, publishStatus =
                        createTextPublishRevision
                            session
                            workPath
                            "verification-deadline.txt"
                            "verification-deadline"

                    let! result =
                        promise {
                            try
                                let publishPromise =
                                    (synchronization session).Publish
                                        {
                                            ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                            ExpectedTargetRevision = None
                                        }
                                        (OperationContext.create
                                            "verification-deadline-publish"
                                            callerCancellation.Cancellation
                                            ignore)
                                    |> Async.StartAsPromise

                                do! fakeTimersEnabled

                                let! _ =
                                    vitestTimers?advanceTimersByTimeAsync (0)
                                    |> unbox<JS.Promise<obj>>

                                do! verificationStarted

                                let! _ =
                                    vitestTimers?advanceTimersByTimeAsync (60_000)
                                    |> unbox<JS.Promise<obj>>

                                return! publishPromise
                            finally
                                vitestTimers?useRealTimers ()
                        }

                    Vitest.expect(pushAdvancedRemote).toBe true
                    Vitest.expect(corePushCount).toBe 1
                    Vitest.expect(callerCancellation.IsCancellationRequested).toBe true
                    Vitest.expect(verificationStartedUncanceled).toBe true
                    Vitest.expect(verificationCancellationObserved).toBe true
                    Vitest.expect(verificationSafetyReleased).toBe false

                    match result with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Network
                        Vitest.expect(failure.Code).toBe "injected_push_response_loss"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Retryable).toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (
                            Some "retry_publish_verification"
                        )
                        Vitest.expect(
                            failure.Details
                            |> Array.exists _.Contains("publish_verification_timeout")
                        ).toBe true
                    | PartiallySucceeded _ ->
                        failwith "A timed-out verifier cannot claim that publication was observed."
                    | Succeeded _ -> failwith "A timed-out verifier cannot report success."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe localHead
                    do! removeDirectoryAsync root
                with error ->
                    vitestTimers?useRealTimers ()
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "push cancellation after remote advancement verifies with a noncanceled context",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let cancellation = OperationCancellation.Source()
                let mutable pushAdvancedRemote = false
                let mutable verificationUsedNonCanceledContext = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request operationContext ->
                            async {
                                if request.Arguments |> Array.contains "push" then
                                    let! pushResult = NodeProcess.run request operationContext

                                    match pushResult with
                                    | Succeeded outcome when outcome.Value.ExitCode = 0 ->
                                        pushAdvancedRemote <- true
                                        cancellation.Cancel()

                                        return
                                            OperationResult.canceled
                                                "The caller canceled after the remote accepted the ref."
                                    | _ -> return pushResult
                                elif
                                    pushAdvancedRemote
                                    && request.Arguments |> Array.contains "ls-remote"
                                then
                                    verificationUsedNonCanceledContext <-
                                        not (operationContext.Cancellation.IsCancellationRequested())

                                    return! NodeProcess.run request operationContext
                                else
                                    return! NodeProcess.run request operationContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createPublishFixture hooks

                try
                    do! writeUtf8FileAsync (join [| workPath; "canceled-response.txt" |]) "published\n"
                    let! status = sessionStatus session

                    let! revision =
                        session.Core.CreateRevision
                            {
                                Message = "test: publish before caller cancellation"
                                Paths = [| repositoryPath "canceled-response.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "push-canceled-revision")
                        |> Async.StartAsPromise

                    match revision with
                    | Succeeded _ -> ()
                    | _ -> failwith "Expected the publication revision to be created."

                    let! previousTarget = runGitOk barePath [| "rev-parse"; "main" |]
                    let! localHead = runGitOk workPath [| "rev-parse"; "HEAD" |]
                    let! publishStatus = sessionStatus session
                    let publishContext = OperationContext.create "push-canceled" cancellation.Cancellation ignore

                    let! result =
                        (synchronization session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            publishContext
                        |> Async.StartAsPromise

                    Vitest.expect(pushAdvancedRemote).toBe true
                    Vitest.expect(cancellation.IsCancellationRequested).toBe true
                    Vitest.expect(verificationUsedNonCanceledContext).toBe true

                    match result with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(outcome.Publication).toEqual Published
                        Vitest.expect(outcome.AffectedPaths).toEqual [| "canceled-response.txt" |]
                        Vitest.expect(failure.AffectedPaths).toEqual [| "canceled-response.txt" |]
                        Vitest.expect(outcome.ResultingRevision |> Option.map RevisionId.value).toEqual (
                            Some(localHead.Trim())
                        )
                        Vitest.expect(failure.Category).toEqual Canceled
                        Vitest.expect(failure.Code).toBe "operation_canceled"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Message.Contains "caller canceled").toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (
                            Some "retry_publish_verification"
                        )
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("previous_target", previousTarget.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("published_revision", localHead.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("observed_target", localHead.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                    | Failed failure ->
                        failwith $"Remote advancement was concealed as Failed ({failure.Code})."
                    | Succeeded _ -> failwith "Expected cancellation to remain visible after publication."

                    let! targetHead = runGitOk barePath [| "rev-parse"; "main" |]
                    Vitest.expect(targetHead.Trim()).toBe(localHead.Trim())
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
                    let! previousTarget = runGitOk barePath [| "rev-parse"; "main" |]
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
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(outcome.Publication).toEqual Published
                        Vitest.expect(outcome.AffectedPaths).toEqual [| "published.txt" |]
                        Vitest.expect(failure.AffectedPaths).toEqual [| "published.txt" |]
                        Vitest.expect(outcome.ResultingRevision |> Option.map RevisionId.value).toEqual (
                            Some(localHead.Trim())
                        )
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual(
                            Some "retry_publish_verification"
                        )
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("previous_target", previousTarget.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("published_revision", localHead.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
                        Vitest.expect(
                            failure.RevisionEvidence
                            |> Array.contains ("observed_target", localHead.Trim() |> RevisionId.tryCreate |> Result.defaultWith failwith)
                        ).toBe true
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
