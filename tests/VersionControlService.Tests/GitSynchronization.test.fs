module VersionControlService.Tests.GitSynchronizationTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Tests.NodePath
open Vitest

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module NodeProcess = VersionControlService.Runtime.Node.Process
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"
let private cryptoDynamic: obj = importAll "node:crypto"

let private createTempDirectoryAsync () : JS.Promise<string> = promise {
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-git-sync-" |]
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

let private ensureDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?mkdir (path, createObj [ "recursive" ==> true ]) |> unbox<JS.Promise<obj>>
    return ()
}

let private removePathAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private createDirectoryLinkAsync (targetPath: string) (linkPath: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?symlink (targetPath, linkPath, "junction") |> unbox<JS.Promise<obj>>
    return ()
}

let private writeUtf8FileAsync (path: string) (content: string) : JS.Promise<unit> = promise {
    do! ensureDirectoryAsync (dirname path)
    let! _ = fsPromisesDynamic?writeFile (path, content, "utf8") |> unbox<JS.Promise<obj>>
    return ()
}

[<Emit("Buffer.from($0)")>]
let private bufferFromBytes (_bytes: int[]) : obj = jsNative

[<Emit("$0.equals($1)")>]
let private buffersEqual (_left: obj) (_right: obj) : bool = jsNative

let private sha256Hex (bytes: int[]) =
    let hash: obj = cryptoDynamic?createHash "sha256"
    hash?update (bufferFromBytes bytes) |> ignore
    hash?digest "hex" |> unbox<string>

let private writeBinaryFileAsync (path: string) (bytes: int[]) : JS.Promise<unit> = promise {
    do! ensureDirectoryAsync (dirname path)
    let! _ = fsPromisesDynamic?writeFile (path, bufferFromBytes bytes) |> unbox<JS.Promise<obj>>
    return ()
}

let private tryReadUtf8FileAsync (path: string) : JS.Promise<string option> = promise {
    try
        let! content = fsPromisesDynamic?readFile (path, "utf8") |> unbox<JS.Promise<string>>
        return Some content
    with _ ->
        return None
}

let private pathExistsAsync (path: string) : JS.Promise<bool> = promise {
    try
        let! _ = fsPromisesDynamic?stat path |> unbox<JS.Promise<obj>>
        return true
    with _ ->
        return false
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

let private runGitResultIn (cwd: string) (arguments: string[]) : JS.Promise<NodeProcess.ProcessOutput> = promise {
    let request = {
        NodeProcess.ProcessRequest.create "git" arguments with
            WorkingDirectory = Some cwd
    }

    let! result = Async.StartAsPromise(NodeProcess.run request (OperationContext.detached "git-sync-fixture"))

    match result with
    | Succeeded outcome -> return outcome.Value
    | PartiallySucceeded(_, failure)
    | Failed failure -> return failwith $"fixture git invocation failed: {failure.Code}"
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

let private expectProviderFailure (operationName: string) (result: OperationResult<'T>) : OperationFailure =
    match result with
    | Failed failure -> failure
    | Succeeded _
    | PartiallySucceeded _ -> failwith $"Expected {operationName} to fail."

let private expectOperationInProgress (operationName: string) (result: OperationResult<'T>) =
    let failure = expectProviderFailure operationName result
    Vitest.expect(failure.Category).toEqual (Conflict)
    Vitest.expect(failure.Code).toBe ("operation_in_progress")
    Vitest.expect(failure.StateChanged).toBe (false)
    Vitest.expect(failure.RecoveryAction).toEqual (None)
    failure

let private expectActiveConflictSession (operationName: string) (result: OperationResult<'T>) =
    let failure = expectProviderFailure operationName result
    Vitest.expect(failure.Category).toEqual (Conflict)
    Vitest.expect(failure.Code).toBe ("conflict_session_active")
    Vitest.expect(failure.StateChanged).toBe (false)
    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "resolve_conflict_session")
    Vitest.expect(failure.Message).toBe ("A conflict session is active. Resolve or cancel it first.")
    failure

let private expectSucceeded operationName result =
    expectValue operationName result

let private expectSucceededOutcome (operationName: string) (result: OperationResult<'T>) : OperationOutcome<'T> =
    match result with
    | Succeeded outcome -> outcome
    | PartiallySucceeded(_, failure) ->
        failwith $"{operationName} unexpectedly returned partial success ({failure.Code})."
    | Failed failure -> failwith $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

let private toFileRemoteUrl (path: string) =
    let normalized = path.Replace("\\", "/")

    if normalized.StartsWith("/", StringComparison.Ordinal) then
        $"file://{normalized}"
    else
        $"file:///{normalized}"

let private isConflictedChange (change: FileChange) =
    change.Kind = ConflictedChange

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

let private syncBinding (workPath: string) (barePath: string) : WorkspaceBinding = {
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

let private identitySession
    (hooks: GitWorkspaceSession.GitSessionHooks)
    (identity: GitCredentialStrategy.GitIdentityStrategy)
    (workPath: string)
    (barePath: string)
    =
    GitWorkspaceSession.createSessionWithCredentialsAndIdentity
        hooks
        GitCredentialStrategy.anonymous
        identity
        (syncBinding workPath barePath)

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

let private advanceTargetWithBinary (root: string) (barePath: string) (path: string) (bytes: int[]) = promise {
    let clonePath = join [| root; $"advance-binary-{DateTime.Now.Ticks}" |]

    let! _ =
        runGitIn root [| "clone"; "-c"; "core.autocrlf=false"; barePath; clonePath |]

    let! _ = runGitIn clonePath [| "config"; "user.name"; "External Client" |]
    let! _ = runGitIn clonePath [| "config"; "user.email"; "external@example.org" |]
    do! writeBinaryFileAsync (join [| clonePath; path |]) bytes

    let! _ = runGitIn clonePath [| "add"; "-A" |]
    let! _ = runGitIn clonePath [| "commit"; "-m"; "external: advance binary" |]
    let! _ = runGitIn clonePath [| "push"; "origin"; "HEAD" |]
    return ()
}

let private createCherryPickConflictFixture () = promise {
    let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
    let! _ = runGitIn workPath [| "checkout"; "-b"; "cherry-source" |]
    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "cherry-pick source\n"
    let! _ = runGitIn workPath [| "add"; "base.txt" |]
    let! _ = runGitIn workPath [| "commit"; "-m"; "cherry-pick source" |]
    let! sourceRevision = runGitIn workPath [| "rev-parse"; "HEAD" |]
    let! _ = runGitIn workPath [| "checkout"; "main" |]
    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "main conflict\n"
    let! _ = runGitIn workPath [| "add"; "base.txt" |]
    let! _ = runGitIn workPath [| "commit"; "-m"; "main conflict" |]

    let! cherryPick = runGitResultIn workPath [| "cherry-pick"; sourceRevision.Trim() |]

    if cherryPick.ExitCode = 0 then
        return failwith "Expected the cherry-pick fixture to stop with a conflict."
    else
        return root, workPath, barePath, session
}

let private createRebaseConflictFixture () = promise {
    let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
    let! _ = runGitIn workPath [| "checkout"; "-b"; "rebase-source" |]
    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "rebase source\n"
    let! _ = runGitIn workPath [| "add"; "base.txt" |]
    let! _ = runGitIn workPath [| "commit"; "-m"; "rebase source" |]
    let! _ = runGitIn workPath [| "checkout"; "main" |]
    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "rebase main\n"
    let! _ = runGitIn workPath [| "add"; "base.txt" |]
    let! _ = runGitIn workPath [| "commit"; "-m"; "rebase main" |]

    let! rebase = runGitResultIn workPath [| "rebase"; "rebase-source" |]

    if rebase.ExitCode = 0 then
        return failwith "Expected the rebase fixture to stop with a conflict."
    else
        return root, workPath, barePath, session
}

let private syncService (session: WorkspaceSession) =
    match session.Synchronization with
    | Some service -> service
    | None -> failwith "Expected the Git synchronization service."

/// A process runner that fakes `git --version` and delegates everything else.
let private versionFakingRunner (fakeVersion: string) : GitWorkspaceSession.GitSessionHooks = {
    RunBytesProcess = None
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

let private conflictService (session: WorkspaceSession) =
    match session.ConflictResolution with
    | Some service -> service
    | None -> failwith "Expected the Git conflict-resolution service."

let private sessionStatus (session: WorkspaceSession) = promise {
    let! result = Async.StartAsPromise(session.Core.GetStatus(ctx "sync-status"))
    return expectValue "status" result
}

let private lfsMediaDirectory (environmentOutput: string) =
    environmentOutput.Split('\n')
    |> Array.tryPick (fun line ->
        let prefix = "LocalMediaDir="
        let trimmed = line.TrimEnd '\r'
        if trimmed.StartsWith(prefix, StringComparison.Ordinal) then Some(trimmed.Substring(prefix.Length).Trim()) else None)
    |> Option.defaultWith (fun () -> failwith "Expected LocalMediaDir in git lfs env output.")

let private createLfsConflictFixtureWithOptions
    (hooks: GitWorkspaceSession.GitSessionHooks)
    (path: string)
    (mediaDirectory: string option)
    (extraFiles: (string * int[])[])
    =
    promise {
        let baseBytes = [| 0; 1; 2; 3; 4 |]
        let workspaceBytes = [| 0; 159; 146; 150; 255 |]
        let targetBytes = [| 0; 255; 1; 2; 3 |]
        let! root, workPath, barePath, session = createSyncFixture hooks

        try
            let! _ = runGitIn workPath [| "lfs"; "install"; "--local" |]
            match mediaDirectory with
            | Some directory ->
                let! _ = runGitIn workPath [| "config"; "lfs.storage"; directory |]
                ()
            | None -> ()

            let extension = path.Substring(path.LastIndexOf ".")
            let! _ = runGitIn workPath [| "lfs"; "track"; $"*{extension}" |]
            do! writeBinaryFileAsync (join [| workPath; path |]) baseBytes

            for extraPath, bytes in extraFiles do
                do! writeBinaryFileAsync (join [| workPath; extraPath |]) bytes

            let! _ = runGitIn workPath [| "--literal-pathspecs"; "add"; "--"; ".gitattributes"; path |]

            for extraPath, _ in extraFiles do
                let! _ = runGitIn workPath [| "add"; extraPath |]
                ()

            let! _ = runGitIn workPath [| "commit"; "-m"; "init: LFS conflict path" |]
            let! _ = runGitIn workPath [| "push"; "origin"; "main" |]

            do! writeBinaryFileAsync (join [| workPath; path |]) workspaceBytes
            let! workspaceStatus = sessionStatus session
            let! saveResult =
                Async.StartAsPromise(
                    session.Core.CreateRevision
                        {
                            Message = "local LFS binary conflict"
                            Paths = [| mkPath path |]
                            ExpectedWorkspaceVersion = workspaceStatus.WorkspaceVersion
                        }
                        (ctx "lfs-binary-conflict-save")
                )

            expectValue "local LFS binary conflict revision" saveResult |> ignore

            let clonePath = join [| root; "advance-lfs-binary" |]
            let! _ = runGitIn root [| "clone"; "-c"; "core.autocrlf=false"; barePath; clonePath |]
            let! _ = runGitIn clonePath [| "config"; "user.name"; "External Client" |]
            let! _ = runGitIn clonePath [| "config"; "user.email"; "external@example.org" |]
            let! _ = runGitIn clonePath [| "lfs"; "install"; "--local" |]
            do! writeBinaryFileAsync (join [| clonePath; path |]) targetBytes
            let! _ = runGitIn clonePath [| "add"; "-A" |]
            let! _ = runGitIn clonePath [| "commit"; "-m"; "external: advance LFS binary" |]
            let! _ = runGitIn clonePath [| "push"; "origin"; "HEAD" |]

            let! updateStatus = sessionStatus session
            let! updateResult =
                Async.StartAsPromise(
                    (syncService session).Update
                        { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                        (ctx "lfs-binary-conflict-update")
                )

            match updateResult with
            | Failed failure
            | PartiallySucceeded(_, failure) when failure.Code = "conflicts_detected" -> ()
            | Failed failure
            | PartiallySucceeded(_, failure) ->
                failwith $"Expected conflicts_detected, received {failure.Category}/{failure.Code}."
            | Succeeded _ -> failwith "Expected the LFS binary changes to conflict."

            return root, workPath, path, baseBytes, workspaceBytes, targetBytes, session
        with error ->
            do! removeDirectoryAsync root
            return raise error
    }

let private createLfsConflictFixture () =
    createLfsConflictFixtureWithOptions GitWorkspaceSession.GitSessionHooks.none "tracked.xlsx" None [||]

let private createModifyDeleteConflictFixture
    (hooks: GitWorkspaceSession.GitSessionHooks)
    (path: string)
    (initialBytes: int[])
    (workspaceBytes: int[] option)
    (targetBytes: int[] option)
    (useLfs: bool)
    =
    promise {
        let! root, workPath, barePath, session = createSyncFixture hooks

        try
            if useLfs then
                let! _ = runGitIn workPath [| "lfs"; "install"; "--local" |]
                let extension = path.Substring(path.LastIndexOf ".")
                let! _ = runGitIn workPath [| "lfs"; "track"; $"*{extension}" |]
                do! writeBinaryFileAsync (join [| workPath; path |]) initialBytes
                let! _ = runGitIn workPath [| "add"; ".gitattributes"; path |]
                let! _ = runGitIn workPath [| "commit"; "-m"; "init: LFS modify-delete path" |]
                ()
            else
                do! writeBinaryFileAsync (join [| workPath; path |]) initialBytes
                let! _ = runGitIn workPath [| "add"; path |]
                let! _ = runGitIn workPath [| "commit"; "-m"; "init: modify-delete path" |]
                ()

            let! _ = runGitIn workPath [| "push"; "origin"; "main" |]

            match workspaceBytes with
            | Some bytes -> do! writeBinaryFileAsync (join [| workPath; path |]) bytes
            | None -> do! removePathAsync (join [| workPath; path |])

            let! workspaceStatus = sessionStatus session
            let! saveResult =
                Async.StartAsPromise(
                    session.Core.CreateRevision
                        {
                            Message = "local modify-delete change"
                            Paths = [| mkPath path |]
                            ExpectedWorkspaceVersion = workspaceStatus.WorkspaceVersion
                        }
                        (ctx "modify-delete-save")
                )

            expectValue "local modify-delete revision" saveResult |> ignore

            let clonePath = join [| root; "advance-modify-delete" |]
            let! _ = runGitIn root [| "clone"; "-c"; "core.autocrlf=false"; barePath; clonePath |]
            let! _ = runGitIn clonePath [| "config"; "user.name"; "External Client" |]
            let! _ = runGitIn clonePath [| "config"; "user.email"; "external@example.org" |]

            match targetBytes with
            | Some bytes -> do! writeBinaryFileAsync (join [| clonePath; path |]) bytes
            | None ->
                let! _ = runGitIn clonePath [| "rm"; path |]
                ()

            let! _ = runGitIn clonePath [| "add"; "-A" |]
            let! _ = runGitIn clonePath [| "commit"; "-m"; "external: modify-delete change" |]
            let! _ = runGitIn clonePath [| "push"; "origin"; "HEAD" |]

            let! updateStatus = sessionStatus session
            let! updateResult =
                Async.StartAsPromise(
                    (syncService session).Update
                        { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                        (ctx "modify-delete-update")
                )

            match updateResult with
            | Failed failure
            | PartiallySucceeded(_, failure) when failure.Code = "conflicts_detected" -> ()
            | Failed failure
            | PartiallySucceeded(_, failure) ->
                failwith $"Expected conflicts_detected, received {failure.Category}/{failure.Code}."
            | Succeeded _ -> failwith "Expected the modify-delete changes to conflict."

            return root, workPath, barePath, session
        with error ->
            do! removeDirectoryAsync root
            return raise error
    }

let private createMergeHeadConflictFixture () = promise {
    let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
    do! advanceTarget root barePath [ "base.txt", "target merge conflict\n" ]
    let! targetHash = runGitIn barePath [| "rev-parse"; "main" |]
    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "workspace merge conflict\n"

    let! saveStatus = sessionStatus session

    let! saveResult =
        session.Core.CreateRevision
            {
                Message = "local merge conflict"
                Paths = [| mkPath "base.txt" |]
                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
            }
            (ctx "merge-head-conflict-save")
        |> Async.StartAsPromise

    expectValue "merge-head conflict local revision" saveResult |> ignore

    let! mergeHeadPath = runGitIn workPath [| "rev-parse"; "--git-path"; "MERGE_HEAD" |]
    do! writeUtf8FileAsync (join [| workPath; mergeHeadPath.Trim() |]) (targetHash.Trim() + "\n")
    return root, workPath, barePath, session
}

let private createUnmergedConflictFixtureWithHooks (hooks: GitWorkspaceSession.GitSessionHooks) = promise {
    let! root, workPath, barePath, session = createSyncFixture hooks
    do! advanceTarget root barePath [ "base.txt", "target version\n" ]
    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "workspace version\n"

    let! saveStatus = sessionStatus session

    let! saveResult =
        Async.StartAsPromise(
            session.Core.CreateRevision
                {
                    Message = "local conflicting change"
                    Paths = [| mkPath "base.txt" |]
                    ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                }
                (ctx "preview-conflict-save")
        )

    expectValue "local conflict revision" saveResult |> ignore
    let! updateStatus = sessionStatus session

    let! updateResult =
        Async.StartAsPromise(
            (syncService session).Update
                { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                (ctx "preview-conflict-update")
        )

    match updateResult with
    | Failed failure
    | PartiallySucceeded(_, failure) when failure.Code = "conflicts_detected" -> ()
    | Failed failure
    | PartiallySucceeded(_, failure) ->
        failwith $"Expected conflicts_detected, received {failure.Category}/{failure.Code}."
    | Succeeded _ -> failwith "Expected the update to create a real unmerged conflict."

    let conflicts = conflictService session
    let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "preview-conflict-session"))

    let summary =
        match expectValue "preview conflict session" summaryResult with
        | Some value -> value
        | None -> failwith "Expected an active conflict session."

    let! capturedStatus = sessionStatus session
    return root, workPath, session, conflicts, summary, capturedStatus.WorkspaceVersion
}

let private createUnmergedConflictFixture () =
    createUnmergedConflictFixtureWithHooks GitWorkspaceSession.GitSessionHooks.none

let private createPathUnmergedConflictFixtureWithHooks
    (hooks: GitWorkspaceSession.GitSessionHooks)
    (conflictPath: string)
    = promise {
    let! root, workPath, barePath, session = createSyncFixture hooks
    do! writeUtf8FileAsync (join [| workPath; conflictPath |]) "nested base content\n"
    let! _ = runGitIn workPath [| "add"; "-A" |]
    let! _ = runGitIn workPath [| "commit"; "-m"; "test: nested conflict base" |]
    let! _ = runGitIn workPath [| "push"; "origin"; "HEAD" |]
    do! advanceTarget root barePath [ conflictPath, "nested target version\n" ]
    do! writeUtf8FileAsync (join [| workPath; conflictPath |]) "nested workspace version\n"
    let! saveStatus = sessionStatus session

    let! saveResult =
        Async.StartAsPromise(
            session.Core.CreateRevision
                {
                    Message = "local nested conflicting change"
                    Paths = [| mkPath conflictPath |]
                    ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                }
                (ctx "nested-conflict-save")
        )

    expectValue "local nested conflict revision" saveResult |> ignore
    let! updateStatus = sessionStatus session

    let! updateResult =
        Async.StartAsPromise(
            (syncService session).Update
                { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                (ctx "nested-conflict-update")
        )

    match updateResult with
    | Failed failure
    | PartiallySucceeded(_, failure) when failure.Code = "conflicts_detected" -> ()
    | Failed failure
    | PartiallySucceeded(_, failure) ->
        failwith $"Expected nested conflicts_detected, received {failure.Category}/{failure.Code}."
    | Succeeded _ -> failwith "Expected the nested update to create a real unmerged conflict."

    return root, workPath, session, conflictService session, conflictPath
}

let private createNestedUnmergedConflictFixture () =
    createPathUnmergedConflictFixtureWithHooks GitWorkspaceSession.GitSessionHooks.none "nested/conflict.txt"

let private createNestedUnmergedConflictFixtureWithHooks hooks =
    createPathUnmergedConflictFixtureWithHooks hooks "nested/conflict.txt"

let private createRenameRenameConflictFixture () = promise {
    let! root, workPath, _, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
    let! _ = runGitIn workPath [| "checkout"; "-b"; "rename-left" |]
    let! _ = runGitIn workPath [| "mv"; "base.txt"; "left.txt" |]
    let! _ = runGitIn workPath [| "commit"; "-m"; "rename: left" |]
    let! _ = runGitIn workPath [| "checkout"; "main" |]
    let! _ = runGitIn workPath [| "checkout"; "-b"; "rename-right" |]
    let! _ = runGitIn workPath [| "mv"; "base.txt"; "right.txt" |]
    let! _ = runGitIn workPath [| "commit"; "-m"; "rename: right" |]

    let mutable mergeSucceeded = false

    try
        let! _ = runGitIn workPath [| "merge"; "rename-left" |]
        mergeSucceeded <- true
    with _ ->
        ()

    if mergeSucceeded then
        return failwith "Expected the rename/rename merge to conflict."

    let! porcelain = runGitIn workPath [| "status"; "--porcelain=v2" |]

    let renameConflictPaths =
        porcelain.Split '\n'
        |> Array.choose (fun line ->
            let parts = (line.TrimEnd '\r').Split([| ' ' |], 11)

            if parts.Length = 11 && parts[0] = "u" && parts[1] = "DD" then
                Some parts[10]
            else
                None)

    if renameConflictPaths.Length = 0 then
        return failwith $"Expected porcelain DD entries, received: {porcelain}"

    for conflictPath in renameConflictPaths do
        let! exists = pathExistsAsync (join [| workPath; conflictPath |])

        if exists then
            return failwith $"Expected the rename/rename conflict path '{conflictPath}' to be absent from the worktree."

    return root, workPath, session
}

Vitest.describe (
    "Git conflict preview tracks out-of-band edits",
    fun () ->
        Vitest.test (
            "returns the current edited marker text as the combined preview",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, _, conflicts, _, _ = createUnmergedConflictFixture ()

                try
                    let editedMarkerText =
                        "<<<<<<< edited workspace\nmanually edited marker content\n=======\ntarget version\n>>>>>>> edited target\n"

                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) editedMarkerText
                    let! refreshedResult =
                        Async.StartAsPromise(conflicts.GetActiveSession(ctx "edited-conflict-preview"))

                    let refreshed =
                        match expectValue "edited conflict preview" refreshedResult with
                        | Some value -> value
                        | None -> failwith "Expected the edited conflict session to remain active."

                    Vitest.expect(refreshed.Items.Length).toBe (1)
                    Vitest.expect(refreshed.Items[0].CombinedPreview).toEqual (Some(TextPreview editedMarkerText))

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "rejects the captured handle and workspace token after an unstaged marker edit",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, _, conflicts, capturedSummary, capturedWorkspaceVersion =
                    createUnmergedConflictFixture ()

                try
                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "base.txt" |])
                            "<<<<<<< edited workspace\nout-of-band edit\n=======\ntarget version\n>>>>>>> edited target\n"

                    let! staleResolve =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = capturedSummary.Handle
                                    ExpectedWorkspaceVersion = capturedWorkspaceVersion
                                    Path = mkPath "base.txt"
                                    Resolution = PickCandidate "target"
                                }
                                (ctx "out-of-band-stale-resolve")
                        )

                    let failure =
                        match staleResolve with
                        | Failed failure -> failure
                        | PartiallySucceeded(_, failure) -> failure
                        | Succeeded _ -> failwith "Expected the out-of-band edit to stale the captured request."

                    Vitest.expect(failure.Category).toEqual (Concurrency)
                    Vitest.expect(failure.Code).toBe ("precondition_failed")

                    Vitest
                        .expect(failure.RecoveryAction |> Option.map _.Code)
                        .toEqual (Some "refresh_conflict_session")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "Git / synchronization revision identity",
    fun () ->
        Vitest.test (
            "up-to-date update without revision identity remains a NoOp",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, _ = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    let emptyGlobal = join [| root; "empty-global.gitconfig" |]
                    let emptySystem = join [| root; "empty-system.gitconfig" |]
                    do! writeUtf8FileAsync emptyGlobal ""
                    do! writeUtf8FileAsync emptySystem ""
                    let! _ = runGitIn workPath [| "config"; "--local"; "--unset-all"; "user.name" |]
                    let! _ = runGitIn workPath [| "config"; "--local"; "--unset-all"; "user.email" |]

                    let environment = [| "GIT_CONFIG_GLOBAL", emptyGlobal; "GIT_CONFIG_SYSTEM", emptySystem |]
                    let hooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request processContext ->
                                    NodeProcess.run
                                        { request with
                                            Environment = Array.append environment request.Environment }
                                        processContext)
                    }

                    let session = GitWorkspaceSession.createSession hooks (syncBinding workPath barePath)
                    let! status = sessionStatus session
                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "revision-identity-noop-update")
                        |> Async.StartAsPromise

                    match updateResult with
                    | Succeeded outcome ->
                        match outcome.Effect with
                        | NoOp _ -> ()
                        | Performed -> failwith "Expected the up-to-date update to be a NoOp."
                    | PartiallySucceeded(_, failure)
                    | Failed failure ->
                        failwith $"Expected a NoOp, received {failure.Category}/{failure.Code}."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "update merge commits use the injected revision identity",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let identity: GitCredentialStrategy.GitIdentityStrategy = {
                    ResolveIdentity = fun _request ->
                        async {
                            return Some { Name = "Merge Author"; Email = "merge@example.org" }
                        }
                }

                let! root, workPath, barePath, _ = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
                let session = identitySession GitWorkspaceSession.GitSessionHooks.none identity workPath barePath

                try
                    do! advanceTarget root barePath [ "target-only.txt", "target content\n" ]
                    do! writeUtf8FileAsync (join [| workPath; "workspace-only.txt" |]) "workspace content\n"

                    let! saveStatus = sessionStatus session
                    let! saveResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: local merge side"
                                Paths = [| mkPath "workspace-only.txt" |]
                                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                            }
                            (ctx "revision-identity-merge-local")
                        |> Async.StartAsPromise

                    expectValue "local merge side" saveResult |> ignore
                    let! updateStatus = sessionStatus session
                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                            (ctx "revision-identity-merge-update")
                        |> Async.StartAsPromise

                    expectValue "revision identity merge update" updateResult |> ignore

                    let! commitIdentity = runGitIn workPath [| "log"; "-1"; "--format=%an;%ae;%cn;%ce" |]
                    Vitest.expect(commitIdentity.Trim()).toBe "Merge Author;merge@example.org;Merge Author;merge@example.org"
                    let! commitSubject = runGitIn workPath [| "log"; "-1"; "--format=%s" |]
                    Vitest.expect(commitSubject.Trim()).toBe "Merge online changes"
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "conflict finalize commits use the injected revision identity",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let identity: GitCredentialStrategy.GitIdentityStrategy = {
                    ResolveIdentity = fun _request ->
                        async {
                            return Some { Name = "Finalize Author"; Email = "finalize@example.org" }
                        }
                }

                let! root, workPath, barePath, _ = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
                let session = identitySession GitWorkspaceSession.GitSessionHooks.none identity workPath barePath

                try
                    do! advanceTarget root barePath [ "base.txt", "target version\n" ]
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "workspace version\n"

                    let! saveStatus = sessionStatus session
                    let! saveResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: local conflicting change"
                                Paths = [| mkPath "base.txt" |]
                                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                            }
                            (ctx "revision-identity-finalize-local")
                        |> Async.StartAsPromise

                    expectValue "local conflicting change" saveResult |> ignore
                    let! updateStatus = sessionStatus session
                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                            (ctx "revision-identity-finalize-update")
                        |> Async.StartAsPromise

                    match updateResult with
                    | Failed failure
                    | PartiallySucceeded(_, failure) when failure.Code = "conflicts_detected" -> ()
                    | Failed failure
                    | PartiallySucceeded(_, failure) ->
                        failwith $"Expected conflicts_detected, received {failure.Category}/{failure.Code}."
                    | Succeeded _ -> failwith "Expected a conflict session."

                    let conflicts = conflictService session
                    let! activeResult = conflicts.GetActiveSession(ctx "revision-identity-finalize-session") |> Async.StartAsPromise
                    let summary =
                        match expectValue "revision identity finalize session" activeResult with
                        | Some value -> value
                        | None -> failwith "Expected an active conflict session."

                    let! resolveStatus = sessionStatus session
                    let! resolveResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = resolveStatus.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (ctx "revision-identity-finalize-resolve")
                        |> Async.StartAsPromise

                    let resolution = expectValue "revision identity finalize resolution" resolveResult
                    let! finalizeStatus = sessionStatus session
                    let! finalizeResult =
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "test: revision identity finalize"
                            }
                            (ctx "revision-identity-finalize")
                        |> Async.StartAsPromise

                    expectValue "revision identity finalize" finalizeResult |> ignore
                    let! commitIdentity = runGitIn workPath [| "log"; "-1"; "--format=%an;%ae;%cn;%ce" |]
                    Vitest.expect(commitIdentity.Trim()).toBe "Finalize Author;finalize@example.org;Finalize Author;finalize@example.org"
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "Git publish target identity",
    fun () ->
        Vitest.test (
            "publish target identity reports a missing remote before ls-remote or push",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    observed.Add request
                                    return! NodeProcess.run request processContext
                                })
                }

                let! root, workPath, _, session = createSyncFixture hooks

                try
                    let! status = sessionStatus session
                    let! _ = runGitIn workPath [| "branch"; "--unset-upstream" |]
                    let! _ = runGitIn workPath [| "remote"; "remove"; "origin" |]
                    let credentialCalls = ResizeArray<string * string option>()
                    let strategy: GitCredentialStrategy.GitCredentialStrategy = {
                        ResolveCredential =
                            fun host profileId ->
                                async {
                                    credentialCalls.Add(host, profileId)
                                    return
                                        Some {
                                            Username = "missing-target"
                                            Secret = "must-not-be-resolved"
                                        }
                                }
                    }

                    let binding: WorkspaceBinding = {
                        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
                        ProviderId = gitProviderId
                        WorkspaceRoot = workPath
                        ProviderStateRef = None
                        Location = {
                            ProviderId = gitProviderId
                            DisplayName = None
                            ProviderLocation = "https://origin.local.test/origin.git"
                            ConnectionProfileId = Some "missing-target-profile"
                        }
                        ConnectionProfileId = Some "missing-target-profile"
                    }

                    let credentialSession =
                        GitWorkspaceSession.createSessionWithCredentials hooks strategy binding

                    observed.Clear()

                    let! publishResult =
                        (syncService credentialSession).Publish
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-without-remote")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "publish without configured remote" publishResult
                    Vitest.expect(failure.Category).toEqual (Validation)
                    Vitest.expect(failure.Code).toBe ("publish_target_missing")

                    Vitest
                        .expect(
                            observed
                            |> Seq.exists (fun request ->
                                request.Arguments
                                |> Array.exists (fun argument -> argument = "ls-remote" || argument = "push"))
                        )
                        .toBe false

                    Vitest.expect(credentialCalls.Count).toBe 0

                    do! removeDirectoryAsync root
                with error ->
                    let! _ = removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "publish target identity follows the configured upstream remote",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    observed.Add request
                                    return! NodeProcess.run request processContext
                                })
                }

                let! root, workPath, originPath, session = createSyncFixture hooks

                try
                    let upstreamPath = join [| root; "upstream.git" |]
                    let! _ = runGitIn root [| "init"; "--bare"; "-b"; "main"; upstreamPath |]
                    let! _ = runGitIn workPath [| "remote"; "add"; "upstream"; upstreamPath |]
                    let! _ = runGitIn workPath [| "push"; "-u"; "upstream"; "main" |]
                    let! _ = runGitIn workPath [| "branch"; "--set-upstream-to=upstream/main"; "main" |]

                    do! writeUtf8FileAsync (join [| workPath; "upstream-target.txt" |]) "upstream target\n"
                    let! beforeRevisionStatus = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: publish to configured upstream"
                                Paths = [| mkPath "upstream-target.txt" |]
                                ExpectedWorkspaceVersion = beforeRevisionStatus.WorkspaceVersion
                            }
                            (ctx "publish-upstream-revision")
                        |> Async.StartAsPromise

                    expectValue "configured upstream revision" revisionResult |> ignore
                    let! localRevision = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    let! publishStatus = sessionStatus session
                    observed.Clear()

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-to-configured-upstream")
                        |> Async.StartAsPromise

                    expectValue "publish to configured upstream" publishResult |> ignore

                    let! upstreamRevision = runGitIn upstreamPath [| "rev-parse"; "main" |]
                    let! originRevision = runGitIn originPath [| "rev-parse"; "main" |]

                    Vitest.expect(upstreamRevision.Trim()).toBe (localRevision.Trim())
                    Vitest.expect(originRevision.Trim()).not.toBe (localRevision.Trim())

                    let pushRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "push")

                    Vitest.expect(pushRequest.Arguments |> Array.contains "upstream").toBe true
                    Vitest.expect(pushRequest.Arguments |> Array.contains "origin").toBe false

                    let lsRemoteRequests =
                        observed
                        |> Seq.filter (fun request -> request.Arguments |> Array.contains "ls-remote")
                        |> Seq.toArray

                    Vitest.expect(lsRemoteRequests.Length > 0).toBe true
                    Vitest
                        .expect(lsRemoteRequests |> Array.forall (fun request -> request.Arguments |> Array.contains "upstream"))
                        .toBe true

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "publish target identity rejects a configured upstream whose remote was deleted",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()
                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    observed.Add request
                                    return! NodeProcess.run request processContext
                                })
                }

                let! root, workPath, originPath, session = createSyncFixture hooks

                try
                    let upstreamPath = join [| root; "deleted-upstream.git" |]
                    let! _ = runGitIn root [| "init"; "--bare"; "-b"; "main"; upstreamPath |]
                    let! _ = runGitIn workPath [| "remote"; "add"; "upstream"; upstreamPath |]
                    let! _ = runGitIn workPath [| "push"; "-u"; "upstream"; "main" |]
                    let! _ = runGitIn workPath [| "remote"; "remove"; "upstream" |]
                    let! _ = runGitIn workPath [| "config"; "branch.main.remote"; "upstream" |]
                    let! _ = runGitIn workPath [| "config"; "branch.main.merge"; "refs/heads/main" |]
                    do! writeUtf8FileAsync (join [| workPath; "deleted-upstream.txt" |]) "local only\n"
                    let! _ = runGitIn workPath [| "add"; "deleted-upstream.txt" |]
                    let! _ = runGitIn workPath [| "commit"; "-m"; "test: deleted upstream" |]
                    let! originBefore = runGitIn originPath [| "rev-parse"; "main" |]
                    let! status = sessionStatus session
                    observed.Clear()

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-deleted-upstream")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "publish with deleted upstream" publishResult
                    Vitest.expect(failure.Category).toEqual (Validation)
                    Vitest.expect(failure.Code).toBe ("configured_target_invalid")

                    Vitest
                        .expect(
                            observed
                            |> Seq.exists (fun request ->
                                request.Arguments
                                |> Array.exists (fun argument -> argument = "ls-remote" || argument = "push"))
                        )
                        .toBe false

                    let! originAfter = runGitIn originPath [| "rev-parse"; "main" |]
                    Vitest.expect(originAfter.Trim()).toBe (originBefore.Trim())

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "publish target identity scopes credentials and LFS URLs to the configured upstream URL",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()
                let upstreamUrl = "https://upstream.local.test/upstream.git"

                // The fixture redirects the https upstream to a local repository with
                // insteadOf so the push can run for real. git would report the rewritten
                // file URL as the effective fetch and push URL, which is where credentials
                // would rightly stop, so the hook answers both get-url queries with the
                // https URL the test is about. Every other command runs unchanged.
                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    observed.Add request

                                    if
                                        request.Arguments |> Array.contains "get-url"
                                        && request.Arguments |> Array.contains "upstream"
                                    then
                                        let output: NodeProcess.ProcessOutput = {
                                            ExitCode = 0
                                            StdOut = upstreamUrl + "
"
                                            StdErr = ""
                                        }

                                        return OperationResult.succeeded output
                                    else
                                        return! NodeProcess.run request processContext
                                })
                }

                let! root, workPath, originPath, _ = createSyncFixture hooks

                try
                    let originHost = "origin.local.test"
                    let upstreamHost = "upstream.local.test"
                    let originUrl = $"https://{originHost}/origin.git"
                    let upstreamPath = join [| root; "credential-upstream.git" |]
                    let! _ = runGitIn root [| "init"; "--bare"; "-b"; "main"; upstreamPath |]
                    let! _ =
                        runGitIn workPath [|
                            "config"
                            "--add"
                            $"url.{toFileRemoteUrl originPath}.insteadOf"
                            originUrl
                        |]

                    let! _ = runGitIn workPath [| "remote"; "set-url"; "origin"; originUrl |]
                    let! _ =
                        runGitIn workPath [|
                            "config"
                            "--add"
                            $"url.{toFileRemoteUrl upstreamPath}.insteadOf"
                            upstreamUrl
                        |]

                    let! _ = runGitIn workPath [| "remote"; "add"; "upstream"; upstreamUrl |]
                    let! _ = runGitIn workPath [| "push"; "-u"; "upstream"; "main" |]
                    let! _ = runGitIn workPath [| "branch"; "--set-upstream-to=upstream/main"; "main" |]
                    do! writeUtf8FileAsync (join [| workPath; "credential-upstream.txt" |]) "upstream credentials\n"
                    let! _ = runGitIn workPath [| "add"; "credential-upstream.txt" |]
                    let! _ = runGitIn workPath [| "commit"; "-m"; "test: upstream credentials" |]

                    let credentialCalls = ResizeArray<string * string option>()
                    let strategy: GitCredentialStrategy.GitCredentialStrategy = {
                        ResolveCredential =
                            fun host profileId ->
                                async {
                                    credentialCalls.Add(host, profileId)

                                    if host = upstreamHost then
                                        return
                                            Some {
                                                Username = "upstream-user"
                                                Secret = "upstream-secret"
                                            }
                                    else
                                        return
                                            Some {
                                                Username = "origin-user"
                                                Secret = "origin-secret"
                                            }
                                }
                    }

                    let binding: WorkspaceBinding = {
                        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
                        ProviderId = gitProviderId
                        WorkspaceRoot = workPath
                        ProviderStateRef = None
                        Location = {
                            ProviderId = gitProviderId
                            DisplayName = None
                            ProviderLocation = originUrl
                            ConnectionProfileId = Some "publish-profile"
                        }
                        ConnectionProfileId = Some "publish-profile"
                    }

                    let session = GitWorkspaceSession.createSessionWithCredentials hooks strategy binding
                    let! status = sessionStatus session
                    observed.Clear()
                    credentialCalls.Clear()

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-upstream-credentials")
                        |> Async.StartAsPromise

                    expectValue "publish with upstream credentials" publishResult |> ignore
                    // One resolution for the ls-remote pre-check (fetch URL) and one for the push
                    // (push URL). Both name the upstream host, never origin's.
                    Vitest.expect(credentialCalls.ToArray()).toEqual [|
                        upstreamHost, Some "publish-profile"
                        upstreamHost, Some "publish-profile"
                    |]

                    // The credential host must come from the effective push URL query, which
                    // the hook answered, and not from the raw remote.<name>.url value.
                    let askedForPushUrl =
                        observed
                        |> Seq.exists (fun request ->
                            request.Arguments = [| "remote"; "get-url"; "--push"; "--all"; "upstream" |])

                    Vitest.expect(askedForPushUrl).toBe true

                    let pushRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "push")

                    let expectedHeader = $"http.https://{upstreamHost}/.extraHeader=Authorization: Basic "
                    let expectedLfsUrl =
                        $"lfs.url=https://upstream-user:upstream-secret@{upstreamHost}/upstream.git/info/lfs"

                    Vitest
                        .expect(pushRequest.Arguments |> Array.exists (fun argument -> argument.StartsWith expectedHeader))
                        .toBe true

                    Vitest.expect(pushRequest.Arguments |> Array.contains expectedLfsUrl).toBe true

                    Vitest
                        .expect(
                            pushRequest.Arguments
                            |> Array.exists (fun argument ->
                                argument.Contains(originHost) || argument.Contains("origin-secret"))
                        )
                        .toBe false

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "refresh scopes fetch credentials to the configured remote when the binding is a local path",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                observed.Add request

                                if
                                    request.Arguments |> Array.contains "get-url"
                                    && request.Arguments |> Array.contains "origin"
                                then
                                    let output: NodeProcess.ProcessOutput = {
                                        ExitCode = 0
                                        StdOut = "https://fetch.local.test/origin.git\n"
                                        StdErr = ""
                                    }

                                    return OperationResult.succeeded output
                                else
                                    return! NodeProcess.run request processContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, _ = createSyncFixture hooks

                try
                    let credentialCalls = ResizeArray<string * string option>()
                    let strategy: GitCredentialStrategy.GitCredentialStrategy = {
                        ResolveCredential =
                            fun host profileId ->
                                async {
                                    credentialCalls.Add(host, profileId)

                                    return
                                        Some {
                                            Username = "fetch-user"
                                            Secret = "fetch-secret"
                                        }
                                }
                    }

                    let baseBinding = syncBinding workPath barePath

                    let binding = {
                        baseBinding with
                            Location = {
                                baseBinding.Location with
                                    ProviderLocation = workPath
                                    ConnectionProfileId = Some "fetch-profile"
                            }
                            ConnectionProfileId = Some "fetch-profile"
                    }

                    let session = GitWorkspaceSession.createSessionWithCredentials hooks strategy binding

                    let! refreshResult =
                        (syncService session).Refresh(ctx "refresh-fetch-credentials")
                        |> Async.StartAsPromise

                    expectValue "refresh fetch credentials" refreshResult |> ignore

                    Vitest.expect(credentialCalls.ToArray()).toEqual [| "fetch.local.test", Some "fetch-profile" |]

                    Vitest
                        .expect(
                            observed
                            |> Seq.exists (fun request -> request.Arguments = [| "remote"; "get-url"; "origin" |])
                        )
                        .toBe true

                    let fetchRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "fetch")

                    let expectedHeader =
                        "http.https://fetch.local.test/.extraHeader=Authorization: Basic "

                    Vitest
                        .expect(fetchRequest.Arguments |> Array.exists (fun argument -> argument.StartsWith expectedHeader))
                        .toBe true

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "publish authenticates the pre-check against the fetch url and the push against the push url",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                observed.Add request

                                if request.Arguments |> Array.contains "get-url" then
                                    let output: NodeProcess.ProcessOutput = {
                                        ExitCode = 0
                                        StdOut =
                                            if request.Arguments |> Array.contains "--push" then
                                                "ssh://git@push.local.test/origin.git\n"
                                            else
                                                "https://fetch.local.test/origin.git\n"
                                        StdErr = ""
                                    }

                                    return OperationResult.succeeded output
                                else
                                    return! NodeProcess.run request processContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, _ = createSyncFixture hooks

                try
                    let! _ =
                        runGitIn workPath [|
                            "config"
                            "--add"
                            $"url.{toFileRemoteUrl barePath}.insteadOf"
                            "https://fetch.local.test/origin.git"
                        |]

                    let! _ =
                        runGitIn workPath [|
                            "config"
                            "--add"
                            $"url.{toFileRemoteUrl barePath}.insteadOf"
                            "ssh://git@push.local.test/origin.git"
                        |]

                    let! _ = runGitIn workPath [| "remote"; "set-url"; "origin"; "https://fetch.local.test/origin.git" |]

                    let! _ =
                        runGitIn workPath [|
                            "remote"
                            "set-url"
                            "--push"
                            "origin"
                            "ssh://git@push.local.test/origin.git"
                        |]

                    let credentialCalls = ResizeArray<string * string option>()
                    let strategy: GitCredentialStrategy.GitCredentialStrategy = {
                        ResolveCredential =
                            fun host profileId ->
                                async {
                                    credentialCalls.Add(host, profileId)

                                    if host = "fetch.local.test" then
                                        return
                                            Some {
                                                Username = "fetch-user"
                                                Secret = "fetch-secret"
                                            }
                                    else
                                        return None
                                }
                    }

                    let baseBinding = syncBinding workPath barePath

                    let binding = {
                        baseBinding with
                            Location = { baseBinding.Location with ConnectionProfileId = Some "split-profile" }
                            ConnectionProfileId = Some "split-profile"
                    }

                    let session = GitWorkspaceSession.createSessionWithCredentials hooks strategy binding
                    do! writeUtf8FileAsync (join [| workPath; "split-publish.txt" |]) "split publish\n"
                    let! status = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: split publish URLs"
                                Paths = [| mkPath "split-publish.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "split-publish-revision")
                        |> Async.StartAsPromise

                    expectValue "split publish revision" revisionResult |> ignore
                    let! publishStatus = sessionStatus session
                    observed.Clear()
                    credentialCalls.Clear()

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-split-urls")
                        |> Async.StartAsPromise

                    expectValue "split URL publish" publishResult |> ignore

                    Vitest.expect(credentialCalls.ToArray()).toEqual [|
                        "fetch.local.test", Some "split-profile"
                        "push.local.test", Some "split-profile"
                    |]

                    let lsRemoteRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "ls-remote")

                    let expectedHeader =
                        "http.https://fetch.local.test/.extraHeader=Authorization: Basic "

                    Vitest
                        .expect(lsRemoteRequest.Arguments |> Array.exists (fun argument -> argument.StartsWith expectedHeader))
                        .toBe true

                    let pushRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "push")

                    Vitest
                        .expect(
                            pushRequest.Arguments
                            |> Array.exists (fun argument -> argument.Contains "extraHeader")
                        )
                        .toBe false

                    Vitest
                        .expect(
                            pushRequest.Arguments
                            |> Array.exists (fun argument -> argument.Contains "fetch-secret")
                        )
                        .toBe false

                    Vitest
                        .expect(
                            observed
                            |> Seq.exists (fun request ->
                                request.Arguments = [| "remote"; "get-url"; "--push"; "--all"; "origin" |])
                        )
                        .toBe true

                    Vitest
                        .expect(
                            observed
                            |> Seq.exists (fun request -> request.Arguments = [| "remote"; "get-url"; "origin" |])
                        )
                        .toBe true

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        // No stub here. git itself expands the insteadOf rule, and the credential must
        // name the rewritten host. The fetch then fails because that host does not exist,
        // which is fine: the assertion is about the credential call, not the transfer.
        Vitest.test (
            "refresh reads the insteadOf-rewritten fetch url from git",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, _ = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    let! _ =
                        runGitIn workPath [|
                            "config"
                            "url.https://real.local.test/.insteadOf"
                            "https://alias.local.test/"
                        |]

                    let! _ = runGitIn workPath [| "remote"; "set-url"; "origin"; "https://alias.local.test/origin.git" |]

                    let credentialCalls = ResizeArray<string * string option>()

                    let strategy: GitCredentialStrategy.GitCredentialStrategy = {
                        ResolveCredential =
                            fun host profileId ->
                                async {
                                    credentialCalls.Add(host, profileId)
                                    return None
                                }
                    }

                    let session =
                        GitWorkspaceSession.createSessionWithCredentials
                            GitWorkspaceSession.GitSessionHooks.none
                            strategy
                            (syncBinding workPath barePath)

                    let! refreshResult =
                        (syncService session).Refresh(ctx "refresh-insteadof")
                        |> Async.StartAsPromise

                    match refreshResult with
                    | Succeeded _ -> failwith "Expected the fetch against a nonexistent host to fail."
                    | _ -> ()

                    Vitest.expect(credentialCalls.ToArray()).toEqual [| "real.local.test", None |]

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "clone scopes credentials to the insteadOf-expanded location",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                observed.Add request

                                if request.Arguments |> Array.contains "--get-url" then
                                    return
                                        OperationResult.succeeded {
                                            NodeProcess.ExitCode = 0
                                            StdOut = "https://rewritten.local.test/repo.git\n"
                                            StdErr = ""
                                        }
                                elif request.Arguments |> Array.contains "clone" then
                                    return
                                        OperationResult.succeeded {
                                            NodeProcess.ExitCode = 128
                                            StdOut = ""
                                            StdErr = "fatal: simulated clone failure"
                                        }
                                else
                                    return! NodeProcess.run request processContext
                            })
                    Barrier = None
                }

                let credentialCalls = ResizeArray<string * string option>()
                let strategy: GitCredentialStrategy.GitCredentialStrategy = {
                    ResolveCredential =
                        fun host profileId ->
                            async {
                                credentialCalls.Add(host, profileId)

                                return
                                    Some {
                                        Username = "clone-user"
                                        Secret = "clone-secret"
                                    }
                            }
                }

                let! root = createTempDirectoryAsync ()
                let targetPath = join [| root; "rewritten-clone" |]

                try
                    let factory = GitWorkspaceSession.createFactoryWithCredentials hooks strategy

                    let! cloneResult =
                        factory.Clone
                            {
                                Location = {
                                    ProviderId = gitProviderId
                                    DisplayName = None
                                    ProviderLocation = "https://alias.local.test/repo.git"
                                    ConnectionProfileId = Some "clone-profile"
                                }
                                TargetPath = targetPath
                                TargetRef = None
                                MaterializeAllObjects = false
                            }
                            (ctx "clone-rewritten")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "clone rewritten location" cloneResult
                    Vitest.expect(failure.Code).toBe "clone_failed"
                    Vitest.expect(credentialCalls.ToArray()).toEqual [| "rewritten.local.test", Some "clone-profile" |]

                    let cloneRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "clone")

                    let expectedHeader =
                        "http.https://rewritten.local.test/.extraHeader=Authorization: Basic "

                    Vitest
                        .expect(cloneRequest.Arguments |> Array.exists (fun argument -> argument.StartsWith expectedHeader))
                        .toBe true

                    Vitest.expect(cloneRequest.Arguments |> Array.contains "https://alias.local.test/repo.git").toBe true

                    Vitest
                        .expect(
                            observed
                            |> Seq.exists (fun request ->
                                request.Arguments = [|
                                    "ls-remote"
                                    "--get-url"
                                    "https://alias.local.test/repo.git"
                                |])
                        )
                        .toBe true

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "Git unmerged worktree evidence",
    fun () ->
        Vitest.test (
            "preserves unmerged worktree evidence for rename/rename paths with no worktree files",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, _, session = createRenameRenameConflictFixture ()

                try
                    let! status = Async.StartAsPromise(session.Core.GetStatus(ctx "status-rename-unmerged"))
                    let workspace = expectSucceeded "status with missing unmerged worktree file" status
                    Vitest
                        .expect(workspace.Changes |> Array.exists (fun change ->
                            isConflictedChange change
                            && RepositoryPath.value change.Path = "base.txt"))
                        .toBe true
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "rotates unmerged worktree evidence after a conflicted file is deleted",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, session, conflicts, capturedSummary, _ = createUnmergedConflictFixture ()

                try
                    let! beforeDeletionStatus =
                        Async.StartAsPromise(session.Core.GetStatus(ctx "status-before-unmerged-deletion"))

                    let beforeDeletion =
                        expectSucceeded "status before deleting unmerged worktree file" beforeDeletionStatus

                    let conflictFile = join [| workPath; "base.txt" |]
                    do! removePathAsync conflictFile

                    let! deletedStatus = Async.StartAsPromise(session.Core.GetStatus(ctx "status-deleted-unmerged"))
                    let deletedWorkspace = expectSucceeded "status with deleted unmerged worktree file" deletedStatus
                    Vitest.expect(deletedWorkspace.Changes |> Array.filter isConflictedChange |> Array.length > 0).toBe true
                    Vitest.expect(deletedWorkspace.WorkspaceVersion).not.toBe (beforeDeletion.WorkspaceVersion)

                    let! staleResolve =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = capturedSummary.Handle
                                    ExpectedWorkspaceVersion = beforeDeletion.WorkspaceVersion
                                    Path = mkPath "base.txt"
                                    Resolution = PickCandidate "target"
                                }
                                (ctx "missing-unmerged-stale-resolve")
                        )

                    let failure =
                        match staleResolve with
                        | Failed failure -> failure
                        | PartiallySucceeded(_, failure) -> failure
                        | Succeeded _ -> failwith "Expected the deleted unmerged worktree file to stale the captured request."

                    Vitest.expect(failure.Category).toEqual (Concurrency)
                    Vitest.expect(failure.Code).toBe ("precondition_failed")
                    Vitest
                        .expect(failure.RecoveryAction |> Option.map _.Code)
                        .toEqual (Some "refresh_conflict_session")

                    do! ensureDirectoryAsync conflictFile

                    let! directoryStatus =
                        Async.StartAsPromise(session.Core.GetStatus(ctx "status-directory-replaced-unmerged"))

                    let directoryWorkspace =
                        expectSucceeded "status with directory-replaced unmerged worktree file" directoryStatus

                    Vitest
                        .expect(directoryWorkspace.Changes |> Array.filter isConflictedChange |> Array.length > 0)
                        .toBe true

                    Vitest.expect(directoryWorkspace.WorkspaceVersion).not.toBe (deletedWorkspace.WorkspaceVersion)

                    let! cancelResult =
                        Async.StartAsPromise(
                            conflicts.Cancel
                                {
                                    Handle = capturedSummary.Handle
                                    ExpectedWorkspaceVersion = directoryWorkspace.WorkspaceVersion
                                }
                                (ctx "directory-replaced-unmerged-cancel")
                        )

                    expectSucceeded "cancel after directory-replaced unmerged worktree file" cancelResult |> ignore
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

)

Vitest.describe (
    "Git conflict evidence hardening",
    fun () ->
        let expectFailure operationName result =
            match result with
            | Failed failure -> failure
            | PartiallySucceeded(_, failure) -> failure
            | Succeeded _ -> failwith $"Expected {operationName} to fail."

        Vitest.test (
            "rotates the workspace token for byte sequences that collide under DJB2",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, session, _, _, _ = createUnmergedConflictFixture ()

                try
                    let conflictFile = join [| workPath; "base.txt" |]
                    do! writeUtf8FileAsync conflictFile "AB"
                    let! firstStatus = Async.StartAsPromise(session.Core.GetStatus(ctx "collision-evidence-first"))
                    let firstVersion = (expectValue "first collision evidence" firstStatus).WorkspaceVersion

                    do! writeUtf8FileAsync conflictFile "B!"
                    let! secondStatus = Async.StartAsPromise(session.Core.GetStatus(ctx "collision-evidence-second"))
                    let secondVersion = (expectValue "second collision evidence" secondStatus).WorkspaceVersion

                    Vitest.expect(secondVersion).not.toBe (firstVersion)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "returns manual-resolution previews for invalid UTF-8 conflict content",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, _, conflicts, _, _ = createUnmergedConflictFixture ()

                try
                    do! writeBinaryFileAsync (join [| workPath; "base.txt" |]) [| 255; 254; 253 |]
                    let! result = Async.StartAsPromise(conflicts.GetActiveSession(ctx "binary-conflict-preview"))

                    let summary =
                        match expectValue "binary conflict preview" result with
                        | Some value -> value
                        | None -> failwith "Expected an active binary conflict session."

                    Vitest.expect(summary.Items[0].CombinedPreview).toEqual (
                        Some(UnsupportedPreview(Some "Unsupported git content for 'base.txt'."))
                    )
                    Vitest.expect(summary.Items[0].SupportsResolvedContent).toBe (false)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "keeps invalid UTF-8 stage candidates byte-oriented and rejects a text resolution",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable injectBinaryStages = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess =
                        Some(fun request context ->
                            if
                                injectBinaryStages
                                && request.Arguments.Length = 3
                                && request.Arguments[0] = "cat-file"
                                && request.Arguments[1] = "blob"
                            then
                                async {
                                    let output: NodeProcess.ByteProcessOutput = {
                                        ExitCode = 0
                                        StdOut = bufferFromBytes [| 255; 254; 253 |]
                                        StdErr = ""
                                    }

                                    return OperationResult.succeeded output
                                }
                            else
                                NodeProcess.runBytes request context)
                    RunProcess = None
                    Barrier = None
                }

                let! root, workPath, session, conflicts, _, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    injectBinaryStages <- true
                    let! summaryResult =
                        Async.StartAsPromise(conflicts.GetActiveSession(ctx "binary-stage-conflict-preview"))

                    let summary =
                        match expectValue "binary stage conflict preview" summaryResult with
                        | Some value -> value
                        | None -> failwith "Expected an active binary stage conflict session."

                    let previews = summary.Items[0].Candidates |> Array.choose _.Preview
                    Vitest.expect(previews |> Array.forall (function UnsupportedPreview _ -> true | _ -> false)).toBe (true)
                    Vitest.expect(summary.Items[0].SupportsResolvedContent).toBe (false)

                    let! statusResult = Async.StartAsPromise(session.Core.GetStatus(ctx "binary-stage-status"))
                    let workspaceVersion = (expectValue "binary stage status" statusResult).WorkspaceVersion
                    let! resolveResult =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = workspaceVersion
                                    Path = mkPath "base.txt"
                                    Resolution = SupplyResolvedContent "manual text resolution\n"
                                }
                                (ctx "binary-stage-resolve")
                        )

                    let failure = expectFailure "binary stage text resolution" resolveResult
                    Vitest.expect(failure.Category).toEqual (Unsupported)
                    Vitest.expect(failure.Code).toBe ("manual_resolution_required")
                    let! markerContent = tryReadUtf8FileAsync (join [| workPath; "base.txt" |])
                    Vitest.expect(markerContent |> Option.exists (fun content -> content.Contains "<<<<<<<")).toBe (true)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "picks an original candidate for a binary conflict",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let workspaceBytes = [| 0; 159; 146; 150; 255 |]
                let targetBytes = [| 0; 255; 1; 2; 3 |]
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    do! writeBinaryFileAsync (join [| workPath; "base.txt" |]) workspaceBytes
                    let! workspaceStatus = sessionStatus session
                    let! saveResult =
                        Async.StartAsPromise(
                            session.Core.CreateRevision
                                {
                                    Message = "local binary conflict"
                                    Paths = [| mkPath "base.txt" |]
                                    ExpectedWorkspaceVersion = workspaceStatus.WorkspaceVersion
                                }
                                (ctx "binary-conflict-save")
                        )

                    expectValue "local binary conflict revision" saveResult |> ignore
                    do! advanceTargetWithBinary root barePath "base.txt" targetBytes

                    let! updateStatus = sessionStatus session
                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                                (ctx "binary-conflict-update")
                        )

                    match updateResult with
                    | Failed failure
                    | PartiallySucceeded(_, failure) when failure.Code = "conflicts_detected" -> ()
                    | Failed failure
                    | PartiallySucceeded(_, failure) ->
                        failwith $"Expected conflicts_detected, received {failure.Category}/{failure.Code}."
                    | Succeeded _ -> failwith "Expected the binary changes to conflict."

                    let conflicts = conflictService session
                    let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "binary-conflict-session"))
                    let summary =
                        match expectValue "binary conflict session" summaryResult with
                        | Some value -> value
                        | None -> failwith "Expected an active binary conflict session."

                    Vitest.expect(summary.Items[0].SupportsResolvedContent).toBe (false)

                    let! status = sessionStatus session
                    let! resolveResult =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                    Path = mkPath "base.txt"
                                    Resolution = PickCandidate "target"
                                }
                                (ctx "binary-conflict-pick-target")
                        )

                    let resolution = expectValue "binary conflict target pick" resolveResult
                    Vitest.expect(resolution.RemainingItems.Length).toBe (0)

                    let! resolvedBytes =
                        fsPromisesDynamic?readFile (join [| workPath; "base.txt" |])
                        |> unbox<JS.Promise<obj>>

                    Vitest.expect(buffersEqual resolvedBytes (bufferFromBytes targetBytes)).toBe (true)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "an LFS conflict is pick-only and describes each candidate's object",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, _, path, baseBytes, workspaceBytes, targetBytes, session = createLfsConflictFixture ()

                    try
                        let! summaryResult = Async.StartAsPromise((conflictService session).GetActiveSession(ctx "lfs-object-summary"))
                        let summary =
                            match expectValue "LFS conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active LFS conflict session."

                        let item = summary.Items[0]
                        Vitest.expect(RepositoryPath.value item.Path).toBe path
                        Vitest.expect(item.SupportsResolvedContent).toBe false

                        for candidate in item.Candidates do
                            let expectedBytes =
                                match candidate.CandidateId with
                                | "base" -> baseBytes
                                | "workspace" -> workspaceBytes
                                | "target" -> targetBytes
                                | candidateId -> failwith $"Unexpected LFS conflict candidate '{candidateId}'."

                            match candidate.Object with
                            | None -> failwith $"Expected object metadata for candidate '{candidate.CandidateId}'."
                            | Some objectInfo ->
                                Vitest.expect(objectInfo.ObjectId).toEqual (Some(sha256Hex expectedBytes))
                                Vitest.expect(objectInfo.SizeBytes).toEqual (Some(float expectedBytes.Length))


                                if candidate.CandidateId = "workspace" then
                                    Vitest.expect(objectInfo.IsLocallyAvailable).toBe true

                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "picking the workspace version of an LFS conflict restores its content from the local cache",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, workPath, path, _, workspaceBytes, _, session = createLfsConflictFixture ()

                    try
                        let conflicts = conflictService session
                        let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "lfs-workspace-pick-session"))
                        let summary =
                            match expectValue "LFS conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active LFS conflict session."

                        let! status = sessionStatus session
                        let! resolveResult =
                            Async.StartAsPromise(
                                conflicts.Resolve
                                    {
                                        Handle = summary.Handle
                                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        Path = mkPath path
                                        Resolution = PickCandidate "workspace"
                                    }
                                    (ctx "lfs-workspace-pick")
                            )

                        let outcome = expectSucceededOutcome "LFS workspace pick" resolveResult
                        Vitest.expect(outcome.Value.RemainingItems.Length).toBe 0
                        Vitest.expect(outcome.Warnings.Length).toBe 0

                        let! resolvedBytes = fsPromisesDynamic?readFile (join [| workPath; path |]) |> unbox<JS.Promise<obj>>
                        Vitest.expect(buffersEqual resolvedBytes (bufferFromBytes workspaceBytes)).toBe true

                        let! indexOutput = runGitIn workPath [| "ls-files"; "-s"; "-z"; "--"; path |]
                        let indexEntries = indexOutput.Split '\000' |> Array.filter ((<>) "")
                        Vitest.expect(indexEntries.Length).toBe 1
                        let metadata = indexEntries[0].Substring(0, indexEntries[0].IndexOf '\t').Split ' '
                        Vitest.expect(metadata[2]).toBe "0"

                        let! pointer = runGitIn workPath [| "cat-file"; "-p"; $":0:{path}" |]
                        Vitest.expect(pointer.StartsWith "version https://git-lfs.github.com/spec/v1").toBe true
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "picking a target whose LFS object is not local keeps the pointer and warns",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, workPath, path, _, _, _, session = createLfsConflictFixture ()

                    try
                        let conflicts = conflictService session
                        let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "lfs-target-missing-session"))
                        let summary =
                            match expectValue "LFS conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active LFS conflict session."

                        let targetCandidate =
                            summary.Items[0].Candidates
                            |> Array.find (fun candidate -> candidate.CandidateId = "target")

                        let targetObject =
                            targetCandidate.Object
                            |> Option.defaultWith (fun () -> failwith "Expected target LFS object metadata.")

                        let targetOid =
                            targetObject.ObjectId
                            |> Option.defaultWith (fun () -> failwith "Expected the target LFS object id.")

                        let! lfsEnvironment = runGitIn workPath [| "lfs"; "env" |]
                        let mediaDirectory = lfsMediaDirectory lfsEnvironment
                        let targetObjectPath =
                            join [|
                                mediaDirectory
                                targetOid.Substring(0, 2)
                                targetOid.Substring(2, 2)
                                targetOid
                            |]

                        do! removePathAsync targetObjectPath

                        let! status = sessionStatus session
                        let! resolveResult =
                            Async.StartAsPromise(
                                conflicts.Resolve
                                    {
                                        Handle = summary.Handle
                                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        Path = mkPath path
                                        Resolution = PickCandidate "target"
                                    }
                                    (ctx "lfs-target-pick-missing-object")
                            )

                        let outcome = expectSucceededOutcome "LFS target pick without cached object" resolveResult
                        Vitest.expect(outcome.Value.RemainingItems.Length).toBe 0
                        Vitest.expect(outcome.Warnings |> Array.exists (fun warning -> warning.Code = "object_not_materialized")).toBe true

                        let! pointer = tryReadUtf8FileAsync (join [| workPath; path |])

                        match pointer with
                        | Some content ->
                            Vitest.expect(content.StartsWith "version https://git-lfs.github.com/spec/v1").toBe true
                        | None -> failwith "Expected the picked LFS pointer file to exist."

                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "picking a target with a fetched LFS object materializes its bytes",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, workPath, path, _, _, targetBytes, session = createLfsConflictFixture ()

                    try
                        let! mergeHead = runGitIn workPath [| "rev-parse"; "MERGE_HEAD" |]
                        let! _ = runGitIn workPath [| "lfs"; "fetch"; "origin"; mergeHead.Trim() |]
                        let conflicts = conflictService session
                        let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "lfs-target-local-session"))
                        let summary =
                            match expectValue "LFS conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active LFS conflict session."

                        let! status = sessionStatus session
                        let! resolveResult =
                            Async.StartAsPromise(
                                conflicts.Resolve
                                    {
                                        Handle = summary.Handle
                                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        Path = mkPath path
                                        Resolution = PickCandidate "target"
                                    }
                                    (ctx "lfs-target-local-pick")
                            )

                        let outcome = expectSucceededOutcome "LFS target pick with local object" resolveResult
                        Vitest.expect(outcome.Warnings.Length).toBe 0
                        let! resolvedBytes = fsPromisesDynamic?readFile (join [| workPath; path |]) |> unbox<JS.Promise<obj>>
                        Vitest.expect(buffersEqual resolvedBytes (bufferFromBytes targetBytes)).toBe true
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "picking a root LFS path leaves a same-named nested pointer unchanged",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let nestedBytes = [| 11; 12; 13; 14 |]
                    let! root, workPath, path, _, workspaceBytes, _, session =
                        createLfsConflictFixtureWithOptions
                            GitWorkspaceSession.GitSessionHooks.none
                            "tracked.xlsx"
                            None
                            [| "sub/tracked.xlsx", nestedBytes |]

                    try
                        let nestedPath = join [| workPath; "sub/tracked.xlsx" |]
                        let! nestedPointer = runGitIn workPath [| "lfs"; "pointer"; $"--file={nestedPath}" |]
                        do! writeUtf8FileAsync nestedPath nestedPointer
                        let conflicts = conflictService session
                        let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "lfs-root-anchored-session"))
                        let summary =
                            match expectValue "LFS conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active LFS conflict session."

                        let! status = sessionStatus session
                        let! resolveResult =
                            Async.StartAsPromise(
                                conflicts.Resolve
                                    {
                                        Handle = summary.Handle
                                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        Path = mkPath path
                                        Resolution = PickCandidate "workspace"
                                    }
                                    (ctx "lfs-root-anchored-pick")
                            )

                        expectSucceededOutcome "root LFS pick" resolveResult |> ignore
                        let! rootBytes = fsPromisesDynamic?readFile (join [| workPath; path |]) |> unbox<JS.Promise<obj>>
                        Vitest.expect(buffersEqual rootBytes (bufferFromBytes workspaceBytes)).toBe true
                        let! nestedAfterPick = tryReadUtf8FileAsync (join [| workPath; "sub/tracked.xlsx" |])
                        Vitest.expect(nestedAfterPick).toEqual (Some nestedPointer)
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "reports a partial LFS pick when checkout fails after staging",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let hooks: GitWorkspaceSession.GitSessionHooks = {
                        GitWorkspaceSession.GitSessionHooks.none with
                            RunProcess =
                                Some(fun request context ->
                                    if
                                        request.Arguments.Length >= 2
                                        && request.Arguments[0] = "lfs"
                                        && request.Arguments[1] = "checkout"
                                    then
                                        async {
                                            return
                                                OperationResult.succeeded {
                                                    NodeProcess.ExitCode = 2
                                                    StdOut = ""
                                                    StdErr = "boom"
                                                }
                                        }
                                    else
                                        NodeProcess.run request context)
                    }

                    let! root, workPath, path, _, _, _, session =
                        createLfsConflictFixtureWithOptions hooks "tracked.xlsx" None [||]

                    try
                        let conflicts = conflictService session
                        let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "lfs-checkout-failure-session"))
                        let summary =
                            match expectValue "LFS conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active LFS conflict session."

                        let! status = sessionStatus session
                        let! resolveResult =
                            Async.StartAsPromise(
                                conflicts.Resolve
                                    {
                                        Handle = summary.Handle
                                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        Path = mkPath path
                                        Resolution = PickCandidate "workspace"
                                    }
                                    (ctx "lfs-checkout-failure-pick")
                            )

                        match resolveResult with
                        | PartiallySucceeded(outcome, failure) ->
                            Vitest.expect(failure.Category).toEqual (ProviderError)
                            Vitest.expect(failure.Code).toBe "object_materialization_failed"
                            Vitest.expect(failure.StateChanged).toBe true
                            Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "retry_materialization")
                            Vitest.expect(outcome.Value.RefreshedHandle.Version <> summary.Handle.Version).toBe true
                        | Failed failure ->
                            failwith $"Expected partial success, received {failure.Category}/{failure.Code}."
                        | Succeeded _ -> failwith "Expected partial success after lfs checkout failed."

                        let! unmerged = runGitIn workPath [| "ls-files"; "-u"; "-z"; "--"; path |]
                        Vitest.expect(unmerged).toBe ""
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "rejects supplied content for an LFS conflict without changing the file or index",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, workPath, path, _, _, _, session = createLfsConflictFixture ()

                    try
                        let conflicts = conflictService session
                        let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "lfs-supply-content-session"))
                        let summary =
                            match expectValue "LFS conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active LFS conflict session."

                        let! indexBefore = runGitIn workPath [| "ls-files"; "-u"; "-z"; "--"; path |]
                        let! fileBefore = fsPromisesDynamic?readFile (join [| workPath; path |]) |> unbox<JS.Promise<obj>>
                        let! status = sessionStatus session
                        let! resolveResult =
                            Async.StartAsPromise(
                                conflicts.Resolve
                                    {
                                        Handle = summary.Handle
                                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        Path = mkPath path
                                        Resolution = SupplyResolvedContent "manual replacement"
                                    }
                                    (ctx "lfs-supply-content")
                            )

                        let failure = expectProviderFailure "supplied content for LFS conflict" resolveResult
                        Vitest.expect(failure.Category).toEqual (Unsupported)
                        Vitest.expect(failure.Code).toBe "manual_resolution_required"
                        Vitest.expect(failure.StateChanged).toBe false
                        let! indexAfter = runGitIn workPath [| "ls-files"; "-u"; "-z"; "--"; path |]
                        Vitest.expect(indexAfter).toBe indexBefore
                        let! fileAfter = fsPromisesDynamic?readFile (join [| workPath; path |]) |> unbox<JS.Promise<obj>>
                        Vitest.expect(buffersEqual fileAfter fileBefore).toBe true
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "marks a conflict pick-only when one side stores the file in LFS",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let path = "mixed.bin"
                    let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                    try
                        do! writeBinaryFileAsync (join [| workPath; path |]) [| 98; 97; 115; 101 |]
                        let! _ = runGitIn workPath [| "add"; path |]
                        let! _ = runGitIn workPath [| "commit"; "-m"; "init: plain conflict file" |]
                        let! _ = runGitIn workPath [| "push"; "origin"; "main" |]
                        do! writeBinaryFileAsync (join [| workPath; path |]) [| 119; 111; 114; 107; 115; 112; 97; 99; 101 |]
                        let! saveStatus = sessionStatus session
                        let! saveResult =
                            Async.StartAsPromise(
                                session.Core.CreateRevision
                                    {
                                        Message = "local plain file edit"
                                        Paths = [| mkPath path |]
                                        ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                                    }
                                    (ctx "mixed-lfs-save")
                            )

                        expectValue "local plain file revision" saveResult |> ignore

                        let clonePath = join [| root; "advance-mixed-lfs" |]
                        let! _ = runGitIn root [| "clone"; "-c"; "core.autocrlf=false"; barePath; clonePath |]
                        let! _ = runGitIn clonePath [| "config"; "user.name"; "External Client" |]
                        let! _ = runGitIn clonePath [| "config"; "user.email"; "external@example.org" |]
                        let! _ = runGitIn clonePath [| "lfs"; "install"; "--local" |]
                        let! _ = runGitIn clonePath [| "lfs"; "track"; "*.bin" |]
                        do! writeBinaryFileAsync (join [| clonePath; path |]) [| 116; 97; 114; 103; 101; 116 |]
                        let! _ = runGitIn clonePath [| "add"; "-A" |]
                        let! _ = runGitIn clonePath [| "commit"; "-m"; "external: move file into LFS" |]
                        let! _ = runGitIn clonePath [| "push"; "origin"; "HEAD" |]

                        let! updateStatus = sessionStatus session
                        let! updateResult =
                            Async.StartAsPromise(
                                (syncService session).Update
                                    { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                                    (ctx "mixed-lfs-update")
                            )

                        match updateResult with
                        | Failed failure
                        | PartiallySucceeded(_, failure) when failure.Code = "conflicts_detected" -> ()
                        | Failed failure
                        | PartiallySucceeded(_, failure) ->
                            failwith $"Expected conflicts_detected, received {failure.Category}/{failure.Code}."
                        | Succeeded _ -> failwith "Expected the plain-to-LFS changes to conflict."

                        let! summaryResult = Async.StartAsPromise((conflictService session).GetActiveSession(ctx "mixed-lfs-session"))
                        let summary =
                            match expectValue "mixed LFS conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active mixed LFS conflict session."

                        let item = summary.Items[0]
                        let workspaceCandidate = item.Candidates |> Array.find (fun candidate -> candidate.CandidateId = "workspace")
                        let targetCandidate = item.Candidates |> Array.find (fun candidate -> candidate.CandidateId = "target")
                        Vitest.expect(item.SupportsResolvedContent).toBe false
                        Vitest.expect(workspaceCandidate.Object).toEqual None
                        Vitest.expect(Option.isSome targetCandidate.Object).toBe true
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "materializes an LFS pick for a path with spaces and brackets",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let path = "dir with space/a[1].bin"
                    let! root, workPath, _, _, workspaceBytes, _, session =
                        createLfsConflictFixtureWithOptions
                            GitWorkspaceSession.GitSessionHooks.none
                            path
                            None
                            [||]

                    try
                        let conflicts = conflictService session
                        let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "lfs-special-path-session"))
                        let summary =
                            match expectValue "special path LFS conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active LFS conflict session."

                        let! status = sessionStatus session
                        let! resolveResult =
                            Async.StartAsPromise(
                                conflicts.Resolve
                                    {
                                        Handle = summary.Handle
                                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        Path = mkPath path
                                        Resolution = PickCandidate "workspace"
                                    }
                                    (ctx "lfs-special-path-pick")
                            )

                        expectSucceededOutcome "special path LFS pick" resolveResult |> ignore
                        let! resolvedBytes = fsPromisesDynamic?readFile (join [| workPath; path |]) |> unbox<JS.Promise<obj>>
                        Vitest.expect(buffersEqual resolvedBytes (bufferFromBytes workspaceBytes)).toBe true
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "uses the configured local LFS storage for object lookup and materialization",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! mediaDirectory = createTempDirectoryAsync ()

                    try
                        let! root, workPath, path, _, workspaceBytes, _, session =
                            createLfsConflictFixtureWithOptions
                                GitWorkspaceSession.GitSessionHooks.none
                                "custom-storage.xlsx"
                                (Some mediaDirectory)
                                [||]

                        try
                            let conflicts = conflictService session
                            let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "lfs-custom-storage-session"))
                            let summary =
                                match expectValue "custom storage LFS conflict session" summaryResult with
                                | Some value -> value
                                | None -> failwith "Expected an active LFS conflict session."

                            let workspaceCandidate =
                                summary.Items[0].Candidates
                                |> Array.find (fun candidate -> candidate.CandidateId = "workspace")

                            let objectInfo =
                                workspaceCandidate.Object
                                |> Option.defaultWith (fun () -> failwith "Expected workspace LFS object metadata.")

                            Vitest.expect(objectInfo.IsLocallyAvailable).toBe true
                            let! status = sessionStatus session
                            let! resolveResult =
                                Async.StartAsPromise(
                                    conflicts.Resolve
                                        {
                                            Handle = summary.Handle
                                            ExpectedWorkspaceVersion = status.WorkspaceVersion
                                            Path = mkPath path
                                            Resolution = PickCandidate "workspace"
                                        }
                                        (ctx "lfs-custom-storage-pick")
                                )

                            let outcome = expectSucceededOutcome "custom storage LFS pick" resolveResult
                            Vitest.expect(outcome.Warnings.Length).toBe 0
                            let! resolvedBytes = fsPromisesDynamic?readFile (join [| workPath; path |]) |> unbox<JS.Promise<obj>>
                            Vitest.expect(buffersEqual resolvedBytes (bufferFromBytes workspaceBytes)).toBe true
                            do! removeDirectoryAsync root
                        with error ->
                            do! removeDirectoryAsync root
                            return raise error
                        do! removeDirectoryAsync mediaDirectory
                    with error ->
                        do! removeDirectoryAsync mediaDirectory
                        return raise error
            }
        )

        Vitest.test (
            "picking the deleted side of an LFS conflict deletes the file",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let path = "deleted-lfs.xlsx"
                    let! root, workPath, _, session =
                        createModifyDeleteConflictFixture
                            GitWorkspaceSession.GitSessionHooks.none
                            path
                            [| 0; 1; 2; 3 |]
                            (Some [| 0; 159; 146; 150 |])
                            None
                            true

                    try
                        let conflicts = conflictService session
                        let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "lfs-delete-session"))
                        let summary =
                            match expectValue "LFS modify/delete conflict session" summaryResult with
                            | Some value -> value
                            | None -> failwith "Expected an active LFS modify/delete conflict."

                        let targetCandidate =
                            summary.Items[0].Candidates
                            |> Array.find (fun candidate -> candidate.CandidateId = "target")

                        Vitest.expect(targetCandidate.Preview).toEqual None
                        Vitest.expect(targetCandidate.Object).toEqual None
                        let! status = sessionStatus session
                        let! resolveResult =
                            Async.StartAsPromise(
                                conflicts.Resolve
                                    {
                                        Handle = summary.Handle
                                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                                        Path = mkPath path
                                        Resolution = PickCandidate "target"
                                    }
                                    (ctx "lfs-delete-pick")
                            )

                        let outcome = expectSucceededOutcome "LFS deleted-side pick" resolveResult
                        Vitest.expect(outcome.Value.RemainingItems.Length).toBe 0
                        let! worktreeExists = pathExistsAsync (join [| workPath; path |])
                        Vitest.expect(worktreeExists).toBe false
                        let! unmerged = runGitIn workPath [| "ls-files"; "-u"; "-z"; "--"; path |]
                        Vitest.expect(unmerged).toBe ""
                        let! indexEntries = runGitIn workPath [| "ls-files"; "-s"; "-z"; "--"; path |]
                        Vitest.expect(indexEntries).toBe ""
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "RestorePaths refuses a conflicted LFS path",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! lfsProbe = runGitResultIn "." [| "lfs"; "version" |]

                if lfsProbe.ExitCode <> 0 then
                    Vitest.expect(true).toBe true
                else
                    let! root, workPath, path, _, _, _, session = createLfsConflictFixture ()

                    try
                        let! beforeBytes = fsPromisesDynamic?readFile (join [| workPath; path |]) |> unbox<JS.Promise<obj>>
                        let! status = sessionStatus session
                        let! restoreResult =
                            Async.StartAsPromise(
                                session.Core.RestorePaths
                                    {
                                        Paths = [| mkPath path |]
                                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                                    }
                                    (ctx "restore-conflicted-lfs-path")
                            )

                        let failure = expectProviderFailure "RestorePaths for an LFS conflict" restoreResult
                        Vitest.expect(failure.Category).toEqual (Validation)
                        Vitest.expect(failure.Code).toBe "restore_unmerged_paths"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.AffectedPaths).toEqual [| path |]

                        let! afterBytes = fsPromisesDynamic?readFile (join [| workPath; path |]) |> unbox<JS.Promise<obj>>
                        Vitest.expect(buffersEqual afterBytes beforeBytes).toBe true
                        do! removeDirectoryAsync root
                    with error ->
                        do! removeDirectoryAsync root
                        return raise error
            }
        )

        Vitest.test (
            "picking the deleted side of a binary conflict deletes the file",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let path = "binary-delete.bin"
                let! root, workPath, _, session =
                    createModifyDeleteConflictFixture
                        GitWorkspaceSession.GitSessionHooks.none
                        path
                        [| 0; 1; 2; 3 |]
                        (Some [| 0; 159; 146; 150; 255 |])
                        None
                        false

                try
                    let conflicts = conflictService session
                    let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "binary-delete-session"))
                    let summary =
                        match expectValue "binary modify/delete conflict session" summaryResult with
                        | Some value -> value
                        | None -> failwith "Expected an active binary modify/delete conflict session."

                    let! status = sessionStatus session
                    let! resolveResult =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                    Path = mkPath path
                                    Resolution = PickCandidate "target"
                                }
                                (ctx "binary-delete-pick")
                        )

                    let outcome = expectSucceededOutcome "binary deleted-side pick" resolveResult
                    Vitest.expect(outcome.Value.RemainingItems.Length).toBe 0
                    Vitest.expect(outcome.Value.RefreshedHandle.Version <> summary.Handle.Version).toBe true
                    let! worktreeExists = pathExistsAsync (join [| workPath; path |])
                    Vitest.expect(worktreeExists).toBe false

                    let! unmerged = runGitIn workPath [| "ls-files"; "-u"; "-z"; "--"; path |]
                    Vitest.expect(unmerged).toBe ""
                    let! indexEntries = runGitIn workPath [| "ls-files"; "-s"; "-z"; "--"; path |]
                    Vitest.expect(indexEntries).toBe ""

                    let! finalizeStatus = sessionStatus session
                    let! finalizeResult =
                        Async.StartAsPromise(
                            conflicts.Finalize
                                {
                                    Handle = outcome.Value.RefreshedHandle
                                    ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                    Message = Some "test: delete binary conflict path"
                                }
                                (ctx "binary-delete-finalize")
                        )

                    expectValue "binary deleted-side finalize" finalizeResult |> ignore
                    let! committedPath = runGitIn workPath [| "ls-tree"; "-r"; "--name-only"; "HEAD"; "--"; path |]
                    Vitest.expect(committedPath.Trim()).toBe ""
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "picking the workspace deletion of a text conflict removes its index stages",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let path = "workspace-deleted.txt"
                let! root, workPath, _, session =
                    createModifyDeleteConflictFixture
                        GitWorkspaceSession.GitSessionHooks.none
                        path
                        [| 98; 97; 115; 101; 10 |]
                        None
                        (Some [| 116; 97; 114; 103; 101; 116; 10 |])
                        false

                try
                    let conflicts = conflictService session
                    let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "workspace-delete-session"))
                    let summary =
                        match expectValue "workspace deleted text conflict" summaryResult with
                        | Some value -> value
                        | None -> failwith "Expected an active text modify/delete conflict."

                    let workspaceCandidate = summary.Items[0].Candidates |> Array.find (fun candidate -> candidate.CandidateId = "workspace")
                    Vitest.expect(workspaceCandidate.Preview).toEqual None
                    Vitest.expect(workspaceCandidate.Object).toEqual None

                    let! status = sessionStatus session
                    let! resolveResult =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                    Path = mkPath path
                                    Resolution = PickCandidate "workspace"
                                }
                                (ctx "workspace-delete-pick")
                        )

                    let outcome = expectSucceededOutcome "workspace deleted-side pick" resolveResult
                    Vitest.expect(outcome.Value.RemainingItems.Length).toBe 0
                    let! worktreeExists = pathExistsAsync (join [| workPath; path |])
                    Vitest.expect(worktreeExists).toBe false
                    let! unmerged = runGitIn workPath [| "ls-files"; "-u"; "-z"; "--"; path |]
                    Vitest.expect(unmerged).toBe ""
                    let! indexEntries = runGitIn workPath [| "ls-files"; "-s"; "-z"; "--"; path |]
                    Vitest.expect(indexEntries).toBe ""
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "picking a deleted side succeeds when the worktree file is already absent",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let path = "absent-before-pick.txt"
                let! root, workPath, _, session =
                    createModifyDeleteConflictFixture
                        GitWorkspaceSession.GitSessionHooks.none
                        path
                        [| 98; 97; 115; 101; 10 |]
                        (Some [| 119; 111; 114; 107; 115; 112; 97; 99; 101; 10 |])
                        None
                        false

                try
                    let conflicts = conflictService session
                    let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "absent-delete-session"))
                    let summary =
                        match expectValue "already absent modify/delete conflict" summaryResult with
                        | Some value -> value
                        | None -> failwith "Expected an active modify/delete conflict."

                    do! removePathAsync (join [| workPath; path |])
                    let! worktreeAbsent = pathExistsAsync (join [| workPath; path |])
                    Vitest.expect(worktreeAbsent).toBe false
                    let! status = sessionStatus session
                    let! resolveResult =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                    Path = mkPath path
                                    Resolution = PickCandidate "target"
                                }
                                (ctx "already-absent-delete-pick")
                        )

                    let outcome = expectSucceededOutcome "already absent deleted-side pick" resolveResult
                    Vitest.expect(outcome.Value.RemainingItems.Length).toBe 0
                    let! unmerged = runGitIn workPath [| "ls-files"; "-u"; "-z"; "--"; path |]
                    Vitest.expect(unmerged).toBe ""
                    let! indexEntries = runGitIn workPath [| "ls-files"; "-s"; "-z"; "--"; path |]
                    Vitest.expect(indexEntries).toBe ""
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "answers unknown_candidate for a base pick without a base stage",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
                let addedPath = "added.txt"

                try
                    do! writeUtf8FileAsync (join [| workPath; addedPath |]) "workspace addition\n"
                    let! workspaceStatus = sessionStatus session
                    let! saveResult =
                        Async.StartAsPromise(
                            session.Core.CreateRevision
                                {
                                    Message = "local add-add conflict"
                                    Paths = [| mkPath addedPath |]
                                    ExpectedWorkspaceVersion = workspaceStatus.WorkspaceVersion
                                }
                                (ctx "add-add-conflict-save")
                        )

                    expectValue "local add-add conflict revision" saveResult |> ignore
                    do! advanceTarget root barePath [ addedPath, "target addition\n" ]

                    let! updateStatus = sessionStatus session
                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                                (ctx "add-add-conflict-update")
                        )

                    match updateResult with
                    | Failed failure
                    | PartiallySucceeded(_, failure) when failure.Code = "conflicts_detected" -> ()
                    | Failed failure
                    | PartiallySucceeded(_, failure) ->
                        failwith $"Expected conflicts_detected, received {failure.Category}/{failure.Code}."
                    | Succeeded _ -> failwith "Expected the add/add changes to conflict."

                    let conflicts = conflictService session
                    let! summaryResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "add-add-conflict-session"))
                    let summary =
                        match expectValue "add/add conflict session" summaryResult with
                        | Some value -> value
                        | None -> failwith "Expected an active add/add conflict session."

                    Vitest.expect(summary.Items[0].Candidates |> Array.exists (fun candidate -> candidate.CandidateId = "base")).toBe (false)

                    let! status = sessionStatus session
                    let! resolveResult =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = status.WorkspaceVersion
                                    Path = mkPath addedPath
                                    Resolution = PickCandidate "base"
                                }
                                (ctx "add-add-conflict-pick-base")
                        )

                    let failure = expectFailure "add/add base candidate pick" resolveResult
                    Vitest.expect(failure.Category).toEqual (Validation)
                    Vitest.expect(failure.Code).toBe ("unknown_candidate")
                    Vitest.expect(failure.StateChanged).toBe (false)
                    Vitest.expect(failure.Message).toBe ("The candidate ID is not part of this conflict item.")

                    let! markerContent = tryReadUtf8FileAsync (join [| workPath; addedPath |])
                    Vitest.expect(markerContent |> Option.exists (fun content -> content.Contains "<<<<<<<")).toBe (true)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "does not buffer conflict content for known unsupported extensions",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable inspectReads = false
                let stageBlobReads = ResizeArray<string>()
                let mutable combinedPreviewChunks = 0

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess =
                        Some(fun request context ->
                            if
                                inspectReads
                                && request.Arguments.Length = 3
                                && request.Arguments[0] = "cat-file"
                                && request.Arguments[1] = "blob"
                            then
                                stageBlobReads.Add request.Arguments[2]

                            NodeProcess.runBytes request context)
                    RunProcess = None
                    Barrier =
                        Some(fun _ point _ ->
                            async {
                                if inspectReads && point = "conflict-content-chunk" then
                                    combinedPreviewChunks <- combinedPreviewChunks + 1
                            })
                }

                let! root, workPath, _, conflicts, _ =
                    createPathUnmergedConflictFixtureWithHooks hooks "document.pdf"

                try
                    inspectReads <- true
                    let! result =
                        Async.StartAsPromise(conflicts.GetActiveSession(ctx "unsupported-extension-conflict"))
                    inspectReads <- false

                    let summary =
                        match expectValue "unsupported extension conflict preview" result with
                        | Some value -> value
                        | None -> failwith "Expected an active unsupported-extension conflict session."

                    Vitest.expect((stageBlobReads |> Seq.distinct |> Seq.length) = stageBlobReads.Count).toBe true
                    Vitest.expect(stageBlobReads.Count <= 3).toBe true
                    Vitest.expect(combinedPreviewChunks).toBe (0)
                    Vitest.expect(summary.Items[0].SupportsResolvedContent).toBe (false)
                    do! removeDirectoryAsync root
                with error ->
                    inspectReads <- false
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "does not buffer oversized conflict stage blobs for supported extensions",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable inspectReads = false
                let mutable stageBlobReads = 0

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess =
                        Some(fun request context ->
                            if
                                inspectReads
                                && request.Arguments.Length = 3
                                && request.Arguments[0] = "cat-file"
                                && request.Arguments[1] = "blob"
                            then
                                stageBlobReads <- stageBlobReads + 1

                            NodeProcess.runBytes request context)
                    RunProcess =
                        Some(fun request context ->
                            if
                                inspectReads
                                && request.Arguments.Length = 3
                                && request.Arguments[0] = "cat-file"
                                && request.Arguments[1] = "-s"
                            then
                                async {
                                    return
                                        OperationResult.succeeded {
                                            NodeProcess.ExitCode = 0
                                            StdOut = string (1024 * 1024 + 1)
                                            StdErr = ""
                                        }
                                }
                            else
                                NodeProcess.run request context)
                    Barrier = None
                }

                let! root, _, _, conflicts, _, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    inspectReads <- true
                    let! result =
                        Async.StartAsPromise(conflicts.GetActiveSession(ctx "oversized-stage-conflict"))
                    inspectReads <- false

                    let summary =
                        match expectValue "oversized stage conflict preview" result with
                        | Some value -> value
                        | None -> failwith "Expected an active oversized-stage conflict session."

                    Vitest.expect(stageBlobReads).toBe (0)
                    Vitest.expect(summary.Items[0].SupportsResolvedContent).toBe (false)
                    Vitest
                        .expect(summary.Items[0].Candidates |> Array.forall (fun candidate ->
                            match candidate.Preview with
                            | Some(UnsupportedPreview _) -> true
                            | _ -> false))
                        .toBe (true)
                    do! removeDirectoryAsync root
                with error ->
                    inspectReads <- false
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "reads conflict stages through frozen object ids",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable inspectReads = false
                let mutable symbolicStageReads = 0

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess =
                        Some(fun request context ->
                            if
                                inspectReads
                                && request.Arguments.Length = 3
                                && request.Arguments[0] = "cat-file"
                                && request.Arguments[1] = "blob"
                                && request.Arguments[2].StartsWith(":", StringComparison.Ordinal)
                            then
                                symbolicStageReads <- symbolicStageReads + 1

                            NodeProcess.runBytes request context)
                    RunProcess = None
                    Barrier = None
                }

                let! root, _, _, conflicts, _, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    inspectReads <- true
                    let! result =
                        Async.StartAsPromise(conflicts.GetActiveSession(ctx "frozen-stage-conflict"))
                    inspectReads <- false

                    match expectValue "frozen stage conflict preview" result with
                    | Some summary -> Vitest.expect(summary.Items.Length).toBe (1)
                    | None -> failwith "Expected an active frozen-stage conflict session."

                    Vitest.expect(symbolicStageReads).toBe (0)
                    do! removeDirectoryAsync root
                with error ->
                    inspectReads <- false
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "resolves each conflict stage to one immutable blob before size and content reads",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable inspectReads = false
                let sizeObjects = ResizeArray<string>()
                let contentObjects = ResizeArray<string>()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess =
                        Some(fun request context ->
                            if
                                inspectReads
                                && request.Arguments.Length = 3
                                && request.Arguments[0] = "cat-file"
                                && request.Arguments[1] = "blob"
                            then
                                contentObjects.Add request.Arguments[2]

                            NodeProcess.runBytes request context)
                    RunProcess =
                        Some(fun request context ->
                            if
                                inspectReads
                                && request.Arguments.Length = 3
                                && request.Arguments[0] = "cat-file"
                                && request.Arguments[1] = "-s"
                            then
                                sizeObjects.Add request.Arguments[2]

                            NodeProcess.run request context)
                    Barrier = None
                }

                let! root, _, _, conflicts, _, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    inspectReads <- true
                    let! result =
                        Async.StartAsPromise(conflicts.GetActiveSession(ctx "immutable-stage-preview"))
                    inspectReads <- false
                    expectValue "immutable stage preview" result |> ignore

                    Vitest.expect(sizeObjects.Count).toBeGreaterThan (0)
                    Vitest.expect(sizeObjects.ToArray()).toEqual (contentObjects.ToArray())
                    Vitest
                        .expect(sizeObjects |> Seq.forall (fun objectName -> not (objectName.StartsWith ":")))
                        .toBe (true)
                    do! removeDirectoryAsync root
                with error ->
                    inspectReads <- false
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "bounds combined-preview reads for oversized conflict files",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable countPreviewChunks = false
                let mutable previewChunks = 0

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess = None
                    Barrier =
                        Some(fun _ point _ ->
                            async {
                                if countPreviewChunks && point = "conflict-content-chunk" then
                                    previewChunks <- previewChunks + 1
                            })
                }

                let! root, workPath, _, conflicts, _, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    let largeMarkerText = "z".PadRight(8 * 1024 * 1024, 'z')
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) largeMarkerText
                    countPreviewChunks <- true
                    let! result = Async.StartAsPromise(conflicts.GetActiveSession(ctx "bounded-conflict-preview"))
                    countPreviewChunks <- false

                    let summary =
                        match expectValue "bounded conflict preview" result with
                        | Some value -> value
                        | None -> failwith "Expected an active oversized conflict session."

                    match summary.Items[0].CombinedPreview with
                    | Some(UnsupportedPreview _) -> ()
                    | preview -> failwith $"Expected an unsupported oversized preview, received {preview}."

                    Vitest.expect(previewChunks).toBeLessThanOrEqual (18)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "propagates an unmerged-path process failure instead of minting a token",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable armFailure = false
                let injectedFailure = OperationFailure.create Network "unmerged_probe_failed" "Injected unmerged probe failure."

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            if
                                armFailure
                                && request.Arguments = [| "diff"; "--name-only"; "--diff-filter=U"; "-z" |]
                            then
                                async { return OperationResult.failed injectedFailure }
                            else
                                NodeProcess.run request context)
                    Barrier = None
                }

                let! root, _, session, _, _, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    armFailure <- true
                    let! result = Async.StartAsPromise(session.Core.GetStatus(ctx "failed-unmerged-probe"))
                    let failure = expectFailure "status with failed unmerged probe" result
                    Vitest.expect(failure.Category).toEqual (Network)
                    Vitest.expect(failure.Code).toBe ("unmerged_probe_failed")
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "hashes a large unmerged file from multiple bounded handle chunks",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable observedChunks = 0

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess = None
                    Barrier =
                        Some(fun _ point _ ->
                            async {
                                if point = "conflict-content-chunk" then
                                    observedChunks <- observedChunks + 1
                            })
                }

                let! root, workPath, session, _, _, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    observedChunks <- 0
                    let largeMarkerText = "x".PadRight(8 * 1024 * 1024, 'x')
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) largeMarkerText
                    let! statusResult = Async.StartAsPromise(session.Core.GetStatus(ctx "large-unmerged-status"))
                    expectValue "large unmerged status" statusResult |> ignore
                    Vitest.expect(observedChunks).toBeGreaterThan (1)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "cancels conflict hashing between bounded handle chunks",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let source = OperationCancellation.Source()
                let mutable armed = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess = None
                    Barrier =
                        Some(fun _ point _ ->
                            async {
                                if armed && point = "conflict-content-chunk" then
                                    armed <- false
                                    source.Cancel()
                            })
                }

                let! root, workPath, session, _, _, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    let largeMarkerText = "y".PadRight(2 * 1024 * 1024, 'y')
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) largeMarkerText
                    armed <- true
                    let context = OperationContext.create "cancel-conflict-chunks" source.Cancellation ignore
                    let! result = Async.StartAsPromise(session.Core.GetStatus context)
                    let failure = expectFailure "canceled conflict handle read" result
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "rejects a parent junction swapped in after validation before conflict hashing",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable armRace = false
                let mutable linkedDirectory = ""
                let mutable backupDirectory = ""
                let mutable outsideDirectory = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if armRace && request.Arguments |> Array.contains "hash-object" then
                                    armRace <- false
                                    let! _ =
                                        fsPromisesDynamic?rename (linkedDirectory, backupDirectory)
                                        |> unbox<JS.Promise<obj>>
                                        |> Async.AwaitPromise

                                    do! createDirectoryLinkAsync outsideDirectory linkedDirectory |> Async.AwaitPromise
                                    let! result = NodeProcess.run request context
                                    do! removePathAsync linkedDirectory |> Async.AwaitPromise
                                    let! _ =
                                        fsPromisesDynamic?rename (backupDirectory, linkedDirectory)
                                        |> unbox<JS.Promise<obj>>
                                        |> Async.AwaitPromise

                                    return result
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier =
                        Some(fun _ point _ ->
                            async {
                                if armRace && point = "conflict-content-validated" then
                                    armRace <- false
                                    let! _ =
                                        fsPromisesDynamic?rename (linkedDirectory, backupDirectory)
                                        |> unbox<JS.Promise<obj>>
                                        |> Async.AwaitPromise

                                    do! createDirectoryLinkAsync outsideDirectory linkedDirectory |> Async.AwaitPromise
                            })
                }

                let! root, workPath, session, _, conflictPath = createNestedUnmergedConflictFixtureWithHooks hooks

                try
                    linkedDirectory <- join [| workPath; "nested" |]
                    backupDirectory <- join [| workPath; "nested-safe" |]
                    outsideDirectory <- join [| root; "outside-hash-race" |]
                    let secretText = "outside hash race secret must not influence a token\n"
                    do! writeUtf8FileAsync (join [| outsideDirectory; "conflict.txt" |]) secretText
                    armRace <- true
                    let! result = Async.StartAsPromise(session.Core.GetStatus(ctx "junction-hash-race"))
                    let failure = expectFailure "status during a junction hash race" result
                    Vitest.expect(failure.Category).toEqual (Validation)
                    Vitest.expect(failure.Code).toBe ("unsafe_workspace_path")
                    Vitest.expect(failure.Message.Contains secretText).toBe (false)
                    Vitest.expect(failure.AffectedPaths).toEqual ([| conflictPath |])
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "rejects a parent junction swapped in after validation before combined preview read",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable armRace = false
                let mutable linkedDirectory = ""
                let mutable backupDirectory = ""
                let mutable outsideDirectory = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess = None
                    Barrier =
                        Some(fun _ point _ ->
                            async {
                                if armRace && point = "conflict-content-validated" then
                                    armRace <- false
                                    let! _ =
                                        fsPromisesDynamic?rename (linkedDirectory, backupDirectory)
                                        |> unbox<JS.Promise<obj>>
                                        |> Async.AwaitPromise

                                    do! createDirectoryLinkAsync outsideDirectory linkedDirectory |> Async.AwaitPromise
                            })
                }

                let! root, workPath, _, conflicts, conflictPath = createNestedUnmergedConflictFixtureWithHooks hooks

                try
                    linkedDirectory <- join [| workPath; "nested" |]
                    backupDirectory <- join [| workPath; "nested-safe" |]
                    outsideDirectory <- join [| root; "outside-preview-race" |]
                    let secretText = "outside preview race secret must not be exposed\n"
                    do! writeUtf8FileAsync (join [| outsideDirectory; "conflict.txt" |]) secretText
                    armRace <- true
                    let! result = Async.StartAsPromise(conflicts.GetActiveSession(ctx "junction-preview-race"))
                    let failure = expectFailure "preview during a junction race" result
                    Vitest.expect(failure.Category).toEqual (Validation)
                    Vitest.expect(failure.Code).toBe ("unsafe_workspace_path")
                    Vitest.expect(failure.Message.Contains secretText).toBe (false)
                    Vitest.expect(failure.AffectedPaths).toEqual ([| conflictPath |])
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "rejects a parent junction before conflict hashing can read outside the workspace",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, session, _, conflictPath = createNestedUnmergedConflictFixture ()

                try
                    let outsideDirectory = join [| root; "outside-status" |]
                    let linkedDirectory = join [| workPath; "nested" |]
                    let secretText = "outside status secret must not be read\n"
                    do! writeUtf8FileAsync (join [| outsideDirectory; "conflict.txt" |]) secretText
                    do! removePathAsync linkedDirectory
                    do! createDirectoryLinkAsync outsideDirectory linkedDirectory
                    let! result = Async.StartAsPromise(session.Core.GetStatus(ctx "linked-conflict-status"))
                    let failure = expectFailure "status through a parent junction" result
                    Vitest.expect(failure.Category).toEqual (Validation)
                    Vitest.expect(failure.Code).toBe ("unsafe_workspace_path")
                    Vitest.expect(failure.Message.Contains secretText).toBe (false)
                    Vitest.expect(failure.AffectedPaths).toEqual ([| conflictPath |])
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "rejects a parent junction before combined preview can expose outside bytes",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, _, conflicts, conflictPath = createNestedUnmergedConflictFixture ()

                try
                    let outsideDirectory = join [| root; "outside-preview" |]
                    let linkedDirectory = join [| workPath; "nested" |]
                    let secretText = "outside preview secret must not be exposed\n"
                    do! writeUtf8FileAsync (join [| outsideDirectory; "conflict.txt" |]) secretText
                    do! removePathAsync linkedDirectory
                    do! createDirectoryLinkAsync outsideDirectory linkedDirectory
                    let! result = Async.StartAsPromise(conflicts.GetActiveSession(ctx "linked-conflict-preview"))
                    let failure = expectFailure "preview through a parent junction" result
                    Vitest.expect(failure.Category).toEqual (Validation)
                    Vitest.expect(failure.Code).toBe ("unsafe_workspace_path")
                    Vitest.expect(failure.Message.Contains secretText).toBe (false)
                    Vitest.expect(failure.AffectedPaths).toEqual ([| conflictPath |])
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "Git workspace synchronization",
    fun () ->
        Vitest.test (
            "preview classifies unrelated histories as indeterminate",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, _, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
                let unrelatedPath = join [| root; "unrelated" |]

                try
                    let! _ = runGitIn root [| "init"; "-b"; "main"; unrelatedPath |]
                    let! _ = runGitIn unrelatedPath [| "config"; "user.name"; "Unrelated Target" |]
                    let! _ = runGitIn unrelatedPath [| "config"; "user.email"; "unrelated@example.org" |]
                    do! writeUtf8FileAsync (join [| unrelatedPath; "unrelated.txt" |]) "unrelated target\n"
                    let! _ = runGitIn unrelatedPath [| "add"; "-A" |]
                    let! _ = runGitIn unrelatedPath [| "commit"; "-m"; "unrelated target" |]
                    let! _ = runGitIn unrelatedPath [| "remote"; "add"; "origin"; barePath |]
                    let! _ = runGitIn unrelatedPath [| "push"; "--force"; "origin"; "main" |]

                    let! previewResult =
                        Async.StartAsPromise((syncService session).PreviewUpdate(ctx "preview-unrelated-histories"))

                    match previewResult with
                    | Failed failure ->
                        Vitest.expect(failure.Code).toBe ("preview_indeterminate")
                        Vitest.expect(failure.Retryable).toBe (true)
                    | Succeeded outcome ->
                        failwith $"Expected an indeterminate preview, received {outcome.Value.ChangedPaths.Length} changed paths."
                    | PartiallySucceeded _ -> failwith "Expected unrelated-history preview to fail, not partially succeed."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "conflicting update opens a versioned conflict session and rejects stale tokens before verified finalize",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // A barrier that can advance the local branch between the finalize
                // pre-check and the compare-and-swap ref update.
                let mutable armFinalizeRace = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess = None
                    Barrier =
                        Some(fun root point _context ->
                            async {
                                if point = "finalize-precheck-done" && armFinalizeRace then
                                    armFinalizeRace <- false

                                    // Another process advances the branch through plumbing:
                                    // a dummy commit on top of HEAD, then the ref moves.
                                    let runRaw (arguments: string[]) = async {
                                        let request = {
                                            NodeProcess.ProcessRequest.create "git" arguments with
                                                WorkingDirectory = Some root
                                        }

                                        let! result =
                                            NodeProcess.run request (OperationContext.detached "race-move")

                                        match result with
                                        | Succeeded outcome -> return outcome.Value.StdOut.Trim()
                                        | _ -> return failwith "race plumbing failed"
                                    }

                                    let! tree = runRaw [| "rev-parse"; "HEAD^{tree}" |]
                                    let! head = runRaw [| "rev-parse"; "HEAD" |]

                                    let! dummy =
                                        runRaw [|
                                            "commit-tree"
                                            tree
                                            "-p"
                                            head
                                            "-m"
                                            "race: concurrent advance"
                                        |]

                                    let! _ = runRaw [| "update-ref"; "refs/heads/main"; dummy |]
                                    return ()
                            })
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    // Real conflict: committed local change vs target change.
                    do! advanceTarget root barePath [ "base.txt", "target version\n" ]
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "workspace version\n"

                    let! saveStatus = sessionStatus session

                    let! saveResult =
                        Async.StartAsPromise(
                            session.Core.CreateRevision
                                {
                                    Message = "local conflicting change"
                                    Paths = [| mkPath "base.txt" |]
                                    ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                                }
                                (ctx "conflict-save")
                        )

                    expectValue "local revision" saveResult |> ignore

                    let! updateStatus = sessionStatus session

                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                                (ctx "conflict-update")
                        )

                    let updateFailure =
                        match updateResult with
                        | Failed failure -> failure
                        | PartiallySucceeded(_, failure) -> failure
                        | Succeeded _ -> failwith "Expected the conflicting update to report conflicts."

                    Vitest.expect(updateFailure.Code).toBe ("conflicts_detected")

                    let conflicts = conflictService session
                    let! sessionResult = Async.StartAsPromise(conflicts.GetActiveSession(ctx "conflict-session"))

                    let summary =
                        match expectValue "active session" sessionResult with
                        | Some summary -> summary
                        | None -> failwith "Expected an active conflict session."

                    Vitest.expect(summary.Items.Length).toBe (1)

                    let candidateIds = summary.Items[0].Candidates |> Array.map _.CandidateId
                    Vitest.expect(candidateIds |> Array.contains "workspace").toBe (true)
                    Vitest.expect(candidateIds |> Array.contains "target").toBe (true)
                    Vitest.expect(summary.Items[0].SupportsResolvedContent).toBe (true)

                    // A stale workspace version is rejected before any mutation.
                    let! headBefore = runGitIn workPath [| "rev-parse"; "HEAD" |]

                    let! staleResolve =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = "not-the-current-token"
                                    Path = mkPath "base.txt"
                                    Resolution = PickCandidate "target"
                                }
                                (ctx "stale-resolve")
                        )

                    let staleFailure =
                        match staleResolve with
                        | Failed failure -> failure
                        | _ -> failwith "Expected the stale resolve to fail."

                    Vitest.expect(staleFailure.Category).toEqual (Concurrency)
                    Vitest.expect(staleFailure.Code).toBe ("precondition_failed")

                    Vitest
                        .expect(staleFailure.RecoveryAction |> Option.map _.Code)
                        .toEqual (Some "refresh_conflict_session")

                    let! headAfterStale = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    Vitest.expect(headAfterStale.Trim()).toBe (headBefore.Trim())

                    // A live resolution rotates the handle; replaying the old handle fails.
                    let! resolveStatus = sessionStatus session

                    let! resolveResult =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = resolveStatus.WorkspaceVersion
                                    Path = mkPath "base.txt"
                                    Resolution = PickCandidate "target"
                                }
                                (ctx "live-resolve")
                        )

                    let resolution = expectValue "live resolve" resolveResult
                    Vitest.expect(resolution.RefreshedHandle.Version = summary.Handle.Version).toBe (false)
                    Vitest.expect(resolution.RemainingItems.Length).toBe (0)

                    let! replayStatus = sessionStatus session

                    let! replayResolve =
                        Async.StartAsPromise(
                            conflicts.Resolve
                                {
                                    Handle = summary.Handle
                                    ExpectedWorkspaceVersion = replayStatus.WorkspaceVersion
                                    Path = mkPath "base.txt"
                                    Resolution = PickCandidate "workspace"
                                }
                                (ctx "replay-resolve")
                        )

                    match replayResolve with
                    | Failed failure -> Vitest.expect(failure.Code).toBe ("precondition_failed")
                    | _ -> failwith "Expected the replayed stale handle to be rejected."

                    // Deterministic finalize race: the destination advances after the
                    // pre-check; finalize reports revision evidence and stays live.
                    armFinalizeRace <- true
                    let! raceStatus = sessionStatus session

                    let! racedFinalize =
                        Async.StartAsPromise(
                            conflicts.Finalize
                                {
                                    Handle = resolution.RefreshedHandle
                                    ExpectedWorkspaceVersion = raceStatus.WorkspaceVersion
                                    Message = Some "finalize during race"
                                }
                                (ctx "raced-finalize")
                        )

                    let raceFailure =
                        match racedFinalize with
                        | Failed failure -> failure
                        | PartiallySucceeded(_, failure) -> failure
                        | Succeeded _ -> failwith "Expected the raced finalize to fail structurally."

                    Vitest.expect(raceFailure.Category).toEqual (Concurrency)
                    Vitest.expect(raceFailure.RevisionEvidence.Length >= 2).toBe (true)

                    // Refresh supplies the live handle; the deliberate retry succeeds.
                    let! refreshedSession = Async.StartAsPromise(conflicts.GetActiveSession(ctx "refresh-session"))

                    let liveSummary =
                        match expectValue "refreshed session" refreshedSession with
                        | Some value -> value
                        | None -> failwith "Expected the session to stay live after the raced finalize."

                    let! retryStatus = sessionStatus session

                    let! retryFinalize =
                        Async.StartAsPromise(
                            conflicts.Finalize
                                {
                                    Handle = liveSummary.Handle
                                    ExpectedWorkspaceVersion = retryStatus.WorkspaceVersion
                                    Message = None
                                }
                                (ctx "retry-finalize")
                        )

                    let mergedRevision = expectValue "verified finalize" retryFinalize
                    Vitest.expect(mergedRevision.IsSome).toBe (true)
                    let! commitSubject = runGitIn workPath [| "log"; "-1"; "--format=%s" |]
                    Vitest.expect(commitSubject.Trim()).toBe "Merge online changes"

                    // The session is closed: replaying finalize is rejected without mutation.
                    let! closedSession = Async.StartAsPromise(conflicts.GetActiveSession(ctx "closed-session"))
                    Vitest.expect((expectValue "closed session" closedSession).IsNone).toBe (true)

                    let! finalStatus = sessionStatus session

                    let! replayFinalize =
                        Async.StartAsPromise(
                            conflicts.Finalize
                                {
                                    Handle = liveSummary.Handle
                                    ExpectedWorkspaceVersion = finalStatus.WorkspaceVersion
                                    Message = None
                                }
                                (ctx "replay-finalize")
                        )

                    match replayFinalize with
                    | Failed failure -> Vitest.expect(failure.Code).toBe ("precondition_failed")
                    | _ -> failwith "Expected the closed-handle finalize replay to be rejected."

                    // The picked target content is materialized in the workspace.
                    let! merged = tryReadUtf8FileAsync (join [| workPath; "base.txt" |])
                    Vitest.expect(merged).toEqual (Some "target version\n")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a finalize whose ref update was canceled after it applied closes the session",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable cancelUpdateRef = false

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    if cancelUpdateRef && (request.Arguments |> Array.contains "update-ref") then
                                        let! result = NodeProcess.run request processContext
                                        cancelUpdateRef <- false

                                        return
                                            OperationResult.failed (
                                                OperationFailure.create
                                                    Canceled
                                                    "operation_canceled"
                                                    "simulated cancellation after the ref update"
                                            )
                                    else
                                        return! NodeProcess.run request processContext
                                })
                }

                let! root, workPath, session, conflicts, summary, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    cancelUpdateRef <- true
                    let! status = sessionStatus session
                    let! resolveResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (ctx "late-finalize-cancel-resolve")
                        |> Async.StartAsPromise

                    let resolution = expectValue "late finalize cancellation resolve" resolveResult
                    let! finalizeStatus = sessionStatus session
                    let! finalizeResult =
                        conflicts.Finalize
                            {
                                Handle = resolution.RefreshedHandle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "late finalize cancellation"
                            }
                            (ctx "late-finalize-cancel")
                        |> Async.StartAsPromise

                    let mergedRevision = expectValue "late finalize cancellation" finalizeResult
                    let! head = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    let! parents = runGitIn workPath [| "rev-list"; "--parents"; "-n"; "1"; "HEAD" |]
                    let! mergeHeadPath = runGitIn workPath [| "rev-parse"; "--git-path"; "MERGE_HEAD" |]
                    let! mergeHead = tryReadUtf8FileAsync (join [| workPath; mergeHeadPath.Trim() |])

                    Vitest.expect(mergedRevision |> Option.map RevisionId.value).toEqual (Some(head.Trim()))
                    Vitest.expect(parents.Trim().Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries).Length).toBe (3)
                    Vitest.expect(mergeHead).toEqual (None)

                    let! activeResult = conflicts.GetActiveSession(ctx "late-finalize-cancel-closed") |> Async.StartAsPromise
                    Vitest.expect((expectValue "late finalize cancellation session" activeResult).IsNone).toBe (true)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a finalize over an already committed merge cleans up instead of committing again",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, session, conflicts, summary, _ = createUnmergedConflictFixture ()

                try
                    let! mergeCountBefore = runGitIn workPath [| "rev-list"; "--count"; "--merges"; "HEAD" |]
                    let! _ = runGitIn workPath [| "checkout"; "--theirs"; "base.txt" |]
                    let! _ = runGitIn workPath [| "add"; "base.txt" |]
                    let! tree = runGitIn workPath [| "write-tree" |]
                    let! headBefore = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    let! mergeHeadPath = runGitIn workPath [| "rev-parse"; "--git-path"; "MERGE_HEAD" |]
                    let! mergeHead = tryReadUtf8FileAsync (join [| workPath; mergeHeadPath.Trim() |])

                    let mergeParent =
                        mergeHead
                        |> Option.map (fun value -> value.Trim())
                        |> Option.defaultWith (fun () -> failwith "Expected MERGE_HEAD before the hand-written merge.")

                    let! handCommitted =
                        runGitIn
                            workPath
                            [|
                                "commit-tree"
                                tree.Trim()
                                "-p"
                                headBefore.Trim()
                                "-p"
                                mergeParent
                                "-m"
                                "hand-written conflict merge"
                            |]

                    let! _ = runGitIn workPath [| "update-ref"; "refs/heads/main"; handCommitted.Trim(); headBefore.Trim() |]
                    let! finalizeStatus = sessionStatus session
                    let! finalizeResult =
                        conflicts.Finalize
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = finalizeStatus.WorkspaceVersion
                                Message = Some "cleanup committed merge"
                            }
                            (ctx "already-committed-finalize")
                        |> Async.StartAsPromise

                    let finalized = expectValue "already committed finalize" finalizeResult
                    let! headAfter = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    let! mergeCountAfter = runGitIn workPath [| "rev-list"; "--count"; "--merges"; "HEAD" |]
                    let! mergeHeadAfter = tryReadUtf8FileAsync (join [| workPath; mergeHeadPath.Trim() |])

                    Vitest.expect(finalized |> Option.map RevisionId.value).toEqual (Some(handCommitted.Trim()))
                    Vitest.expect(headAfter.Trim()).toBe (handCommitted.Trim())
                    Vitest.expect(Int32.Parse(mergeCountAfter.Trim())).toBe (Int32.Parse(mergeCountBefore.Trim()) + 1)
                    Vitest.expect(mergeHeadAfter).toEqual (None)

                    let! activeResult = conflicts.GetActiveSession(ctx "already-committed-closed") |> Async.StartAsPromise
                    Vitest.expect((expectValue "already committed session" activeResult).IsNone).toBe (true)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a cancel whose abort was canceled after it ran closes the session",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable cancelAbort = false

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    if cancelAbort && (request.Arguments |> Array.contains "--abort") then
                                        let! result = NodeProcess.run request processContext
                                        cancelAbort <- false

                                        return
                                            OperationResult.failed (
                                                OperationFailure.create
                                                    Canceled
                                                    "operation_canceled"
                                                    "simulated cancellation after merge abort"
                                            )
                                    else
                                        return! NodeProcess.run request processContext
                                })
                }

                let! root, workPath, session, conflicts, summary, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    cancelAbort <- true
                    let! status = sessionStatus session
                    let! cancelResult =
                        conflicts.Cancel
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "late-cancel-abort")
                        |> Async.StartAsPromise

                    expectValue "late cancel abort" cancelResult |> ignore
                    let! mergeHeadPath = runGitIn workPath [| "rev-parse"; "--git-path"; "MERGE_HEAD" |]
                    let! mergeHead = tryReadUtf8FileAsync (join [| workPath; mergeHeadPath.Trim() |])
                    Vitest.expect(mergeHead).toEqual (None)

                    let! activeResult = conflicts.GetActiveSession(ctx "late-cancel-abort-closed") |> Async.StartAsPromise
                    Vitest.expect((expectValue "late cancel abort session" activeResult).IsNone).toBe (true)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a resolve that fails after the write reports the state as changed",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable failAdd = false

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    if failAdd && request.Arguments |> Array.contains "add" then
                                        return
                                            OperationResult.failed (
                                                OperationFailure.create
                                                    ProviderError
                                                    "git_failure"
                                                    "simulated staging failure"
                                            )
                                    else
                                        return! NodeProcess.run request processContext
                                })
                }

                let! root, workPath, session, conflicts, summary, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    failAdd <- true
                    let! status = sessionStatus session
                    let! resolveResult =
                        conflicts.Resolve
                            {
                                Handle = summary.Handle
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                Path = mkPath "base.txt"
                                Resolution = PickCandidate "target"
                            }
                            (ctx "resolve-stage-failure")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "resolve stage failure" resolveResult
                    Vitest.expect(failure.StateChanged).toBe (true)
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (
                        Some ConflictRecovery.RefreshConflictSession
                    )

                    let! content = tryReadUtf8FileAsync (join [| workPath; "base.txt" |])
                    Vitest.expect(content).toEqual (Some "target version\n")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "the canceled-merge recovery leaves a foreign merge alone",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable foreignMergeWritten = false
                let mutable canceledMerge = false
                let mutable abortRan = false
                let mutable foreignRevision = ""

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    if request.Arguments |> Array.contains "--abort" then
                                        abortRan <- true
                                        return! NodeProcess.run request processContext
                                    elif
                                        foreignMergeWritten
                                        && not canceledMerge
                                        && (request.Arguments |> Array.contains "merge")
                                    then
                                        canceledMerge <- true

                                        return
                                            OperationResult.failed (
                                                OperationFailure.create
                                                    Canceled
                                                    "operation_canceled"
                                                    "simulated canceled merge with foreign state"
                                            )
                                    else
                                        return! NodeProcess.run request processContext
                                })
                        Barrier =
                            Some(fun workspaceRoot point _ ->
                                async {
                                    if point = "update-merge" && not foreignMergeWritten then
                                        foreignMergeWritten <- true
                                        let! mergeHeadPath =
                                            Async.AwaitPromise(
                                                runGitIn workspaceRoot [| "rev-parse"; "--git-path"; "MERGE_HEAD" |]
                                            )
                                        do!
                                            Async.AwaitPromise(
                                                writeUtf8FileAsync
                                                    (join [| workspaceRoot; mergeHeadPath.Trim() |])
                                                    (foreignRevision + "\n")
                                            )
                                })
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    let! startHead = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    foreignRevision <- startHead.Trim()
                    do! advanceTarget root barePath [ "foreign-merge-recovery.txt", "target content\n" ]
                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                            (ctx "foreign-merge-recovery")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "foreign merge recovery" updateResult
                    Vitest.expect(failure.StateChanged).toBe (true)
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "inspect_workspace")
                    Vitest.expect(abortRan).toBe (false)

                    let! mergeHeadPath = runGitIn workPath [| "rev-parse"; "--git-path"; "MERGE_HEAD" |]
                    let! mergeHead = tryReadUtf8FileAsync (join [| workPath; mergeHeadPath.Trim() |])
                    Vitest.expect(mergeHead |> Option.map (fun value -> value.Trim())).toEqual (Some foreignRevision)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "concurrent conflict mutations run one at a time",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable holdResolution = false

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        Barrier =
                            Some(fun _ point _ ->
                                async {
                                    if holdResolution && point = "conflict-content-validated" then
                                        do! Async.Sleep 50
                                })
                }

                let! root, _, session, conflicts, summary, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    holdResolution <- true
                    let! status = sessionStatus session
                    let request = {
                        Handle = summary.Handle
                        ExpectedWorkspaceVersion = status.WorkspaceVersion
                        Path = mkPath "base.txt"
                        Resolution = PickCandidate "target"
                    }

                    let first = conflicts.Resolve request (ctx "concurrent-resolve-one") |> Async.StartAsPromise
                    let second = conflicts.Resolve request (ctx "concurrent-resolve-two") |> Async.StartAsPromise
                    let! firstResult = first
                    let! secondResult = second
                    let results = [| firstResult; secondResult |]
                    let successes = results |> Array.filter (function | Succeeded _ -> true | _ -> false)
                    let failures =
                        results
                        |> Array.choose (function
                            | Failed failure -> Some failure
                            | PartiallySucceeded(_, failure) -> Some failure
                            | Succeeded _ -> None)

                    Vitest.expect(successes.Length).toBe (1)
                    Vitest.expect(failures.Length).toBe (1)
                    Vitest.expect(failures[0].Category).toEqual (Concurrency)
                    Vitest.expect(failures[0].Code).toBe ("precondition_failed")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "cancellation stops refresh update publish and clone with structured canceled results",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // Deterministic interruption: the transfer-start barrier cancels the
                // armed operation's context before its transfer runs.
                let mutable armCancel: OperationCancellation.Source option = None

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess = None
                    Barrier =
                        Some(fun _root point _context ->
                            async {
                                if point = "transfer-start" then
                                    match armCancel with
                                    | Some source ->
                                        armCancel <- None
                                        source.Cancel()
                                    | None -> ()
                            })
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    let expectCanceled (operationName: string) (result: OperationResult<'T>) =
                        match result with
                        | Failed failure ->
                            Vitest.expect(failure.Category).toEqual (Canceled)
                            Vitest.expect(failure.Code).toBe ("operation_canceled")
                            Vitest.expect(failure.StateChanged).toBe (false)
                        | Succeeded _
                        | PartiallySucceeded _ -> failwith $"Expected {operationName} to report a canceled failure."

                    let cancelableContext (name: string) =
                        let source = OperationCancellation.Source()
                        armCancel <- Some source
                        OperationContext.create name source.Cancellation ignore

                    // Refresh.
                    do! advanceTarget root barePath [ "refresh-cancel.txt", "content\n" ]
                    let! refreshResult = Async.StartAsPromise((syncService session).Refresh(cancelableContext "c-refresh"))
                    expectCanceled "canceled refresh" refreshResult

                    // Update: the target change never reaches the workspace.
                    let! updateStatus = sessionStatus session

                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                                (cancelableContext "c-update")
                        )

                    expectCanceled "canceled update" updateResult
                    let! updateFile = tryReadUtf8FileAsync (join [| workPath; "refresh-cancel.txt" |])
                    Vitest.expect(updateFile).toEqual (None)

                    // Publish: the bare target head never moves.
                    do! writeUtf8FileAsync (join [| workPath; "publish-cancel.txt" |]) "content\n"
                    let! saveStatus = sessionStatus session

                    let! saveResult =
                        Async.StartAsPromise(
                            session.Core.CreateRevision
                                {
                                    Message = "revision awaiting canceled publish"
                                    Paths = [| mkPath "publish-cancel.txt" |]
                                    ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                                }
                                (ctx "c-save")
                        )

                    expectValue "revision before canceled publish" saveResult |> ignore

                    let! bareHeadBefore = runGitIn barePath [| "rev-parse"; "main" |]
                    let! publishStatus = sessionStatus session

                    let! publishResult =
                        Async.StartAsPromise(
                            (syncService session).Publish
                                {
                                    ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                    ExpectedTargetRevision = None
                                }
                                (cancelableContext "c-publish")
                        )

                    expectCanceled "canceled publish" publishResult
                    let! bareHeadAfter = runGitIn barePath [| "rev-parse"; "main" |]
                    Vitest.expect(bareHeadAfter.Trim()).toBe (bareHeadBefore.Trim())

                    // Clone: cancellation before the transfer leaves no target directory.
                    let cloneSource = OperationCancellation.Source()
                    cloneSource.Cancel()
                    let clonePath = join [| root; "canceled-clone" |]
                    let factory = GitWorkspaceSession.createFactory hooks

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
                                    MaterializeAllObjects = false
                                }
                                (OperationContext.create "c-clone" cloneSource.Cancellation ignore)
                        )

                    expectCanceled "canceled clone" cloneResult

                    let! cloneExists = tryReadUtf8FileAsync (join [| clonePath; "base.txt" |])
                    Vitest.expect(cloneExists).toEqual (None)

                    // After all cancellations the operations still work: incorporate
                    // the target's earlier advance, then publish cleanly.
                    let! retryUpdateStatus = sessionStatus session

                    let! retryUpdate =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = retryUpdateStatus.WorkspaceVersion }
                                (ctx "retry-update")
                        )

                    expectValue "update after cancellations" retryUpdate |> ignore

                    let! retryStatus = sessionStatus session

                    let! retryPublish =
                        Async.StartAsPromise(
                            (syncService session).Publish
                                {
                                    ExpectedWorkspaceVersion = retryStatus.WorkspaceVersion
                                    ExpectedTargetRevision = None
                                }
                                (ctx "retry-publish")
                        )

                    expectValue "publish after cancellations" retryPublish |> ignore

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "canceled clone removes the target directory it created",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if request.Arguments |> Array.contains "clone" then
                                    let targetPath = request.Arguments[request.Arguments.Length - 1]
                                    do! ensureDirectoryAsync targetPath |> Async.AwaitPromise
                                    do!
                                        writeUtf8FileAsync (join [| targetPath; "partial.txt" |]) "partial\n"
                                        |> Async.AwaitPromise

                                    return
                                        OperationResult.failed (
                                            OperationFailure.create
                                                Canceled
                                                "operation_canceled"
                                                "simulated kill during clone"
                                        )
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier = None
                }

                let! root, _, barePath, _ = createSyncFixture hooks
                let targetPath = join [| root; "partial-clone" |]

                try
                    let factory = GitWorkspaceSession.createFactory hooks

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
                                    TargetPath = targetPath
                                    TargetRef = None
                                    MaterializeAllObjects = false
                                }
                                (ctx "clone-residue")
                        )

                    let failure = expectProviderFailure "canceled clone residue" cloneResult
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    Vitest.expect(failure.StateChanged).toBe (false)
                    Vitest.expect(NodeFileSystem.existsSync targetPath).toBe (false)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "failed clone into an existing empty directory leaves it empty",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if request.Arguments |> Array.contains "clone" then
                                    let targetPath = request.Arguments[request.Arguments.Length - 1]
                                    do! ensureDirectoryAsync targetPath |> Async.AwaitPromise
                                    do!
                                        writeUtf8FileAsync (join [| targetPath; "partial.txt" |]) "partial\n"
                                        |> Async.AwaitPromise

                                    // A real failing clone exits nonzero, so the provider, not the
                                    // test, produces the failure code.
                                    let output: NodeProcess.ProcessOutput = {
                                        ExitCode = 128
                                        StdOut = ""
                                        StdErr = "fatal: simulated clone failure"
                                    }

                                    return OperationResult.succeeded output
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier = None
                }

                let! root, _, barePath, _ = createSyncFixture hooks
                let targetPath = join [| root; "empty-target" |]

                try
                    do! ensureDirectoryAsync targetPath
                    let factory = GitWorkspaceSession.createFactory hooks

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
                                    TargetPath = targetPath
                                    TargetRef = None
                                    MaterializeAllObjects = false
                                }
                                (ctx "failed-clone-residue")
                        )

                    let failure = expectProviderFailure "failed clone residue" cloneResult
                    Vitest.expect(failure.Code).toBe ("clone_failed")
                    Vitest.expect(failure.StateChanged).toBe (false)
                    Vitest.expect(NodeFileSystem.existsSync targetPath).toBe (true)
                    Vitest.expect((NodeFileSystem.statSync targetPath).isDirectory()).toBe (true)
                    Vitest.expect(NodeFileSystem.readdirSync targetPath).toEqual [||]

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        // The hook stands in for a true merge killed after it wrote MERGE_HEAD and its
        // index lock. Nothing here can prove who owns a lock, so the recovery reports
        // it and touches nothing else.
        Vitest.test (
            "canceled update reports a stale index lock and leaves the repository alone",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable targetHash = ""
                let mutable workspacePath = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if
                                    request.Arguments |> Array.contains "merge"
                                    && not (request.Arguments |> Array.contains "--abort")
                                then
                                    do!
                                        writeUtf8FileAsync
                                            (join [| workspacePath; ".git"; "MERGE_HEAD" |])
                                            (targetHash + "\n")
                                        |> Async.AwaitPromise

                                    do!
                                        writeUtf8FileAsync (join [| workspacePath; ".git"; "index.lock" |]) ""
                                        |> Async.AwaitPromise

                                    return
                                        OperationResult.failed (
                                            OperationFailure.create
                                                Canceled
                                                "operation_canceled"
                                                "simulated kill during merge"
                                        )
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks
                workspacePath <- workPath

                try
                    do! advanceTarget root barePath [ "merge-residue.txt", "content\n" ]
                    let! observedTargetHash = runGitIn barePath [| "rev-parse"; "main" |]
                    targetHash <- observedTargetHash.Trim()

                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                                (ctx "cancel-update-lock")
                        )

                    let failure = expectProviderFailure "canceled update with lock" updateResult
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    Vitest.expect(failure.StateChanged).toBe (true)

                    Vitest
                        .expect(failure.RecoveryAction |> Option.map (fun action -> action.Code))
                        .toEqual (Some "remove_index_lock")

                    let! mergeHead = tryReadUtf8FileAsync (join [| workPath; ".git"; "MERGE_HEAD" |])
                    let! indexLock = tryReadUtf8FileAsync (join [| workPath; ".git"; "index.lock" |])
                    Vitest.expect(mergeHead).toEqual (Some(targetHash + "\n"))
                    Vitest.expect(indexLock).toEqual (Some "")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        // The same kill, but the lock is gone (git released it before dying). The
        // recovery then aborts the merge state this merge created.
        Vitest.test (
            "canceled update aborts a half-finished merge",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable targetHash = ""
                let mutable workspacePath = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if
                                    request.Arguments |> Array.contains "merge"
                                    && not (request.Arguments |> Array.contains "--abort")
                                then
                                    // A true merge stages the incoming file before it writes
                                    // MERGE_HEAD. The abort has to undo that staging.
                                    do!
                                        writeUtf8FileAsync (join [| workspacePath; "merge-residue.txt" |]) "content\n"
                                        |> Async.AwaitPromise

                                    let! _ =
                                        runGitIn workspacePath [| "add"; "--"; "merge-residue.txt" |] |> Async.AwaitPromise

                                    do!
                                        writeUtf8FileAsync
                                            (join [| workspacePath; ".git"; "MERGE_HEAD" |])
                                            (targetHash + "\n")
                                        |> Async.AwaitPromise

                                    return
                                        OperationResult.failed (
                                            OperationFailure.create
                                                Canceled
                                                "operation_canceled"
                                                "simulated kill during merge"
                                        )
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks
                workspacePath <- workPath

                try
                    do! advanceTarget root barePath [ "merge-residue.txt", "content\n" ]
                    let! observedTargetHash = runGitIn barePath [| "rev-parse"; "main" |]
                    targetHash <- observedTargetHash.Trim()

                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                                (ctx "cancel-update-merge")
                        )

                    let failure = expectProviderFailure "canceled update merge residue" updateResult
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    Vitest.expect(failure.StateChanged).toBe (false)
                    Vitest.expect(failure.RecoveryAction).toEqual (None)

                    let! mergeHead = tryReadUtf8FileAsync (join [| workPath; ".git"; "MERGE_HEAD" |])
                    let! mergeResidue = tryReadUtf8FileAsync (join [| workPath; "merge-residue.txt" |])
                    let! status = runGitIn workPath [| "status"; "--porcelain" |]
                    let! afterUpdate = sessionStatus session

                    Vitest.expect(mergeHead).toEqual (None)
                    Vitest.expect(mergeResidue).toEqual (None)
                    Vitest.expect(status.Trim()).toBe ("")
                    Vitest.expect(afterUpdate.ActiveConflictSession).toEqual (None)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        // A fast-forward writes no MERGE_HEAD. The hook stands in for a merge process
        // killed after it rewrote one file. The provider reports the file and leaves it,
        // because it cannot prove the file held nothing of the user's.
        Vitest.test (
            "canceled fast-forward merge reports the file git may have rewritten",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable workspacePath = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if request.Arguments |> Array.contains "merge" then
                                    do!
                                        writeUtf8FileAsync (join [| workspacePath; "ff-residue.txt" |]) "content\n"
                                        |> Async.AwaitPromise

                                    return
                                        OperationResult.failed (
                                            OperationFailure.create
                                                Canceled
                                                "operation_canceled"
                                                "simulated kill during fast-forward"
                                        )
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks
                workspacePath <- workPath

                try
                    do! advanceTarget root barePath [ "ff-residue.txt", "content\n" ]
                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                                (ctx "cancel-ff-merge")
                        )

                    let failure = expectProviderFailure "canceled fast-forward" updateResult
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    Vitest.expect(failure.StateChanged).toBe (true)

                    Vitest
                        .expect(failure.RecoveryAction |> Option.map (fun action -> action.Code))
                        .toEqual (Some "restore_workspace")

                    Vitest.expect(failure.AffectedPaths).toEqual [| "ff-residue.txt" |]

                    let! residue = tryReadUtf8FileAsync (join [| workPath; "ff-residue.txt" |])
                    let! status = runGitIn workPath [| "status"; "--porcelain" |]
                    Vitest.expect(residue).toEqual (Some "content\n")
                    Vitest.expect(status.Trim()).toBe ("?? ff-residue.txt")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        // Same kill against a file that already exists in HEAD. The rewritten content
        // stays in place and the path is reported.
        Vitest.test (
            "canceled fast-forward merge reports a rewritten tracked file",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable workspacePath = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if request.Arguments |> Array.contains "merge" then
                                    do!
                                        writeUtf8FileAsync (join [| workspacePath; "base.txt" |]) "rewritten base\n"
                                        |> Async.AwaitPromise

                                    return
                                        OperationResult.failed (
                                            OperationFailure.create
                                                Canceled
                                                "operation_canceled"
                                                "simulated kill during fast-forward"
                                        )
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks
                workspacePath <- workPath

                try
                    do! advanceTarget root barePath [ "base.txt", "rewritten base\n" ]
                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                                (ctx "cancel-ff-tracked")
                        )

                    let failure = expectProviderFailure "canceled tracked fast-forward" updateResult
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.StateChanged).toBe (true)

                    Vitest
                        .expect(failure.RecoveryAction |> Option.map (fun action -> action.Code))
                        .toEqual (Some "restore_workspace")

                    Vitest.expect(failure.AffectedPaths).toEqual [| "base.txt" |]

                    let! baseContent = tryReadUtf8FileAsync (join [| workPath; "base.txt" |])
                    let! status = runGitIn workPath [| "status"; "--porcelain" |]
                    Vitest.expect(baseContent).toEqual (Some "rewritten base\n")
                    Vitest.expect(status.TrimEnd()).toBe (" M base.txt")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        // The hook lets the real merge finish and only then reports the cancellation,
        // which is what a kill signal that arrives too late looks like to the caller.
        Vitest.test (
            "a merge that finished before the cancellation landed reports refresh_workspace",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if request.Arguments |> Array.contains "merge" then
                                    let! _ = NodeProcess.run request context

                                    return
                                        OperationResult.failed (
                                            OperationFailure.create
                                                Canceled
                                                "operation_canceled"
                                                "simulated late cancellation"
                                        )
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    do! advanceTarget root barePath [ "late-cancel.txt", "content\n" ]
                    let! targetHash = runGitIn barePath [| "rev-parse"; "main" |]
                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                                (ctx "late-cancel-merge")
                        )

                    let failure = expectProviderFailure "late canceled merge" updateResult
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    Vitest.expect(failure.StateChanged).toBe (true)

                    Vitest
                        .expect(failure.RecoveryAction |> Option.map (fun action -> action.Code))
                        .toEqual (Some "refresh_workspace")

                    let! landed = tryReadUtf8FileAsync (join [| workPath; "late-cancel.txt" |])
                    let! headAfter = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    Vitest.expect(landed).toEqual (Some "content\n")
                    Vitest.expect(headAfter.Trim()).toBe (targetHash.Trim())

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a successful merge with a failed post-merge inspection reports refresh_workspace",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable failInspection = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                if request.Arguments |> Array.contains "merge" then
                                    let! result = NodeProcess.run request processContext
                                    failInspection <- true
                                    return result
                                elif
                                    failInspection
                                    && request.Arguments
                                       = [| "rev-parse"; "--symbolic-full-name"; "@{upstream}" |]
                                then
                                    return
                                        OperationResult.failed (
                                            OperationFailure.create
                                                ProviderError
                                                "git_failure"
                                                "simulated post-merge inspection failure"
                                        )
                                else
                                    return! NodeProcess.run request processContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    do! advanceTarget root barePath [ "post-merge-inspection.txt", "content\n" ]
                    let! targetHash = runGitIn barePath [| "rev-parse"; "main" |]
                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                            (ctx "post-merge-inspection-failure")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "post-merge inspection failure" updateResult
                    Vitest.expect(failure.StateChanged).toBe (true)
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "refresh_workspace")

                    let! headAfter = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    Vitest.expect(headAfter.Trim()).toBe (targetHash.Trim())

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a delayed post-merge inspection reports inspection_timeout",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable mergeFinished = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                if request.Arguments |> Array.contains "merge" then
                                    let! result = NodeProcess.run request processContext
                                    mergeFinished <- true
                                    return result
                                elif
                                    mergeFinished
                                    && request.Arguments
                                       = [| "rev-parse"; "--symbolic-full-name"; "@{upstream}" |]
                                then
                                    do! Async.Sleep 10
                                    return! NodeProcess.run request processContext
                                else
                                    return! NodeProcess.run request processContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    GitWorkspaceSession.GitSessionHooks.postMergeInspectionTimeoutOverride <- Some 1
                    do! advanceTarget root barePath [ "inspection-timeout.txt", "content\n" ]
                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                            (ctx "post-merge-inspection-timeout")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "post-merge inspection timeout" updateResult
                    Vitest.expect(failure.Category).toEqual (Timeout)
                    Vitest.expect(failure.Code).toBe ("inspection_timeout")
                    Vitest.expect(failure.StateChanged).toBe (true)
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "refresh_workspace")

                    GitWorkspaceSession.GitSessionHooks.postMergeInspectionTimeoutOverride <- None
                    do! removeDirectoryAsync root
                with error ->
                    GitWorkspaceSession.GitSessionHooks.postMergeInspectionTimeoutOverride <- None
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a cancellation after a successful merge returns truthful synchronization state",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let cancellation = OperationCancellation.Source()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                let! result = NodeProcess.run request processContext

                                if request.Arguments |> Array.contains "merge" then
                                    cancellation.Cancel()

                                return result
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    do! advanceTarget root barePath [ "late-success.txt", "content\n" ]
                    let! targetHash = runGitIn barePath [| "rev-parse"; "main" |]
                    let! beforeUpdate = sessionStatus session
                    let updateContext = OperationContext.create "late-successful-merge" cancellation.Cancellation ignore

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                            updateContext
                        |> Async.StartAsPromise

                    match updateResult with
                    | Succeeded outcome ->
                        Vitest.expect(outcome.Value.WorkspaceRevision |> Option.map RevisionId.value).toEqual (
                            Some(targetHash.Trim())
                        )
                        Vitest.expect(outcome.Value.Relationship).toEqual (UpToDate)
                        Vitest.expect(outcome.Value.Relationship).not.toEqual (NoTarget)
                    | Failed failure ->
                        failwith $"Expected the late-canceled merge to succeed, received {failure.Category}/{failure.Code}."
                    | PartiallySucceeded(_, failure) ->
                        failwith $"Expected the late-canceled merge to succeed, received partial {failure.Code}."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a cancellation after a conflicting merge returns the conflict session",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let cancellation = OperationCancellation.Source()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                let! result = NodeProcess.run request processContext

                                if request.Arguments |> Array.contains "merge" then
                                    cancellation.Cancel()

                                return result
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    do! advanceTarget root barePath [ "base.txt", "target version\n" ]
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "workspace version\n"

                    let! saveStatus = sessionStatus session

                    let! saveResult =
                        session.Core.CreateRevision
                            {
                                Message = "local conflicting change after cancellation"
                                Paths = [| mkPath "base.txt" |]
                                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                            }
                            (ctx "conflict-after-cancellation-save")
                        |> Async.StartAsPromise

                    expectValue "conflict after cancellation local revision" saveResult |> ignore
                    let! updateStatus = sessionStatus session

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = updateStatus.WorkspaceVersion }
                            (OperationContext.create "conflict-after-cancellation-update" cancellation.Cancellation ignore)
                        |> Async.StartAsPromise

                    let outcome, failure =
                        match updateResult with
                        | PartiallySucceeded(outcome, failure) -> outcome, failure
                        | Failed failure -> failwith $"Expected a conflict partial, received {failure.Code}."
                        | Succeeded _ -> failwith "Expected the conflicting update to return a partial result."

                    Vitest.expect(outcome.Value.Relationship).toEqual (Diverged)
                    Vitest.expect(failure.Code).toBe ("conflicts_detected")
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "resolve_conflict_session")

                    let! mergeHeadPath = runGitIn workPath [| "rev-parse"; "--git-path"; "MERGE_HEAD" |]
                    let! mergeHead = tryReadUtf8FileAsync (join [| workPath; mergeHeadPath.Trim() |])
                    Vitest.expect(mergeHead.IsSome).toBe (true)

                    let conflicts = conflictService session
                    let! activeResult =
                        conflicts.GetActiveSession(OperationContext.detached "conflict-after-cancellation-session")
                        |> Async.StartAsPromise
                    let active = expectValue "conflict after cancellation session" activeResult
                    Vitest.expect(active.IsSome).toBe (true)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a cancellation after fetch returns a canceled refresh failure",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let cancellation = OperationCancellation.Source()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                let! result = NodeProcess.run request processContext

                                if request.Arguments |> Array.contains "fetch" then
                                    cancellation.Cancel()

                                return result
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    do! advanceTarget root barePath [ "refresh-cancel-after-fetch.txt", "content\n" ]
                    let refreshContext = OperationContext.create "refresh-after-fetch" cancellation.Cancellation ignore

                    let! refreshResult =
                        (syncService session).Refresh refreshContext
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "canceled refresh" refreshResult
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.Code).toBe ("operation_canceled")
                    Vitest.expect(failure.Message).not.toContain ("NoTarget")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        // git writes the index before it moves HEAD, so a kill in that window leaves the
        // target's new file staged. It is reported like any other rewritten path.
        Vitest.test (
            "canceled fast-forward merge reports a file git had already staged",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable workspacePath = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if request.Arguments |> Array.contains "merge" then
                                    do!
                                        writeUtf8FileAsync (join [| workspacePath; "staged-residue.txt" |]) "content\n"
                                        |> Async.AwaitPromise

                                    let! _ = runGitIn workspacePath [| "add"; "--"; "staged-residue.txt" |] |> Async.AwaitPromise

                                    return
                                        OperationResult.failed (
                                            OperationFailure.create
                                                Canceled
                                                "operation_canceled"
                                                "simulated kill after the index write"
                                        )
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks
                workspacePath <- workPath

                try
                    do! advanceTarget root barePath [ "staged-residue.txt", "content\n" ]
                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                                (ctx "cancel-ff-staged")
                        )

                    let failure = expectProviderFailure "canceled staged fast-forward" updateResult
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.StateChanged).toBe (true)

                    Vitest
                        .expect(failure.RecoveryAction |> Option.map (fun action -> action.Code))
                        .toEqual (Some "restore_workspace")

                    Vitest.expect(failure.AffectedPaths).toEqual [| "staged-residue.txt" |]

                    let! residue = tryReadUtf8FileAsync (join [| workPath; "staged-residue.txt" |])
                    let! status = runGitIn workPath [| "status"; "--porcelain" |]
                    Vitest.expect(residue).toEqual (Some "content\n")
                    Vitest.expect(status.Trim()).toBe ("A  staged-residue.txt")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        // The user edited base.txt before the update and the target changes the same
        // file. git would refuse that fast-forward, and a kill before the refusal writes
        // nothing. An unrelated scratch file appearing meanwhile moves the workspace
        // version, so the recovery runs. It must leave the user's edit alone, and because
        // base.txt was already changed beforehand it is not a rewritten path, so the
        // report says that nothing attributable to the update changed.
        Vitest.test (
            "canceled merge recovery never restores paths the user had changed beforehand",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable workspacePath = ""

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request context ->
                            async {
                                if request.Arguments |> Array.contains "merge" then
                                    do!
                                        writeUtf8FileAsync (join [| workspacePath; "scratch.txt" |]) "editor scratch\n"
                                        |> Async.AwaitPromise

                                    return
                                        OperationResult.failed (
                                            OperationFailure.create
                                                Canceled
                                                "operation_canceled"
                                                "simulated kill before the overwrite check"
                                        )
                                else
                                    return! NodeProcess.run request context
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks
                workspacePath <- workPath

                try
                    do! advanceTarget root barePath [ "base.txt", "target base\n" ]
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "local edit\n"
                    let! beforeUpdate = sessionStatus session

                    let! updateResult =
                        Async.StartAsPromise(
                            (syncService session).Update
                                { ExpectedWorkspaceVersion = beforeUpdate.WorkspaceVersion }
                                (ctx "cancel-protected")
                        )

                    let failure = expectProviderFailure "canceled protected merge" updateResult
                    Vitest.expect(failure.Category).toEqual (Canceled)
                    Vitest.expect(failure.StateChanged).toBe (true)

                    Vitest
                        .expect(failure.RecoveryAction |> Option.map (fun action -> action.Code))
                        .toEqual (Some "inspect_workspace")

                    Vitest.expect(failure.AffectedPaths).toEqual [||]

                    let! baseContent = tryReadUtf8FileAsync (join [| workPath; "base.txt" |])
                    let! scratch = tryReadUtf8FileAsync (join [| workPath; "scratch.txt" |])
                    Vitest.expect(baseContent).toEqual (Some "local edit\n")
                    Vitest.expect(scratch).toEqual (Some "editor scratch\n")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "preview includes dirty workspace and requires Git 2.38",
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
                    Vitest.expect(preview.PredictedConflictPaths).toEqual (Some [||])
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

        Vitest.test (
            "diverged preview predicts committed conflicts and omits a clean target change",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session =
                    createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "local committed change\n"
                    let! beforeRevision = sessionStatus session

                    let! revisionResult =
                        Async.StartAsPromise(
                            session.Core.CreateRevision
                                {
                                    Message = "save local conflict"
                                    Paths = [| mkPath "base.txt" |]
                                    ExpectedWorkspaceVersion = beforeRevision.WorkspaceVersion
                                }
                                (ctx "preview-local-conflict")
                        )

                    expectValue "create local conflicting revision" revisionResult |> ignore

                    do!
                        advanceTarget root barePath [
                            "base.txt", "target conflicting change\n"
                            "target-clean.txt", "clean target change\n"
                        ]

                    let! previewResult =
                        Async.StartAsPromise((syncService session).PreviewUpdate(ctx "preview-committed-conflict"))

                    let preview = expectValue "preview committed conflict" previewResult
                    let overlapping = preview.OverlappingPaths |> Array.map RepositoryPath.value
                    Vitest.expect(overlapping).toEqual [||]

                    let predictedConflicts =
                        preview.PredictedConflictPaths
                        |> Option.defaultWith (fun () -> failwith "Expected Git to predict committed conflict paths.")
                        |> Array.map RepositoryPath.value

                    Vitest.expect(predictedConflicts).toEqual [| "base.txt" |]
                    Vitest.expect(preview.WouldCreateConflictSession).toBe (true)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

)

Vitest.describe (
    "Git LFS transfer ordering",
    fun () ->
        Vitest.test (
            "update skips implicit smudge and reports failed explicit hydration as partial success",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                observed.Add request

                                if
                                    request.Arguments |> Array.contains "lfs"
                                    && request.Arguments |> Array.contains "pull"
                                then
                                    return
                                        OperationResult.succeeded {
                                            NodeProcess.ExitCode = 1
                                            StdOut = ""
                                            StdErr = "HTTP 401 Unauthorized during LFS hydration"
                                        }
                                else
                                    return! NodeProcess.run request processContext
                            })
                    Barrier = None
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    let! _ =
                        runGitIn
                            workPath
                            [|
                                "config"
                                "--local"
                                "versioncontrolservice.lfs.materializelargeobjects"
                                "true"
                            |]

                    do! advanceTarget root barePath [ "target-lfs.bin", "target content\n" ]
                    let! status = sessionStatus session

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "lfs-update-hydration")
                        |> Async.StartAsPromise

                    match updateResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(failure.Category).toEqual Authentication
                        Vitest.expect(failure.Code).toBe "hydration_failed"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.AffectedPaths).toContain "target-lfs.bin"
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "retry_materialization")
                        Vitest.expect(outcome.AffectedPaths).toContain "target-lfs.bin"
                    | Failed failure ->
                        failwith $"Expected partial hydration result, received {failure.Code}."
                    | Succeeded _ -> failwith "Expected explicit hydration failure after the successful Git update."

                    let mergeRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "merge")

                    Vitest
                        .expect(mergeRequest.Environment |> Array.contains ("GIT_LFS_SKIP_SMUDGE", "1"))
                        .toBe true

                    let hydrationIndex =
                        observed
                        |> Seq.findIndex (fun request ->
                            request.Arguments |> Array.contains "lfs"
                            && request.Arguments |> Array.contains "pull")

                    let mergeIndex =
                        observed
                        |> Seq.findIndex (fun request -> request.Arguments |> Array.contains "merge")

                    Vitest.expect(hydrationIndex > mergeIndex).toBe true
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "clone skips implicit smudge before explicit authenticated hydration",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                observed.Add request

                                if
                                    request.Arguments |> Array.contains "lfs"
                                    && request.Arguments |> Array.contains "pull"
                                then
                                    return
                                        OperationResult.succeeded {
                                            NodeProcess.ExitCode = 1
                                            StdOut = ""
                                            StdErr = "injected clone hydration failure"
                                        }
                                else
                                    return! NodeProcess.run request processContext
                            })
                    Barrier = None
                }

                let! root, _, barePath, _ = createSyncFixture hooks
                let clonePath = join [| root; "hydrated-clone" |]

                try
                    let factory = GitWorkspaceSession.createFactory hooks

                    let! cloneResult =
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
                            (ctx "lfs-clone-hydration")
                        |> Async.StartAsPromise

                    match cloneResult with
                    | PartiallySucceeded(_, failure) ->
                        Vitest.expect(failure.Code).toBe "hydration_failed"
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "retry_materialization")
                    | _ -> failwith "Expected clone hydration failure to preserve the successful clone as partial success."

                    let cloneRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "clone")

                    Vitest
                        .expect(cloneRequest.Environment |> Array.contains ("GIT_LFS_SKIP_SMUDGE", "1"))
                        .toBe true

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "publish uploads selected-ref LFS objects before a hook-disabled ref push",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let observed = ResizeArray<NodeProcess.ProcessRequest>()

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunBytesProcess = None
                    RunProcess =
                        Some(fun request processContext ->
                            async {
                                observed.Add request
                                return! NodeProcess.run request processContext
                            })
                    Barrier = None
                }

                let! root, workPath, _, session = createSyncFixture hooks

                try
                    let barePath = join [| root; "origin.git" |]
                    let! _ = runGitIn workPath [| "checkout"; "-b"; "unrelated-lfs-history" |]

                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "unrelated-large.bin" |])
                            (String.replicate (1024 * 1024) "u")

                    let! unrelatedStatus = sessionStatus session

                    let! unrelatedRevision =
                        session.Core.CreateRevision
                            {
                                Message = "test: unrelated LFS history"
                                Paths = [| mkPath "unrelated-large.bin" |]
                                ExpectedWorkspaceVersion = unrelatedStatus.WorkspaceVersion
                            }
                            (ctx "unrelated-lfs-revision")
                        |> Async.StartAsPromise

                    expectValue "unrelated LFS revision" unrelatedRevision |> ignore

                    let! unrelatedPointer = runGitIn workPath [| "show"; "HEAD:unrelated-large.bin" |]
                    let unrelatedOid =
                        unrelatedPointer.Replace("\r\n", "\n").Split('\n')
                        |> Array.find (fun line -> line.StartsWith "oid sha256:")
                        |> fun line -> line.Substring("oid sha256:".Length).Trim()

                    let! _ = runGitIn workPath [| "checkout"; "main" |]

                    do!
                        writeUtf8FileAsync
                            (join [| workPath; "publish-large.bin" |])
                            (String.replicate (1024 * 1024) "p")

                    let! saveStatus = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "test: publish explicit LFS object"
                                Paths = [| mkPath "publish-large.bin" |]
                                ExpectedWorkspaceVersion = saveStatus.WorkspaceVersion
                            }
                            (ctx "lfs-publish-revision")
                        |> Async.StartAsPromise

                    expectValue "LFS revision" revisionResult |> ignore

                    let! publishedPointer = runGitIn workPath [| "show"; "HEAD:publish-large.bin" |]
                    let publishedOid =
                        publishedPointer.Replace("\r\n", "\n").Split('\n')
                        |> Array.find (fun line -> line.StartsWith "oid sha256:")
                        |> fun line -> line.Substring("oid sha256:".Length).Trim()

                    observed.Clear()

                    let reports = ResizeArray<OperationProgress>()
                    let publishContext =
                        OperationContext.create
                            "lfs-explicit-publish"
                            OperationCancellation.none
                            reports.Add

                    let! publishStatus = sessionStatus session

                    let! publishResult =
                        promise {
                            let previousGoMaxProcs: obj = emitJsExpr () "process.env.GOMAXPROCS"
                            let previousForceProgress: obj = emitJsExpr () "process.env.GIT_LFS_FORCE_PROGRESS"

                            try
                                // A single Go scheduler exposes Git LFS exiting before its final
                                // piped summary is written. The provider must request live progress,
                                // even when the host environment has disabled it.
                                emitJsStatement
                                    ()
                                    "process.env.GOMAXPROCS = '1'; process.env.GIT_LFS_FORCE_PROGRESS = '0';"

                                return!
                                    (syncService session).Publish
                                        {
                                            ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                            ExpectedTargetRevision = None
                                        }
                                        publishContext
                                    |> Async.StartAsPromise
                            finally
                                emitJsStatement
                                    (previousGoMaxProcs, previousForceProgress)
                                    "for (const [key, value] of [['GOMAXPROCS', $0], ['GIT_LFS_FORCE_PROGRESS', $1]]) { if (value == null) delete process.env[key]; else process.env[key] = value; }"
                        }

                    expectValue "explicit LFS publish" publishResult |> ignore

                    let pushRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "push")

                    Vitest
                        .expect(pushRequest.Environment |> Array.contains ("GIT_LFS_SKIP_PUSH", "1"))
                        .toBe true

                    let reportEvents = reports.ToArray()
                    let lfsUploadEvents =
                        reportEvents
                        |> Array.filter (fun report -> report.PhaseCode = "lfs-upload")

                    Vitest.expect(lfsUploadEvents.Length > 0).toBe true

                    let numericLfsUploadEvents =
                        lfsUploadEvents
                        |> Array.filter (fun report -> report.Completed.IsSome || report.Total.IsSome)

                    for report in numericLfsUploadEvents do
                        Vitest.expect(report.Total).toEqual (Some 100.0)

                    let completedLfsUploadEvents =
                        numericLfsUploadEvents
                        |> Array.choose (fun report -> report.Completed)

                    for index = 1 to completedLfsUploadEvents.Length - 1 do
                        let previous = completedLfsUploadEvents[index - 1]
                        let current = completedLfsUploadEvents[index]
                        Vitest.expect(previous <= current).toBe true

                    let lastLfsUploadIndex =
                        reportEvents
                        |> Array.findIndexBack (fun report -> report.PhaseCode = "lfs-upload")

                    let pushEventIndex =
                        reportEvents
                        |> Array.findIndex (fun report ->
                            report.PhaseCode = "push"
                            && report.Completed.IsNone
                            && report.Total.IsNone)

                    Vitest.expect(reportEvents[pushEventIndex].Item).toEqual None
                    Vitest.expect(reportEvents[pushEventIndex].DisplayMessage).toEqual None
                    Vitest.expect(pushEventIndex > lastLfsUploadIndex).toBe true

                    let lfsObjectPath (oid: string) =
                        join [|
                            barePath
                            "lfs"
                            "objects"
                            oid.Substring(0, 2)
                            oid.Substring(2, 2)
                            oid
                        |]

                    let! publishedObjectPresent = pathExistsAsync (lfsObjectPath publishedOid)
                    let! unrelatedObjectPresent = pathExistsAsync (lfsObjectPath unrelatedOid)
                    Vitest.expect(publishedObjectPresent).toBe true
                    Vitest.expect(unrelatedObjectPresent).toBe false
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "Git synchronize composition",
    fun () ->
        Vitest.test (
            "synchronize publishes a branch that tracks a differently named upstream",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    let! _ = runGitIn workPath [| "checkout"; "-b"; "feature" |]
                    let! _ = runGitIn workPath [| "branch"; "--set-upstream-to=origin/main"; "feature" |]
                    do! writeUtf8FileAsync (join [| workPath; "feature.txt" |]) "feature content\n"
                    let! status = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "feature revision"
                                Paths = [| mkPath "feature.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "feature-revision")
                        |> Async.StartAsPromise

                    let revision = expectSucceeded "feature revision" revisionResult
                    let! refreshResult =
                        (syncService session).Refresh(ctx "feature-refresh")
                        |> Async.StartAsPromise

                    let refreshed = expectSucceeded "feature refresh" refreshResult
                    let originMainRevision =
                        refreshed.TargetRevision
                        |> Option.defaultWith (fun () -> failwith "Expected origin/main revision.")

                    let! afterRevision = sessionStatus session
                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = afterRevision.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "feature-synchronize")
                        |> Async.StartAsPromise

                    match synchronizeResult with
                    | Succeeded outcome ->
                        Vitest.expect(outcome.Publication).toEqual (Published)
                        Vitest.expect(outcome.ResultingRevision).toEqual (Some revision)
                        Vitest.expect(outcome.Value.TargetRef |> Option.map _.Name).toEqual (Some "origin/main")
                        Vitest.expect(outcome.Value.TargetRevision).toEqual (Some originMainRevision)
                        Vitest.expect(outcome.Value.Relationship).toEqual (LocalAhead)
                    | Failed failure ->
                        Vitest.expect(failure.Code).not.toBe ("precondition_failed")
                        failwith $"Expected feature synchronize to succeed, received {failure.Code}."
                    | PartiallySucceeded(_, failure) ->
                        failwith $"Expected feature synchronize to succeed, received partial {failure.Code}."

                    let! remoteFeature = runGitIn root [| "ls-remote"; barePath; "refs/heads/feature" |]
                    let remoteRevision = remoteFeature.Trim().Split([| '\t'; ' ' |], StringSplitOptions.RemoveEmptyEntries).[0]
                    Vitest.expect(remoteRevision).toEqual (RevisionId.value revision)
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "synchronize applies the previewed target when the target moves during update",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable raceApplied = false

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        Barrier =
                            Some(fun workspaceRoot point _ ->
                                async {
                                    if point = "update-merge" && not raceApplied then
                                        raceApplied <- true
                                        let fixtureRoot = dirname workspaceRoot
                                        let barePath = join [| fixtureRoot; "origin.git" |]

                                        do!
                                            Async.AwaitPromise(
                                                advanceTarget
                                                    fixtureRoot
                                                    barePath
                                                    [ "two.txt", "two\n" ]
                                            )
                                })
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    do! advanceTarget root barePath [ "one.txt", "one\n" ]
                    let! status = sessionStatus session

                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = false
                            }
                            (ctx "synchronize-target-race")
                        |> Async.StartAsPromise

                    let outcome =
                        match synchronizeResult with
                        | Succeeded outcome -> outcome
                        | Failed failure ->
                            failwith $"Expected target race synchronize to succeed, received {failure.Code}."
                        | PartiallySucceeded(_, failure) ->
                            failwith $"Expected target race synchronize to succeed, received partial {failure.Code}."

                    ignore outcome
                    let! first = tryReadUtf8FileAsync (join [| workPath; "one.txt" |])
                    let! second = tryReadUtf8FileAsync (join [| workPath; "two.txt" |])
                    Vitest.expect(first).toEqual (Some "one\n")
                    Vitest.expect(second).toEqual (None)

                    let! refreshResult =
                        (syncService session).Refresh(ctx "synchronize-target-race-refresh")
                        |> Async.StartAsPromise

                    let refreshed = expectSucceeded "synchronize target race refresh" refreshResult
                    Vitest.expect(refreshed.Relationship).toEqual (TargetAhead)
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "update reports a held index lock when the remote has advanced",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    do! advanceTarget root barePath [ "remote.txt", "remote update\n" ]
                    let! status = sessionStatus session
                    do! writeUtf8FileAsync (join [| workPath; ".git"; "index.lock" |]) ""

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "update-held-index-lock")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "update with a held index lock" updateResult
                    Vitest.expect(failure.Category).toEqual (Concurrency)
                    Vitest.expect(failure.Code).toBe "index_locked"
                    Vitest.expect(failure.StateChanged).toBe false
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "remove_index_lock")
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a rejected non-conflicting merge reports update_rejected without a conflict session",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    do! advanceTarget root barePath [ "base.txt", "target version\n" ]
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "dirty workspace version\n"
                    let! status = sessionStatus session

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "rejected-update")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "rejected update" updateResult
                    Vitest.expect(failure.Category).toEqual (ProviderError)
                    Vitest.expect(failure.Code).toBe "update_rejected"
                    Vitest.expect(failure.StateChanged).toBe (false)
                    Vitest.expect(failure.Retryable).toBe (false)
                    Vitest.expect(failure.Message).toBe "Git rejected the update."
                    Vitest.expect(failure.Details.Length > 0).toBe (true)
                    let! mergeHeadPath = runGitIn workPath [| "rev-parse"; "--git-path"; "MERGE_HEAD" |]
                    let! mergeHeadExists = pathExistsAsync (join [| workPath; mergeHeadPath.Trim() |])
                    Vitest.expect(mergeHeadExists).toBe (false)
                    let! dirty = tryReadUtf8FileAsync (join [| workPath; "base.txt" |])
                    Vitest.expect(dirty).toEqual (Some "dirty workspace version\n")
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a rejected update reports inspect_workspace when post-attempt status cannot be read",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable statusFailureArmed = false

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    let statusRead =
                                        request.Arguments |> Array.contains "--porcelain=v2"

                                    if statusRead && statusFailureArmed then
                                        return
                                            OperationResult.failed (
                                                OperationFailure.create
                                                    ProviderError
                                                    "status_read_failed"
                                                    "simulated post-attempt status failure"
                                            )
                                    else
                                        let! result = NodeProcess.run request processContext

                                        if request.Arguments |> Array.contains "merge" then
                                            statusFailureArmed <- true

                                        return result
                                })
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    do! advanceTarget root barePath [ "base.txt", "target version\n" ]
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "dirty workspace version\n"
                    let! status = sessionStatus session

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "rejected-update-status-failure")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "rejected update with unreadable status" updateResult
                    Vitest.expect(failure.Category).toEqual (ProviderError)
                    Vitest.expect(failure.Code).toBe ("update_rejected")
                    Vitest.expect(failure.StateChanged).toBe (true)
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "inspect_workspace")

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a cherry-pick conflict blocks synchronize before the remote can move",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createCherryPickConflictFixture ()

                try
                    let! remoteBefore = runGitIn root [| "ls-remote"; barePath; "refs/heads/main" |]
                    let! status = sessionStatus session

                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "cherry-pick-conflict-guard")
                        |> Async.StartAsPromise

                    ignore (expectOperationInProgress "cherry-pick conflict synchronize" synchronizeResult)
                    let! remoteAfter = runGitIn root [| "ls-remote"; barePath; "refs/heads/main" |]
                    Vitest.expect(remoteAfter.Trim()).toBe (remoteBefore.Trim())

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a direct update blocks a cherry-pick in progress",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, _, _, session = createCherryPickConflictFixture ()

                try
                    let! status = sessionStatus session

                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "cherry-pick-conflict-update")
                        |> Async.StartAsPromise

                    ignore (expectOperationInProgress "cherry-pick conflict update" updateResult)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a direct update blocks an active merge conflict session",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, _, _, session = createMergeHeadConflictFixture ()

                try
                    let! status = sessionStatus session
                    let! updateResult =
                        (syncService session).Update
                            { ExpectedWorkspaceVersion = status.WorkspaceVersion }
                            (ctx "merge-head-conflict-update")
                        |> Async.StartAsPromise

                    ignore (expectActiveConflictSession "merge-head conflict update" updateResult)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a direct publish blocks an active merge conflict session",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, _, _, session = createMergeHeadConflictFixture ()

                try
                    let! status = sessionStatus session
                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "merge-head-conflict-publish")
                        |> Async.StartAsPromise

                    ignore (expectActiveConflictSession "merge-head conflict publish" publishResult)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "unmerged index entries block synchronize without an operation marker",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, _, session = createCherryPickConflictFixture ()

                try
                    let! cherryPickHeadPath = runGitIn workPath [| "rev-parse"; "--git-path"; "CHERRY_PICK_HEAD" |]
                    do! removePathAsync (join [| workPath; cherryPickHeadPath.Trim() |])
                    let! status = sessionStatus session

                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = false
                            }
                            (ctx "unmerged-index-guard")
                        |> Async.StartAsPromise

                    ignore (expectOperationInProgress "unmerged index synchronize" synchronizeResult)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a rebase conflict blocks synchronize before refresh",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, _, session = createRebaseConflictFixture ()

                try
                    let! status = sessionStatus session

                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = false
                            }
                            (ctx "rebase-conflict-guard")
                        |> Async.StartAsPromise

                    ignore (expectOperationInProgress "rebase conflict synchronize" synchronizeResult)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a sequencer marker blocks synchronize before refresh",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, _, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    let! sequencerPath = runGitIn workPath [| "rev-parse"; "--git-path"; "sequencer" |]
                    let sequencerDirectory = join [| workPath; sequencerPath.Trim() |]
                    do! ensureDirectoryAsync sequencerDirectory
                    do! writeUtf8FileAsync (join [| sequencerDirectory; "todo" |]) "pick abc123 commit\n"

                    let! status = sessionStatus session
                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = false
                            }
                            (ctx "sequencer-guard")
                        |> Async.StartAsPromise

                    ignore (expectOperationInProgress "sequencer synchronize" synchronizeResult)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a publish rejected by the remote after an applied update is partial with retry_publish and a later synchronize publishes",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
                let hookPath = join [| barePath; "hooks"; "pre-receive" |]

                try
                    do! advanceTarget root barePath [ "target-only.txt", "target content\n" ]
                    do! writeUtf8FileAsync (join [| workPath; "local-only.txt" |]) "local content\n"
                    let! status = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "local revision before rejected publish"
                                Paths = [| mkPath "local-only.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "rejected-publish-revision")
                        |> Async.StartAsPromise

                    let localRevision = expectSucceeded "local revision before rejected publish" revisionResult

                    do! writeUtf8FileAsync hookPath "#!/bin/sh\necho \"protected branch\" >&2\nexit 1\n"

                    if (osDynamic?platform () |> unbox<string>) <> "win32" then
                        let! _ = fsPromisesDynamic?chmod (hookPath, 493) |> unbox<JS.Promise<obj>>
                        ()

                    let! beforeSynchronize = sessionStatus session
                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeSynchronize.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "rejected-publish-synchronize")
                        |> Async.StartAsPromise

                    match synchronizeResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(outcome.Publication).toEqual (LocalOnly)
                        Vitest.expect(failure.Code).toBe ("publish_rejected")
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.Retryable).toBe (false)
                        Vitest.expect(failure.Message).toContain ("protected branch")
                        Vitest
                            .expect(failure.RecoveryAction |> Option.map _.Code)
                            .toEqual (Some "retry_publish")

                        let! targetFile = tryReadUtf8FileAsync (join [| workPath; "target-only.txt" |])
                        Vitest.expect(targetFile).toEqual (Some "target content\n")

                        let! remoteRefs = runGitIn root [| "ls-remote"; barePath |]
                        Vitest
                            .expect(remoteRefs.Contains(RevisionId.value localRevision, StringComparison.Ordinal))
                            .toBe false
                    | Failed failure ->
                        let recoveryCode = failure.RecoveryAction |> Option.map _.Code |> Option.defaultValue "<none>"
                        failwith
                            $"Expected PartiallySucceeded after rejected publish, received Failed {failure.Category}/{failure.Code}, StateChanged={failure.StateChanged}, RecoveryAction={recoveryCode}."
                    | Succeeded _ -> failwith "Expected the rejected publish to return partial success."

                    do! removePathAsync hookPath
                    let! retryStatus = sessionStatus session
                    let! retryResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = retryStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "retry-rejected-publish-synchronize")
                        |> Async.StartAsPromise

                    match retryResult with
                    | Succeeded outcome ->
                        Vitest.expect(outcome.Publication).toEqual (Published)

                        let workspaceRevision =
                            outcome.Value.WorkspaceRevision
                            |> Option.map RevisionId.value
                            |> Option.defaultWith (fun () -> failwith "Expected the synchronized workspace revision.")

                        let! remoteMain = runGitIn root [| "ls-remote"; barePath; "refs/heads/main" |]
                        let remoteRevision =
                            remoteMain.Trim().Split([| '\t'; ' ' |], StringSplitOptions.RemoveEmptyEntries).[0]

                        Vitest.expect(remoteRevision).toEqual workspaceRevision
                    | Failed failure ->
                        failwith $"Expected retry synchronize to succeed, received {failure.Category}/{failure.Code}."
                    | PartiallySucceeded(_, failure) ->
                        failwith $"Expected retry synchronize to succeed, received partial {failure.Code}."
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a remote rejection from a local-ahead workspace stays a publish_rejected failure",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
                let hookPath = join [| barePath; "hooks"; "pre-receive" |]

                try
                    do! writeUtf8FileAsync (join [| workPath; "local-only.txt" |]) "local content\n"
                    let! status = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "local revision for rejected publish"
                                Paths = [| mkPath "local-only.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "local-ahead-rejected-publish-revision")
                        |> Async.StartAsPromise

                    let localRevision = expectSucceeded "local-ahead rejected publish revision" revisionResult
                    let! remoteBefore = runGitIn root [| "ls-remote"; barePath; "refs/heads/main" |]
                    do! writeUtf8FileAsync hookPath "#!/bin/sh\necho \"protected branch\" >&2\nexit 1\n"

                    if (osDynamic?platform () |> unbox<string>) <> "win32" then
                        let! _ = fsPromisesDynamic?chmod (hookPath, 493) |> unbox<JS.Promise<obj>>
                        ()

                    let! beforeSynchronize = sessionStatus session
                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeSynchronize.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "local-ahead-rejected-publish")
                        |> Async.StartAsPromise

                    match synchronizeResult with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual (ProviderError)
                        Vitest.expect(failure.Code).toBe ("publish_rejected")
                        Vitest.expect(failure.StateChanged).toBe (false)
                        Vitest.expect(failure.Retryable).toBe (false)
                        Vitest.expect(failure.Message).toContain ("protected branch")
                    | Succeeded _ -> failwith "Expected the local-ahead publish to be rejected."
                    | PartiallySucceeded(_, failure) ->
                        failwith $"Expected a failed local-ahead publish, received partial {failure.Code}."

                    let! remoteAfter = runGitIn root [| "ls-remote"; barePath; "refs/heads/main" |]
                    Vitest.expect(remoteAfter.Trim()).toBe (remoteBefore.Trim())
                    Vitest.expect(remoteAfter.Contains(RevisionId.value localRevision, StringComparison.Ordinal)).toBe false

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a locked remote ref is a retryable concurrency failure",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
                let lockPath = join [| barePath; "refs"; "heads"; "main.lock" |]

                try
                    do! writeUtf8FileAsync (join [| workPath; "locked-publish.txt" |]) "local content\n"
                    let! status = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "local revision for locked publish"
                                Paths = [| mkPath "locked-publish.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "locked-publish-revision")
                        |> Async.StartAsPromise

                    expectSucceeded "local revision for locked publish" revisionResult |> ignore
                    do! writeUtf8FileAsync lockPath ""

                    let! beforePublish = sessionStatus session
                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = beforePublish.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "locked-publish")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "locked publish" publishResult
                    Vitest.expect(failure.Category).toEqual (Concurrency)
                    Vitest.expect(failure.Code).toBe ("precondition_failed")
                    Vitest.expect(failure.Retryable).toBe (true)
                    Vitest.expect(failure.StateChanged).toBe (false)

                    do! removePathAsync lockPath
                    let! retryStatus = sessionStatus session
                    let! retryResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = retryStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "retry-locked-publish")
                        |> Async.StartAsPromise

                    expectSucceeded "retry locked publish" retryResult |> ignore
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "a batched rejection of a locked or moved remote ref is a retryable concurrency failure",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable barePath = ""
                let mutable rejectPush = false
                let mutable rejectionReason = "reference already exists"

                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    if rejectPush && (request.Arguments |> Array.contains "push") then
                                        return
                                            OperationResult.succeeded {
                                                ExitCode = 1
                                                StdOut = ""
                                                StdErr =
                                                    $"remote: error: Unable to create '{barePath}/refs/heads/main.lock': File exists.\nTo {barePath}\n ! [remote rejected] main -> main ({rejectionReason})\nerror: failed to push some refs to '{barePath}'\n"
                                            }
                                    else
                                        return! NodeProcess.run request processContext
                                })
                }

                let! root, workPath, createdBarePath, session = createSyncFixture hooks
                barePath <- createdBarePath
                rejectPush <- true

                try
                    do! writeUtf8FileAsync (join [| workPath; "batched-publish.txt" |]) "local content\n"
                    let! status = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "local revision for batched publish rejection"
                                Paths = [| mkPath "batched-publish.txt" |]
                                ExpectedWorkspaceVersion = status.WorkspaceVersion
                            }
                            (ctx "batched-publish-revision")
                        |> Async.StartAsPromise

                    expectSucceeded "local revision for batched publish rejection" revisionResult |> ignore

                    let assertConcurrencyFailure contextId = promise {
                        let! beforePublish = sessionStatus session
                        let! publishResult =
                            (syncService session).Publish
                                {
                                    ExpectedWorkspaceVersion = beforePublish.WorkspaceVersion
                                    ExpectedTargetRevision = None
                                }
                                (ctx contextId)
                            |> Async.StartAsPromise

                        let failure = expectProviderFailure contextId publishResult
                        Vitest.expect(failure.Category).toEqual (Concurrency)
                        Vitest.expect(failure.Code).toBe ("precondition_failed")
                        Vitest.expect(failure.Retryable).toBe (true)
                        Vitest.expect(failure.StateChanged).toBe (false)
                    }

                    do! assertConcurrencyFailure "batched locked publish"
                    rejectionReason <- "incorrect old value provided"
                    do! assertConcurrencyFailure "batched moved publish"

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

let private createUntrackedFeature (workPath: string) = promise {
    let! _ = runGitIn workPath [| "config"; "branch.autoSetupMerge"; "false" |]
    let! _ = runGitIn workPath [| "config"; "push.autoSetupRemote"; "false" |]
    let! _ = runGitIn workPath [| "checkout"; "-b"; "feature" |]
    return ()
}

let private advanceBranch
    (root: string)
    (barePath: string)
    (branch: string)
    (path: string)
    (content: string)
    =
    promise {
        let clonePath = join [| root; $"advance-{branch}-{DateTime.Now.Ticks}" |]
        let! _ = runGitIn root [| "clone"; barePath; clonePath |]
        let! _ = runGitIn clonePath [| "fetch"; "origin"; branch |]
        let! _ = runGitIn clonePath [| "checkout"; branch |]
        let! _ = runGitIn clonePath [| "config"; "user.name"; "External Client" |]
        let! _ = runGitIn clonePath [| "config"; "user.email"; "external@example.org" |]
        do! writeUtf8FileAsync (join [| clonePath; path |]) content
        let! _ = runGitIn clonePath [| "add"; "-A" |]
        let! _ = runGitIn clonePath [| "commit"; "-m"; "external: advance branch" |]
        let! revision = runGitIn clonePath [| "rev-parse"; "HEAD" |]
        let! _ = runGitIn clonePath [| "push"; "origin"; branch |]
        return revision.Trim()
    }

let private initializeAndBindWorkspace
    (hooks: GitWorkspaceSession.GitSessionHooks)
    (workPath: string)
    (barePath: string)
    (operationPrefix: string)
    =
    promise {
        let factory = GitWorkspaceSession.createFactory hooks

        let! initializeResult =
            factory.Initialize
                {
                    TargetPath = workPath
                    Location = None
                }
                (ctx $"{operationPrefix}-initialize")
            |> Async.StartAsPromise

        let initializedBinding = expectSucceeded "initialize Git workspace" initializeResult

        let! bindResult =
            factory.Bind
                {
                    WorkspaceRoot = initializedBinding.WorkspaceRoot
                    Location = (syncBinding workPath barePath).Location
                }
                (ctx $"{operationPrefix}-bind")
            |> Async.StartAsPromise

        let binding = expectSucceeded "bind Git workspace" bindResult

        let! openResult =
            factory.Open binding (ctx $"{operationPrefix}-open")
            |> Async.StartAsPromise

        return expectSucceeded "open bound Git workspace" openResult
    }

Vitest.describe (
    "Git publish tracking",
    fun () ->
        Vitest.test (
            "a first publish of a new branch sets its upstream and the next synchronize updates",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    do! createUntrackedFeature workPath
                    do! writeUtf8FileAsync (join [| workPath; "feature.txt" |]) "feature content\n"
                    let! revisionStatus = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "feature revision"
                                Paths = [| mkPath "feature.txt" |]
                                ExpectedWorkspaceVersion = revisionStatus.WorkspaceVersion
                            }
                            (ctx "publish-tracking-feature-revision")
                        |> Async.StartAsPromise

                    let revision = expectSucceeded "feature revision" revisionResult
                    let! beforeSynchronize = sessionStatus session

                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeSynchronize.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "publish-tracking-feature-first-sync")
                        |> Async.StartAsPromise

                    let synchronized = expectSucceededOutcome "feature first synchronize" synchronizeResult
                    Vitest.expect(synchronized.Publication).toEqual (Published)
                    Vitest.expect(synchronized.ResultingRevision).toEqual (Some revision)
                    Vitest.expect(synchronized.Value.TargetRef |> Option.map _.Name).toEqual (Some "origin/feature")

                    let! mergeConfig = runGitIn workPath [| "config"; "--get"; "branch.feature.merge" |]
                    Vitest.expect(mergeConfig.Trim()).toBe "refs/heads/feature"

                    let! theirs = advanceBranch root barePath "feature" "from-second-clone.txt" "second clone\n"
                    let! beforeNextSynchronize = sessionStatus session

                    let! nextSynchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeNextSynchronize.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "publish-tracking-feature-second-sync")
                        |> Async.StartAsPromise

                    expectSucceeded "feature second synchronize" nextSynchronizeResult |> ignore
                    let! head = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    Vitest.expect(head.Trim()).toBe theirs
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "an unborn branch synchronizes with a NoOp and no upstream",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let barePath = join [| root; "empty-origin.git" |]
                let workPath = join [| root; "empty-work" |]

                try
                    let! _ = runGitIn root [| "init"; "--bare"; "-b"; "main"; barePath |]
                    let! session =
                        initializeAndBindWorkspace
                            GitWorkspaceSession.GitSessionHooks.none
                            workPath
                            barePath
                            "publish-tracking-unborn"

                    let! beforeSynchronize = sessionStatus session

                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeSynchronize.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "publish-tracking-unborn-sync")
                        |> Async.StartAsPromise

                    let synchronized = expectSucceededOutcome "unborn branch synchronize" synchronizeResult

                    match synchronized.Effect with
                    | NoOp _ -> ()
                    | Performed -> failwith "Expected an unborn branch synchronize to return a NoOp."

                    let! branch = runGitIn workPath [| "symbolic-ref"; "--short"; "HEAD" |]
                    let! mergeConfig =
                        runGitResultIn workPath [| "config"; "--get"; $"branch.{branch.Trim()}.merge" |]

                    Vitest.expect(mergeConfig.ExitCode).toBe 1
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a first publish into an empty remote sets the upstream and the next synchronize updates",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let barePath = join [| root; "empty-origin.git" |]
                let workPath = join [| root; "empty-work" |]

                try
                    let! _ = runGitIn root [| "init"; "--bare"; "-b"; "main"; barePath |]
                    let! session =
                        initializeAndBindWorkspace
                            GitWorkspaceSession.GitSessionHooks.none
                            workPath
                            barePath
                            "publish-tracking-empty"

                    let! _ = runGitIn workPath [| "config"; "user.name"; "VCS Sync Tests" |]
                    let! _ = runGitIn workPath [| "config"; "user.email"; "sync@example.org" |]
                    let! _ = runGitIn workPath [| "config"; "core.autocrlf"; "false" |]

                    do! writeUtf8FileAsync (join [| workPath; "initial.txt" |]) "initial commit\n"
                    let! revisionStatus = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "initial empty remote revision"
                                Paths = [| mkPath "initial.txt" |]
                                ExpectedWorkspaceVersion = revisionStatus.WorkspaceVersion
                            }
                            (ctx "publish-tracking-empty-revision")
                        |> Async.StartAsPromise

                    expectSucceeded "empty remote initial revision" revisionResult |> ignore
                    let! beforeSynchronize = sessionStatus session

                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeSynchronize.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "publish-tracking-empty-first-sync")
                        |> Async.StartAsPromise

                    expectSucceeded "empty remote first synchronize" synchronizeResult |> ignore
                    let! mergeConfig = runGitIn workPath [| "config"; "--get"; "branch.main.merge" |]
                    Vitest.expect(mergeConfig.Trim()).toBe "refs/heads/main"

                    let! theirs = advanceBranch root barePath "main" "from-second-clone.txt" "second clone\n"
                    let! beforeNextSynchronize = sessionStatus session

                    let! nextSynchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeNextSynchronize.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "publish-tracking-empty-second-sync")
                        |> Async.StartAsPromise

                    expectSucceeded "empty remote second synchronize" nextSynchronizeResult |> ignore
                    let! head = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    Vitest.expect(head.Trim()).toBe theirs
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a publish over an equal remote branch without tracking sets tracking",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, _, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    do! createUntrackedFeature workPath
                    do! writeUtf8FileAsync (join [| workPath; "feature.txt" |]) "feature content\n"
                    let! revisionStatus = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "feature revision before plain push"
                                Paths = [| mkPath "feature.txt" |]
                                ExpectedWorkspaceVersion = revisionStatus.WorkspaceVersion
                            }
                            (ctx "publish-tracking-equal-revision")
                        |> Async.StartAsPromise

                    expectSucceeded "equal remote feature revision" revisionResult |> ignore
                    let! _ = runGitIn workPath [| "push"; "origin"; "feature" |]
                    let! beforeMergeConfig =
                        runGitResultIn workPath [| "config"; "--get"; "branch.feature.merge" |]

                    Vitest.expect(beforeMergeConfig.ExitCode).toBe 1
                    let! beforePublish = sessionStatus session

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = beforePublish.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-tracking-equal-remote")
                        |> Async.StartAsPromise

                    let outcome = expectSucceededOutcome "publish equal remote without tracking" publishResult

                    match outcome.Effect with
                    | Performed -> ()
                    | NoOp _ -> failwith "Expected publish to perform the upstream setup."

                    Vitest.expect(outcome.Publication).toEqual (Published)
                    Vitest.expect(outcome.AffectedPaths.Length).toBe 0
                    let! mergeConfig = runGitIn workPath [| "config"; "--get"; "branch.feature.merge" |]
                    Vitest.expect(mergeConfig.Trim()).toBe "refs/heads/feature"
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a tracked publish rejects an unfetched remote advance",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    let! theirs = advanceBranch root barePath "main" "external-advance.txt" "external revision\n"
                    let! beforePublish = sessionStatus session

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = beforePublish.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-tracking-unfetched-target")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "publish with an unfetched remote advance" publishResult
                    Vitest.expect(failure.Category).toEqual (Concurrency)
                    Vitest.expect(failure.Code).toBe "precondition_failed"
                    Vitest.expect(failure.StateChanged).toBe false
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "refresh_workspace")

                    let! remoteRevision = runGitIn barePath [| "rev-parse"; "refs/heads/main" |]
                    Vitest.expect(remoteRevision.Trim()).toBe theirs
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a diverged untracked target is adopted before publish fails",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    do! createUntrackedFeature workPath
                    do! writeUtf8FileAsync (join [| workPath; "feature.txt" |]) "feature base\n"
                    let! firstStatus = sessionStatus session

                    let! firstRevision =
                        session.Core.CreateRevision
                            {
                                Message = "feature base revision"
                                Paths = [| mkPath "feature.txt" |]
                                ExpectedWorkspaceVersion = firstStatus.WorkspaceVersion
                            }
                            (ctx "publish-tracking-diverged-base")
                        |> Async.StartAsPromise

                    expectSucceeded "feature base revision" firstRevision |> ignore
                    let! _ = runGitIn workPath [| "push"; "origin"; "feature" |]
                    do! writeUtf8FileAsync (join [| workPath; "local-only.txt" |]) "local commit\n"
                    let! localStatus = sessionStatus session

                    let! localRevision =
                        session.Core.CreateRevision
                            {
                                Message = "local feature revision"
                                Paths = [| mkPath "local-only.txt" |]
                                ExpectedWorkspaceVersion = localStatus.WorkspaceVersion
                            }
                            (ctx "publish-tracking-diverged-local")
                        |> Async.StartAsPromise

                    expectSucceeded "local feature revision" localRevision |> ignore
                    let! _ = advanceBranch root barePath "feature" "remote-only.txt" "remote commit\n"
                    let! beforePublish = sessionStatus session

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = beforePublish.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-tracking-diverged")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "publish diverged target" publishResult
                    Vitest.expect(failure.Category).toEqual (Concurrency)
                    Vitest.expect(failure.Code).toBe "precondition_failed"
                    Vitest.expect(failure.StateChanged).toBe true
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "refresh_workspace")

                    let! mergeConfig = runGitIn workPath [| "config"; "--get"; "branch.feature.merge" |]
                    Vitest.expect(mergeConfig.Trim()).toBe "refs/heads/feature"
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a publish over a remote branch that moved ahead adopts it and asks for a refresh",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    do! createUntrackedFeature workPath
                    do! writeUtf8FileAsync (join [| workPath; "feature.txt" |]) "feature content\n"
                    let! revisionStatus = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "feature revision before remote advance"
                                Paths = [| mkPath "feature.txt" |]
                                ExpectedWorkspaceVersion = revisionStatus.WorkspaceVersion
                            }
                            (ctx "publish-tracking-adopt-revision")
                        |> Async.StartAsPromise

                    expectSucceeded "feature revision before remote advance" revisionResult |> ignore
                    let! _ = runGitIn workPath [| "push"; "origin"; "feature" |]
                    let! theirs = advanceBranch root barePath "feature" "remote-only.txt" "remote commit\n"
                    let! beforePublish = sessionStatus session

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = beforePublish.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-tracking-adopt")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "publish remote branch ahead" publishResult
                    Vitest.expect(failure.Category).toEqual (Concurrency)
                    Vitest.expect(failure.Code).toBe "precondition_failed"
                    Vitest.expect(failure.StateChanged).toBe true
                    Vitest.expect(failure.Retryable).toBe true
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "refresh_workspace")
                    let! mergeConfig = runGitIn workPath [| "config"; "--get"; "branch.feature.merge" |]
                    Vitest.expect(mergeConfig.Trim()).toBe "refs/heads/feature"
                    let! fetchedFeature = runGitIn workPath [| "rev-parse"; "refs/remotes/origin/feature" |]
                    Vitest.expect(fetchedFeature.Trim()).toBe theirs

                    let! beforeSynchronize = sessionStatus session

                    let! synchronizeResult =
                        (syncService session).Synchronize
                            {
                                ExpectedWorkspaceVersion = beforeSynchronize.WorkspaceVersion
                                ExpectedTargetRevision = None
                                AcceptUpdateRisks = false
                                PublishLocalRevisions = true
                            }
                            (ctx "publish-tracking-adopt-synchronize")
                        |> Async.StartAsPromise

                    expectSucceeded "synchronize adopted remote branch" synchronizeResult |> ignore
                    let! head = runGitIn workPath [| "rev-parse"; "HEAD" |]
                    Vitest.expect(head.Trim()).toBe theirs
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a publish whose expected target is stale carries the refresh recovery",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none

                try
                    let! refreshResult =
                        (syncService session).Refresh(ctx "publish-tracking-stale-refresh")
                        |> Async.StartAsPromise

                    let refreshed = expectSucceeded "publish tracking stale refresh" refreshResult
                    let expectedTarget =
                        refreshed.TargetRevision
                        |> Option.defaultWith (fun () -> failwith "Expected the tracked target revision.")

                    let! _ = advanceBranch root barePath "main" "external-advance.txt" "external revision\n"
                    let! beforePublish = sessionStatus session

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = beforePublish.WorkspaceVersion
                                ExpectedTargetRevision = Some expectedTarget
                            }
                            (ctx "publish-tracking-stale-target")
                        |> Async.StartAsPromise

                    let failure = expectProviderFailure "publish with stale expected target" publishResult
                    Vitest.expect(failure.Category).toEqual (Concurrency)
                    Vitest.expect(failure.Code).toBe "precondition_failed"
                    Vitest.expect(failure.Retryable).toBe true
                    Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "refresh_workspace")
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )

        Vitest.test (
            "a verified publish reports tracking failure and rolls back the remote key",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let hooks = {
                    GitWorkspaceSession.GitSessionHooks.none with
                        RunProcess =
                            Some(fun request processContext ->
                                async {
                                    let mergeWrite =
                                        request.Arguments.Length >= 3
                                        && request.Arguments[0] = "config"
                                        && request.Arguments[1].StartsWith("branch.", StringComparison.Ordinal)
                                        && request.Arguments[1].EndsWith(".merge", StringComparison.Ordinal)

                                    if mergeWrite then
                                        return
                                            OperationResult.succeeded {
                                                NodeProcess.ExitCode = 1
                                                StdOut = ""
                                                StdErr = "error: could not lock config file"
                                            }
                                    elif
                                        (request.Arguments |> Array.contains "push")
                                        && (request.Arguments |> Array.contains "--set-upstream")
                                    then
                                        // Git would write tracking during push, so this reproduces a push without that side effect.
                                        let strippedRequest = {
                                            request with
                                                Arguments = request.Arguments |> Array.filter ((<>) "--set-upstream")
                                        }

                                        return! NodeProcess.run strippedRequest processContext
                                    else
                                        return! NodeProcess.run request processContext
                                })
                }

                let! root, workPath, barePath, session = createSyncFixture hooks

                try
                    do! createUntrackedFeature workPath
                    do! writeUtf8FileAsync (join [| workPath; "feature.txt" |]) "feature content\n"
                    let! revisionStatus = sessionStatus session

                    let! revisionResult =
                        session.Core.CreateRevision
                            {
                                Message = "feature revision for tracking failure"
                                Paths = [| mkPath "feature.txt" |]
                                ExpectedWorkspaceVersion = revisionStatus.WorkspaceVersion
                            }
                            (ctx "publish-tracking-config-failure-revision")
                        |> Async.StartAsPromise

                    expectSucceeded "feature revision for tracking failure" revisionResult |> ignore
                    let! beforePublish = sessionStatus session

                    let! publishResult =
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = beforePublish.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            (ctx "publish-tracking-config-failure")
                        |> Async.StartAsPromise

                    match publishResult with
                    | PartiallySucceeded(outcome, failure) ->
                        Vitest.expect(outcome.Publication).toEqual (Published)
                        Vitest.expect(failure.Code).toBe "upstream_config_failed"
                        Vitest.expect(failure.StateChanged).toBe true
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "retry_publish")

                        let! remoteConfig =
                            runGitResultIn workPath [| "config"; "--get"; "branch.feature.remote" |]

                        Vitest.expect(remoteConfig.ExitCode).toBe 1
                        let! branchRevision = runGitIn barePath [| "rev-parse"; "refs/heads/feature" |]
                        let! head = runGitIn workPath [| "rev-parse"; "HEAD" |]
                        Vitest.expect(branchRevision.Trim()).toBe (head.Trim())
                    | Succeeded _ -> failwith "Expected tracking setup to fail after publication."
                    | Failed failure -> failwith $"Publish failed before the verified push ({failure.Code})."
                with error ->
                    do! removeDirectoryAsync root
                    return raise error

                do! removeDirectoryAsync root
            }
        )
)
