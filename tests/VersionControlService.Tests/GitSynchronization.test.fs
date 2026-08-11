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

let private expectSucceeded operationName result =
    expectValue operationName result

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
            "keeps invalid UTF-8 stage candidates byte-oriented and rejects automatic resolution",
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
                                    Resolution = PickCandidate "workspace"
                                }
                                (ctx "binary-stage-resolve")
                        )

                    let failure = expectFailure "binary stage automatic resolution" resolveResult
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
            "does not buffer conflict content for known unsupported extensions",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable inspectReads = false
                let mutable stageBlobReads = 0
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
                                stageBlobReads <- stageBlobReads + 1

                            NodeProcess.runBytes request context)
                    RunProcess = None
                    Barrier =
                        Some(fun _ point _ ->
                            async {
                                if inspectReads && point = "conflict-content-chunk" then
                                    combinedPreviewChunks <- combinedPreviewChunks + 1
                            })
                }

                let! root, _, _, conflicts, _ =
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

                    Vitest.expect(stageBlobReads).toBe (0)
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
                                    Message = Some "finalize after refresh"
                                }
                                (ctx "retry-finalize")
                        )

                    let mergedRevision = expectValue "verified finalize" retryFinalize
                    Vitest.expect(mergedRevision.IsSome).toBe (true)

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
                        (syncService session).Publish
                            {
                                ExpectedWorkspaceVersion = publishStatus.WorkspaceVersion
                                ExpectedTargetRevision = None
                            }
                            publishContext
                        |> Async.StartAsPromise

                    expectValue "explicit LFS publish" publishResult |> ignore

                    let pushRequest =
                        observed
                        |> Seq.find (fun request -> request.Arguments |> Array.contains "push")

                    Vitest
                        .expect(pushRequest.Environment |> Array.contains ("GIT_LFS_SKIP_PUSH", "1"))
                        .toBe true

                    Vitest.expect(reports |> Seq.exists (fun report -> report.PhaseCode = "lfs-upload")).toBe true
                    Vitest
                        .expect(
                            reports
                            |> Seq.exists (fun (report: OperationProgress) ->
                                report.PhaseCode = "lfs-upload"
                                && report.Completed.IsSome
                                && report.Total.IsSome)
                        )
                        .toBe true

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
