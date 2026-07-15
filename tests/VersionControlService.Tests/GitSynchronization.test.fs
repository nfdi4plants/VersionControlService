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

let private createNestedUnmergedConflictFixture () = promise {
    let! root, workPath, barePath, session = createSyncFixture GitWorkspaceSession.GitSessionHooks.none
    let conflictPath = "nested/conflict.txt"
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
    "Git conflict evidence hardening",
    fun () ->
        let expectFailure operationName result =
            match result with
            | Failed failure -> failure
            | PartiallySucceeded(_, failure) -> failure
            | Succeeded _ -> failwith $"Expected {operationName} to fail."

        Vitest.test (
            "propagates an unmerged-path process failure instead of minting a token",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable armFailure = false
                let injectedFailure = OperationFailure.create Network "unmerged_probe_failed" "Injected unmerged probe failure."

                let hooks: GitWorkspaceSession.GitSessionHooks = {
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
            "propagates an unreadable unmerged worktree path instead of minting a token",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root, workPath, session, _, _, _ = createUnmergedConflictFixture ()

                try
                    let conflictFile = join [| workPath; "base.txt" |]
                    do! removePathAsync conflictFile
                    do! ensureDirectoryAsync conflictFile
                    let! result = Async.StartAsPromise(session.Core.GetStatus(ctx "unreadable-unmerged-path"))
                    let failure = expectFailure "status with unreadable unmerged path" result
                    Vitest.expect(failure.Category).toEqual (ProviderError)
                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "hashes a large unmerged file through Git instead of buffering it in the session",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let mutable observedHashObject = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
                    RunProcess =
                        Some(fun request context ->
                            if request.Arguments |> Array.contains "hash-object" then
                                observedHashObject <- true

                            NodeProcess.run request context)
                    Barrier = None
                }

                let! root, workPath, session, _, _, _ = createUnmergedConflictFixtureWithHooks hooks

                try
                    observedHashObject <- false
                    let largeMarkerText = "x".PadRight(8 * 1024 * 1024, 'x')
                    do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) largeMarkerText
                    let! statusResult = Async.StartAsPromise(session.Core.GetStatus(ctx "large-unmerged-status"))
                    expectValue "large unmerged status" statusResult |> ignore
                    Vitest.expect(observedHashObject).toBe (true)
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
    "GitWorkspaceSession v2 synchronization",
    fun () ->
        Vitest.test (
            "v2 conflicting update opens a versioned conflict session and rejects stale tokens before verified finalize",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // A barrier that can advance the local branch between the finalize
                // pre-check and the compare-and-swap ref update.
                let mutable armFinalizeRace = false

                let hooks: GitWorkspaceSession.GitSessionHooks = {
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
            "v2 cancellation stops refresh update publish and clone with structured canceled results",
            TestOptions(timeout = 120000),
            fun () -> promise {
                // Deterministic interruption: the transfer-start barrier cancels the
                // armed operation's context before its transfer runs.
                let mutable armCancel: OperationCancellation.Source option = None

                let hooks: GitWorkspaceSession.GitSessionHooks = {
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
