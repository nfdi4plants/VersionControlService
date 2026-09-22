module VersionControlService.Tests.GitRevisionPolicyTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Tests.NodePath
open Vitest

module GitCredentialStrategy = VersionControlService.Git.GitCredentialStrategy
module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsProviderOptions = VersionControlService.LakeFs.LakeFsProviderOptions
module LakeFsWorkspaceSession = VersionControlService.LakeFs.LakeFsWorkspaceSession
module NodeProcess = VersionControlService.Runtime.Node.Process

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private lfsPointerPrefix = "version https://git-lfs.github.com/spec/v1"

let private createTempDirectoryAsync () : JS.Promise<string> = promise {
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-git-revision-policy-" |]
    let! created = fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>
    return! fsPromisesDynamic?realpath (created) |> unbox<JS.Promise<string>>
}

let private removeDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private ensureDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?mkdir (path, createObj [ "recursive" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private writeUtf8FileAsync (path: string) (content: string) : JS.Promise<unit> = promise {
    do! ensureDirectoryAsync (dirname path)
    let! _ = fsPromisesDynamic?writeFile (path, content, "utf8") |> unbox<JS.Promise<obj>>
    return ()
}

let private readUtf8FileAsync (path: string) : JS.Promise<string> = promise {
    let! content = fsPromisesDynamic?readFile (path, "utf8") |> unbox<JS.Promise<string>>
    return content
}

let private runGit (cwd: string) (arguments: string[]) : JS.Promise<Result<string, string>> = promise {
    let request = {
        NodeProcess.ProcessRequest.create "git" arguments with
            WorkingDirectory = Some cwd
    }

    let! result =
        Async.StartAsPromise(NodeProcess.run request (OperationContext.detached "git-revision-policy-fixture"))

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

let private repositoryPath value =
    RepositoryPath.tryCreate value |> Result.defaultWith failwith

let private context name = OperationContext.detached name

let private expectSucceeded label result =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded(_, failure) ->
        failwith $"Expected {label} to succeed, got partial success ({failure.Code})."
    | Failed failure -> failwith $"Expected {label} to succeed, got failure ({failure.Code})."

type private GitRevisionPolicyFixture = {
    Root: string
    WorkPath: string
    Session: WorkspaceSession
}

let private createGitFixture
    (strategy: RevisionPolicyStrategy)
    (baseAttributes: string option)
    : JS.Promise<GitRevisionPolicyFixture> =
    promise {
        let! root = createTempDirectoryAsync ()
        let workPath = join [| root; "work" |]
        let! _ = runGitOk root [| "init"; "--initial-branch=main"; workPath |]
        let! _ = runGitOk workPath [| "config"; "user.name"; "VCS Revision Policy Tests" |]
        let! _ = runGitOk workPath [| "config"; "user.email"; "revision-policy@example.org" |]
        let! _ = runGitOk workPath [| "config"; "core.autocrlf"; "false" |]
        let! _ = runGitOk workPath [| "lfs"; "install"; "--local" |]

        do! writeUtf8FileAsync (join [| workPath; "base.txt" |]) "base\n"

        match baseAttributes with
        | Some content -> do! writeUtf8FileAsync (join [| workPath; ".gitattributes" |]) content
        | None -> ()

        let! _ = runGitOk workPath [| "add"; "-A" |]
        let! _ = runGitOk workPath [| "commit"; "-m"; "init: base" |]
        let! _ =
            runGitOk
                workPath
                [| "config"; "--local"; "versioncontrolservice.lfs.autotrackthresholdmb"; "1" |]

        let factory =
            GitWorkspaceSession.createFactoryWithCredentialsIdentityAndPolicy
                GitWorkspaceSession.GitSessionHooks.none
                GitCredentialStrategy.anonymous
                GitCredentialStrategy.anonymousIdentity
                strategy

        let adoptRequest: AdoptRequest = {
            WorkspaceRoot = workPath
            ConnectionProfileId = None
        }

        let! adoptedResult = factory.Adopt adoptRequest (context "revision-policy-adopt") |> Async.StartAsPromise
        let binding = expectSucceeded "revision policy adoption" adoptedResult
        let! openedResult = factory.Open binding (context "revision-policy-open") |> Async.StartAsPromise
        let session = expectSucceeded "revision policy open" openedResult

        return {
            Root = root
            WorkPath = workPath
            Session = session
        }
    }

let private withGitFixture
    (strategy: RevisionPolicyStrategy)
    (baseAttributes: string option)
    (action: GitRevisionPolicyFixture -> JS.Promise<'T>)
    : JS.Promise<'T> =
    promise {
        let! fixture = createGitFixture strategy baseAttributes

        try
            let! result = action fixture
            do! removeDirectoryAsync fixture.Root
            return result
        with error ->
            do! removeDirectoryAsync fixture.Root
            return raise error
    }

let private createRevision
    (fixture: GitRevisionPolicyFixture)
    (message: string)
    (paths: string[])
    : JS.Promise<OperationResult<RevisionId>> =
    promise {
        let! statusResult = fixture.Session.Core.GetStatus(context "revision-policy-status") |> Async.StartAsPromise
        let status = expectSucceeded "revision policy status" statusResult

        let request: CreateRevisionRequest = {
            Message = message
            Paths = paths |> Array.map repositoryPath
            ExpectedWorkspaceVersion = status.WorkspaceVersion
        }

        return!
            fixture.Session.Core.CreateRevision request (context "revision-policy-create")
            |> Async.StartAsPromise
    }

let private lfsPointer payloadSize =
    let oid = String.replicate 64 "0"
    $"{lfsPointerPrefix}\noid sha256:{oid}\nsize {payloadSize}\n"

Vitest.describe (
    "Git revision policy",
    fun () ->
        Vitest.test (
            "an inline path above the threshold is committed as a plain blob",
            TestOptions(timeout = 120000),
            fun () ->
                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun request ->
                        if RepositoryPath.value request.Path = "meta/workbook.xlsx" then
                            RevisionPathPolicy.Inline
                        else
                            RevisionPathPolicy.Automatic
                }

                withGitFixture strategy None (fun fixture -> promise {
                    do!
                        writeUtf8FileAsync
                            (join [| fixture.WorkPath; "meta/workbook.xlsx" |])
                            (String.replicate (2 * 1024 * 1024) "m")

                    do!
                        writeUtf8FileAsync
                            (join [| fixture.WorkPath; "data/raw.bin" |])
                            (String.replicate (2 * 1024 * 1024) "d")

                    let! revision =
                        createRevision
                            fixture
                            "test: inline path above threshold"
                            [| "meta/workbook.xlsx"; "data/raw.bin" |]

                    expectSucceeded "inline path revision" revision |> ignore

                    let! inlineSize =
                        runGitOk fixture.WorkPath [| "cat-file"; "-s"; "HEAD:meta/workbook.xlsx" |]

                    let! trackedContent =
                        runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:data/raw.bin" |]

                    let! attributes = runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:.gitattributes" |]
                    Vitest.expect(inlineSize.Trim() |> int).toBe (2 * 1024 * 1024)
                    Vitest.expect(trackedContent.StartsWith(lfsPointerPrefix)).toBe true
                    Vitest.expect(attributes.Contains("\"/data/raw.bin\" filter=lfs")).toBe true
                    Vitest.expect(attributes.Contains("meta/workbook.xlsx")).toBe false
                })
        )

        Vitest.test (
            "an inline path overrides an lfs attribute rule",
            TestOptions(timeout = 120000),
            fun () ->
                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun _ -> RevisionPathPolicy.Inline
                }

                withGitFixture
                    strategy
                    (Some "meta/*.xlsx filter=lfs diff=lfs merge=lfs -text\n")
                    (fun fixture -> promise {
                        do!
                            writeUtf8FileAsync
                                (join [| fixture.WorkPath; "meta/workbook.xlsx" |])
                                (String.replicate (2 * 1024 * 1024) "w")

                        let! revision =
                            createRevision
                                fixture
                                "test: inline path overrides attributes"
                                [| "meta/workbook.xlsx" |]

                        expectSucceeded "inline attribute revision" revision |> ignore

                        let! inlineSize =
                            runGitOk fixture.WorkPath [| "cat-file"; "-s"; "HEAD:meta/workbook.xlsx" |]

                        let! attributes =
                            runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:.gitattributes" |]

                        let! status = runGitOk fixture.WorkPath [| "status"; "--porcelain" |]
                        Vitest.expect(inlineSize.Trim() |> int).toBe (2 * 1024 * 1024)
                        Vitest.expect(attributes.Contains("\"/meta/workbook.xlsx\" -filter -diff -merge")).toBe true
                        Vitest.expect(status.Trim()).toBe ""
                    })
        )

        Vitest.test (
            "a large object path below the threshold becomes a pointer",
            TestOptions(timeout = 120000),
            fun () ->
                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun request ->
                        if RepositoryPath.value request.Path = "assets/small.bin" then
                            RevisionPathPolicy.LargeObject
                        else
                            RevisionPathPolicy.Automatic
                }

                withGitFixture strategy None (fun fixture -> promise {
                    do!
                        writeUtf8FileAsync
                            (join [| fixture.WorkPath; "assets/small.bin" |])
                            (String.replicate 100 "s")

                    let! revision =
                        createRevision fixture "test: force a small large object" [| "assets/small.bin" |]

                    expectSucceeded "large object revision" revision |> ignore

                    let! content = runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:assets/small.bin" |]
                    let! attributes = runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:.gitattributes" |]
                    Vitest.expect(content.StartsWith(lfsPointerPrefix)).toBe true
                    Vitest.expect(attributes.Contains("\"/assets/small.bin\" filter=lfs")).toBe true
                })
        )

        Vitest.test (
            "an inline path holding a pointer is refused before any mutation",
            TestOptions(timeout = 120000),
            fun () ->
                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun _ -> RevisionPathPolicy.Inline
                }

                withGitFixture strategy None (fun fixture -> promise {
                    let path = join [| fixture.WorkPath; "meta/workbook.xlsx" |]
                    let pointer = lfsPointer 2097152
                    do! writeUtf8FileAsync path pointer

                    let! headBefore = runGitOk fixture.WorkPath [| "rev-parse"; "HEAD" |]
                    let! statusBefore = runGitOk fixture.WorkPath [| "status"; "--porcelain" |]
                    let! revision = createRevision fixture "test: reject an inline pointer" [| "meta/workbook.xlsx" |]

                    match revision with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual Validation
                        Vitest.expect(failure.Code).toBe "inline_content_not_materialized"
                        Vitest.expect(failure.Message).toBe "Materialize the affected files before creating an inline revision."
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.Retryable).toBe false
                        Vitest.expect(failure.AffectedPaths).toEqual [| "meta/workbook.xlsx" |]
                        Vitest.expect(failure.RecoveryAction |> Option.map _.Code).toEqual (Some "retry_materialization")
                        Vitest
                            .expect(failure.RecoveryAction |> Option.bind _.Instructions)
                            .toEqual (Some "Materialize the affected files, refresh the status and create the revision again.")
                    | Succeeded _
                    | PartiallySucceeded _ -> failwith "Expected an inline pointer to be refused."

                    let! headAfter = runGitOk fixture.WorkPath [| "rev-parse"; "HEAD" |]
                    let! statusAfter = runGitOk fixture.WorkPath [| "status"; "--porcelain" |]
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                    Vitest.expect(statusAfter).toBe statusBefore
                })
        )

        Vitest.test (
            "the strategy receives the repository path and the size",
            TestOptions(timeout = 120000),
            fun () ->
                let requests = ResizeArray<RevisionPathPolicyRequest>()

                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun request ->
                        requests.Add request
                        RevisionPathPolicy.Automatic
                }

                withGitFixture strategy None (fun fixture -> promise {
                    do! writeUtf8FileAsync (join [| fixture.WorkPath; "tiny.txt" |]) (String.replicate 10 "t")
                    do! writeUtf8FileAsync (join [| fixture.WorkPath; "medium.bin" |]) (String.replicate 3000 "m")

                    let! revision =
                        createRevision
                            fixture
                            "test: record revision policy requests"
                            [| "tiny.txt"; "medium.bin" |]

                    expectSucceeded "request recording revision" revision |> ignore

                    let recorded =
                        requests.ToArray()
                        |> Array.map (fun request -> RepositoryPath.value request.Path, request.SizeInBytes)
                        |> Array.sortBy fst

                    Vitest.expect(recorded |> Array.map fst).toEqual [| "medium.bin"; "tiny.txt" |]
                    Vitest.expect(recorded |> Array.map snd).toEqual [| 3000.0; 10.0 |]
                })
        )

        Vitest.test (
            "an inline path with a dirty unrelated attributes file is refused",
            TestOptions(timeout = 120000),
            fun () ->
                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun _ -> RevisionPathPolicy.Inline
                }

                withGitFixture
                    strategy
                    (Some "meta/*.xlsx filter=lfs diff=lfs merge=lfs -text\n")
                    (fun fixture -> promise {
                        do!
                            writeUtf8FileAsync
                                (join [| fixture.WorkPath; "meta/workbook.xlsx" |])
                                (String.replicate (2 * 1024 * 1024) "w")

                        do!
                            writeUtf8FileAsync
                                (join [| fixture.WorkPath; ".gitattributes" |])
                                "meta/*.xlsx filter=lfs diff=lfs merge=lfs -text\nunrelated.txt text\n"

                        let! revision =
                            createRevision
                                fixture
                                "test: reject dirty unrelated attributes"
                                [| "meta/workbook.xlsx" |]

                        match revision with
                        | Failed failure ->
                            Vitest.expect(failure.Category).toEqual Validation
                            Vitest.expect(failure.Code).toBe "precondition_failed"
                            Vitest.expect(failure.AffectedPaths).toEqual [| ".gitattributes" |]
                        | Succeeded _
                        | PartiallySucceeded _ -> failwith "Expected dirty attributes to be refused."
                    })
        )

        Vitest.test (
            "a throwing strategy fails the revision before any mutation",
            TestOptions(timeout = 120000),
            fun () ->
                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun _ -> raise (Exception "revision policy test failure")
                }

                withGitFixture strategy None (fun fixture -> promise {
                    do! writeUtf8FileAsync (join [| fixture.WorkPath; "throwing.bin" |]) (String.replicate 100 "x")
                    let! headBefore = runGitOk fixture.WorkPath [| "rev-parse"; "HEAD" |]

                    let! revision =
                        createRevision fixture "test: reject a throwing policy" [| "throwing.bin" |]

                    match revision with
                    | Failed failure ->
                        Vitest.expect(failure.Category).toEqual ProviderError
                        Vitest.expect(failure.Code).toBe "revision_policy_failed"
                        Vitest.expect(failure.StateChanged).toBe false
                        Vitest.expect(failure.AffectedPaths).toEqual [| "throwing.bin" |]
                    | Succeeded _
                    | PartiallySucceeded _ -> failwith "Expected the throwing strategy to fail the revision."

                    let! headAfter = runGitOk fixture.WorkPath [| "rev-parse"; "HEAD" |]
                    Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())
                })
        )

        Vitest.test (
            "the automatic strategy keeps the existing behavior",
            TestOptions(timeout = 120000),
            fun () ->
                withGitFixture RevisionPolicyStrategy.automatic None (fun fixture -> promise {
                    do!
                        writeUtf8FileAsync
                            (join [| fixture.WorkPath; "automatic/large.bin" |])
                            (String.replicate (2 * 1024 * 1024) "l")

                    do!
                        writeUtf8FileAsync
                            (join [| fixture.WorkPath; "automatic/small.bin" |])
                            (String.replicate 100 "s")

                    let! revision =
                        createRevision
                            fixture
                            "test: preserve automatic policy behavior"
                            [| "automatic/large.bin"; "automatic/small.bin" |]

                    expectSucceeded "automatic policy revision" revision |> ignore

                    let! largeContent =
                        runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:automatic/large.bin" |]

                    let! smallSize =
                        runGitOk fixture.WorkPath [| "cat-file"; "-s"; "HEAD:automatic/small.bin" |]

                    Vitest.expect(largeContent.StartsWith(lfsPointerPrefix)).toBe true
                    Vitest.expect(smallSize.Trim() |> int).toBe 100
                })
        )

        Vitest.test (
            "untracking a path writes an unset rule that survives the next automatic revision",
            TestOptions(timeout = 180000),
            fun () ->
                withGitFixture RevisionPolicyStrategy.automatic None (fun fixture -> promise {
                    let path = "automatic/opt-out.bin"
                    let absolutePath = join [| fixture.WorkPath; path |]
                    do! writeUtf8FileAsync absolutePath (String.replicate (2 * 1024 * 1024) "a")

                    let! first = createRevision fixture "test: track before automatic opt-out" [| path |]
                    expectSucceeded "automatic tracking revision" first |> ignore

                    let storagePolicy =
                        fixture.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! unsetResult =
                        storagePolicy.SetPathPolicy
                            (repositoryPath path)
                            false
                            (context "automatic-opt-out")
                        |> Async.StartAsPromise

                    expectSucceeded "automatic opt-out" unsetResult |> ignore

                    let! filter = runGitOk fixture.WorkPath [| "check-attr"; "filter"; "--"; path |]
                    Vitest.expect(filter.Trim().EndsWith("unset")).toBe true

                    do! writeUtf8FileAsync absolutePath (String.replicate (2 * 1024 * 1024) "b")
                    let! second = createRevision fixture "test: preserve automatic opt-out" [| path; ".gitattributes" |]
                    expectSucceeded "automatic opt-out revision" second |> ignore

                    let! committed = runGitOk fixture.WorkPath [| "cat-file"; "-p"; $"HEAD:{path}" |]
                    let! lfsFiles = runGitOk fixture.WorkPath [| "lfs"; "ls-files"; "--name-only" |]
                    Vitest.expect(committed.StartsWith(lfsPointerPrefix)).toBe false
                    Vitest.expect(lfsFiles.Trim()).toBe ""
                })
        )

        Vitest.test (
            "untracking a path that was never tracked records the opt-out only",
            TestOptions(timeout = 120000),
            fun () ->
                withGitFixture RevisionPolicyStrategy.automatic (Some "\n") (fun fixture -> promise {
                    let path = "manual/plain.txt"
                    let absolutePath = join [| fixture.WorkPath; path |]
                    let original = "plain content\n"
                    do! writeUtf8FileAsync absolutePath original

                    let! first = createRevision fixture "test: commit plain path" [| path |]
                    expectSucceeded "plain path revision" first |> ignore

                    let storagePolicy =
                        fixture.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! unsetResult =
                        storagePolicy.SetPathPolicy
                            (repositoryPath path)
                            false
                            (context "plain-path-opt-out")
                        |> Async.StartAsPromise

                    expectSucceeded "plain path opt-out" unsetResult |> ignore

                    let! attributes = readUtf8FileAsync (join [| fixture.WorkPath; ".gitattributes" |])
                    let! workingContent = readUtf8FileAsync absolutePath
                    let! status = runGitOk fixture.WorkPath [| "status"; "--porcelain" |]

                    Vitest.expect(attributes.Contains("\"/manual/plain.txt\" -filter -diff -merge")).toBe true
                    Vitest.expect(workingContent).toBe original
                    Vitest.expect(status.Trim()).toBe "M .gitattributes"
                })
        )

        Vitest.test (
            "a LargeObject policy tracks a path after an explicit opt-out",
            TestOptions(timeout = 120000),
            fun () ->
                let path = "forced/large.bin"

                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun request ->
                        if RepositoryPath.value request.Path = path then
                            RevisionPathPolicy.LargeObject
                        else
                            RevisionPathPolicy.Automatic
                }

                withGitFixture strategy (Some "\n") (fun fixture -> promise {
                    let absolutePath = join [| fixture.WorkPath; path |]
                    let storagePolicy =
                        fixture.Session.StoragePolicy
                        |> Option.defaultWith (fun () -> failwith "Expected Git storage policy.")

                    let! unsetResult =
                        storagePolicy.SetPathPolicy
                            (repositoryPath path)
                            false
                            (context "large-object-opt-out")
                        |> Async.StartAsPromise

                    expectSucceeded "large object opt-out" unsetResult |> ignore
                    do! writeUtf8FileAsync absolutePath (String.replicate 100 "f")

                    let! revision = createRevision fixture "test: restore large object policy" [| path; ".gitattributes" |]
                    expectSucceeded "large object revision after opt-out" revision |> ignore

                    let! committed = runGitOk fixture.WorkPath [| "cat-file"; "-p"; $"HEAD:{path}" |]
                    let! filter = runGitOk fixture.WorkPath [| "check-attr"; "filter"; "--"; path |]
                    Vitest.expect(committed.StartsWith(lfsPointerPrefix)).toBe true
                    Vitest.expect(filter.Trim().EndsWith("lfs")).toBe true
                })
        )

        Vitest.test (
            "switching a path from large object to inline and back leaves one effective rule",
            TestOptions(timeout = 180000),
            fun () ->
                let policy = ref RevisionPathPolicy.LargeObject

                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun request ->
                        if RepositoryPath.value request.Path = "assets/flip.bin" then
                            policy.Value
                        else
                            RevisionPathPolicy.Automatic
                }

                withGitFixture strategy None (fun fixture -> promise {
                    let path = join [| fixture.WorkPath; "assets/flip.bin" |]
                    do! writeUtf8FileAsync path "first\n"
                    let! first = createRevision fixture "test: large object" [| "assets/flip.bin" |]
                    expectSucceeded "large object revision" first |> ignore

                    policy.Value <- RevisionPathPolicy.Inline
                    do! writeUtf8FileAsync path "second\n"
                    let! second = createRevision fixture "test: inline" [| "assets/flip.bin" |]
                    expectSucceeded "inline revision" second |> ignore
                    let! inlineContent = runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:assets/flip.bin" |]
                    Vitest.expect(inlineContent).toBe "second\n"

                    policy.Value <- RevisionPathPolicy.LargeObject
                    do! writeUtf8FileAsync path "third\n"
                    let! third = createRevision fixture "test: large object again" [| "assets/flip.bin" |]
                    expectSucceeded "second large object revision" third |> ignore

                    let! content = runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:assets/flip.bin" |]
                    let! attributes = runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:.gitattributes" |]
                    let! filter = runGitOk fixture.WorkPath [| "check-attr"; "filter"; "--"; "assets/flip.bin" |]
                    let! status = runGitOk fixture.WorkPath [| "status"; "--porcelain" |]
                    // Attribute rules are only appended, so the earlier rule stays in the
                    // file. Git applies the last matching line, so the new rule has to
                    // come after it and the effective filter has to follow.
                    let trackingIndex = attributes.LastIndexOf("\"/assets/flip.bin\" filter=lfs")
                    let untrackingIndex = attributes.LastIndexOf("\"/assets/flip.bin\" -filter")
                    Vitest.expect(content.StartsWith(lfsPointerPrefix)).toBe true
                    Vitest.expect(trackingIndex > untrackingIndex).toBe true
                    Vitest.expect(filter.Trim().EndsWith("lfs")).toBe true
                    Vitest.expect(status.Trim()).toBe ""
                })
        )

        Vitest.test (
            "an inline text file keeps the line ending normalization git would apply",
            TestOptions(timeout = 120000),
            fun () ->
                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun request ->
                        if RepositoryPath.value request.Path = "notes.txt" then
                            RevisionPathPolicy.Inline
                        else
                            RevisionPathPolicy.Automatic
                }

                withGitFixture strategy (Some "notes.txt text\n") (fun fixture -> promise {
                    do! writeUtf8FileAsync (join [| fixture.WorkPath; "notes.txt" |]) "a\r\nb\r\n"
                    let! revision = createRevision fixture "test: inline text" [| "notes.txt" |]
                    expectSucceeded "inline text revision" revision |> ignore
                    let! content = runGitOk fixture.WorkPath [| "cat-file"; "-p"; "HEAD:notes.txt" |]
                    let! status = runGitOk fixture.WorkPath [| "status"; "--porcelain" |]
                    Vitest.expect(content).toBe "a\nb\n"
                    Vitest.expect(status.Trim()).toBe ""
                })
        )

        Vitest.test (
            "a directory selection with an inline file above the threshold is not reported as changed content",
            TestOptions(timeout = 120000),
            fun () ->
                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun request ->
                        if (RepositoryPath.value request.Path).StartsWith("meta/") then
                            RevisionPathPolicy.Inline
                        else
                            RevisionPathPolicy.Automatic
                }

                withGitFixture strategy None (fun fixture -> promise {
                    do!
                        writeUtf8FileAsync
                            (join [| fixture.WorkPath; "meta/isa.study.xlsx" |])
                            (String.replicate (2 * 1024 * 1024) "m")

                    do! writeUtf8FileAsync (join [| fixture.WorkPath; "meta/readme.md" |]) "notes\n"
                    let! revision = createRevision fixture "test: directory selection" [| "meta" |]
                    expectSucceeded "directory selection revision" revision |> ignore
                    let! size = runGitOk fixture.WorkPath [| "cat-file"; "-s"; "HEAD:meta/isa.study.xlsx" |]
                    Vitest.expect(size.Trim() |> int).toBe (2 * 1024 * 1024)
                })
        )

        Vitest.test (
            "the lakeFS factory accepts a strategy and never calls it",
            fun () ->
                let mutable called = false

                let strategy: RevisionPolicyStrategy = {
                    ResolvePathPolicy = fun _ ->
                        called <- true
                        raise (Exception "lakeFS must not resolve revision paths")
                }

                let options: LakeFsProviderOptions.LakeFsProviderOptions = {
                    StateRoot = "unused-lakefs-state"
                    PathCaseSensitivity = CaseInsensitive
                }

                let factory =
                    LakeFsWorkspaceSession.createFactoryWithPolicy
                        options
                        LakeFsCredentials.unconfigured
                        strategy

                Vitest.expect(ProviderId.value factory.Id).toBe "lakefs"
                Vitest.expect(called).toBe false
        )
)
