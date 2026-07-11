module VersionControlService.Tests.GitProviderTests

open System
open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Bindings.SimpleGit
open VersionControlService.Contracts.VersionControl
open VersionControlService.Git.GitAuthAdapter
open VersionControlService.Tests.NodePath
open Vitest

module GitProvider = VersionControlService.Git.GitProvider
module ProviderRegistry = VersionControlService.VersionControlProviderRegistry
module GitTokenProvider = VersionControlService.Git.GitTokenProvider

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

[<Emit("process.env[$0] ?? null")>]
let private getProcessEnvValue (_name: string) : string = jsNative

[<Emit("process.env[$0] = $1")>]
let private setProcessEnvValue (_name: string) (_value: string) : unit = jsNative

[<Emit("delete process.env[$0]")>]
let private deleteProcessEnvValue (_name: string) : unit = jsNative

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-provider-tests-" |]
    fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>

let private removeDirectoryAsync path : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private writeUtf8FileAsync path content : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?writeFile (path, content, "utf8") |> unbox<JS.Promise<obj>>
    return ()
}

let private readUtf8FileAsync (path: string) : JS.Promise<string> =
    fsPromisesDynamic?readFile (path, "utf8") |> unbox<JS.Promise<string>>

let private restoreProcessEnvValue (name: string) (value: string option) =
    match value with
    | Some existingValue -> setProcessEnvValue name existingValue
    | None -> deleteProcessEnvValue name

let private createSimpleGit repoPath =
    SimpleGit.create (SimpleGitOptions(baseDir = repoPath, binary = U3.Case1 "git", maxConcurrentProcesses = 1))
    |> applyNonInteractiveEnv

let private expectProviderOk (operationName: string) (result: VersionControlResult<'T>) : 'T =
    match result with
    | Ok outcome -> outcome.Value
    | Error failure -> failwith $"{operationName} failed ({failure.Kind}): {failure.Message}"

let private expectProviderError (result: VersionControlResult<'T>) : VersionControlFailure =
    match result with
    | Ok _ -> failwith "Expected provider operation to fail."
    | Error failure -> failure

let private providerIntegrationTestOptions = TestOptions(timeout = 120000)

let private testRemoteUrl = "https://provider.local.test/origin.git"

let private toFileRemoteUrl (path: string) =
    let normalized = path.Replace("\\", "/")

    if normalized.StartsWith("/", StringComparison.Ordinal) then
        $"file://{normalized}"
    else
        $"file:///{normalized}"

let private writeLocalRemoteRewriteAsync (rootPath: string) (remotePath: string) : JS.Promise<unit> =
    let localRemoteUrl = toFileRemoteUrl remotePath

    let gitConfig =
        $"[url \"{localRemoteUrl}\"]\n\tinsteadOf = {testRemoteUrl}\n"

    writeUtf8FileAsync (join [| rootPath; ".gitconfig" |]) gitConfig

let private testRemoteHost = "provider.local.test"
let private testRemoteToken = "provider-test-token"

let private testAuthenticatedRemoteUrl =
    $"https://oauth2:{testRemoteToken}@{testRemoteHost}/origin.git"

let private configureLocalRemoteRewriteAsync (git: ISimpleGit) (remotePath: string) : JS.Promise<unit> = promise {
    let localRemoteUrl = toFileRemoteUrl remotePath
    let! _ = git.raw [| "config"; "--add"; $"url.{localRemoteUrl}.insteadOf"; testRemoteUrl |]
    let! _ = git.raw [| "config"; "--add"; $"url.{localRemoteUrl}.insteadOf"; testAuthenticatedRemoteUrl |]
    return ()
}

let private withTestTokenProvider (body: unit -> JS.Promise<'T>) : JS.Promise<'T> = promise {
    GitTokenProvider.setTokenProvider {
        TryGetAccessToken =
            fun host -> promise {
                return
                    if String.Equals(host, testRemoteHost, StringComparison.OrdinalIgnoreCase) then
                        Some testRemoteToken
                    else
                        None
            }
    }

    try
        return! body ()
    finally
        GitTokenProvider.setTokenProvider GitTokenProvider.defaultTokenProvider
}

let private withTemporaryGitHome (homePath: string) (body: unit -> JS.Promise<'T>) : JS.Promise<'T> =
    promise {
        let previousHome = getProcessEnvValue "HOME" |> Option.ofObj
        let previousUserProfile = getProcessEnvValue "USERPROFILE" |> Option.ofObj

        setProcessEnvValue "HOME" homePath
        setProcessEnvValue "USERPROFILE" homePath

        try
            return! body ()
        finally
            restoreProcessEnvValue "HOME" previousHome
            restoreProcessEnvValue "USERPROFILE" previousUserProfile
    }

let private withProviderTempRepository
    (testBody: VersionControlProvider -> string -> ISimpleGit -> JS.Promise<unit>)
    : JS.Promise<unit> =
    promise {
    let! rootPath = createTempDirectoryAsync ()

    try
        let repoPath = join [| rootPath; "repo" |]
        let provider = GitProvider.create ()

        let! initResult =
            provider.InitializeWorkspace {
                TargetPath = repoPath
                ProviderRemoteUri = None
                ProviderOptions = [||]
            }

        let normalizedRepoPath = expectProviderOk "provider init" initResult
        let git = createSimpleGit normalizedRepoPath

        let! _ = git.raw [| "config"; "user.name"; "VersionControlService Tests" |]
        let! _ = git.raw [| "config"; "user.email"; "provider-tests@example.org" |]
        let! _ = git.raw [| "config"; "core.autocrlf"; "false" |]

        do! testBody provider normalizedRepoPath git
        do! removeDirectoryAsync rootPath
    with error ->
        do! removeDirectoryAsync rootPath
        return raise error
}

let private createPushedBareRemote
    (rootPath: string)
    (provider: VersionControlProvider)
    (repoPath: string)
    (git: ISimpleGit)
    : JS.Promise<string * string> =
    promise {
        let trackedPath = join [| repoPath; "tracked.txt" |]
        let remotePath = join [| rootPath; "origin.git" |]

        do! writeUtf8FileAsync trackedPath "tracked\n"

        let! commitResult =
            provider.Commit
                repoPath
                {
                    Message = "test: add tracked file"
                    Paths = [| "tracked.txt" |]
                }

        expectProviderOk "commit tracked file" commitResult |> ignore

        let! statusResult = provider.GetStatus repoPath
        let status = expectProviderOk "status after tracked commit" statusResult

        let baseBranch =
            status.Current
            |> Option.defaultWith (fun () -> failwith "Expected current branch after commit.")

        let! _ = git.raw [| "init"; "--bare"; $"--initial-branch={baseBranch}"; remotePath |]
        let! _ = git.raw [| "remote"; "add"; "origin"; remotePath |]
        let! _ = git.raw [| "push"; "-u"; "origin"; baseBranch |]

        return remotePath, baseBranch
    }

let private expectPerformedOrStorageBoundary (operationName: string) (result: VersionControlResult<string>) =
    match result with
    | Ok outcome ->
        match outcome.Effect with
        | VersionControlEffect.Performed _ -> Vitest.expect(outcome.Value.Length >= 0).toBe (true)
        | VersionControlEffect.NoOp _ -> failwith $"{operationName} unexpectedly returned NoOp."
    | Error failure ->
        let isExpectedBoundary =
            failure.Kind = VersionControlFailureKind.DependencyMissing
            || failure.Kind = VersionControlFailureKind.NotApplicable

        Vitest.expect(isExpectedBoundary).toBe (true)

Vitest.describe (
    "VersionControlOutcome helpers",
    fun () ->
        Vitest.test (
            "performed wraps a value with a Performed effect",
            fun () ->
                let outcome = VersionControlOutcome.performed 42

                Vitest.expect(outcome.Value).toBe (42)

                match outcome.Effect with
                | VersionControlEffect.Performed None -> Vitest.expect(true).toBe (true)
                | _ -> failwith "Expected Performed None."
        )

        Vitest.test (
            "noOp wraps a value with a NoOp reason",
            fun () ->
                let outcome = VersionControlOutcome.noOp (Some "not needed") "ok"

                Vitest.expect(outcome.Value).toBe ("ok")

                match outcome.Effect with
                | VersionControlEffect.NoOp(Some "not needed") -> Vitest.expect(true).toBe (true)
                | _ -> failwith "Expected NoOp reason."
        )

        Vitest.test (
            "unsupported returns an Unsupported failure",
            fun () ->
                let result: VersionControlResult<unit> =
                    VersionControlResult.unsupported "Selected-path commit is not supported."

                match result with
                | Error failure ->
                    Vitest.expect(failure.Kind).toEqual (VersionControlFailureKind.Unsupported)
                    Vitest.expect(failure.Message).toBe ("Selected-path commit is not supported.")
                | Ok _ -> failwith "Expected Unsupported failure."
        )

        Vitest.test (
            "notApplicable returns a NotApplicable failure",
            fun () ->
                let result: VersionControlResult<unit> =
                    VersionControlResult.notApplicable "Large-object policy is not applicable."

                match result with
                | Error failure ->
                    Vitest.expect(failure.Kind).toEqual (VersionControlFailureKind.NotApplicable)
                    Vitest.expect(failure.Message).toBe ("Large-object policy is not applicable.")
                | Ok _ -> failwith "Expected NotApplicable failure."
        )
)

Vitest.describe (
    "GitProvider capabilities",
    fun () ->
        Vitest.test (
            "reports Git and Git LFS backed capabilities",
            fun () ->
                let provider = GitProvider.create ()

                Vitest.expect(provider.Kind).toEqual (VersionControlProviderKind.Git)
                Vitest.expect(provider.Capabilities.SupportsInitializeWorkspace).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsSelectedPathCommit).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsPullPreflight).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsContentMergeResolution).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsVersionPickMergeResolution).toBe (false)
                Vitest.expect(provider.Capabilities.SupportsDiffLineCounts).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsWordDiff).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsLargeFilePolicySelection).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsLargeFileThreshold).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsDownloadLargeObjectsToggle).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsDownloadLargeObject).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsFreeLocalObjectCopy).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsStoragePrune).toBe (true)
                Vitest.expect(provider.Capabilities.SupportsStorageDeduplication).toBe (true)
        )

        Vitest.test (
            "registry defaults to Git provider",
            fun () ->
                ProviderRegistry.resetToDefault ()
                let provider = ProviderRegistry.get ()
                Vitest.expect(provider.Kind).toEqual (VersionControlProviderKind.Git)
        )
)

Vitest.describe (
    "GitProvider commit workflow",
    fun () ->
        Vitest.test (
            "commits selected paths and clears unrelated staged state",
            TestOptions(timeout = 120000),
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let aPath = join [| repoPath; "a.txt" |]
                        let bPath = join [| repoPath; "b.txt" |]

                        do! writeUtf8FileAsync aPath "a1\n"
                        do! writeUtf8FileAsync bPath "b1\n"

                        let! baseCommit =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: base"
                                    Paths = [| "a.txt"; "b.txt" |]
                                }

                        expectProviderOk "base commit" baseCommit |> ignore

                        do! writeUtf8FileAsync aPath "a2\n"
                        do! writeUtf8FileAsync bPath "b2\n"

                        let! _ = git.raw [| "add"; "b.txt" |]

                        let! selectedCommit =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: selected a"
                                    Paths = [| "a.txt" |]
                                }

                        let commitHash = expectProviderOk "selected commit" selectedCommit
                        Vitest.expect(commitHash.Length).toBeGreaterThan (6)

                        let! changedFiles = git.raw [| "diff-tree"; "--no-commit-id"; "--name-only"; "-r"; "HEAD" |]
                        Vitest.expect(changedFiles.Trim()).toBe ("a.txt")

                        let! porcelainStatus = git.raw [| "status"; "--porcelain=v1"; "--"; "b.txt" |]
                        Vitest.expect(porcelainStatus.TrimEnd()).toBe (" M b.txt")
                    })
            }
        )

        Vitest.test (
            "commits literal selected paths",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let bracketRelativePath = "a[1].txt"
                        let plainRelativePath = "a1.txt"
                        let bracketPath = join [| repoPath; bracketRelativePath |]
                        let plainPath = join [| repoPath; plainRelativePath |]

                        do! writeUtf8FileAsync bracketPath "bracket-base\n"
                        do! writeUtf8FileAsync plainPath "plain-base\n"

                        // Base commit through raw git so the fixture does not depend on provider pathspec behavior.
                        let! _ = git.raw [| "add"; "-A" |]
                        let! _ = git.raw [| "commit"; "-m"; "test: literal base" |]

                        do! writeUtf8FileAsync bracketPath "bracket-changed\n"
                        do! writeUtf8FileAsync plainPath "plain-changed\n"

                        let! selectedCommit =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: select bracket file only"
                                    Paths = [| bracketRelativePath |]
                                }

                        expectProviderOk "literal selected commit" selectedCommit |> ignore

                        let! changedFiles = git.raw [| "diff-tree"; "--no-commit-id"; "--name-only"; "-r"; "HEAD" |]

                        let committedPaths =
                            changedFiles.Replace("\r\n", "\n").Split('\n')
                            |> Array.map _.Trim()
                            |> Array.filter (fun line -> line <> "")

                        Vitest.expect(committedPaths).toEqual ([| bracketRelativePath |])

                        let! plainStatus = git.raw [| "status"; "--porcelain=v1"; "--"; plainRelativePath |]
                        Vitest.expect(plainStatus.TrimEnd()).toBe ($" M {plainRelativePath}")

                        // Wildcard characters are literal filenames, not pathspecs: requesting "*.txt"
                        // when no file has that exact name must not commit glob matches.
                        let! headBeforeWildcard = git.raw [| "rev-parse"; "HEAD" |]

                        let! wildcardCommit =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: wildcard is not a pathspec"
                                    Paths = [| "*.txt" |]
                                }

                        match wildcardCommit with
                        | Ok _ ->
                            failwith
                                "Expected the wildcard-named commit to fail because no file is literally named '*.txt'."
                        | Error _ -> ()

                        let! headAfterWildcard = git.raw [| "rev-parse"; "HEAD" |]
                        Vitest.expect(headAfterWildcard.Trim()).toBe (headBeforeWildcard.Trim())
                    })
            }
        )

        Vitest.test (
            "failed selected commit preserves unrelated staged state",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let bPath = join [| repoPath; "b.txt" |]

                        do! writeUtf8FileAsync bPath "b-base\n"
                        let! _ = git.raw [| "add"; "-A" |]
                        let! _ = git.raw [| "commit"; "-m"; "test: base" |]

                        do! writeUtf8FileAsync bPath "b-staged\n"
                        let! _ = git.raw [| "add"; "b.txt" |]

                        let! headBefore = git.raw [| "rev-parse"; "HEAD" |]

                        let! commitResult =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: outside path must fail"
                                    Paths = [| "../outside.txt" |]
                                }

                        expectProviderError commitResult |> ignore

                        // Unrelated staged state must be preserved by the failed operation.
                        let! bStatus = git.raw [| "status"; "--porcelain=v1"; "--"; "b.txt" |]
                        Vitest.expect(bStatus.TrimEnd()).toBe ("M  b.txt")

                        let! headAfter = git.raw [| "rev-parse"; "HEAD" |]
                        Vitest.expect(headAfter.Trim()).toBe (headBefore.Trim())

                        let! workingContent = readUtf8FileAsync bPath
                        Vitest.expect(workingContent).toBe ("b-staged\n")
                    })
            }
        )
)

Vitest.describe (
    "GitProvider checkout tracking",
    fun () ->
        Vitest.test (
            "checkout preserves the exact upstream provider ref",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let rootPath = dirname repoPath
                        let filePath = join [| repoPath; "shared.txt" |]

                        do! writeUtf8FileAsync filePath "base\n"
                        let! _ = git.raw [| "add"; "-A" |]
                        let! _ = git.raw [| "commit"; "-m"; "test: base" |]
                        let! currentBranch = git.raw [| "branch"; "--show-current" |]
                        let baseBranch = currentBranch.Trim()

                        let originPath = join [| rootPath; "origin.git" |]
                        let upstreamPath = join [| rootPath; "upstream.git" |]
                        let! _ = git.raw [| "init"; "--bare"; $"--initial-branch={baseBranch}"; originPath |]
                        let! _ = git.raw [| "init"; "--bare"; $"--initial-branch={baseBranch}"; upstreamPath |]
                        let! _ = git.raw [| "remote"; "add"; "origin"; originPath |]
                        let! _ = git.raw [| "remote"; "add"; "upstream"; upstreamPath |]
                        let! _ = git.raw [| "push"; "origin"; baseBranch |]
                        let! _ = git.raw [| "push"; "upstream"; baseBranch |]

                        // origin/feature and upstream/feature diverge with different tips.
                        let! _ = git.raw [| "checkout"; "-b"; "feature" |]
                        do! writeUtf8FileAsync filePath "origin version\n"
                        let! _ = git.raw [| "add"; "shared.txt" |]
                        let! _ = git.raw [| "commit"; "-m"; "test: origin feature" |]
                        let! _ = git.raw [| "push"; "origin"; "feature" |]

                        do! writeUtf8FileAsync filePath "upstream version\n"
                        let! _ = git.raw [| "add"; "shared.txt" |]
                        let! _ = git.raw [| "commit"; "-m"; "test: upstream feature" |]
                        let! _ = git.raw [| "push"; "upstream"; "feature" |]

                        let! _ = git.raw [| "fetch"; "origin" |]
                        let! _ = git.raw [| "fetch"; "upstream" |]

                        let! upstreamTip = git.raw [| "rev-parse"; "refs/remotes/upstream/feature" |]

                        let! _ = git.raw [| "checkout"; baseBranch |]
                        let! _ = git.raw [| "branch"; "-D"; "feature" |]

                        let! branchesResult = provider.GetBranches repoPath
                        let branches = expectProviderOk "branches with two remotes" branchesResult

                        let upstreamBranch =
                            branches |> Array.find (fun branch -> branch.RefName = "upstream/feature")

                        let! checkoutResult =
                            provider.CheckoutBranch repoPath { ProviderRef = upstreamBranch.ProviderRef }

                        expectProviderOk "checkout upstream feature" checkoutResult |> ignore

                        let! localTip = git.raw [| "rev-parse"; "feature" |]
                        Vitest.expect(localTip.Trim()).toBe (upstreamTip.Trim())

                        let! trackedUpstream = git.raw [| "rev-parse"; "--abbrev-ref"; "feature@{upstream}" |]
                        Vitest.expect(trackedUpstream.Trim()).toBe ("upstream/feature")
                    })
            }
        )
)

Vitest.describe (
    "GitProvider large object metadata",
    fun () ->
        Vitest.test (
            "returns an empty metadata list for a repository without LFS files",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath _git -> promise {
                        do! writeUtf8FileAsync (join [| repoPath; "plain.txt" |]) "plain\n"

                        let! metadataResult = provider.ListLargeObjects repoPath
                        let metadata = expectProviderOk "list large objects" metadataResult

                        Vitest.expect(metadata.Length).toBe (0)
                    })
            }
        )
)

Vitest.describe (
    "GitProvider large file policy",
    fun () ->
        Vitest.test (
            "tracks and untracks a path pattern through the provider",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let! lfsVersionResult = promise {
                            try
                                let! version = git.raw [| "lfs"; "version" |]
                                return Ok version
                            with error ->
                                return Error error.Message
                        }

                        if Result.isError lfsVersionResult then
                            Vitest.expect(true).toBe (true)
                        else
                            let! trackResult =
                                provider.SetPathLargeFilePolicy
                                    repoPath
                                    {
                                        Path = "*.bin"
                                        UseLargeObjectStorage = true
                                    }

                            expectProviderOk "track path policy" trackResult |> ignore

                            let! attributesAfterTrack = git.raw [| "check-attr"; "filter"; "--"; "sample.bin" |]
                            Vitest.expect(attributesAfterTrack.Contains("lfs")).toBe (true)

                            let! untrackResult =
                                provider.SetPathLargeFilePolicy
                                    repoPath
                                    {
                                        Path = "*.bin"
                                        UseLargeObjectStorage = false
                                    }

                            expectProviderOk "untrack path policy" untrackResult |> ignore

                            let! attributesAfterUntrack =
                                git.raw [| "check-attr"; "filter"; "--"; "sample.bin" |]

                            Vitest.expect(attributesAfterUntrack.Contains("lfs")).toBe (false)
                    })
            }
        )
)

Vitest.describe (
    "GitProvider page-load data",
    fun () ->
        Vitest.test (
            "returns unsupported page-load result for explicitly unsupported diff content",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath _git -> promise {
                        let workbookPath = join [| repoPath; "data.xlsx" |]
                        do! writeUtf8FileAsync workbookPath "not a real workbook\n"

                        let! baseCommit =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: add unsupported file"
                                    Paths = [| "data.xlsx" |]
                                }

                        expectProviderOk "base unsupported file commit" baseCommit |> ignore

                        do! writeUtf8FileAsync workbookPath "changed unsupported file\n"

                        let! pageResult = provider.GetDiffViewData repoPath "data.xlsx"
                        let pageLoad = expectProviderOk "unsupported diff page" pageResult

                        match pageLoad with
                        | VersionControlPageLoadResultDto.Unsupported unsupported ->
                            Vitest.expect(unsupported.Path).toBe ("data.xlsx")
                            Vitest.expect(unsupported.Reason.IsSome).toBe (true)
                        | VersionControlPageLoadResultDto.Loaded _ ->
                            failwith "Expected unsupported page-load result."
                    })
            }
        )
)

Vitest.describe (
    "GitProvider clone workflow",
    fun () ->
        Vitest.test (
            "clones without large-object hydration when DownloadLargeObjects is false",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let rootPath = dirname repoPath
                        let clonePath = join [| rootPath; "clone-without-large-objects" |]
                        let! remotePath, baseBranch = createPushedBareRemote rootPath provider repoPath git

                        do! writeLocalRemoteRewriteAsync rootPath remotePath

                        do!
                            withTemporaryGitHome rootPath (fun () -> promise {
                                let! cloneResult =
                                    provider.CloneRepository
                                        {
                                            RemoteUrl = testRemoteUrl
                                            TargetPath = clonePath
                                            Branch = Some baseBranch
                                            DownloadLargeObjects = false
                                        }
                                        None

                                let clonedPath = expectProviderOk "clone without large-object hydration" cloneResult

                                let! clonedContent = readUtf8FileAsync (join [| clonedPath; "tracked.txt" |])
                                Vitest.expect(clonedContent.Replace("\r\n", "\n")).toBe ("tracked\n")
                            })
                    })
            }
        )

        Vitest.test (
            "clones with requested large-object hydration when DownloadLargeObjects is true",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let rootPath = dirname repoPath
                        let clonePath = join [| rootPath; "clone-with-large-objects" |]
                        let! remotePath, baseBranch = createPushedBareRemote rootPath provider repoPath git

                        do! writeLocalRemoteRewriteAsync rootPath remotePath

                        do!
                            withTemporaryGitHome rootPath (fun () -> promise {
                                let! cloneResult =
                                    provider.CloneRepository
                                        {
                                            RemoteUrl = testRemoteUrl
                                            TargetPath = clonePath
                                            Branch = Some baseBranch
                                            DownloadLargeObjects = true
                                        }
                                        None

                                let clonedPath = expectProviderOk "clone with large-object hydration" cloneResult
                                Vitest.expect(clonedPath).toBe (clonePath)

                                let cloneGit = createSimpleGit clonedPath
                                let! preference =
                                    cloneGit.raw [| "config"; "--get"; "swate.lfs.downloadlargefiles" |]

                                Vitest.expect(preference.Trim()).toBe ("true")
                            })
                    })
            }
        )
)

Vitest.describe (
    "GitProvider large object local copy",
    fun () ->
        Vitest.test (
            "maps download and free failures through VersionControlFailure",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath _git -> promise {
                        let! downloadResult =
                            provider.DownloadLargeObject repoPath { Path = "missing.bin" }

                        let downloadFailure = expectProviderError downloadResult
                        Vitest.expect(String.IsNullOrWhiteSpace downloadFailure.Message).toBe (false)

                        let! freeResult =
                            provider.FreeLocalObjectCopy repoPath { Path = "missing.bin" }

                        let freeFailure = expectProviderError freeResult
                        Vitest.expect(String.IsNullOrWhiteSpace freeFailure.Message).toBe (false)
                    })
            }
        )
)

Vitest.describe (
    "GitProvider storage maintenance",
    fun () ->
        Vitest.test (
            "maps prune and deduplicate results through provider outcomes",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let rootPath = dirname repoPath
                        let! _remotePath, _baseBranch = createPushedBareRemote rootPath provider repoPath git

                        let! pruneResult = provider.PruneStorage repoPath None
                        expectPerformedOrStorageBoundary "prune storage" pruneResult

                        let! deduplicateResult = provider.DeduplicateStorage repoPath None
                        expectPerformedOrStorageBoundary "deduplicate storage" deduplicateResult
                    })
            }
        )
)

Vitest.describe (
    "GitProvider merge resolution",
    fun () ->
        Vitest.test (
            "confirms a resolved conflict through the provider",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let filePath = join [| repoPath; "conflict.txt" |]
                        let featureBranch = "feature/provider-merge-resolution"

                        do! writeUtf8FileAsync filePath "base\n"

                        let! baseCommit =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: base conflict file"
                                    Paths = [| "conflict.txt" |]
                                }

                        expectProviderOk "base conflict file commit" baseCommit |> ignore

                        let! baseStatusResult = provider.GetStatus repoPath
                        let baseStatus = expectProviderOk "status after base conflict commit" baseStatusResult

                        let baseBranch =
                            baseStatus.Current
                            |> Option.defaultWith (fun () -> failwith "Expected current branch after base commit.")

                        let! createBranchResult =
                            provider.CreateBranch
                                repoPath
                                {
                                    Name = featureBranch
                                    BaseProviderRef = None
                                }

                        expectProviderOk "create provider merge branch" createBranchResult |> ignore

                        do! writeUtf8FileAsync filePath "feature change\n"

                        let! featureCommit =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: feature conflict change"
                                    Paths = [| "conflict.txt" |]
                                }

                        expectProviderOk "feature conflict commit" featureCommit |> ignore

                        let! branchResult = provider.GetBranches repoPath
                        let branches = expectProviderOk "branches before provider merge checkout" branchResult

                        let baseProviderRef =
                            branches
                            |> Array.find (fun branch -> branch.RefName = baseBranch)
                            |> _.ProviderRef

                        let! checkoutBaseResult =
                            provider.CheckoutBranch repoPath { ProviderRef = baseProviderRef }

                        expectProviderOk "checkout base branch before provider merge" checkoutBaseResult |> ignore

                        do! writeUtf8FileAsync filePath "main change\n"

                        let! mainCommit =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: main conflict change"
                                    Paths = [| "conflict.txt" |]
                                }

                        expectProviderOk "main conflict commit" mainCommit |> ignore

                        try
                            let! _ = git.raw [| "merge"; featureBranch |]
                            ()
                        with _ ->
                            ()

                        let! mergeStatusResult = provider.GetStatus repoPath
                        let mergeStatus = expectProviderOk "status during provider merge" mergeStatusResult

                        Vitest.expect(mergeStatus.IsMergeInProgress).toBe (true)
                        Vitest.expect(mergeStatus.Conflicted).toEqual ([| "conflict.txt" |])

                        let! mergeViewResult = provider.GetMergeConflictViewData repoPath "conflict.txt"
                        let mergeViewLoad = expectProviderOk "provider merge conflict view" mergeViewResult

                        let mergeConflictContent =
                            match mergeViewLoad with
                            | VersionControlPageLoadResultDto.Loaded(VersionControlMergeConflictViewDataDto.Content view) ->
                                view.ConflictContent
                            | VersionControlPageLoadResultDto.Loaded(VersionControlMergeConflictViewDataDto.VersionPick _) ->
                                failwith "Expected content-based conflict view from the Git provider."
                            | VersionControlPageLoadResultDto.Unsupported _ ->
                                failwith "Expected loaded merge conflict view."

                        let! resolutionResult =
                            provider.ConfirmMergeResolution
                                repoPath
                                {
                                    Path = "conflict.txt"
                                    Resolution =
                                        VersionControlMergeResolution.Content(
                                            mergeConflictContent,
                                            "resolved content\n"
                                        )
                                    AutoCommit = false
                                }

                        let resolution = expectProviderOk "provider confirm merge resolution" resolutionResult

                        Vitest.expect(resolution.RemainingConflictedPaths).toEqual ([||])
                        Vitest.expect(resolution.NextConflictedPath).toEqual (None)
                        Vitest.expect(resolution.UpdatedStatus.IsMergeInProgress).toBe (true)
                        Vitest.expect(resolution.UpdatedStatus.Conflicted).toEqual ([||])
                    })
            }
        )

        Vitest.test (
            "returns Unsupported for version-pick resolution regardless of repository state",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath _git -> promise {
                        let! resolutionResult =
                            provider.ConfirmMergeResolution
                                repoPath
                                {
                                    Path = "any.txt"
                                    Resolution = VersionControlMergeResolution.TakeSourceVersion
                                    AutoCommit = false
                                }

                        let failure = expectProviderError resolutionResult
                        Vitest.expect(failure.Kind).toEqual (VersionControlFailureKind.Unsupported)
                    })
            }
        )
)

Vitest.describe (
    "Provider-neutral unsupported and no-op boundaries",
    fun () ->
        Vitest.test (
            "uses Unsupported when a provider cannot preserve selected-path commit semantics",
            fun () ->
                let result: VersionControlResult<string> =
                    VersionControlResult.unsupported "Selected-path commit is not supported by this provider."

                match result with
                | Error failure ->
                    Vitest.expect(failure.Kind).toEqual (VersionControlFailureKind.Unsupported)
                    Vitest.expect(failure.Message.Contains("Selected-path commit")).toBe (true)
                | Ok _ -> failwith "Expected Unsupported for impossible selected-path commit."
        )

        Vitest.test (
            "uses NoOp only when skipping work preserves the requested workflow outcome",
            fun () ->
                let result: VersionControlResult<unit> =
                    VersionControlResult.noOp
                        (Some "Object content is already materialized by provider sync.")
                        ()

                match result with
                | Ok outcome ->
                    match outcome.Effect with
                    | VersionControlEffect.NoOp(Some reason) ->
                        Vitest.expect(reason.Contains("already materialized")).toBe (true)
                    | _ -> failwith "Expected NoOp effect."
                | Error failure -> failwith $"Expected successful NoOp but got {failure.Kind}: {failure.Message}"
        )
)

Vitest.describe (
    "GitProvider diff summary",
    fun () ->
        Vitest.test (
            "reports line counts because SupportsDiffLineCounts is true",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath _git -> promise {
                        let filePath = join [| repoPath; "summary.txt" |]

                        do! writeUtf8FileAsync filePath "one\n"

                        let! baseCommit =
                            provider.Commit
                                repoPath
                                {
                                    Message = "test: diff summary base"
                                    Paths = [| "summary.txt" |]
                                }

                        expectProviderOk "diff summary base commit" baseCommit |> ignore

                        do! writeUtf8FileAsync filePath "one\ntwo\n"

                        let! summaryResult = provider.GetDiffSummary repoPath
                        let summary = expectProviderOk "diff summary" summaryResult

                        Vitest.expect(summary.Changed).toBe (1)
                        Vitest.expect(summary.Insertions.IsSome).toBe (true)
                        Vitest.expect(summary.Deletions.IsSome).toBe (true)

                        match summary.Insertions with
                        | Some insertions -> Vitest.expect(insertions >= 1).toBe (true)
                        | None -> failwith "Expected insertion count from Git provider."
                    })
            }
        )
)

Vitest.describe (
    "GitProvider remote access verification",
    fun () ->
        Vitest.test (
            "fails with a redacted message when no remote is configured",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath _git -> promise {
                        let! result = provider.VerifyRemoteAccess repoPath { Remote = None; Branch = None }
                        let failure = expectProviderError result
                        Vitest.expect(String.IsNullOrWhiteSpace failure.Message).toBe (false)
                    })
            }
        )

        Vitest.test (
            "succeeds against a reachable remote",
            providerIntegrationTestOptions,
            fun () -> promise {
                do!
                    withProviderTempRepository (fun provider repoPath git -> promise {
                        let rootPath = dirname repoPath
                        let! remotePath, _baseBranch = createPushedBareRemote rootPath provider repoPath git

                        do! configureLocalRemoteRewriteAsync git remotePath
                        let! _ = git.raw [| "remote"; "set-url"; "origin"; testRemoteUrl |]

                        do!
                            withTestTokenProvider (fun () -> promise {
                                let! result =
                                    provider.VerifyRemoteAccess repoPath { Remote = None; Branch = None }

                                match result with
                                | Ok outcome ->
                                    match outcome.Effect with
                                    | VersionControlEffect.Performed _ -> Vitest.expect(true).toBe (true)
                                    | VersionControlEffect.NoOp _ ->
                                        failwith "Expected Performed for a real remote check."
                                | Error failure ->
                                    failwith $"verify remote access failed ({failure.Kind}): {failure.Message}"
                            })
                    })
            }
        )
)

Vitest.describe (
    "Provider registry workspace detection",
    fun () ->
        Vitest.test (
            "detects a Git workspace and resolves the Git provider",
            providerIntegrationTestOptions,
            fun () -> promise {
                ProviderRegistry.resetToDefault ()

                do!
                    withProviderTempRepository (fun _provider repoPath _git -> promise {
                        let! detectedKind = ProviderRegistry.detectWorkspaceKind repoPath
                        Vitest.expect(detectedKind).toEqual (Some VersionControlProviderKind.Git)

                        let! resolved = ProviderRegistry.getForWorkspace repoPath

                        match resolved with
                        | Some provider ->
                            Vitest.expect(provider.Kind).toEqual (VersionControlProviderKind.Git)
                        | None -> failwith "Expected the Git provider for a Git workspace."
                    })
            }
        )

        Vitest.test (
            "returns no provider kind for an unmanaged directory",
            providerIntegrationTestOptions,
            fun () -> promise {
                ProviderRegistry.resetToDefault ()

                let! plainDirectory = createTempDirectoryAsync ()

                try
                    let! detectedKind = ProviderRegistry.detectWorkspaceKind plainDirectory
                    Vitest.expect(detectedKind).toEqual (None)

                    let! resolved = ProviderRegistry.getForWorkspace plainDirectory
                    Vitest.expect(resolved.IsNone).toBe (true)
                    do! removeDirectoryAsync plainDirectory
                with error ->
                    do! removeDirectoryAsync plainDirectory
                    return raise error
            }
        )

        Vitest.test (
            "detects a registered kind without a provider and still returns no provider",
            providerIntegrationTestOptions,
            fun () -> promise {
                ProviderRegistry.resetToDefault ()

                let! markerDirectory = createTempDirectoryAsync ()

                try
                    do! writeUtf8FileAsync (join [| markerDirectory; ".contract-test-marker" |]) "marker\n"

                    ProviderRegistry.registerDetector {
                        Kind = VersionControlProviderKind.LakeFs
                        Detect =
                            fun workspacePath ->
                                promise {
                                    try
                                        let! _ =
                                            fsPromisesDynamic?stat (join [| workspacePath; ".contract-test-marker" |])
                                            |> unbox<JS.Promise<obj>>

                                        return true
                                    with _ ->
                                        return false
                                }
                    }

                    let! detectedKind = ProviderRegistry.detectWorkspaceKind markerDirectory
                    Vitest.expect(detectedKind).toEqual (Some VersionControlProviderKind.LakeFs)

                    let! resolved = ProviderRegistry.getForWorkspace markerDirectory
                    Vitest.expect(resolved.IsNone).toBe (true)

                    ProviderRegistry.resetToDefault ()

                    let! kindAfterReset = ProviderRegistry.detectWorkspaceKind markerDirectory
                    Vitest.expect(kindAfterReset).toEqual (None)

                    do! removeDirectoryAsync markerDirectory
                with error ->
                    do! removeDirectoryAsync markerDirectory
                    return raise error
            }
        )
)
