module VersionControlService.Tests.GitProviderContractTests

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Bindings.SimpleGit
open VersionControlService.Contracts.VersionControl
open VersionControlService.Git.GitAuthAdapter
open VersionControlService.Tests.NodePath
open VersionControlService.Tests.ProviderContractSuite

module GitProvider = VersionControlService.Git.GitProvider

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-contract-tests-" |]
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

let private readUtf8FileAsync (path: string) : JS.Promise<string> =
    fsPromisesDynamic?readFile (path, "utf8") |> unbox<JS.Promise<string>>

let private createGitWorkspace () : JS.Promise<ProviderWorkspace> = promise {
    let! rootPath = createTempDirectoryAsync ()
    let repoPath = join [| rootPath; "repo" |]
    let provider = GitProvider.create ()

    let! initResult =
        provider.InitializeWorkspace {
            TargetPath = repoPath
            ProviderRemoteUri = None
            ProviderOptions = [||]
        }

    let workspacePath =
        match initResult with
        | Ok outcome -> outcome.Value
        | Error failure ->
            failwith $"contract workspace init failed ({failure.Kind}): {failure.Message}"

    let git =
        SimpleGit.create (
            SimpleGitOptions(baseDir = workspacePath, binary = U3.Case1 "git", maxConcurrentProcesses = 1)
        )
        |> applyNonInteractiveEnv

    let! _ = git.raw [| "config"; "user.name"; "VersionControlService Contract Tests" |]
    let! _ = git.raw [| "config"; "user.email"; "contract-tests@example.org" |]
    let! _ = git.raw [| "config"; "core.autocrlf"; "false" |]

    return {
        Provider = provider
        WorkspacePath = workspacePath
        WriteFile = fun relativePath content -> writeUtf8FileAsync (join [| workspacePath; relativePath |]) content
        ReadFile = fun relativePath -> readUtf8FileAsync (join [| workspacePath; relativePath |])
        Dispose = fun () -> removeDirectoryAsync rootPath
    }
}

registerProviderContractTests {
    ProviderName = "Git"
    CreateWorkspace = createGitWorkspace
}
