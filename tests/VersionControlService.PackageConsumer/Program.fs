module VersionControlService.PackageConsumer.Program

open VersionControlService.Abstractions
open Fable.Core
open Fable.Core.JsInterop

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsProviderOptions = VersionControlService.LakeFs.LakeFsProviderOptions
module LakeFsWorkspaceSession = VersionControlService.LakeFs.LakeFsWorkspaceSession
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path
module ProviderResolver = VersionControlService.Abstractions.ProviderResolver

[<Import("tmpdir", "node:os")>]
let private temporaryDirectory () : string = jsNative

[<Import("mkdtempSync", "node:fs")>]
let private makeTemporaryDirectory (_prefix: string) : string = jsNative

[<Import("rmSync", "node:fs")>]
let private removeTemporaryDirectory (_path: string) (_options: NodeFileSystem.RmOptions) : unit = jsNative

let private runGitOperation (factory: ProviderFactory) =
    async {
        let targetPath =
            NodePath.join [| temporaryDirectory (); "version-control-service-consumer-" |]
            |> makeTemporaryDirectory

        let context = OperationContext.detached "package-consumer-git-operation"

        try
            let! initialized =
                factory.Initialize
                    { TargetPath = targetPath; Location = None }
                    context

            match initialized with
            | Succeeded initialization ->
                let! opened = factory.Open initialization.Value context

                match opened with
                | Succeeded outcome ->
                    let session = outcome.Value
                    let! status = Async.Catch(session.Core.GetStatus context)
                    let! closed = Async.Catch(session.Close())

                    return
                        match status, closed with
                        | Choice1Of2(Succeeded _), Choice1Of2() -> true
                        | _ -> false
                | _ -> return false
            | _ -> return false
        finally
            removeTemporaryDirectory targetPath (NodeFileSystem.RmOptions(recursive = true, force = true))
    }

[<EntryPoint>]
let main _ =
    let gitFactory =
        GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none

    let lakeFsOptions: LakeFsProviderOptions.LakeFsProviderOptions = {
        StateRoot = ".version-control-service-package-consumer-state"
        PathCaseSensitivity = CaseInsensitive
    }

    let lakeFsFactory =
        LakeFsWorkspaceSession.createFactory lakeFsOptions LakeFsCredentials.unconfigured

    let operation =
        match ProviderResolver.tryCreateCatalog [ gitFactory; lakeFsFactory ] with
        | Ok catalog when ProviderResolver.factories catalog |> Array.length = 2 -> runGitOperation gitFactory
        | Ok _
        | Error _ -> async.Return false

#if FABLE_COMPILER
    Async.StartImmediate(async {
        let! result = Async.Catch operation

        let code =
            match result with
            | Choice1Of2 true -> 0
            | _ -> 1

        Fable.Core.JsInterop.emitJsStatement code "process.exitCode = $0"
    })
    0
#else
    try
        if Async.RunSynchronously operation then 0 else 1
    with _ ->
        1
#endif
