module VersionControlService.PackageConsumer.Program

open VersionControlService.Abstractions

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsProviderOptions = VersionControlService.LakeFs.LakeFsProviderOptions
module LakeFsWorkspaceSession = VersionControlService.LakeFs.LakeFsWorkspaceSession
module ProviderResolver = VersionControlService.Abstractions.ProviderResolver

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

    match ProviderResolver.tryCreateCatalog [ gitFactory; lakeFsFactory ] with
    | Ok catalog when ProviderResolver.factories catalog |> Array.length = 2 -> 0
    | Ok _
    | Error _ -> 1
