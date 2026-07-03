module VersionControlService.VersionControlProviderRegistry

open VersionControlService.Contracts.VersionControl

let private defaultProvider () =
    VersionControlService.Git.GitProvider.create ()

let mutable private selectedProvider = defaultProvider ()

let get () : VersionControlProvider = selectedProvider

let set (provider: VersionControlProvider) =
    selectedProvider <- provider

let resetToDefault () =
    selectedProvider <- defaultProvider ()
