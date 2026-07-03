module VersionControlService.VersionControlProviderRegistry

open Fable.Core
open VersionControlService.Contracts.VersionControl

/// Detects whether a local workspace directory is owned by a provider kind.
/// Detectors must be cheap, must not mutate the workspace, and must return
/// false (not throw) for directories they do not recognize.
type WorkspaceProviderDetector = {
    Kind: VersionControlProviderKind
    Detect: string -> JS.Promise<bool>
}

let private defaultProvider () =
    VersionControlService.Git.GitProvider.create ()

let private gitWorkspaceDetector: WorkspaceProviderDetector = {
    Kind = VersionControlProviderKind.Git
    Detect =
        fun workspacePath ->
            promise {
                // A Git workspace has a .git directory, or a .git file for worktrees.
                return
                    VersionControlService.Runtime.Node.FileSystem.existsSync (
                        VersionControlService.Runtime.Node.Path.join [| workspacePath; ".git" |]
                    )
            }
}

let private defaultProviders () =
    Map.ofList [ VersionControlProviderKind.Git, defaultProvider () ]

let mutable private registeredProviders = defaultProviders ()
let mutable private registeredDetectors = [ gitWorkspaceDetector ]
let mutable private selectedProvider = defaultProvider ()

/// Process-wide selected provider for hosts that manage a single workspace.
let get () : VersionControlProvider = selectedProvider

let set (provider: VersionControlProvider) =
    selectedProvider <- provider
    registeredProviders <- registeredProviders |> Map.add provider.Kind provider

/// Makes a provider resolvable by kind without changing the process-wide selection.
let registerProvider (provider: VersionControlProvider) =
    registeredProviders <- registeredProviders |> Map.add provider.Kind provider

/// Registers or replaces the workspace detector for a provider kind.
let registerDetector (detector: WorkspaceProviderDetector) =
    registeredDetectors <-
        (registeredDetectors
         |> List.filter (fun existing -> existing.Kind <> detector.Kind))
        @ [ detector ]

let tryGetProvider (kind: VersionControlProviderKind) : VersionControlProvider option =
    registeredProviders |> Map.tryFind kind

/// Returns the provider kind owning the workspace, or None for unmanaged directories.
let detectWorkspaceKind (workspacePath: string) : JS.Promise<VersionControlProviderKind option> =
    let rec loop detectors = promise {
        match detectors with
        | [] -> return None
        | detector: WorkspaceProviderDetector :: rest ->
            let! matches = detector.Detect workspacePath

            if matches then
                return Some detector.Kind
            else
                return! loop rest
    }

    loop registeredDetectors

/// Resolves the provider for a workspace by detection. Returns None when the
/// directory is unmanaged or no provider is registered for the detected kind.
let getForWorkspace (workspacePath: string) : JS.Promise<VersionControlProvider option> = promise {
    let! detectedKind = detectWorkspaceKind workspacePath
    return detectedKind |> Option.bind tryGetProvider
}

let resetToDefault () =
    let gitProvider = defaultProvider ()
    selectedProvider <- gitProvider
    registeredProviders <- Map.ofList [ VersionControlProviderKind.Git, gitProvider ]
    registeredDetectors <- [ gitWorkspaceDetector ]
