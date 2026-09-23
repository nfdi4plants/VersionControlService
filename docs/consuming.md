# Consuming the library

A host that puts version control over a local workspace needs four things from this
library: a factory for each provider it supports, a catalog holding those factories, a
binding for the workspace, and an opened session. Everything below builds those four in
order, and then covers the two ways to handle services a provider does not have.

Writing a provider is a different job, covered in [provider authoring](provider-authoring.md).

## What the host owns

The library has no settings store, no credential store, and no UI state. The host keeps:

- the bindings, one for each workspace root it manages
- the credentials, behind a strategy the factory calls when it needs one
- the choice to adopt a workspace a probe found, and the choice to retry after a failure
- the workspace version it last showed the user, which it passes back on the next mutation

The library owns provider mechanics. Nothing in a host should parse provider output,
read provider configuration files, or build provider command lines.

## The composition root

Factories are ordinary values. Build them once, at startup, with host-owned
configuration, then put them in an immutable catalog.

```fsharp
open VersionControlService.Abstractions

module Git = VersionControlService.Git.GitWorkspaceSession
module LakeFsCredentials = VersionControlService.LakeFs.LakeFsCredentials
module LakeFsOptions = VersionControlService.LakeFs.LakeFsProviderOptions
module LakeFs = VersionControlService.LakeFs.LakeFsWorkspaceSession
module Resolver = VersionControlService.Abstractions.ProviderResolver

let gitFactory = Git.createFactory Git.GitSessionHooks.none

let lakeFsOptions: LakeFsOptions.LakeFsProviderOptions = {
    StateRoot = applicationStateDirectory
    PathCaseSensitivity = CaseInsensitive
}

let lakeFsFactory =
    LakeFs.createFactory lakeFsOptions LakeFsCredentials.unconfigured

let catalog =
    Resolver.tryCreateCatalog [ gitFactory; lakeFsFactory ]
    |> Result.defaultWith invalidOp
```

`tryCreateCatalog` rejects duplicate provider IDs. `StateRoot` is where the lakeFS
provider keeps its own state, and it has to sit outside every managed workspace.
`PathCaseSensitivity` is how that provider compares local paths, which is how it checks
that a state directory really sits under `StateRoot` and not inside a workspace. Pass
the semantics of the host's filesystem.

### Credentials

`Git.createFactory` uses the anonymous credential strategy, which covers public HTTPS,
an SSH agent, and local paths. A host holding tokens passes its own strategy. It receives
the host of the remote git will contact and the connection profile from the binding:

```fsharp
module GitCredentials = VersionControlService.Git.GitCredentialStrategy

let credentials: GitCredentials.GitCredentialStrategy = {
    ResolveCredential =
        fun targetHost profileId ->
            async {
                match tokenStore.TryFind(targetHost, profileId) with
                | Some token -> return Some { Username = "x-access-token"; Secret = token }
                | None -> return None
            }
}

let gitFactory = Git.createFactoryWithCredentials Git.GitSessionHooks.none credentials
```

Returning `None` means anonymous. Secrets stay inside the strategy and never reach a
binding, a message, or progress output.

lakeFS resolves a whole connection, endpoint plus access keys, from the profile:

```fsharp
let lakeFsFactory =
    LakeFs.createFactory lakeFsOptions (LakeFsCredentials.fixedConnection connection)
```

`LakeFsCredentials.unconfigured` refuses every profile and is the right default until
the user has configured one.

## Opening a workspace

Resolution answers one question: which provider owns this directory. A binding the host
already stored answers it directly. Without one, the resolver asks every registered
factory to probe.

```fsharp
/// What the host has to decide for itself, because the library cannot.
type OpenFailure =
    | Unmanaged
    | Ambiguous of Resolver.DetectionCandidate[]
    | ProviderFailed of OperationFailure

let openBinding (factory: ProviderFactory) binding context = async {
    let! opened = factory.Open binding context

    match opened with
    | Succeeded outcome -> return Ok outcome.Value
    | PartiallySucceeded(outcome, _) -> return Ok outcome.Value
    | Failed failure -> return Error(ProviderFailed failure)
}

let openWorkspace
    (catalog: Resolver.ProviderCatalog)
    (storedBinding: WorkspaceBinding option)
    (workspaceRoot: string)
    (context: OperationContext)
    =
    async {
        let! report =
            Resolver.resolve catalog {
                WorkspacePath = workspaceRoot
                ExplicitBinding = storedBinding
                PathCaseSensitivity = CaseInsensitive
            }

        match report.Resolution with
        | Resolver.Bound(binding, factory) ->
            let! session = openBinding factory binding context
            return session |> Result.map (fun session -> session, binding)

        | Resolver.ProbedWorkspace(factory, candidate) ->
            // A probe is a hint. Adoption is the explicit step that produces a binding,
            // and the host decides whether to take it.
            let! adopted =
                factory.Adopt
                    {
                        WorkspaceRoot = candidate.Root
                        ConnectionProfileId = None
                    }
                    context

            match adopted with
            | Failed failure -> return Error(ProviderFailed failure)
            | Succeeded outcome
            | PartiallySucceeded(outcome, _) ->
                let binding = outcome.Value
                let! session = openBinding factory binding context
                return session |> Result.map (fun session -> session, binding)

        | Resolver.AmbiguousWorkspace candidates -> return Error(Ambiguous candidates)
        | Resolver.UnmanagedWorkspace -> return Error Unmanaged
    }
```

The four resolutions and what a host does with each:

| Resolution | Meaning | What the host does |
|---|---|---|
| `Bound` | The stored binding named a registered provider. Probes were skipped. | Open it. |
| `ProbedWorkspace` | One provider owns the nearest root. | Ask the user, call `Adopt`, persist the binding, open. |
| `AmbiguousWorkspace` | Several providers claim the same root. | Ask which one. Registration order is not a tie-breaker. |
| `UnmanagedWorkspace` | No provider claims it. | Offer `Initialize` or `Clone`. |

`report.Diagnostics` carries structured failures from probes that failed or threw. A
probe never breaks resolution, so read the diagnostics when a workspace you expected to
be detected comes back unmanaged.

Persist the binding `Adopt` returned, keyed by the normalized workspace root, and give it
back unchanged on the next open. Details are in
[workspace bindings and resolution](workspace-bindings.md).

`Close` returns `Async<unit>`, so await it when the host releases the session:
`do! session.Close()`. A session owns locks, clients, and caches that outlive the call
that created it.

## Reading a result

Every operation returns `OperationResult<'T>` with three cases.

```fsharp
match result with
| Succeeded outcome ->
    // outcome.Value is the payload. outcome.Effect says whether work happened.
    match outcome.Effect with
    | Performed -> showRevision outcome.ResultingRevision
    | NoOp reason -> showNothingToDo reason

| PartiallySucceeded(outcome, failure) ->
    // State changed before a later step failed. There is always a recovery action.
    showRecovery failure.RecoveryAction

| Failed failure ->
    match failure.Category with
    | Concurrency -> refreshAndRetry ()
    | Authentication -> askForCredentials ()
    | _ -> showMessage failure.Message
```

`Category` is a closed set a host can branch on. `Code` is an open string a provider
extends, so treat an unknown code as its category. `StateChanged` says whether provider
state may have moved before the failure, which decides whether the host can keep showing
its last known status or has to re-read it. `AffectedPaths` and `RevisionEvidence` name
what the failure was about. Messages and details arrive redacted, so a host can show
them to a user.

`ResultingWorkspaceVersion` is the token to pass into the next mutation. It is opaque.
Do not parse it, compare it for ordering, or keep it past the view it belongs to.

## Optional services

Every session has `Core`. The rest are `option`, and a provider leaves one absent when it
has nothing to offer there. Feature discovery is an `Option.isSome` check, and there are
two reasonable ways to write a host against that.

### Direct: an absent service stays absent

The host checks presence and enables the matching part of its interface. Someone using a
provider without synchronization never sees a publish button.

```fsharp
let describe (session: WorkspaceSession) (context: OperationContext) = async {
    let services = WorkspaceSession.availability session

    let! status = session.Core.GetStatus context

    match status with
    | Failed failure -> return Error(failure.Category, failure.Code, failure.Message)
    | Succeeded outcome
    | PartiallySucceeded(outcome, _) ->
        let changedCount = outcome.Value.Changes.Length

        let! webUrl =
            match session.RepositoryBrowser with
            | Some browser -> async {
                let! result = browser.GetRepositoryWebUrl context

                return
                    match result with
                    | Succeeded outcome -> outcome.Value
                    | PartiallySucceeded(outcome, _) -> outcome.Value
                    | Failed _ -> None
              }
            | None -> async.Return None

        return Ok(changedCount, services.Synchronization, webUrl)
}
```

`WorkspaceSession.availability` returns a `ServiceAvailability` record with one boolean
per service, which is easier to hand to a view model than seven options.

### Filled: every service present, no-ops where the provider has nothing

A host that would rather write one code path fills the gaps.
`ProviderFactory.withFallbackServices` wraps a factory so every session it opens has
every service. Wrap at the composition root, before building the catalog, and no later
code checks presence again.

```fsharp
let createCatalog (factories: ProviderFactory seq) =
    factories
    |> Seq.map ProviderFactory.withFallbackServices
    |> Resolver.tryCreateCatalog
```

A fallback read returns an empty answer and marks it. `ListObjects` gives an empty array,
the text diff reads give `UnsupportedContent`, `GetActiveSession` and
`GetRepositoryWebUrl` give `None`, `GetSettings` gives no threshold with
`MaterializeLargeObjects = true`, and `Prune` and `Deduplicate` give the reason as their
report. Each one succeeds as a no-op carrying a `service_unavailable` warning, so a
caller that wants to tell a real answer from a filled one reads the warnings:

```fsharp
let private isFallback (warnings: OperationWarning[]) =
    warnings
    |> Array.exists (fun warning -> warning.Code = FallbackServiceCodes.ServiceUnavailable)

let repositoryUrl (session: WorkspaceSession) (context: OperationContext) = async {
    // Safe on a wrapped factory: every session it opens has every service.
    let browser = session.RepositoryBrowser |> Option.get

    let! result = browser.GetRepositoryWebUrl context

    match result with
    | Succeeded outcome -> return Ok(outcome.Value, isFallback outcome.Warnings)
    | PartiallySucceeded(outcome, _) -> return Ok(outcome.Value, isFallback outcome.Warnings)
    | Failed failure -> return Error failure
}
```

Two groups of operations fail instead of succeeding quietly: the whole synchronization
service, and the conflict mutations `Resolve`, `Finalize` and `Cancel`. Both change what
a user believes about where their work is. A publish that succeeded as a no-op would tell
someone their work is safe on a server that never received it.

```fsharp
match result with
| Failed failure when failure.Code = FallbackServiceCodes.ServiceUnavailable ->
    // The provider has no synchronization at all. Hide the action for this workspace.
    hidePublish ()
| Failed failure -> showMessage failure.Message
| Succeeded outcome
| PartiallySucceeded(outcome, _) -> showState outcome.Value
```

`service_unavailable` means the provider has no such service. A provider that has the
service and cannot perform one particular request reports its own
`operation_not_supported`. The two are different answers and a host can treat them
differently.

### Reading availability and still filling

A filled session reports every service as present, and a wrapped factory never yields an
unfilled one. A host that wants the honest report and the uniform code path keeps the
unwrapped factory and fills each session itself, after reading the report:

```fsharp
let openWithReport (factory: ProviderFactory) binding (context: OperationContext) = async {
    let! opened = factory.Open binding context

    match opened with
    | Failed failure -> return Error failure
    | Succeeded outcome
    | PartiallySucceeded(outcome, _) ->
        let services = WorkspaceSession.availability outcome.Value
        return Ok(WorkspaceSession.withFallbackServices outcome.Value, services)
}
```

### Choosing

Fill the gaps when the host is generic over providers and a missing feature should
degrade quietly. Keep them absent when the interface changes shape per provider, or when
a host must never offer an action the provider cannot carry out. Two hosts can make
opposite choices against the same provider, which is the intent.

## Saving work

Paths are exact repository-relative keys. `RepositoryPath.tryCreate` rejects absolute
paths, backslashes, and `.` or `..` segments. It never normalizes Unicode, folds case, or
reads a wildcard, so a file genuinely named `report [draft].csv` round-trips.

```fsharp
let saveSelected
    (session: WorkspaceSession)
    (selected: string[])
    (message: string)
    (expectedWorkspaceVersion: string)
    (context: OperationContext)
    =
    async {
        let paths, rejected =
            selected
            |> Array.map (fun raw -> raw, RepositoryPath.tryCreate raw)
            |> Array.partition (fun (_, parsed) -> Result.isOk parsed)

        if rejected.Length > 0 then
            let names = rejected |> Array.map fst |> String.concat ", "
            return Error $"These are not repository-relative paths: {names}"
        else
            let! result =
                session.Core.CreateRevision
                    {
                        Message = message
                        Paths = paths |> Array.map (snd >> Result.defaultWith failwith)
                        ExpectedWorkspaceVersion = expectedWorkspaceVersion
                    }
                    context

            match result with
            | Succeeded outcome ->
                match outcome.Effect with
                | NoOp reason -> return Ok(None, reason)
                | Performed -> return Ok(outcome.ResultingRevision, None)

            | PartiallySucceeded(_, failure) ->
                let recovery =
                    failure.RecoveryAction
                    |> Option.map (fun action -> action.Code)
                    |> Option.defaultValue "none"

                return Error $"Partly saved ({failure.Code}). Recovery: {recovery}."

            | Failed failure when failure.Category = Concurrency ->
                return Error "The workspace moved. Refresh the status and try again."

            | Failed failure -> return Error failure.Message
    }
```

`ExpectedWorkspaceVersion` is the version from the status the host last showed. A
workspace that moved underneath rejects the save with `Concurrency` instead of committing
something the user never saw.

`Paths` are exact literal keys, never patterns. A provider that receives
`data/*.csv` looks for a file with that name.

## Cancellation and progress

Every operation takes an `OperationContext`. `OperationContext.detached` is enough for a
call nobody will cancel or watch.

```fsharp
let context = OperationContext.detached "status-1"
```

For anything a user can stop or watch, build the context from a cancellation source the
host keeps, plus a progress callback:

```fsharp
let createContext (operationId: string) (onProgress: OperationProgress -> unit) =
    let source = OperationCancellation.Source()
    let context = OperationContext.create operationId source.Cancellation onProgress
    context, source
```

The host calls `source.Cancel()` from wherever the user asks, such as a stop button. A
canceled operation comes back as a `Canceled` failure that describes what the
cancellation left behind:

```fsharp
| Failed failure when failure.Category = Canceled ->
    let recovery =
        failure.RecoveryAction
        |> Option.map (fun action -> action.Code)
        |> Option.defaultValue "none"

    // StateChanged says whether the workspace still matches what the user last saw.
    return Error $"Canceled. State changed: {failure.StateChanged}. Recovery: {recovery}."
```

A canceled Git update is the case worth handling properly. On Windows a killed git
process leaves its `index.lock` behind, so `remove_index_lock` is the usual recovery code
and a host needs a path for it. The recovery codes a Git update can report are listed in
[provider authoring](provider-authoring.md).

`OperationProgress` carries a stable `PhaseCode` such as `transfer`, an optional item,
completed and total counts, and a sanitized `DisplayMessage`. Byte totals use `float` so
values above two GiB stay exact in JavaScript. Show the message, and branch on the phase
code.

## Building against the packages

The umbrella package brings the abstractions, the Node runtime, and both providers at one
coordinated version:

```console
dotnet add package VersionControlService --prerelease
```

The Git and lakeFS providers run on Fable and Node. A .NET consumer can reference the
packages and compile against the contracts, and the built-in providers need the Node
runtime to execute. A host that only defines or consumes contracts references
`VersionControlService.Abstractions` alone.

Two files in this repository are worth copying from.
`tests/VersionControlService.PackageConsumer/Program.fs` is the smallest composition root
that builds against the published packages, and `samples/ExternalProvider/Provider.fs` is
a compiling core-only provider.
