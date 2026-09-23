# Consuming the library

A host that puts version control over a local workspace needs four things from this
library: a factory for each provider it supports, a catalog holding those factories, a
binding for the workspace, and an opened session. This page builds those four in order, and
then covers the two ways to handle services a provider does not have.

Writing a provider is a different job, covered in [provider authoring](provider-authoring.md).

## What the host owns

The library stores nothing of its own. There is no settings store, no credential store, no
UI state, and no registry of known workspaces. The host keeps:

- the bindings, one for each workspace root it manages
- the credentials, behind a strategy the factory calls when it needs one
- the choice to adopt a workspace a probe found, and the choice to retry after a failure
- the workspace version it last showed the user, which it passes back on the next mutation

The library owns provider mechanics. Nothing in a host should parse provider output, read
provider configuration files, build provider command lines, or reimplement provider rules.

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
    // The host's own filesystem semantics. CaseSensitive on Linux.
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

Git also wants an identity to attribute revisions to.
`Git.createFactoryWithCredentialsAndIdentity` takes a `GitIdentityStrategy` alongside the
credentials, and its `ResolveIdentity` runs for each operation that may create a revision.
Returning `None` falls back to the repository's configured `user.name` and `user.email`.
When the strategy gives nothing and the repository has no complete identity either, the
revision fails before it mutates anything, with the validation code `identity_missing` and
a `configure_git_identity` recovery. A host on a machine that has never had git configured
needs either the strategy or a path that walks the user through configuring git.

lakeFS resolves a whole connection, endpoint plus access keys, from the profile:

```fsharp
let lakeFsFactory =
    LakeFs.createFactory lakeFsOptions (LakeFsCredentials.fixedConnection connection)
```

`LakeFsCredentials.unconfigured` refuses every profile and is the right default until
the user has configured one.

## Naming a repository

A `RepositoryLocation` is nonsecret provider-owned location data plus the profile that
resolves its credentials. `ProviderLocation` is whatever that provider understands. Git
takes a clone URL or a local path. lakeFS takes `lakefs://repository/ref` with an optional
prefix after it.

```fsharp
let gitLocation: RepositoryLocation = {
    ProviderId = ProviderId.tryCreate WellKnownProviderIds.Git |> Result.defaultWith invalidOp
    DisplayName = Some "Study archive"
    ProviderLocation = "https://github.com/example/study-archive.git"
    // The key the credential strategy will be asked for. None means anonymous.
    ConnectionProfileId = None
}

let lakeFsLocation: RepositoryLocation = {
    ProviderId = ProviderId.tryCreate WellKnownProviderIds.LakeFs |> Result.defaultWith invalidOp
    DisplayName = Some "Study archive"
    ProviderLocation = "lakefs://study-archive/main"
    ConnectionProfileId = Some "lakefs-production"
}
```

The lakeFS connection the profile resolves to is endpoint plus keys:

```fsharp
let connection: VersionControlService.LakeFs.LakeFsTypes.LakeFsConnection = {
    Endpoint = "https://lakefs.example.org"
    AccessKeyId = accessKeyId
    SecretAccessKey = secretAccessKey
}
```

Secrets belong here, behind the strategy, and never in the location or the binding.

## Creating a workspace

Four factory operations produce a binding, and none of them needs an opened session. Three are
here, and `Adopt` belongs to the next section. Each one returns the binding for the host to
persist, after which `Open` works exactly as in the next section.

`Clone` wants a destination that is missing or empty:

```fsharp
let cloneWorkspace (factory: ProviderFactory) location targetPath (context: OperationContext) = async {
    let! result =
        factory.Clone
            {
                Location = location
                TargetPath = targetPath
                // None checks out whatever the provider treats as the default ref.
                TargetRef = None
                // False leaves large objects as pointers until something asks for them.
                MaterializeAllObjects = false
            }
            context

    match result with
    | Succeeded outcome
    | PartiallySucceeded(outcome, _) -> return Ok outcome.Value
    | Failed failure -> return Error failure
}
```

`Initialize` takes a directory the provider does not already own, keeps the files already in
it, and reports them as ordinary workspace changes. `Location` is optional in the contract and
Git accepts `None`, so a Git workspace can start with no target and get one through `Bind`
later. lakeFS requires one and answers `Validation` with `location_required` without it:

```fsharp
let! result = factory.Initialize { TargetPath = targetPath; Location = Some location } context
```

`Bind` attaches or retargets a local workspace without cloning, which is the answer to
`publish_target_missing`:

```fsharp
let! result = factory.Bind { WorkspaceRoot = workspaceRoot; Location = location } context
```

`Adopt` is the fourth, and it is for a workspace the provider already owns. The next section
uses it, and it is the one a provider is most likely to refuse.

## Checking the provider can run

A provider that shells out to external tools cannot work until those tools are installed.
Ask before the user hits a failure in the middle of a save. The Git provider reports on git
itself, on Git LFS, and on whether the LFS filter is configured.

Only git is required. Git LFS is optional, and the services that use it (object
materialization, storage policy and maintenance) report their own dependency status when it
is absent. A host that treats a missing `git-lfs` as fatal refuses workspaces that would
have worked, so separate the two:

```fsharp
let checkTools (factory: ProviderFactory) (context: OperationContext) = async {
    let! result = factory.CheckDependencies context

    match result with
    | Failed failure -> return Error failure.Message
    | Succeeded outcome
    | PartiallySucceeded(outcome, _) ->
        let unusable (status: DependencyStatus) =
            not status.Installed || not status.Compatible

        // git missing means nothing works. Anything else only narrows what works.
        let blocking =
            outcome.Value
            |> Array.filter (fun status -> status.Component = "git" && unusable status)

        let degraded =
            outcome.Value
            |> Array.filter (fun status -> status.Component <> "git" && unusable status)

        return Ok(blocking, degraded)
}
```

Each `DependencyStatus` names the component, whether it is installed, the version it found,
whether that version is compatible, and a `Remediation` string to show the user. Git reports
three components: `git`, `git-lfs`, and `git-lfs-configuration` for whether the LFS filter
is set up. Show the remediation text for a degraded component and let the user carry on.

`InstallDependency` takes a component name and attempts the remediation the provider
supports. Git supports only `git-lfs-configuration` and answers anything else
with a structured `Unsupported` failure, so a host shows the `Remediation` text for git
and Git LFS themselves and offers a button only for the filter. Treat a successful
install as the exception and the message as the normal path.

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
    | PartiallySucceeded(outcome, failure) ->
        // The session is usable and something went wrong opening it. A host that keeps
        // this short is dropping a recovery action, so report the failure somewhere
        // before returning the session.
        reportOpenWarning failure
        return Ok outcome.Value
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
                // Decides whether a detected root owns this path. CaseSensitive on Linux.
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
| `Bound` | The stored binding named a registered provider, so the resolver ran no probes. | Open it. |
| `ProbedWorkspace` | One provider owns the nearest root. | Ask the user, call `Adopt`, persist the binding, open. `Adopt` may answer `Unsupported`. |
| `AmbiguousWorkspace` | Several providers claim the same root. | Ask which one. Registration order is not a tie-breaker. |
| `UnmanagedWorkspace` | No provider claims it, or the stored binding named a provider the catalog does not hold. | Read the diagnostics before treating it as a fresh directory. Then offer `Initialize` or `Clone`. |

A probe never breaks resolution, and neither does a binding the catalog cannot serve. Both
become data in `report.Diagnostics`. A probe contributes its own failure when it returns
`ProbeFailed`, `probe_exception` when it throws, `probe_invalid` when it reports an invalid
workspace, and `probe_root_outside_workspace` when it claims a root that does not own the
path. The binding path contributes `provider_not_registered` when a stored binding names a
provider the catalog does not hold. Only the `ProbeFailed` entry carries a provider's own
code, so treat that one by its category.

`Adopt` is allowed to refuse. A provider that cannot derive a binding it can verify answers
with the `Unsupported` category, and the code differs by provider: lakeFS reports
`operation_not_supported` and Git reports `adoption_unsupported` for a workspace whose
metadata it cannot parse. Branch on the category, because the two providers reach similar
behavior through different mechanisms. The built-in lakeFS factory always refuses, even
though it probes the `.lakefs_ref.yaml` marker and reports a candidate, so a probed lakeFS
workspace never adopts. Fall back to `Bind` with a location the user supplies, or to `Clone`
into a fresh directory.

The `provider_not_registered` case is the one to handle first. A workspace whose provider
was dropped from the catalog resolves as `UnmanagedWorkspace` carrying that diagnostic,
and it is still a managed workspace holding someone's history. A host that offers
`Initialize` on it without reading the diagnostics is offering to initialize over a
repository.

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
    // State changed before a later step failed. The contract obliges the provider to
    // attach a recovery action here, and the field stays an option on the type.
    showRecovery failure.RecoveryAction

| Failed failure ->
    match failure.Category with
    | Concurrency -> refreshAndRetry ()
    | Authentication -> askForCredentials ()
    | _ -> showMessage failure.Message
```

`Category` is a closed set a host can branch on. `Code` is an open string a provider extends,
so treat an unknown code as its category. `StateChanged` says whether provider state may have
moved before the failure, which decides whether the host can keep showing its last known
status or has to re-read it. `Retryable` says whether sending the same request again may
succeed without further changes, which is what decides whether the host offers a retry button.
`AffectedPaths` and `RevisionEvidence` name what the failure was about. Messages and details
arrive redacted, so a host can show them to a user.

`OperationOutcome.Publication` is the field to read before telling anyone their work is safe.
`Published` means the result is visible on the logical target. `LocalOnly` means the revisions
exist locally and a publish can still be retried. `PublicationNotApplicable` means the
operation has no publication meaning at all, so it is not a claim that anything was published.

`ResultingWorkspaceVersion` is optional and a provider may leave it `None`. The built-in Git
provider never sets it, and lakeFS does, so do not build a host around it. Two payloads carry
a workspace version: `Core.GetStatus` and `SwitchRef`, which both return a `WorkspaceStatus`.
The synchronization operations return a `SynchronizationState`, which has revisions and a
target but no workspace version, so call `Core.GetStatus` after a synchronize to get the token
for the next mutation. The token is opaque either way, so do not parse it or compare it for
ordering.

## Optional services

Every session has `Descriptor`, `Core` and `Close`. The seven services are `option`, and a
provider leaves one absent when it has nothing to offer there. Feature discovery is an
`Option.isSome` check, and there are two reasonable ways to write a host against that.

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

Every filled operation succeeds as a no-op carrying a `service_unavailable` warning,
apart from the two groups named further down. A fallback read hands back an empty answer:
`ListObjects` an empty array, the text diff reads `UnsupportedContent`, `GetActiveSession`
and `GetRepositoryWebUrl` `None`, `GetSettings` no threshold with
`MaterializeLargeObjects = true`, and `Prune` and `Deduplicate` the reason as their report.

A fallback write also succeeds and changes nothing. `Materialize`, `Dematerialize`,
`SetPathPolicy` and `SetSettings` return `Succeeded` with the same warning. Read the
warning before you treat one of these as done. The storage settings reseed in
[provider authoring](provider-authoring.md) shows why this matters. It tells a host to
write its saved threshold through `SetSettings` and mark the migration complete only
after success. On a filled session that success stored nothing, so a host doing that
migration should check the warning or keep the unwrapped session for it.

A caller that wants to tell a real answer from a filled one reads the warnings:

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

Two groups of operations report the failure and do no quiet work: the whole
synchronization service, and the conflict mutations `Resolve`, `Finalize` and `Cancel`.
Both change what a user believes about where their work is. A publish that succeeded as a
no-op would tell someone their work is safe on a server that never received it.

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

Paths are exact repository-relative keys. `RepositoryPath.tryCreate` rejects an empty
string, NUL, a backslash anywhere, an absolute or drive-rooted path, an empty segment, a
trailing separator, and any `.` or `..` segment. The two a Windows host hits in practice
are the drive-rooted one and the trailing separator, because `C:/data/x.csv` contains no
backslash and `data/` looks harmless. The drive-rooted rule tests the shape of the string
on every platform, so a Linux file named `a:b.csv` at the repository root is rejected too.
Forward slash is the only separator. Anything the rules do not reject is a literal
filename byte, with no Unicode normalization, no case folding and no wildcard expansion,
so a file genuinely named `report [draft].csv` round-trips.

```fsharp
/// What the host needs back from a save: the revision when one was created, and the
/// token the next mutation has to carry.
type SaveResult = {
    Revision: RevisionId option
    WorkspaceVersion: string option
}

let saveSelected
    (session: WorkspaceSession)
    (selected: string[])
    (message: string)
    (expectedWorkspaceVersion: string)
    (context: OperationContext)
    =
    async {
        let parsed = selected |> Array.map (fun raw -> raw, RepositoryPath.tryCreate raw)
        let rejected = parsed |> Array.filter (fun (_, result) -> Result.isError result)

        if rejected.Length > 0 then
            let names = rejected |> Array.map fst |> String.concat ", "
            return Error $"These are not repository-relative paths: {names}"
        else
            let! result =
                session.Core.CreateRevision
                    {
                        Message = message
                        Paths = parsed |> Array.choose (snd >> Result.toOption)
                        ExpectedWorkspaceVersion = expectedWorkspaceVersion
                    }
                    context

            match result with
            | Succeeded outcome ->
                match outcome.Effect with
                | NoOp _ ->
                    // Nothing needed committing, and the workspace version has not moved.
                    return Ok { Revision = None; WorkspaceVersion = Some expectedWorkspaceVersion }
                | Performed ->
                    // CreateRevision is an OperationResult<RevisionId>, so the new revision is
                    // outcome.Value. ResultingRevision is optional metadata that a conforming
                    // provider may leave as None, so do not read the revision out of it.
                    //
                    // The commit moved the workspace version, and ResultingWorkspaceVersion is
                    // None on Git. None here means the host re-reads the status before the
                    // next mutation. It does not mean the version is unchanged.
                    return
                        Ok {
                            Revision = Some outcome.Value
                            WorkspaceVersion = outcome.ResultingWorkspaceVersion
                        }

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
workspace that moved underneath rejects the save with `Concurrency`, so nobody commits
work they never saw.

`Paths` are exact literal keys. A provider that receives `data/*.csv` looks for a file
whose name contains an asterisk.

## Switching refs without losing work

`PreflightSwitchRef` takes the same request as `SwitchRef` and reports whether the switch
is safe. Ask first, because the switch itself will not negotiate. Git runs a plain
`git checkout`, so git refuses a switch that would overwrite local changes and the provider
returns `checkout_failed`. `SwitchRefRequest` carries only the target ref and the expected
workspace version, so there is no way to say "switch anyway". Deal with the paths first.

```fsharp
let switchRef (session: WorkspaceSession) (request: SwitchRefRequest) (context: OperationContext) = async {
    let! preflight = session.Core.PreflightSwitchRef request context

    match preflight with
    | Failed failure -> return Error failure.Message
    | Succeeded outcome
    | PartiallySucceeded(outcome, _) ->
        if not outcome.Value.IsSafe then
            // PathsAtRisk names the files that block the switch. The host commits them with
            // CreateRevision or throws them away with RestorePaths. Either one moves the
            // workspace version, so re-read the status and build a fresh request. Resending
            // this one fails. There is no flag that forces the switch.
            return Error(sprintf "%d paths block the switch" outcome.Value.PathsAtRisk.Length)
        else
            let! switched = session.Core.SwitchRef request context

            match switched with
            | Succeeded outcome
            | PartiallySucceeded(outcome, _) -> return Ok outcome.Value
            | Failed failure -> return Error failure.Message
}
```

`PathsAtRisk` is the overlap between the paths that carry local changes and the paths that
differ between the current revision and the target, which is the set the checkout would
have to overwrite. `IsSafe` false is therefore a prediction that the checkout will be
refused, and it names the files to deal with first.

`request.TargetRef` is a `ProviderRef`, and the place to get one is the `ProviderRef` field
of a `LogicalRef` that `ListRefs` returned. It is opaque, so pass it back unchanged.

The rest of `Core` follows the same shape. `ListRefs` and `CreateRef` cover the branch
list and branch creation, `RestorePaths` throws away local changes on exact paths, and
`GetDiffSummary` gives per-path entries whose line counts are optional because an
object-oriented provider cannot always compute them.

## Synchronizing

`Synchronization` is optional, so a provider that has no remote leaves it absent. The
service separates observing from mutating. `Refresh` observes the target and changes no
workspace content. `PreviewUpdate` reports what an update would change. `Update` and
`Publish` are the two mutations, and `Synchronize` runs a refresh, the update the
workspace needs, and the publish of its revisions as one operation under the provider's
lock.

Prefer `Synchronize` for a button that means "bring me up to date". It refuses when it cannot
decide safely, and the refusal says what the host has to ask the user. Keep
`PublishLocalRevisions` true unless the user asked to update without publishing. A missing
`TargetRef` is not a reason to turn it off, because Git derives `TargetRef` from the branch's
upstream while its publish falls back to the `origin` remote, so a branch created with
`checkout -b` has no `TargetRef` and still publishes:

```fsharp
let synchronize
    (session: WorkspaceSession)
    (expectedWorkspaceVersion: string)
    // The target the user was looking at when they decided. A target that moved since then
    // fails before anything mutates. It is required when acceptedConflictSession is true.
    (observedTarget: RevisionId option)
    // The user saw the preview and accepted that the update opens a conflict session. This
    // is consent, and it is a separate question from which target they saw.
    (acceptedConflictSession: bool)
    (publishLocalRevisions: bool)
    (context: OperationContext)
    =
    async {
        match session.Synchronization with
        | None -> return Error "This provider has no synchronization service."
        | Some synchronization ->
            let! result =
                synchronization.Synchronize
                    {
                        ExpectedWorkspaceVersion = expectedWorkspaceVersion
                        ExpectedTargetRevision = observedTarget
                        AcceptUpdateRisks = acceptedConflictSession
                        // Leave this true and let publish_target_missing tell you there is
                        // nowhere to publish. Git publishes through origin when the branch has
                        // no upstream, so a missing TargetRef does not mean publishing fails.
                        PublishLocalRevisions = publishLocalRevisions
                    }
                    context

            match result with
            | Succeeded outcome -> return Ok outcome.Value
            | PartiallySucceeded(outcome, failure) ->
                // The update landed and the publish did not. Do not report this as success:
                // the user's revisions are still local. The recovery is usually retry_publish.
                let recovery =
                    failure.RecoveryAction
                    |> Option.map (fun action -> action.Code)
                    |> Option.defaultValue "none"

                return Error $"Updated, but not published ({failure.Code}). Recovery: {recovery}."
            | Failed failure when failure.Code = SynchronizationCodes.UpdateWouldOverwriteLocalChanges ->
                // AcceptUpdateRisks does not override this one. The user saves or discards
                // the paths in AffectedPaths first.
                return Error "Save or discard your changes to these files first."
            | Failed failure when failure.Code = SynchronizationCodes.UpdateWouldCreateConflictSession ->
                // Show the preview, then call again with acceptedConflictSession true and the
                // target revision the failure reported in RevisionEvidence.
                return Error "This update opens a conflict session. Ask the user to accept."
            | Failed failure when failure.Code = "publish_target_missing" ->
                // Nowhere to publish. Offer Bind to give the workspace a target.
                return Error "This workspace has no publication target yet."
            | Failed failure -> return Error failure.Message
    }
```

Two refusals need the host to do something specific.
`update_would_overwrite_local_changes` carries the `resolve_local_changes` recovery and
names the overlapping paths, and setting `AcceptUpdateRisks` does not get past it.
`update_would_create_conflict_session` carries `accept_update_risks`, and the retry has
to supply both `AcceptUpdateRisks` and the `ExpectedTargetRevision` the evidence
reported. Setting `AcceptUpdateRisks` without a target revision fails with
`acceptance_target_required`, which exists so an acceptance can never apply to a target
the user never saw. `conflict_session_active` means an open conflict session has to be
resolved or cancelled first. Git also reports `publish_target_missing` as a `Validation`
failure before it touches the network, which means the workspace has no publication
target and the host has to bind one.

A `PartiallySucceeded` result here is its own outcome and not a success. It means the update
applied and the publish did not, so the revisions are local while the workspace is up to
date. The recovery is `retry_publish` unless the provider supplied its own. Show that
difference, because a host that folds this into success tells the user their work is on the
server when it is not. The provider codes and evidence are tabulated in
[provider authoring](provider-authoring.md).

## Conflict sessions

When an update leaves conflicts, the provider opens a session and
`WorkspaceStatus.ActiveConflictSession` reports it. `GetActiveSession` returns the
summary, with one `ConflictItem` per path and the candidates that path offers.

Resolution is one path at a time, and each success rotates the handle:

```fsharp
let resolvePath
    (session: WorkspaceSession)
    (handle: ConflictSessionHandle)
    (expectedWorkspaceVersion: string)
    (path: RepositoryPath)
    (choice: ConflictResolution)
    (context: OperationContext)
    =
    async {
        match session.ConflictResolution with
        | None -> return Error "This provider has no conflict resolution service."
        | Some conflicts ->
            let! result =
                conflicts.Resolve
                    {
                        Handle = handle
                        ExpectedWorkspaceVersion = expectedWorkspaceVersion
                        Path = path
                        Resolution = choice
                    }
                    context

            match result with
            | Succeeded outcome
            | PartiallySucceeded(outcome, _) ->
                // Keep RefreshedHandle. The one you passed in is now stale, so replaying an
                // earlier choice fails. Nothing is redone silently.
                return Ok(outcome.Value.RefreshedHandle, outcome.Value.RemainingItems)
            | Failed failure -> return Error failure.Message
    }
```

`ConflictSessionHandle.Version` rotates after every successful nonterminal resolution and
has a different lifetime from the workspace version, so carry both and never collapse
them into one field. A stale, foreign, or closed handle is rejected before any provider
state changes, with category `Concurrency`, code `precondition_failed`, and the
`refresh_conflict_session` recovery. That recovery means: call `GetActiveSession` again
and rebuild the view from the summary it returns.

Pick a candidate with `PickCandidate candidateId`, using an id the item advertises.
Supply merged text with `SupplyResolvedContent`, and only when the item's
`SupportsResolvedContent` is true. `Finalize` needs every item resolved and returns the
revision it created, which is `None` when the provider closed the session without needing
a second commit. `Cancel` abandons the session. Both close the handle.

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

`OperationContext.create` catches and drops exceptions from the progress callback. A
callback that reports to a closed window cannot fail the operation it watches.

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

A canceled Git update needs real handling. On Windows a killed git
process leaves its `index.lock` behind, so `remove_index_lock` is the usual recovery code
and a host needs a path for it. The recovery codes a Git update can report are listed in
[provider authoring](provider-authoring.md).

`OperationProgress` carries a stable `PhaseCode` such as `transfer`, an optional item,
completed and total counts, and a sanitized `DisplayMessage`. Counts are `float` because
JavaScript has no native 64-bit integer, so a float is what carries byte totals above two
GiB exactly on both runtimes. Show the message, and branch on the
phase code.

## Building against the packages

The packages are not on nuget.org yet. Until they are, produce them locally and restore
from that feed:

```console
dotnet restore VersionControlService.slnx
dotnet run --project build/Build.fsproj -- pack --version=0.0.1-local --output=<feed-dir>
dotnet nuget add source <feed-dir> --name vcs-local
dotnet add package VersionControlService --version 0.0.1-local
```

Run this from the repository root, because `pack` resolves the repository root from the
current directory. The restore on the first line is not optional: `pack` packs the five
projects with `--no-restore`, and running the build project does not restore them, so a fresh
clone fails without it.

`pack` empties the output directory first, so it refuses a path inside the repository, a
volume root, or anything reached through a junction. Pick a directory outside the clone.

`pack` builds five coordinated packages, and the umbrella depends on the other four at
that exact version. Referencing the umbrella is enough. A host that only defines or
consumes contracts references `VersionControlService.Abstractions` alone and needs
nothing else. Once the packages are published, `dotnet add package VersionControlService
--prerelease` replaces the three commands above.

### What the built-in providers need at runtime

The contracts in `VersionControlService.Abstractions` are portable and compile on .NET and
on Fable. The Git and lakeFS providers are different: they go through
`VersionControlService.Runtime.Node`, which imports Node modules, so they run on Fable and
Node. Referencing the package compiles, and executing a built-in provider needs the rest
of that toolchain.

A Fable host also needs the npm side and a Fable compile step:

```console
npm install simple-git
dotnet new tool-manifest
dotnet tool install fable --version 5.5.0
dotnet tool run fable YourApp.fsproj --outDir output
node output/<your-entry-file>.js
```

Fable is a dotnet tool, so a consumer needs its own manifest. Pin the version, because this
guide describes 5.5.0, which is what `.config/dotnet-tools.json` pins here. CI runs
`dotnet tool restore` before any target that compiles with it, which is why the command works
in this repository without the install step. Fable names each output file after its source
file, so the entry point is the compiled name of your entry `.fs` file.

The Git provider shells out to the real tools on top of that. It requires Git 2.38 or newer,
because it uses `merge-tree --write-tree`. Git LFS 3.7 or newer is optional, and without it
the object materialization, storage policy and maintenance services report their own
dependency status while core Git work carries on. `CheckDependencies` reports all of that at
runtime, which is why it is worth calling before the user gets far. lakeFS needs no local
tool and talks to its server over HTTP.

Two files in this repository are worth copying from.
`tests/VersionControlService.PackageConsumer/Program.fs` is the smallest composition root
that builds against the packed packages, and `samples/ExternalProvider/Provider.fs` is a
compiling core-only provider.

Related reading: [workspace bindings and resolution](workspace-bindings.md) for binding
persistence, and [provider authoring](provider-authoring.md) for the revision path policy
a host hands to a factory (`RevisionPolicyStrategy`) and for the storage settings reseed
recipe.
