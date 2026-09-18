# Provider authoring

An external provider needs one project reference or package reference: `VersionControlService.Abstractions`. It should not depend on the Node runtime, Git, lakeFS, or the umbrella unless it uses those implementations directly. The sample in [`samples/ExternalProvider`](../samples/ExternalProvider/Provider.fs) is a compiling core-only factory.

## Factory lifecycle

`ProviderFactory` is the provider entry point. Register one factory per `ProviderId` in a host-created `ProviderCatalog`.

- `Probe` is read-only detection. It must not throw, create a binding, or claim a root it does not own.
- `VerifyLocation` checks explicit access intents against a repository location.
- `Initialize` creates provider state in place and preserves existing user files.
- `Clone` requires a missing or empty destination.
- `Adopt` derives a binding from an existing provider-owned workspace without cloning. Return an `Unsupported` failure if the provider cannot do that safely.
- `Bind` attaches or retargets an existing local workspace without cloning.
- `Open` creates one session for one persisted binding.
- `CheckDependencies` reports installed, missing, and incompatible components.
- `InstallDependency` performs only remediation the provider genuinely supports. Unsupported installation is a structured failure.

Factories receive credentials and provider settings through constructor arguments. Do not use global mutable registries. A factory can open multiple independent sessions, including sessions with different connection profiles.

The Git provider also accepts a `GitIdentityStrategy` extension point for host-supplied revision attribution. Its `ResolveIdentity` function receives a `RevisionIdentityRequest` and is called for each operation that may create a revision. The result is not cached when a session opens. The request carries the workspace root and the session's connection profile. Its `TargetHost` is the host of the effective push URL of the current branch's publish remote (pushurl over url, with `insteadOf` rewriting applied), or of the bound location when no remote is configured or HEAD is detached. It is `None` when the upstream configuration is invalid or when no host can be read from the value, which includes local paths and file URLs. The provider lowercases hosts and drops IPv6 brackets. It passes percent-encoded and internationalized names through as written. Credentials follow the URL git connects to for each command. Fetch, ls-remote, LFS downloads and the first fetch after Initialize use the remote's effective fetch URL. The push and LFS uploads use its effective push URL. Clone and location verification use the `insteadOf`-expanded location. The push credential and the identity therefore resolve against the same host, so an application that keeps accounts per host and connection profile can return the identity that matches where the revision will be published. Credential and identity strategies must return `None` when they have nothing to offer. An exception thrown by a strategy escapes the operation that called it. Returning `Some` supplies the author and committer name and email for that operation. Returning `None` uses the repository's configured `user.name` and `user.email`. If neither a strategy identity nor a complete repository identity is available, the operation fails before mutation with the stable validation code `identity_missing` and a recovery action describing how to configure Git or supply the strategy.

A canceled Git `Update` reports what a killed `git merge` left behind and mutates nothing on its own except one case: when the merge wrote MERGE_HEAD, the provider runs `git merge --abort`, which is git's own rollback. Everything else comes back as a `Canceled` failure with a `RecoveryAction`. `remove_index_lock` means an `index.lock` is present (with `StateChanged` false when it already existed before the update). `abort_merge` means `git merge --abort` failed. `refresh_workspace` means the merge finished before the cancellation landed. `restore_workspace` means a fast-forward may have rewritten the paths in `AffectedPaths`, which the application reviews and restores or keeps. `inspect_workspace` means the workspace differs from its pre-merge state in a way the provider could not attribute, or git could not report it. On Windows a killed git process does not remove its lock, so `remove_index_lock` is the usual outcome of a mid-merge cancellation and the application needs a path for it.

## Session contract

Every `WorkspaceSession` supplies `CoreVersionControl`: status, refs, switching, selected-path revisions, restoration, and diff summaries. `WorkspaceSession.createCoreOnly` is the shortest correct starting point for a provider with no extensions.

Optional services advertise real capability:

| Service | Purpose |
|---|---|
| `Synchronization` | refresh, preview update, update, and publish |
| `TextDiff` | text, word, and committed-base content |
| `ConflictResolution` | provider-owned conflict sessions and stale-handle checks |
| `ObjectMaterialization` | list, hydrate, and dematerialize large or lazy objects |
| `StoragePolicy` | literal-path policy and host-facing settings |
| `Maintenance` | prune and deduplicate local storage |
| `RepositoryBrowser` | a credential-free repository URL |

Leave an unsupported service as `None`. Do not add a service whose methods are required stubs that always return `Unsupported`.

## Results and concurrency

Return `OperationResult<'T>` from every operation:

- `Succeeded` includes the value, whether work was performed, affected paths, resulting revision and workspace version, warnings, and publication state.
- `PartiallySucceeded` means state changed before a later step failed. It must include a recovery action.
- `Failed` carries a stable category and provider-extensible code. Set `StateChanged` when mutation may have happened, even if verification was inconclusive.

Workspace versions are opaque optimistic-concurrency tokens. Conflict handles have a separate lifetime and rotate after each successful nonterminal resolution. Validate both tokens where the request carries both. A stale request must fail before further mutation.

## Cancellation, progress, and redaction

Use the supplied `OperationContext` throughout the operation. Check cancellation between bounded units of work and pass it into process, network, and streaming helpers. Report progress with stable phase codes; byte totals use `float` so values above two GiB remain exact across .NET and JavaScript.

Messages, details, progress text, bindings, and repository URLs must not contain secrets. Resolve credentials from the binding's `ConnectionProfileId`, scope them to the operation, and pass provider output through the redaction guard before returning it.

## Storage settings reseed

A host that already stores a large-object threshold and materialization preference can hand them to a provider once through the public service. Read the host-owned values, write both in one settings record, and mark the host migration complete only after success:

```fsharp
let reseedStorageSettings
    (session: WorkspaceSession)
    (savedThresholdMb: int option)
    (savedMaterializeLargeObjects: bool)
    (context: OperationContext)
    = async {
        match session.StoragePolicy with
        | None -> return OperationResult.noOp (Some "storage policy unavailable") ()
        | Some policy ->
            return!
                policy.SetSettings
                    {
                        AutoPolicyThresholdMb = savedThresholdMb
                        MaterializeLargeObjects = savedMaterializeLargeObjects
                    }
                    context
    }
```

The provider owns its storage keys and escaping rules. Hosts should not read or write provider configuration directly.

## Compatibility

The published records are intentionally small. Add new capability as a new optional service record instead of adding required members to an existing published record. Provider-specific mechanics stay behind the SPI; a host should not need provider-specific parsing to resolve, adopt, open, or operate a workspace.

Run the shared conformance profiles before publishing an external provider. The harness contract and profile expectations are described in [conformance-profiles.md](conformance-profiles.md).
