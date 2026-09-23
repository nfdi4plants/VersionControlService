# Workspace bindings and resolution

The host owns binding persistence. VersionControlService does not keep an application settings database and does not write a generic binding file into managed workspaces.

## Binding contents

`WorkspaceBinding` contains:

- the schema version and provider ID
- the normalized workspace root
- an optional opaque provider state reference
- a nonsecret repository location
- an optional connection profile ID

Do not serialize access keys, tokens, passwords, credential-bearing URLs, process arguments, or provider clients into a binding. The host resolves `ConnectionProfileId` through the credential strategy injected into the selected factory.

## Resolution order

Create a catalog with `ProviderResolver.tryCreateCatalog`. Duplicate provider IDs are rejected.

For each workspace request:

1. Look up the host-persisted binding by normalized workspace root.
2. Pass that binding to `ProviderResolver.resolve`. An explicit binding selects its registered factory, and probes are not consulted.
3. Without a binding, inspect the resolution report. One nearest owning root is a candidate, equal-depth candidates are ambiguous, and no candidates means unmanaged.
4. If the user or host policy accepts a unique candidate, call that factory's `Adopt` method.
5. Persist the returned binding in the host store, then call `Open`.

Probing is a hint. It must not mutate provider or host state. Ambiguity requires an explicit provider choice. Registration order is not a tie-breaker.

The host supplies `CaseSensitive` or `CaseInsensitive` path comparison to match its filesystem. Resolution normalizes separators and trailing separators for root comparison, but it preserves case and Unicode spelling.

## Adoption and initialization

Adoption registers an existing provider-owned workspace without cloning. The provider derives whatever repository location it can verify. If it cannot derive a safe binding, it returns `Unsupported` rather than guessing.

Initialization is for a directory that the provider does not already own. Providers preserve existing user files and report them as ordinary workspace changes. Clone retains the stricter missing-or-empty destination rule. If a workspace is already initialized, use adoption.

## External provider state

`ProviderStateRef` is opaque to the host. The host persists it unchanged and gives it back to `Open`.

Providers that need mutable indexes, transactions, or recovery data keep them outside the managed workspace. The lakeFS factory requires a host-selected `StateRoot`, and every state reference resolves beneath that root and outside every workspace. Missing, corrupt, mismatched, or in-workspace state fails structurally. The provider does not recreate state silently.

Keeping this boundary matters for two reasons: provider metadata cannot collide with repository objects, and a failed materialization can recover without treating transaction files as user content.

## Session ownership

An opened session owns its locks, clients, and caches. Call `WorkspaceSession.Close` when the host releases it. Do not reuse a session for a different binding or connection profile. Multiple sessions can exist at once. Mutation safety comes from workspace-version and conflict-handle checks, not global singleton state.
