module VersionControlService.LakeFs.LakeFsSynchronization

open VersionControlService.Abstractions

let private repositoryPaths (paths: string list) =
    paths
    |> List.choose (RepositoryPath.tryCreate >> Result.toOption)
    |> List.toArray

/// Adds a provider-neutral, object-level remote diff to a live synchronization
/// snapshot. Revision counts remain absent because lakeFS does not expose Git
/// ahead/behind semantics.
let withRemoteChangedPaths (changedPaths: string list) (state: SynchronizationState) =
    {
        state with
            LocalRevisionCount = None
            TargetRevisionCount = None
            RemoteChangedPaths = Some(repositoryPaths changedPaths)
    }

/// Composes update preview semantics from paginated literal object diffs.
let createPreview
    (changedPaths: string list)
    (dirtyPaths: Set<string>)
    (localCommittedPaths: Set<string>)
    =
    let changed = Set.ofList changedPaths
    let overlapping = Set.intersect changed (Set.union dirtyPaths localCommittedPaths)

    {
        ChangedPaths = repositoryPaths changedPaths
        OverlappingPaths = overlapping |> Set.toList |> repositoryPaths
        HasDataLossRisk = not (Set.isEmpty (Set.intersect changed dirtyPaths))
        WouldCreateConflictSession = not (Set.isEmpty overlapping)
    }
