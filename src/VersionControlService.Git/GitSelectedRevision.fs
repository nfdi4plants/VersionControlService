/// Isolated selected-revision transaction: a temporary index seeded from HEAD,
/// exact selected paths, commit-tree, and a compare-and-swap ref update that
/// never disturbs the real index or unrelated working-tree state.
/// Implemented by the Task 8 behavior cycles; this module intentionally starts
/// empty so the defect-record shell keeps its recorded v1 behavior observable.
module VersionControlService.Git.GitSelectedRevision
