# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

**Types of changes**

-   `Added` for new features.
-   `Changed` for changes in existing functionality.
-   `Deprecated` for soon-to-be removed features.
-   `Removed` for now removed features.
-   `Fixed` for any bug fixes.
-   `Security` in case of vulnerabilities.

## [Unreleased]

## 0.1.2 - 2026-09-28

### Fixed

-   The Git provider passes repository paths to git as literal pathspecs. Git used to read `[`, `]`, `*` and `?` in a file name as a pattern, so freeing or discarding `runs/sample[1].csv` also reset uncommitted edits of `runs/sample1.csv`, and staging or unstaging such a name could include other files.
-   When a file changes while `Dematerialize` frees it, the Git provider keeps the new content and the backup and fails with a message that names the backup path. The rollback used to delete whatever was at the path and put the older backup back. The failure has code `lfs_backup_retained` and `StateChanged = true`.
-   `Materialize` downloads the file when `GIT_LFS_SKIP_SMUDGE=1` is set in the environment. The download of a single object used to get the pointer back and fail.
-   After `Dematerialize`, the Git provider keeps the cached object when only an unpushed merge commit adds it, also when `log.diffMerges` is set in the git config. The check used to skip merge commits.

## 0.1.1 - 2026-09-28

### Changed

-   `Dematerialize` in the Git provider also removes the object from the local Git LFS cache, so freeing a file frees its disk space. It first checks that the remote has that exact object, and it keeps the object when the repository uses a custom `lfs.storage` or has unpushed commits that add the object. `Materialize` downloads it again.
-   For an HTTP(S) Git LFS server, `Dematerialize` checks the object with a dry run and downloads nothing. Any other endpoint, or a repository with a standalone transfer agent, still copies the object once, because only a real fetch checks it there.
-   `Materialize` and `Dematerialize` ignore `lfs.fetchexclude` for the object they fetch or check.
-   `GetStatus` runs `git status` once. It used to run it a second time for the workspace version. The Git provider now parses `git status --porcelain=v2` output itself and derives the status and the workspace version from the same output.
-   When the remote lacks a file's object, `Dematerialize` says "The remote does not have '<path>' yet. Push it before freeing its local copy." and `Materialize` says the remote does not have the file's content, instead of showing git-lfs output. A server's per-object 404 answer is recognized as a missing object too.
-   `Dematerialize` keeps the file with "Could not check whether the remote has '<path>', so its local copy stays." when the remote check does not list the exact object.
-   `Materialize` fails with "Git LFS did not download '<path>'." when Git LFS still lists the file as not downloaded after the transfer.
-   `Materialize` downloads a file's object with one `git lfs fetch` for that path and then checks the file out. When another file's name differs from the requested one only at special characters, it downloads only the requested object with `git lfs smudge`, so the download adds no object for the other file. The checkout afterwards still runs `git lfs checkout` with the path pattern, so it also replaces the other file's pointer when that file's object is already in the local cache.

### Fixed

-   The Node runtime and lakeFS provider import Node modules as ES modules, so compiled output runs with plain `node` without a CommonJS bundle.
-   `GetStatus`, the word diff and the checks before `Materialize`, `Dematerialize`, `RestorePaths`, `Prune` and `Deduplicate` no longer stop after 30 or 120 seconds without output. Git prints nothing while it re-hashes a changed Git LFS file, so these failed for large files.
-   `Dematerialize` checks a file's hash without a time limit and reports the real reason when the check fails. A slow check of a large file used to fail as a mismatch.
-   `Materialize` runs `git lfs checkout` to completion. For a large file it used to hit a time limit and report a failure, while on Windows git-lfs kept running and held `.git/index.lock`.
-   `Materialize`, `Dematerialize` and picking a large-object conflict candidate work for file names with brackets, commas or other glob characters. git-lfs read such a name as a pattern that matched no file.
-   `Materialize`, `Dematerialize` and `StoragePolicy.SetSettings` report success when they finished, also when a cancel request arrived during their last step.
-   A `GetStatus` canceled during its repository check returns a canceled result.
-   `Materialize`, `Dematerialize` and cleaning the Git LFS cache pass the account credential to git-lfs, so they work on private repositories. The server used to answer 401.
-   `Dematerialize` keeps the file in place while it checks the file's hash, and then moves it into a backup inside the Git directory (`.git/vcs-lfs-backup/`). When the Git directory is on another volume, the backup is a file next to the original, named `<file>.vcs-lfs-backup-<32 hex digits>`. If the file's size or modification time changes between the check and the move, `Dematerialize` restores it and fails. The file used to leave the working tree for the whole check, and git status listed the backup as a new file.
-   `Materialize` and `Dematerialize` wait for other workspace changes, and other workspace changes wait for them. A save during a free could commit the file while it was missing from the working tree.
-   The credential Git LFS URL replaces a user name or password in the remote URL. A remote URL with its own user name used to produce a URL with two of them, and authentication failed.

## 0.1.0 - 2026-09-24

### Added

-   Added provider-neutral abstractions for workspace sessions and core version control. Optional services can be absent, with fallback wrappers for consumers that need a complete session.
-   Added a Git provider that runs Git and Git LFS through the git CLI.
-   Added a lakeFS provider.
-   Added the Node runtime and dependency-only umbrella package for coordinated provider installation.
-   Added Fable and .NET targets.
