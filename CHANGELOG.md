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

### Changed

-   `Dematerialize` in the Git provider also removes the object from the local Git LFS cache, so freeing a file frees its disk space. It does this only after the remote confirmed it can supply the object again. `Materialize` downloads it again.
-   `Dematerialize` asks an HTTP(S) Git LFS server whether it has the object instead of downloading the whole object again. Local remotes keep the full download, because a dry run proves nothing there.

### Fixed

-   The Node runtime and lakeFS provider import Node modules as ES modules, so compiled output runs with plain `node` without a CommonJS bundle.
-   Status reads, the word diff and the checks before `Materialize`, `Dematerialize`, `RestorePaths`, `Prune` and `Deduplicate` no longer stop after 30 or 120 seconds without output. Git prints nothing while it re-hashes a changed Git LFS file, so these failed for large files.
-   `Dematerialize` checks a file's hash without a time limit and reports the real reason when the check fails. A slow check of a large file used to fail as a mismatch.
-   `Materialize` runs `git lfs checkout` to completion. For a large file it used to hit a time limit and report a failure, while on Windows git-lfs kept running and held `.git/index.lock`.

## 0.1.0 - 2026-09-24

### Added

-   Added provider-neutral abstractions for workspace sessions and core version control. Optional services can be absent, with fallback wrappers for consumers that need a complete session.
-   Added a Git provider that runs Git and Git LFS through the git CLI.
-   Added a lakeFS provider.
-   Added the Node runtime and dependency-only umbrella package for coordinated provider installation.
-   Added Fable and .NET targets.
