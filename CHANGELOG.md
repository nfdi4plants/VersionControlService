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

## 0.1.0 - 2026-09-24

### Added

-   Added provider-neutral abstractions for workspace sessions and core version control. Optional services can be absent, with fallback wrappers for consumers that need a complete session.
-   Added a Git provider that runs Git and Git LFS through the git CLI.
-   Added a lakeFS provider.
-   Added the Node runtime and dependency-only umbrella package for coordinated provider installation.
-   Added Fable and .NET targets.
