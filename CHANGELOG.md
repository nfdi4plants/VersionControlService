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

### Added

-   `VersionControlService.TextDiff` adds a streaming diff engine for files of any size. It bounds memory use and work per request and targets .NET and Fable.
-   The Git provider can run text diffs in a worker pool supervised by `TextDiffSupervisor`. Hosts create a `TextDiffPool` with `TextDiffPoolOptions.create` and `TextDiffPool.create`, and use `TextDiffTransport.WorkerThreadTransport.create` to start a worker thread from their own worker file.
-   `DiffSide` and `DiffContentBlocked` describe which side of a diff could not be read as text and the evidence found. `DiffContentBlocked.InvalidSequenceOffset` holds the byte offset of a sequence that is invalid in the encoding of that side, and is `None` for other evidence. Code that builds the record sets it. `OperationFailure.DiffDetail` carries a `DiffContentBlocked` for a `diff_content_not_text` or `diff_encoding_mismatch` failure.
-   `TextDiffFailureCodes.EncodingMismatch` (`diff_encoding_mismatch`) reports a source that classification read as UTF-8 and that later contains a byte sequence valid in Windows-1252 only. `Open` returns `DiffBlocker.EncodingRequired` for that side when it reaches the sequence before the first page, and the caller can choose Windows-1252.
-   `TextDiffFailureCodes.ReadFailed` (`diff_read_failed`) names the code that any text diff call returns when a source cannot be read because of a file system or process error.

### Changed

-   Version 0.2.0 replaces the whole-file contract of `TextDiffService`, which existed before, with resumable paged operations. Its `Open`, `ReadPage`, `ReplayPage`, `Expand`, `ReadLine`, `GetSourceInfo` and `Close` operations return resumable work where needed. `DiffPart` represents hunk fragments or hidden equal gaps. Each `LineSlice` can carry `Highlight` spans for changed or unchanged text.
-   `Expand` returns an `ExpandedContext` part and, when lines of the gap remain hidden, a `HiddenEqual` part with a new gap id for the rest of the gap. `ReplayPage` returns a page that was already read, by its `PageId`. The replayed page is identical to the first one except that `Pending` is `None`, so a caller can drop pages it no longer shows and read them again later.
-   `HunkBody.UnalignedSides` marks lines whose alignment could not be established. A gap can exceed the Myers step budget inside a window. A forward search can reach the default limit of 1,000,000 lines or 256 MiB per side, configured by `ResyncScanLines` and `ResyncScanBytes`. The search continues after it reaches a limit, from the same line offset between the sides, so after an insertion or deletion larger than the limit the rest of the file shows as unaligned regions, like a rewrite, with every line present. A full rewrite also produces unaligned regions.
-   The host writes its own worker file that calls `TextDiffWorker.bootstrap`, passes its pool through `GitSessionOptions.TextDiff`, calls `Prewarm` when it starts and disposes the pool when it shuts down. The package ships no worker script, and an Electron app keeps that file outside the asar archive.
-   The Git factories and sessions created without options no longer supply a text diff service, so `session.TextDiff` is `None` for `createFactory`, the credential factories and the `createSession*` functions that take no `GitSessionOptions`. Only `createFactoryWithOptions` and `createSessionWithOptions` with a pool supply one.
-   `TextDiffPoolOptions.create` makes a pool with three workers. Each worker runs one session, so no two diffs share a worker's scratch memory. A fourth `Open` closes the least recently used idle session, or waits when every session is busy.
-   `Open` pins source identities and checks binary content, including HDF5 signatures. A recognized BOM selects the source encoding. `DiffBlocker.EncodingRequired` with a `PreparationToken` and `EncodingCandidate` values asks for a choice when a source that classification read as UTF-8 holds a sequence that only Windows-1252 decodes, and `Open` reaches it before the first page. A source that is not text fails with `TextDiffFailureCodes.ContentNotText` and structured `OperationFailure.DiffDetail`. The evidence is a binary signature, a NUL byte, a control-character ratio above 1 percent in a 64 KiB window, or an invalid byte sequence. The scan judges every window. Classification reads samples and judges a window only when they cover at least 4 KiB of it or the window is shorter than that. A BMP signature needs zero reserved bytes and a known DIB header size, so a text file that starts with `BM` is text. Bytes that are invalid as UTF-8 open as Windows-1252 without asking.
-   The supervisor creates its scratch folders with mode 0700 and its files with mode 0600, and it refuses an existing `text-diff` folder in the temp root that belongs to another user. Its Git children run with `LC_ALL=C` and `LANG=C`, so their messages are not localized.
-   A working-tree path that runs through a symlinked folder leaving the workspace, or through a `.git` folder, fails with `diff_read_failed`.
-   `Open` answers `diff_session_closed` for a continuation whose preparation session the worker has closed or evicted. The caller opens the diff again.
-   Pages contain at most 1,000 rows and 32 fragments. A page response, including the first page from `Open`, has a limit of 512 KiB, and a `ReadLine` response has a limit of 64 KiB. The service clamps `ReadLineRequest.MaxUtf16` to 8,192 UTF-16 code units and `ExpandRequest.Count` to 100 lines.
-   The Git worker pool answers cancellation immediately. The worker closes an idle diff session after 15 minutes, and the pool closes an idle session earlier when an `Open` needs its slot. Both cases fail later requests with `diff_session_closed`, and the caller opens a new diff to continue.
-   `OperationFailure` has a new `DiffDetail` field, so code that builds the record has to set it. `OperationFailure.create` sets it to `None`.
-   The text diff fallback of `WorkspaceSession.withFallbackServices` used to return `UnsupportedContent` from its three reads. It now returns `NotDiffable ProviderUnsupported` from `Open`. `ReadPage`, `ReplayPage`, `Expand`, `ReadLine` and `GetSourceInfo` fail with `service_unavailable`, and `Close` succeeds with that warning.
-   The Git provider requires Git 2.42 or newer, and `CheckDependencies` reports older Git as incompatible. Git LFS 3.7 reads the index in `git lfs checkout` only with Git 2.42 or newer. With Git 2.38 to 2.41 it reads the HEAD tree, so a conflict pick of an LFS candidate whose object is in the local cache left a pointer and the warning `object_not_materialized`.
-   A session keeps at most eight suspended `ReadLine` reads. A continuation for a read that the session dropped answers `continuation_mismatch`, and the caller starts the read again without a continuation.

### Removed

-   `ContentView` and its `TextContent` and `UnsupportedContent` cases no longer exist. They belonged to the whole-file contract.
-   Version 0.2.0 removes `TextDiffService.GetDiff`, `TextDiffService.GetWordDiff` and `TextDiffService.GetBaseContent`. Consumers use `TextDiffService` pages, replay, gap expansion and line slices instead.
-   `VersionControlService.Runtime.Node` no longer exports `Process.isProcessAlive`. `Process.processExistence` replaces it and tells a process that is gone apart from one the caller cannot signal.
-   `Scanner.ObservationWindowBytes` no longer exists. Nothing used it.

### Fixed

-   `Publish` and `Synchronize` no longer fail on Git 2.38 to 2.41 when the remote already has branches. The Git LFS upload planning wrote a `--not` line to `git rev-list --stdin`, and Git accepts option lines on standard input only from 2.42 on, so every such publish failed with `fatal: options not supported in --stdin mode`. The planning now excludes each remote tip with a `^<oid>` line. A rev-list that still rejects its input with that message makes the planning fall back to its slower path.
-   When `git --version` or `git lfs version` cannot start or exits with an error, the `CheckDependencies` remediation for `git` or `git-lfs` starts with the start error, or with the exit code and stderr. It used to give the install advice alone, so a Git that could not start looked like a missing one.

## 0.1.2 - 2026-09-28

### Fixed

-   The Git provider passes repository paths to git as literal pathspecs. Git used to read `[`, `]`, `*` and `?` in a file name as a pattern, so `Dematerialize` or `RestorePaths` on `runs/sample[1].csv` also reset uncommitted edits of `runs/sample1.csv`.
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
