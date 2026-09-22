module VersionControlService.Tests.Contracts.ProviderHarness

open System
open Fable.Core
open VersionControlService.Abstractions

/// A mutation applied to the shared target as another authorized client would:
/// Some content writes/overwrites the object, None deletes it.
type TargetMutation = {
    Path: string
    Content: string option
}

/// One isolated local workspace with an open provider session.
type HarnessWorkspace = {
    Session: WorkspaceSession
    Binding: WorkspaceBinding
    /// Repository-relative UTF-8 file IO against the local workspace.
    WriteFile: string -> string -> JS.Promise<unit>
    ReadFile: string -> JS.Promise<string option>
    RemoveFile: string -> JS.Promise<unit>
    /// True when the local store aliases case-different names (case-insensitive filesystem).
    LocalFileSystemAliasesCase: bool
    /// True when the local store aliases canonically equivalent NFC/NFD names.
    LocalFileSystemAliasesNormalization: bool
    /// True when the local store enforces Windows naming rules (reserved names, <>:"|?* etc.).
    LocalFileSystemWindowsRules: bool
}

/// Everything a provider supplies so the shared conformance suites can run against it.
/// Real harnesses own isolated repository creation, remote mutation, credential
/// variants, deterministic barriers, and cleanup.
type ProviderTestHarness = {
    /// Exact describe prefix: "fake", "Git", or "lakeFS".
    Name: string
    Factory: ProviderFactory
    /// Names of the optional services this provider is expected to advertise.
    ExpectedServices: string list
    /// Fresh, empty, authorized repository location for provisioning tests.
    CreateLocation: unit -> JS.Promise<RepositoryLocation>
    /// A location the configured credentials cannot write to.
    CreateUnauthorizedLocation: unit -> JS.Promise<RepositoryLocation>
    /// Fresh empty local directory path.
    CreateLocalPath: unit -> JS.Promise<string>
    /// Isolated workspace with one base revision (containing base.txt) and an open session.
    CreateWorkspace: unit -> JS.Promise<HarnessWorkspace>
    /// Second workspace bound to the SAME target as `anchor`, with its own connection profile.
    CreateLinkedWorkspace: HarnessWorkspace -> JS.Promise<HarnessWorkspace>
    /// A second independent session over the SAME local workspace.
    OpenSecondSession: HarnessWorkspace -> JS.Promise<WorkspaceSession>
    /// Applies mutations to the workspace's target as another authorized client (commit + publish there).
    AdvanceTarget: HarnessWorkspace -> TargetMutation[] -> JS.Promise<unit>
    /// Creates a new ref whose tip contains exactly the given (path, content) pairs on
    /// top of the base revision, without touching the local workspace files.
    SeedRevisionOnNewRef: HarnessWorkspace -> string -> (string * string)[] -> JS.Promise<ProviderRef>
    /// Makes Publish fail recoverably until RestorePublish runs.
    BreakPublish: HarnessWorkspace -> JS.Promise<unit>
    RestorePublish: HarnessWorkspace -> JS.Promise<unit>
    /// Arms a deterministic destination advance between the next verified mutation's
    /// pre-check and its post-operation verification.
    ArmDestinationRace: HarnessWorkspace -> TargetMutation[] -> JS.Promise<unit>
    /// Arms a deterministic pause in the next transfer-heavy operation so cancellation
    /// can interrupt it; the operation reports progress while paused.
    ArmSlowTransfer: HarnessWorkspace -> JS.Promise<unit>
    Cleanup: unit -> JS.Promise<unit>
}

/// Stable failure codes shared by the suites.
module ConformanceCodes =

    [<Literal>]
    let PreconditionFailed = "precondition_failed"

    [<Literal>]
    let PathCollision = "path_collision"

    [<Literal>]
    let UnrepresentablePath = "unrepresentable_path"

    [<Literal>]
    let ConflictsDetected = "conflicts_detected"

    [<Literal>]
    let TargetNotEmpty = "target_not_empty"

/// Shared redaction property: no failure message, detail, or progress output may
/// contain likely credential material.
let assertRedacted (text: string) =
    if not (isNull text) then
        let lowered = text.ToLowerInvariant()

        if lowered.Contains "bearer " && not (lowered.Contains "[redacted]") then
            failwith $"Credential material leaked (bearer token): {text}"

        if
            System.Text.RegularExpressions.Regex.IsMatch(text, "://[^/@\\s]*:[^/@\\s]+@")
            && not (lowered.Contains "[redacted]")
        then
            failwith $"Credential material leaked (URL credentials): {text}"

/// Helpers shared by every conformance suite module.
module SuiteHelpers =

    open Vitest

    let run (operation: Async<'T>) : JS.Promise<'T> = Async.StartAsPromise operation

    let ctx (name: string) = OperationContext.detached name

    let mkPath (value: string) =
        match RepositoryPath.tryCreate value with
        | Ok path -> path
        | Error message -> failwith message

    let pathValues (paths: RepositoryPath[]) =
        paths |> Array.map RepositoryPath.value |> Array.sort

    let expectOutcome (operationName: string) (result: OperationResult<'T>) : OperationOutcome<'T> =
        match result with
        | Succeeded outcome -> outcome
        | PartiallySucceeded(_, failure) ->
            failwith $"{operationName} unexpectedly returned partial success ({failure.Code})."
        | Failed failure -> failwith $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

    let expectValue (operationName: string) (result: OperationResult<'T>) : 'T =
        (expectOutcome operationName result).Value

    let expectNoOp (operationName: string) (result: OperationResult<'T>) : 'T =
        let outcome = expectOutcome operationName result

        match outcome.Effect with
        | NoOp _ -> outcome.Value
        | Performed -> failwith $"{operationName} was expected to be a NoOp but reported Performed."

    let expectPerformed (operationName: string) (result: OperationResult<'T>) : OperationOutcome<'T> =
        let outcome = expectOutcome operationName result

        match outcome.Effect with
        | Performed -> outcome
        | NoOp reason -> failwith $"{operationName} was unexpectedly a NoOp ({reason})."

    let expectFailure (operationName: string) (result: OperationResult<'T>) : OperationFailure =
        match result with
        | Failed failure -> failure
        | PartiallySucceeded _
        | Succeeded _ -> failwith $"Expected {operationName} to fail."

    /// The failure carried by either a Failed or PartiallySucceeded result.
    let carriedFailure (operationName: string) (result: OperationResult<'T>) : OperationFailure =
        match result with
        | Failed failure -> failure
        | PartiallySucceeded(_, failure) -> failure
        | Succeeded _ -> failwith $"Expected {operationName} to carry a failure."

    let getStatus (workspace: HarnessWorkspace) : JS.Promise<WorkspaceStatus> = promise {
        let! result = run (workspace.Session.Core.GetStatus(ctx "suite-status"))
        return expectValue "get status" result
    }

    let sessionStatus (session: WorkspaceSession) : JS.Promise<WorkspaceStatus> = promise {
        let! result = run (session.Core.GetStatus(ctx "suite-status"))
        return expectValue "get status" result
    }

    let syncService (session: WorkspaceSession) =
        match session.Synchronization with
        | Some service -> service
        | None -> failwith "The synchronization service is required for this profile."

    let conflictService (session: WorkspaceSession) =
        match session.ConflictResolution with
        | Some service -> service
        | None -> failwith "The conflict-resolution service is required for this profile."

    let changePaths (status: WorkspaceStatus) =
        status.Changes
        |> Array.map (fun change -> RepositoryPath.value change.Path)
        |> Array.sort

    /// Feature discovery used across suites: which optional services are present.
    let discoverFeatures (session: WorkspaceSession) =
        [
            if session.Synchronization.IsSome then
                "synchronization"
            if session.TextDiff.IsSome then
                "text-diff"
            if session.ConflictResolution.IsSome then
                "conflicts"
            if session.ObjectMaterialization.IsSome then
                "materialization"
            if session.StoragePolicy.IsSome then
                "storage-policy"
            if session.Maintenance.IsSome then
                "maintenance"
            if session.RepositoryBrowser.IsSome then
                "browser"
        ]
        |> List.sort

    let suiteTestOptions = TestOptions(timeout = 120000)

// ---------------------------------------------------------------------------
// In-memory fake provider: the reference implementation the discovery test runs
// every suite against. It is deliberately well-behaved.
// ---------------------------------------------------------------------------

module FakeHarness =

    [<Emit("$0.normalize('NFC')")>]
    let private normalizeNfc (_text: string) : string = jsNative

    let private mkProviderId name =
        match ProviderId.tryCreate name with
        | Ok id -> id
        | Error message -> failwith message

    let private mkPath value =
        match RepositoryPath.tryCreate value with
        | Ok path -> path
        | Error message -> failwith message

    let private mkRevisionId value =
        match RevisionId.tryCreate value with
        | Ok id -> id
        | Error message -> failwith message

    let private mkProviderRef value =
        match ProviderRef.tryCreate value with
        | Ok reference -> reference
        | Error message -> failwith message

    let fakeId = mkProviderId "fake.reference"

    type FakeRevision = {
        Id: string
        Parents: string list
        Files: Map<string, string>
        Message: string
    }

    /// Shared repository target: an object/revision store plus published refs.
    type FakeTarget = {
        mutable Revisions: Map<string, FakeRevision>
        mutable Refs: Map<string, string>
        mutable PublishBroken: bool
        mutable RaceMutations: TargetMutation[] option
        mutable SlowTransferArmed: bool
        Locked: bool
    }

    /// Globally unique revision IDs so re-targeted workspaces never confuse
    /// revisions from different targets.
    let mutable private globalRevisionCounter = 0

    type FakeConflictItem = {
        ItemPath: string
        BaseContent: string option
        OursContent: string
        TheirsContent: string
        mutable ResolvedContent: string option
    }

    type FakeConflictSession = {
        SessionId: string
        mutable HandleVersion: int
        mutable ConflictItems: FakeConflictItem list
        TheirRevisionId: string
        PreUpdateFiles: Map<string, string>
        PreUpdateBase: string
    }

    type FakeWorkspace = {
        mutable Target: FakeTarget
        Root: string
        Profile: string option
        mutable LocalFiles: Map<string, string>
        mutable BaseRevisionId: string
        mutable CurrentRef: string
        mutable LocalRefs: Map<string, string>
        mutable MutationCounter: int
        mutable ActiveConflict: FakeConflictSession option
    }

    let private newTarget (locked: bool) : FakeTarget = {
        Revisions = Map.empty
        Refs = Map.empty
        PublishBroken = false
        RaceMutations = None
        SlowTransferArmed = false
        Locked = locked
    }

    let private addRevision (target: FakeTarget) (parents: string list) (files: Map<string, string>) (message: string) =
        globalRevisionCounter <- globalRevisionCounter + 1
        let id = $"rev-{globalRevisionCounter}"

        target.Revisions <-
            target.Revisions
            |> Map.add id {
                Id = id
                Parents = parents
                Files = files
                Message = message
            }

        id

    let private revisionFiles (target: FakeTarget) (revisionId: string) =
        match target.Revisions.TryFind revisionId with
        | Some revision -> revision.Files
        | None -> Map.empty

    let private headRevisionId (workspace: FakeWorkspace) =
        workspace.LocalRefs.TryFind workspace.CurrentRef
        |> Option.defaultValue workspace.BaseRevisionId

    let private versionToken (workspace: FakeWorkspace) =
        let conflictPart =
            match workspace.ActiveConflict with
            | Some session -> $"c{session.HandleVersion}"
            | None -> "none"

        $"fake:{workspace.CurrentRef}:{headRevisionId workspace}:{workspace.MutationCounter}:{conflictPart}"

    let private bump (workspace: FakeWorkspace) =
        workspace.MutationCounter <- workspace.MutationCounter + 1

    let private staleFailure () =
        OperationFailure.create Concurrency ConformanceCodes.PreconditionFailed "The workspace version is stale."

    let private pathValue = RepositoryPath.value

    /// Case folding plus NFC normalization: how an aliasing local filesystem sees names.
    let private aliasKey (path: string) = (normalizeNfc path).ToLowerInvariant()

    let private windowsReservedNames =
        set [
            "con"
            "prn"
            "aux"
            "nul"
            "com1"
            "com2"
            "com3"
            "lpt1"
            "lpt2"
            "lpt3"
        ]

    let private isWindowsInvalidName (path: string) =
        path.Split('/')
        |> Array.exists (fun segment ->
            let baseName = (segment.Split '.').[0].ToLowerInvariant()

            windowsReservedNames.Contains baseName
            || segment.EndsWith "."
            || segment.EndsWith " "
            || segment |> Seq.exists (fun c -> "<>:\"|?*".Contains(string c)))

    let private changesAgainstHead (workspace: FakeWorkspace) =
        let headFiles = revisionFiles workspace.Target (headRevisionId workspace)

        let modifiedOrAdded =
            workspace.LocalFiles
            |> Map.toList
            |> List.choose (fun (path, content) ->
                match headFiles.TryFind path with
                | Some headContent when headContent = content -> None
                | Some _ -> Some(path, ModifiedChange)
                | None -> Some(path, AddedChange))

        let deleted =
            headFiles
            |> Map.toList
            |> List.choose (fun (path, _) ->
                if workspace.LocalFiles.ContainsKey path then
                    None
                else
                    Some(path, DeletedChange))

        modifiedOrAdded @ deleted

    let private conflictSummary (session: FakeConflictSession) : ConflictSessionSummary =
        let unresolved = session.ConflictItems |> List.filter (fun item -> item.ResolvedContent.IsNone)

        {
            Handle = {
                SessionId = session.SessionId
                Version = string session.HandleVersion
            }
            Items =
                unresolved
                |> List.map (fun item -> {
                    Path = mkPath item.ItemPath
                    Candidates = [|
                        {
                            CandidateId = "workspace"
                            Label = "Workspace version"
                            Revision = None
                            Preview = Some(TextPreview item.OursContent)
                        }
                        {
                            CandidateId = "target"
                            Label = "Target version"
                            Revision = Some(mkRevisionId session.TheirRevisionId)
                            Preview = Some(TextPreview item.TheirsContent)
                        }
                        yield!
                            match item.BaseContent with
                            | Some baseContent ->
                                [|
                                    {
                                        CandidateId = "base"
                                        Label = "Base version"
                                        Revision = None
                                        Preview = Some(TextPreview baseContent)
                                    }
                                |]
                            | None -> [||]
                    |]
                    CombinedPreview = None
                    SupportsResolvedContent = true
                })
                |> List.toArray
        }

    let private synchronizationState (workspace: FakeWorkspace) : SynchronizationState =
        let targetRevision = workspace.Target.Refs.TryFind workspace.CurrentRef
        let localHead = headRevisionId workspace

        let relationship =
            match targetRevision with
            | None -> NoTarget
            | Some target when target = localHead -> UpToDate
            | Some target when target = workspace.BaseRevisionId -> LocalAhead
            | Some _ when localHead = workspace.BaseRevisionId -> TargetAhead
            | Some _ -> Diverged

        let remoteChanged =
            match targetRevision with
            | Some target when target <> workspace.BaseRevisionId ->
                let baseFiles = revisionFiles workspace.Target workspace.BaseRevisionId
                let targetFiles = revisionFiles workspace.Target target

                let changed =
                    Set.union (Set.ofSeq (Map.keys baseFiles)) (Set.ofSeq (Map.keys targetFiles))
                    |> Set.filter (fun path -> baseFiles.TryFind path <> targetFiles.TryFind path)
                    |> Set.toArray
                    |> Array.map mkPath

                Some changed
            | _ -> None

        {
            BaseRevision = Some(mkRevisionId workspace.BaseRevisionId)
            WorkspaceRevision = Some(mkRevisionId localHead)
            TargetRevision = targetRevision |> Option.map mkRevisionId
            TargetRef = None
            LocalRevisionCount = None
            TargetRevisionCount = None
            RemoteChangedPaths = remoteChanged
            Relationship = relationship
        }

    /// Deterministic pause used by cancellation tests: reports progress until canceled.
    let private slowTransferGate (workspace: FakeWorkspace) (context: OperationContext) : Async<bool> =
        async {
            if workspace.Target.SlowTransferArmed then
                workspace.Target.SlowTransferArmed <- false
                let mutable canceled = false
                let mutable step = 0

                if context.OperationId = "large-byte-progress" then
                    context.ReportProgress {
                        PhaseCode = "transfer"
                        Item = None
                        Completed = Some(3.0 * 1024.0 * 1024.0 * 1024.0)
                        Total = Some(4.0 * 1024.0 * 1024.0 * 1024.0)
                        DisplayMessage = Some "Transferring large objects"
                    }

                while not canceled && step < 200 do
                    do! Async.Sleep 2
                    step <- step + 1

                    context.ReportProgress {
                        PhaseCode = "transfer"
                        Item = None
                        Completed = Some(float step)
                        Total = Some 200.0
                        DisplayMessage = Some "Transferring objects"
                    }

                    if context.Cancellation.IsCancellationRequested() then
                        canceled <- true

                return canceled
            else
                return false
        }

    let private raceEvidence (expected: string) (observed: string) = [|
        "expected_destination", mkRevisionId expected
        "observed_destination", mkRevisionId observed
    |]

    /// Applies armed race mutations after a pre-check, simulating another client
    /// advancing the destination inside the check-then-act-then-verify window.
    let private applyArmedRace (workspace: FakeWorkspace) : string option =
        match workspace.Target.RaceMutations with
        | Some mutations ->
            workspace.Target.RaceMutations <- None
            let targetHead = workspace.Target.Refs.TryFind workspace.CurrentRef

            let baseFiles =
                targetHead
                |> Option.map (revisionFiles workspace.Target)
                |> Option.defaultValue Map.empty

            let raceFiles =
                mutations
                |> Array.fold
                    (fun files mutation ->
                        match mutation.Content with
                        | Some content -> Map.add mutation.Path content files
                        | None -> Map.remove mutation.Path files)
                    baseFiles

            let raceRevision =
                addRevision workspace.Target (targetHead |> Option.toList) raceFiles "race: concurrent advance"

            workspace.Target.Refs <- workspace.Target.Refs |> Map.add workspace.CurrentRef raceRevision
            Some raceRevision
        | None -> None

    let private createCore (workspace: FakeWorkspace) : CoreVersionControl =
        let status () : WorkspaceStatus =
            let changes =
                changesAgainstHead workspace
                |> List.map (fun (path, kind) -> {
                    Path = mkPath path
                    OldPath = None
                    Kind = kind
                })
                |> List.toArray

            {
                CurrentRef =
                    Some {
                        Name = workspace.CurrentRef
                        ProviderRef = mkProviderRef $"fake:{workspace.CurrentRef}"
                        Kind = LocalRef
                        IsCurrent = true
                    }
                WorkspaceVersion = versionToken workspace
                Changes = changes
                ActiveConflictSession = workspace.ActiveConflict |> Option.map conflictSummary
                Synchronization = Some(synchronizationState workspace)
            }

        {
            GetStatus = fun _ -> async { return OperationResult.succeeded (status ()) }
            ListRefs =
                fun _ -> async {
                    let refs =
                        workspace.LocalRefs
                        |> Map.toArray
                        |> Array.map (fun (name, _) -> {
                            Name = name
                            ProviderRef = mkProviderRef $"fake:{name}"
                            Kind = LocalRef
                            IsCurrent = name = workspace.CurrentRef
                        })

                    return OperationResult.succeeded refs
                }
            CreateRef =
                fun request _ -> async {
                    if request.ExpectedWorkspaceVersion <> versionToken workspace then
                        return OperationResult.failed (staleFailure ())
                    elif workspace.LocalRefs.ContainsKey request.Name then
                        return
                            OperationResult.failed (
                                OperationFailure.create Validation "ref_exists" $"Ref '{request.Name}' already exists."
                            )
                    else
                        let baseRevision =
                            match request.BaseRef with
                            | Some baseRef ->
                                let name = (ProviderRef.value baseRef).Replace("fake:", "")

                                workspace.LocalRefs.TryFind name
                                |> Option.defaultValue (headRevisionId workspace)
                            | None -> headRevisionId workspace

                        workspace.LocalRefs <- workspace.LocalRefs |> Map.add request.Name baseRevision

                        if request.SwitchTo then
                            workspace.CurrentRef <- request.Name

                        bump workspace

                        return
                            OperationResult.succeeded {
                                Name = request.Name
                                ProviderRef = mkProviderRef $"fake:{request.Name}"
                                Kind = LocalRef
                                IsCurrent = request.SwitchTo
                            }
                }
            PreflightSwitchRef =
                fun request _ -> async {
                    let refName = (ProviderRef.value request.TargetRef).Replace("fake:", "")

                    match workspace.LocalRefs.TryFind refName with
                    | None ->
                        return
                            OperationResult.failed (
                                OperationFailure.create NotFound "ref_not_found" $"Ref '{refName}' does not exist."
                            )
                    | Some targetRevision ->
                        let targetFiles = revisionFiles workspace.Target targetRevision
                        let headFiles = revisionFiles workspace.Target (headRevisionId workspace)

                        let dirtyPaths =
                            changesAgainstHead workspace |> List.map fst |> Set.ofList

                        let atRisk =
                            dirtyPaths
                            |> Set.filter (fun path -> headFiles.TryFind path <> targetFiles.TryFind path)
                            |> Set.toArray
                            |> Array.map mkPath

                        return
                            OperationResult.succeeded {
                                PathsAtRisk = atRisk
                                IsSafe = atRisk.Length = 0
                            }
                }
            SwitchRef =
                fun request _ -> async {
                    if request.ExpectedWorkspaceVersion <> versionToken workspace then
                        return OperationResult.failed (staleFailure ())
                    else
                        let refName = (ProviderRef.value request.TargetRef).Replace("fake:", "")

                        match workspace.LocalRefs.TryFind refName with
                        | None ->
                            return
                                OperationResult.failed (
                                    OperationFailure.create NotFound "ref_not_found" $"Ref '{refName}' does not exist."
                                )
                        | Some targetRevision ->
                            let targetFiles = revisionFiles workspace.Target targetRevision

                            // Materialization guards: aliasing collisions and Windows-invalid names.
                            let paths = targetFiles |> Map.toList |> List.map fst

                            let collisions =
                                paths
                                |> List.groupBy aliasKey
                                |> List.filter (fun (_, group) -> group.Length > 1)
                                |> List.collect snd

                            let invalidNames = paths |> List.filter isWindowsInvalidName

                            if collisions.Length > 0 then
                                return
                                    OperationResult.failed {
                                        OperationFailure.create
                                            Validation
                                            ConformanceCodes.PathCollision
                                            "Distinct repository paths alias to one local file." with
                                            AffectedPaths = collisions |> List.toArray
                                    }
                            elif invalidNames.Length > 0 then
                                return
                                    OperationResult.failed {
                                        OperationFailure.create
                                            Validation
                                            ConformanceCodes.UnrepresentablePath
                                            "A repository path cannot be represented on the local filesystem." with
                                            AffectedPaths = invalidNames |> List.toArray
                                    }
                            else
                                workspace.CurrentRef <- refName
                                workspace.LocalFiles <- targetFiles
                                workspace.BaseRevisionId <- targetRevision
                                bump workspace
                                return OperationResult.succeeded (status ())
                }
            CreateRevision =
                fun request context -> async {
                    let! canceled = slowTransferGate workspace context

                    if canceled then
                        return OperationResult.canceled "The revision transfer was canceled."
                    elif request.ExpectedWorkspaceVersion <> versionToken workspace then
                        return OperationResult.failed (staleFailure ())
                    elif request.Paths.Length = 0 then
                        return
                            OperationResult.failed (
                                OperationFailure.create Validation "no_paths_selected" "Select at least one path."
                            )
                    else
                        let headId = headRevisionId workspace
                        let headFiles = revisionFiles workspace.Target headId

                        let missing =
                            request.Paths
                            |> Array.map pathValue
                            |> Array.filter (fun path ->
                                not (workspace.LocalFiles.ContainsKey path) && not (headFiles.ContainsKey path))

                        if missing.Length > 0 then
                            return
                                OperationResult.failed {
                                    OperationFailure.create
                                        NotFound
                                        "path_not_found"
                                        "A selected path does not exist in the workspace." with
                                        AffectedPaths = missing
                                }
                        else
                            let selectedChanges =
                                request.Paths
                                |> Array.map pathValue
                                |> Array.choose (fun path ->
                                    let localContent = workspace.LocalFiles.TryFind path
                                    let headContent = headFiles.TryFind path

                                    if localContent = headContent then
                                        None
                                    else
                                        Some(path, localContent))

                            if selectedChanges.Length = 0 then
                                return OperationResult.noOp (Some "The selected paths are unchanged.") (mkRevisionId headId)
                            else
                                let newFiles =
                                    selectedChanges
                                    |> Array.fold
                                        (fun files (path, content) ->
                                            match content with
                                            | Some value -> Map.add path value files
                                            | None -> Map.remove path files)
                                        headFiles

                                let revisionId =
                                    addRevision workspace.Target [ headId ] newFiles request.Message

                                workspace.LocalRefs <- workspace.LocalRefs |> Map.add workspace.CurrentRef revisionId
                                bump workspace

                                return
                                    Succeeded {
                                        OperationOutcome.performed (mkRevisionId revisionId) with
                                            AffectedPaths = selectedChanges |> Array.map fst
                                            ResultingRevision = Some(mkRevisionId revisionId)
                                            ResultingWorkspaceVersion = Some(versionToken workspace)
                                            Publication = LocalOnly
                                    }
                }
            RestorePaths =
                fun request _ -> async {
                    if request.ExpectedWorkspaceVersion <> versionToken workspace then
                        return OperationResult.failed (staleFailure ())
                    else
                        let headFiles = revisionFiles workspace.Target (headRevisionId workspace)

                        for path in request.Paths |> Array.map pathValue do
                            match headFiles.TryFind path with
                            | Some content -> workspace.LocalFiles <- workspace.LocalFiles |> Map.add path content
                            | None -> workspace.LocalFiles <- workspace.LocalFiles |> Map.remove path

                        bump workspace
                        return OperationResult.succeeded ()
                }
            GetDiffSummary =
                fun _ -> async {
                    let entries =
                        changesAgainstHead workspace
                        |> List.map (fun (path, kind) -> {
                            Path = mkPath path
                            OldPath = None
                            Kind = kind
                            LineInsertions = None
                            LineDeletions = None
                        })
                        |> List.toArray

                    return OperationResult.succeeded { Entries = entries }
                }
        }

    let private createSynchronization (workspace: FakeWorkspace) : SynchronizationService =
        let localDirtyPaths () =
            changesAgainstHead workspace |> List.map fst |> Set.ofList

        let targetChanges () =
            match workspace.Target.Refs.TryFind workspace.CurrentRef with
            | Some targetRevision when targetRevision <> workspace.BaseRevisionId ->
                let baseFiles = revisionFiles workspace.Target workspace.BaseRevisionId
                let targetFiles = revisionFiles workspace.Target targetRevision

                Set.union (Set.ofSeq (Map.keys baseFiles)) (Set.ofSeq (Map.keys targetFiles))
                |> Set.filter (fun path -> baseFiles.TryFind path <> targetFiles.TryFind path)
            | _ -> Set.empty

        let refresh _ = async { return OperationResult.succeeded (synchronizationState workspace) }
        let previewUpdate _ = async {
            let changed = targetChanges ()
            let localChanged = localDirtyPaths ()

            // Paths the local side changed in commits since base also count as local.
            let localCommitted =
                let baseFiles = revisionFiles workspace.Target workspace.BaseRevisionId
                let headFiles = revisionFiles workspace.Target (headRevisionId workspace)

                Set.union (Set.ofSeq (Map.keys baseFiles)) (Set.ofSeq (Map.keys headFiles))
                |> Set.filter (fun path -> baseFiles.TryFind path <> headFiles.TryFind path)

            let overlapping = Set.intersect changed (Set.union localChanged localCommitted)

            return
                OperationResult.succeeded {
                    ChangedPaths = changed |> Set.toArray |> Array.map mkPath
                    OverlappingPaths = overlapping |> Set.toArray |> Array.map mkPath
                    HasDataLossRisk = not (Set.isEmpty (Set.intersect changed localChanged))
                    WouldCreateConflictSession = not (Set.isEmpty overlapping)
                }
        }
        let update (request: UpdateRequest) (context: OperationContext) = async {
            let! canceled = slowTransferGate workspace context

            if canceled then
                return OperationResult.canceled "The update transfer was canceled."
            elif request.ExpectedWorkspaceVersion <> versionToken workspace then
                return OperationResult.failed (staleFailure ())
            elif workspace.ActiveConflict.IsSome then
                return
                    OperationResult.failed (
                        OperationFailure.create
                            Conflict
                            "conflict_session_active"
                            "Resolve or cancel the active conflict session first."
                    )
            else
                match workspace.Target.Refs.TryFind workspace.CurrentRef with
                | None ->
                    return
                        OperationResult.noOp (Some "No target is configured.") (synchronizationState workspace)
                | Some targetRevision when targetRevision = workspace.BaseRevisionId ->
                    return
                        OperationResult.noOp
                            (Some "The workspace is already up to date.")
                            (synchronizationState workspace)
                | Some targetRevision ->
                    let baseFiles = revisionFiles workspace.Target workspace.BaseRevisionId
                    let targetFiles = revisionFiles workspace.Target targetRevision
                    let headFiles = revisionFiles workspace.Target (headRevisionId workspace)

                    let changedOnTarget =
                        Set.union (Set.ofSeq (Map.keys baseFiles)) (Set.ofSeq (Map.keys targetFiles))
                        |> Set.filter (fun path -> baseFiles.TryFind path <> targetFiles.TryFind path)

                    let oursContent path =
                        workspace.LocalFiles.TryFind path
                        |> Option.orElse (headFiles.TryFind path)

                    let conflicts =
                        changedOnTarget
                        |> Set.filter (fun path ->
                            let ours = oursContent path
                            let baseContent = baseFiles.TryFind path
                            let theirs = targetFiles.TryFind path
                            ours <> baseContent && ours <> theirs)
                        |> Set.toList

                    let preUpdateFiles = workspace.LocalFiles
                    let preUpdateBase = workspace.BaseRevisionId

                    // Apply non-conflicting target changes.
                    for path in Set.toList changedOnTarget do
                        if not (List.contains path conflicts) then
                            match targetFiles.TryFind path with
                            | Some content ->
                                workspace.LocalFiles <- workspace.LocalFiles |> Map.add path content
                            | None -> workspace.LocalFiles <- workspace.LocalFiles |> Map.remove path

                    if conflicts.IsEmpty then
                        // Fast-forward or clean merge.
                        let mergedFiles =
                            workspace.LocalFiles

                        let localHead = headRevisionId workspace

                        let newHead =
                            if localHead = workspace.BaseRevisionId then
                                targetRevision
                            else
                                addRevision
                                    workspace.Target
                                    [ localHead; targetRevision ]
                                    mergedFiles
                                    "merge: update from target"

                        workspace.LocalRefs <- workspace.LocalRefs |> Map.add workspace.CurrentRef newHead
                        workspace.BaseRevisionId <- targetRevision
                        bump workspace
                        return OperationResult.succeeded (synchronizationState workspace)
                    else
                        workspace.ActiveConflict <-
                            Some {
                                SessionId = $"conflict-{workspace.MutationCounter}-{targetRevision}"
                                HandleVersion = 1
                                ConflictItems =
                                    conflicts
                                    |> List.map (fun path -> {
                                        ItemPath = path
                                        BaseContent = baseFiles.TryFind path
                                        OursContent = oursContent path |> Option.defaultValue ""
                                        TheirsContent = targetFiles.TryFind path |> Option.defaultValue ""
                                        ResolvedContent = None
                                    })
                                TheirRevisionId = targetRevision
                                PreUpdateFiles = preUpdateFiles
                                PreUpdateBase = preUpdateBase
                            }

                        bump workspace

                        return
                            OperationResult.partiallySucceeded
                                {
                                    OperationOutcome.performed (synchronizationState workspace) with
                                        AffectedPaths = conflicts |> List.toArray
                                        ResultingWorkspaceVersion = Some(versionToken workspace)
                                }
                                (OperationFailure.create
                                    Conflict
                                    ConformanceCodes.ConflictsDetected
                                    "The update produced conflicts that need resolution.")
                                {
                                    Code = "resolve_conflict_session"
                                    Instructions = Some "Resolve every conflict item, then finalize."
                                }
        }
        let publish (request: PublishRequest) (context: OperationContext) = async {
            let! canceled = slowTransferGate workspace context

            if canceled then
                return OperationResult.canceled "The publish transfer was canceled."
            elif request.ExpectedWorkspaceVersion <> versionToken workspace then
                return OperationResult.failed (staleFailure ())
            elif workspace.Target.PublishBroken then
                return
                    OperationResult.failed {
                        OperationFailure.create
                            Network
                            "target_unreachable"
                            "The publication target is unreachable; local revisions are preserved for retry." with
                            Retryable = true
                    }
            else
                let localHead = headRevisionId workspace
                let observedTarget = workspace.Target.Refs.TryFind workspace.CurrentRef

                // Client-side pre-check against the consumer-observed target revision.
                let expectedMatchesObserved =
                    match request.ExpectedTargetRevision, observedTarget with
                    | None, _ -> true
                    | Some expected, Some observed -> RevisionId.value expected = observed
                    | Some _, None -> false

                if not expectedMatchesObserved then
                    return
                        OperationResult.failed {
                            staleFailure () with
                                RevisionEvidence = [|
                                    yield!
                                        request.ExpectedTargetRevision
                                        |> Option.map (fun r -> "expected_target", r)
                                        |> Option.toList
                                    yield!
                                        observedTarget
                                        |> Option.map (fun o -> "observed_target", mkRevisionId o)
                                        |> Option.toList
                                |]
                        }
                else
                    match observedTarget with
                    | Some target when target = localHead ->
                        return
                            OperationResult.noOp
                                (Some "The target already has every local revision.")
                                (synchronizationState workspace)
                    | _ ->
                        // Simulated race window between pre-check and verification.
                        match applyArmedRace workspace with
                        | Some raceRevision ->
                            return
                                OperationResult.partiallySucceeded
                                    (OperationOutcome.performed (synchronizationState workspace))
                                    {
                                        OperationFailure.create
                                            Concurrency
                                            ConformanceCodes.PreconditionFailed
                                            "The target advanced during publish verification." with
                                            RevisionEvidence =
                                                raceEvidence
                                                    (observedTarget |> Option.defaultValue localHead)
                                                    raceRevision
                                    }
                                    {
                                        Code = "review_and_retry"
                                        Instructions =
                                            Some "Review the observed target revision, refresh, and retry."
                                    }
                        | None ->
                            workspace.BaseRevisionId <- localHead
                            workspace.Target.Refs <- workspace.Target.Refs |> Map.add workspace.CurrentRef localHead
                            bump workspace

                            return
                                Succeeded {
                                    OperationOutcome.performed (synchronizationState workspace) with
                                        Publication = Published
                                        ResultingRevision = Some(mkRevisionId localHead)
                                        ResultingWorkspaceVersion = Some(versionToken workspace)
                                }
        }
        let baseService : SynchronizationService = {
            Refresh = refresh
            PreviewUpdate = previewUpdate
            Update = update
            Publish = publish
            Synchronize =
                fun request context ->
                    async {
                        if request.ExpectedWorkspaceVersion <> versionToken workspace then
                            return OperationResult.failed (staleFailure ())
                        else
                            return!
                                Synchronization.compose
                                    {
                                        HasActiveConflictSession =
                                            fun _ -> async { return OperationResult.succeeded workspace.ActiveConflict.IsSome }
                                        Refresh = refresh
                                        PreviewUpdate = fun _ context -> previewUpdate context
                                        Update =
                                            fun _ context ->
                                                update
                                                    { ExpectedWorkspaceVersion = versionToken workspace }
                                                    context
                                        Publish =
                                            fun syncState context ->
                                                publish
                                                    {
                                                        ExpectedWorkspaceVersion = versionToken workspace
                                                        ExpectedTargetRevision = syncState.TargetRevision
                                                    }
                                                    context
                                    }
                                    request
                                    context
                    }
        }

        baseService

    let private createConflictService (workspace: FakeWorkspace) : ConflictResolutionService =
        let handleRejection () =
            OperationResult.failed {
                OperationFailure.create
                    Concurrency
                    ConformanceCodes.PreconditionFailed
                    "The conflict-session handle is stale, foreign, or closed." with
                    RecoveryAction =
                        Some {
                            Code = ConflictRecovery.RefreshConflictSession
                            Instructions = Some "Refresh the conflict session and retry with the live handle."
                        }
            }

        let validateHandle (handle: ConflictSessionHandle) (expectedVersion: string) =
            match workspace.ActiveConflict with
            | Some session when
                session.SessionId = handle.SessionId
                && string session.HandleVersion = handle.Version
                && expectedVersion = versionToken workspace
                ->
                Ok session
            | _ -> Error()

        {
            GetActiveSession =
                fun _ -> async {
                    return OperationResult.succeeded (workspace.ActiveConflict |> Option.map conflictSummary)
                }
            Resolve =
                fun request _ -> async {
                    match validateHandle request.Handle request.ExpectedWorkspaceVersion with
                    | Error() -> return handleRejection ()
                    | Ok session ->
                        let pathText = pathValue request.Path

                        match
                            session.ConflictItems
                            |> List.tryFind (fun item -> item.ItemPath = pathText && item.ResolvedContent.IsNone)
                        with
                        | None ->
                            return
                                OperationResult.failed (
                                    OperationFailure.create
                                        NotFound
                                        "conflict_item_not_found"
                                        $"No unresolved conflict exists for '{pathText}'."
                                )
                        | Some item ->
                            let resolvedContent =
                                match request.Resolution with
                                | PickCandidate "workspace" -> Some item.OursContent
                                | PickCandidate "target" -> Some item.TheirsContent
                                | PickCandidate "base" -> item.BaseContent
                                | PickCandidate _ -> None
                                | SupplyResolvedContent content -> Some content

                            match resolvedContent with
                            | None ->
                                return
                                    OperationResult.failed (
                                        OperationFailure.create
                                            Validation
                                            "unknown_candidate"
                                            "The candidate ID is not part of this conflict item."
                                    )
                            | Some content ->
                                item.ResolvedContent <- Some content
                                session.HandleVersion <- session.HandleVersion + 1
                                bump workspace

                                let remaining =
                                    session.ConflictItems
                                    |> List.filter (fun candidate -> candidate.ResolvedContent.IsNone)

                                return
                                    OperationResult.succeeded {
                                        RefreshedHandle = {
                                            SessionId = session.SessionId
                                            Version = string session.HandleVersion
                                        }
                                        RemainingItems = (conflictSummary session).Items
                                    }
                }
            Finalize =
                fun request _ -> async {
                    match validateHandle request.Handle request.ExpectedWorkspaceVersion with
                    | Error() -> return handleRejection ()
                    | Ok session ->
                        if session.ConflictItems |> List.exists (fun item -> item.ResolvedContent.IsNone) then
                            return
                                OperationResult.failed (
                                    OperationFailure.create
                                        Validation
                                        "conflicts_unresolved"
                                        "Every conflict item must be resolved before finalizing."
                                )
                        else
                            // Pre-check the destination, then detect an armed concurrent advance.
                            let observedBefore = workspace.Target.Refs.TryFind workspace.CurrentRef

                            match applyArmedRace workspace with
                            | Some raceRevision ->
                                return
                                    OperationResult.failed {
                                        OperationFailure.create
                                            Concurrency
                                            ConformanceCodes.PreconditionFailed
                                            "The destination advanced between the finalize pre-check and verification." with
                                            RevisionEvidence =
                                                raceEvidence
                                                    (observedBefore |> Option.defaultValue session.TheirRevisionId)
                                                    raceRevision
                                            RecoveryAction =
                                                Some {
                                                    Code = ConflictRecovery.RefreshConflictSession
                                                    Instructions =
                                                        Some "Refresh the conflict session and deliberately retry."
                                                }
                                    }
                            | None ->
                                for item in session.ConflictItems do
                                    match item.ResolvedContent with
                                    | Some content ->
                                        workspace.LocalFiles <- workspace.LocalFiles |> Map.add item.ItemPath content
                                    | None -> ()

                                let localHead = headRevisionId workspace

                                let mergedRevision =
                                    addRevision
                                        workspace.Target
                                        [ localHead; session.TheirRevisionId ]
                                        workspace.LocalFiles
                                        (request.Message |> Option.defaultValue "merge: finalize conflict session")

                                workspace.LocalRefs <- workspace.LocalRefs |> Map.add workspace.CurrentRef mergedRevision
                                workspace.BaseRevisionId <- session.TheirRevisionId
                                workspace.ActiveConflict <- None
                                bump workspace
                                return OperationResult.succeeded (Some(mkRevisionId mergedRevision))
                }
            Cancel =
                fun request _ -> async {
                    match validateHandle request.Handle request.ExpectedWorkspaceVersion with
                    | Error() -> return handleRejection ()
                    | Ok session ->
                        workspace.LocalFiles <- session.PreUpdateFiles
                        workspace.BaseRevisionId <- session.PreUpdateBase
                        workspace.ActiveConflict <- None
                        bump workspace
                        return OperationResult.succeeded ()
                }
        }

    let private createTextDiff (workspace: FakeWorkspace) : TextDiffService =
        let diffFor (path: RepositoryPath) =
            let pathText = pathValue path
            let headFiles = revisionFiles workspace.Target (headRevisionId workspace)
            let localContent = workspace.LocalFiles.TryFind pathText

            match localContent with
            | Some content when content.StartsWith " " ->
                UnsupportedContent(Some "Binary content has no text diff.")
            | _ ->
                let before = headFiles.TryFind pathText |> Option.defaultValue ""
                let after = localContent |> Option.defaultValue ""
                TextContent $"--- {pathText}\n-{before}\n+{after}"

        {
            GetDiff = fun path _ -> async { return OperationResult.succeeded (diffFor path) }
            GetWordDiff = fun path _ -> async { return OperationResult.succeeded (diffFor path) }
            GetBaseContent =
                fun _ _ ->
                    async {
                        return
                            OperationResult.succeeded (
                                UnsupportedContent(Some "Base content is not supported by the fake provider yet.")
                            )
                    }
        }

    let private createBrowser () : RepositoryBrowserService = {
        GetRepositoryWebUrl = fun _ -> async { return OperationResult.succeeded (Some "https://fake.example/repo") }
    }

    let private createSession (workspace: FakeWorkspace) (binding: WorkspaceBinding) : WorkspaceSession =
        let descriptor = {
            ProviderId = fakeId
            WorkspaceRoot = binding.WorkspaceRoot
            Location = Some binding.Location
        }

        {
            WorkspaceSession.createCoreOnly descriptor (createCore workspace) with
                Synchronization = Some(createSynchronization workspace)
                ConflictResolution = Some(createConflictService workspace)
                TextDiff = Some(createTextDiff workspace)
                RepositoryBrowser = Some(createBrowser ())
        }

    /// Builds the complete fake harness with its own isolated state.
    let create () : ProviderTestHarness =
        let mutable locationCounter = 0
        let mutable workspaceCounter = 0
        let targetsByLocation = System.Collections.Generic.Dictionary<string, FakeTarget>()
        let workspacesByRoot = System.Collections.Generic.Dictionary<string, FakeWorkspace>()

        let newLocation (locked: bool) =
            locationCounter <- locationCounter + 1
            let providerLocation = $"fake://repository-{locationCounter}"
            targetsByLocation[providerLocation] <- newTarget locked

            {
                ProviderId = fakeId
                DisplayName = None
                ProviderLocation = providerLocation
                ConnectionProfileId = Some "default-profile"
            }

        let bindingFor (root: string) (location: RepositoryLocation) : WorkspaceBinding = {
            SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
            ProviderId = fakeId
            WorkspaceRoot = root
            ProviderStateRef = None
            Location = location
            ConnectionProfileId = location.ConnectionProfileId
        }

        let getTarget (location: RepositoryLocation) =
            match targetsByLocation.TryGetValue location.ProviderLocation with
            | true, target -> target
            | false, _ ->
                let target = newTarget false
                targetsByLocation[location.ProviderLocation] <- target
                target

        let factory: ProviderFactory = {
            Id = fakeId
            Probe = fun _ -> async { return NotDetected }
            VerifyLocation =
                fun request _ -> async {
                    let target = getTarget request.Location

                    if target.Locked then
                        let denied =
                            request.Intents
                            |> Array.filter (fun intent -> intent <> ReadIntent)

                        return
                            OperationResult.succeeded {
                                Location = request.Location
                                GrantedIntents = request.Intents |> Array.filter (fun intent -> intent = ReadIntent)
                                DeniedIntents = denied
                            }
                    else
                        return
                            OperationResult.succeeded {
                                Location = request.Location
                                GrantedIntents = request.Intents
                                DeniedIntents = [||]
                            }
                }
            Initialize =
                fun request _ -> async {
                    let location = request.Location |> Option.defaultValue (newLocation false)
                    return OperationResult.succeeded (bindingFor request.TargetPath location)
                }
            Clone =
                fun request _ -> async {
                    if workspacesByRoot.ContainsKey request.TargetPath then
                        return
                            OperationResult.failed (
                                OperationFailure.create
                                    Validation
                                    ConformanceCodes.TargetNotEmpty
                                    "The clone target directory is not empty."
                            )
                    else
                        return OperationResult.succeeded (bindingFor request.TargetPath request.Location)
                }
            Adopt =
                fun _ _ ->
                    async {
                        return
                            OperationResult.failed (
                                OperationFailure.create
                                    Unsupported
                                    "operation_not_supported"
                                    "Adoption is not supported by this provider."
                            )
                    }
            Bind =
                fun request _ -> async {
                    return OperationResult.succeeded (bindingFor request.WorkspaceRoot request.Location)
                }
            Open =
                fun binding _ -> async {
                    let target = getTarget binding.Location

                    let workspace =
                        match workspacesByRoot.TryGetValue binding.WorkspaceRoot with
                        | true, existing ->
                            // Bind re-targets an existing workspace: opening it with a new
                            // location switches its target while keeping local state.
                            if not (System.Object.ReferenceEquals(existing.Target, target)) then
                                existing.Target <- target

                            existing
                        | false, _ ->
                            let targetHead = target.Refs.TryFind "main"

                            let baseRevision =
                                match targetHead with
                                | Some head -> head
                                | None ->
                                    // A fresh workspace starts from an empty base revision.
                                    addRevision target [] Map.empty "init: empty base"

                            let created = {
                                Target = target
                                Root = binding.WorkspaceRoot
                                Profile = binding.ConnectionProfileId
                                LocalFiles = revisionFiles target baseRevision
                                BaseRevisionId = baseRevision
                                CurrentRef = "main"
                                LocalRefs = Map.ofList [ "main", baseRevision ]
                                MutationCounter = 0
                                ActiveConflict = None
                            }

                            workspacesByRoot[binding.WorkspaceRoot] <- created
                            created

                    return OperationResult.succeeded (createSession workspace binding)
                }
            CheckDependencies =
                fun _ -> async {
                    return
                        OperationResult.succeeded [|
                            {
                                Component = "fake-engine"
                                Installed = true
                                Version = Some "1.0"
                                Compatible = true
                                Remediation = None
                            }
                        |]
                }
            InstallDependency =
                fun _ _ ->
                    async {
                        return
                            OperationResult.failed (
                                OperationFailure.create
                                    Unsupported
                                    "operation_not_supported"
                                    "Dependency installation is not supported by this provider."
                            )
                    }
        }

        let getWorkspaceState (harnessWorkspace: HarnessWorkspace) =
            workspacesByRoot[harnessWorkspace.Binding.WorkspaceRoot]

        let createHarnessWorkspace (binding: WorkspaceBinding) (session: WorkspaceSession) : HarnessWorkspace =
            let state = workspacesByRoot[binding.WorkspaceRoot]

            {
                Session = session
                Binding = binding
                WriteFile =
                    fun path content -> promise {
                        state.LocalFiles <- state.LocalFiles |> Map.add path content
                        state.MutationCounter <- state.MutationCounter + 1
                        return ()
                    }
                ReadFile = fun path -> promise { return state.LocalFiles.TryFind path }
                RemoveFile =
                    fun path -> promise {
                        state.LocalFiles <- state.LocalFiles |> Map.remove path
                        state.MutationCounter <- state.MutationCounter + 1
                        return ()
                    }
                LocalFileSystemAliasesCase = true
                LocalFileSystemAliasesNormalization = true
                LocalFileSystemWindowsRules = true
            }

        let openWorkspace (binding: WorkspaceBinding) = promise {
            let! openResult = Async.StartAsPromise(factory.Open binding (OperationContext.detached "harness-open"))

            match openResult with
            | Succeeded outcome -> return createHarnessWorkspace binding outcome.Value
            | PartiallySucceeded _
            | Failed _ -> return failwith "The fake factory failed to open a session."
        }

        let createWorkspaceWithBase () = promise {
            workspaceCounter <- workspaceCounter + 1
            let location = newLocation false
            let target = targetsByLocation[location.ProviderLocation]

            // Base revision published on the target so every workspace starts synchronized.
            let baseRevision =
                addRevision target [] (Map.ofList [ "base.txt", "base content\n" ]) "init: base"

            target.Refs <- target.Refs |> Map.add "main" baseRevision

            let binding = bindingFor $"/fake-workspace-{workspaceCounter}" location
            return! openWorkspace binding
        }

        {
            Name = "fake"
            Factory = factory
            ExpectedServices = [ "synchronization"; "text-diff"; "conflicts"; "browser" ]
            CreateLocation = fun () -> promise { return newLocation false }
            CreateUnauthorizedLocation = fun () -> promise { return newLocation true }
            CreateLocalPath =
                fun () -> promise {
                    workspaceCounter <- workspaceCounter + 1
                    return $"/fake-fresh-{workspaceCounter}"
                }
            CreateWorkspace = createWorkspaceWithBase
            CreateLinkedWorkspace =
                fun anchor -> promise {
                    workspaceCounter <- workspaceCounter + 1

                    let linkedLocation = {
                        anchor.Binding.Location with
                            ConnectionProfileId = Some $"linked-profile-{workspaceCounter}"
                    }

                    let binding = {
                        bindingFor $"/fake-linked-{workspaceCounter}" linkedLocation with
                            ConnectionProfileId = linkedLocation.ConnectionProfileId
                    }

                    return! openWorkspace binding
                }
            OpenSecondSession =
                fun anchor -> promise {
                    let! openResult =
                        Async.StartAsPromise(
                            factory.Open anchor.Binding (OperationContext.detached "harness-second-session")
                        )

                    match openResult with
                    | Succeeded outcome -> return outcome.Value
                    | PartiallySucceeded _
                    | Failed _ -> return failwith "The fake factory failed to open a second session."
                }
            AdvanceTarget =
                fun harnessWorkspace mutations -> promise {
                    let state = getWorkspaceState harnessWorkspace
                    let target = state.Target
                    let targetHead = target.Refs.TryFind state.CurrentRef

                    let baseFiles =
                        targetHead
                        |> Option.map (revisionFiles target)
                        |> Option.defaultValue Map.empty

                    let newFiles =
                        mutations
                        |> Array.fold
                            (fun files mutation ->
                                match mutation.Content with
                                | Some content -> Map.add mutation.Path content files
                                | None -> Map.remove mutation.Path files)
                            baseFiles

                    let revision =
                        addRevision target (targetHead |> Option.toList) newFiles "external: target advance"

                    target.Refs <- target.Refs |> Map.add state.CurrentRef revision
                    return ()
                }
            SeedRevisionOnNewRef =
                fun harnessWorkspace refName files -> promise {
                    let state = getWorkspaceState harnessWorkspace
                    let baseFiles = revisionFiles state.Target state.BaseRevisionId

                    let seededFiles =
                        files
                        |> Array.fold (fun map (path, content) -> Map.add path content map) baseFiles

                    let revision =
                        addRevision state.Target [ state.BaseRevisionId ] seededFiles $"seed: {refName}"

                    state.LocalRefs <- state.LocalRefs |> Map.add refName revision
                    return mkProviderRef $"fake:{refName}"
                }
            BreakPublish =
                fun harnessWorkspace -> promise {
                    (getWorkspaceState harnessWorkspace).Target.PublishBroken <- true
                    return ()
                }
            RestorePublish =
                fun harnessWorkspace -> promise {
                    (getWorkspaceState harnessWorkspace).Target.PublishBroken <- false
                    return ()
                }
            ArmDestinationRace =
                fun harnessWorkspace mutations -> promise {
                    (getWorkspaceState harnessWorkspace).Target.RaceMutations <- Some mutations
                    return ()
                }
            ArmSlowTransfer =
                fun harnessWorkspace -> promise {
                    (getWorkspaceState harnessWorkspace).Target.SlowTransferArmed <- true
                    return ()
                }
            Cleanup =
                fun () -> promise {
                    targetsByLocation.Clear()
                    workspacesByRoot.Clear()
                    return ()
                }
        }
