module VersionControlService.LakeFs.LakeFsMaterialization

open System
open Fable.Core
open VersionControlService.Abstractions

module LakeFsIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex
module LakeFsStateStore = VersionControlService.LakeFs.LakeFsStateStore
module NodeBinaryIO = VersionControlService.Runtime.Node.BinaryIO
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeInterop = VersionControlService.Runtime.Node.Interop
module NodePath = VersionControlService.Runtime.Node.Path

type MaterializationObject = {
    Path: RepositoryPath
    ObjectKey: string
    TargetPath: string
    BaseChecksum: string
    Mtime: float
}

type PreparedReplacement = {
    Path: RepositoryPath
    TemporaryPath: string
    TargetPath: string
    Sha256: string
    ExpectedTargetHash: string option
}

type MaterializationPlan = {
    TransactionId: string
    TransactionDirectory: string
    Replacements: PreparedReplacement[]
    Removals: (RepositoryPath * string)[]
    CurrentIndex: LakeFsIndex.WorkspaceIndex
    NextIndex: LakeFsIndex.WorkspaceIndex
}

type MaterializationResult =
    | Materialized of
        index: LakeFsIndex.WorkspaceIndex *
        affectedPaths: string[] *
        warnings: OperationWarning[]
    | MaterializationFailed of failure: OperationFailure
    | MaterializationPartiallyApplied of
        index: LakeFsIndex.WorkspaceIndex option *
        failure: OperationFailure

type PendingRecovery = {
    RecoveryPath: string
    ExpectedWorkspace: string option
    ObservedMaterialization: string
    AffectedPaths: string[]
    Plan: MaterializationPlan
}

[<Emit("JSON.stringify($0, null, 2)")>]
let private jsonStringify (_value: obj) : string = jsNative

let private canceledFailure () =
    OperationFailure.create
        Canceled
        "operation_canceled"
        "The lakeFS materialization was canceled before visible workspace changes."

let private cleanupDirectory path =
    async {
        try
            do!
                NodeFileSystem.rmAsync
                    path
                    (NodeFileSystem.RmOptions(recursive = true, force = true))
                |> Async.AwaitPromise
                |> Async.Ignore

            return Ok()
        with error ->
            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "materialization_cleanup_failed"
                        $"Cleaning the external materialization transaction failed: {error.Message}"
                )
    }

let cleanup (plan: MaterializationPlan) = cleanupDirectory plan.TransactionDirectory

let private revisionEvidence expectedRevision observedRevision =
    [|
        yield!
            expectedRevision
            |> Option.bind (RevisionId.tryCreate >> Result.toOption)
            |> Option.map (fun revision -> "expected_workspace", revision)
            |> Option.toList

        yield!
            RevisionId.tryCreate observedRevision
            |> Result.toOption
            |> Option.map (fun revision -> "observed_materialization", revision)
            |> Option.toList
    |]

let private recoveryPath recoveryDirectory transactionId =
    NodePath.join [| recoveryDirectory; $"materialization-{transactionId}.json" |]

let private deleteRecovery recoveryPath =
    try
        if NodeFileSystem.existsSync recoveryPath then
            NodeFileSystem.unlinkSync recoveryPath

        Ok()
    with error ->
        Error(
            OperationFailure.createRedacted
                ProviderError
                "materialization_recovery_delete_failed"
                $"Deleting lakeFS materialization recovery metadata failed: {error.Message}"
        )

let private recoveryFailure
    recoveryPaths
    expectedRevision
    observedRevision
    affectedPaths
    (source: OperationFailure)
    =
    {
        source with
            StateChanged = true
            Retryable = true
            AffectedPaths =
                Array.append source.AffectedPaths affectedPaths
                |> Array.distinct
            RecoveryAction = Some(LakeFsStateStore.reconcileRecoveryAction recoveryPaths)
            RevisionEvidence =
                Array.append
                    source.RevisionEvidence
                    (revisionEvidence expectedRevision observedRevision)
    }

let private writeRecovery
    recoveryDirectory
    expectedRevision
    observedRevision
    (plan: MaterializationPlan)
    =
    let path = recoveryPath recoveryDirectory plan.TransactionId

    let record: LakeFsStateStore.RecoveryRecord = {
        SchemaVersion = LakeFsStateStore.RecoverySchemaVersion
        TransactionId = plan.TransactionId
        TransactionDirectory = plan.TransactionDirectory
        ExpectedWorkspace = expectedRevision
        ObservedMaterialization = observedRevision
        AffectedPaths = [||]
        Replacements =
            plan.Replacements
            |> Array.map (fun replacement -> {
                Path = RepositoryPath.value replacement.Path
                TemporaryPath = replacement.TemporaryPath
                TargetPath = replacement.TargetPath
                Sha256 = replacement.Sha256
                ExpectedTargetHash = replacement.ExpectedTargetHash
            })
        Removals =
            plan.Removals
            |> Array.map (fun (path, targetPath) -> {
                Path = RepositoryPath.value path
                TargetPath = targetPath
            })
        CurrentIndex = box plan.CurrentIndex
        NextIndex = box plan.NextIndex
    }

    try
        if not (NodeFileSystem.existsSync path) then
            NodeFileSystem.writeUtf8FileExclusiveAndFlushSync path (jsonStringify record)

        Ok()
    with error ->
        Error(
            OperationFailure.createRedacted
                ProviderError
                "materialization_recovery_write_failed"
                $"Writing lakeFS materialization recovery metadata failed: {error.Message}"
        )

let private recoveryCorrupt path message =
    {
        OperationFailure.createRedacted
            ProviderError
            "materialization_recovery_corrupt"
            message with
            RecoveryAction = Some(LakeFsStateStore.reconcileRecoveryAction [| path |])
    }

let private toRepositoryPath recoveryPath path =
    RepositoryPath.tryCreate path
    |> Result.mapError (fun message ->
        recoveryCorrupt
            recoveryPath
            $"The retained materialization path in '{recoveryPath}' is invalid: {message}.")

let private planOwnedPaths (plan: MaterializationPlan) =
    [|
        yield!
            plan.Replacements
            |> Array.map (fun replacement -> RepositoryPath.value replacement.Path)
        yield!
            plan.Removals
            |> Array.map (fun (path, _) -> RepositoryPath.value path)
        yield!
            plan.NextIndex.Entries
            |> Array.choose (fun next ->
                match plan.CurrentIndex.Entries |> Array.tryFind (fun current -> current.Path = next.Path) with
                | Some current when current = next -> None
                | _ -> Some next.Path)
        yield!
            plan.CurrentIndex.Entries
            |> Array.choose (fun current ->
                if plan.NextIndex.Entries |> Array.exists (fun next -> next.Path = current.Path) then
                    None
                else
                    Some current.Path)
    |]
    |> Array.distinct

let private pendingRecoveryFromRecord (loaded: LakeFsStateStore.LoadedRecoveryRecord) =
    let record = loaded.Record

    let replacements =
        record.Replacements
        |> Array.fold
            (fun result replacement ->
                result
                |> Result.bind (fun values ->
                    toRepositoryPath loaded.Path replacement.Path
                    |> Result.map (fun path ->
                        Array.append
                            values
                            [| {
                                   Path = path
                                   TemporaryPath = replacement.TemporaryPath
                                   TargetPath = replacement.TargetPath
                                   Sha256 = replacement.Sha256
                                   ExpectedTargetHash = replacement.ExpectedTargetHash
                               } |])))
            (Ok [||])

    let removals =
        record.Removals
        |> Array.fold
            (fun result removal ->
                result
                |> Result.bind (fun values ->
                    toRepositoryPath loaded.Path removal.Path
                    |> Result.map (fun path ->
                        Array.append values [| path, removal.TargetPath |])))
            (Ok [||])

    try
        let currentIndex = unbox<LakeFsIndex.WorkspaceIndex> record.CurrentIndex
        let nextIndex = unbox<LakeFsIndex.WorkspaceIndex> record.NextIndex

        if
            currentIndex.SchemaVersion <> LakeFsIndex.CurrentSchemaVersion
            || nextIndex.SchemaVersion <> LakeFsIndex.CurrentSchemaVersion
        then
            Error(
                recoveryCorrupt
                    loaded.Path
                    $"The retained materialization indexes in '{loaded.Path}' use an unsupported schema."
            )
        else
            match replacements, removals with
            | Ok replacementValues, Ok removalValues ->
                let plan = {
                    TransactionId = record.TransactionId
                    TransactionDirectory = record.TransactionDirectory
                    Replacements = replacementValues
                    Removals = removalValues
                    CurrentIndex = currentIndex
                    NextIndex = nextIndex
                }

                Ok {
                    RecoveryPath = loaded.Path
                    ExpectedWorkspace = record.ExpectedWorkspace
                    ObservedMaterialization = record.ObservedMaterialization
                    AffectedPaths =
                        if record.AffectedPaths.Length = 0 then
                            planOwnedPaths plan
                        else
                            record.AffectedPaths
                    Plan = plan
                }
            | Error failure, _
            | _, Error failure -> Error failure
    with error ->
        Error(
            recoveryCorrupt
                loaded.Path
                $"Reading retained materialization indexes from '{loaded.Path}' failed: {error.Message}"
        )

let loadPendingRecoveries recoveryDirectory : Result<PendingRecovery[], OperationFailure> =
    LakeFsStateStore.loadRecoveryRecords recoveryDirectory
    |> Result.bind (fun records ->
        records
        |> Array.fold
            (fun pending loaded ->
                pending
                |> Result.bind (fun values ->
                    pendingRecoveryFromRecord loaded
                    |> Result.map (fun value -> Array.append values [| value |])))
            (Ok [||]))

let pendingFailure (pending: PendingRecovery[]) =
    let recoveryPaths = pending |> Array.map _.RecoveryPath

    {
        OperationFailure.create
            ProviderError
            "materialization_recovery_pending"
            "A retained lakeFS materialization must be reconciled before another mutation." with
            StateChanged = true
            Retryable = true
            AffectedPaths =
                pending
                |> Array.collect _.AffectedPaths
                |> Array.distinct
            Details = recoveryPaths
            RecoveryAction = Some(LakeFsStateStore.reconcileRecoveryAction recoveryPaths)
    }

let private inspectTarget workspaceRoot (replacement: PreparedReplacement) =
    LakeFsPathSafety.inspectFile workspaceRoot replacement.Path
    |> Result.map (Option.map _.Sha256)

let private installReplacement workspaceRoot (replacement: PreparedReplacement) =
    let stagingPath =
        $"{replacement.TemporaryPath}.install-{NodeInterop.randomUuid()}.tmp"

    let mutable identity: NodeFileSystem.Stats option = None

    try
        try
            let created =
                NodeFileSystem.copyFileExclusiveAndFlushWithIdentitySync
                    replacement.TemporaryPath
                    stagingPath

            identity <- Some created

            LakeFsPathSafety.replaceFileFromTemporary
                workspaceRoot
                replacement.Path
                stagingPath
        with error ->
            Error(
                OperationFailure.createRedacted
                    ProviderError
                    "workspace_replace_failed"
                    $"Replacing a workspace file failed: {error.Message}"
            )
    finally
        identity
        |> Option.iter (fun created ->
            NodeFileSystem.removeFileIfIdentityMatchesSync stagingPath created
            |> ignore)

let private updateIndexForPath
    (plan: MaterializationPlan)
    (index: LakeFsIndex.WorkspaceIndex)
    (path: string)
    =
    let isRemoval =
        plan.Removals
        |> Array.exists (fun (removalPath, _) -> RepositoryPath.value removalPath = path)

    if isRemoval then
        {
            index with
                Entries = index.Entries |> Array.filter (fun entry -> entry.Path <> path)
        }
    else
        match plan.NextIndex.Entries |> Array.tryFind (fun entry -> entry.Path = path) with
        | None -> index
        | Some desired ->
            let mutable found = false

            let entries =
                index.Entries
                |> Array.map (fun entry ->
                    if entry.Path = path then
                        found <- true
                        desired
                    else
                        entry)

            {
                index with
                    Entries =
                        if found then
                            entries
                        else
                            Array.append entries [| desired |]
            }

let private graftMetadata (plan: MaterializationPlan) (index: LakeFsIndex.WorkspaceIndex) =
    let choose current expected desired =
        if current = expected then desired else current

    {
        index with
            SchemaVersion =
                choose index.SchemaVersion plan.CurrentIndex.SchemaVersion plan.NextIndex.SchemaVersion
            Repository = choose index.Repository plan.CurrentIndex.Repository plan.NextIndex.Repository
            TargetRef = choose index.TargetRef plan.CurrentIndex.TargetRef plan.NextIndex.TargetRef
            Prefix = choose index.Prefix plan.CurrentIndex.Prefix plan.NextIndex.Prefix
            WorkspaceBranch =
                choose
                    index.WorkspaceBranch
                    plan.CurrentIndex.WorkspaceBranch
                    plan.NextIndex.WorkspaceBranch
            OwnershipToken =
                choose
                    index.OwnershipToken
                    plan.CurrentIndex.OwnershipToken
                    plan.NextIndex.OwnershipToken
            BaseRevision =
                choose index.BaseRevision plan.CurrentIndex.BaseRevision plan.NextIndex.BaseRevision
            WorkspaceRevision =
                choose
                    index.WorkspaceRevision
                    plan.CurrentIndex.WorkspaceRevision
                    plan.NextIndex.WorkspaceRevision
            Generation = index.Generation
    }

let private loadIndexForApply stateDirectory (plan: MaterializationPlan) =
    match LakeFsIndex.load stateDirectory with
    | LakeFsIndex.Loaded index
        when index.Repository = plan.CurrentIndex.Repository
             && index.WorkspaceBranch = plan.CurrentIndex.WorkspaceBranch
             && index.OwnershipToken = plan.CurrentIndex.OwnershipToken ->
        Ok(index, true)
    | LakeFsIndex.Loaded _ ->
        Error(
            OperationFailure.create
                Concurrency
                "workspace_binding_changed"
                "The persisted lakeFS workspace ownership changed; reopen the session."
        )
    | LakeFsIndex.Missing -> Ok(plan.CurrentIndex, false)
    | LakeFsIndex.Corrupt message ->
        Error(
            OperationFailure.createRedacted
                ProviderError
                "workspace_index_corrupt"
                message
        )

let private appendFailureDetails (source: OperationFailure) (details: string[]) =
    {
        source with
            Details = Array.append source.Details details
    }

let apply
    (workspaceRoot: string)
    (stateDirectory: string)
    (recoveryDirectory: string)
    (expectedRevision: string option)
    (observedRevision: string)
    (plan: MaterializationPlan)
    (barrier: string -> OperationContext -> Async<unit>)
    (context: OperationContext)
    : Async<MaterializationResult> =
    async {
        let recordPath = recoveryPath recoveryDirectory plan.TransactionId
        let recoveryExisted = NodeFileSystem.existsSync recordPath

        match writeRecovery recoveryDirectory expectedRevision observedRevision plan with
        | Error writeFailure when NodeFileSystem.existsSync recordPath ->
            return
                MaterializationPartiallyApplied(
                    None,
                    recoveryFailure
                        [| recordPath |]
                        expectedRevision
                        observedRevision
                        [||]
                        writeFailure
                )
        | Error writeFailure ->
            let! cleaned = cleanup plan

            return
                MaterializationFailed(
                    match cleaned with
                    | Ok() -> writeFailure
                    | Error cleanupFailure ->
                        appendFailureDetails cleanupFailure [| writeFailure.Code; writeFailure.Message |]
                )
        | Ok() ->
            match loadIndexForApply stateDirectory plan with
            | Error failure ->
                return
                    MaterializationPartiallyApplied(
                        None,
                        recoveryFailure
                            [| recordPath |]
                            expectedRevision
                            observedRevision
                            [||]
                            failure
                    )
            | Ok(initialIndex, indexWasPersisted) ->
                let affectedPaths = ResizeArray<string>()
                let mutable failure: OperationFailure option = None
                let mutable visibleIndex = initialIndex
                let mutable indexChanged = false
                let mutable persistedIndex = if indexWasPersisted then Some initialIndex else None

                let addAffected path =
                    if not (affectedPaths.Contains path) then
                        affectedPaths.Add path

                let updateVisible path =
                    let updated = updateIndexForPath plan visibleIndex path

                    if updated <> visibleIndex then
                        visibleIndex <- updated
                        indexChanged <- true

                let canceledDuringApply () =
                    OperationFailure.create
                        Canceled
                        "operation_canceled"
                        "The lakeFS materialization was canceled during workspace apply."

                let divergedFailure path =
                    {
                        OperationFailure.create
                            Conflict
                            "materialization_target_diverged"
                            "A workspace path changed after partial materialization and was not overwritten." with
                            Retryable = true
                            AffectedPaths = [| path |]
                            RecoveryAction =
                                Some(LakeFsStateStore.reconcileRecoveryAction [| recordPath |])
                    }

                try
                    for replacement in plan.Replacements do
                        if failure.IsNone then
                            if context.Cancellation.IsCancellationRequested() then
                                failure <- Some(canceledDuringApply ())
                            else
                                let path = RepositoryPath.value replacement.Path

                                match inspectTarget workspaceRoot replacement with
                                | Error inspectionFailure -> failure <- Some inspectionFailure
                                | Ok(Some observedHash) when observedHash = replacement.Sha256 ->
                                    updateVisible path
                                | Ok observedHash
                                    when recoveryExisted
                                         && observedHash <> replacement.ExpectedTargetHash ->
                                    failure <- Some(divergedFailure path)
                                | Ok _ ->
                                    match installReplacement workspaceRoot replacement with
                                    | Error replaceFailure ->
                                        if replaceFailure.StateChanged then
                                            addAffected path

                                        failure <- Some replaceFailure
                                    | Ok() ->
                                        addAffected path
                                        updateVisible path
                                        do! barrier "materialization-apply-object" context

                                        if context.Cancellation.IsCancellationRequested() then
                                            failure <- Some(canceledDuringApply ())

                    for removalPath, targetPath in plan.Removals do
                        if failure.IsNone then
                            if context.Cancellation.IsCancellationRequested() then
                                failure <- Some(canceledDuringApply ())
                            else
                                let path = RepositoryPath.value removalPath
                                let existed = NodeFileSystem.tryLstatSync targetPath |> Option.isSome

                                match LakeFsPathSafety.removeFile workspaceRoot removalPath with
                                | Error removalFailure ->
                                    if removalFailure.StateChanged then
                                        addAffected path
                                        updateVisible path

                                    failure <- Some removalFailure
                                | Ok() ->
                                    if existed then
                                        addAffected path

                                    updateVisible path

                                    if existed then
                                        do! barrier "materialization-apply-object" context

                                        if context.Cancellation.IsCancellationRequested() then
                                            failure <- Some(canceledDuringApply ())

                    match failure with
                    | None ->
                        for path in planOwnedPaths plan do
                            updateVisible path

                        let finalIndex = graftMetadata plan visibleIndex
                        indexChanged <- indexChanged || finalIndex <> visibleIndex
                        visibleIndex <- finalIndex

                        match LakeFsIndex.save stateDirectory visibleIndex with
                        | Ok saved ->
                            persistedIndex <- Some saved
                            visibleIndex <- saved
                        | Error message ->
                            failure <-
                                Some(
                                    OperationFailure.createRedacted
                                        ProviderError
                                        "index_write_failed"
                                        message
                                )
                    | Some _ when indexChanged ->
                        match LakeFsIndex.save stateDirectory visibleIndex with
                        | Ok saved ->
                            persistedIndex <- Some saved
                            visibleIndex <- saved
                        | Error message ->
                            let source = failure |> Option.get

                            failure <-
                                Some(
                                    appendFailureDetails
                                        source
                                        [| "index_write_failed"; message |]
                                )
                    | Some _ -> ()
                with error ->
                    failure <-
                        Some(
                            OperationFailure.createRedacted
                                ProviderError
                                "materialization_apply_failed"
                                $"Applying lakeFS workspace materialization failed: {error.Message}"
                        )

                    if indexChanged then
                        match LakeFsIndex.save stateDirectory visibleIndex with
                        | Ok saved -> persistedIndex <- Some saved
                        | Error message ->
                            failure <-
                                failure
                                |> Option.map (fun source ->
                                    appendFailureDetails
                                        source
                                        [| "index_write_failed"; message |])

                let visiblePaths = affectedPaths.ToArray()

                match failure, persistedIndex with
                | None, Some saved ->
                    match deleteRecovery recordPath with
                    | Error deleteFailure ->
                        return
                            MaterializationPartiallyApplied(
                                Some saved,
                                recoveryFailure
                                    [| recordPath |]
                                    expectedRevision
                                    observedRevision
                                    visiblePaths
                                    deleteFailure
                            )
                    | Ok() ->
                        let! cleaned = cleanup plan

                        let warnings =
                            match cleaned with
                            | Ok() -> [||]
                            | Error cleanupFailure -> [| {
                                  Code = cleanupFailure.Code
                                  Message = cleanupFailure.Message
                              } |]

                        return Materialized(saved, visiblePaths, warnings)
                | None, None ->
                    let source =
                        OperationFailure.create
                            ProviderError
                            "materialization_apply_failed"
                            "The materialization finished without publishing an index."

                    return
                        MaterializationPartiallyApplied(
                            None,
                            recoveryFailure
                                [| recordPath |]
                                expectedRevision
                                observedRevision
                                visiblePaths
                                source
                        )
                | Some source, saved ->
                    return
                        MaterializationPartiallyApplied(
                            saved,
                            recoveryFailure
                                [| recordPath |]
                                expectedRevision
                                observedRevision
                                visiblePaths
                                source
                        )
    }

let reapplyPending
    (workspaceRoot: string)
    (stateDirectory: string)
    (recoveryDirectory: string)
    (barrier: string -> OperationContext -> Async<unit>)
    (context: OperationContext)
    : Async<Result<MaterializationResult option, OperationFailure>> =
    async {
        match loadPendingRecoveries recoveryDirectory with
        | Error failure -> return Error failure
        | Ok pending when pending.Length = 0 -> return Ok None
        | Ok pending ->
            let affectedPaths = ResizeArray<string>()
            let warnings = ResizeArray<OperationWarning>()
            let mutable lastIndex: LakeFsIndex.WorkspaceIndex option = None
            let mutable stopped: MaterializationResult option = None

            let collect paths =
                for path in paths do
                    if not (affectedPaths.Contains path) then
                        affectedPaths.Add path

            for recovery in pending do
                if stopped.IsNone then
                    let! reapplied =
                        apply
                            workspaceRoot
                            stateDirectory
                            recoveryDirectory
                            recovery.ExpectedWorkspace
                            recovery.ObservedMaterialization
                            recovery.Plan
                            barrier
                            context

                    match reapplied with
                    | Materialized(index, paths, recoveryWarnings) ->
                        lastIndex <- Some index
                        collect paths
                        warnings.AddRange recoveryWarnings
                    | MaterializationFailed failure ->
                        stopped <-
                            Some(
                                MaterializationFailed {
                                    failure with
                                        StateChanged = failure.StateChanged || affectedPaths.Count > 0
                                        AffectedPaths =
                                            Array.append (affectedPaths.ToArray()) failure.AffectedPaths
                                            |> Array.distinct
                                }
                            )
                    | MaterializationPartiallyApplied(index, failure) ->
                        stopped <-
                            Some(
                                MaterializationPartiallyApplied(
                                    index,
                                    {
                                        failure with
                                            AffectedPaths =
                                                Array.append
                                                    (affectedPaths.ToArray())
                                                    failure.AffectedPaths
                                                |> Array.distinct
                                    }
                                )
                            )

            return
                match stopped, lastIndex with
                | Some result, _ -> Ok(Some result)
                | None, Some index ->
                    Ok(
                        Some(
                            Materialized(
                                index,
                                affectedPaths.ToArray(),
                                warnings.ToArray()
                            )
                        )
                    )
                | None, None -> Ok None
    }

let private prepareInternal
    (retainUnselectedEntries: bool)
    (transactionsDirectory: string)
    (currentIndex: LakeFsIndex.WorkspaceIndex)
    (preserveExistingFiles: bool)
    (objects: MaterializationObject[])
    (removals: (RepositoryPath * string)[])
    (download:
        MaterializationObject ->
        string ->
        OperationContext ->
        Async<Result<NodeBinaryIO.StreamCopyResult, OperationFailure>>)
    (barrier: string -> OperationContext -> Async<unit>)
    (context: OperationContext)
    : Async<Result<MaterializationPlan, OperationFailure>> =
    async {
        let transactionId = NodeInterop.randomUuid()
        let transactionDirectory = NodePath.join [| transactionsDirectory; transactionId |]

        try
            NodeFileSystem.mkdirSync transactionDirectory (NodeFileSystem.MkdirOptions(recursive = false))
            let replacements = ResizeArray<PreparedReplacement>()
            let entries = ResizeArray<LakeFsIndex.IndexEntry>()
            let mutable failure: OperationFailure option = None

            for index, preparedObject in objects |> Array.indexed do
                if failure.IsNone then
                    if context.Cancellation.IsCancellationRequested() then
                        failure <- Some(canceledFailure ())
                    else
                        let temporaryPath =
                            NodePath.join [| transactionDirectory; $"object-{index:D8}.tmp" |]

                        let! downloaded = download preparedObject temporaryPath context

                        match downloaded with
                        | Error downloadFailure -> failure <- Some downloadFailure
                        | Ok transfer ->
                            entries.Add {
                                Path = RepositoryPath.value preparedObject.Path
                                BaseChecksum = preparedObject.BaseChecksum
                                LocalHash = transfer.Sha256
                                LocalSize = transfer.BytesCopied
                                LocalMtimeMs = preparedObject.Mtime
                            }

                            if
                                not preserveExistingFiles
                                || not (NodeFileSystem.existsSync preparedObject.TargetPath)
                            then
                                let expectedTargetHash =
                                    try
                                        match NodeFileSystem.tryLstatSync preparedObject.TargetPath with
                                        | Some stats when stats.isFile() && not (stats.isSymbolicLink()) ->
                                            Some(
                                                (NodeFileSystem.hashFileNoFollowSync
                                                    preparedObject.TargetPath).Sha256
                                            )
                                        | _ -> None
                                    with _ ->
                                        None

                                replacements.Add {
                                    Path = preparedObject.Path
                                    TemporaryPath = temporaryPath
                                    TargetPath = preparedObject.TargetPath
                                    Sha256 = transfer.Sha256
                                    ExpectedTargetHash = expectedTargetHash
                                }

                            do! barrier "materialization-prepare-object" context

                            if context.Cancellation.IsCancellationRequested() then
                                failure <- Some(canceledFailure ())

            match failure with
            | Some prepareFailure ->
                let! cleaned = cleanupDirectory transactionDirectory

                return
                    match cleaned with
                    | Ok() -> Error prepareFailure
                    | Error cleanupFailure ->
                        Error {
                            cleanupFailure with
                                Details = [| prepareFailure.Code; prepareFailure.Message |]
                        }
            | None ->
                let selectedPaths =
                    [|
                        yield! objects |> Array.map (fun preparedObject -> RepositoryPath.value preparedObject.Path)
                        yield! removals |> Array.map (fun (path, _) -> RepositoryPath.value path)
                    |]
                    |> Set.ofArray

                let nextEntries =
                    if retainUnselectedEntries then
                        Array.append
                            (currentIndex.Entries
                             |> Array.filter (fun entry -> not (selectedPaths.Contains entry.Path)))
                            (entries.ToArray())
                    else
                        entries.ToArray()

                return
                    Ok {
                        TransactionId = transactionId
                        TransactionDirectory = transactionDirectory
                        Replacements = replacements.ToArray()
                        Removals = removals
                        CurrentIndex = currentIndex
                        NextIndex = {
                            currentIndex with
                                Entries = nextEntries
                        }
                    }
        with error ->
            let! _ = cleanupDirectory transactionDirectory

            return
                Error(
                    OperationFailure.createRedacted
                        ProviderError
                        "materialization_prepare_failed"
                        $"Preparing lakeFS workspace materialization failed: {error.Message}"
                )
    }

let prepare
    transactionsDirectory
    currentIndex
    preserveExistingFiles
    objects
    removals
    download
    barrier
    context
    =
    prepareInternal
        false
        transactionsDirectory
        currentIndex
        preserveExistingFiles
        objects
        removals
        download
        barrier
        context

let prepareSelected
    transactionsDirectory
    currentIndex
    preserveExistingFiles
    objects
    removals
    download
    barrier
    context
    =
    prepareInternal
        true
        transactionsDirectory
        currentIndex
        preserveExistingFiles
        objects
        removals
        download
        barrier
        context
