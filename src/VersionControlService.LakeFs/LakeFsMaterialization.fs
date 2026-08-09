module VersionControlService.LakeFs.LakeFsMaterialization

open Fable.Core
open VersionControlService.Abstractions

module LakeFsIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex
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
}

type MaterializationPlan = {
    TransactionId: string
    TransactionDirectory: string
    Replacements: PreparedReplacement[]
    Removals: (RepositoryPath * string)[]
    NextIndex: LakeFsIndex.WorkspaceIndex
}

type MaterializationResult =
    | Materialized of index: LakeFsIndex.WorkspaceIndex * affectedPaths: string[]
    | MaterializationFailed of failure: OperationFailure
    | MaterializationPartiallyApplied of
        index: LakeFsIndex.WorkspaceIndex option *
        failure: OperationFailure

type private RecoveryRecord = {
    SchemaVersion: int
    TransactionId: string
    ExpectedWorkspace: string option
    ObservedMaterialization: string
    AffectedPaths: string[]
    FailureCode: string
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

let private recoveryFailure
    expectedRevision
    observedRevision
    affectedPaths
    (source: OperationFailure)
    =
    {
        source with
            StateChanged = true
            Retryable = true
            AffectedPaths = affectedPaths
            RecoveryAction =
                Some {
                    Code = "reconcile_materialization"
                    Instructions =
                        Some
                            "Refresh the workspace, inspect the listed paths, and retry materialization deliberately."
                }
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
    (affectedPaths: string[])
    (source: OperationFailure)
    =
    let recoveryPath =
        NodePath.join [| recoveryDirectory; $"materialization-{plan.TransactionId}.json" |]

    let record = {
        SchemaVersion = 1
        TransactionId = plan.TransactionId
        ExpectedWorkspace = expectedRevision
        ObservedMaterialization = observedRevision
        AffectedPaths = affectedPaths
        FailureCode = source.Code
    }

    try
        NodeFileSystem.writeUtf8FileExclusiveAndFlushSync recoveryPath (jsonStringify record)
        Ok()
    with error ->
        Error(
            OperationFailure.createRedacted
                ProviderError
                "materialization_recovery_write_failed"
                $"Writing lakeFS materialization recovery metadata failed: {error.Message}"
        )

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
        let affectedPaths = ResizeArray<string>()
        let mutable failure: OperationFailure option = None
        let mutable persistedIndex: LakeFsIndex.WorkspaceIndex option = None

        let canceledDuringApply () =
            OperationFailure.create
                Canceled
                "operation_canceled"
                "The lakeFS materialization was canceled during workspace apply."

        try
            for replacement in plan.Replacements do
                if failure.IsNone then
                    if context.Cancellation.IsCancellationRequested() then
                        failure <- Some(canceledDuringApply ())
                    else
                        match
                            LakeFsPathSafety.replaceFileFromTemporary
                                workspaceRoot
                                replacement.Path
                                replacement.TemporaryPath
                        with
                        | Error replaceFailure ->
                            if replaceFailure.StateChanged then
                                affectedPaths.Add(RepositoryPath.value replacement.Path)

                            failure <- Some replaceFailure
                        | Ok() ->
                            affectedPaths.Add(RepositoryPath.value replacement.Path)
                            do! barrier "materialization-apply-object" context

                            if context.Cancellation.IsCancellationRequested() then
                                failure <- Some(canceledDuringApply ())

            for removalPath, targetPath in plan.Removals do
                if failure.IsNone then
                    if context.Cancellation.IsCancellationRequested() then
                        failure <- Some(canceledDuringApply ())
                    else
                        let existed = NodeFileSystem.tryLstatSync targetPath |> Option.isSome

                        match LakeFsPathSafety.removeFile workspaceRoot removalPath with
                        | Error removalFailure -> failure <- Some removalFailure
                        | Ok() ->
                            if existed then
                                affectedPaths.Add(RepositoryPath.value removalPath)
                                do! barrier "materialization-apply-object" context

                                if context.Cancellation.IsCancellationRequested() then
                                    failure <- Some(canceledDuringApply ())

            if failure.IsNone then
                match LakeFsIndex.save stateDirectory plan.NextIndex with
                | Ok saved -> persistedIndex <- Some saved
                | Error message ->
                    failure <-
                        Some(
                            OperationFailure.createRedacted
                                ProviderError
                                "index_write_failed"
                                message
                        )
        with error ->
            failure <-
                Some(
                    OperationFailure.createRedacted
                        ProviderError
                        "materialization_apply_failed"
                        $"Applying lakeFS workspace materialization failed: {error.Message}"
                )

        let visiblePaths = affectedPaths.ToArray()

        match failure, persistedIndex with
        | None, Some saved ->
            let! cleaned = cleanup plan

            match cleaned with
            | Ok() -> return Materialized(saved, visiblePaths)
            | Error cleanupFailure ->
                let recovery = recoveryFailure expectedRevision observedRevision visiblePaths cleanupFailure
                let _ = writeRecovery recoveryDirectory expectedRevision observedRevision plan visiblePaths recovery
                return MaterializationPartiallyApplied(Some saved, recovery)
        | None, None ->
            let! cleaned = cleanup plan

            return
                match cleaned with
                | Ok() ->
                    MaterializationFailed(
                        OperationFailure.create
                            ProviderError
                            "materialization_apply_failed"
                            "The materialization finished without publishing an index."
                    )
                | Error cleanupFailure -> MaterializationFailed cleanupFailure
        | Some source, _ when visiblePaths.Length = 0 ->
            let! cleaned = cleanup plan

            return
                match cleaned with
                | Ok() -> MaterializationFailed source
                | Error cleanupFailure ->
                    MaterializationFailed {
                        cleanupFailure with
                            Details =
                                Array.append
                                    cleanupFailure.Details
                                    [| source.Code; source.Message |]
                    }
        | Some source, saved ->
            let recovery = recoveryFailure expectedRevision observedRevision visiblePaths source
            let recoveryWrite =
                writeRecovery recoveryDirectory expectedRevision observedRevision plan visiblePaths recovery

            let! cleaned = cleanup plan
            let details =
                [|
                    match recoveryWrite with
                    | Error recoveryWriteFailure -> yield recoveryWriteFailure.Code
                    | Ok() -> ()

                    match cleaned with
                    | Error cleanupFailure -> yield cleanupFailure.Code
                    | Ok() -> ()
                |]

            return
                MaterializationPartiallyApplied(
                    saved,
                    { recovery with Details = Array.append recovery.Details details }
                )
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
                                replacements.Add {
                                    Path = preparedObject.Path
                                    TemporaryPath = temporaryPath
                                    TargetPath = preparedObject.TargetPath
                                    Sha256 = transfer.Sha256
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
