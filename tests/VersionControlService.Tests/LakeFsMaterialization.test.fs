module VersionControlService.Tests.LakeFsMaterializationTests

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Tests.NodePath
open Vitest

module LakeFsIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex
module LakeFsMaterialization = VersionControlService.LakeFs.LakeFsMaterialization
module NodeBinaryIO = VersionControlService.Runtime.Node.BinaryIO
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-lakefs-materialization-" |]
    fsPromisesDynamic?mkdtemp prefix |> unbox<JS.Promise<string>>

let private removeDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (
            path,
            createObj [ "recursive" ==> true; "force" ==> true; "maxRetries" ==> 10; "retryDelay" ==> 50 ]
        )
        |> unbox<JS.Promise<obj>>

    return ()
}

let private repositoryPath value =
    RepositoryPath.tryCreate value |> Result.defaultWith failwith

let private emptyIndex: LakeFsIndex.WorkspaceIndex = {
    SchemaVersion = LakeFsIndex.CurrentSchemaVersion
    Repository = "repo"
    TargetRef = "main"
    Prefix = ""
    WorkspaceBranch = "workspace"
    OwnershipToken = "token"
    BaseRevision = None
    WorkspaceRevision = None
    Generation = 0
    Entries = [||]
}

Vitest.describe (
    "lakeFS materialization preparation",
    fun () ->
        Vitest.test (
            "two objects in one materialization transaction get distinct temporary files",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let transactionsDirectory = join [| root; "transactions" |]
                let! _ = fsPromisesDynamic?mkdir transactionsDirectory |> unbox<JS.Promise<obj>>

                try
                    let objects: LakeFsMaterialization.MaterializationObject[] =
                        [|
                            {
                                Path = repositoryPath "first.bin"
                                ObjectKey = "first"
                                TargetPath = join [| root; "first.bin" |]
                                BaseChecksum = "base-first"
                                Mtime = 1.0
                            }
                            {
                                Path = repositoryPath "second.bin"
                                ObjectKey = "second"
                                TargetPath = join [| root; "second.bin" |]
                                BaseChecksum = "base-second"
                                Mtime = 2.0
                            }
                        |]

                    let download
                        (prepared: LakeFsMaterialization.MaterializationObject)
                        (temporaryPath: string)
                        (_: OperationContext)
                        : Async<Result<NodeBinaryIO.StreamCopyResult, OperationFailure>> =
                        async {
                            NodeFileSystem.writeUtf8FileExclusiveAndFlushSync
                                temporaryPath
                                $"body-{prepared.ObjectKey}"

                            return
                                Ok {
                                    BytesCopied = 1.0
                                    Sha256 = $"hash-{prepared.ObjectKey}"
                                }
                        }

                    let barrier (_: string) (_: OperationContext) = async { return () }

                    let! preparedResult =
                        LakeFsMaterialization.prepare
                            transactionsDirectory
                            emptyIndex
                            false
                            objects
                            [||]
                            download
                            barrier
                            (OperationContext.detached "lakefs-materialization-temporary-files")
                        |> Async.StartAsPromise

                    match preparedResult with
                    | Error failure -> failwith $"Materialization preparation failed: {failure.Code}"
                    | Ok plan ->
                        let temporaryPaths = plan.Replacements |> Array.map _.TemporaryPath

                        Vitest.expect(temporaryPaths.Length).toBe 2
                        Vitest.expect((temporaryPaths |> Array.distinct).Length).toBe 2
                        Vitest.expect(temporaryPaths |> Array.forall NodeFileSystem.existsSync).toBe true

                        let! cleaned = LakeFsMaterialization.cleanup plan |> Async.StartAsPromise

                        match cleaned with
                        | Ok() -> ()
                        | Error failure -> failwith $"Materialization cleanup failed: {failure.Code}"

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )

        Vitest.test (
            "selected materialization retains unselected index entries",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let transactionsDirectory = join [| root; "transactions" |]
                let! _ = fsPromisesDynamic?mkdir transactionsDirectory |> unbox<JS.Promise<obj>>

                try
                    let retainedEntry: LakeFsIndex.IndexEntry = {
                        Path = "retained.bin"
                        BaseChecksum = "base-retained"
                        LocalHash = "hash-retained"
                        LocalSize = 15.0
                        LocalMtimeMs = 3.0
                    }

                    let currentIndex = {
                        emptyIndex with
                            Entries = [| retainedEntry |]
                    }

                    let objects: LakeFsMaterialization.MaterializationObject[] =
                        [| {
                               Path = repositoryPath "selected.bin"
                               ObjectKey = "selected"
                               TargetPath = join [| root; "selected.bin" |]
                               BaseChecksum = "base-selected"
                               Mtime = 4.0
                           } |]

                    let download
                        (prepared: LakeFsMaterialization.MaterializationObject)
                        (temporaryPath: string)
                        (_: OperationContext)
                        : Async<Result<NodeBinaryIO.StreamCopyResult, OperationFailure>> =
                        async {
                            NodeFileSystem.writeUtf8FileExclusiveAndFlushSync
                                temporaryPath
                                $"body-{prepared.ObjectKey}"

                            return
                                Ok {
                                    BytesCopied = 1.0
                                    Sha256 = $"hash-{prepared.ObjectKey}"
                                }
                        }

                    let barrier (_: string) (_: OperationContext) = async { return () }

                    let! preparedResult =
                        LakeFsMaterialization.prepareSelected
                            transactionsDirectory
                            currentIndex
                            false
                            objects
                            [||]
                            download
                            barrier
                            (OperationContext.detached "lakefs-materialization-retained-index-entry")
                        |> Async.StartAsPromise

                    match preparedResult with
                    | Error failure -> failwith $"Materialization preparation failed: {failure.Code}"
                    | Ok plan ->
                        let selectedEntry: LakeFsIndex.IndexEntry = {
                            Path = "selected.bin"
                            BaseChecksum = "base-selected"
                            LocalHash = "hash-selected"
                            LocalSize = 1.0
                            LocalMtimeMs = 4.0
                        }

                        Vitest.expect(plan.NextIndex.Entries).toEqual [| retainedEntry; selectedEntry |]

                        let! cleaned = LakeFsMaterialization.cleanup plan |> Async.StartAsPromise

                        match cleaned with
                        | Ok() -> ()
                        | Error failure -> failwith $"Materialization cleanup failed: {failure.Code}"

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS materialization step events",
    fun () ->
        // Runs one preparation with a download that only records itself. The progress events and the downloads
        // share one list, so the order of the announcement and the downloads is visible.
        let prepareAndRecord prepare (objectNames: string[]) (removalNames: string[]) = promise {
            let! root = createTempDirectoryAsync ()
            let transactionsDirectory = join [| root; "transactions" |]
            let! _ = fsPromisesDynamic?mkdir transactionsDirectory |> unbox<JS.Promise<obj>>

            try
                let reports = ResizeArray<OperationProgress>()
                let order = ResizeArray<string>()

                let objects: LakeFsMaterialization.MaterializationObject[] =
                    objectNames
                    |> Array.map (fun name -> {
                        Path = repositoryPath name
                        ObjectKey = name
                        TargetPath = join [| root; name |]
                        BaseChecksum = $"base-{name}"
                        Mtime = 1.0
                    })

                let removals =
                    removalNames
                    |> Array.map (fun name -> repositoryPath name, join [| root; name |])

                let download
                    (prepared: LakeFsMaterialization.MaterializationObject)
                    (temporaryPath: string)
                    (_: OperationContext)
                    : Async<Result<NodeBinaryIO.StreamCopyResult, OperationFailure>> =
                    async {
                        order.Add $"download {prepared.ObjectKey}"
                        NodeFileSystem.writeUtf8FileExclusiveAndFlushSync temporaryPath $"body-{prepared.ObjectKey}"

                        return
                            Ok {
                                BytesCopied = 1.0
                                Sha256 = $"hash-{prepared.ObjectKey}"
                            }
                    }

                let barrier (_: string) (_: OperationContext) = async { return () }

                let context =
                    OperationContext.create
                        "lakefs-materialization-step-events"
                        OperationCancellation.none
                        (fun progress ->
                            reports.Add progress
                            order.Add "progress"
                        )

                let! preparedResult =
                    prepare transactionsDirectory emptyIndex false objects removals download barrier context
                    |> Async.StartAsPromise

                match preparedResult with
                | Error(failure: OperationFailure) -> failwith $"Materialization preparation failed: {failure.Code}"
                | Ok plan ->
                    let! cleaned = LakeFsMaterialization.cleanup plan |> Async.StartAsPromise

                    match cleaned with
                    | Ok() -> ()
                    | Error failure -> failwith $"Materialization cleanup failed: {failure.Code}"

                do! removeDirectoryAsync root
                return reports.ToArray(), order.ToArray()
            with error ->
                do! removeDirectoryAsync root
                return raise error
        }

        for variant, prepare in
            [
                "prepare", LakeFsMaterialization.prepare
                "prepareSelected", LakeFsMaterialization.prepareSelected
            ] do
            Vitest.test (
                $"{variant} announces the downloads of a batch once before the first object",
                TestOptions(timeout = 120000),
                fun () -> promise {
                    let! reports, order = prepareAndRecord prepare [| "first.bin"; "second.bin" |] [||]

                    Vitest.expect(order).toEqual [|
                        "progress"
                        "download first.bin"
                        "download second.bin"
                    |]
                    Vitest.expect(reports.Length).toBe 1
                    Vitest.expect(reports[0].PhaseCode).toBe ProgressPhaseCodes.DownloadObjects
                    Vitest.expect(reports[0].Item).toEqual None
                    Vitest.expect(reports[0].Completed).toEqual None
                    Vitest.expect(reports[0].Total).toEqual None
                    Vitest.expect(reports[0].DisplayMessage).toEqual (Some "Downloading objects")
                }
            )

            Vitest.test (
                $"{variant} announces no download for a batch that downloads nothing",
                TestOptions(timeout = 120000),
                fun () -> promise {
                    let! emptyReports, emptyOrder = prepareAndRecord prepare [||] [||]
                    let! removalReports, removalOrder = prepareAndRecord prepare [||] [| "gone.bin" |]

                    Vitest.expect(emptyReports.Length).toBe 0
                    Vitest.expect(emptyOrder).toEqual [||]
                    Vitest.expect(removalReports.Length).toBe 0
                    Vitest.expect(removalOrder).toEqual [||]
                }
            )
)
