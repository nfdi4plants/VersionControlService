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
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
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
)
