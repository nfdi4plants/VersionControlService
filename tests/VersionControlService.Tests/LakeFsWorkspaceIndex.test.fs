module VersionControlService.Tests.LakeFsWorkspaceIndexTests

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Tests.NodePath
open Vitest

module LakeFsWorkspaceIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex

let private fsPromisesDynamic: obj = importAll "fs/promises"
let private osDynamic: obj = importAll "os"

let private createTempDirectoryAsync () : JS.Promise<string> =
    let prefix = join [| osDynamic?tmpdir () |> unbox<string>; "vcs-lakefs-index-" |]
    fsPromisesDynamic?mkdtemp (prefix) |> unbox<JS.Promise<string>>

let private removeDirectoryAsync (path: string) : JS.Promise<unit> = promise {
    let! _ =
        fsPromisesDynamic?rm (path, createObj [ "recursive" ==> true; "force" ==> true ])
        |> unbox<JS.Promise<obj>>

    return ()
}

let private writeUtf8FileAsync (path: string) (content: string) : JS.Promise<unit> = promise {
    let! _ = fsPromisesDynamic?writeFile (path, content, "utf8") |> unbox<JS.Promise<obj>>
    return ()
}

let private sampleIndex: LakeFsWorkspaceIndex.WorkspaceIndex = {
    SchemaVersion = LakeFsWorkspaceIndex.CurrentSchemaVersion
    Repository = "repo"
    TargetRef = "main"
    Prefix = "data/vault-a"
    WorkspaceBranch = "vcs-workspace-abc123"
    OwnershipToken = "token-abc"
    BaseRevision = Some "rev-base"
    WorkspaceRevision = Some "rev-work"
    Generation = 0
    Entries = [|
        {
            Path = "data/vault-a/file.txt"
            BaseChecksum = "chk-1"
            LocalHash = LakeFsWorkspaceIndex.hashContent "original content\n"
            LocalSize = 17.0
            LocalMtimeMs = 1000.0
        }
    |]
}

Vitest.describe (
    "lakeFS index core contract",
    fun () ->
        Vitest.test (
            "lakeFS workspace index is atomic migratable and identity-correct",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()

                try
                    // Round trip with generation bump.
                    let saved =
                        match LakeFsWorkspaceIndex.save root sampleIndex with
                        | Ok index -> index
                        | Error message -> failwith message

                    Vitest.expect(saved.Generation).toBe (1)

                    match LakeFsWorkspaceIndex.load root with
                    | LakeFsWorkspaceIndex.Loaded loaded ->
                        Vitest.expect(loaded.WorkspaceBranch).toBe ("vcs-workspace-abc123")
                        Vitest.expect(loaded.Generation).toBe (1)
                        Vitest.expect(loaded.Entries.Length).toBe (1)
                    | _ -> failwith "Expected the saved index to load."

                    // An interrupted write (stray temporary file) never corrupts the
                    // previous index: saves go through write-then-rename.
                    do! writeUtf8FileAsync (LakeFsWorkspaceIndex.indexPath root + ".tmp") "garbage {"

                    match LakeFsWorkspaceIndex.load root with
                    | LakeFsWorkspaceIndex.Loaded loaded -> Vitest.expect(loaded.Generation).toBe (1)
                    | _ -> failwith "Expected the index to survive an interrupted write."

                    // Corruption is structural, not an exception.
                    do! writeUtf8FileAsync (LakeFsWorkspaceIndex.indexPath root) "not json at all {"

                    match LakeFsWorkspaceIndex.load root with
                    | LakeFsWorkspaceIndex.Corrupt message -> Vitest.expect(message.Length > 0).toBe (true)
                    | _ -> failwith "Expected corruption to be reported."

                    // Unknown schema versions are rejected for migration handling.
                    do!
                        writeUtf8FileAsync
                            (LakeFsWorkspaceIndex.indexPath root)
                            """{"SchemaVersion":99,"Repository":"repo","WorkspaceBranch":"b"}"""

                    match LakeFsWorkspaceIndex.load root with
                    | LakeFsWorkspaceIndex.Corrupt message -> Vitest.expect(message.Contains "99").toBe (true)
                    | _ -> failwith "Expected the unknown schema version to be rejected."

                    // Identity classification: content is the authority.
                    let entry = Some sampleIndex.Entries[0]

                    // Unchanged content — even with a different mtime (touch-only).
                    let touchOnly =
                        LakeFsWorkspaceIndex.classifyLocalObject entry (Some "original content\n")

                    let touchOnlyUnchanged = touchOnly = LakeFsWorkspaceIndex.UnchangedObject
                    Vitest.expect(touchOnlyUnchanged).toBe (true)

                    // Content change at identical size.
                    let sameSizeEdit =
                        LakeFsWorkspaceIndex.classifyLocalObject entry (Some "0riginal content\n")

                    let sameSizeModified = sameSizeEdit = LakeFsWorkspaceIndex.ModifiedObject
                    Vitest.expect(sameSizeModified).toBe (true)

                    // Deleted and added.
                    let deleted = LakeFsWorkspaceIndex.classifyLocalObject entry None
                    let deletedClassified = deleted = LakeFsWorkspaceIndex.DeletedObject
                    Vitest.expect(deletedClassified).toBe (true)

                    let added = LakeFsWorkspaceIndex.classifyLocalObject None (Some "new content\n")
                    let addedClassified = added = LakeFsWorkspaceIndex.AddedObject
                    Vitest.expect(addedClassified).toBe (true)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
