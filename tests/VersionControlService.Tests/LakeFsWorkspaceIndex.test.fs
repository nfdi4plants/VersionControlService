module VersionControlService.Tests.LakeFsWorkspaceIndexTests

open Fable.Core
open Fable.Core.JsInterop
open VersionControlService.Abstractions
open VersionControlService.Tests.NodePath
open Vitest

module LakeFsWorkspaceIndex = VersionControlService.LakeFs.LakeFsWorkspaceIndex
module LakeFsProviderOptions = VersionControlService.LakeFs.LakeFsProviderOptions
module LakeFsStateStore = VersionControlService.LakeFs.LakeFsStateStore
module RuntimeNodePath = VersionControlService.Runtime.Node.Path

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
            LocalHash = LakeFsWorkspaceIndex.hashMetadata "original content\n"
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
                        LakeFsWorkspaceIndex.classifyLocalObject
                            entry
                            (Some(LakeFsWorkspaceIndex.hashMetadata "original content\n"))

                    let touchOnlyUnchanged = touchOnly = LakeFsWorkspaceIndex.UnchangedObject
                    Vitest.expect(touchOnlyUnchanged).toBe (true)

                    // Content change at identical size.
                    let sameSizeEdit =
                        LakeFsWorkspaceIndex.classifyLocalObject
                            entry
                            (Some(LakeFsWorkspaceIndex.hashMetadata "0riginal content\n"))

                    let sameSizeModified = sameSizeEdit = LakeFsWorkspaceIndex.ModifiedObject
                    Vitest.expect(sameSizeModified).toBe (true)

                    // Deleted and added.
                    let deleted = LakeFsWorkspaceIndex.classifyLocalObject entry None
                    let deletedClassified = deleted = LakeFsWorkspaceIndex.DeletedObject
                    Vitest.expect(deletedClassified).toBe (true)

                    let added =
                        LakeFsWorkspaceIndex.classifyLocalObject
                            None
                            (Some(LakeFsWorkspaceIndex.hashMetadata "new content\n"))
                    let addedClassified = added = LakeFsWorkspaceIndex.AddedObject
                    Vitest.expect(addedClassified).toBe (true)

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)

Vitest.describe (
    "lakeFS external state",
    fun () ->
        Vitest.test (
            "allocates and resolves opaque provider state outside the workspace",
            TestOptions(timeout = 120000),
            fun () -> promise {
                let! root = createTempDirectoryAsync ()
                let workspaceRoot = join [| root; "workspace" |]
                let stateRoot = join [| root; "provider-state" |]
                let! _ = fsPromisesDynamic?mkdir (workspaceRoot) |> unbox<JS.Promise<obj>>

                try
                    let options: LakeFsProviderOptions.LakeFsProviderOptions = { StateRoot = stateRoot }

                    let allocated =
                        match LakeFsStateStore.create options workspaceRoot with
                        | Ok value -> value
                        | Error failure -> failwith $"State allocation failed: {failure.Code}"

                    Vitest.expect(RuntimeNodePath.isAbsolute allocated.StateId).toBe false
                    Vitest.expect(allocated.StateDirectory.StartsWith(stateRoot)).toBe true

                    for child in [| "transactions"; "recovery"; "temporary" |] do
                        let childPath = join [| allocated.StateDirectory; child |]
                        let! stats = fsPromisesDynamic?stat (childPath) |> unbox<JS.Promise<obj>>
                        Vitest.expect(stats?isDirectory () |> unbox<bool>).toBe true

                    let workspaceFiles = fsPromisesDynamic?readdir (workspaceRoot) |> unbox<JS.Promise<string[]>>
                    let! workspaceFiles = workspaceFiles
                    Vitest.expect(workspaceFiles).toEqual [||]

                    let saved =
                        match LakeFsWorkspaceIndex.save allocated.StateDirectory sampleIndex with
                        | Ok value -> value
                        | Error message -> failwith message

                    Vitest.expect(saved.Generation).toBe 1

                    let resolved =
                        match LakeFsStateStore.resolve options workspaceRoot (Some allocated.StateId) with
                        | Ok value -> value
                        | Error failure -> failwith $"State resolution failed: {failure.Code}"

                    Vitest.expect(resolved.StateDirectory).toBe allocated.StateDirectory

                    let otherWorkspaceRoot = join [| root; "another-workspace" |]
                    let! _ =
                        fsPromisesDynamic?mkdir (otherWorkspaceRoot)
                        |> unbox<JS.Promise<obj>>

                    match LakeFsStateStore.resolve options otherWorkspaceRoot (Some allocated.StateId) with
                    | Error failure -> Vitest.expect(failure.Code).toBe "provider_state_mismatch"
                    | Ok _ -> failwith "Expected provider state to remain owned by its allocating workspace."

                    match LakeFsWorkspaceIndex.load resolved.StateDirectory with
                    | LakeFsWorkspaceIndex.Loaded loaded -> Vitest.expect(loaded.Generation).toBe 1
                    | _ -> failwith "Expected the externally stored index to load."

                    match LakeFsStateStore.resolve options workspaceRoot None with
                    | Error failure -> Vitest.expect(failure.Code).toBe "provider_state_ref_missing"
                    | Ok _ -> failwith "Expected an absent state reference to be rejected."

                    match LakeFsStateStore.resolve options workspaceRoot (Some "../escaped") with
                    | Error failure -> Vitest.expect(failure.Code).toBe "provider_state_ref_invalid"
                    | Ok _ -> failwith "Expected an escaping state reference to be rejected."

                    do! removeDirectoryAsync allocated.StateDirectory

                    match LakeFsStateStore.resolve options workspaceRoot (Some allocated.StateId) with
                    | Error failure -> Vitest.expect(failure.Code).toBe "provider_state_missing"
                    | Ok _ -> failwith "Expected deleted external state to remain missing."

                    let unsafeOptions: LakeFsProviderOptions.LakeFsProviderOptions = {
                        StateRoot = join [| workspaceRoot; "provider-state" |]
                    }

                    match LakeFsStateStore.create unsafeOptions workspaceRoot with
                    | Error failure -> Vitest.expect(failure.Code).toBe "provider_state_inside_workspace"
                    | Ok _ -> failwith "Expected in-workspace provider state to be rejected."

                    let linkedStateTarget = join [| workspaceRoot; "linked-provider-state" |]
                    let linkedStateRoot = join [| root; "provider-state-link" |]
                    let! _ = fsPromisesDynamic?mkdir (linkedStateTarget) |> unbox<JS.Promise<obj>>
                    let! _ =
                        fsPromisesDynamic?symlink (linkedStateTarget, linkedStateRoot, "junction")
                        |> unbox<JS.Promise<obj>>

                    let linkedOptions: LakeFsProviderOptions.LakeFsProviderOptions = {
                        StateRoot = linkedStateRoot
                    }

                    match LakeFsStateStore.create linkedOptions workspaceRoot with
                    | Error failure -> Vitest.expect(failure.Code).toBe "provider_state_link_not_supported"
                    | Ok _ -> failwith "Expected a linked provider state root to be rejected."

                    let childLinkStateRoot = join [| root; "child-link-state" |]
                    let childLinkOptions: LakeFsProviderOptions.LakeFsProviderOptions = {
                        StateRoot = childLinkStateRoot
                    }

                    let childLinkState =
                        match LakeFsStateStore.create childLinkOptions workspaceRoot with
                        | Ok value -> value
                        | Error failure -> failwith $"Child-link state allocation failed: {failure.Code}"

                    do! removeDirectoryAsync childLinkState.TemporaryDirectory
                    let! _ =
                        fsPromisesDynamic?symlink
                            (workspaceRoot, childLinkState.TemporaryDirectory, "junction")
                        |> unbox<JS.Promise<obj>>

                    match
                        LakeFsStateStore.resolve
                            childLinkOptions
                            workspaceRoot
                            (Some childLinkState.StateId)
                    with
                    | Error failure -> Vitest.expect(failure.Code).toBe "provider_state_link_not_supported"
                    | Ok _ -> failwith "Expected a linked provider state child directory to be rejected."

                    do! removeDirectoryAsync root
                with error ->
                    do! removeDirectoryAsync root
                    return raise error
            }
        )
)
