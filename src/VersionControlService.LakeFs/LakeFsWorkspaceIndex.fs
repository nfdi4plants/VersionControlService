/// Local lakeFS workspace index: binding schema, hidden workspace branch identity,
/// base/workspace revisions, and per-object identity for dirty detection.
/// Spike 7 decision: size+mtime is only a fast pre-filter — a SHA-256 content hash
/// confirms every change, so touch-only mtime updates and same-size content edits
/// classify correctly across storage adapters.
module VersionControlService.LakeFs.LakeFsWorkspaceIndex

open System
open Fable.Core
open Fable.Core.JsInterop

module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodePath = VersionControlService.Runtime.Node.Path

[<Literal>]
let CurrentSchemaVersion = 1

[<Literal>]
let IndexFileName = ".vcs-lakefs-index.json"

type IndexEntry = {
    Path: string
    /// Object checksum on the workspace branch this entry was materialized from.
    BaseChecksum: string
    /// Local content identity: SHA-256 over file bytes at last sync.
    LocalHash: string
    /// Fast pre-filter data; never trusted on its own.
    LocalSize: float
    LocalMtimeMs: float
}

type WorkspaceIndex = {
    SchemaVersion: int
    Repository: string
    TargetRef: string
    Prefix: string
    /// The provider-owned, server-visible workspace branch.
    WorkspaceBranch: string
    /// Random token proving this workspace created the branch; cleanup requires it.
    OwnershipToken: string
    /// Last synchronized target revision.
    BaseRevision: string option
    /// Head of the workspace branch.
    WorkspaceRevision: string option
    /// Bumped on every index write; part of the workspace version token.
    Generation: int
    Entries: IndexEntry[]
}

[<Emit("JSON.stringify($0, null, 2)")>]
let private jsonStringify (_value: obj) : string = jsNative

[<Emit("JSON.parse($0)")>]
let private jsonParse (_text: string) : obj = jsNative

[<Emit("require('crypto').createHash('sha256').update($0).digest('hex')")>]
let private sha256Hex (_content: string) : string = jsNative

[<Emit("require('crypto').randomBytes(16).toString('hex')")>]
let private randomToken () : string = jsNative

let createOwnershipToken () = randomToken ()

/// Hashes local file content for identity confirmation.
let hashContent (content: string) : string = sha256Hex content

let indexPath (workspaceRoot: string) =
    NodePath.join [| workspaceRoot; IndexFileName |]

/// Atomic save: write to a temporary sibling, then rename over the index file.
/// An interrupted write never corrupts the previous index.
let save (workspaceRoot: string) (index: WorkspaceIndex) : Result<WorkspaceIndex, string> =
    try
        let next = { index with Generation = index.Generation + 1 }
        let target = indexPath workspaceRoot
        let temporary = target + ".tmp"
        NodeFileSystem.writeFileSync temporary (jsonStringify next) NodeFileSystem.TextEncoding.Utf8
        NodeFileSystem.renameSync temporary target
        Ok next
    with error ->
        Error $"Saving the lakeFS workspace index failed: {error.Message}"

type LoadResult =
    | Loaded of WorkspaceIndex
    | Missing
    | Corrupt of message: string

/// Loads and validates the index. Unknown schema versions are migratable by
/// construction: version 1 is current; anything newer is rejected structurally.
let load (workspaceRoot: string) : LoadResult =
    let target = indexPath workspaceRoot

    if not (NodeFileSystem.existsSync target) then
        Missing
    else
        try
            let raw = NodeFileSystem.readFileSync target NodeFileSystem.TextEncoding.Utf8
            let parsed = jsonParse raw

            let schemaVersion =
                parsed?SchemaVersion
                |> Option.ofObj
                |> Option.map unbox<int>
                |> Option.defaultValue 0

            if schemaVersion <> CurrentSchemaVersion then
                Corrupt $"Unsupported lakeFS index schema version {schemaVersion}."
            elif isNull parsed?WorkspaceBranch || isNull parsed?Repository then
                Corrupt "The lakeFS index is missing required fields."
            else
                Loaded(unbox<WorkspaceIndex> parsed)
        with error ->
            Corrupt $"The lakeFS workspace index could not be parsed: {error.Message}"

/// Local file state relative to the index entry.
type LocalObjectState =
    | UnchangedObject
    | ModifiedObject
    | AddedObject
    | DeletedObject

/// Classifies one local file against its index entry. Size+mtime short-circuits
/// only the UNCHANGED verdict when both match AND the hash confirms; content is
/// always the authority.
let classifyLocalObject
    (entry: IndexEntry option)
    (localContent: string option)
    : LocalObjectState =
    match entry, localContent with
    | None, Some _ -> AddedObject
    | None, None -> UnchangedObject
    | Some _, None -> DeletedObject
    | Some indexEntry, Some content ->
        if hashContent content = indexEntry.LocalHash then
            UnchangedObject
        else
            ModifiedObject
