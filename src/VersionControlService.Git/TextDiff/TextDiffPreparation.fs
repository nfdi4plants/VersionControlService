/// Preparation tokens let a later Open continue with the sources an earlier Open resolved. A token is bound
/// to those sources and to the window that received it.
module VersionControlService.Git.TextDiff.TextDiffPreparation

open System
open System.Collections.Generic
open VersionControlService.Abstractions
open VersionControlService.Git.TextDiff.TextDiffSourceResolver

module NodeInterop = VersionControlService.Runtime.Node.Interop

/// What identifies the content of one side. Git content has its object id, and files have their lstat values.
[<RequireQualifiedAccess>]
type SideIdentity =
    | NoSource
    | ObjectId of string
    | File of FileIdentity
    | LfsFile of objectIdentity: FileIdentity * pointerFileIdentity: FileIdentity option

type PreparationBinding = {
    Path: string
    PreviousPath: string option
    CommitId: string option
    Previous: SideIdentity
    Current: SideIdentity
}

[<Literal>]
let TokenLifetimeMilliseconds = 300000.0

let private sideIdentity (side: ResolvedSide) =
    match side with
    | ResolvedSide.Absent -> SideIdentity.NoSource
    | ResolvedSide.GitBlob(oid, _) -> SideIdentity.ObjectId oid
    | ResolvedSide.WorkingFile(_, identity) -> SideIdentity.File identity
    | ResolvedSide.LfsObject(_, _, objectIdentity, pointerIdentity) -> SideIdentity.LfsFile(objectIdentity, pointerIdentity)

let bindingOf (path: string) (previousPath: string option) (sources: ResolvedSources) : PreparationBinding = {
    Path = path
    PreviousPath = previousPath
    CommitId = sources.CommitId
    Previous = sideIdentity sources.Previous
    Current = sideIdentity sources.Current
}

let private mismatch () =
    OperationFailure.create
        Validation
        TextDiffFailureCodes.PreparationMismatch
        "The preparation token does not match the requested sources."

/// The encoding an earlier Open settled for one side, so a later Open with the token does not classify it again.
type KeptClassification = {
    Side: DiffSide
    Encoding: string
    BomLength: int
    WasChosen: bool
}

type private TokenEntry = {
    Binding: PreparationBinding
    WindowOwner: string
    IssuedAt: float
    Kept: KeptClassification[]
}

/// Issued tokens of one worker. The clock returns milliseconds.
type PreparationTokenStore(now: unit -> float) =
    let tokens = Dictionary<string, TokenEntry>()

    let pruneExpired () =
        let current = now ()

        for expired in
            tokens
            |> Seq.filter (fun entry -> current - entry.Value.IssuedAt >= TokenLifetimeMilliseconds)
            |> Seq.map _.Key
            |> Seq.toArray do
            tokens.Remove expired |> ignore

    new() = PreparationTokenStore(fun () -> float DateTime.UtcNow.Ticks / 10000.0)

    member _.Issue(binding: PreparationBinding, windowOwner: string) : PreparationToken =
        pruneExpired ()
        let id = NodeInterop.randomUuid().Replace("-", "").ToLowerInvariant()
        tokens[id] <- { Binding = binding; WindowOwner = windowOwner; IssuedAt = now (); Kept = [||] }
        { Id = id }

    /// Fails with preparation_mismatch when the token is unknown, expired, bound to other sources or owned by another window.
    member _.Validate(token: PreparationToken, binding: PreparationBinding, windowOwner: string) : Result<unit, OperationFailure> =
        pruneExpired ()

        match tokens.TryGetValue token.Id with
        | true, entry when entry.WindowOwner = windowOwner && entry.Binding = binding -> Ok()
        | _ -> Error(mismatch ())

    /// Stores the classified sides of an Open that ended with EncodingRequired.
    member _.Keep(token: PreparationToken, kept: KeptClassification[]) =
        match tokens.TryGetValue token.Id with
        | true, entry -> tokens[token.Id] <- { entry with Kept = kept }
        | _ -> ()

    /// The classified sides stored for a token, or none when the token is unknown.
    member _.Kept(token: PreparationToken) : KeptClassification[] =
        match tokens.TryGetValue token.Id with
        | true, entry -> entry.Kept
        | _ -> [||]

    member _.Release(token: PreparationToken) = tokens.Remove token.Id |> ignore

    /// Drops the tokens a window holds for a path, for an Open of that path that carries no token.
    member _.ReleaseForPath(windowOwner: string, path: string) =
        for id in
            tokens
            |> Seq.filter (fun entry -> entry.Value.WindowOwner = windowOwner && entry.Value.Binding.Path = path)
            |> Seq.map _.Key
            |> Seq.toArray do
            tokens.Remove id |> ignore
