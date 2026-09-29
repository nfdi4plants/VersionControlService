namespace VersionControlService.TextDiff

open System.Collections.Generic

/// A session that can give back the scratch memory it holds between requests.
type IScratchHolder =
    /// True while the session holds memory that a spill would release.
    abstract HoldsScratch: bool
    /// True while a request runs on the session. A busy session is not spilled.
    abstract IsBusy: bool
    /// Writes the suspended state to the session's temp store and releases the scratch memory.
    abstract Spill: unit -> Async<unit>

/// Coordinates the sessions of one worker. Scratch memory is limited per worker, so a session that starts a
/// request first asks every other idle session to spill what it holds.
type WorkerScratch() =
    let holders = List<IScratchHolder>()

    member _.Register(holder: IScratchHolder) =
        if not (holders.Contains holder) then holders.Add holder

    member _.Unregister(holder: IScratchHolder) = holders.Remove holder |> ignore

    member _.Count = holders.Count

    /// Spills every registered session except the requester that holds scratch memory and is idle.
    member _.BeginRequest(requester: IScratchHolder) = async {
        for other in holders.ToArray() do
            if not (obj.ReferenceEquals(other, requester)) && other.HoldsScratch && not other.IsBusy then
                do! other.Spill()
    }
