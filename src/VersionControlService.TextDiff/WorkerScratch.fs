namespace VersionControlService.TextDiff

open System.Collections.Generic

/// A session that can give back the scratch memory it holds between requests.
type IScratchHolder =
    /// True while the session holds memory that a spill would release.
    abstract HoldsScratch: bool
    /// True while a request runs on the session. A busy session is not spilled.
    abstract IsBusy: bool
    /// True while the session is in the middle of an alignment step or a restore, and during the request that
    /// follows a restore. Such a session keeps its scratch memory until then, and other sessions wait for it.
    abstract MustKeepScratch: bool
    /// Writes the suspended state to the session's temp store and releases the scratch memory.
    abstract Spill: unit -> Async<unit>

type private Registration(holder: IScratchHolder) =
    member _.Holder = holder
    /// The position of the session's last served request. Zero means it was never served.
    member val LastServed = 0L with get, set
    member val Waiting = false with get, set

/// Coordinates the sessions of one worker. Scratch memory is limited per worker. A session that is in the
/// middle of an alignment step or a restore keeps the memory until that step completes, and the requests of
/// other sessions that arrive meanwhile wait. When no such session exists, a request first asks every other
/// idle session to spill what it holds. Waiting sessions are served in order of their last served request.
type WorkerScratch() =
    let registrations = List<Registration>()
    let mutable served = 0L

    let find (holder: IScratchHolder) =
        let mutable found: Registration option = None
        for registration in registrations do
            if found.IsNone && obj.ReferenceEquals(registration.Holder, holder) then found <- Some registration
        found

    let othersKeepScratch (requester: IScratchHolder) =
        let mutable keeps = false
        for registration in registrations do
            if not (obj.ReferenceEquals(registration.Holder, requester)) && registration.Holder.MustKeepScratch then keeps <- true
        keeps

    /// The waiting session that was served least recently. Sessions with the same position keep the order in
    /// which they registered.
    let firstWaiting () =
        let mutable first: Registration option = None
        for registration in registrations do
            if registration.Waiting then
                match first with
                | Some current when current.LastServed <= registration.LastServed -> ()
                | _ -> first <- Some registration
        first

    member _.Register(holder: IScratchHolder) =
        if (find holder).IsNone then registrations.Add(Registration holder)

    member _.Unregister(holder: IScratchHolder) =
        match find holder with
        | Some registration -> registrations.Remove registration |> ignore
        | None -> ()

    member _.Count = registrations.Count

    /// Decides whether the requester may use the worker's scratch memory now. A session that is in the middle
    /// of a step always may. Any other session waits while another session keeps the scratch memory, and
    /// while a session that waited longer has not been served. A requester that may proceed first spills every
    /// other idle session that holds scratch memory and is not in the middle of a step. It returns false when
    /// the requester has to wait.
    member _.BeginRequest(requester: IScratchHolder) = async {
        let registration =
            match find requester with
            | Some existing -> existing
            | None -> Registration requester
        let mustWait =
            not requester.MustKeepScratch
            && (othersKeepScratch requester
                || (match firstWaiting () with
                    | Some first -> not (obj.ReferenceEquals(first, registration))
                    | None -> false))
        if mustWait then
            registration.Waiting <- true
            return false
        else
            registration.Waiting <- false
            served <- served + 1L
            registration.LastServed <- served
            for other in registrations.ToArray() do
                let holder = other.Holder
                if not (obj.ReferenceEquals(holder, requester)) && holder.HoldsScratch && not holder.IsBusy && not holder.MustKeepScratch then
                    do! holder.Spill()
            return true
    }
