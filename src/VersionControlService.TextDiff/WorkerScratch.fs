namespace VersionControlService.TextDiff

open System.Collections.Generic

/// A session that can give back the scratch memory it holds between requests.
type IScratchHolder =
    /// True while the session holds memory that a spill would release.
    abstract HoldsScratch: bool
    /// True while a request runs on the session. A busy session is not spilled.
    abstract IsBusy: bool
    /// True while the session is in the middle of an alignment step or a restore.
    abstract MustKeepScratch: bool
    /// Gives up idle alignment work or a partial restore so another session can use the worker scratch.
    abstract YieldScratch: unit -> Async<unit>
    /// Writes the suspended state to the session's temp store and releases the scratch memory.
    abstract Spill: unit -> Async<unit>

type private Registration(holder: IScratchHolder) =
    member _.Holder = holder
    /// The position of the session's last served request. Zero means it was never served.
    member val LastServed = 0L with get, set
    member val Waiting = false with get, set
    /// The position of the session's latest request among all requests of the worker.
    member val LastRequest = 0L with get, set
    /// The number of requests in a row that were refused. An admitted request resets it.
    member val Refusals = 0 with get, set
    /// The position of the first refused request of the current run of refusals.
    member val RefusedSince = 0L with get, set

/// Coordinates the sessions of one worker. Scratch memory is limited per worker. A session that is in the
/// middle of an alignment step or a restore keeps its scratch while it keeps requesting. When it asks for nothing
/// while another session is refused several times in a row, it yields. A request first asks every other idle
/// session to spill what it holds. Waiting sessions are served in order of their last served request.
type WorkerScratch() =
    /// The number of refused requests in a row after which an idle mid-step session gives up its unfinished work.
    let refusalsBeforeYield = 3
    let registrations = List<Registration>()
    let mutable served = 0L
    let mutable requestTick = 0L

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

    /// Decides whether the requester may use the worker's scratch memory now. A holder that is idle in the
    /// middle of a step yields once the requester has been refused several times in a row and the holder has
    /// asked for nothing since the first of those refusals. A requester that may proceed first spills every other
    /// idle session that holds scratch memory and is not in the middle of a step. It returns false when the
    /// requester has to wait.
    member _.BeginRequest(requester: IScratchHolder) = async {
        let registration =
            match find requester with
            | Some existing -> existing
            | None ->
                let created = Registration requester
                registrations.Add created
                created
        requestTick <- requestTick + 1L
        registration.LastRequest <- requestTick
        registration.Waiting <- true
        let refusedInARow = registration.Refusals + 1
        let refusedSince = if registration.Refusals = 0 then requestTick else registration.RefusedSince
        if not requester.MustKeepScratch && refusedInARow >= refusalsBeforeYield then
            for other in registrations.ToArray() do
                if not (obj.ReferenceEquals(other.Holder, requester))
                   && not other.Holder.IsBusy
                   && other.Holder.MustKeepScratch
                   && other.LastRequest < refusedSince then
                    do! other.Holder.YieldScratch()
        let mustWait =
            not requester.MustKeepScratch
            && (othersKeepScratch requester
                || (match firstWaiting () with
                    | Some first -> not (obj.ReferenceEquals(first, registration))
                    | None -> false))
        registration.Waiting <- false
        if mustWait then
            if registration.Refusals = 0 then registration.RefusedSince <- refusedSince
            registration.Refusals <- refusedInARow
            return false
        else
            registration.Refusals <- 0
            served <- served + 1L
            registration.LastServed <- served
            for other in registrations.ToArray() do
                let holder = other.Holder
                if not (obj.ReferenceEquals(holder, requester)) && holder.HoldsScratch && not holder.IsBusy && not holder.MustKeepScratch then
                    do! holder.Spill()
            return true
    }
