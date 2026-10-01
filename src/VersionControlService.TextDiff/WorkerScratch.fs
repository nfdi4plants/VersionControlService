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

module private ScratchTiming =
    /// How long a session must be idle before another requester may ask it to give up unfinished alignment work.
    [<Literal>]
    let ScratchYieldIdleMs = 2_000.0

    /// How long a session admitted through a yield may be idle before it loses its protection. The limit is
    /// longer than ScratchYieldIdleMs so that a session that polls a few seconds apart is not displaced at once.
    [<Literal>]
    let ScratchProtectedIdleMs = 4_000.0

    /// The longest idle time that a protected session survives, however far apart its requests are.
    [<Literal>]
    let ScratchProtectedMaxIdleMs = 60_000.0

type private Registration(holder: IScratchHolder, clock: IClock) =
    member _.Holder = holder
    member _.Clock = clock
    /// The engine clock time when this session last began or ended a request.
    member val LastActivityMs = clock.NowMs() with get, set
    /// True while this session, admitted through a yield, is shielded from every other requester.
    member val Protected = false with get, set
    /// True once this session reported an unfinished step after it gained protection.
    member val KeptSinceProtected = false with get, set
    /// The idle time that preceded the most recent request of this session.
    member val LastGapMs = 0.0 with get, set
    member this.IdleMs = clock.NowMs() - this.LastActivityMs

/// Coordinates scratch memory for the sessions of one worker. A session keeps unfinished work while it stays
/// active. Once it has been idle for ScratchYieldIdleMs, measured from the end of its last request, another
/// requester may ask it to yield. The requester that gains scratch through a yield is protected from every other
/// requester. The protection ends when the session finishes the step it was in the middle of. A ready page ends
/// it only if no step is in progress. It also ends when the session has been idle for ScratchProtectedIdleMs, or
/// for twice its last gap between requests when that is longer (at most ScratchProtectedMaxIdleMs). An admitted
/// request spills other idle sessions whose scratch is safe to release.
type WorkerScratch() =
    let registrations = List<Registration>()

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

    let endProtection (registration: Registration) =
        registration.Protected <- false
        registration.KeptSinceProtected <- false

    /// Follows the step state of a protected session. Protection ends once the session that was in the middle
    /// of a step no longer is.
    let observeStep (registration: Registration) =
        if registration.Protected then
            if registration.Holder.MustKeepScratch then registration.KeptSinceProtected <- true
            elif registration.KeptSinceProtected then endProtection registration

    let isProtected (registration: Registration) =
        let limit = max ScratchTiming.ScratchProtectedIdleMs (min ScratchTiming.ScratchProtectedMaxIdleMs (2.0 * registration.LastGapMs))
        if registration.Protected && registration.IdleMs >= limit then endProtection registration
        registration.Protected

    member _.Register(holder: IScratchHolder, clock: IClock) =
        if (find holder).IsNone then registrations.Add(Registration(holder, clock))

    member _.Unregister(holder: IScratchHolder) =
        match find holder with
        | Some registration -> registrations.Remove registration |> ignore
        | None -> ()

    member _.Count = registrations.Count

    /// Records that a request of the session returned. The idle time of the session counts from this moment.
    /// A ready page ends the protection of a session that gained scratch through a yield.
    member _.EndRequest(holder: IScratchHolder, pageReady: bool) =
        match find holder with
        | Some registration ->
            registration.LastActivityMs <- registration.Clock.NowMs()
            if pageReady && not registration.Holder.MustKeepScratch then endProtection registration else observeStep registration
        | None -> ()

    /// Decides whether the requester may use the worker's scratch memory now. Returns false when the requester
    /// has to wait because another session is in the middle of a step. An idle holder yields unfinished work
    /// after ScratchYieldIdleMs, unless it is protected. A requester admitted after a yield is protected until
    /// it is between steps again or its idle time passes its protection limit.
    /// An admitted requester first spills other idle sessions whose scratch is safe to release.
    member _.BeginRequest(requester: IScratchHolder) = async {
        let registration =
            match find requester with
            | Some existing -> existing
            | None -> invalidOp "The scratch holder must be registered before it begins a request."
        registration.LastGapMs <- registration.IdleMs
        registration.LastActivityMs <- registration.Clock.NowMs()
        for current in registrations do observeStep current
        let mutable displaced = false
        if not requester.MustKeepScratch then
            for other in registrations.ToArray() do
                if
                    not (obj.ReferenceEquals(other.Holder, requester))
                    && not other.Holder.IsBusy
                    && other.Holder.MustKeepScratch
                    && other.IdleMs >= ScratchTiming.ScratchYieldIdleMs
                    && not (isProtected other)
                then
                    do! other.Holder.YieldScratch()
                    if not other.Holder.MustKeepScratch then
                        endProtection other
                        displaced <- true
        let mustWait =
            not requester.MustKeepScratch
            && othersKeepScratch requester
        if mustWait then return false
        else
            if displaced then
                registration.Protected <- true
                registration.KeptSinceProtected <- false
            for other in registrations.ToArray() do
                let holder = other.Holder
                if not (obj.ReferenceEquals(holder, requester)) && holder.HoldsScratch && not holder.IsBusy && not holder.MustKeepScratch then
                    do! holder.Spill()
            return true
    }
