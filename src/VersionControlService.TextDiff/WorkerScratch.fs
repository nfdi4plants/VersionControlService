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
    [<Literal>]
    let ScratchYieldIdleMs = 2_000.0

type private Registration(holder: IScratchHolder, clock: IClock) =
    member _.Holder = holder
    member _.Clock = clock
    /// The engine clock time when this session last asked to use scratch memory.
    member val LastRequestMs = clock.NowMs() with get, set
    /// The session that yielded so this one could finish its current scratch-backed step.
    member val ProtectedFrom: Registration option = None with get, set
    /// True once this session reported an unfinished step after it gained protection.
    member val KeptSinceProtected = false with get, set

/// Coordinates scratch memory for the sessions of one worker. A session keeps unfinished work while it requests
/// within the idle interval. After that interval, another requester may ask it to yield. The requester that gains
/// scratch through a yield keeps it until it completes one step, so the displaced session cannot yield it back while
/// the new holder is still warming up. An admitted request spills other idle sessions whose scratch is safe to release.
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

    member _.Register(holder: IScratchHolder, clock: IClock) =
        if (find holder).IsNone then registrations.Add(Registration(holder, clock))

    member _.Unregister(holder: IScratchHolder) =
        match find holder with
        | Some registration ->
            registrations.Remove registration |> ignore
            for other in registrations do
                match other.ProtectedFrom with
                | Some displaced when obj.ReferenceEquals(displaced, registration) ->
                    other.ProtectedFrom <- None
                    other.KeptSinceProtected <- false
                | _ -> ()
        | None -> ()

    member _.Count = registrations.Count

    /// Decides whether the requester may use the worker's scratch memory now. An idle holder yields unfinished
    /// work after the yield interval. A holder that asks within the interval keeps its work. A requester admitted
    /// after a yield may finish its current step before the displaced holder can take the scratch back. An admitted
    /// requester first spills other idle sessions whose scratch is safe to release.
    member _.BeginRequest(requester: IScratchHolder) = async {
        let registration =
            match find requester with
            | Some existing -> existing
            | None -> invalidOp "The scratch holder must be registered before it begins a request."
        registration.LastRequestMs <- registration.Clock.NowMs()
        for current in registrations do
            if current.ProtectedFrom.IsSome then
                if current.Holder.MustKeepScratch then current.KeptSinceProtected <- true
                elif current.KeptSinceProtected then
                    current.ProtectedFrom <- None
                    current.KeptSinceProtected <- false
        let mutable displaced: Registration option = None
        if not requester.MustKeepScratch then
            for other in registrations.ToArray() do
                let protectedFromRequester =
                    match other.ProtectedFrom with
                    | Some protectedFrom -> obj.ReferenceEquals(protectedFrom.Holder, requester)
                    | None -> false
                if
                    not (obj.ReferenceEquals(other.Holder, requester))
                    && not other.Holder.IsBusy
                    && other.Holder.MustKeepScratch
                    && not protectedFromRequester
                    && other.Clock.NowMs() - other.LastRequestMs >= ScratchTiming.ScratchYieldIdleMs
                then
                    do! other.Holder.YieldScratch()
                    if not other.Holder.MustKeepScratch then
                        other.ProtectedFrom <- None
                        other.KeptSinceProtected <- false
                        displaced <- Some other
        let mustWait =
            not requester.MustKeepScratch
            && othersKeepScratch requester
        if mustWait then return false
        else
            match displaced with
            | Some yielded ->
                registration.ProtectedFrom <- Some yielded
                registration.KeptSinceProtected <- false
            | None -> ()
            for other in registrations.ToArray() do
                let holder = other.Holder
                if not (obj.ReferenceEquals(holder, requester)) && holder.HoldsScratch && not holder.IsBusy && not holder.MustKeepScratch then
                    do! holder.Spill()
            return true
    }
