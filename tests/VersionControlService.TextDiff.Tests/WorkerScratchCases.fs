namespace VersionControlService.TextDiff.Tests

open System.Collections.Generic
open VersionControlService.TextDiff

module WorkerScratchCases =
    type private ScratchSession(coordinator: WorkerScratch, clock: ManualClock, requestsPerStep: int, stepCount: int) as this =
        let output = ResizeArray<int>()
        let mutable holdsScratch = false
        let mutable mustKeepScratch = false
        let mutable requestInStep = 0
        let mutable step = 0
        let mutable yields = 0

        member _.Finished = step = stepCount
        member _.Output = output.ToArray()
        member _.Yields = yields
        /// The number of requests of a step that run before the session reports a step in progress.
        member val Warmup = 0 with get, set
        /// The engine time in milliseconds that one request takes.
        member val Duration = 0.0 with get, set
        /// True when every request that leaves the step in progress also returns a page.
        member val PagesMidStep = false with get, set

        member _.Register() =
            coordinator.Register(this :> IScratchHolder, clock :> IClock)

        member _.Request() = async {
            let! admitted = coordinator.BeginRequest(this :> IScratchHolder)
            let mutable pageReady = false
            if admitted && step < stepCount then
                holdsScratch <- true
                requestInStep <- requestInStep + 1
                mustKeepScratch <- requestInStep > this.Warmup
                if requestInStep = requestsPerStep then
                    output.Add step
                    step <- step + 1
                    requestInStep <- 0
                    mustKeepScratch <- false
                    pageReady <- true
                elif this.PagesMidStep then pageReady <- true
            clock.Advance this.Duration
            coordinator.EndRequest(this :> IScratchHolder, pageReady)
            return admitted
        }

        interface IScratchHolder with
            member _.HoldsScratch = holdsScratch
            member _.IsBusy = false
            member _.MustKeepScratch = mustKeepScratch

            member _.YieldScratch() = async {
                yields <- yields + 1
                holdsScratch <- false
                mustKeepScratch <- false
                requestInStep <- 0
            }

            member _.Spill() = async {
                holdsScratch <- false
            }

    let private createSession coordinator clock requestsPerStep stepCount =
        let session = ScratchSession(coordinator, clock, requestsPerStep, stepCount)
        session.Register()
        session

    let private starvationCase () = async {
        let clock = ManualClock 0.0
        let coordinator = WorkerScratch()
        let holder = createSession coordinator clock 4 1
        let requester = createSession coordinator clock 1 1
        let! holderStarted = holder.Request()
        Check.true' holderStarted "The first session starts its scratch-backed step."
        let! firstRefusal = requester.Request()
        Check.equal false firstRefusal "The other session waits while the holder is active."
        let! holderContinued = holder.Request()
        Check.true' holderContinued "The holder can request again after the first refusal."
        clock.Advance 1_999.0
        let! early = requester.Request()
        Check.equal false early "The holder keeps its partial step before the idle interval ends."
        clock.Advance 1.0
        let! admitted = requester.Request()
        Check.true' admitted "The waiting session is admitted when the holder has been idle for two seconds."
        Check.true' requester.Finished "The admitted session finishes its step."
        Check.equal 1 holder.Yields "The idle holder yields once."
    }

    let private burstCase burst = async {
        let clock = ManualClock 0.0
        let coordinator = WorkerScratch()
        let first = createSession coordinator clock (burst * 3 + 1) 1
        let second = createSession coordinator clock (burst * 3 + 1) 1
        second.Warmup <- 2
        let! started = first.Request()
        Check.true' started "The first viewer starts a partial step."
        first.Warmup <- 2
        let viewers = [| second; first |]
        let mutable rounds = 0
        while not first.Finished || not second.Finished do
            rounds <- rounds + 1
            if rounds > 32 then failwith $"Bursts of {burst} requests did not finish."
            for viewer in viewers do
                if not viewer.Finished then
                    clock.Advance 2_000.0
                    for _ in 1 .. burst do
                        if not viewer.Finished then
                            let! _ = viewer.Request()
                            ()
        Check.sequence [| 0 |] first.Output $"The first viewer keeps its output with bursts of {burst} requests."
        Check.sequence [| 0 |] second.Output $"The second viewer keeps its output with bursts of {burst} requests."
        Check.equal 1 first.Yields "The original holder yields once."
        Check.equal 0 second.Yields "The displaced holder cannot abandon the new holder before its step completes."
    }

    let private roundRobinCase () = async {
        let clock = ManualClock 0.0
        let coordinator = WorkerScratch()
        let sessions = Array.init 4 (fun _ -> createSession coordinator clock 8 3)
        let mutable requests = 0
        while sessions |> Array.exists (fun session -> not session.Finished) do
            for session in sessions do
                if not session.Finished then
                    requests <- requests + 1
                    if requests > 1_000 then failwith "Round-robin requests did not finish."
                    clock.Advance 400.0
                    let! _ = session.Request()
                    ()
        for session in sessions do
            Check.sequence [| 0; 1; 2 |] session.Output "Round-robin requests preserve uninterrupted output."
            Check.equal 0 session.Yields "A holder that requests every round keeps its current step."
    }

    let private activeHolderCase () = async {
        let clock = ManualClock 0.0
        let coordinator = WorkerScratch()
        let holder = createSession coordinator clock 100 1
        let requester = createSession coordinator clock 1 1
        let! started = holder.Request()
        Check.true' started "The holder starts its step."
        for _ in 1 .. 8 do
            clock.Advance 1_999.0
            let! continued = holder.Request()
            Check.true' continued "The active holder keeps the scratch."
            let! refused = requester.Request()
            Check.equal false refused "The requester waits after a recent holder request."
        Check.equal 0 holder.Yields "Repeated holder requests prevent an idle yield."
        Check.equal false requester.Finished "The waiting requester has not run."
    }

    let private frozenProtectedCase showsPage = async {
        let clock = ManualClock 0.0
        let coordinator = WorkerScratch()
        let holder = createSession coordinator clock 10 1
        let newcomer = createSession coordinator clock 10 1
        newcomer.PagesMidStep <- showsPage
        let! started = holder.Request()
        Check.true' started "The holder starts its step."
        clock.Advance 2_000.0
        let! admitted = newcomer.Request()
        Check.true' admitted "The newcomer is admitted after the holder went idle."
        Check.equal 1 holder.Yields "The idle holder yields once."
        let freezeMs = (clock :> IClock).NowMs()
        while not holder.Finished && (clock :> IClock).NowMs() - freezeMs < 10_000.0 do
            clock.Advance 100.0
            let! _ = holder.Request()
            ()
        Check.true' holder.Finished $"The displaced session finishes within ten seconds after the newcomer stops (page mid-step {showsPage})."
    }

    let private slowRoundRobinCase () = async {
        let clock = ManualClock 0.0
        let coordinator = WorkerScratch()
        let sessions = Array.init 3 (fun _ -> createSession coordinator clock 6 2)
        for session in sessions do session.Duration <- 2_100.0
        let mutable rounds = 0
        while sessions |> Array.exists (fun session -> not session.Finished) do
            rounds <- rounds + 1
            if rounds > 400 then failwith "Round-robin requests of 2.1 seconds did not finish."
            for session in sessions do
                if not session.Finished then
                    let! _ = session.Request()
                    ()
        for session in sessions do
            Check.sequence [| 0; 1 |] session.Output "Every session keeps its output when each request takes 2.1 seconds."
    }

    let cases: (string * (unit -> Async<unit>)) list = [
        "an idle holder yields two seconds after its last request", starvationCase
        "bursts of three and four requests finish without reciprocal abandonment", fun () -> async {
            do! burstCase 3
            do! burstCase 4
        }
        "four round-robin sessions finish with uninterrupted output", roundRobinCase
        "a holder that keeps requesting is never preempted", activeHolderCase
        "a newcomer that goes idle gives the displaced session the scratch again within a bounded time", fun () -> async {
            do! frozenProtectedCase false
            do! frozenProtectedCase true
        }
        "three sessions with 2.1 second requests all finish", slowRoundRobinCase
    ]
