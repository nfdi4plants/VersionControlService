namespace VersionControlService.TextDiff.Tests

open System.Collections.Generic
open VersionControlService.TextDiff

module WorkerScratchCases =
    type private ScratchSession(coordinator: WorkerScratch, requestsPerStep: int, stepCount: int) as this =
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

        member _.Register(clock: IClock) =
            coordinator.Register(this :> IScratchHolder, clock)

        member _.Request() = async {
            let! admitted = coordinator.BeginRequest(this :> IScratchHolder)
            if admitted && step < stepCount then
                holdsScratch <- true
                requestInStep <- requestInStep + 1
                mustKeepScratch <- requestInStep > this.Warmup
                if requestInStep = requestsPerStep then
                    output.Add step
                    step <- step + 1
                    requestInStep <- 0
                    mustKeepScratch <- false
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
        let session = ScratchSession(coordinator, requestsPerStep, stepCount)
        session.Register(clock)
        session

    let private starvationCase () = async {
        let clock = ManualClock 0.0
        let coordinator = WorkerScratch()
        let holder = createSession coordinator (clock :> IClock) 4 1
        let requester = createSession coordinator (clock :> IClock) 1 1
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
        let first = createSession coordinator (clock :> IClock) (burst * 3 + 1) 1
        let second = createSession coordinator (clock :> IClock) (burst * 3 + 1) 1
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
        let sessions = Array.init 4 (fun _ -> createSession coordinator (clock :> IClock) 8 3)
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
        let holder = createSession coordinator (clock :> IClock) 100 1
        let requester = createSession coordinator (clock :> IClock) 1 1
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

    let cases: (string * (unit -> Async<unit>)) list = [
        "an idle holder yields two seconds after its last request", starvationCase
        "bursts of three and four requests finish without reciprocal abandonment", fun () -> async {
            do! burstCase 3
            do! burstCase 4
        }
        "four round-robin sessions finish with uninterrupted output", roundRobinCase
        "a holder that keeps requesting is never preempted", activeHolderCase
    ]
