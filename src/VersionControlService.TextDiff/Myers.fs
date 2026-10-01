namespace VersionControlService.TextDiff

open System.Collections.Generic

[<RequireQualifiedAccess>]
type MyersOperation =
    | Equal of previousIndex: int * currentIndex: int
    | Delete of previousIndex: int
    | Insert of currentIndex: int

[<RequireQualifiedAccess>]
type MyersStepResult =
    | Running
    | NeedComparison of previousIndex: int * currentIndex: int
    | Complete of MyersOperation[]
    | StepLimitExceeded

[<RequireQualifiedAccess>]
type private ComparisonStep =
    | Compared of bool
    | Waiting of previousIndex: int * currentIndex: int
    | LimitExceeded

type private MyersTask =
    | VisitRange of previousStart: int * previousEnd: int * currentStart: int * currentEnd: int
    | WriteEqual of previousStart: int * currentStart: int * count: int
    | WriteDeletes of previousStart: int * count: int
    | WriteInserts of currentStart: int * count: int

[<RequireQualifiedAccess>]
type internal RangeStage =
    | Prefix
    | Suffix
    | Prepare
    | SingleSearch
    | ForwardFrontier
    | ForwardSnake
    | ReverseFrontier
    | ReverseSnake
    | Fallback

type internal MyersRange(previousStart: int, previousEnd: int, currentStart: int, currentEnd: int) =
    member val PreviousStart = previousStart with get, set
    member val PreviousEnd = previousEnd with get
    member val CurrentStart = currentStart with get, set
    member val CurrentEnd = currentEnd with get
    member val CorePreviousEnd = previousEnd with get, set
    member val CoreCurrentEnd = currentEnd with get, set
    member val SuffixCount = 0 with get, set
    member val Stage = RangeStage.Prefix with get, set
    member val SinglePrevious = true with get, set
    member val SingleCandidate = 0 with get, set
    member val SingleMatch = -1 with get, set
    member val N = 0 with get, set
    member val M = 0 with get, set
    member val MaxD = 0 with get, set
    member val Offset = 0 with get, set
    member val Delta = 0 with get, set
    member val OddDelta = false with get, set
    member val Forward = Array.empty<int> with get, set
    member val Reverse = Array.empty<int> with get, set
    member val ForwardSet = Array.empty<byte> with get, set
    member val ReverseSet = Array.empty<byte> with get, set
    member val D = 0 with get, set
    member val K = 0 with get, set
    member val X = 0 with get, set
    member val Y = 0 with get, set

module private MyersFrontier =
    let inline intAt (values: int[]) index = Native.readInt values index
    let inline isSet (values: byte[]) index = Native.readByte values index <> 0
    let inline writeInt (values: int[]) index value = Native.writeInt values index value
    let inline markSet (values: byte[]) index = Native.writeByte values index 1

type MyersStepper(
    previousStart: int,
    previousCount: int,
    currentStart: int,
    currentCount: int,
    stepLimit: int,
    initialMeter: Meter,
    equal: int -> int -> bool option
) =
    let tasks = Stack<MyersTask>()
    let output = ResizeArray<MyersOperation>()
    let limit = max 0 stepLimit
    let mutable meter = initialMeter
    let mutable active: MyersRange option = None
    let mutable steps = 0
    let mutable terminal: MyersStepResult option = None
    let mutable pendingComparison: struct (int * int * bool option) option = None

    let pushSuffix (range: MyersRange) =
        if range.SuffixCount > 0 then
            tasks.Push(
                WriteEqual(
                    range.PreviousEnd - range.SuffixCount,
                    range.CurrentEnd - range.SuffixCount,
                    range.SuffixCount
                )
            )

    let pushDeleteInsert previousStart previousCount currentStart currentCount =
        if currentCount > 0 then tasks.Push(WriteInserts(currentStart, currentCount))
        if previousCount > 0 then tasks.Push(WriteDeletes(previousStart, previousCount))

    let pushSingle (range: MyersRange) =
        let previousRangeStart = range.PreviousStart
        let currentRangeStart = range.CurrentStart
        let previousRangeCount = range.CorePreviousEnd - previousRangeStart
        let currentRangeCount = range.CoreCurrentEnd - currentRangeStart
        pushSuffix range
        pushDeleteInsert previousRangeStart previousRangeCount currentRangeStart currentRangeCount
        active <- None

    let pushFallback (range: MyersRange) =
        let previousCount = range.CorePreviousEnd - range.PreviousStart
        let currentCount = range.CoreCurrentEnd - range.CurrentStart
        pushSuffix range
        pushDeleteInsert range.PreviousStart previousCount range.CurrentStart currentCount
        active <- None

    let beginBisect (range: MyersRange) =
        let n = range.CorePreviousEnd - range.PreviousStart
        let m = range.CoreCurrentEnd - range.CurrentStart
        let maxD = (n + m + 1) / 2
        let offset = maxD + 1
        let length = 2 * maxD + 3
        let forward = Array.zeroCreate<int> length
        let reverse = Array.zeroCreate<int> length
        let forwardSet = Array.zeroCreate<byte> length
        let reverseSet = Array.zeroCreate<byte> length
        Native.writeInt forward (offset + 1) 0
        Native.writeByte forwardSet (offset + 1) 1
        Native.writeInt reverse (offset + 1) 0
        Native.writeByte reverseSet (offset + 1) 1
        range.N <- n
        range.M <- m
        range.MaxD <- maxD
        range.Offset <- offset
        range.Delta <- n - m
        range.OddDelta <- ((n - m) &&& 1) <> 0
        range.Forward <- forward
        range.Reverse <- reverse
        range.ForwardSet <- forwardSet
        range.ReverseSet <- reverseSet
        range.D <- 0
        range.K <- 0
        range.Stage <- RangeStage.ForwardFrontier

    let emitOne (task: MyersTask) =
        match task with
        | WriteEqual(previous, current, count) when count > 0 ->
            output.Add(MyersOperation.Equal(previous, current))
            if count > 1 then tasks.Push(WriteEqual(previous + 1, current + 1, count - 1))
            true
        | WriteDeletes(previous, count) when count > 0 ->
            output.Add(MyersOperation.Delete previous)
            if count > 1 then tasks.Push(WriteDeletes(previous + 1, count - 1))
            true
        | WriteInserts(current, count) when count > 0 ->
            output.Add(MyersOperation.Insert current)
            if count > 1 then tasks.Push(WriteInserts(current + 1, count - 1))
            true
        | _ -> false

    let chargeWork () =
        if steps >= limit then
            false
        else
            Meter.charge meter 1
            steps <- steps + 1
            true

    let compare previousIndex currentIndex =
        if steps >= limit then ComparisonStep.LimitExceeded
        else
            match pendingComparison with
            | Some(struct (pendingPrevious, pendingCurrent, None)) ->
                if pendingPrevious <> previousIndex || pendingCurrent <> currentIndex then
                    invalidOp "The pending comparison no longer matches the active Myers step."
                ComparisonStep.Waiting(previousIndex, currentIndex)
            | Some(struct (pendingPrevious, pendingCurrent, Some result)) ->
                if pendingPrevious <> previousIndex || pendingCurrent <> currentIndex then
                    invalidOp "The resolved comparison no longer matches the active Myers step."
                pendingComparison <- None
                Meter.charge meter 1
                steps <- steps + 1
                ComparisonStep.Compared result
            | None ->
                match equal previousIndex currentIndex with
                | Some result ->
                    Meter.charge meter 1
                    steps <- steps + 1
                    ComparisonStep.Compared result
                | None ->
                    pendingComparison <- Some(struct (previousIndex, currentIndex, None))
                    ComparisonStep.Waiting(previousIndex, currentIndex)

    let markLimitExceeded () =
        terminal <- Some MyersStepResult.StepLimitExceeded

    let finishIfIdle () =
        if active.IsNone && tasks.Count = 0 then
            let result = MyersStepResult.Complete(output.ToArray())
            terminal <- Some result
            Some result
        else None

    do
        if previousCount < 0 then invalidArg (nameof previousCount) "The previous count cannot be negative."
        if currentCount < 0 then invalidArg (nameof currentCount) "The current count cannot be negative."
        if isNull (box equal) then nullArg (nameof equal)
        tasks.Push(VisitRange(previousStart, previousStart + previousCount, currentStart, currentStart + currentCount))

    new(previousStart, previousCount, currentStart, currentCount, stepLimit, meter, equal: int -> int -> bool) =
        MyersStepper(
            previousStart,
            previousCount,
            currentStart,
            currentCount,
            stepLimit,
            meter,
            fun previousIndex currentIndex -> Some(equal previousIndex currentIndex)
        )

    member _.Steps = steps

    member _.UseMeter(value: Meter) = meter <- value

    member _.ResolveComparison(result: bool) =
        match pendingComparison with
        | Some(struct (previousIndex, currentIndex, None)) ->
            pendingComparison <- Some(struct (previousIndex, currentIndex, Some result))
        | Some(struct (_, _, Some _)) -> invalidOp "The pending comparison has already been resolved."
        | None -> invalidOp "There is no pending comparison to resolve."

    member _.Step() =
        match terminal, pendingComparison with
        | Some result, _ -> result
        | None, Some(struct (previousIndex, currentIndex, None)) ->
            MyersStepResult.NeedComparison(previousIndex, currentIndex)
        | None, _ ->
            let mutable didWork = false
            let mutable answer: MyersStepResult option = None
            while answer.IsNone && not didWork && terminal.IsNone do
                match active with
                | None ->
                    match finishIfIdle () with
                    | Some result -> answer <- Some result
                    | None ->
                        match tasks.Pop() with
                            | VisitRange(aStart, aEnd, bStart, bEnd) ->
                                active <- Some(MyersRange(aStart, aEnd, bStart, bEnd))
                            | task ->
                                if emitOne task then
                                    Meter.charge meter 1
                                    didWork <- true
                | Some range ->
                    match range.Stage with
                    | RangeStage.Prefix ->
                        if range.PreviousStart < range.PreviousEnd && range.CurrentStart < range.CurrentEnd then
                            match compare range.PreviousStart range.CurrentStart with
                            | ComparisonStep.Compared matches ->
                                if matches then
                                    output.Add(MyersOperation.Equal(range.PreviousStart, range.CurrentStart))
                                    range.PreviousStart <- range.PreviousStart + 1
                                    range.CurrentStart <- range.CurrentStart + 1
                                else
                                    let previousRemaining = range.PreviousEnd - range.PreviousStart
                                    let currentRemaining = range.CurrentEnd - range.CurrentStart
                                    if previousRemaining = 1 && currentRemaining = 1 then
                                        pushDeleteInsert range.PreviousStart 1 range.CurrentStart 1
                                        active <- None
                                    else range.Stage <- RangeStage.Suffix
                                didWork <- true
                            | ComparisonStep.Waiting(previousIndex, currentIndex) ->
                                answer <- Some(MyersStepResult.NeedComparison(previousIndex, currentIndex))
                            | ComparisonStep.LimitExceeded -> markLimitExceeded ()
                        else range.Stage <- RangeStage.Suffix
                    | RangeStage.Suffix ->
                        let previousRemaining = range.PreviousEnd - range.PreviousStart - range.SuffixCount
                        let currentRemaining = range.CurrentEnd - range.CurrentStart - range.SuffixCount
                        if previousRemaining > 0 && currentRemaining > 0 then
                            let previousIndex = range.PreviousEnd - range.SuffixCount - 1
                            let currentIndex = range.CurrentEnd - range.SuffixCount - 1
                            match compare previousIndex currentIndex with
                            | ComparisonStep.Compared matches ->
                                if matches then
                                    range.SuffixCount <- range.SuffixCount + 1
                                else
                                    range.Stage <- RangeStage.Prepare
                                didWork <- true
                            | ComparisonStep.Waiting(waitPrevious, waitCurrent) ->
                                answer <- Some(MyersStepResult.NeedComparison(waitPrevious, waitCurrent))
                            | ComparisonStep.LimitExceeded -> markLimitExceeded ()
                        else range.Stage <- RangeStage.Prepare
                    | RangeStage.Prepare ->
                        let previousLength = range.PreviousEnd - range.PreviousStart - range.SuffixCount
                        let currentLength = range.CurrentEnd - range.CurrentStart - range.SuffixCount
                        range.CorePreviousEnd <- range.PreviousEnd - range.SuffixCount
                        range.CoreCurrentEnd <- range.CurrentEnd - range.SuffixCount
                        if previousLength = 0 || currentLength = 0 then
                            pushSuffix range
                            pushDeleteInsert range.PreviousStart previousLength range.CurrentStart currentLength
                            active <- None
                        elif previousLength = 1 || currentLength = 1 then
                            range.SinglePrevious <- (previousLength = 1)
                            range.SingleCandidate <- if previousLength = 1 then range.CurrentStart else range.PreviousStart
                            range.SingleMatch <- -1
                            range.Stage <- RangeStage.SingleSearch
                        else
                            beginBisect range
                    | RangeStage.SingleSearch ->
                        let candidatesRemain =
                            if range.SinglePrevious then range.SingleCandidate < range.CoreCurrentEnd
                            else range.SingleCandidate < range.CorePreviousEnd
                        if candidatesRemain then
                            let previousIndex = if range.SinglePrevious then range.PreviousStart else range.SingleCandidate
                            let currentIndex = if range.SinglePrevious then range.SingleCandidate else range.CurrentStart
                            match compare previousIndex currentIndex with
                            | ComparisonStep.Compared matches ->
                                if matches then
                                    range.SingleMatch <- if range.SinglePrevious then currentIndex else previousIndex
                                    let matched = range.SingleMatch
                                    let previousCoreEnd = range.CorePreviousEnd
                                    let currentCoreEnd = range.CoreCurrentEnd
                                    let actions = ResizeArray<MyersTask>()
                                    if range.SinglePrevious then
                                        actions.Add(WriteInserts(range.CurrentStart, matched - range.CurrentStart))
                                        actions.Add(WriteEqual(range.PreviousStart, matched, 1))
                                        actions.Add(WriteInserts(matched + 1, currentCoreEnd - matched - 1))
                                    else
                                        actions.Add(WriteDeletes(range.PreviousStart, matched - range.PreviousStart))
                                        actions.Add(WriteEqual(matched, range.CurrentStart, 1))
                                        actions.Add(WriteDeletes(matched + 1, previousCoreEnd - matched - 1))
                                    pushSuffix range
                                    for index = actions.Count - 1 downto 0 do
                                        match actions[index] with
                                        | WriteDeletes(_, count)
                                        | WriteInserts(_, count)
                                        | WriteEqual(_, _, count) when count = 0 -> ()
                                        | task -> tasks.Push task
                                    active <- None
                                else
                                    range.SingleCandidate <- range.SingleCandidate + 1
                                didWork <- true
                            | ComparisonStep.Waiting(waitPrevious, waitCurrent) ->
                                answer <- Some(MyersStepResult.NeedComparison(waitPrevious, waitCurrent))
                            | ComparisonStep.LimitExceeded -> markLimitExceeded ()
                        else
                            pushSingle range
                    | RangeStage.ForwardFrontier ->
                        if range.K > range.D then
                            range.Stage <- RangeStage.ReverseFrontier
                            range.K <- -range.D
                        else
                            let index = range.Offset + range.K
                            let forward = range.Forward
                            let forwardSet = range.ForwardSet
                            let x =
                                if range.K = -range.D
                                   || (range.K <> range.D
                                       && (not (MyersFrontier.isSet forwardSet (index - 1))
                                           || MyersFrontier.intAt forward (index - 1) < MyersFrontier.intAt forward (index + 1))) then
                                    if MyersFrontier.isSet forwardSet (index + 1) then MyersFrontier.intAt forward (index + 1) else 0
                                else
                                    (if MyersFrontier.isSet forwardSet (index - 1) then MyersFrontier.intAt forward (index - 1) else 0) + 1
                            range.X <- x
                            range.Y <- x - range.K
                            range.Stage <- RangeStage.ForwardSnake
                            if chargeWork () then didWork <- true else markLimitExceeded ()
                    | RangeStage.ForwardSnake ->
                        if range.X < range.N && range.Y >= 0 && range.Y < range.M then
                            let previousIndex = range.PreviousStart + range.X
                            let currentIndex = range.CurrentStart + range.Y
                            match compare previousIndex currentIndex with
                            | ComparisonStep.Compared matches ->
                                if matches then
                                    range.X <- range.X + 1
                                    range.Y <- range.Y + 1
                                else
                                    let index = range.Offset + range.K
                                    let forward = range.Forward
                                    let forwardSet = range.ForwardSet
                                    let reverse = range.Reverse
                                    let reverseSet = range.ReverseSet
                                    MyersFrontier.writeInt forward index range.X
                                    MyersFrontier.markSet forwardSet index
                                    if range.OddDelta then
                                        let reverseDiagonal = range.Delta - range.K
                                        let reverseIndex = range.Offset + reverseDiagonal
                                        if reverseIndex >= 0
                                           && reverseIndex < reverse.Length
                                           && MyersFrontier.isSet reverseSet reverseIndex
                                           && range.X >= range.N - MyersFrontier.intAt reverse reverseIndex then
                                            range.Stage <- RangeStage.Fallback
                                            let splitPrevious = range.PreviousStart + range.X
                                            let splitCurrent = range.CurrentStart + range.Y
                                            pushSuffix range
                                            tasks.Push(VisitRange(splitPrevious, range.CorePreviousEnd, splitCurrent, range.CoreCurrentEnd))
                                            tasks.Push(VisitRange(range.PreviousStart, splitPrevious, range.CurrentStart, splitCurrent))
                                            active <- None
                                    if active.IsSome then
                                        range.K <- range.K + 2
                                        range.Stage <- RangeStage.ForwardFrontier
                                didWork <- true
                            | ComparisonStep.Waiting(waitPrevious, waitCurrent) ->
                                answer <- Some(MyersStepResult.NeedComparison(waitPrevious, waitCurrent))
                            | ComparisonStep.LimitExceeded -> markLimitExceeded ()
                        else
                            let index = range.Offset + range.K
                            let forward = range.Forward
                            let forwardSet = range.ForwardSet
                            let reverse = range.Reverse
                            let reverseSet = range.ReverseSet
                            MyersFrontier.writeInt forward index range.X
                            MyersFrontier.markSet forwardSet index
                            if range.OddDelta then
                                let reverseDiagonal = range.Delta - range.K
                                let reverseIndex = range.Offset + reverseDiagonal
                                if reverseIndex >= 0
                                   && reverseIndex < reverse.Length
                                   && MyersFrontier.isSet reverseSet reverseIndex
                                   && range.X >= range.N - MyersFrontier.intAt reverse reverseIndex then
                                    let splitPrevious = range.PreviousStart + range.X
                                    let splitCurrent = range.CurrentStart + range.Y
                                    pushSuffix range
                                    tasks.Push(VisitRange(splitPrevious, range.CorePreviousEnd, splitCurrent, range.CoreCurrentEnd))
                                    tasks.Push(VisitRange(range.PreviousStart, splitPrevious, range.CurrentStart, splitCurrent))
                                    active <- None
                                else
                                    range.K <- range.K + 2
                                    range.Stage <- RangeStage.ForwardFrontier
                            else
                                range.K <- range.K + 2
                                range.Stage <- RangeStage.ForwardFrontier
                    | RangeStage.ReverseFrontier ->
                        if range.K > range.D then
                            if range.D + 1 >= range.MaxD then
                                range.Stage <- RangeStage.Fallback
                                pushFallback range
                            else
                                range.D <- range.D + 1
                                range.K <- -range.D
                                range.Stage <- RangeStage.ForwardFrontier
                        else
                            let index = range.Offset + range.K
                            let reverse = range.Reverse
                            let reverseSet = range.ReverseSet
                            let x =
                                if range.K = -range.D
                                   || (range.K <> range.D
                                       && (not (MyersFrontier.isSet reverseSet (index - 1))
                                           || MyersFrontier.intAt reverse (index - 1) < MyersFrontier.intAt reverse (index + 1))) then
                                    if MyersFrontier.isSet reverseSet (index + 1) then MyersFrontier.intAt reverse (index + 1) else 0
                                else
                                    (if MyersFrontier.isSet reverseSet (index - 1) then MyersFrontier.intAt reverse (index - 1) else 0) + 1
                            range.X <- x
                            range.Y <- x - range.K
                            range.Stage <- RangeStage.ReverseSnake
                            if chargeWork () then didWork <- true else markLimitExceeded ()
                    | RangeStage.ReverseSnake ->
                        if range.X < range.N && range.Y >= 0 && range.Y < range.M then
                            let previousIndex = range.CorePreviousEnd - range.X - 1
                            let currentIndex = range.CoreCurrentEnd - range.Y - 1
                            match compare previousIndex currentIndex with
                            | ComparisonStep.Compared matches ->
                                if matches then
                                    range.X <- range.X + 1
                                    range.Y <- range.Y + 1
                                else
                                    let index = range.Offset + range.K
                                    let reverse = range.Reverse
                                    let reverseSet = range.ReverseSet
                                    let forward = range.Forward
                                    let forwardSet = range.ForwardSet
                                    MyersFrontier.writeInt reverse index range.X
                                    MyersFrontier.markSet reverseSet index
                                    if not range.OddDelta then
                                        let forwardDiagonal = range.Delta - range.K
                                        let forwardIndex = range.Offset + forwardDiagonal
                                        if forwardIndex >= 0
                                           && forwardIndex < forward.Length
                                           && MyersFrontier.isSet forwardSet forwardIndex
                                           && MyersFrontier.intAt forward forwardIndex >= range.N - range.X then
                                            let x = MyersFrontier.intAt forward forwardIndex
                                            let y = x - forwardDiagonal
                                            let splitPrevious = range.PreviousStart + x
                                            let splitCurrent = range.CurrentStart + y
                                            pushSuffix range
                                            tasks.Push(VisitRange(splitPrevious, range.CorePreviousEnd, splitCurrent, range.CoreCurrentEnd))
                                            tasks.Push(VisitRange(range.PreviousStart, splitPrevious, range.CurrentStart, splitCurrent))
                                            active <- None
                                    if active.IsSome then
                                        range.K <- range.K + 2
                                        range.Stage <- RangeStage.ReverseFrontier
                                didWork <- true
                            | ComparisonStep.Waiting(waitPrevious, waitCurrent) ->
                                answer <- Some(MyersStepResult.NeedComparison(waitPrevious, waitCurrent))
                            | ComparisonStep.LimitExceeded -> markLimitExceeded ()
                        else
                            let index = range.Offset + range.K
                            let reverse = range.Reverse
                            let reverseSet = range.ReverseSet
                            let forward = range.Forward
                            let forwardSet = range.ForwardSet
                            MyersFrontier.writeInt reverse index range.X
                            MyersFrontier.markSet reverseSet index
                            if not range.OddDelta then
                                let forwardDiagonal = range.Delta - range.K
                                let forwardIndex = range.Offset + forwardDiagonal
                                if forwardIndex >= 0
                                   && forwardIndex < forward.Length
                                   && MyersFrontier.isSet forwardSet forwardIndex
                                   && MyersFrontier.intAt forward forwardIndex >= range.N - range.X then
                                    let x = MyersFrontier.intAt forward forwardIndex
                                    let y = x - forwardDiagonal
                                    let splitPrevious = range.PreviousStart + x
                                    let splitCurrent = range.CurrentStart + y
                                    pushSuffix range
                                    tasks.Push(VisitRange(splitPrevious, range.CorePreviousEnd, splitCurrent, range.CoreCurrentEnd))
                                    tasks.Push(VisitRange(range.PreviousStart, splitPrevious, range.CurrentStart, splitCurrent))
                                    active <- None
                                else
                                    range.K <- range.K + 2
                                    range.Stage <- RangeStage.ReverseFrontier
                            else
                                range.K <- range.K + 2
                                range.Stage <- RangeStage.ReverseFrontier
                    | RangeStage.Fallback ->
                        pushFallback range
            if terminal.IsSome then
                terminal.Value
            elif answer.IsSome then
                answer.Value
            elif didWork then
                match finishIfIdle () with
                | Some result -> result
                | None -> MyersStepResult.Running
            else
                match finishIfIdle () with
                | Some result -> result
                | None -> MyersStepResult.Running
