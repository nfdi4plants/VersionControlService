namespace VersionControlService.TextDiff.Tests

open System
open VersionControlService.TextDiff

type ManualClock(initialTimeMs: float) =
    let mutable nowMs = initialTimeMs

    member _.Set(value: float) = nowMs <- value
    member _.Advance(deltaMs: float) = nowMs <- nowMs + deltaMs

    interface IClock with
        member _.NowMs() = nowMs

type MemoryByteSource(bytes: byte[], ?growing: bool) =
    let canGrow = defaultArg growing false
    let mutable available = if canGrow then 0L else int64 bytes.Length

    member _.AdvanceTo(length: int64) =
        if length < available || length < 0L || length > int64 bytes.Length then
            invalidArg (nameof length) "The available prefix must grow within the source."
        available <- length

    member _.AdvanceBy(count: int64) =
        if count < 0L then invalidArg (nameof count) "The advance count cannot be negative."
        let next = min (int64 bytes.Length) (available + count)
        available <- next

    interface IByteSource with
        member _.IsComplete() = available = int64 bytes.Length
        member _.ReadAt position buffer offset count = async {
            if position < 0L then invalidArg (nameof position) "The position cannot be negative."
            if isNull buffer then nullArg (nameof buffer)
            if offset < 0 || count < 0 || offset > buffer.Length - count then
                invalidArg (nameof offset) "The destination range is invalid."
            if count = 0 then
                if available = int64 bytes.Length then return ReadOutcome.Bytes 0
                else return ReadOutcome.NotYetAvailable
            else
                let readable = available - position
                if readable <= 0L then
                    if available = int64 bytes.Length then return ReadOutcome.EndOfSource
                    else return ReadOutcome.NotYetAvailable
                else
                    let copied = min count (int (min readable (int64 Int32.MaxValue)))
                    if copied = 0 then return ReadOutcome.NotYetAvailable
                    else
                        Array.Copy(bytes, int position, buffer, offset, copied)
                        return ReadOutcome.Bytes copied
        }

module Host =
    let synchronousYield () = async.Return ()

    /// The cap of the stores a test host creates. It is far above any test diff, so only a store test reaches it.
    let memoryCapBytes = 512L * 1024L * 1024L

    let createInMemory (clock: IClock) =
        let group = MemoryStoreGroup memoryCapBytes
        {
            Clock = clock
            Yield = synchronousYield
            CreateTempStore = fun _ -> async.Return(group.Create "test")
            CheckWrite = fun () -> async.Return None
        }
