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
        member _.KnownLength = Some(int64 bytes.Length)
        member _.AvailableLength() = available
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

type MemoryTempStore() =
    let mutable bytes = Array.zeroCreate<byte> 0
    let mutable length = 0
    let mutable disposed = false

    let validateRange (buffer: byte[]) offset count =
        if disposed then invalidOp "The temporary store has been disposed."
        if isNull buffer then nullArg (nameof buffer)
        if offset < 0 || count < 0 || offset > buffer.Length - count then
            invalidArg (nameof offset) "The buffer range is invalid."

    let ensureCapacity required =
        if required < 0 then invalidArg (nameof required) "The requested size is too large."
        if required > bytes.Length then
            let grown = max required (max 256 (bytes.Length * 2))
            Array.Resize(&bytes, grown)

    interface ITempStore with
        member _.Append buffer offset count = async {
            validateRange buffer offset count
            if count > Int32.MaxValue - length then invalidArg (nameof count) "The temporary store is too large."
            let start = int64 length
            ensureCapacity (length + count)
            Array.Copy(buffer, offset, bytes, length, count)
            length <- length + count
            return start
        }

        member _.WriteAt position buffer offset count = async {
            validateRange buffer offset count
            if position < 0L || position > int64 (Int32.MaxValue - count) then
                invalidArg (nameof position) "The write position is outside the supported range."
            let start = int position
            let ending = start + count
            ensureCapacity ending
            Array.Copy(buffer, offset, bytes, start, count)
            length <- max length ending
        }

        member _.ReadAt position buffer offset count = async {
            validateRange buffer offset count
            if position < 0L then invalidArg (nameof position) "The read position cannot be negative."
            if position >= int64 length then return 0
            else
                let copied = min count (length - int position)
                Array.Copy(bytes, int position, buffer, offset, copied)
                return copied
        }

        member _.Length() = int64 length

        member _.Dispose() = async {
            bytes <- Array.empty
            length <- 0
            disposed <- true
        }

module Host =
    let synchronousYield () = async.Return ()

    let createInMemory (clock: IClock) = {
        Clock = clock
        Yield = synchronousYield
        CreateTempStore = fun _ -> async.Return (MemoryTempStore() :> ITempStore)
    }
