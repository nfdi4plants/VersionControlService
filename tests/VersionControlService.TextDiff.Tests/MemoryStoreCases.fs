namespace VersionControlService.TextDiff.Tests

open System
open VersionControlService.TextDiff

module MemoryStoreCases =
    let private chunk = 64 * 1024

    let private pattern (count: int) = Array.init count (fun index -> byte ((index % 250) + 1))

    let private readAll (store: ITempStore) = async {
        let length = int (store.Length())
        let output = Array.create (length + 8) 255uy
        let! read = store.ReadAt 0L output 0 output.Length
        return output[0 .. read - 1]
    }

    /// The message of the exception the work throws, or None when it completes. Fable has no distinct
    /// InvalidOperationException at run time, so the .NET run checks the exception type as well.
    let private thrownMessage (work: Async<unit>) = async {
        let! outcome = Async.Catch work
        match outcome with
        | Choice1Of2 () -> return None
        | Choice2Of2 error ->
#if !FABLE_COMPILER
            Check.true' (error :? InvalidOperationException) "The cap throws an InvalidOperationException."
#endif
            return Some error.Message
    }

    let cases: (string * (unit -> Async<unit>)) list = [
        "memory store appends and reads across a chunk boundary", fun () -> async {
            let store = (MemoryStoreGroup 1_000_000L).Create "boundary"
            let first = pattern (chunk - 10)
            let second = pattern 100
            let! start = store.Append first 0 first.Length
            let! secondStart = store.Append second 0 second.Length
            Check.equal 0L start "The first append starts at zero."
            Check.equal (int64 first.Length) secondStart "The second append starts at the end of the first."
            Check.equal (int64 (first.Length + second.Length)) (store.Length()) "The length covers both appends."
            let! all = readAll store
            Check.true' (Unchecked.equals (Array.append first second) all) "The bytes read back match the bytes written across the boundary."
            let window = Array.zeroCreate<byte> 20
            let! read = store.ReadAt (int64 (chunk - 15)) window 0 window.Length
            Check.equal 20 read "A read that spans the chunk boundary returns the requested count."
            Check.true' (Unchecked.equals (Array.append first[first.Length - 5 ..] second[0 .. 14]) window) "The window across the boundary matches."
            let! atEnd = store.ReadAt (store.Length()) window 0 window.Length
            Check.equal 0 atEnd "A read at the end returns no bytes."
            do! store.Dispose()
        }
        "memory store reads zeros in the hole of a write past the end", fun () -> async {
            let group = MemoryStoreGroup 1_000_000L
            let store = group.Create "hole"
            let payload = [| 7uy; 8uy; 9uy |]
            let position = int64 (chunk * 2 + 5)
            do! store.WriteAt position payload 0 payload.Length
            Check.equal (position + 3L) (store.Length()) "A write past the end moves the length to the end of the write."
            Check.equal (position + 3L) group.StoredBytes "The group counts the hole."
            let! all = readAll store
            Check.true' (all |> Array.take (int position) |> Array.forall (fun value -> value = 0uy)) "The hole reads as zeros."
            Check.true' (Unchecked.equals payload all[int position ..]) "The written bytes follow the hole."
            let output = Array.create 4 99uy
            let! read = store.ReadAt 0L output 0 output.Length
            Check.equal 4 read "A read inside the hole is served."
            Check.true' (output |> Array.forall (fun value -> value = 0uy)) "A read into a reused buffer overwrites it with zeros."
            do! store.Dispose()
        }
        "memory stores of one group share the stored byte count", fun () -> async {
            let group = MemoryStoreGroup 1_000_000L
            let first = group.Create "first"
            let second = group.Create "second"
            let data = pattern 1_000
            let! _ = first.Append data 0 data.Length
            let! _ = second.Append data 0 600
            Check.equal 1_600L group.StoredBytes "The group sums the lengths of its stores."
            do! first.Dispose()
            Check.equal 600L group.StoredBytes "Disposing a store subtracts its length."
            do! first.Dispose()
            Check.equal 600L group.StoredBytes "Disposing twice subtracts once."
            do! second.Dispose()
            Check.equal 0L group.StoredBytes "The group is empty after both stores are disposed."
        }
        "memory store throws when a write grows past the cap but not when it overwrites", fun () -> async {
            let group = MemoryStoreGroup 100L
            let store = group.Create "capped"
            let data = pattern 100
            let! _ = store.Append data 0 data.Length
            Check.equal 100L group.StoredBytes "The store fills the cap exactly."
            do! store.WriteAt 10L data 0 50
            Check.equal 100L (store.Length()) "An overwrite inside the length keeps the length."
            let! append = thrownMessage (async {
                let! _ = store.Append data 0 1
                return ()
            })
            Check.true' append.IsSome "An append past the cap throws."
            Check.true' (append.Value.Contains "100") "The message names the cap."
            let! grow = thrownMessage (store.WriteAt 100L data 0 1)
            Check.true' grow.IsSome "A positional write past the cap throws."
            Check.equal 100L group.StoredBytes "A refused write leaves the stored bytes alone."
            Check.equal 100L (store.Length()) "A refused write leaves the length alone."
            Check.true' (try (MemoryStoreGroup(2L * 1024L * 1024L * 1024L) |> ignore; false) with _ -> true) "A cap above 1 GiB is rejected."
        }
    ]
