open System
open ProjectInfo

[<EntryPoint>]
let main args =
    let argv = args |> Array.map (fun x -> x.ToLower()) |> Array.toList

    match argv with
    | "bundle" :: _ ->
        Bundle.Library ()
        0
    | "tests" :: a
    | "test" :: a ->
        match a with
        | "run" :: _ ->
            match Test.Run.all |> Async.RunSynchronously with
            | Ok() -> 0
            | Error _ -> 1
        | "watch" :: _ ->
            Test.Watch ()
            0
        | _ ->
            printRedfn "Usage: build test [run|watch]"
            1
    | _ ->
        Console.WriteLine("No valid argument provided. Usage: build [bundle|test run|test watch]")
        1
