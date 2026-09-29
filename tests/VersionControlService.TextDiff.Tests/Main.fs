module VersionControlService.TextDiff.Tests.Main

open Expecto
open VersionControlService.TextDiff.Tests

[<Tests>]
let textDiffTests =
    let tests =
        TextDiffEngineCases.cases
        |> List.map (fun (name, run) -> testCaseAsync name (run ()))

    testList "TextDiffEngine" (FnvReferenceTests.randomReferenceTests :: tests)
