module VersionControlService.TextDiff.Tests.Main

open Expecto
open VersionControlService.TextDiff.Tests

[<Tests>]
let textDiffTests =
    let tests =
        (TextDiffEngineCases.cases @ WorkerScratchCases.cases @ TextDiffSessionCases.cases @ TextDiffStreamingCases.cases @ TextDiffResyncCases.cases @ RowLinesCases.cases)
        |> List.map (fun (name, run) -> testCaseAsync name (run ()))

    testList "TextDiffEngine" (FnvReferenceTests.randomReferenceTests :: tests)
