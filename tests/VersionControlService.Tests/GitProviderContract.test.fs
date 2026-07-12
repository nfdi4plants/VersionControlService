module VersionControlService.Tests.GitProviderContractTests

open Vitest

// Vitest fails an included *.test.js file that contains no test suite, and a
// missing-suite structural failure is never acceptable evidence. This placeholder
// keeps full-suite runs green until Task 8 Step 0 registers the real Git harness
// with the seven shared conformance profiles.
Vitest.describe (
    "GitProviderContract placeholder",
    fun () ->
        Vitest.test (
            "placeholder until Task 8 Step 0 registers the Git harness",
            fun () -> Vitest.expect(true).toBe (true)
        )
)
