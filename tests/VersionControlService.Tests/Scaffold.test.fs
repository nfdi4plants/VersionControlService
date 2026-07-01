module VersionControlService.Tests.ScaffoldTests

open Thoth.Json.Core
open VersionControlService.Bindings.SimpleGit
open VersionControlService.Contracts.FileSystem
open VersionControlService.Contracts.Git
open VersionControlService.Support
open VersionControlService.Tests.NodePath
open VersionControlService.Tests.VitestBindings

describe("VersionControlService scaffold", fun () ->
    test("exposes support modules required by the Git extraction", fun () ->
        let lfsInfo: GitLfsLsFileInfo = {
            name = "folder/file.txt"
            size = 42.0
            checkout = true
            downloaded = true
            ``oid_type`` = "sha256"
            oid = "abc"
            version = "v1"
        }

        toEqual (expect (PathHelpers.normalizeSeparators "folder\\file.txt"), "folder/file.txt")
        toEqual (expect lfsInfo.name, "folder/file.txt")
        toEqual (expect GitLfsSkipSmudgeEnvKey, "GIT_LFS_SKIP_SMUDGE")
    )

    test("loads public SimpleGit binding used by the Git module", fun () ->
        let joined = join [| "folder"; "file.txt" |] |> PathHelpers.normalizeSeparators
        let git = SimpleGit.create ()

        toEqual (expect joined, "folder/file.txt")
        toBe (expect (isNull (box git)), false)
    )

    test("loads JSON decoding dependency used by the Git LFS service", fun () ->
        let decoded = ARCtrl.Json.Decode.fromJsonString Decode.string "\"ok\""

        toEqual (expect decoded, "ok")
    )
)
