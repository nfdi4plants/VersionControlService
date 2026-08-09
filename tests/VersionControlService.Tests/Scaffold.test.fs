module VersionControlService.Tests.ScaffoldTests

open VersionControlService.Abstractions
open VersionControlService.Tests.VitestBindings

module GitWorkspaceSession = VersionControlService.Git.GitWorkspaceSession

describe("VersionControlService provider scaffold", fun () ->
    test("exposes the Git provider through the public factory contract", fun () ->
        let factory = GitWorkspaceSession.createFactory GitWorkspaceSession.GitSessionHooks.none
        toEqual (expect (ProviderId.value factory.Id), "git")
    )

    test("keeps repository path validation in the provider-neutral abstraction", fun () ->
        toEqual (
            expect (RepositoryPath.tryCreate "folder/file.txt" |> Result.map RepositoryPath.value),
            Ok "folder/file.txt"
        )

        toBe (expect (RepositoryPath.tryCreate "../outside.txt" |> Result.isError), true)
    )
)
