module VersionControlService.Abstractions.Tests.PathsTests

open Expecto
open VersionControlService.Abstractions

let private expectOkPath (value: string) =
    match RepositoryPath.tryCreate value with
    | Ok path -> path
    | Error message -> failtest $"Expected '{value}' to be a valid repository path but got: {message}"

let private expectErrorPath (value: string) =
    match RepositoryPath.tryCreate value with
    | Ok _ -> failtest $"Expected repository path '{value}' to be rejected."
    | Error _ -> ()

[<Tests>]
let identifierTests =
    testList "Identifiers" [
        testCase "accepts well-known and vendor provider IDs"
        <| fun () ->
            for validId in [ "git"; "lakefs"; "vendor.product" ] do
                match ProviderId.tryCreate validId with
                | Ok providerId -> Expect.equal (ProviderId.value providerId) validId "Provider ID round-trips."
                | Error message -> failtest message

        testCase "unknown provider IDs round-trip unchanged"
        <| fun () ->
            match ProviderId.tryCreate "third-party.experimental_2" with
            | Ok providerId ->
                Expect.equal (ProviderId.value providerId) "third-party.experimental_2" "Unknown ID round-trips."
            | Error message -> failtest message

        testCase "rejects empty and malformed provider IDs"
        <| fun () ->
            for invalidId in [ ""; "   "; ".leading"; "has space"; "slash/id" ] do
                match ProviderId.tryCreate invalidId with
                | Ok _ -> failtest $"Expected provider ID '{invalidId}' to be rejected."
                | Error _ -> ()
    ]

[<Tests>]
let repositoryLocationTests =
    testList "Repository locations" [
        testCase "removes URL userinfo without changing other location forms"
        <| fun () ->
            let cases = [
                "https://user:token@host/org/repo.git/", "https://host/org/repo.git/"
                "https://token@host/org/repo.git", "https://host/org/repo.git"
                "HTTPS://USER:TOKEN@Host:8443/org/repo.git", "HTTPS://Host:8443/org/repo.git"
                "https://user:token@host:3443/org/repo.git", "https://host:3443/org/repo.git"
                "http://user:token@host/org/repo.git", "http://host/org/repo.git"
                "https://host/group/@repo.git", "https://host/group/@repo.git"
                "ssh://git:secret@host:2222/group/repo.git", "ssh://git@host:2222/group/repo.git"
                "ssh://git@host:2222/group/repo.git", "ssh://git@host:2222/group/repo.git"
                "git@host:group/repo.git", "git@host:group/repo.git"
                @"C:\repos\repo", @"C:\repos\repo"
                "lakefs://repo/main", "lakefs://repo/main"
            ]

            for input, expected in cases do
                Expect.equal (RepositoryLocation.withoutUserInfo input) expected $"Location stays correct for '{input}'."
    ]

[<Tests>]
let pathTests =
    testList "Paths" [
        testCase "accepts exact relative paths including metacharacters"
        <| fun () ->
            let validPaths = [
                "a.txt"
                "dir/sub/file"
                "with space.txt"
                " leading-space.txt"
                "über-β/δ.txt"
                "a[1].txt"
                "star*.txt"
                "question?.txt"
                ".hidden"
                "dir/.hidden"
                "line\nbreak.txt"
            ]

            for validPath in validPaths do
                let path = expectOkPath validPath
                Expect.equal (RepositoryPath.value path) validPath "Byte-exact round-trip."

        testCase "canonically equivalent NFC and NFD spellings stay distinct"
        <| fun () ->
            let nfc = "café.txt"
            let nfd = "café.txt"

            let nfcPath = expectOkPath nfc
            let nfdPath = expectOkPath nfd

            Expect.notEqual nfcPath nfdPath "Constructors apply no Unicode normalization."
            Expect.equal (RepositoryPath.value nfcPath) nfc "NFC spelling preserved."
            Expect.equal (RepositoryPath.value nfdPath) nfd "NFD spelling preserved."

        testCase "case-different paths stay distinct"
        <| fun () -> Expect.notEqual (expectOkPath "File.txt") (expectOkPath "file.txt") "No case folding."

        testCase "rejects empty, absolute, traversal, NUL, and separator-malformed paths"
        <| fun () ->
            let invalidPaths = [
                ""
                "/abs"
                "/"
                "C:/drive"
                "c:/drive"
                "a/../b"
                "../escape"
                "./a"
                "a/."
                ".."
                "."
                "nul\000char"
                "back\\slash"
                "a//b"
                "trailing/"
            ]

            for invalidPath in invalidPaths do
                expectErrorPath invalidPath

        testCase "keeps forward slashes as the only separator"
        <| fun () ->
            let path = expectOkPath "a/b/c.txt"
            Expect.equal (RepositoryPath.value path) "a/b/c.txt" "Representation is stable."

        testCase "wildcards are literal file-name characters, not globs"
        <| fun () ->
            Expect.notEqual (expectOkPath "*.txt") (expectOkPath "a.txt") "No glob interpretation."

            Expect.equal (RepositoryPath.value (expectOkPath "*.txt")) "*.txt" "The star is part of the name."
    ]
