module ProjectInfo

open System.IO

module ProjectPaths =

    let srcPath = Path.GetFullPath "src/VersionControlService"
    let testsPath = Path.GetFullPath "tests/VersionControlService.Tests"

let gitOwner = "nfdi4plants"
let project = "VersionControlService"
let projectRepo = $"https://github.com/{gitOwner}/{project}"
