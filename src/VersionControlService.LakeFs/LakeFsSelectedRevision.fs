module VersionControlService.LakeFs.LakeFsSelectedRevision

open VersionControlService.LakeFs.LakeFsTypes

let verifiesExpectedParentAndHead
    (expectedParent: string option)
    (commit: LakeFsCommit)
    (branchAfter: LakeFsBranch)
    =
    branchAfter.CommitId = commit.Id
    &&
       match expectedParent with
       | Some parent -> commit.Parents |> Array.contains parent
       | None -> true
