module VersionControlService.Contracts.FileSystem

type GitLfsLsFileInfo = {
    name: string
    size: float
    checkout: bool
    downloaded: bool
    ``oid_type``: string
    oid: string
    version: string
}
