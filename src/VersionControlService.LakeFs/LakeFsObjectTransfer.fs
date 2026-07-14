module VersionControlService.LakeFs.LakeFsObjectTransfer

open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes

module LakeFsApi = VersionControlService.LakeFs.LakeFsApi

type SelectedObjectTransfer = {
    Path: string
    ObjectKey: string
    Content: string option
    IsDeletion: bool
}

type SelectedTransferResult =
    | TransferCompleted of completedPaths: string[]
    | TransferFailed of failure: OperationFailure * completedPaths: string[]

let transferSelected
    (connection: LakeFsConnection)
    (repository: string)
    (workspaceBranch: string)
    (objects: SelectedObjectTransfer[])
    (context: OperationContext)
    : Async<SelectedTransferResult> =
    async {
        let mutable failure: OperationFailure option = None
        let completed = ResizeArray<string>()

        for selected in objects do
            if failure.IsNone then
                let! result =
                    if selected.IsDeletion then
                        LakeFsApi.deleteObject connection repository workspaceBranch selected.ObjectKey context
                    else
                        match selected.Content with
                        | Some content ->
                            LakeFsApi.uploadObject
                                connection
                                repository
                                workspaceBranch
                                selected.ObjectKey
                                content
                                context
                        | None ->
                            async.Return(
                                Error(
                                    OperationFailure.create
                                        NotFound
                                        "path_not_found"
                                        $"Selected path '{selected.Path}' has no local content."
                                )
                            )

                match result with
                | Ok() -> completed.Add selected.Path
                | Error transferFailure -> failure <- Some transferFailure

        return
            match failure with
            | Some transferFailure -> TransferFailed(transferFailure, completed.ToArray())
            | None -> TransferCompleted(completed.ToArray())
    }
