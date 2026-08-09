module VersionControlService.LakeFs.LakeFsObjectTransfer

open VersionControlService.Abstractions
open VersionControlService.LakeFs.LakeFsTypes

module LakeFsApi = VersionControlService.LakeFs.LakeFsApi
module NodeFileSystem = VersionControlService.Runtime.Node.FileSystem
module NodeBinaryIO = VersionControlService.Runtime.Node.BinaryIO

type SelectedObjectTransfer = {
    Path: string
    ObjectKey: string
    SourcePath: string option
    ValidateSource: (NodeFileSystem.Stats -> Result<unit, OperationFailure>) option
    IsDeletion: bool
}

type CompletedObjectTransfer = {
    Path: string
    Uploaded: NodeBinaryIO.StreamCopyResult option
}

type SelectedTransferResult =
    | TransferCompleted of completed: CompletedObjectTransfer[]
    | TransferFailed of failure: OperationFailure * completed: CompletedObjectTransfer[]

let transferSelected
    (connection: LakeFsConnection)
    (repository: string)
    (workspaceBranch: string)
    (objects: SelectedObjectTransfer[])
    (context: OperationContext)
    : Async<SelectedTransferResult> =
    async {
        let mutable failure: OperationFailure option = None
        let completed = ResizeArray<CompletedObjectTransfer>()

        for selected in objects do
            if failure.IsNone then
                let! result =
                    if selected.IsDeletion then
                        async {
                            let! deleted =
                                LakeFsApi.deleteObject
                                    connection
                                    repository
                                    workspaceBranch
                                    selected.ObjectKey
                                    context

                            return deleted |> Result.map (fun () -> None)
                        }
                    else
                        match selected.SourcePath with
                        | Some sourcePath ->
                            async {
                                let! uploaded =
                                    match selected.ValidateSource with
                                    | Some validate ->
                                        LakeFsApi.uploadObjectFromFileChecked
                                            connection
                                            repository
                                            workspaceBranch
                                            selected.ObjectKey
                                            sourcePath
                                            validate
                                            context
                                    | None ->
                                        LakeFsApi.uploadObjectFromFile
                                            connection
                                            repository
                                            workspaceBranch
                                            selected.ObjectKey
                                            sourcePath
                                            context

                                return uploaded |> Result.map Some
                            }
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
                | Ok uploaded ->
                    completed.Add {
                        Path = selected.Path
                        Uploaded = uploaded
                    }
                | Error transferFailure -> failure <- Some transferFailure

        return
            match failure with
            | Some transferFailure -> TransferFailed(transferFailure, completed.ToArray())
            | None -> TransferCompleted(completed.ToArray())
    }
