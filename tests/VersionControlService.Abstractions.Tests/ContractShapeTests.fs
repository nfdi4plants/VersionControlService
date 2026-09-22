module VersionControlService.Abstractions.Tests.ContractShapeTests

open Expecto
open VersionControlService.Abstractions

/// Fake providers shared by the shape and portable-consumer tests. They implement
/// only what they support: absent services are None, never stubs.
module FakeProvider =

    let providerId (name: string) =
        match ProviderId.tryCreate name with
        | Ok id -> id
        | Error message -> failwith message

    let providerRef (name: string) =
        match ProviderRef.tryCreate name with
        | Ok reference -> reference
        | Error message -> failwith message

    let repositoryPath (path: string) =
        match RepositoryPath.tryCreate path with
        | Ok value -> value
        | Error message -> failwith message

    let progress: OperationProgress = {
        PhaseCode = "transfer"
        Item = None
        Completed = Some 3_000_000_000.0
        Total = Some 4_000_000_000.0
        DisplayMessage = None
    }

    let storagePolicy: StoragePolicyService = {
        SetPathPolicy = fun (_path: RepositoryPath) _enabled _context -> async { return OperationResult.succeeded () }
        GetSettings =
            fun _ ->
                async {
                    return
                        OperationResult.succeeded {
                            AutoPolicyThresholdMb = Some 1
                            MaterializeLargeObjects = true
                        }
                }
        SetSettings = fun _ _ -> async { return OperationResult.succeeded () }
    }

    let private finalPath = repositoryPath "portable.txt"

    let private finalTargetRef: LogicalRef = {
        Name = "fake-target"
        ProviderRef = providerRef "fake-target"
        Kind = RemoteRef
        IsCurrent = false
    }

    let private finalConflictItem: ConflictItem = {
        Path = finalPath
        Candidates = [||]
        CombinedPreview = Some(TextPreview "combined preview")
        SupportsResolvedContent = true
    }

    let private finalObjectState: ObjectState = {
        Path = finalPath
        IsMaterialized = true
        IsLocallyAvailable = true
        SizeBytes = None
        ObjectId = None
    }

    let private unsupported (operation: string) : OperationResult<'T> =
        OperationResult.failed (
            OperationFailure.create Unsupported "operation_not_supported" $"{operation} is not supported by this provider."
        )

    let createFinalTextDiff () : TextDiffService = {
        GetDiff = fun _ _ -> async { return OperationResult.succeeded (TextContent "diff") }
        GetWordDiff = fun _ _ -> async { return OperationResult.succeeded (TextContent "word diff") }
        GetBaseContent = fun _ _ -> async { return OperationResult.succeeded (TextContent "base content") }
    }

    let createFinalSynchronization () : SynchronizationService =
        let state = {
            BaseRevision = None
            WorkspaceRevision = None
            TargetRevision = None
            TargetRef = Some finalTargetRef
            LocalRevisionCount = None
            TargetRevisionCount = None
            RemoteChangedPaths = None
            Relationship = UpToDate
        }

        {
            Refresh = fun _ -> async { return OperationResult.succeeded state }
            PreviewUpdate = fun _ -> async { return unsupported "Synchronization preview" }
            Update = fun _ _ -> async { return unsupported "Synchronization update" }
            Publish = fun _ _ -> async { return unsupported "Synchronization publish" }
            Synchronize = fun _ _ -> async { return unsupported "Synchronization synchronize" }
        }

    let createFinalConflictResolution () : ConflictResolutionService = {
        GetActiveSession =
            fun _ ->
                async {
                    return
                        OperationResult.succeeded (
                            Some {
                                Handle = {
                                    SessionId = "fake-conflict-session"
                                    Version = "1"
                                }
                                Items = [| finalConflictItem |]
                            }
                        )
                }
        Resolve = fun _ _ -> async { return unsupported "Conflict resolution" }
        Finalize = fun _ _ -> async { return unsupported "Conflict finalization" }
        Cancel = fun _ _ -> async { return unsupported "Conflict cancellation" }
    }

    let createFinalObjectMaterialization () : ObjectMaterializationService = {
        ListObjects = fun _ -> async { return OperationResult.succeeded [| finalObjectState |] }
        Materialize = fun _ _ -> async { return unsupported "Object materialization" }
        Dematerialize = fun _ _ -> async { return unsupported "Object dematerialization" }
    }

    let fakeLocation (id: ProviderId) : RepositoryLocation = {
        ProviderId = id
        DisplayName = None
        ProviderLocation = "fake://repository"
        ConnectionProfileId = None
    }

    /// Minimal well-behaved in-memory core for contract-shape tests.
    let createCore () : CoreVersionControl =
        let mutable version = 1
        let mutable revisionCounter = 0
        let currentToken () = $"fake-token-{version}"

        let status () = {
            CurrentRef = None
            WorkspaceVersion = currentToken ()
            Changes = [||]
            ActiveConflictSession = None
            Synchronization = None
        }

        {
            GetStatus = fun _ -> async { return OperationResult.succeeded (status ()) }
            ListRefs = fun _ -> async { return OperationResult.succeeded [||] }
            CreateRef =
                fun request _ -> async {
                    version <- version + 1

                    return
                        OperationResult.succeeded {
                            Name = request.Name
                            ProviderRef = providerRef request.Name
                            Kind = LocalRef
                            IsCurrent = request.SwitchTo
                        }
                }
            PreflightSwitchRef =
                fun _ _ -> async { return OperationResult.succeeded { PathsAtRisk = [||]; IsSafe = true } }
            SwitchRef =
                fun _ _ -> async {
                    version <- version + 1
                    return OperationResult.succeeded (status ())
                }
            CreateRevision =
                fun request _ -> async {
                    if request.ExpectedWorkspaceVersion <> currentToken () then
                        return
                            OperationResult.failed (
                                OperationFailure.create Concurrency "precondition_failed" "Stale workspace version."
                            )
                    else
                        version <- version + 1
                        revisionCounter <- revisionCounter + 1

                        match RevisionId.tryCreate $"fake-rev-{revisionCounter}" with
                        | Ok revisionId -> return OperationResult.succeeded revisionId
                        | Error message -> return failwith message
                }
            RestorePaths =
                fun _ _ -> async {
                    version <- version + 1
                    return OperationResult.succeeded ()
                }
            GetDiffSummary = fun _ -> async { return OperationResult.succeeded { Entries = [||] } }
        }

    let createSynchronization () : SynchronizationService =
        let state = {
            BaseRevision = None
            WorkspaceRevision = None
            TargetRevision = None
            TargetRef = None
            LocalRevisionCount = None
            TargetRevisionCount = None
            RemoteChangedPaths = None
            Relationship = NoTarget
        }

        {
            Refresh = fun _ -> async { return OperationResult.succeeded state }
            PreviewUpdate =
                fun _ -> async {
                    return
                        OperationResult.succeeded {
                            ChangedPaths = [||]
                            OverlappingPaths = [||]
                            HasDataLossRisk = false
                            WouldCreateConflictSession = false
                        }
                }
            Update = fun _ _ -> async { return OperationResult.noOp (Some "no target configured") state }
            Publish = fun _ _ -> async { return OperationResult.noOp (Some "no target configured") state }
            Synchronize = fun _ _ -> async { return OperationResult.noOp (Some "no target configured") state }
        }

    let createBrowser () : RepositoryBrowserService = {
        GetRepositoryWebUrl = fun _ -> async { return OperationResult.succeeded None }
    }

    let createBinding (id: ProviderId) (workspaceRoot: string) (location: RepositoryLocation) : WorkspaceBinding = {
        SchemaVersion = WorkspaceBinding.CurrentSchemaVersion
        ProviderId = id
        WorkspaceRoot = workspaceRoot
        ProviderStateRef = None
        Location = location
        ConnectionProfileId = location.ConnectionProfileId
    }

    let createFactory (id: ProviderId) (buildSession: WorkspaceDescriptor -> WorkspaceSession) : ProviderFactory = {
        Id = id
        Probe = fun _ -> async { return NotDetected }
        VerifyLocation =
            fun request _ -> async {
                return
                    OperationResult.succeeded {
                        Location = request.Location
                        GrantedIntents = request.Intents
                        DeniedIntents = [||]
                    }
            }
        Initialize =
            fun request _ -> async {
                let location = request.Location |> Option.defaultValue (fakeLocation id)
                return OperationResult.succeeded (createBinding id request.TargetPath location)
            }
        Clone =
            fun request _ -> async {
                return OperationResult.succeeded (createBinding id request.TargetPath request.Location)
            }
        Adopt =
            fun _ _ ->
                async {
                    return
                        OperationResult.failed (
                            OperationFailure.create Unsupported "operation_not_supported" "Adoption is not supported by this provider."
                        )
                }
        Bind =
            fun request _ -> async {
                return OperationResult.succeeded (createBinding id request.WorkspaceRoot request.Location)
            }
        Open =
            fun binding _ -> async {
                let descriptor = {
                    ProviderId = binding.ProviderId
                    WorkspaceRoot = binding.WorkspaceRoot
                    Location = Some binding.Location
                }

                return OperationResult.succeeded (buildSession descriptor)
            }
        CheckDependencies = fun _ -> async { return OperationResult.succeeded [||] }
        InstallDependency =
            fun _ _ ->
                async {
                    return
                        OperationResult.failed (
                            OperationFailure.create
                                Unsupported
                                "operation_not_supported"
                                "Dependency installation is not supported by this provider."
                        )
                }
    }

let private expectSucceeded (operationName: string) (result: OperationResult<'T>) : 'T =
    match result with
    | Succeeded outcome -> outcome.Value
    | PartiallySucceeded _ -> failtest $"{operationName} unexpectedly returned partial success."
    | Failed failure -> failtest $"{operationName} failed ({failure.Category}/{failure.Code}): {failure.Message}"

/// The consumer-side feature discovery pattern: check presence once, enable UI.
let private discoverFeatures (session: WorkspaceSession) = [
    if session.Synchronization.IsSome then
        "synchronization"
    if session.TextDiff.IsSome then
        "text-diff"
    if session.ConflictResolution.IsSome then
        "conflicts"
    if session.ObjectMaterialization.IsSome then
        "materialization"
    if session.StoragePolicy.IsSome then
        "storage-policy"
    if session.Maintenance.IsSome then
        "maintenance"
    if session.RepositoryBrowser.IsSome then
        "browser"
]

let private openSession (factory: ProviderFactory) =
    async {
        let context = OperationContext.detached "contract-shape"
        let location = FakeProvider.fakeLocation factory.Id

        let! bindResult =
            factory.Bind
                {
                    WorkspaceRoot = "/fake/workspace"
                    Location = location
                }
                context

        let binding = expectSucceeded "bind" bindResult
        let! openResult = factory.Open binding context
        return expectSucceeded "open" openResult
    }

[<Tests>]
let contractShapeTests =
    testList "ContractShapes" [
        testCase "typed provider fakes retain byte-scale progress and storage settings"
        <| fun () ->
            Expect.equal FakeProvider.progress.Completed (Some 3_000_000_000.0) "Completed progress is not limited to 32 bits."
            Expect.equal FakeProvider.progress.Total (Some 4_000_000_000.0) "Total progress is not limited to 32 bits."

            let context = OperationContext.detached "storage-policy-settings"

            match FakeProvider.storagePolicy.GetSettings context |> Async.RunSynchronously with
            | Succeeded outcome ->
                Expect.equal outcome.Value.AutoPolicyThresholdMb (Some 1) "The fake returns typed storage settings."
                Expect.isTrue outcome.Value.MaterializeLargeObjects "The fake returns its materialization setting."
            | PartiallySucceeded _
            | Failed _ -> failtest "Expected fake storage settings."

        testCaseAsync "an opened session returns all final SPI additions through services and operation results"
        <| async {
            let id = FakeProvider.providerId "fake.final-surface"

            let factory =
                FakeProvider.createFactory id (fun descriptor -> {
                    WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()) with
                        TextDiff = Some(FakeProvider.createFinalTextDiff ())
                        ConflictResolution = Some(FakeProvider.createFinalConflictResolution ())
                        Synchronization = Some(FakeProvider.createFinalSynchronization ())
                        ObjectMaterialization = Some(FakeProvider.createFinalObjectMaterialization ())
                })

            let! session = openSession factory
            let context = OperationContext.detached "final-service-surface"

            let textDiff =
                match session.TextDiff with
                | Some service -> service
                | None -> failtest "Expected the text-diff service."

            let! baseResult = textDiff.GetBaseContent (FakeProvider.repositoryPath "portable.txt") context
            let baseContent = expectSucceeded "get base content" baseResult
            Expect.equal baseContent (TextContent "base content") "Base content comes from the service result."

            let conflicts =
                match session.ConflictResolution with
                | Some service -> service
                | None -> failtest "Expected the conflict-resolution service."

            let! summaryResult = conflicts.GetActiveSession context
            let summary = expectSucceeded "get active conflict session" summaryResult
            let conflict = summary |> Option.defaultWith (fun () -> failtest "Expected an active conflict session.")
            Expect.equal conflict.Items[0].CombinedPreview (Some(TextPreview "combined preview")) "Combined preview comes from the session result."

            let synchronization =
                match session.Synchronization with
                | Some service -> service
                | None -> failtest "Expected the synchronization service."

            let! stateResult = synchronization.Refresh context
            let state = expectSucceeded "refresh synchronization" stateResult
            Expect.equal (state.TargetRef |> Option.map _.Name) (Some "fake-target") "Target ref comes from the refresh result."

            let objects =
                match session.ObjectMaterialization with
                | Some service -> service
                | None -> failtest "Expected the object-materialization service."

            let! objectsResult = objects.ListObjects context
            let objectState = expectSucceeded "list objects" objectsResult |> Array.exactlyOne
            Expect.isTrue objectState.IsLocallyAvailable "Local availability comes from the object-list result."
        }

        testCaseAsync "a core-only provider implements no optional service"
        <| async {
            let id = FakeProvider.providerId "fake.coreonly"

            let factory =
                FakeProvider.createFactory id (fun descriptor ->
                    WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()))

            let! session = openSession factory

            Expect.equal (discoverFeatures session) [] "No optional feature is discovered."

            let context = OperationContext.detached "core-only-status"
            let! statusResult = session.Core.GetStatus context
            let status = expectSucceeded "core status" statusResult

            Expect.equal status.WorkspaceVersion "fake-token-1" "The core works without any optional service."
        }

        testCaseAsync "a synchronization provider needs no object-storage extension"
        <| async {
            let id = FakeProvider.providerId "fake.sync"

            let factory =
                FakeProvider.createFactory id (fun descriptor -> {
                    WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()) with
                        Synchronization = Some(FakeProvider.createSynchronization ())
                })

            let! session = openSession factory

            Expect.equal (discoverFeatures session) [ "synchronization" ] "Only synchronization is discovered."

            match session.Synchronization with
            | Some synchronization ->
                let! refreshResult = synchronization.Refresh(OperationContext.detached "sync-refresh")
                let state = expectSucceeded "refresh" refreshResult
                Expect.equal state.Relationship NoTarget "Truthful state without invented counters."
            | None -> failtest "Expected the synchronization service."
        }

        testCaseAsync "an unknown external provider ID with one optional extension round-trips"
        <| async {
            let id = FakeProvider.providerId "vendor.example-vcs"

            let factory =
                FakeProvider.createFactory id (fun descriptor -> {
                    WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()) with
                        RepositoryBrowser = Some(FakeProvider.createBrowser ())
                })

            Expect.equal (ProviderId.value factory.Id) "vendor.example-vcs" "External ID round-trips unchanged."

            let! session = openSession factory
            Expect.equal (discoverFeatures session) [ "browser" ] "Only the browser extension is discovered."
        }

        testCaseAsync "stale workspace versions are rejected with the stable concurrency code"
        <| async {
            let id = FakeProvider.providerId "fake.concurrency"

            let factory =
                FakeProvider.createFactory id (fun descriptor ->
                    WorkspaceSession.createCoreOnly descriptor (FakeProvider.createCore ()))

            let! session = openSession factory
            let context = OperationContext.detached "stale-mutation"

            let! revisionResult =
                session.Core.CreateRevision
                    {
                        Message = "stale attempt"
                        Paths = [||]
                        ExpectedWorkspaceVersion = "fake-stale-revision"
                    }
                    context

            match revisionResult with
            | Failed failure ->
                Expect.equal failure.Category Concurrency "Stale mutations fail with Concurrency."
                Expect.equal failure.Code "precondition_failed" "The stable code is precondition_failed."
            | Succeeded _
            | PartiallySucceeded _ -> failtest "Expected the stale mutation to fail."
        }
    ]
