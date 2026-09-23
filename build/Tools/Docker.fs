module Docker

/// Fails with the reason the target needs Docker rather than with a missing-command error.
let requireCli (reason: string) =
    try
        run "docker" [ "--version" ] ""
    with _ ->
        failwith reason

let startDetached (name: string) (args: string list) =
    run "docker" ([ "run"; "--detach"; "--name"; name ] @ args) ""

let isRunning (name: string) =
    let exitCode, standardOutput, _ =
        runReadResult "docker" [ "inspect"; "--format"; "{{.State.Running}}"; name ] ""

    exitCode = 0 && standardOutput.Trim() = "true"

let logs (name: string) = run "docker" [ "logs"; name ] ""

/// Removing the container is cleanup, so it must not mask the failure that led here.
let remove (name: string) =
    try
        run "docker" [ "rm"; "--force"; name ] ""
    with _ ->
        printRedfn "Could not remove the container %s." name
