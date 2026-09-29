namespace VersionControlService.TextDiff

type Limits = {
    MaxUnits: int
    RequestMs: float
    QuantumMs: float
}

module Limits =
    let defaults = { MaxUnits = 1_000_000; RequestMs = 400.0; QuantumMs = 10.0 }

type Meter = {
    mutable Units: int
    Deadline: float
    mutable QuantumEnd: float
    mutable ByteRemainder: int
    MaxUnits: int
    QuantumMs: float
    Clock: IClock
}

module Meter =
    let create (clock: IClock) (limits: Limits) =
        let now = clock.NowMs()
        {
            Units = 0
            Deadline = now + limits.RequestMs
            QuantumEnd = now + limits.QuantumMs
            ByteRemainder = 0
            MaxUnits = limits.MaxUnits
            QuantumMs = limits.QuantumMs
            Clock = clock
        }

    let inline charge (meter: Meter) (units: int) =
        if units > 0 then
            if meter.Units > System.Int32.MaxValue - units then meter.Units <- System.Int32.MaxValue
            else meter.Units <- meter.Units + units

    let chargeBytes (meter: Meter) (count: int) =
        if count > 0 then
            let combinedRemainder = meter.ByteRemainder + count % 4096
            charge meter (count / 4096 + combinedRemainder / 4096)
            meter.ByteRemainder <- combinedRemainder % 4096

    let overBudget (meter: Meter) =
        meter.Units >= meter.MaxUnits || meter.Clock.NowMs() >= meter.Deadline

    let quantumDue (meter: Meter) = meter.Clock.NowMs() >= meter.QuantumEnd

    let beginNextQuantum (meter: Meter) =
        meter.QuantumEnd <- meter.Clock.NowMs() + meter.QuantumMs
