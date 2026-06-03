# ppad-censor

Sequential constant-time testing for Haskell, built on
[ppad-eproc](git://git.ppad.tech/eproc.git).

Declare a constant-time hypothesis (an `IO` action parameterised over
input, plus two input samplers); the driver interleaves paired
timings of the two classes, streams them through an anytime-valid
two-sample test, and either rejects (a leak is detected) or exhausts
its budget.

The statistical layer is anytime-valid: type-I error is controlled at
`alpha` regardless of when sampling stops, so the driver halts as
soon as the wealth process crosses threshold rather than running a
fixed budget every time.

## Use

```haskell
import qualified Censor as C

let hyp = C.Hypothesis
      { C.target  = \(a, b) -> ...      -- IO action under test
      , C.sampleA = ...                  -- IO producing class-A input
      , C.sampleB = ...                  -- IO producing class-B input
      }

result <- C.runCT C.wallClock C.defaultConfig hyp

case result of
  C.Reject n w -> putStrLn $
    "leak detected after " ++ show n ++ " sample pairs"
  C.Pass n w -> putStrLn $
    "no leak detected within budget (" ++ show n ++ " samples)"
```

On Linux, `withCounter` swaps the wall-clock meter for a hardware
performance counter (retired instructions or branches), which is
deterministic and immune to frequency scaling, cache, and scheduling
noise — preferable for sub-microsecond targets:

```haskell
-- throws Censor.MeterError if perf_event_open is unavailable
result <- C.withCounter C.InstructionsRetired $ \meter ->
  C.runCT meter C.defaultConfig hyp
```

## Measurement hygiene

The driver times only `target`, so any input-preparation work that
leaks into the timed region shows up as a spurious class difference.
Two rules keep the comparison honest:

  * Return inputs in *normal form* from the samplers (force any lazy
    conversions); otherwise the first use inside `target` pays for them.
  * Build the fixed class the *same way* as the random class — a
    heap-allocated value differing only in its data — rather than a
    top-level constant. A CAF and a fresh heap value are reached by
    different code, a difference an instruction-counter meter will
    (correctly) flag.

## Phasing

Phase 1 ships wall-clock measurement, cross-platform via
`GHC.Clock.getMonotonicTimeNSec`. Phase 2 (this release) adds the
`instructions_retired` and `branches_retired` PMU meters on Linux via
`withCounter`. A precondition tier that catches structural leaks from
the deterministic counts without statistics remains future work.
