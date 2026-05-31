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

## Phasing

Phase 1 (this release) ships wall-clock measurement only, cross-
platform via `GHC.Clock.getMonotonicTimeNSec`. Phase 2 will add PMU
hardware counter meters (`instructions_retired`, `branches_retired`)
on Linux, with a precondition tier that catches structural leaks
without statistics.
