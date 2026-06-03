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

## Foreign targets

For C, Rust, or cgo-shimmed Go constructs, `Censor.FFI` brackets a
single pinned workspace outside the timed region and presents a
regular `Hypothesis (Ptr Word8)` for `runCT` to consume. Samplers
fill the workspace in place; the target reads (and optionally
writes) it via offsets the caller chooses.

```haskell
import qualified Censor as C
import qualified Censor.FFI as F
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word (Word8)

-- C side: void ct_mul(const uint8_t *a, const uint8_t *b,
--                     uint8_t *out);   -- 32-byte operands.
foreign import ccall unsafe "ct_mul"
  c_mul :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()

let hyp = F.FFIHypothesis
      { F.ffiWorkspaceBytes = 96      -- two 32B inputs + 32B output
      , F.ffiSampleA = \p -> do
          fillFixed     (p `plusPtr`  0) 32   -- A: fixed input
          fillRandom ga (p `plusPtr` 32) 32   -- A: random partner
      , F.ffiSampleB = \p -> do
          fillRandom gb (p `plusPtr`  0) 32
          fillRandom gb (p `plusPtr` 32) 32
      , F.ffiTarget = \p ->
          c_mul p (p `plusPtr` 32) (p `plusPtr` 64)
      }

result <- F.withFFIHypothesis hyp $ \h ->
  C.runCT C.wallClock C.defaultConfig h
```

`ffiTarget` should be a single FFI call with no in-Haskell
preparation — any marshalling done inside it lands in the timed
region and shows up as A/B noise. Use `ccall unsafe`, not `safe`,
to avoid the RTS thread-state save/restore on every call.

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
