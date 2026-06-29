# censor

[![](https://img.shields.io/hackage/v/ppad-censor?color=blue)](https://hackage.haskell.org/package/ppad-censor)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-censor-lightblue)](https://docs.ppad.tech/censor)

A pure Haskell framework for sequential constant-time testing via
anytime-valid e-processes. Declare a constant-time hypothesis (an `IO`
action plus two input samplers); the driver interleaves paired timings
of the two classes, streams them through an anytime-valid two-sample
test, and either rejects (a leak is detected) or exhausts its budget.

The statistical layer is anytime-valid: type-I error is controlled at
`alpha` regardless of when sampling stops, so the driver halts as soon
as the wealth process crosses threshold rather than running a fixed
budget every time. (The "wealth process" is a notional bankroll that
bets on the data and grows only when the null is wrong; see
[ppad-eproc][eproc] for the mechanics.)

## Usage

A minimal constant-time test:

```haskell
import qualified Censor as C

let hyp = C.Hypothesis
      { C.target  = \x -> ...               -- IO action under test
      , C.sampleA = ...                     -- IO producing class-A input
      , C.sampleB = ...                     -- IO producing class-B input
      }

result <- C.runCT C.wallClock C.defaultConfig hyp

case result of
  C.Reject n w nc -> putStrLn $
    "leak detected after " ++ show n ++ " sample pairs"
    ++ " (" ++ show nc ++ " readings clipped)"
  C.Pass n w nc -> putStrLn $
    "no leak detected within budget (" ++ show n ++ " samples,"
    ++ " " ++ show nc ++ " readings clipped)"
```

`Reject` and `Pass` both carry a clip count: the number of per-pair
meter readings that exceeded the warmup-estimated upper bound and
were truncated before being fed to the test. Clipping preserves the
type-I error guarantee but reduces power against tail-shifting
leaks, so a clip count that is a substantial fraction of `2 *
samples` indicates the warmup bound was too tight. Raise `cfgWarmup`,
enlarge `cfgBatch`, or treat the run with suspicion.

On Linux, `withCounter` swaps the wall-clock meter for a hardware
performance counter, which is deterministic and immune to the
frequency-scaling, cache, and scheduling noise that limits wall-clock
on sub-microsecond targets:

```haskell
-- throws Censor.MeterError if perf_event_open is unavailable
result <- C.withCounter C.InstructionsRetired $ \meter ->
  C.runCT meter C.defaultConfig hyp
```

Available counters are `InstructionsRetired`, `BranchesRetired`, and
`Cycles`. The first two are deterministic properties of the executed
code; the third is additionally sensitive to data-dependent latency
and microarchitectural state.

### Foreign targets

For C, Rust, or cgo-shimmed Go constructs, `Censor.FFI` brackets a
single pinned workspace outside the timed region and presents a
regular `Hypothesis (Ptr Word8)` for `runCT` to consume. Samplers fill
the workspace in place; the target reads (and optionally writes) it
via offsets the caller chooses.

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
      { F.ffiWorkspaceBytes = 96     -- two 32B inputs + 32B output
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
region and shows up as A/B noise. Use `ccall unsafe`, not `safe`, to
avoid the RTS thread-state save/restore on every call.

### Measurement hygiene

The driver times only `target`, so any input-preparation work that
leaks into the timed region shows up as a spurious class difference.
A few rules keep the comparison honest:

  * Return inputs in *normal form* from the samplers (force any lazy
    conversions); otherwise the first use inside `target` pays for
    them.
  * Build the fixed class the *same way* as the random class — a
    heap-allocated value differing only in its data — rather than a
    top-level constant. A CAF and a fresh heap value are reached by
    different code, a difference an instruction-counter meter will
    (correctly) flag.
  * Make the two samplers symmetric in allocation count, size, and
    order. Draw and discard a random value of the same type that
    class B keeps in place of the fixed input, so nursery occupancy
    and GC scheduling do not drift between classes.

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/censor](https://docs.ppad.tech/censor).

## Performance

The framework's per-pair overhead is small enough that the cost of a
constant-time test is dominated by the target itself for any
microsecond-or-slower target.

Current benchmark figures on an M4 Silicon MacBook Air look like
(use `cabal bench` to run the benchmark suite):

```
  benchmarking runCT (noop target, const meter, batch=1)/budget=1000
  time                 30.49 μs   (30.38 μs .. 30.65 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 30.67 μs   (30.52 μs .. 30.86 μs)
  std dev              411.9 ns   (350.4 ns .. 482.1 ns)

  benchmarking runCT (noop target, const meter, batch=1)/budget=10000
  time                 289.3 μs   (288.8 μs .. 290.1 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 289.5 μs   (289.1 μs .. 290.1 μs)
  std dev              1.171 μs   (714.0 ns .. 1.946 μs)

  benchmarking runCT (noop target, const meter, batch=1)/budget=100000
  time                 2.856 ms   (2.850 ms .. 2.864 ms)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 2.862 ms   (2.859 ms .. 2.866 ms)
  std dev              9.399 μs   (7.838 μs .. 11.80 μs)
```

Per-pair allocation is linear in budget but independent of batch
size, attesting that the driver does not accumulate any state
proportional to the inner-loop count:

```
  runCT (noop target, const meter, batch=1)

    Case            Allocated  GCs
    budget=100         59,656    0
    budget=1000       167,656    0
    budget=10000    1,247,656    0
    budget=100000  12,047,656    2

  runCT (noop target, const meter, budget=1000)

    Case        Allocated  GCs
    batch=1       167,656    0
    batch=10      167,656    0
    batch=100     167,656    0
    batch=1000    167,656    0
```

You should compile with the `llvm` flag for maximum performance.

## Empirical calibration

The framework's pitch — type-I error controlled at `alpha`, anytime
— is a statistical claim, not a code-correctness one. The
`censor-validate` executable runs the driver against synthetic
meters with known H_0 / H_1 distributions, counts rejections across
N independent trials per cell, and reports Wilson 95% binomial CIs
against each cell's alpha bound. It also produces a power table
characterising detection rate vs effect size.

```
$ cabal run -f+validate censor-validate -- 10000 --csv runs/v0.1.csv
```

The first positional argument is the trial count per cell (default
10000); `--csv path` dumps a machine-readable copy of the report for
cross-release diffing.

A cell is **PASS** when its CI's upper bound is comfortably below
its alpha (rejection rate is within budget), **FAIL** when the
lower bound exceeds alpha (a real statistical violation), and
**INCONCLUSIVE** when the CI straddles alpha (more trials needed).
The cell grid uses a Bonferroni-adjusted family confidence so that
a family-wide PASS is meaningful. Cells at very tight alpha levels
(`1e-4`, `1e-6`) and the power table are reported but not gated —
their value is characterisation, not assertion.

This is a pre-release calibration check, not a per-commit gate; a
full run takes 5–10 minutes. The default test suite carries a
lightweight smoke tier that catches gross wiring regressions only.

### Baseline

`etc/validate/baseline.csv` is the captured baseline from N=10000
on master. Headline: zero rejections across all 48 cells (32
calibration + 2 edge + 14 power-H_0). At `alpha = 1e-2`, all 8
gated calibration scenarios PASS; at `alpha = 1e-3` they sit at
INCONCLUSIVE because the Bonferroni-adjusted Wilson upper at 0
rejections in 10000 trials is `0.00108`, marginally above `1e-3`.
N would need to climb to ~15000 to push the upper bound below
`1e-3`. Zero rejections is itself overwhelming evidence the alpha
guarantee holds in practice; the INCONCLUSIVE label is a property
of conservative gating, not of the framework.

### Power

The power-H_1 cells inject a positive shift to class B and measure
detection rate. The headline finding from the baseline:

  * **Symmetric uniform `[0, 200]`**: shift of 5 (2.5% of noise
    range) is detected at 100% with `cfgBudget = 10000`; not
    detected at `cfgBudget = 1000`. Shifts of 10 and 50 detect at
    100% from `cfgBudget = 1000`.
  * **Heavy-tailed (95% uniform `[50, 150]` + 5% uniform
    `[5000, 10000]`)**: 0% detection at all tested shifts up to 50,
    at both budgets.

The second result is a real limitation. A heavy tail inflates the
warmup `p99`, so the bound `2 * p99` becomes huge, the bettor's
`lambda_max` shrinks proportionally, and even consistent
mean-shifts in the bulk of the distribution struggle to grow the
wealth process. The framework remains *calibrated* (no false
positives) under heavy tails but loses *power*. Users testing
targets with heavy-tailed timing distributions should expect
limited sensitivity unless `cfgBudget` is much larger or the
underlying meter has tighter tails (e.g. instructions-retired
under PMU is deterministic).

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter
a development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-censor
```

to get a REPL for the main library.

[eproc]: https://git.ppad.tech/eproc
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[const]: https://www.chosenplaintext.ca/articles/beginners-guide-constant-time-cryptography.html
[subst]: https://git.ppad.tech/substratum
