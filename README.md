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

## Compared with dudect

[dudect][dude]'s ingredients are the right ones — fix-vs-random
input classes, randomised interleaving, cropping at multiple
percentile thresholds — and censor adopts all of them (the
multi-clip hedge is dudect's cropping ladder, combined validly).
The difference is the statistical layer that consumes the
measurements.

dudect reports a t-statistic and leaves the verdict to you: below
~4.5 is fine, above ~10 is a leak, in between is "keep running".
Re-checking a t-test as data accumulates quietly breaks it — every
peek is another chance for noise to cross the line — which is why
the practical threshold is folklore rather than theory, and why
"what is my false-positive rate?" has no answer.

censor's e-process layer is built for continuous monitoring: the
driver checks after every sample pair, halts the moment evidence
suffices, and the type-I guarantee survives the peeking. The
verdict is binary and the knob is `alpha`: if the target really is
constant-time, the probability of ever seeing a false `Reject` —
across the whole run, however long — is at most `alpha`. That claim
is itself checked empirically by the calibration battery described
below.

Neither tool can certify code as constant-time: a `Pass` is absence
of detectable evidence at the given budget, hardware, and meter,
not proof. Both are falsifiers — censor is one whose alarm rate you
can put a number on.

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
  C.Reject {} -> putStrLn $
    "leak detected after " ++ show (C.resPairs result)
    ++ " sample pairs"
  C.Pass {} -> putStrLn $
    "no leak detected within budget ("
    ++ show (C.resPairs result) ++ " pairs, "
    ++ show (C.resClipped result) ++ " clipped)"
```

For the standard dudect-style fix-vs-random protocol, `fixVsRandom`
builds the hypothesis and encodes the measurement-hygiene rules
(below) for you: the fixed secret is drawn once from the sampler —
a heap value reached by the same code path as the random class —
class A draws and discards a fresh secret per pair so the samplers
stay allocation-symmetric, and public inputs stay fresh in both
classes:

```haskell
-- pin a secret key; keep per-pair messages fresh in both classes
hyp <- C.fixVsRandom
  (\(k, m) -> () <$ evaluate (SHA256.hmac k m))         -- target
  (random32 g)                                          -- secret
  (\k -> do m <- randomBytes g 256; pure (k, m))        -- completion

result <- C.runCT C.wallClock C.defaultConfig hyp
```

(`Censor.Rng` provides the splitmix64 generator — `mkRng`,
`nextWord` — that samplers typically build on: deterministic under
a fixed seed, cheap, and allocation-light.)

`Reject` and `Pass` carry the same run report: pairs consumed
(`resPairs`), the mixture's peak log-wealth (`resPeakLogW`),
per-component peak log-wealths (`resPeakSign`, `resPeakMagn`,
`resPeakMagn4`, `resPeakMagn16`), the warmup-estimated clip bound
(`resBound`), and a clip count (`resClipped`): the number of pairs
whose absolute difference `|d| = |ta - tb|` exceeded the bound and
was truncated before being fed to the tight magnitude component.
Clipping preserves the type-I error guarantee but reduces power
against tail-shifting leaks, so a clip count that is a substantial
fraction of `resPairs` indicates the warmup bound was too tight.
Raise `cfgWarmup`, enlarge `cfgBatch`, or treat the run with
suspicion.

### A/A diagnostic

Before trusting a `Reject` on a new hypothesis, run the A/A
diagnostic to rule out harness asymmetry:

```haskell
result <- C.runAA meter C.defaultConfig hyp
-- or, composably:
result <- C.runCT meter C.defaultConfig (C.toAA hyp)
```

`toAA` replaces `sampleB` with `sampleA`, so both classes measure
the same distribution — the A/A hypothesis is H_0 by construction
and should always `Pass`. A `Reject` verdict on `toAA` indicts the
*harness* (unbalanced allocation between the samplers, RTS state
leaking between calls, unfair sample order) rather than a leak in
the target; the real-hypothesis result is not informative until the
asymmetry is fixed.

`toBB` / `runBB` are the symmetric companions — they run
`sampleB` vs `sampleB`, which exercises asymmetries specific to
`sampleB` that `toAA` can't see (a heavier-allocation-path
sampler, RTS state that only its fresh objects trigger). Running
both diagnostics attributes any `Reject` to one of: (a) target
leak, (b) `sampleA`-specific harness bug, (c) `sampleB`-specific
harness bug, or (d) pervasive harness asymmetry across both.

`attribute` packages the whole protocol — it runs both diagnostics
and folds their verdicts into a four-way `Attribution`
(`TargetLeak`, `SampleAAsymmetry`, `SampleBAsymmetry`, or
`PervasiveAsymmetry`, with the rejecting diagnostic results
attached as evidence):

```haskell
att <- C.attribute meter C.defaultConfig hyp
```

### Reproducibility

`cfgSeed :: Maybe Word64` in `Config` controls the per-pair A/B
order-bit PRNG. The default `Nothing` draws fresh entropy from the
monotonic clock per run, so the conditional independence of the
order bit from any periodic ambient noise (timer interrupts,
scheduler quanta) is a fact rather than an assumption. Pin
`cfgSeed = Just s` when you want reproducible runs — for a CI
tripwire, a debugging session, or a validation baseline.

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
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word (Word8)

-- C side: void ct_mul(const uint8_t *a, const uint8_t *b,
--                     uint8_t *out);   -- 32-byte operands.
foreign import ccall unsafe "ct_mul"
  c_mul :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()

let hyp = F.FFIHypothesis
      { F.ffiWorkspaceBytes = 96     -- two 32B inputs + 32B output
      , F.ffiSampleA = \p -> do
          F.fillRandom g (p `plusPtr`  0) 32  -- draw and discard,
          fillBytes      (p `plusPtr`  0) 0 32  -- then fix input a
          F.fillRandom g (p `plusPtr` 32) 32  -- input b: random
      , F.ffiSampleB = \p -> do
          F.fillRandom g (p `plusPtr`  0) 32
          F.fillRandom g (p `plusPtr` 32) 32
      , F.ffiTarget = \p ->
          c_mul p (p `plusPtr` 32) (p `plusPtr` 64)
      }

result <- F.withFFIHypothesis hyp $ \h ->
  C.runCT C.wallClock C.defaultConfig h
```

(`fillRandom` fills workspace bytes from a `Censor.Rng` generator;
the discarded class-A fill keeps the samplers' generator work
symmetric before the fixed operand overwrites it.)

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

The K=4 hedge runs four e-processes per pair plus a log-sum-exp
composition, so the per-pair driver cost is a few times the single-
e-process baseline. For any target that itself takes a microsecond
or longer per call, that overhead is dominated by the target and is
functionally invisible; for sub-microsecond noop targets it shows
up as ~150 nanoseconds per pair.

Current benchmark figures on an M4 Silicon MacBook Air (use
`cabal bench` to reproduce):

```
  benchmarking runCT (noop target, const meter, batch=1)/budget=1000
  time                 147.8 μs   (147.3 μs .. 148.3 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 148.0 μs   (147.6 μs .. 148.4 μs)
  std dev              1.236 μs   (968.0 ns .. 1.667 μs)

  benchmarking runCT (noop target, const meter, batch=1)/budget=10000
  time                 1.451 ms   (1.448 ms .. 1.455 ms)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.448 ms   (1.446 ms .. 1.452 ms)
  std dev              10.61 μs   (7.455 μs .. 17.21 μs)

  benchmarking runCT (noop target, const meter, batch=1)/budget=100000
  time                 14.52 ms   (14.44 ms .. 14.67 ms)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 14.49 ms   (14.46 ms .. 14.55 ms)
  std dev              94.30 μs   (40.16 μs .. 183.2 μs)
```

Per-pair allocation is linear in budget but independent of batch
size, attesting that the driver does not accumulate any state
proportional to the inner-loop count:

```
  runCT (noop target, const meter, batch=1)

    Case            Allocated  GCs
    budget=100         67,112    0
    budget=1000       491,912    0
    budget=10000    4,739,912    1
    budget=100000  47,219,912   11

  runCT (noop target, const meter, budget=1000)

    Case        Allocated  GCs
    batch=1       491,912    0
    batch=10      491,912    0
    batch=100     491,912    0
    batch=1000    491,912    0
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
full run at the current battery size takes roughly an hour under
the K=4 hedge. The default test suite carries a lightweight smoke
tier that catches gross wiring regressions only.

### Hedged e-process composition

The driver internally runs a four-way convex-hedge mixture of
anytime-valid e-processes, all consuming the same per-pair
difference `d = ta - tb`:

  * **Sign** — a two-sided Bernoulli test at `p_0 = 1/2` on
    `sign d` (ties skipped). No bounds, no clipping. Detects any
    distributional asymmetry, including equal-mean shape leaks and
    heavy-tail bulk shifts where the mean signal is swamped by tail
    variance.
  * **Magnitude at c, 4c, 16c** — three two-sided bounded-mean
    tests on `d` clipped to progressively wider intervals. The
    tight bound (`c`, from warmup `p99(|d|)`) gives the largest
    `lambda_max` and dominates against small consistent bulk
    shifts; the wider bounds preserve signal from rare-outlier
    leaks (e.g. a rarely-taken data-dependent branch) that the
    tight clip would truncate away.

Uniform weights over the four components; threshold at
`log(8/alpha)` (the `+log 4` over the single-component
`log(2/alpha)` is the price of hedging). Rejection latches on the
supremum of the mixture's log-wealth, not on the components'
individual suprema. Each component's own peak log-wealth is
surfaced in the `Result` for diagnosis — a rejection driven by the
sign component points at a shape or bulk-shift leak, one driven by
the wide magnitude components at rare outliers — but the peaks are
attained at possibly different times and never gate the verdict.

### Baseline

`etc/validate/baseline.csv` is the captured baseline from N=10000
on master. Headline: zero rejections across all 62 cells (36
calibration + 2 edge + 24 power-H_0). At `alpha = 1e-2`, all 9
gated calibration scenarios PASS; at `alpha = 1e-3` they sit at
INCONCLUSIVE because the Bonferroni-adjusted Wilson upper at 0
rejections in 10000 trials is `0.00108`, marginally above `1e-3`.
N would need to climb to ~15000 to push the upper bound below
`1e-3`. Zero rejections is itself overwhelming evidence the alpha
guarantee holds in practice; the INCONCLUSIVE label is a property
of conservative gating, not of the framework.

### Power

The power-H_1 cells inject a positive shift to class B and measure
detection rate. Headline findings from the baseline:

  * **Symmetric uniform `[0, 200]`**: shift of 5 (2.5% of noise
    range) detects at 100% with `cfgBudget = 10000`; not detected
    at `cfgBudget = 1000`. Shift of 50 detects at 100% from
    `cfgBudget = 1000`.
  * **Narrow-jitter (baseline 10000, noise `±10`)**: shift of 1
    (10% of noise) detects at 100% from `cfgBudget = 1000`. This
    scenario is the regime where betting on the clipped paired
    difference `d = ta - tb` (rather than on the clipped per-class
    readings) earns its keep — `lambda_max` scales with noise
    magnitude (~20) instead of baseline magnitude (~10000), a
    ~500x boost in per-pair wealth growth.
  * **Heavy-tailed (95% uniform `[50, 150]` + 5% uniform
    `[5000, 10000]`)**: shifts of 5, 10, and 50 all detect at 100%
    with `cfgBudget = 10000`; shift of 50 detects even at
    `cfgBudget = 1000`. The sign component is what closes this
    gap — a bulk shift moves `P(d > 0)` off `1/2` regardless of
    tail variance, so the sign test grows wealth cleanly even
    while the magnitude components are drowning in tail noise.
    Shift of 1 remains undetected at these budgets (0.2% of tail
    range; below the noise floor at any statistic).

The hedge composition brings a ~10-15% threshold overhead vs a
single component (from `log(2/alpha)` to `log(8/alpha)`), which
shows up as a small regression at marginal detection points — e.g.
sym-uniform shift=10 at `cfgBudget = 1000` fell from 100% (under
the previous single-component design) to 0% (under the hedge). The
tradeoff is qualitative: we now detect leak shapes that were
undetectable at any budget under the single-component design.

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

[dude]: https://github.com/oreparaz/dudect
[eproc]: https://git.ppad.tech/eproc
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[const]: https://www.chosenplaintext.ca/articles/beginners-guide-constant-time-cryptography.html
[subst]: https://git.ppad.tech/substratum
