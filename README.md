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
[ppad-eproc][eproc] for the mechanics.) The claim that the run-wide
false-alarm probability is at most `alpha` is itself checked
empirically by the calibration battery described below.

A `Pass` is not a proof of constant-time execution — it is absence of
detectable evidence at the given budget, hardware, and meter. censor
is a falsifier whose alarm rate you can put a number on.

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
`nextWord`, `randomBytes` — that samplers typically build on:
deterministic under a fixed seed, cheap, and allocation-light.
`randomBytes` returns a strict, fully-forced `ByteString`, so a
sampler can hand its result straight to the driver.)

`Reject` and `Pass` carry the same run report: pairs consumed
(`resPairs`), the mixture's peak log e-value (`resPeakLogW`; starts
at 0, crosses `log(1/alpha)` on rejection), the anytime-valid
p-value (`resPValue`; at or below `cfgAlpha` exactly when the
verdict is `Reject`, and valid at any data-dependent stopping
time), an effect-size interval (`resEffect`, below), per-component
peak log e-values (`resPeakSign`, `resPeakMagn`, `resPeakMagn4`,
`resPeakMagn16`), the warmup-estimated clip bound (`resBound`), and
a clip count at each of the three magnitude bounds (`resClipped` at
`c`, `resClipped4` at `4c`, `resClipped16` at `16c`): the number of
pairs whose absolute difference `|d| = |ta - tb|` exceeded that
bound and was truncated before being fed to the component behind
it. Clipping preserves the type-I error guarantee but reduces power
against tail-shifting leaks, so a `resClipped` that is a
substantial fraction of `resPairs` indicates the warmup bound was
too tight. Raise `cfgWarmup`, enlarge `cfgBatch`, or treat the run
with suspicion.

The counts nest — `|d| > 16c` implies `|d| > 4c` implies `|d| > c`
— and they answer different questions. `resClipped` bounds what the
tight component could see; `resClipped16` is the count that
qualifies `resEffect`, `16c` being the bound the effect interval's
own input is clipped to. A run can starve the tight component
heavily and leave the interval's input untouched, so a consumer
quoting the interval should read `resClipped16`, not `resClipped`.

### Effect size

`resEffect` is an anytime-valid `1 - alpha` confidence interval
(the hedged-capital construction of Waudby-Smith & Ramdas, via
`Numeric.Eproc.ConfSeq`) for the mean of `d` clipped to the widest
hedge interval `[-16c, 16c]` — `resClipped16` says on how many
pairs that clipping actually bit, and so how far the interval can
be read as bounding the unrestricted effect — in meter units per
batch. On a truly
constant-time target it covers zero; on a `Pass` at full budget it
bounds the size of any undetected effect ("no leak found, and the
mean effect is within ±X"). On a `Reject` it covers the true
(clipped) shift but may still straddle zero: detection is more
sample-efficient than estimation, so a strong leak halts the run
before the interval tightens. On long runs the evidence can pin
the mean below the estimation grid's resolution; the reported
interval is then the last resolvable one — a few grid steps wide,
still covering the true mean, since the sequence's intervals are
nested and the coverage guarantee is time-uniform. Coverage of the
interval and type-I control of the verdict are separate marginal
guarantees, each at `cfgAlpha`.

### A/A and B/B negative controls

Before trusting a `Reject` on a new hypothesis, run the A/A
control to look for harness asymmetry:

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
both controls sorts any `Reject` into: (a) neither control
convicted, (b) a `sampleA`-specific harness bug, (c) a
`sampleB`-specific harness bug, or (d) pervasive harness asymmetry
across both.

`attribute` packages the whole protocol — it runs both controls and
returns an `Attribution` carrying the `ControlVerdict` alongside
*both* control runs in full, so the evidence behind the verdict can
be inspected rather than taken on trust:

```haskell
att <- C.attribute meter C.defaultConfig hyp
C.attVerdict att   -- ControlsPass | SampleAAsymmetry | ...
C.attAA att        -- the complete A/A run
C.attBB att        -- the complete B/B run
```

**These are controls, not an attribution mechanism.** They can
convict the harness; they cannot acquit it. A/A compares `sampleA`
against itself and B/B compares `sampleB` against itself, so an
asymmetry existing only *between* the two samplers is invisible to
both. If `sampleA` returns a fully forced input while `sampleB`
leaves a thunk that the target forces inside the timed region, the
primary run rejects on that harness artefact and both controls
pass. `ControlsPass` therefore reads as "no within-class control
failure was found" — which is why it is not called `TargetLeak`.
Establishing that a rejection really is the target's requires
harness symmetry argued by construction, or an independent harness.

### Paired public context

`fixVsRandom` runs its completion independently for each class, so
per-pair public inputs (message, nonce, AAD, modulus) end up either
independently random across the pair — injecting nuisance variance
into `d` that has nothing to do with the secret — or pinned for the
whole run, which tests exactly one public input.

`fixVsRandomCtx` takes the third option: draw one fresh public
context per pair, then build both classes around *that same*
context.

```haskell
hyp <- C.fixVsRandomCtx
  (\(k, m) -> () <$ evaluate (mac k m))  -- target
  (C.randomBytes g 32)                   -- secret sampler
  (C.randomBytes g 256)                  -- public context, per pair
  (\msg key -> pure (key, msg))          -- build the input
```

The tested axis is then secret dependence *conditional on identical
fresh public input*. Conditioning on the context removes it as a
source of paired variance, which sharpens power, and states the
intended axis explicitly rather than leaving it implicit in how the
completion was written. The null is unaffected: under H_0 the timing
law does not depend on the secret, so given the context the two
readings are conditionally i.i.d. and the pair stays exchangeable.

The mechanism is a `prepare` field on `Hypothesis` — a per-pair
prologue run before either sampler and outside the timed region.
It is `pure ()` for an ordinary hypothesis.

### Battery manifests

Auditing a library one symbol at a time means one `censor` process
per symbol plus a shell loop to stitch the single-case reports back
together — which duplicates the tool version and config by hand in
every script and produces no single artefact covering the battery.

`--manifest` replaces that with a declarative file:

```
meter          wall
alpha          1e-6
budget         20000
warmup         500
target-prefix  censor_target_
workspace      128

case mul   secret=0:32
case ecdh  secret=0:32 context=32:64
case inv   secret=0:32 replicates=3
```

```
$ censor libfoo.dylib --manifest battery.manifest --attribute
```

One dlopen, one `censor/report-v3` object containing every case,
its controls, the host environment, and the family-wise alpha. The
per-case layout flags (`--workspace`, `--target`, `--secret`,
`--public`, `--context`, `--trace`) are rejected in this mode rather
than silently ignored; run-wide flags still override manifest
directives. See `etc/example.manifest` for the annotated format.

A case's ranges come in three kinds. `secret=off:len` is pinned in
class A only — the fix-vs-random axis. `public=off:len` is pinned in
both classes for the whole run. `context=off:len` is redrawn once
per *pair* and written into both classes, which is the manifest's
route to the paired public context described above. Everything
outside a declared range is random per pair, per class.

`replicates=N` runs a case against N independent fixed secrets, as
N ordinary case rows. A dudect-style cell pins *one* fixed secret,
so on its own it answers "does this secret time differently from
random", not "does any secret"; replicating guards against an
unlucky draw, and each replicate counts toward the family-wise
alpha as it should. Replicate #1 keeps the seed the unreplicated
case had, so raising `replicates` extends a battery without
redefining the cell already in it; each case also mixes its own
name into the run-wide seed, so cases do not all begin from
identical bytes.

`family-alpha on` turns the reported union bound into an actual
correction: each of the M expanded cells runs at `alpha / M`, so
"any cell rejects" is a test at `alpha` family-wise. It is off by
default, since it costs power in every cell.

### Reproducibility

`cfgSeed :: Maybe Word64` in `Config` controls the per-pair A/B
order-bit PRNG. The default `Nothing` draws a fresh seed from OS
entropy (`/dev/urandom`) per run, so the conditional independence
of the order bit from any periodic ambient noise (timer interrupts,
scheduler quanta) is a fact rather than an assumption. A clock
reading would not do: it is not independent entropy, and the
nuisance process may itself be periodic in that very clock.

That is the normal path rather than an unconditional one. If
`/dev/urandom` cannot be read, the driver falls back to the
monotonic clock — better than a constant, but it forfeits the
independence argument above. Pin `cfgSeed` explicitly if you need
to know which of the two you got.

Whichever way it resolves, the seed actually used comes back as
`resSeed` on the `Result`, appears under each case row in the human
report, and is recorded as `orderSeed` in the JSON. A fresh audit
run is therefore statistically randomised *and* exactly replayable
afterwards — feed `resSeed` back as `cfgSeed = Just s`.

Keep this seed distinct from whatever seeds your input samplers.
The conditional null needs the order bit independent of the sampled
timing sequence; deriving both from one seed forfeits that. The
`censor` CLI enforces the split: `--seed` pins the samplers,
`--order-seed` pins the order bit.

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

### Wall-clock rejections on noisy hosts

Even a hypothesis that obeys every rule above can reject under the
wall meter, far in excess of `alpha`, on a busy or frequency-scaled
host — while the hardware counters consistently pass the same
target. This is not a failure of the anytime guarantee, and "host
noise" in the innocent sense cannot explain it: class-independent
noise, however heavy-tailed, drifting, or autocorrelated, cannot
inflate the false-alarm rate, because the guarantee is
distribution-free given within-pair exchangeability, which the
randomised order supplies whatever the noise's marginal law.
Symmetric noise costs power, never validity. A wall rejection that
*reproduces* is therefore a systematic: the exchangeability null
really is false for the wall readings, and the question is which
channel falsified it.

The meter signature localises the channel. Wall time is on-thread
user cycles divided by frequency, plus interrupt and kernel time,
plus descheduled time. The perf meters count on-thread, user-only
events: clean instructions rules out any class difference in the
executed stream, and clean cycles additionally rules out
data-dependent latency and microarchitectural state inside the
timed region. What remains is exactly the wall-exclusive residue —
frequency variation, interrupt service, scheduling gaps — all of
it host-mediated.

Class reaches those channels through *interference*, the one thing
order randomisation cannot remove (the nuisance-process caveat
under "The null, and what it does and does not cover", made
concrete). The coin makes position independent of class; it does
nothing about one arm's class modulating host state that the other
arm's reading then absorbs. Concretely: constant time is not
constant power. Writing and processing random bytes has higher
switching activity than re-writing a fixed value, and DVFS
hardware converts power to frequency on sub-millisecond
timescales — so the second arm of a pair is slowed by an amount
that depends on the *first* arm's class, an asymmetry that
survives randomisation and leaves cycle counts invariant by
construction. Interrupt phase behaves the same way:
content-dependent wobble in the untimed sampler code shifts where
the periodic tick lands relative to the timed window, and wall
absorbs interrupt time that an `exclude_kernel` counter does not
count.

The sequential layer then does what it is built to do: the test is
consistent, so any stable systematic — sub-nanosecond is enough
for the sign and indicator components over enough pairs — becomes
near-certain eventual rejection. A reproducible wall-only `Reject`
with clean counters therefore reads as "the class difference lives
in host-mediated time, not in the executed instruction stream" —
the expected signature of a power or phase coupling, though a
genuine power side channel in the target produces the same
signature and the meters alone cannot tell the two apart. Pinning
the frequency (performance governor, turbo off) removes the
dominant channel; the A/A control above separates residual order
coupling from class-side coupling, since it shares the harness
shape but not the content asymmetry. Prefer the counters where
they are available.

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/censor](https://docs.ppad.tech/censor).

## Performance

The hedge runs `K = 4 + n` e-processes per pair — sign, three
clipped magnitudes, and one CDF indicator per resolved cut point —
plus the mixture composition, and the effect-size confidence
sequence updates its live candidate grid every pair. The estimator
dominates the driver cost early — O(live candidates) per pair, ~300
at the start — and amortizes down as evidence rejects candidates,
so the per-pair driver cost falls from ~1 μs over short runs toward
~250 ns over long ones. For any target that itself takes a
microsecond or longer per call the overhead is dominated by the
target; for sub-microsecond targets, raise `cfgBatch` (which the
wall-clock quantum demands anyway) — but only if repetition
genuinely recomputes. See the batching contract below.

### The batching contract

`cfgBatch` repeats the *same* `target a` action on one pre-drawn
sample. That only multiplies real work when each repetition
genuinely recomputes: a foreign (FFI) call, or a target driving
mutable state. For a pure Haskell target `\a -> evaluate (f a)`
the `f a` thunk is shared across the batch and forced once, so the
remaining repetitions are no-ops on an already-evaluated value and
a per-batch reading divided by `cfgBatch` is *not* a per-call
figure.

This is a property of the target that cannot be inferred, so suites
declare it per case as a `BatchPolicy`:

  * `SingleShot` — force `batch = 1`. The correct policy for pure
    Haskell targets. If such a target is sub-quantum under the
    meter, widen the work rather than batching.
  * `RepeatableAuto` — each repetition recomputes; calibrate the
    batch with `calibrateBatchReport`. For FFI calls and targets
    driving mutable state.
  * `FixedBatch n` — pin it, for comparability across meters.

`--batch N` on any suite overrides every case's policy.

Current benchmark figures on an M4 Silicon MacBook Air (use
`cabal bench` to reproduce):

```
  benchmarking runCT (noop target, const meter, batch=1)/budget=1000
  time                 1.094 ms   (1.079 ms .. 1.112 ms)

  benchmarking runCT (noop target, const meter, batch=1)/budget=10000
  time                 3.700 ms   (3.675 ms .. 3.726 ms)

  benchmarking runCT (noop target, const meter, batch=1)/budget=100000
  time                 25.40 ms   (25.14 ms .. 25.61 ms)
```

Per-pair allocation is independent of batch size, attesting that
the driver does not accumulate any state proportional to the
inner-loop count. It is front-loaded rather than linear in budget:
the estimator's live-candidate list is rebuilt each pair and
shrinks as candidates are rejected:

```
  runCT (noop target, const meter, batch=1)

    Case            Allocated  GCs
    budget=100      1,708,008    0
    budget=1000     4,827,208    1
    budget=10000   15,123,368    3
    budget=100000  91,979,168   22

  runCT (noop target, const meter, budget=1000)

    Case        Allocated  GCs
    batch=1     4,827,208    1
    batch=10    4,827,208    1
    batch=100   4,827,208    1
    batch=1000  4,827,208    1
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
the hedge. The default test suite carries a lightweight tier that
catches gross wiring regressions only.

### The null, and what it does and does not cover

The null censor tests is **conditional exchangeability** of the
measured pair `(ta, tb)`: given everything observed so far,
swapping the two class labels leaves the joint law unchanged.

This is *stronger* than the informal property "the two input
classes induce the same timing distribution", not weaker.
Exchangeability implies the two readings share a marginal law;
equal marginals on their own say nothing about the joint, and so
cannot deliver it. What does deliver it is the randomised per-pair
A/B order — under the assumption that the within-pair nuisance
dynamics (cache and predictor state, frequency drift) are
themselves blind to the class labels. Randomisation makes
*position* independent of class; it cannot undo a nuisance process
that reacts to the class itself.

Every component of the test bets on an **antisymmetric functional**
of the pair, a statistic `g` with `g(ta, tb) = -g(tb, ta)`. Under
exchangeability each such functional is conditionally symmetric
about zero, hence has conditional mean zero, which is what makes it
a valid input to a two-sided mean-zero e-process.

The alternatives censor can detect are exactly those that move at
least one functional in its family off zero. That family is finite
and therefore **not a complete cover**: no fixed set of functionals
detects every departure from exchangeability. A `Pass` means "none
of these channels found evidence", never "this code is constant
time".

### Hedged e-process composition

The driver runs a convex-hedge mixture of anytime-valid e-processes
over that family:

  * **Sign** — a two-sided Bernoulli test at `p_0 = 1/2` on
    `sign d`, where `d = ta - tb` (ties skipped). No bounds, no
    clipping. Detects asymmetry of the difference itself
    (`P(d > 0) /= 1/2`), including heavy-tail bulk shifts where the
    mean signal is swamped by tail variance. Note that it does
    *not* detect every distributional difference: if both classes
    are symmetric about a common centre, `d` is symmetric and this
    channel sees nothing however different the two shapes are.
  * **Magnitude at c, 4c, 16c** — three two-sided bounded-mean
    tests on `d` clipped to progressively wider intervals. The
    tight bound (`c`, from warmup `p99(|d|)`) gives the largest
    `lambda_max` and dominates against small consistent bulk
    shifts; the wider bounds preserve signal from rare-outlier
    leaks (e.g. a rarely-taken data-dependent branch) that the
    tight clip would truncate away.
  * **CDF indicators** — one bounded-mean test on
    `1[ta <= q] - 1[tb <= q]` per cut point `q`, with the cut
    points fixed during warmup as order statistics of the *pooled*
    readings (deciles and quartiles, deduplicated). Each has
    conditional mean `F_A(q) - F_B(q)`, so together they see any
    difference between the two classes' CDFs at those points. This
    is the channel that catches **dispersion leaks** — a target
    whose timing *variance* depends on the secret while its mean
    does not — which leave `d` symmetric and are invisible to every
    other component.

The convex combination itself is delegated to
`Numeric.Eproc.Mixture`: uniform weights over the `K = 4 + n`
components (`n` resolved cut points), rejecting when the mixture
e-value crosses `1/alpha`. The `+log K` on the log scale over a
single component is the price of hedging — against
`log(1/alpha) = 13.8` at the default alpha, the indicator channel
costs well under a nat. Rejection latches on the supremum of the
mixture's log e-value, not on the components' individual suprema.
Each component's own peak log e-value is surfaced in the `Result`
for diagnosis — a rejection driven by the sign component points at
a shape leak, one driven by the wide magnitude components at rare
outliers, one driven by the indicators at a dispersion or general
CDF difference — but the peaks are attained at possibly different
times and never gate the verdict.

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
single component (`+log 4` on the rejection threshold), which
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

[eproc]: https://git.ppad.tech/eproc
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
