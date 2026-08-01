# Pervasive A/A + B/B control rejections on
# long-duration pure Haskell targets (timing
# meters only)

Source: `research/timing/reports/ATTACK165/`
(ppad-secp256k1 v0.5.8 audit; censor 0.4.2).
Surfaced while capturing the dynamic lanes for
the measure report.

## Summary

On the `censor-secp` suite at `alpha = 1e-6`,
`budget = 50000`, `batch = 1`, the two timing
meters reject on most cases while the
deterministic instruction counter passes every
case at full budget:

    instructions  8/8 PASS   peakLogW 0.00-2.01
    cycles        7/8 REJECT peakLogW 13.85-14.07
    wall (linux)  7/8 REJECT peakLogW 13.82-13.99
    wall (darwin) 3/8 REJECT peakLogW 13.83-13.98

Every rejection carries a controls verdict of
`pervasive asymmetry` (both A/A and B/B reject)
or `sampleA|sampleB asymmetry`. Since A/A draws
both classes from `sampleA`, no target
difference exists there by construction, so the
apparatus itself is producing the asymmetry.

Three properties suggest this is not a target
signal and not ordinary noise:

1. **Threshold grazing.** Rejections cluster in
   13.82-14.07 against a `log(1/alpha)` of
   13.8155. A real effect drives wealth well
   past threshold (the corpus's known-leaky
   positives run far higher).

2. **Non-reproducibility across replications.**
   Three back-to-back uncontended darwin wall
   replications disagree about which cases
   reject: `mul` PASS/REJECT/REJECT,
   `sign_ecdsa'` REJECT/REJECT/PASS,
   `sign_schnorr'` PASS/REJECT/PASS.

3. **Meter selectivity.** The instruction
   counter — which sees the same code, the same
   samplers, the same order — is clean at
   `peakLogW <= 2.01`. Whatever differs between
   the paired slots costs cycles without
   retiring instructions.

## Why this matters

A controls-explained rejection is correctly
reported as uninformative, so nothing is
*wrong* in the verdicts. But at this alpha the
timing meters currently bound nothing on this
target class: the audit's wall and cycles cells
carry no information either way, and only the
PMU instruction lane supports a claim.

## Notes toward a cause

Not yet diagnosed. Observations that constrain it:

- Reproduces on two unrelated machines (Apple
  M4 darwin; AMD EPYC 7713 linux, pinned with
  `taskset -c 0 nice -n 5`, load ~1.0).
- Does not reproduce on `censor-aead`, whose
  targets are ~10-100x shorter. Duration
  dependence is the most obvious axis: a
  per-pair fixed overhead that lands
  asymmetrically would matter more as the timed
  region grows, not less, so the opposite
  scaling is worth explaining.
- Instruction counts being clean rules out a
  work asymmetry between the classes (GC
  landing in one slot, an extra allocation, a
  different code path).
- The rejection driver is reported as
  `shape: cdf` rather than sign, pointing at a
  distributional/tail difference rather than a
  bulk shift.

## Suggested next steps

- Run `toAA` on a trivially constant-time
  long-duration target (a fixed-iteration
  arithmetic loop) to confirm the effect is
  independent of ppad-secp256k1.
- Check whether pair order is randomised per
  pair and whether the randomisation is
  balanced over the budget; a fixed or
  weakly-mixed slot order would give exactly
  this shape.
- Consider whether the clip bound calibrated
  from `cfgWarmup` is appropriate when the
  per-call time is large relative to the timer
  quantum.

## Diagnosis

The apparatus verdict is confirmed and the
channel identified. Established empirically on
the darwin host (Apple M4):

1. The statistical machinery is sound. Feeding
   the unmodified driver a synthetic meter whose
   readings come from controlled noise processes
   (iid, AR(1), slow drift, GC-like sawtooth,
   slot-alternating offset, warmup-to-main level
   shift, and their combination) produced 0
   rejections in 70 runs at `alpha = 1e-6`. With
   a fair order bit independent of the readings,
   no flip-blind noise process can defeat the
   supermartingale, so the recorded A/A
   rejections require the readings to depend on
   the current pair's order bit.

2. They do. Instrumented `toAA` runs on
   `mul_wnaf` (budget 3e5, order seed pinned,
   every raw reading recorded) reject in 2k-9k
   pairs on quiet-machine stretches, and
   conditioning the readings on the replayed
   flip stream shows the whole pair running
   ~0.5-1.5% slower (~150-280 ns at ~17 us)
   when `flipOrder = True` -- both slots,
   persistently (z to -8) -- plus a smaller
   flip-conditional modulation of the
   slot1-slot2 gap. Quiet-vs-noisy regime drift
   explains the non-reproducibility across
   replications. The effect is multiplicative
   in target duration (explaining the aead
   immunity, the opposite of fixed-overhead
   scaling) and needs a memory-active target:
   an equal-duration register-only spin loop
   passes cleanly minutes apart.

3. The dominant channel is the order branch in
   `runPair`. Its two textual arms compile to
   two distinct code blocks, so the flip bit
   selects which physical code executes the
   pair; replacing the branch with a single
   call path cut the per-pair asymmetry 3-6x in
   interleaved same-regime comparisons. The
   microsecond quantisation of darwin's
   `CLOCK_MONOTONIC` amplifies what remains:
   readings collapse onto 1000 ns atoms, warmup
   places CDF cut points exactly on atom
   boundaries, and a nanosecond-scale shift
   converts to a percent-scale indicator
   asymmetry at a single cut point -- the
   recorded signature (one hot cut point,
   `shape: cdf`, threshold grazing because the
   latch stops at crossing).

   Meter selectivity follows: the coupling is
   latency, not executed code. The instruction
   stream inside the measured regions is
   identical across flips, so the instruction
   lane is structurally immune.

## Resolution

Two changes:

- `runPair` is now flip-oblivious: one code
  path with a single call site per sampler and
  measurement; sampler order selected by
  indexing a two-element array with the order
  word; class labels recovered by mask
  arithmetic after both measurements; the order
  bit carried as a word and never scrutinised
  as a `Bool` in the pair loop. Verified by
  disassembly: no conditional branch on the
  order bit between the bit draw and the
  completed pair, in the main or warmup loop.

- On darwin the wall meter now reads
  `clock_gettime_nsec_np(CLOCK_UPTIME_RAW)`
  (the `mach_absolute_time` counter, ~42 ns
  ticks on Apple Silicon) instead of GHC's
  `getMonotonicTimeNSec`, whose
  `CLOCK_MONOTONIC` macOS quantises to
  microseconds. This removes the
  atom-concentration amplifier and improves
  wall resolution ~24x.

With both changes, interleaved same-regime
runs of the reproducer: stock rejects 3/3 at
3.3k-6.8k pairs; fixed passes 3/3 at the full
3e5 budget (`peakLogW <= 1.65`), residual
flip-conditional reading deltas at the
analysis noise floor. The synthetic-meter null
battery stays 0/70, and the test suite passes.

The x86_64-linux lanes carry the same
signature (cycle-resolved readings put high
density at the cut points instead of
quantisation atoms) but have not been re-run;
ATTACK165's wall and cycles cells remain
uninformative until revalidated there.
