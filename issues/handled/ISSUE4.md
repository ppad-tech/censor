# fixVsRandom classes differ in embed copy-source locality

## Summary

`fixVsRandom` balances the per-sample RNG draws between classes
(class A draws and discards), but the classes still differ in
what the embed reads from: class A embeds the *same* long-lived
fixed secret every sample, class B embeds a fresh allocation
each time. Under the standard per-draw `BS.copy` embed pattern,
class A copies from one hot, fixed-address source buffer for the
whole run while class B copies from a different fresh buffer per
sample. The A/A and B/B controls are each internally symmetric,
so neither can observe this asymmetry.

On a sensitive meter this surfaces as a distributional-shape
rejection that has nothing to do with the secret.

## Observed

ppad-poly1305 `mac`, 320 B fixed message, key fix-vs-random,
cycles meter, x86_64-linux (EPYC 7713, `taskset -c 0 nice -n 5`),
censor 0.5.0 (ad86e32), alpha 1e-6, budget 50000:

- Three consecutive REJECTs across two engagement runs and one
  re-run (peakLogW 13.83–13.88 against 13.8155, p 9.3e-7–9.8e-7,
  18k–47k pairs to rejection), every one with effect CI
  straddling zero and `RejectionDriver` naming the cdf/shape
  component — never a mean shift.
- Controls PASS on every rejection: A/A p 0.82–0.94, B/B
  p 0.36–0.63, 50k pairs each.
- `instructions` concurrently clean on the same host and runs;
  `wall` clean everywhere; the 245 B sibling case mildly
  elevated (p 0.005–0.09) but never rejecting.
- Across fresh process instances the effect is intermittent
  (subsequent runs: p 3.6e-4, 0.45, 1.0, 0.0098), while within
  a process it reproduces to rejection. Consistent with
  per-process heap layout modulating how the hot fixed-source
  buffer interacts with the cache.

## Discriminator

A pinned-pair case (class A always key k1, class B always key
k2, both drawn once at setup, no per-sample draw, both embeds
copying from long-lived sources) is flat in four of four runs
(peakLogW <= 2.4) in the same seals where fix-vs-random leans
or rejects. Secret-content dependence is thereby excluded; the
signal is the instrument's.

The harness lives at
`synthesis/fixtures/ppad-poly1305/harness/Main.hs` (`posMac`,
`posMacFixed`); the sealed evidence with full statistics and
controls sits under
`synthesis/.sandbox/ppad-poly1305/.synthesis/evidence/` (run
`censor/poly1305-mac-hw/x86_64-linux/cycles`).

## Proposed fix

Make the immediate embed source fresh in both classes: in
`fixVsRandom` (and `fixVsRandomCtx`), re-materialize the pinned
secret into a fresh allocation each class-A sample and hand
*that* to the embed, so both classes read from a fresh buffer of
identical size and age. The balanced draw-and-discard stays; the
"ordinary heap value reached by the same code path" property of
the fixed secret is preserved (strengthened: it is now a fresh
ordinary heap value per sample, as in class B).

Re-materialization is type-directed today only for the harness
(`BS.copy` in the embed); inside censor it needs a small
per-type story — a `Dup` class or a `s -> IO s` refresh argument
defaulting to `pure` — worth deciding there rather than pushing
the burden onto every harness.

## Acceptance

- The poly1305 320 B cycles case stops flapping across repeated
  runs on the same host (no rejections, p not persistently
  depressed).
- A pinned-pair (k1 vs k2) null stays flat, unchanged.
- Existing suites (aead, poly demo, validate cells) unaffected
  on wall and instructions.

## Resolution

Fixed on impl/issue4:

- 4576b08 -- `fixVsRandom`/`fixVsRandomCtx` take a
  re-materialisation function run on the pinned secret by *both*
  classes each sample: class A completes around the fresh copy,
  class B discards its copy and completes around its draw.
  Re-materialising in class A alone, as proposed above, would not
  have removed the artefact -- the per-sample read of the hot
  pinned buffer is itself the class-correlated signal, so the
  copy must run in both classes (the dual of draw-and-discard).
- e675c00 -- the dlopen CLI's FFI form of the same asymmetry:
  secret ranges are regenerated in place per sample in both
  classes via one shared reseeded generator
  (`Censor.Rng.reseed`); the long-lived fixSec buffer is gone.
- edd1f6f -- the pinned-pair discriminator (k1 vs k2, 320 B)
  added to censor-poly, making this acceptance battery runnable
  from censor alone.

Acceptance (pollux, EPYC 7713, `taskset -c 0 nice -n 5`,
censor-poly cycles, alpha 1e-6, budget 50000, warmup 500, six
interleaved fresh-process runs per tree, 2026-08-22, full
reports under `~/issue4-runs/` on the host):

- Baseline (master, ad86e32): mac key 320 B REJECTed in 2/6
  runs (p 9.9e-7 and 1.0e-6, drivers cdf and mixed, effect CI
  straddling zero, controls pass at 50k pairs each), with p
  depressed on two more (9.7e-3, 3.3e-2); the 245 B sibling
  REJECTed in 1/6 (p 9.8e-7, driver cdf) with p 1.9e-5 in
  another. The observed signature, reproduced.
- Fix (impl/issue4): mac key 320 B passed 6/6 with p in
  [0.29, 0.97]; 245 B passed 6/6 with p in [0.41, 1.00]; the
  k1-vs-k2 pinned pair flat in 6/6 (peakLogW <= 1.46).
- instructions clean on both trees (poly and aead suites); aead
  wall all-pass (encrypt elevated at p 1.7e-5 but passing,
  unremarkable for the wall meter on a shared host). The unit
  suite passes on x86_64-linux and aarch64-darwin; the
  integration suite on aarch64-darwin.

The cross-lane MAC == case (@0 vs @15) rejected intermittently
in *both* trees (driver cdf). The devshell's ppad-poly1305 is
the llvm build, so this is consistent with the known
short-circuit behaviour of the unrolled MAC Eq under -fllvm --
a true positive orthogonal to the instrument artefact fixed
here.
