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
budget every time.

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
  C.Reject n w -> putStrLn $
    "leak detected after " ++ show n ++ " sample pairs"
  C.Pass n w -> putStrLn $
    "no leak detected within budget (" ++ show n ++ " samples)"
```

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

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[const]: https://www.chosenplaintext.ca/articles/beginners-guide-constant-time-cryptography.html
[subst]: https://git.ppad.tech/substratum
