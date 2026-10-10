# censor

[![](https://img.shields.io/hackage/v/ppad-censor?color=blue)](https://hackage.haskell.org/package/ppad-censor)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-censor-lightblue)](https://docs.ppad.tech/censor)

A pure Haskell framework for sequential constant-time testing via
anytime-valid e-processes.

Declare a constant-time hypothesis (an `IO` action plus two input
samplers). The driver times the action on paired inputs from the two
classes, in random order, and streams the pairs through an
anytime-valid two-sample test (via [ppad-eproc][eproc]). It either
rejects, i.e. detects a leak, or exhausts its budget.

Type-I error is controlled at `alpha` whenever sampling stops, so the
driver halts as soon as the evidence suffices. The test hedges over
several statistics of the timing difference, so it sees mean shifts,
rare outliers, and leaks that change only the variance.

A `Pass` is not a proof of constant-time execution. It means not enough
contrary evidence was found at the given budget, on the given hardware,
under the given meter.

## Usage

The standard [dudect][dudect]-style fix-vs-random test, here of a HMAC
under a pinned secret key:

```haskell
import qualified Censor as C
import Control.Exception (evaluate)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS

main :: IO ()
main = do
  g <- C.mkRng 42

  -- class A pins one secret key; class B draws a fresh one per
  -- sample. messages are fresh in both classes.
  hyp <- C.fixVsRandom
    (\(k, m) -> () <$ evaluate (SHA256.hmac k m))  -- target
    (C.randomBytes g 32)                           -- secret
    (\k -> evaluate (BS.copy k))                   -- re-materialise
    (\k -> do                                      -- complete input
      m <- C.randomBytes g 256
      pure (k, m))

  r <- C.runCT C.wallClock C.defaultConfig hyp
  case r of
    C.Reject {} -> putStrLn $ "leak after " ++ show (C.resPairs r)
    C.Pass {}   -> putStrLn "no leak detected within budget"
```

`fixVsRandom` keeps the two samplers symmetric in everything but the
secret (laziness, allocation, memory locality). The result also
carries an anytime-valid p-value, an effect-size confidence
interval, and per-component evidence.

Beyond that:

* `attribute` runs A/A and B/B negative controls. A rejection on
  either indicts the harness rather than the target; run them before
  trusting a `Reject`.
* `withCounter` (Linux) replaces the wall clock with a hardware
  performance counter (instructions retired, cycles, etc.), which is
  far less noisy, and can attribute a difference to a channel.
* `Censor.FFI` adapts foreign (C, Rust, etc.) targets.
* The `censor` executable (`-f+run`) tests symbols in a shared
  library directly, singly or as a battery declared in a manifest
  (see `etc/example.manifest`):

```
$ censor libfoo.dylib --manifest battery.manifest --attribute
```

## Caveats

The driver times only the target, so any asymmetry in how the
samplers prepare inputs (laziness, allocation, where the secret is
read from) shows up as a class difference. `fixVsRandom` handles the
usual cases.

`cfgBatch` repeats the target on one sample, which multiplies real
work only if each repetition recomputes (e.g. an FFI call). A pure
Haskell target is forced once per batch, so use `batch = 1` there.
Under the hardware counters on virtualised hosts, per-batch counter
overhead is large, so batch where the target allows it.

On frequency-scaled hosts, the wall clock can reject even
constant-time code: constant time is not constant power, and DVFS
converts a data-dependent power draw into a timing difference. A
wall-only rejection with clean counters is the signature. Prefer the
counters; where only the wall clock is available, pin the frequency,
or test an interval null with `cfgMargin` (`--margin`), calibrated on
the host with `censor-validate --power-gain`.

## Calibration

The `censor-validate` executable (`-f+validate`) checks the type-I
error claim empirically, running the driver against synthetic meters
with known distributions, and tabulates power against effect size:

```
$ cabal run -f+validate censor-validate -- 10000 --csv out.csv
```

The current baseline is in `etc/validate/baseline.csv`; it records
zero rejections in every null cell at every alpha.

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/censor](https://docs.ppad.tech/censor).

## Performance

Driver overhead is roughly 250 ns to 1 μs per pair, negligible for
targets that take a microsecond or more. Current benchmark figures
on an M4 Silicon MacBook Air look like (use `cabal bench` to run the
benchmark suite):

```
  benchmarking runCT (noop target, const meter, batch=1)/budget=1000
  time                 1.094 ms   (1.079 ms .. 1.112 ms)

  benchmarking runCT (noop target, const meter, batch=1)/budget=10000
  time                 3.700 ms   (3.675 ms .. 3.726 ms)

  benchmarking runCT (noop target, const meter, batch=1)/budget=100000
  time                 25.40 ms   (25.14 ms .. 25.61 ms)
```

You should compile with the 'llvm' flag for maximum performance.

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

## References

- Reparaz, Balasch & Verbauwhede (2017), "[Dude, is my code constant
  time?][dudect]" DATE.
- Waudby-Smith & Ramdas (2024), "[Estimating means of bounded random
  variables by betting][wsr24]." JRSS-B.

[eproc]: https://git.ppad.tech/eproc
[dudect]: https://eprint.iacr.org/2016/1123
[wsr24]: https://arxiv.org/abs/2010.09686
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
