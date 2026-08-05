{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main
import qualified Censor as C
import Censor.Meter (Meter(..))

-- the small ADTs Result and Verdict are fully strict in their fields,
-- so WHNF == NF. orphan instance keeps the library API untouched.
instance NFData C.Result where rnf !_ = ()

-- A constant-returning meter. Still runs the wrapped action so any
-- per-call cost gets charged, but the reading itself is fixed -- this
-- isolates framework driver overhead from real meter / target work and
-- guarantees a deterministic Pass path (both classes report identical
-- readings, so the e-process wealth stays flat and the run goes to
-- budget).
constMeter :: Meter
constMeter = Meter $ \k act -> do
  let go 0 = pure ()
      go i = act >> go (i - 1)
  go k
  pure 100
{-# NOINLINE constMeter #-}

-- noop hypothesis: samplers produce (), target does nothing. Combined
-- with constMeter, this exercises only the runCT driver loop and the
-- ppad-eproc per-step update.
noopHyp :: C.Hypothesis ()
noopHyp = C.Hypothesis
  { C.target  = \_ -> pure ()
  , C.prepare = pure ()
  , C.sampleA = pure ()
  , C.sampleB = pure ()
  }

main :: IO ()
main = defaultMain [
    runCT_budget
  , runCT_batch
  ]

-- runCT driver overhead vs budget. With a noop target + constMeter,
-- the per-pair cost is (sampleA + sampleB + 2 * meter-overhead +
-- EP.update + EP.decide). Scaling should be linear in budget after
-- warmup is amortised.
runCT_budget :: Benchmark
runCT_budget =
  let cfg n = C.defaultConfig
        { C.cfgBudget = n
        , C.cfgWarmup = 50
        , C.cfgBatch  = 1
        }
  in  bgroup "runCT (noop target, const meter, batch=1)" [
          bench "budget=100"    $ nfIO (C.runCT constMeter (cfg    100) noopHyp)
        , bench "budget=1000"   $ nfIO (C.runCT constMeter (cfg   1000) noopHyp)
        , bench "budget=10000"  $ nfIO (C.runCT constMeter (cfg  10000) noopHyp)
        , bench "budget=100000" $ nfIO (C.runCT constMeter (cfg 100000) noopHyp)
        ]

-- runCT driver overhead vs batch size. Total target invocations is
-- budget * batch; for a noop target the relative driver vs runOne cost
-- should be visible here.
runCT_batch :: Benchmark
runCT_batch =
  let cfg b = C.defaultConfig
        { C.cfgBudget = 1000
        , C.cfgWarmup = 50
        , C.cfgBatch  = b
        }
  in  bgroup "runCT (noop target, const meter, budget=1000)" [
          bench "batch=1"    $ nfIO (C.runCT constMeter (cfg    1) noopHyp)
        , bench "batch=10"   $ nfIO (C.runCT constMeter (cfg   10) noopHyp)
        , bench "batch=100"  $ nfIO (C.runCT constMeter (cfg  100) noopHyp)
        , bench "batch=1000" $ nfIO (C.runCT constMeter (cfg 1000) noopHyp)
        ]
