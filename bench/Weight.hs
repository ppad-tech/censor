{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq (NFData(..))
import qualified Censor as C
import Censor.Meter (Meter(..))
import Weigh

instance NFData C.Result where rnf !_ = ()

constMeter :: Meter
constMeter = Meter $ \k act -> do
  let go 0 = pure ()
      go i = act >> go (i - 1)
  go k
  pure 100
{-# NOINLINE constMeter #-}

noopHyp :: C.Hypothesis ()
noopHyp = C.Hypothesis
  { C.target  = \_ -> pure ()
  , C.prepare = pure ()
  , C.sampleA = pure ()
  , C.sampleB = pure ()
  }

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith $ do
  runCT_budget
  runCT_batch

-- per-pair allocation should be constant: a budget-scaled run that
-- allocates O(budget) bytes per pair would surface as super-linear
-- growth across the rows.
runCT_budget :: Weigh ()
runCT_budget =
  let cfg n = C.defaultConfig
        { C.cfgBudget = n
        , C.cfgWarmup = 50
        , C.cfgBatch  = 1
        }
  in  wgroup "runCT (noop target, const meter, batch=1)" $ do
        io "budget=100"    (C.runCT constMeter (cfg    100)) noopHyp
        io "budget=1000"   (C.runCT constMeter (cfg   1000)) noopHyp
        io "budget=10000"  (C.runCT constMeter (cfg  10000)) noopHyp
        io "budget=100000" (C.runCT constMeter (cfg 100000)) noopHyp

runCT_batch :: Weigh ()
runCT_batch =
  let cfg b = C.defaultConfig
        { C.cfgBudget = 1000
        , C.cfgWarmup = 50
        , C.cfgBatch  = b
        }
  in  wgroup "runCT (noop target, const meter, budget=1000)" $ do
        io "batch=1"    (C.runCT constMeter (cfg    1)) noopHyp
        io "batch=10"   (C.runCT constMeter (cfg   10)) noopHyp
        io "batch=100"  (C.runCT constMeter (cfg  100)) noopHyp
        io "batch=1000" (C.runCT constMeter (cfg 1000)) noopHyp
