{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (try)
import Control.Monad (replicateM_)
import Data.IORef
import Data.Word (Word64)
import qualified Censor as C
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "ppad-censor" [
    wallClockTests
  , flatMeterTests
  , calibrationTests
  , deterministicLeakTests
  ]

-- Test config tuned for fast feedback: less strict alpha and small
-- budget, since both fixtures produce strong signals (CT: identical
-- work, leaky: 20x work imbalance).
testConfig :: C.Config
testConfig = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgBudget = 5000
  , C.cfgWarmup = 100
  , C.cfgBatch  = 1
  }

-- a meter that runs the wrapped action k times and reports a fixed
-- reading. Both classes look identical, so the wealth process never
-- grows and runCT exhausts its budget deterministically.
constMeter :: Word64 -> C.Meter
constMeter v = C.Meter $ \k act ->
  let go 0 = pure ()
      go i = act >> go (i - 1)
  in  go k >> pure v

wallClockTests :: TestTree
wallClockTests = testGroup "wall-clock" [
    testCase "constant-time target passes within budget" $ do
      ref <- newIORef (0 :: Int)
      let hyp = C.Hypothesis
            { C.target  = \_ ->
                replicateM_ 1000 (modifyIORef' ref (+ 1))
            , C.sampleA = pure (2000 :: Int)
            , C.sampleB = pure (100  :: Int)
            }
      result <- C.runCT C.wallClock testConfig hyp
      case result of
        C.Pass _ _ -> pure ()
        C.Reject n w -> assertFailure $
          "CT target falsely rejected at n=" ++ show n
          ++ " logW=" ++ show w

  , testCase "leaky target rejected within budget" $ do
      ref <- newIORef (0 :: Int)
      let hyp = C.Hypothesis
            { C.target  = \n ->
                replicateM_ n (modifyIORef' ref (+ 1))
            , C.sampleA = pure (2000 :: Int)
            , C.sampleB = pure (100  :: Int)
            }
      result <- C.runCT C.wallClock testConfig hyp
      case result of
        C.Reject _ _ -> pure ()
        C.Pass n w -> assertFailure $
          "leaky target not detected; passed n=" ++ show n
          ++ " logW=" ++ show w
  ]

noopHyp :: C.Hypothesis ()
noopHyp = C.Hypothesis
  { C.target  = \_ -> pure ()
  , C.sampleA = pure ()
  , C.sampleB = pure ()
  }

flatMeterTests :: TestTree
flatMeterTests = testGroup "flat meter" [
    testCase "constant reading exhausts budget exactly" $ do
      let c = C.defaultConfig
            { C.cfgBudget = 500
            , C.cfgWarmup = 20
            , C.cfgBatch  = 1
            }
      r <- C.runCT (constMeter 100) c noopHyp
      case r of
        C.Pass n _ -> n @?= C.cfgBudget c
        C.Reject n w -> assertFailure $
          "flat meter rejected at n=" ++ show n ++ " logW=" ++ show w

  , testCase "Pass count is invariant under cfgBatch" $ do
      let mkCfg b = C.defaultConfig
            { C.cfgBudget = 100
            , C.cfgWarmup = 10
            , C.cfgBatch  = b
            }
          collect r = case r of
            C.Pass n _   -> n
            C.Reject n _ -> n
      ns <- mapM (\b -> collect <$>
                          C.runCT (constMeter 100) (mkCfg b) noopHyp)
                 [1, 10, 100, 1000]
      ns @?= replicate 4 100
  ]

calibrationTests :: TestTree
calibrationTests = testGroup "calibration" [
    testCase "zero-duration warmup throws WarmupZeroDuration" $ do
      let c = C.defaultConfig { C.cfgWarmup = 5, C.cfgBudget = 10 }
      r <- try (C.runCT (constMeter 0) c noopHyp)
             :: IO (Either C.CensorError C.Result)
      case r of
        Left C.WarmupZeroDuration   -> pure ()
        Left (C.InvalidEprocConfig e) -> assertFailure $
          "expected WarmupZeroDuration, got InvalidEprocConfig "
          ++ show e
        Right _                     -> assertFailure
          "expected WarmupZeroDuration"
  ]

-- deterministic A/B divergence via a meter that reports state the
-- target wrote during the timed region. No wall-clock noise: every
-- pair lands the same delta into the wealth process and rejection is
-- guaranteed.
deterministicLeakTests :: TestTree
deterministicLeakTests = testGroup "deterministic leak" [
    testCase "deterministic A/B divergence is rejected" $ do
      ref <- newIORef (0 :: Word64)
      let leakMeter = C.Meter $ \k act ->
            let go 0 = pure ()
                go i = act >> go (i - 1)
            in  go k >> readIORef ref
          hyp = C.Hypothesis
            { C.target  = \n -> writeIORef ref n
            , C.sampleA = pure 100
            , C.sampleB = pure 200
            }
          c = C.defaultConfig
            { C.cfgAlpha  = 1.0e-3
            , C.cfgBudget = 500
            , C.cfgWarmup = 20
            , C.cfgBatch  = 1
            }
      r <- C.runCT leakMeter c hyp
      case r of
        C.Reject _ _ -> pure ()
        C.Pass n w   -> assertFailure $
          "deterministic divergence not detected; passed n="
          ++ show n ++ " logW=" ++ show w
  ]
