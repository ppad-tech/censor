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
  , configValidationTests
  , deterministicLeakTests
  , offByOneTests
  , clipCountTests
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
        C.Pass _ _ _ -> pure ()
        C.Reject n w _ -> assertFailure $
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
        C.Reject _ _ _ -> pure ()
        C.Pass n w _ -> assertFailure $
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
        C.Pass n _ _ -> n @?= C.cfgBudget c
        C.Reject n w _ -> assertFailure $
          "flat meter rejected at n=" ++ show n ++ " logW=" ++ show w

  , testCase "Pass count is invariant under cfgBatch" $ do
      let mkCfg b = C.defaultConfig
            { C.cfgBudget = 100
            , C.cfgWarmup = 10
            , C.cfgBatch  = b
            }
          collect r = case r of
            C.Pass n _ _   -> n
            C.Reject n _ _ -> n
      ns <- mapM (\b -> collect <$>
                          C.runCT (constMeter 100) (mkCfg b) noopHyp)
                 [1, 10, 100, 1000]
      ns @?= replicate 4 100

  , testCase "no clipping under a flat meter" $ do
      let c = C.defaultConfig
            { C.cfgBudget = 200
            , C.cfgWarmup = 20
            , C.cfgBatch  = 1
            }
      r <- C.runCT (constMeter 100) c noopHyp
      case r of
        C.Pass _ _ nc -> nc @?= 0
        C.Reject _ _ _ -> assertFailure "flat meter unexpectedly rejected"
  ]

calibrationTests :: TestTree
calibrationTests = testGroup "calibration" [
    testCase "zero-duration warmup throws WarmupZeroDuration" $ do
      let c = C.defaultConfig { C.cfgWarmup = 5, C.cfgBudget = 10 }
      r <- try (C.runCT (constMeter 0) c noopHyp)
             :: IO (Either C.CensorError C.Result)
      case r of
        Left C.WarmupZeroDuration -> pure ()
        Left other -> assertFailure $
          "expected WarmupZeroDuration, got " ++ show other
        Right _ -> assertFailure "expected WarmupZeroDuration"
  ]

configValidationTests :: TestTree
configValidationTests = testGroup "config validation" [
    testCase "cfgBatch = 0 is rejected" $
      assertInvalidConfig
        (C.defaultConfig { C.cfgBatch = 0 })

  , testCase "cfgBatch < 0 is rejected" $
      assertInvalidConfig
        (C.defaultConfig { C.cfgBatch = -1 })

  , testCase "cfgWarmup = 0 is rejected" $
      assertInvalidConfig
        (C.defaultConfig { C.cfgWarmup = 0 })

  , testCase "cfgWarmup < 0 is rejected" $
      assertInvalidConfig
        (C.defaultConfig { C.cfgWarmup = -1 })

  , testCase "cfgBudget < 0 is rejected" $
      assertInvalidConfig
        (C.defaultConfig { C.cfgBudget = -1 })
  ]
  where
    assertInvalidConfig c = do
      r <- try (C.runCT (constMeter 100) c noopHyp)
             :: IO (Either C.CensorError C.Result)
      case r of
        Left (C.InvalidConfig _) -> pure ()
        Left other -> assertFailure $
          "expected InvalidConfig, got " ++ show other
        Right _ -> assertFailure "expected InvalidConfig"

-- deterministic A/B divergence via a meter that reports state the
-- target wrote during the timed region. No wall-clock noise: every
-- pair lands the same delta into the wealth process and rejection is
-- guaranteed.
leakHyp :: IO (C.Hypothesis Word64, C.Meter)
leakHyp = do
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
  pure (hyp, leakMeter)

leakCfg :: C.Config
leakCfg = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgBudget = 500
  , C.cfgWarmup = 20
  , C.cfgBatch  = 1
  }

deterministicLeakTests :: TestTree
deterministicLeakTests = testGroup "deterministic leak" [
    testCase "deterministic A/B divergence is rejected" $ do
      (hyp, meter) <- leakHyp
      r <- C.runCT meter leakCfg hyp
      case r of
        C.Reject _ _ _ -> pure ()
        C.Pass n w _   -> assertFailure $
          "deterministic divergence not detected; passed n="
          ++ show n ++ " logW=" ++ show w
  ]

-- regression for the off-by-one: a threshold crossing on the final
-- allowed pair must surface as Reject, not Pass.
offByOneTests :: TestTree
offByOneTests = testGroup "budget/decide off-by-one" [
    testCase "rejection observed on the final allowed pair" $ do
      -- 1. find the rejection-pair count under a generous budget.
      (hyp1, meter1) <- leakHyp
      k <- do
        r <- C.runCT meter1 leakCfg hyp1
        case r of
          C.Reject n _ _ -> pure n
          C.Pass _ _ _   -> assertFailure
            "leak fixture failed to reject under generous budget"
            >> pure 0  -- unreachable
      -- 2. re-run with cfgBudget = k. Without the fix the run would
      --    pass at n = k. With the fix the rejection is reported.
      (hyp2, meter2) <- leakHyp
      let tight = leakCfg { C.cfgBudget = k }
      r2 <- C.runCT meter2 tight hyp2
      case r2 of
        C.Reject n _ _ -> n @?= k
        C.Pass n w _   -> assertFailure $
          "off-by-one regression: tight budget passed at n="
          ++ show n ++ " logW=" ++ show w
  ]

-- meter that returns small readings during the warmup window and
-- large readings thereafter. With cfgWarmup = 10 the meter sees
-- 10 * 2 = 20 calls before the main loop starts; the bound becomes
-- 2 * p99(small) and every main-loop reading clips.
clipCountTests :: TestTree
clipCountTests = testGroup "clip counts" [
    testCase "clip count rises when main-loop readings exceed bound" $ do
      ctr <- newIORef (0 :: Int)
      let meter = C.Meter $ \k act ->
            let go 0 = pure ()
                go i = act >> go (i - 1)
            in do
              go k
              n <- atomicModifyIORef' ctr $ \i -> (i + 1, i)
              pure $ if n < 20 then 100 else 1000
          c = C.defaultConfig
            { C.cfgAlpha  = 1.0e-3
            , C.cfgBudget = 50
            , C.cfgWarmup = 10  -- 10 pairs * 2 classes = 20 meter calls
            , C.cfgBatch  = 1
            }
      r <- C.runCT meter c noopHyp
      let nc = case r of
            C.Pass _ _ x   -> x
            C.Reject _ _ x -> x
      assertBool ("expected nonzero clip count, got " ++ show nc)
                 (nc > 0)
  ]
