{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (try)
import Control.Monad (replicateM_)
import Data.IORef
import Data.Word (Word8, Word64)
import qualified Censor as C
import qualified Censor.FFI as F
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
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
  , aaDiagnosticTests
  , attributeTests
  , signOnlyLeakTests
  , fixVsRandomTests
  , rngTests
  , fillRandomTests
  , smokeCalibrationTests
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
        C.Pass{} -> pure ()
        C.Reject { C.resPairs = n, C.resPeakLogW = w } ->
          assertFailure $
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
        C.Reject{} -> pure ()
        C.Pass { C.resPairs = n, C.resPeakLogW = w } ->
          assertFailure $
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
        C.Pass { C.resPairs = n } -> n @?= C.cfgBudget c
        C.Reject { C.resPairs = n, C.resPeakLogW = w } ->
          assertFailure $
            "flat meter rejected at n=" ++ show n ++ " logW=" ++ show w

  , testCase "Pass count is invariant under cfgBatch" $ do
      let mkCfg b = C.defaultConfig
            { C.cfgBudget = 100
            , C.cfgWarmup = 10
            , C.cfgBatch  = b
            }
      ns <- mapM (\b -> C.resPairs <$>
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
        C.Pass { C.resClipped = nc } -> nc @?= 0
        C.Reject{} -> assertFailure "flat meter unexpectedly rejected"

    -- zero warmup differences floor the clip bound at 1, and a flat
    -- wealth process leaves every peak at its starting value: log 2
    -- per component, log 8 for the mixture.
  , testCase "flat meter report: unit bound, floor peaks" $ do
      let c = C.defaultConfig
            { C.cfgBudget = 200
            , C.cfgWarmup = 20
            , C.cfgBatch  = 1
            }
      r <- C.runCT (constMeter 100) c noopHyp
      C.resBound r @?= 1
      assertBool "mixture peak below floor"
        (C.resPeakLogW r >= log 8 - 1e-9)
      let !minPeak = min (min (C.resPeakSign r) (C.resPeakMagn r))
                         (min (C.resPeakMagn4 r) (C.resPeakMagn16 r))
      assertBool "component peak below floor"
        (minPeak >= log 2 - 1e-9)
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
        C.Reject{} -> pure ()
        C.Pass { C.resPairs = n, C.resPeakLogW = w } ->
          assertFailure $
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
          C.Reject { C.resPairs = n } -> pure n
          C.Pass{} -> assertFailure
            "leak fixture failed to reject under generous budget"
            >> pure 0  -- unreachable
      -- 2. re-run with cfgBudget = k. Without the fix the run would
      --    pass at n = k. With the fix the rejection is reported.
      (hyp2, meter2) <- leakHyp
      let tight = leakCfg { C.cfgBudget = k }
      r2 <- C.runCT meter2 tight hyp2
      case r2 of
        C.Reject { C.resPairs = n } -> n @?= k
        C.Pass { C.resPairs = n, C.resPeakLogW = w } ->
          assertFailure $
            "off-by-one regression: tight budget passed at n="
            ++ show n ++ " logW=" ++ show w
  ]

-- An equal-mean shape leak that /only/ the sign component of the
-- hedge can detect. Class A returns 97 with probability 1/4 and 101
-- with probability 3/4 (so E[ta] = 100), class B returns 100
-- constantly. Then d = ta - tb has mean 0 (magnitude components
-- can't grow wealth) but P(d > 0) = 3/4 (sign component grows fast).
-- Guards the sign-component wiring per-commit; the equivalent
-- coverage in censor-validate takes an hour.
signOnlyLeakTests :: TestTree
signOnlyLeakTests = testGroup "sign-only leak" [
    testCase "equal-mean shape leak is rejected by the hedge" $ do
      ref <- newIORef (0 :: Word64)
      ctr <- newIORef (0 :: Int)
      let leakMeter = C.Meter $ \k act ->
            let go 0 = pure ()
                go i = act >> go (i - 1)
            in  go k >> readIORef ref
          hyp = C.Hypothesis
            { C.target  = writeIORef ref
            , C.sampleA = do
                n <- atomicModifyIORef' ctr (\i -> (i + 1, i))
                pure $! if n `mod` 4 == 0 then 97 else 101
            , C.sampleB = pure 100
            }
          c = C.defaultConfig
            { C.cfgAlpha  = 1.0e-3
            , C.cfgBudget = 5000
            , C.cfgWarmup = 100
            , C.cfgBatch  = 1
            , C.cfgSeed   = Just 0xC0FFEE
            }
      r <- C.runCT leakMeter c hyp
      case r of
        -- a shape leak grows the sign component, not the magnitude
        -- ones; the per-component peaks should reflect that.
        C.Reject{} -> assertBool
          "sign component did not dominate the rejection"
          (C.resPeakSign r > C.resPeakMagn r)
        C.Pass { C.resPairs = n, C.resPeakLogW = w } ->
          assertFailure $
            "shape leak not detected; passed n=" ++ show n
            ++ " logW=" ++ show w
  ]

-- toAA / toBB collapse a leaky hypothesis into H_0 by construction —
-- either sampleB is replaced by sampleA or vice versa, so both
-- classes measure the same distribution. A hypothesis that Rejects
-- under runCT should Pass under both runAA and runBB.
aaDiagnosticTests :: TestTree
aaDiagnosticTests = testGroup "A/A and B/B diagnostics" [
    testCase "runAA passes a hypothesis that runCT would reject" $ do
      (hyp, meter) <- leakHyp
      -- Fixed seed to keep the test deterministic; the leak fixture
      -- itself is deterministic in outcome regardless of order.
      let c = leakCfg { C.cfgSeed = Just 0xABAD1DEA }
      r <- C.runAA meter c hyp
      case r of
        C.Pass{} -> pure ()
        C.Reject { C.resPairs = n, C.resPeakLogW = w } ->
          assertFailure $
            "A/A diagnostic falsely rejected at n=" ++ show n
            ++ " logW=" ++ show w

  , testCase "runBB passes a hypothesis that runCT would reject" $ do
      (hyp, meter) <- leakHyp
      let c = leakCfg { C.cfgSeed = Just 0xABAD1DEA }
      r <- C.runBB meter c hyp
      case r of
        C.Pass{} -> pure ()
        C.Reject { C.resPairs = n, C.resPeakLogW = w } ->
          assertFailure $
            "B/B diagnostic falsely rejected at n=" ++ show n
            ++ " logW=" ++ show w
  ]

-- attribute composes runAA and runBB: the leak fixture has
-- class-constant samplers, so both diagnostics are flat and the
-- rejection attributes to the target.
attributeTests :: TestTree
attributeTests = testGroup "attribute" [
    testCase "symmetric-harness leak attributes to the target" $ do
      (hyp, meter) <- leakHyp
      let c = leakCfg { C.cfgSeed = Just 0xABAD1DEA }
      att <- C.attribute meter c hyp
      case att of
        C.TargetLeak -> pure ()
        other -> assertFailure $
          "expected TargetLeak, got " ++ show other
  ]

-- fixVsRandom pins the first secret draw as class A's input and
-- feeds fresh draws to class B. fixture: the target writes its
-- input to an IORef and the meter reads it back, so a sampler whose
-- first draw differs from all later draws leaks deterministically,
-- while the A/A collapse of the same hypothesis is null by
-- construction.
fixVsRandomTests :: TestTree
fixVsRandomTests = testGroup "fixVsRandom" [
    testCase "pinned-vs-random divergence is rejected" $ do
      (hyp, meter) <- pinnedHyp
      r <- C.runCT meter leakCfg hyp
      case r of
        C.Reject{} -> pure ()
        C.Pass { C.resPairs = n } -> assertFailure $
          "pinned divergence not detected; passed n=" ++ show n

  , testCase "A/A collapse of the pinned hypothesis passes" $ do
      (hyp, meter) <- pinnedHyp
      let c = leakCfg { C.cfgSeed = Just 0xABAD1DEA }
      r <- C.runAA meter c hyp
      case r of
        C.Pass{} -> pure ()
        C.Reject { C.resPairs = n } -> assertFailure $
          "A/A collapse falsely rejected at n=" ++ show n

  , testCase "constant secret sampler passes at budget" $ do
      ref <- newIORef (0 :: Word64)
      let meter = C.Meter $ \k act ->
            let go 0 = pure ()
                go i = act >> go (i - 1)
            in  go k >> readIORef ref
      hyp <- C.fixVsRandom (writeIORef ref) (pure (100 :: Word64)) pure
      r <- C.runCT meter leakCfg hyp
      case r of
        C.Pass { C.resPairs = n } -> n @?= C.cfgBudget leakCfg
        C.Reject { C.resPairs = n } -> assertFailure $
          "constant sampler rejected at n=" ++ show n
  ]
  where
    -- first draw 100 (the pinned secret), all later draws 200.
    pinnedHyp = do
      ref <- newIORef (0 :: Word64)
      ctr <- newIORef (0 :: Int)
      let meter = C.Meter $ \k act ->
            let go 0 = pure ()
                go i = act >> go (i - 1)
            in  go k >> readIORef ref
          sec = do
            n <- atomicModifyIORef' ctr (\i -> (i + 1, i))
            pure $! if n == 0 then 100 else 200 :: Word64
      hyp <- C.fixVsRandom (writeIORef ref) sec pure
      pure (hyp, meter)

rngTests :: TestTree
rngTests = testGroup "Censor.Rng" [
    testCase "equal seeds give equal streams" $ do
      g1 <- C.mkRng 0xFEEDFACE
      g2 <- C.mkRng 0xFEEDFACE
      ws1 <- mapM (\_ -> C.nextWord g1) [1 .. 16 :: Int]
      ws2 <- mapM (\_ -> C.nextWord g2) [1 .. 16 :: Int]
      ws1 @?= ws2

  , testCase "distinct seeds give distinct streams" $ do
      g1 <- C.mkRng 1
      g2 <- C.mkRng 2
      w1 <- C.nextWord g1
      w2 <- C.nextWord g2
      assertBool "streams coincide" (w1 /= w2)
  ]

fillRandomTests :: TestTree
fillRandomTests = testGroup "Censor.FFI.fillRandom" [
    testCase "deterministic under a fixed seed, handles ragged n" $ do
      let n = 13  -- deliberately not a multiple of 8
      bs1 <- fillWith 0xABCD n
      bs2 <- fillWith 0xABCD n
      bs1 @?= bs2
      assertBool "expected nonzero bytes" (any (/= 0) bs1)
  ]
  where
    fillWith :: Word64 -> Int -> IO [Word8]
    fillWith seed n = do
      g <- C.mkRng seed
      allocaBytes n $ \p -> do
        F.fillRandom g p n
        peekArray n p

-- a tripwire calibration tier: run runCT a few hundred times under
-- a true-H_0 synthetic meter and assert the empirical rejection rate
-- stays well below a generous bound. Catches gross wiring regressions
-- (rejection rate exploding to 0.5+) but does *not* validate the
-- alpha guarantee in any rigorous sense — that's what censor-validate
-- is for. N is deliberately small so the suite stays fast.
data Class = ClassA | ClassB

mkSynthMeter
  :: IO Word64 -> IO Word64 -> IO (C.Meter, C.Hypothesis Class)
mkSynthMeter nextA nextB = do
  ref <- newIORef ClassA
  let meter = C.Meter $ \k act -> do
        let go 0 = pure ()
            go i = act >> go (i - 1)
        go k
        c <- readIORef ref
        case c of
          ClassA -> nextA
          ClassB -> nextB
      hyp = C.Hypothesis
        { C.target  = writeIORef ref
        , C.sampleA = pure ClassA
        , C.sampleB = pure ClassB
        }
  pure (meter, hyp)

uniformInRange :: C.Rng -> Word64 -> Word64 -> IO Word64
uniformInRange g lo hi = do
  w <- C.nextWord g
  pure $! lo + (w `mod` (hi - lo + 1))

countRejects :: Int -> IO C.Result -> IO Int
countRejects = go 0
  where
    go !acc 0  _   = pure acc
    go !acc !n act = do
      r <- act
      let !inc = case r of
            C.Reject{} -> 1
            C.Pass{}   -> 0
      go (acc + inc) (n - 1) act

smokeCalibrationTests :: TestTree
smokeCalibrationTests = testGroup "smoke calibration" [
    -- two H_0 fixtures, alpha = 0.1, N = 200, loose bound: a true
    -- 0.1-FPR test would produce ~20 +/- ~10 rejections; a broken
    -- driver flips this to 100+ rejections. The 0.30 bound is a
    -- regression tripwire, not a real calibration check.
    testCase "sym-uniform H_0 stays under 30%" $
      assertCalibrated symUniformTrial

  , testCase "asym-uniform-equal-mean H_0 stays under 30%" $
      assertCalibrated asymUniformTrial
  ]
  where
    cfg = C.defaultConfig
      { C.cfgAlpha  = 0.1
      , C.cfgBudget = 1000
      , C.cfgWarmup = 50
      , C.cfgBatch  = 1
      }
    symUniformTrial = do
      ra <- C.mkRng 0x5EED01
      rb <- C.mkRng 0x5EED02
      (m, h) <- mkSynthMeter (uniformInRange ra 0 200)
                             (uniformInRange rb 0 200)
      C.runCT m cfg h
    asymUniformTrial = do
      ra <- C.mkRng 0x5EED03
      rb <- C.mkRng 0x5EED04
      (m, h) <- mkSynthMeter (uniformInRange ra 0 200)
                             (uniformInRange rb 50 150)
      C.runCT m cfg h
    assertCalibrated trial = do
      let n = 200
      rejects <- countRejects n trial
      let rate = fromIntegral rejects / (fromIntegral n :: Double)
      assertBool ("smoke calibration regressed: " ++ show rejects
                  ++ "/" ++ show n ++ " rejections (rate "
                  ++ show rate ++ ")")
                 (rate <= 0.30)

-- clip count rises when |ta - tb| exceeds the warmup-derived bound.
-- fixture: target writes its input to an IORef, meter reads it;
-- class A always writes 100, class B writes 200 during warmup (|d|
-- = 100, so bound = 2 * 100 = 200) and 1000000 during the main loop
-- (|d| ~= 999900, well above the bound).
clipCountTests :: TestTree
clipCountTests = testGroup "clip counts" [
    testCase "clip count rises when |d| exceeds bound" $ do
      ref <- newIORef (0 :: Word64)
      ctr <- newIORef (0 :: Int)
      let leakMeter = C.Meter $ \k act ->
            let go 0 = pure ()
                go i = act >> go (i - 1)
            in  go k >> readIORef ref
          warmupPairs = 20
          hyp = C.Hypothesis
            { C.target  = writeIORef ref
            , C.sampleA = pure 100
            , C.sampleB = do
                n <- atomicModifyIORef' ctr (\i -> (i + 1, i))
                pure $! if n < warmupPairs
                          then 200
                          else 1000000
            }
          c = C.defaultConfig
            { C.cfgAlpha  = 1.0e-3
            , C.cfgBudget = 50
            , C.cfgWarmup = warmupPairs
            , C.cfgBatch  = 1
            }
      r <- C.runCT leakMeter c hyp
      -- every warmup |d| is exactly 100, so the bound is pinned at
      -- 2 * p99(|d|) = 200.
      C.resBound r @?= 200
      assertBool ("expected nonzero clip count, got "
                  ++ show (C.resClipped r))
                 (C.resClipped r > 0)
  ]
