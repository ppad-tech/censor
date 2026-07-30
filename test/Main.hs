{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (try)
import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word8, Word64)
import Data.List (isInfixOf, isPrefixOf)
import qualified Censor as C
import qualified Censor.FFI as F
import qualified Censor.Runner as R
import qualified Censor.Runner.Env as E
import qualified Censor.Runner.Report as Rep
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import qualified System.Info as Info
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "ppad-censor" [
    wallClockTests
  , flatMeterTests
  , calibrationTests
  , batchCalibrationTests
  , configValidationTests
  , deterministicLeakTests
  , runCTWithTests
  , evidenceTests
  , offByOneTests
  , clipCountTests
  , aaDiagnosticTests
  , attributeTests
  , attributedTests
  , signOnlyLeakTests
  , fixVsRandomTests
  , fixVsRandomCtxTests
  , rngTests
  , randomBytesTests
  , fillRandomTests
  , smokeCalibrationTests
  , diagnoseTests
  , renderingTests
  , calibReportTests
  , envTests
  , reportJSONTests
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
            , C.prepare = pure ()
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
            , C.prepare = pure ()
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
  , C.prepare = pure ()
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
    -- wealth process leaves every peak at its calibrated floor: a
    -- log e-value of 0, for components and mixture alike.
  , testCase "flat meter report: unit bound, floor peaks" $ do
      let c = C.defaultConfig
            { C.cfgBudget = 200
            , C.cfgWarmup = 20
            , C.cfgBatch  = 1
            }
      r <- C.runCT (constMeter 100) c noopHyp
      C.resBound r @?= 1
      assertBool "mixture peak below floor"
        (C.resPeakLogW r >= negate 1e-9)
      let !minPeak = min (min (C.resPeakSign r) (C.resPeakMagn r))
                         (min (C.resPeakMagn4 r) (C.resPeakMagn16 r))
      assertBool "component peak below floor"
        (minPeak >= negate 1e-9)
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

-- a meter whose reading scales linearly with the batch: reads
-- @k * perCall@. Lets calibrateBatch resolve a per-call cost, which a
-- flat 'constMeter' (reading independent of @k@) cannot express.
linMeter :: Word64 -> C.Meter
linMeter perCall = C.Meter $ \k act ->
  let go 0 = pure ()
      go i = act >> go (i - 1)
  in  go k >> pure (fromIntegral k * perCall)

batchCalibrationTests :: TestTree
batchCalibrationTests = testGroup "batch calibration" [
    testCase "nanosecond-scale target picks a large batch" $ do
      -- reading = 64 * batch; the target magnitude (65536) is reached
      -- at batch 1024, which refines to itself.
      b <- C.calibrateBatch (linMeter 64) noopHyp
      b @?= 1024

  , testCase "heavy target picks batch 1" $ do
      -- a single call already exceeds the target magnitude.
      b <- C.calibrateBatch (linMeter 100000) noopHyp
      b @?= 1

  , testCase "unresolvable meter picks batch 1" $ do
      -- a meter stuck at zero never reaches the target magnitude;
      -- calibration gives up at the cap and returns 1, so the
      -- subsequent run fails fast in warmup (WarmupZeroDuration).
      b <- C.calibrateBatch (linMeter 0) noopHyp
      b @?= 1
  ]

runCTWithTests :: TestTree
runCTWithTests = testGroup "runCTWith / Frame" [
    testCase "one frame per pair on a passing run" $ do
      let c = C.defaultConfig
            { C.cfgBudget = 300, C.cfgWarmup = 20, C.cfgBatch = 1 }
      ref <- newIORef []
      r   <- C.runCTWith (constMeter 100) c noopHyp $ \ !fr ->
               modifyIORef' ref (fr :)
      frs <- fmap reverse (readIORef ref)
      length frs @?= C.resPairs r
      map C.frPair frs @?= [1 .. C.resPairs r]

  , testCase "one frame per pair on a rejecting run" $ do
      (hyp, meter) <- leakHyp
      ref <- newIORef (0 :: Int)
      r   <- C.runCTWith meter leakCfg hyp $ \_ ->
               modifyIORef' ref (+ 1)
      n <- readIORef ref
      case r of
        C.Reject{} -> n @?= C.resPairs r
        C.Pass{}   -> assertFailure "leak fixture did not reject"

  , testCase "runCT agrees with runCTWith on a leak (pinned seed)" $ do
      (h1, m1) <- leakHyp
      (h2, m2) <- leakHyp
      let c = leakCfg { C.cfgSeed = Just 12345 }
      r1 <- C.runCT m1 c h1
      r2 <- C.runCTWith m2 c h2 (\_ -> pure ())
      C.resPairs r1 @?= C.resPairs r2
  ]

attributedTests :: TestTree
attributedTests = testGroup "runCTAttributed" [
    testCase "pass yields no attribution" $ do
      let c = C.defaultConfig
            { C.cfgBudget = 200, C.cfgWarmup = 20, C.cfgBatch = 1 }
      (r, ma) <- C.runCTAttributed (constMeter 100) c noopHyp
      case (r, ma) of
        (C.Pass{}, Nothing) -> pure ()
        _                   -> assertFailure "expected (Pass, Nothing)"

  , testCase "reject runs controls, and they pass" $ do
      (hyp, meter) <- leakHyp
      (r, ma) <- C.runCTAttributed meter leakCfg hyp
      case (r, ma) of
        (C.Reject{}, Just att)
          | C.attVerdict att == C.ControlsPass -> pure ()
        (C.Reject{}, Just other)               -> assertFailure $
          "expected ControlsPass, got " ++ show (C.attVerdict other)
        _ -> assertFailure "leak fixture did not reject"
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
        , C.prepare = pure ()
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

-- resPValue is the mixture evidence recast as an anytime-valid
-- p-value: at or below alpha iff the verdict is Reject. resEffect
-- is an anytime-valid confidence interval for the mean clipped
-- difference: it covers the true mean at the stopping time (0 for
-- CT fixtures, the deterministic delta for the leak fixture).
-- Exclusion of 0 at a rejection stopping time is deliberately NOT
-- asserted: detection is more sample-efficient than estimation, so
-- a fast reject legitimately halts while the interval still
-- straddles zero.
evidenceTests :: TestTree
evidenceTests = testGroup "calibrated evidence" [
    testCase "leak fixture: p <= alpha, effect covers true delta" $ do
      (hyp, meter) <- leakHyp
      let c = leakCfg { C.cfgSeed = Just 0xE71DE0 }
      r <- C.runCT meter c hyp
      case r of
        C.Reject{} -> do
          assertBool "Reject but resPValue > alpha"
            (C.resPValue r <= C.cfgAlpha c)
          -- the fixture's difference is d = -100, every pair.
          let (lo, hi) = C.resEffect r
          assertBool "interval misses the true mean -100"
            (lo <= -100 && -100 <= hi)
        C.Pass{} -> assertFailure "leak fixture failed to reject"

  , testCase "flat fixture: p = 1, effect covers zero" $ do
      let c = C.defaultConfig
            { C.cfgBudget = 500
            , C.cfgWarmup = 20
            , C.cfgBatch  = 1
            }
      r <- C.runCT (constMeter 100) c noopHyp
      case r of
        C.Pass{} -> do
          C.resPValue r @?= 1
          let (lo, hi) = C.resEffect r
          assertBool "interval misses zero" (lo <= 0 && 0 <= hi)
        C.Reject{} -> assertFailure "flat meter unexpectedly rejected"
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
            , C.prepare = pure ()
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
    testCase "symmetric harness: neither control convicts" $ do
      (hyp, meter) <- leakHyp
      let c = leakCfg { C.cfgSeed = Just 0xABAD1DEA }
      att <- C.attribute meter c hyp
      case C.attVerdict att of
        C.ControlsPass -> pure ()
        other -> assertFailure $
          "expected ControlsPass, got " ++ show other

  , testCase "controls carry both full runs, not just a label" $ do
      (hyp, meter) <- leakHyp
      let c = leakCfg { C.cfgSeed = Just 0xABAD1DEA }
      att <- C.attribute meter c hyp
      -- both control runs must be present and non-degenerate, so a
      -- reader can weigh how much evidence backs ControlsPass.
      assertBool "A/A control consumed no pairs"
        (C.resPairs (C.attAA att) > 0)
      assertBool "B/B control consumed no pairs"
        (C.resPairs (C.attBB att) > 0)
  ]

-- fixVsRandom pins the first secret draw as class A's input and
-- feeds fresh draws to class B. fixture: the target writes its
-- input to an IORef and the meter reads it back, so a sampler whose
-- first draw differs from all later draws leaks deterministically,
-- while the A/A collapse of the same hypothesis is null by
-- construction.
-- The shared per-pair public context must be exactly that: one
-- draw, visible identically to both classes, refreshed every pair.
-- The context sampler here is a counter, and the completion returns
-- the context itself, so each sampler's return value *is* what it
-- saw.
fixVsRandomCtxTests :: TestTree
fixVsRandomCtxTests = testGroup "fixVsRandomCtx" [
    testCase "one context per pair, shared by both classes" $ do
      ctr  <- newIORef (0 :: Int)
      seen <- newIORef ([] :: [(Char, Int)])
      base <- C.fixVsRandomCtx
        (\_ -> pure ())
        (pure ())
        (atomicModifyIORef' ctr (\n -> (n + 1, n)))
        (\c _ -> pure c)
      let note t act = do
            c <- act
            modifyIORef' seen ((t, c) :)
            pure c
          hyp = base
            { C.sampleA = note 'A' (C.sampleA base)
            , C.sampleB = note 'B' (C.sampleB base)
            }
          cfg = C.defaultConfig
            { C.cfgWarmup = 5, C.cfgBudget = 5, C.cfgBatch = 1 }
      _  <- C.runCT (constMeter 100) cfg hyp
      xs <- fmap reverse (readIORef seen)
      let pairs = chunk2 xs
      assertBool "expected 10 pairs of samples" (length pairs == 10)
      mapM_ checkPair pairs
      -- Contexts advance by exactly one per pair: each pair drew a
      -- fresh one, and drew it only once. They start at 1, not 0:
      -- the constructor's seeding draw takes 0 and 'prepare'
      -- overwrites it before the first pair, so that draw is never
      -- observed by either class.
      let ctxs = map (snd . fst) pairs
      ctxs @?= take 10 [1 ..]
  ]
  where
    chunk2 (a : b : rest) = (a, b) : chunk2 rest
    chunk2 _              = []
    checkPair ((t1, c1), (t2, c2)) = do
      assertBool ("both halves of a pair ran the same class: " ++ [t1, t2])
        (t1 /= t2)
      assertEqual "classes saw different contexts in one pair" c1 c2

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

randomBytesTests :: TestTree
randomBytesTests = testGroup "Censor.Rng.randomBytes" [
    testCase "lengths, including ragged, empty, and negative" $ do
      g <- C.mkRng 0xABCD
      mapM_
        (\n -> do
            bs <- C.randomBytes g n
            BS.length bs @?= max 0 n)
        [-1, 0, 1, 7, 8, 13, 32]

  , testCase "deterministic under a fixed seed" $ do
      g1 <- C.mkRng 0xABCD
      g2 <- C.mkRng 0xABCD
      b1 <- C.randomBytes g1 33
      b2 <- C.randomBytes g2 33
      b1 @?= b2
      assertBool "expected nonzero bytes" (BS.any (/= 0) b1)

  , testCase "agrees with fillRandom on the same stream" $ do
      let n = 13
      g1 <- C.mkRng 0xABCD
      bs <- C.randomBytes g1 n
      g2 <- C.mkRng 0xABCD
      ws <- allocaBytes n $ \p -> do
        F.fillRandom g2 p n
        peekArray n p
      BS.unpack bs @?= ws

  , testCase "a shorter draw is a prefix of a longer one" $ do
      g1 <- C.mkRng 0xABCD
      g2 <- C.mkRng 0xABCD
      short <- C.randomBytes g1 13
      long  <- C.randomBytes g2 32
      short @?= BS.take 13 long
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
        , C.prepare = pure ()
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
    -- an H_0 fixture, alpha = 0.1, N = 200, loose bound: a true
    -- 0.1-FPR test would produce ~20 +/- ~10 rejections; a broken
    -- driver flips this to 100+ rejections. The 0.30 bound is a
    -- regression tripwire, not a real calibration check.
    testCase "sym-uniform H_0 stays under 30%" $
      assertCalibrated symUniformTrial

    -- the mirror-image tripwire: a variance-only alternative must
    -- be detected, not tolerated. This is the channel that a null
    -- of "d is symmetric" cannot see and exchangeability can.
  , testCase "asym-uniform-equal-mean H_1 detected above 90%" $
      assertDetected asymUniformTrial
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
    -- equal means, different variances. Under exchangeability this
    -- is a genuine alternative: d stays symmetric (so sign and
    -- magnitude see nothing), but the class CDFs differ, which the
    -- indicator components detect.
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
    assertDetected trial = do
      let n = 200
      rejects <- countRejects n trial
      let rate = fromIntegral rejects / (fromIntegral n :: Double)
      assertBool ("variance-only detection regressed: " ++ show rejects
                  ++ "/" ++ show n ++ " rejections (rate "
                  ++ show rate ++ ")")
                 (rate >= 0.90)

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
            , C.prepare = pure ()
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

-- deterministic constant-mean bulk shift, seeded for reproducibility.
-- Reused as a "reliably-rejects" fixture across diagnose / rendering
-- tests below.
runLeak :: IO C.Result
runLeak = do
  (hyp, meter) <- leakHyp
  let c = leakCfg { C.cfgSeed = Just 0xB0BFACE }
  C.runCT meter c hyp

-- deterministic Pass fixture with a well-behaved effect interval.
runFlatPass :: IO C.Result
runFlatPass = do
  let c = C.defaultConfig
        { C.cfgBudget = 500, C.cfgWarmup = 20, C.cfgBatch = 1 }
  C.runCT (constMeter 100) c noopHyp

isRejDriver :: C.Advisory -> Bool
isRejDriver (C.RejectionDriver _) = True
isRejDriver _                     = False

isHighClipRate :: C.Advisory -> Bool
isHighClipRate (C.HighClipRate _) = True
isHighClipRate _                  = False

diagnoseTests :: TestTree
diagnoseTests = testGroup "diagnose" [
    testCase "equal-mean shape leak -> a shape driver, not bulk" $ do
      ref <- newIORef (0 :: Word64)
      ctr <- newIORef (0 :: Int)
      let leakMeter = C.Meter $ \k act ->
            let go 0 = pure ()
                go i = act >> go (i - 1)
            in  go k >> readIORef ref
          hyp = C.Hypothesis
            { C.target  = writeIORef ref
            , C.prepare = pure ()
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
        C.Reject{} -> do
          let advs = C.diagnose r
              -- class A averages 100, as class B always does, so no
              -- mean shift exists for the magnitude channel to find.
              -- The leak lives in the shape, which both the sign and
              -- the CDF-indicator channels can see; which of the two
              -- dominates depends on where the pooled cut points
              -- land, so accept either.
              shapeDriven = C.RejectionDriver C.ShapeLeak `elem` advs
                         || C.RejectionDriver C.CdfShift  `elem` advs
          assertBool ("expected a shape driver in " ++ show advs)
            shapeDriven
          assertBool "sign component saw no evidence at all"
            (C.resPeakSign r > 0)
        C.Pass{} -> assertFailure "shape leak fixture did not reject"

  , testCase "bulk leak -> some RejectionDriver" $ do
      r <- runLeak
      case r of
        C.Reject{} ->
          let advs = C.diagnose r
          in  assertBool
                ("expected some RejectionDriver in " ++ show advs)
                (any isRejDriver advs)
        C.Pass{} -> assertFailure "bulk leak did not reject"

  , testCase "flat Pass -> no RejectionDriver" $ do
      r <- runFlatPass
      case r of
        C.Pass{} -> do
          let advs = C.diagnose r
          assertBool ("unexpected RejectionDriver on Pass: " ++ show advs)
            (not (any isRejDriver advs))
        C.Reject{} -> assertFailure "flat meter unexpectedly rejected"

  , testCase "heavy clipping -> HighClipRate advisory" $ do
      ref <- newIORef (0 :: Word64)
      ctr <- newIORef (0 :: Int)
      let leakMeter = C.Meter $ \k act ->
            let go 0 = pure ()
                go i = act >> go (i - 1)
            in  go k >> readIORef ref
          warmupPairs = 20
          hyp = C.Hypothesis
            { C.target  = writeIORef ref
            , C.prepare = pure ()
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
      let advs = C.diagnose r
      assertBool ("expected HighClipRate in " ++ show advs)
        (any isHighClipRate advs)
  ]

renderingTests :: TestTree
renderingTests = testGroup "summary / resultToJSON" [
    testCase "summary on Pass starts with PASS" $ do
      r <- runFlatPass
      let s = R.summary r
      assertBool ("expected PASS prefix, got: " ++ s)
        ("PASS" `isPrefixOf` s)

  , testCase "summary on Reject starts with REJECT and names driver" $ do
      r <- runLeak
      let s = R.summary r
      assertBool ("expected REJECT prefix, got: " ++ s)
        ("REJECT" `isPrefixOf` s)
      assertBool ("expected driver= in: " ++ s)
        ("driver=" `isInfixOf` s)

  , testCase "resultToJSON on Pass exposes expected keys" $ do
      r <- runFlatPass
      let j = R.resultToJSON r
      mapM_ (\k -> assertBool ("expected " ++ k ++ " in " ++ j)
                              (k `isInfixOf` j))
        [ "\"verdict\":\"PASS\""
        , "\"pairs\":"
        , "\"peakLogW\":"
        , "\"pvalue\":"
        , "\"effect\":"
        , "\"clipped\":"
        , "\"bound\":"
        , "\"peaks\":"
        , "\"sign\":"
        , "\"magn\":"
        , "\"magn4\":"
        , "\"magn16\":"
        , "\"advisories\":"
        ]

  , testCase "resultToJSON on Reject reports the driver in advisories" $ do
      r <- runLeak
      let j = R.resultToJSON r
      assertBool ("expected verdict REJECT in " ++ j)
        ("\"verdict\":\"REJECT\"" `isInfixOf` j)
      assertBool ("expected RejectionDriver in " ++ j)
        ("RejectionDriver" `isInfixOf` j)
  ]

calibReportTests :: TestTree
calibReportTests = testGroup "calibrateBatchReport" [
    testCase "resolves a nanosecond target and records probes" $ do
      let m = linMeter 64
      rep <- C.calibrateBatchReport m noopHyp
      -- same batch as calibrateBatch's convenience wrapper.
      b <- C.calibrateBatch m noopHyp
      C.crBatch rep @?= b
      C.crBatch rep @?= 1024
      C.crCapHit rep @?= False
      -- probes grow geometrically (x4) from 1 until the reading
      -- reaches calibTarget (65536); at 64 per call that lands
      -- exactly at batch 1024.
      map fst (C.crProbes rep) @?= [1, 4, 16, 64, 256, 1024]

  , testCase "records the noise floor at the selected batch" $ do
      rep <- C.calibrateBatchReport (linMeter 64) noopHyp
      -- linMeter is deterministic (reading = batch * 64), so the
      -- median at the selected batch 1024 is exactly the calibration
      -- target and the IQR is zero.
      C.crNoise rep @?= Just (C.Noise 65536 0)

  , testCase "unresolvable meter -> capHit True, batch 1, no noise" $ do
      rep <- C.calibrateBatchReport (linMeter 0) noopHyp
      C.crBatch rep @?= 1
      C.crCapHit rep @?= True
      C.crNoise rep @?= Nothing
  ]

envTests :: TestTree
envTests = testGroup "captureEnv" [
    testCase "captures platform identity" $ do
      e <- E.captureEnv
      E.envOs e @?= Info.os
      E.envArch e @?= Info.arch
      assertBool "cores >= 1" (E.envCores e >= 1)
      assertBool "kernel present" (E.envKernel e /= Nothing)
      assertBool "time present" (E.envTime e /= Nothing)

  , testCase "load averages are nonnegative when present" $ do
      e <- E.captureEnv
      case E.envLoad e of
        Nothing            -> pure ()
        Just (l1, l5, l15) ->
          assertBool "loads >= 0" (l1 >= 0 && l5 >= 0 && l15 >= 0)
  ]

reportJSONTests :: TestTree
reportJSONTests = testGroup "reportJSON" [
    testCase "surfaces env and per-case noise" $ do
      r   <- C.runCT (constMeter 100) testConfig noopHyp
      env <- E.captureEnv
      let hdr = Rep.ReportHeader
            { Rep.rhMeter     = "wall"
            , Rep.rhConfig    = testConfig
            , Rep.rhSeed      = Nothing
            , Rep.rhTarget    = Just "test"
            , Rep.rhNote      = Nothing
            , Rep.rhEnv       = Just env
            , Rep.rhPerCaseBatch = False
            }
          cr = Rep.CaseReport
            { Rep.crName        = "case"
            , Rep.crResult      = r
            , Rep.crAttribution = Nothing
            , Rep.crBatch       = Just 3
            , Rep.crNoise       = Just (C.Noise 100 5)
            , Rep.crTrace       = Nothing
            }
          j = Rep.reportJSON hdr [cr]
      assertBool ("expected env block in " ++ j)
        ("\"env\":{\"os\":" `isInfixOf` j)
      assertBool ("expected cores in " ++ j)
        ("\"cores\":" `isInfixOf` j)
      assertBool ("expected noise in " ++ j)
        ("\"noise\":{\"med\":100,\"iqr\":5}" `isInfixOf` j)

  , testCase "omits env and noise when absent" $ do
      r <- C.runCT (constMeter 100) testConfig noopHyp
      let hdr = Rep.ReportHeader
            { Rep.rhMeter     = "wall"
            , Rep.rhConfig    = testConfig
            , Rep.rhSeed      = Nothing
            , Rep.rhTarget    = Nothing
            , Rep.rhNote      = Nothing
            , Rep.rhEnv       = Nothing
            , Rep.rhPerCaseBatch = False
            }
          cr = Rep.CaseReport "case" r Nothing Nothing Nothing Nothing
          j  = Rep.reportJSON hdr [cr]
      assertBool ("unexpected env block in " ++ j)
        (not ("\"env\":" `isInfixOf` j))
      assertBool ("unexpected noise in " ++ j)
        (not ("\"noise\":" `isInfixOf` j))
  ]
