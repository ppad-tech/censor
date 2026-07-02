{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Censor
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Sequential constant-time testing.
--
-- Declare a 'Hypothesis' (an @IO@ action plus two input samplers);
-- 'runCT' measures paired class-A \/ class-B timings, streams them
-- through an anytime-valid two-sample test, and either rejects (a
-- leak is detected) or exhausts its budget.
--
-- The wealth process is a nonnegative supermartingale under the null
-- (a notional bankroll that bets on the data and grows only when the
-- null is wrong; see "Numeric.Eproc.Paired" for the details). The
-- test is valid under optional stopping: 'runCT' halts as soon as
-- the wealth process crosses the threshold @2 \/ alpha@, with type-I
-- error controlled at @alpha@ regardless of when sampling stops.

module Censor (
    -- * Hypothesis
    Hypothesis(..)

    -- * Configuration
  , Config(..)
  , defaultConfig

    -- * Result
  , Result(..)

    -- * Errors
  , CensorError(..)

    -- * Running
  , runCT

    -- * Measurement (re-exports)
  , Meter(..)
  , wallClock
  , Counter(..)
  , MeterError(..)
  , withCounter
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.Bits as B
import Data.List (sort)
import Data.Word (Word64)
import qualified Numeric.Eproc.Paired as EP
import Numeric.Eproc.Paired (Bettor(Newton), ConfigError)

import Censor.Meter

-- | A constant-time hypothesis.
--
--   * @target@: the @IO@ action under test, parameterised on input.
--   * @sampleA@, @sampleB@: independent samplers for two input
--     classes (typically a \"fixed\" class and a \"random\" class).
--
--   The null asserts that the paired difference of (clipped, batched)
--   measurements of @target a@ and @target b@ has conditional mean
--   zero — informally, that the two classes run for equally long on
--   average. The test is sensitive to first-moment timing leaks
--   (data-dependent branches and memory traffic typically show up
--   here) but cannot distinguish distributions that agree in mean
--   and differ only in higher moments.
data Hypothesis a = Hypothesis
  { target  :: !(a -> IO ())
  , sampleA :: !(IO a)
  , sampleB :: !(IO a)
  }

-- | Driver configuration.
data Config = Config
  { cfgAlpha  :: !Double  -- ^ significance level
  , cfgBudget :: !Int     -- ^ max sample pairs to consume
  , cfgBatch  :: !Int     -- ^ target repetitions per timed region
  , cfgWarmup :: !Int     -- ^ warmup samples per class. The clip
                          --   bound is set from the @~p99@ of
                          --   @2 * cfgWarmup@ readings; at small
                          --   warmup sizes the order-statistic
                          --   discretisation degenerates to the
                          --   sample maximum (specifically, any
                          --   @cfgWarmup <= 50@ picks the max).
  } deriving Show

-- | Defaults: @alpha = 1e-6@, budget @100000@ pairs, batch 1 (no
--   inner repetition), warmup 200 per class.
defaultConfig :: Config
defaultConfig = Config
  { cfgAlpha  = 1.0e-6
  , cfgBudget = 100000
  , cfgBatch  = 1
  , cfgWarmup = 200
  }

-- | Test outcome.
--
--   Both constructors carry the number of sample pairs consumed, the
--   peak (supremum-so-far) log-wealth of the underlying e-process,
--   and a clip count: the number of per-pair readings that exceeded
--   the warmup-derived upper bound and were truncated before being
--   fed to the test. A clip count that is a substantial fraction of
--   @2 * samples@ indicates the warmup bound was too tight; raise
--   'cfgWarmup', enlarge 'cfgBatch', or treat the run with
--   suspicion (clipping reduces power against tail-shifting leaks).
data Result
  = Reject
      {-# UNPACK #-} !Int     -- ^ sample pairs consumed
      {-# UNPACK #-} !Double  -- ^ peak log-wealth
      {-# UNPACK #-} !Int     -- ^ readings clipped to the bound
    -- ^ leak detected.
  | Pass
      {-# UNPACK #-} !Int     -- ^ sample pairs consumed
      {-# UNPACK #-} !Double  -- ^ peak log-wealth
      {-# UNPACK #-} !Int     -- ^ readings clipped to the bound
    -- ^ no leak detected within budget.
  deriving Show

-- | Errors thrown by 'runCT' when calibration or configuration fails.
data CensorError =
    WarmupZeroDuration
    -- ^ warmup observed only zero-duration measurements: the meter
    --   cannot resolve the target under the current 'cfgBatch'.
    --   Raise the batch size so the target runs in an inner loop.
  | InvalidConfig !String
    -- ^ a 'Config' field was outside its admissible range. The
    --   'String' names which field and why.
  | InvalidEprocConfig !ConfigError
    -- ^ the underlying e-process rejected the test configuration
    --   derived from 'cfgAlpha' and the warmup-estimated bound. In
    --   practice this is reachable only via a 'cfgAlpha' outside
    --   @(0, 1)@; adjust 'cfgAlpha' to the standard significance
    --   range.
  deriving (Eq, Show)

instance Exception CensorError

-- | Run a constant-time test.
--
--   Phases:
--
--   1. /Warmup/ — take @cfgWarmup@ measurements from each class to
--      estimate an upper bound on per-call timings.
--   2. /Main/ — for each pair, choose A-first or B-first uniformly
--      at random, sample and measure both classes, and stream the
--      pair to the e-process test. Halt as soon as the wealth
--      process crosses threshold, or when the budget is exhausted.
runCT :: Meter -> Config -> Hypothesis a -> IO Result
runCT meter cfg hyp = do
  validateConfig cfg
  !bound <- warmup meter cfg hyp
  case bound of
    0 -> throwIO WarmupZeroDuration
    _ -> pure ()
  let !lo = 0 :: Double
      !hi = fromIntegral bound :: Double
  !epCfg <- case EP.config lo hi (cfgAlpha cfg) Newton of
    Left  e -> throwIO (InvalidEprocConfig e)
    Right c -> pure c
  let go !n !st !rng !nclip
        | n >= cfgBudget cfg =
            pure $! Pass n (EP.log_wealth st) nclip
        | otherwise = do
            let (!rng', !flipOrder) = nextOrderBit rng
            (!ta, !tb) <- runPair meter cfg hyp flipOrder
            let !taClip = ta > bound
                !tbClip = tb > bound
                !tac    = clip bound ta
                !tbc    = clip bound tb
                !st'    = EP.update epCfg st
                            (fromIntegral tac, fromIntegral tbc)
                !nclip' = nclip
                          + (if taClip then 1 else 0)
                          + (if tbClip then 1 else 0)
                !n'     = n + 1
            case EP.decide epCfg st' of
              EP.Reject ->
                pure $! Reject n' (EP.log_wealth st') nclip'
              EP.Continue ->
                go n' st' rng' nclip'
  go 0 (EP.initial epCfg) mainSeed 0

-- internal

validateConfig :: Config -> IO ()
validateConfig Config{..}
  | cfgBatch  <= 0 = throwIO $! InvalidConfig
      "cfgBatch must be positive"
  | cfgWarmup <= 0 = throwIO $! InvalidConfig
      "cfgWarmup must be positive"
  | cfgBudget <  0 = throwIO $! InvalidConfig
      "cfgBudget must be nonnegative"
  | otherwise      = pure ()

-- | Run one (sampleA, measure, sampleB, measure) pair, in the chosen
--   order, and return (measurement-of-A, measurement-of-B). The
--   returned pair is independent of execution order.
runPair
  :: Meter -> Config -> Hypothesis a -> Bool -> IO (Word64, Word64)
runPair meter cfg hyp flipOrder
  | flipOrder = do
      !b <- sampleB hyp
      !y <- measure meter (cfgBatch cfg) (target hyp b)
      !a <- sampleA hyp
      !x <- measure meter (cfgBatch cfg) (target hyp a)
      pure (x, y)
  | otherwise = do
      !a <- sampleA hyp
      !x <- measure meter (cfgBatch cfg) (target hyp a)
      !b <- sampleB hyp
      !y <- measure meter (cfgBatch cfg) (target hyp b)
      pure (x, y)
{-# INLINE runPair #-}

clip :: Word64 -> Word64 -> Word64
clip !lim !x = if x > lim then lim else x
{-# INLINE clip #-}

-- The order-bit PRNG. A deterministic splitmix64 stream is fine for
-- the conditional-null guarantee: what matters is that the per-pair
-- order bit is conditionally independent of past measurements, and
-- a PRNG seeded with a fixed constant depends only on the call
-- counter, never on the observed timings.

splitMix :: Word64 -> (Word64, Word64)
splitMix !s =
  let !s' = s + 0x9E3779B97F4A7C15
      !z0 = (s' `B.xor` (s' `B.shiftR` 30)) * 0xBF58476D1CE4E5B9
      !z1 = (z0 `B.xor` (z0 `B.shiftR` 27)) * 0x94D049BB133111EB
      !z2 =  z1 `B.xor` (z1 `B.shiftR` 31)
  in  (s', z2)
{-# INLINE splitMix #-}

nextOrderBit :: Word64 -> (Word64, Bool)
nextOrderBit !s =
  let (!s', !w) = splitMix s
  in  (s', B.testBit w 0)
{-# INLINE nextOrderBit #-}

warmupSeed, mainSeed :: Word64
warmupSeed = 0xCAFEBABEDEADBEEF
mainSeed   = 0xDEADBEEFCAFEBABE

-- Warmup also randomises per-pair order, matching runCT's main loop
-- so the p99 estimate doesn't inherit a parity-locked nuisance bias.
warmup :: Meter -> Config -> Hypothesis a -> IO Word64
warmup meter cfg hyp = do
  let go !i !rng !acc
        | i >= cfgWarmup cfg = pure acc
        | otherwise = do
            let (!rng', !flipOrder) = nextOrderBit rng
            (!x, !y) <- runPair meter cfg hyp flipOrder
            go (i + 1) rng' (x : y : acc)
  ts <- go 0 warmupSeed []
  let !sorted = sort ts
      !m      = length sorted
      !idx    = max 0 (min (m - 1) ((m * 99) `div` 100))
      !p99    = case drop idx sorted of
                  (x : _) -> x
                  []      -> 0
  -- "p99" is fuzzy: at default cfgWarmup = 200 (m = 400) idx = 396,
  -- so we take the 397th of 400 readings (empirical rank ~99.25%).
  -- At cfgWarmup <= 50 (m <= 100) idx lands at m - 1, so p99
  -- degenerates to the sample max — surfaced in the cfgWarmup
  -- docstring.
  --
  -- 2x the order statistic gives headroom for occasional outliers;
  -- observations above are clipped during the main pass.
  pure $! 2 * p99
