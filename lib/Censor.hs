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
-- 'runCT' measures paired class-A / class-B timings, streams them
-- through an anytime-valid two-sample test, and either rejects (a
-- leak is detected) or exhausts its budget.
--
-- The test is valid under optional stopping: 'runCT' halts as soon
-- as the wealth process crosses the threshold @2 \/ alpha@, with
-- type-I error controlled at @alpha@ regardless of when sampling
-- stops.

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
--   The hypothesis asserts that the measurement distribution of
--   @target a@ matches that of @target b@ when @a@ is drawn from
--   @sampleA@ and @b@ from @sampleB@.
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
  , cfgWarmup :: !Int     -- ^ warmup samples per class
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
--   The reported log-wealth is the supremum observed over the run,
--   not the value at the stopping time.
data Result
  = Reject {-# UNPACK #-} !Int {-# UNPACK #-} !Double
    -- ^ leak detected: sample pairs consumed, peak log-wealth
  | Pass   {-# UNPACK #-} !Int {-# UNPACK #-} !Double
    -- ^ no leak detected within budget
  deriving Show

-- | Errors thrown by 'runCT' when calibration fails.
data CensorError =
    WarmupZeroDuration
    -- ^ warmup observed only zero-duration measurements: the meter
    --   cannot resolve the target under the current 'cfgBatch'.
    --   Raise the batch size so the target runs in an inner loop.
  | InvalidEprocConfig !ConfigError
    -- ^ the underlying e-process rejected the test configuration
    --   derived from 'cfgAlpha' and the warmup-estimated bound.
  deriving (Eq, Show)

instance Exception CensorError

-- | Run a constant-time test.
--
--   Phases:
--
--   1. /Warmup/ — take @cfgWarmup@ measurements from each class to
--      estimate an upper bound on per-call timings.
--   2. /Main/ — alternate (sample, measure) for the two classes,
--      streaming pairs to the e-process test. Halt as soon as the
--      wealth process crosses threshold, or when the budget is
--      exhausted.
runCT :: Meter -> Config -> Hypothesis a -> IO Result
runCT meter cfg hyp = do
  !bound <- warmup meter cfg hyp
  case bound of
    0 -> throwIO WarmupZeroDuration
    _ -> pure ()
  let !lo = 0 :: Double
      !hi = fromIntegral bound :: Double
  !epCfg <- case EP.config lo hi (cfgAlpha cfg) Newton of
    Left  e -> throwIO (InvalidEprocConfig e)
    Right c -> pure c
  let go !n !st
        | n >= cfgBudget cfg =
            pure $! Pass n (EP.log_wealth st)
        | otherwise = case EP.decide epCfg st of
            EP.Reject -> pure $! Reject n (EP.log_wealth st)
            EP.Continue -> do
              -- alternate which class is measured first to cancel
              -- order-induced bias (cache warmth etc.).
              (!ta, !tb) <-
                if even n
                  then do
                    !a  <- sampleA hyp
                    !x  <- measure meter (cfgBatch cfg) (target hyp a)
                    !b  <- sampleB hyp
                    !y  <- measure meter (cfgBatch cfg) (target hyp b)
                    pure (x, y)
                  else do
                    !b  <- sampleB hyp
                    !y  <- measure meter (cfgBatch cfg) (target hyp b)
                    !a  <- sampleA hyp
                    !x  <- measure meter (cfgBatch cfg) (target hyp a)
                    pure (x, y)
              let !tac = clip bound ta
                  !tbc = clip bound tb
                  !st' = EP.update epCfg st
                    ( fromIntegral tac
                    , fromIntegral tbc
                    )
              go (n + 1) st'
  go 0 (EP.initial epCfg)

-- internal

clip :: Word64 -> Word64 -> Word64
clip !lim !x = if x > lim then lim else x
{-# INLINE clip #-}

-- alternates A/B order per pair, matching runCT's main loop, so the
-- p99 estimate doesn't inherit order-induced drift from collecting one
-- class then the other.
warmup :: Meter -> Config -> Hypothesis a -> IO Word64
warmup meter Config{..} hyp = do
  let go !i !acc
        | i >= cfgWarmup = pure acc
        | even i = do
            !a <- sampleA hyp
            !x <- measure meter cfgBatch (target hyp a)
            !b <- sampleB hyp
            !y <- measure meter cfgBatch (target hyp b)
            go (i + 1) (x : y : acc)
        | otherwise = do
            !b <- sampleB hyp
            !y <- measure meter cfgBatch (target hyp b)
            !a <- sampleA hyp
            !x <- measure meter cfgBatch (target hyp a)
            go (i + 1) (x : y : acc)
  ts <- go 0 []
  let !sorted = sort ts
      !m      = length sorted
      !idx    = max 0 (min (m - 1) ((m * 99) `div` 100))
      !p99    = case drop idx sorted of
                  (x : _) -> x
                  []      -> 0
  -- 2x the 99th percentile gives generous headroom for occasional
  -- outliers; observations above are clipped during the main pass.
  pure $! 2 * p99
