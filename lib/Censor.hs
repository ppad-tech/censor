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

    -- * Running
  , runCT

    -- * Measurement (re-exports)
  , Meter(..)
  , wallClock
  , Counter(..)
  , MeterError(..)
  , withCounter
  ) where

import Control.Monad (replicateM)
import Data.List (sort)
import Data.Word (Word64)
import qualified Numeric.Eproc.Paired as EP
import Numeric.Eproc.Paired (Bettor(Ons))

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
data Result
  = Reject {-# UNPACK #-} !Int {-# UNPACK #-} !Double
    -- ^ leak detected: sample pairs consumed, final log-wealth
  | Pass   {-# UNPACK #-} !Int {-# UNPACK #-} !Double
    -- ^ no leak detected within budget
  deriving Show

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
    0 -> error
      "Censor.runCT: warmup yielded zero-duration measurements. \
      \Increase Config.cfgBatch so the target runs in inner loops."
    _ -> pure ()
  let !lo    = 0 :: Double
      !hi    = fromIntegral bound :: Double
      !epCfg = EP.config lo hi (cfgAlpha cfg) Ons
      go !n !st
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
                    !x  <- runOne meter (cfgBatch cfg) (target hyp a)
                    !b  <- sampleB hyp
                    !y  <- runOne meter (cfgBatch cfg) (target hyp b)
                    pure (x, y)
                  else do
                    !b  <- sampleB hyp
                    !y  <- runOne meter (cfgBatch cfg) (target hyp b)
                    !a  <- sampleA hyp
                    !x  <- runOne meter (cfgBatch cfg) (target hyp a)
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

runOne :: Meter -> Int -> IO () -> IO Word64
runOne (Meter m) !k act = m (rep k)
  where
    rep 0 = pure ()
    rep i = act >> rep (i - 1)
{-# INLINE runOne #-}

warmup :: Meter -> Config -> Hypothesis a -> IO Word64
warmup meter Config{..} hyp = do
  tas <- replicateM cfgWarmup $ do
    !a <- sampleA hyp
    runOne meter cfgBatch (target hyp a)
  tbs <- replicateM cfgWarmup $ do
    !b <- sampleB hyp
    runOne meter cfgBatch (target hyp b)
  let !sorted = sort (tas ++ tbs)
      !m      = length sorted
      !idx    = max 0 (min (m - 1) ((m * 99) `div` 100))
      !p99    = case drop idx sorted of
                  (x : _) -> x
                  []      -> 0
  -- 2x the 99th percentile gives generous headroom for occasional
  -- outliers; observations above are clipped during the main pass.
  pure $! 2 * p99
