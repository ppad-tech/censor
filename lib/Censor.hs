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
-- null is wrong; see "Numeric.Eproc.Bounded" for the details). The
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

    -- * A/A diagnostic
  , toAA
  , runAA

    -- * Measurement (re-exports)
  , Meter(..)
  , wallClock
  , Counter(..)
  , MeterError(..)
  , withCounter
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import qualified Data.Bits as B
import Data.List (sort)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Numeric.Eproc.Bounded as EP
import Numeric.Eproc.Bounded (Bettor(Newton), ConfigError)

import Censor.Meter

-- | A constant-time hypothesis.
--
--   * @target@: the @IO@ action under test, parameterised on input.
--   * @sampleA@, @sampleB@: independent samplers for two input
--     classes (typically a \"fixed\" class and a \"random\" class).
--
--   The null asserts that the paired difference of measurements of
--   @target a@ and @target b@ has conditional mean zero — informally,
--   that the two classes run for equally long on average. The test
--   is sensitive to first-moment timing leaks (data-dependent
--   branches and memory traffic typically show up here) but cannot
--   distinguish distributions that agree in mean and differ only in
--   higher moments.
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
  , cfgWarmup :: !Int     -- ^ warmup pairs. The clip bound is set
                          --   from the @~p99@ of @cfgWarmup@ per-pair
                          --   absolute differences @|d|@; at small
                          --   warmup sizes the order-statistic
                          --   discretisation degenerates to the
                          --   sample maximum (specifically, any
                          --   @cfgWarmup <= 100@ picks the max).
  , cfgSeed   :: !(Maybe Word64)
                          -- ^ seed for the per-pair A\/B order bit.
                          --   'Nothing' (the default) draws fresh
                          --   entropy per run, so the conditional
                          --   independence of the order bit from
                          --   any periodic ambient noise (timer
                          --   interrupts, scheduler quanta) is a
                          --   fact rather than an assumption.
                          --   @Just s@ pins the seed for
                          --   reproducibility.
  } deriving Show

-- | Defaults: @alpha = 1e-6@, budget @100000@ pairs, batch 1 (no
--   inner repetition), warmup 200 pairs, order-bit seed drawn from
--   entropy per run.
defaultConfig :: Config
defaultConfig = Config
  { cfgAlpha  = 1.0e-6
  , cfgBudget = 100000
  , cfgBatch  = 1
  , cfgWarmup = 200
  , cfgSeed   = Nothing
  }

-- | Test outcome.
--
--   Both constructors carry the number of sample pairs consumed, the
--   peak (supremum-so-far) log-wealth of the underlying e-process,
--   and a clip count: the number of pairs whose absolute difference
--   @|d|@ exceeded the warmup-derived upper bound and were truncated
--   before being fed to the test. A clip count that is a substantial
--   fraction of @samples@ indicates the warmup bound was too tight;
--   raise 'cfgWarmup', enlarge 'cfgBatch', or treat the run with
--   suspicion (clipping reduces power against tail-shifting leaks).
data Result
  = Reject
      {-# UNPACK #-} !Int     -- ^ sample pairs consumed
      {-# UNPACK #-} !Double  -- ^ peak log-wealth
      {-# UNPACK #-} !Int     -- ^ pairs whose @|d|@ was clipped
    -- ^ leak detected.
  | Pass
      {-# UNPACK #-} !Int     -- ^ sample pairs consumed
      {-# UNPACK #-} !Double  -- ^ peak log-wealth
      {-# UNPACK #-} !Int     -- ^ pairs whose @|d|@ was clipped
    -- ^ no leak detected within budget.
  deriving Show

-- | Errors thrown by 'runCT' when calibration or configuration fails.
data CensorError =
    WarmupZeroDuration
    -- ^ warmup observed only zero-duration measurements: the meter
    --   cannot resolve the target under the current 'cfgBatch'.
    --   Raise the batch size so the target runs in an inner loop.
    --   (Zero /differences/ across all warmup pairs are OK — that
    --   just means the target is deterministic in @|d|@ for the
    --   sampled inputs, which is the expected happy path under a
    --   PMU meter on truly CT code.)
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
--   1. /Warmup/ — take @cfgWarmup@ paired measurements to estimate
--      an upper bound @c@ on the magnitude of the per-pair difference
--      @|d| = |ta - tb|@. The subsequent e-process bets on @d@
--      clipped to @[-c, c]@.
--   2. /Main/ — for each pair, choose A-first or B-first uniformly
--      at random, sample and measure both classes, compute
--      @d = ta - tb@, clip to @[-c, c]@, and stream to the e-process
--      test. Halt as soon as the wealth process crosses threshold,
--      or when the budget is exhausted.
--
--   Because the order bit is randomised per pair, @d@ is
--   conditionally symmetric about zero under H_0 (not just conditional
--   mean zero); symmetric clipping is odd and preserves the null.
--   Betting on @d@ directly — rather than on the raw per-class
--   readings — lets @lambda_max@ scale with the noise magnitude
--   @p99(|d|)@ rather than with the target's baseline runtime
--   @p99(t)@, which for narrow-noise heavy-baseline targets can be
--   orders of magnitude better for power at the same alpha.
runCT :: Meter -> Config -> Hypothesis a -> IO Result
runCT meter cfg hyp = do
  validateConfig cfg
  (!wSeed, !mSeed) <- resolveSeeds (cfgSeed cfg)
  (!c, !anyNonzero) <- warmup meter cfg hyp wSeed
  unless anyNonzero $ throwIO WarmupZeroDuration
  !epCfg <- case EP.config 0 (negate c) c (cfgAlpha cfg) Newton of
    Left  e -> throwIO (InvalidEprocConfig e)
    Right x -> pure x
  let go !n !st !rng !nclip
        | n >= cfgBudget cfg =
            pure $! Pass n (EP.log_wealth st) nclip
        | otherwise = do
            let (!rng', !flipOrder) = nextOrderBit rng
            (!ta, !tb) <- runPair meter cfg hyp flipOrder
            let !d      = fromIntegral ta - fromIntegral tb :: Double
                !da     = abs d
                !dClip  = if da > c then signum d * c else d
                !clipped = da > c
                !st'    = EP.update epCfg st dClip
                !nclip' = if clipped then nclip + 1 else nclip
                !n'     = n + 1
            case EP.decide epCfg st' of
              EP.Reject ->
                pure $! Reject n' (EP.log_wealth st') nclip'
              EP.Continue ->
                go n' st' rng' nclip'
  go 0 (EP.initial epCfg) mSeed 0

-- | Convert a 'Hypothesis' into its /A\/A diagnostic/ form: 'sampleB'
--   is replaced by 'sampleA', so the driver measures the same class
--   twice per pair.
--
--   Under a correctly-wired harness the A\/A hypothesis is by
--   construction H_0 (identical distributions), so 'runCT' should
--   always return 'Pass'. A 'Reject' verdict on 'toAA' indicts the
--   /harness/ — asymmetric allocation between the samplers,
--   time-of-day drift picked up by an unbalanced sample order, RTS
--   state leaking between calls — rather than a leak in the target.
--
--   The typical usage is to run this before trusting a 'Reject' from
--   a new hypothesis: if the A\/A form also rejects, the harness
--   itself is at fault and the real-hypothesis result is not
--   informative until the asymmetry is fixed.
toAA :: Hypothesis a -> Hypothesis a
toAA h = h { sampleB = sampleA h }
{-# INLINE toAA #-}

-- | Run the A\/A diagnostic. Equivalent to @'runCT' meter cfg
--   ('toAA' hyp)@; should always 'Pass' if the harness is
--   symmetric.
runAA :: Meter -> Config -> Hypothesis a -> IO Result
runAA meter cfg hyp = runCT meter cfg (toAA hyp)

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

-- The order-bit PRNG. Deterministic splitmix64 from a resolved
-- seed. The conditional-null guarantee needs the per-pair order bit
-- to be independent of the target's timing distribution; the seed
-- itself only needs to be independent of that distribution too,
-- which fresh entropy (default) or a user-supplied constant both
-- satisfy.

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

-- Resolve the (warmup, main) seed pair. From a user-supplied
-- constant we splitmix once to derive each slot; from entropy
-- (Nothing) we take the current monotonic time and splitmix twice
-- to decorrelate low-order bits from wall clock resolution.
resolveSeeds :: Maybe Word64 -> IO (Word64, Word64)
resolveSeeds mseed = do
  !base <- case mseed of
    Just s  -> pure s
    Nothing -> getMonotonicTimeNSec
  let (!s1, !w1) = splitMix base
      (_ , !w2) = splitMix s1
  pure (w1, w2)

-- Warmup collects per-pair @|d|@ (for the bound estimate) and a
-- boolean flag tracking whether any nonzero reading was ever
-- observed (for the meter-broken diagnosis). The order-bit
-- randomisation matches runCT's main loop so the p99 estimate
-- doesn't inherit a parity-locked nuisance bias.
--
-- Returns @(c, anyNonzero)@ where @c = max 1 (2 * p99(|d|))@ is the
-- clipping bound and @anyNonzero@ is True iff at least one warmup
-- reading was nonzero. If @anyNonzero@ is False the meter is broken
-- (returning 0 for everything) and the caller throws
-- 'WarmupZeroDuration'. All-zero /differences/ with nonzero readings
-- are the PMU-on-truly-CT happy path — we fall back to @c = 1@ and
-- let the main loop proceed; under continued zero differences the
-- wealth process stays flat and the run passes at budget.
warmup :: Meter -> Config -> Hypothesis a -> Word64 -> IO (Double, Bool)
warmup meter cfg hyp seed = do
  let go !i !rng !acc !nz
        | i >= cfgWarmup cfg = pure (acc, nz)
        | otherwise = do
            let (!rng', !flipOrder) = nextOrderBit rng
            (!x, !y) <- runPair meter cfg hyp flipOrder
            let !dx = fromIntegral x :: Double
                !dy = fromIntegral y :: Double
                !da = abs (dx - dy)
                !nz' = nz || x /= 0 || y /= 0
            go (i + 1) rng' (da : acc) nz'
  (diffs, anyNonzero) <- go 0 seed [] False
  let !sorted = sort diffs
      !m      = length sorted
      !idx    = max 0 (min (m - 1) ((m * 99) `div` 100))
      !p99    = case drop idx sorted of
                  (x : _) -> x
                  []      -> 0
  -- "p99" is fuzzy: at default cfgWarmup = 200 (m = 200) idx = 198,
  -- so we take the 199th of 200 differences (empirical rank ~99.5%).
  -- At cfgWarmup <= 100 (m <= 100) idx lands at m - 1, so p99
  -- degenerates to the sample max — surfaced in the cfgWarmup
  -- docstring.
  --
  -- 2x the order statistic gives headroom for occasional outliers;
  -- observations above are clipped during the main pass. The
  -- @max 1@ floor keeps @c > 0@ so the underlying e-process config
  -- accepts it even when warmup differences are identically zero
  -- (PMU on truly CT code).
  pure (max 1 (2 * p99), anyNonzero)
