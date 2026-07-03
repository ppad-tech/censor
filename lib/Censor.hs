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
-- Internally the test is a four-way convex-hedge mixture of
-- e-processes — a sign test at @p_0 = 1\/2@ on @sign d@ plus three
-- bounded-mean tests on @d@ clipped to progressively wider intervals
-- (see 'runCT' for the shape). Each component's wealth process is a
-- nonnegative supermartingale under the null (a notional bankroll
-- that bets on the data and grows only when the null is wrong; see
-- "Numeric.Eproc.Bounded" for the mechanics). The mixture itself is
-- an e-process by the same convex-combination argument, and the
-- test is valid under optional stopping: 'runCT' halts as soon as
-- the mixture's log-wealth supremum crosses @log(8 \/ alpha)@, with
-- type-I error controlled at @alpha@ regardless of when sampling
-- stops.

module Censor (
    -- * Hypothesis
    Hypothesis(..)
  , fixVsRandom

    -- * Configuration
  , Config(..)
  , defaultConfig

    -- * Result
  , Result(..)

    -- * Errors
  , CensorError(..)

    -- * Running
  , runCT

    -- * A\/A and B\/B diagnostics
  , toAA
  , runAA
  , toBB
  , runBB
  , Attribution(..)
  , attribute

    -- * Measurement (re-exports)
  , Meter(..)
  , wallClock
  , Counter(..)
  , MeterError(..)
  , withCounter

    -- * Sampler support (re-exports)
  , Rng
  , mkRng
  , nextWord
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import qualified Data.Bits as B
import Data.List (sort)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Numeric.Eproc.Bernoulli.TwoSided as EPSign
import qualified Numeric.Eproc.Bounded as EPMagn
import Numeric.Eproc.Bounded (Bettor(Newton), ConfigError)

import Censor.Meter
import Censor.Rng (Rng, mkRng, nextWord, splitMix)

-- | A constant-time hypothesis.
--
--   * @target@: the @IO@ action under test, parameterised on input.
--   * @sampleA@, @sampleB@: independent samplers for two input
--     classes (typically a \"fixed\" class and a \"random\" class).
--
--   The null asserts that the paired difference @d = ta - tb@ is
--   conditionally symmetric about zero — a strictly stronger
--   statement than conditional mean zero, and the one that the
--   randomised A\/B order actually delivers. The driver tests it
--   via a four-way convex-hedge mixture of anytime-valid
--   e-processes (a sign test at @p_0 = 1\/2@ plus three
--   bounded-mean tests on @d@ clipped to @c@, @4c@, and @16c@),
--   so the alternative class detected is the union: first-moment
--   shifts, equal-mean shape or tail leaks, and rare-outlier leaks
--   all move at least one component off the null.
data Hypothesis a = Hypothesis
  { target  :: !(a -> IO ())
  , sampleA :: !(IO a)
  , sampleB :: !(IO a)
  }

-- | Construct a dudect-style /fix-vs-random/ 'Hypothesis' from a
--   target, a secret sampler, and a completion function building a
--   full input around a secret.
--
--   The fixed secret is drawn once from the sampler at construction
--   time, so it is an ordinary heap value reached by the same code
--   path as the random class (never a shared top-level constant).
--   Class A draws and discards a fresh secret before returning the
--   fixed one, keeping the two samplers symmetric in allocation
--   count, size, and order; class B keeps its draw. The completion
--   function runs identically in both classes, so per-pair public
--   inputs (messages, nonces, associated data) stay fresh on both
--   sides.
--
--   The sampler and completion must return values in normal form;
--   'fixVsRandom' balances allocation between the classes but cannot
--   force what they left lazy.
--
--   For a whole-input fix-vs-random hypothesis, pass 'pure' as the
--   completion:
--
--   > hyp <- fixVsRandom
--   >   (\x -> () <$ evaluate (inv x))   -- target
--   >   (randomMont g)                   -- secret sampler
--   >   pure                             -- input is just the secret
fixVsRandom
  :: (a -> IO ())   -- ^ target
  -> IO s           -- ^ secret sampler
  -> (s -> IO a)    -- ^ complete an input around a secret
  -> IO (Hypothesis a)
fixVsRandom tgt sec embed = do
  !fix <- sec
  pure Hypothesis
    { target  = tgt
    , sampleA = do
        !_ <- sec        -- draw and discard: balance class B
        embed fix
    , sampleB = do
        !s <- sec
        embed s
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

-- | Test outcome. 'Reject' means a leak was detected; 'Pass' means
--   no leak was detected within budget. Both constructors carry the
--   same run report.
--
--   The peak log-wealth of the /mixture/ starts at @log 8@ (each of
--   the four hedged components starts at @log(K^+ + K^-) = log 2@);
--   rejection is at @log(8 \/ alpha)@. A clip count that is a
--   substantial fraction of 'resPairs' indicates the warmup bound
--   'resBound' was too tight for the tight component; raise
--   'cfgWarmup', enlarge 'cfgBatch', or treat the run with suspicion
--   (the wider (@4c@, @16c@) magnitude components in the hedge still
--   see the pre-clip @d@, but their @lambda_max@ is proportionally
--   smaller).
--
--   The per-component peaks identify which evidence channel drove a
--   rejection: 'resPeakSign' growing points at a distributional
--   asymmetry (bulk shift or shape leak), 'resPeakMagn' at a small
--   consistent shift, 'resPeakMagn16' at rare large outliers. Each
--   is a supremum over its own (possibly different) time, so the
--   peaks are diagnostic only -- rejection latches on the mixture's
--   supremum, never on per-component peaks.
data Result
  = Reject
      { resPairs      :: {-# UNPACK #-} !Int
        -- ^ sample pairs consumed.
      , resPeakLogW   :: {-# UNPACK #-} !Double
        -- ^ peak (supremum-so-far) log-wealth of the mixture,
        --   @sup_t log(sum_i K^i_t)@.
      , resClipped    :: {-# UNPACK #-} !Int
        -- ^ pairs whose @|d|@ exceeded 'resBound' and was clipped
        --   before reaching the tight magnitude component.
      , resBound      :: {-# UNPACK #-} !Double
        -- ^ the tight clip bound @c@ estimated during warmup
        --   (@max 1 (2 * p99(|d|))@).
      , resPeakSign   :: {-# UNPACK #-} !Double
        -- ^ peak log-wealth of the sign component.
      , resPeakMagn   :: {-# UNPACK #-} !Double
        -- ^ peak log-wealth of the magnitude component at @c@.
      , resPeakMagn4  :: {-# UNPACK #-} !Double
        -- ^ peak log-wealth of the magnitude component at @4c@.
      , resPeakMagn16 :: {-# UNPACK #-} !Double
        -- ^ peak log-wealth of the magnitude component at @16c@.
      }
  | Pass
      { resPairs      :: {-# UNPACK #-} !Int
      , resPeakLogW   :: {-# UNPACK #-} !Double
      , resClipped    :: {-# UNPACK #-} !Int
      , resBound      :: {-# UNPACK #-} !Double
      , resPeakSign   :: {-# UNPACK #-} !Double
      , resPeakMagn   :: {-# UNPACK #-} !Double
      , resPeakMagn4  :: {-# UNPACK #-} !Double
      , resPeakMagn16 :: {-# UNPACK #-} !Double
      }
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
  (!c, !meterResolves) <- warmup meter cfg hyp wSeed
  unless meterResolves $ throwIO WarmupZeroDuration
  !hcfg <- case mkHedgeCfg (cfgAlpha cfg) c of
    Left  e -> throwIO (InvalidEprocConfig e)
    Right x -> pure x
  let !thresh = hcThresh hcfg
      finish ctor !n !st !nclip = ctor
        n (hsSupLogMix st) nclip c
        (hsSupSign st) (hsSupMagn1 st) (hsSupMagn2 st) (hsSupMagn3 st)
      go !n !st !rng !nclip
        | n >= cfgBudget cfg =
            pure $! finish Pass n st nclip
        | otherwise = do
            let (!rng', !flipOrder) = nextOrderBit rng
            (!ta, !tb) <- runPair meter cfg hyp flipOrder
            let !d       = fromIntegral ta - fromIntegral tb :: Double
                !clipped = abs d > c
                !st'     = updateHedge hcfg st d
                !nclip'  = if clipped then nclip + 1 else nclip
                !n'      = n + 1
            if hsSupLogMix st' >= thresh
              then pure $! finish Reject n' st' nclip'
              else go n' st' rng' nclip'
  go 0 (initialHedge hcfg) mSeed 0

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
--   A\/A only exercises 'sampleA', so asymmetries specific to
--   'sampleB' (a heavier allocation path, RTS state that only its
--   fresh objects trigger) slip through. Pair 'runAA' with 'runBB'
--   for a full attribution.
toAA :: Hypothesis a -> Hypothesis a
toAA h = h { sampleB = sampleA h }
{-# INLINE toAA #-}

-- | Run the A\/A diagnostic. Equivalent to @'runCT' meter cfg
--   ('toAA' hyp)@; should always 'Pass' if the harness is
--   symmetric.
runAA :: Meter -> Config -> Hypothesis a -> IO Result
runAA meter cfg hyp = runCT meter cfg (toAA hyp)

-- | Convert a 'Hypothesis' into its /B\/B diagnostic/ form:
--   'sampleA' is replaced by 'sampleB'. Symmetric companion to
--   'toAA' — a 'Reject' verdict here indicts an asymmetry specific
--   to 'sampleB' rather than the target.
toBB :: Hypothesis a -> Hypothesis a
toBB h = h { sampleA = sampleB h }
{-# INLINE toBB #-}

-- | Run the B\/B diagnostic. Equivalent to @'runCT' meter cfg
--   ('toBB' hyp)@; should always 'Pass' if the harness is
--   symmetric.
runBB :: Meter -> Config -> Hypothesis a -> IO Result
runBB meter cfg hyp = runCT meter cfg (toBB hyp)

-- | Attribution of a 'Reject' verdict, as determined by the A\/A
--   and B\/B diagnostics. Rejecting diagnostics carry their
--   'Result's as evidence.
data Attribution
  = TargetLeak
    -- ^ both diagnostics passed: the harness is symmetric, and a
    --   'Reject' from 'runCT' is attributable to the target.
  | SampleAAsymmetry !Result
    -- ^ the A\/A diagnostic rejected: 'sampleA' itself induces a
    --   timing asymmetry, and the real-hypothesis verdict is not
    --   informative until it is fixed.
  | SampleBAsymmetry !Result
    -- ^ the B\/B diagnostic rejected: as above, for 'sampleB'.
  | PervasiveAsymmetry !Result !Result
    -- ^ both diagnostics rejected (A\/A and B\/B results
    --   respectively): the harness asymmetry is not specific to
    --   either sampler.
  deriving Show

-- | Attribute a 'Reject' verdict by running the A\/A and B\/B
--   diagnostics and combining their outcomes. Only meaningful after
--   'runCT' has rejected the hypothesis: on a symmetric harness both
--   diagnostics pass whether or not the target leaks, so a
--   'TargetLeak' attribution confirms an existing rejection rather
--   than detecting anything by itself.
attribute :: Meter -> Config -> Hypothesis a -> IO Attribution
attribute meter cfg hyp = do
  raa <- runAA meter cfg hyp
  rbb <- runBB meter cfg hyp
  pure $! case (raa, rbb) of
    (Pass{},     Pass{})     -> TargetLeak
    (a@Reject{}, Pass{})     -> SampleAAsymmetry a
    (Pass{},     b@Reject{}) -> SampleBAsymmetry b
    (a@Reject{}, b@Reject{}) -> PervasiveAsymmetry a b

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

-- The K=4 hedge composition ------------------------------------------------

-- Four e-processes running on the same per-pair @d@, combined via
-- an outer convex hedge with uniform weights:
--
--   * /sign/: a two-sided Bernoulli test at @p_0 = 1\/2@ on
--     @sign d@ (ties skipped, valid because non-tie steps are
--     conditionally fair under symmetric @d@). No bounds, no
--     warmup calibration; unbeatable against equal-mean shape
--     leaks and heavy-tail scenarios where a small bulk shift
--     moves @P(d > 0)@ off @1\/2@ but variance from the tail
--     swamps the magnitude signal.
--
--   * /magn(c), magn(4c), magn(16c)/: three two-sided bounded-mean
--     tests on @d@ clipped to progressively wider intervals. The
--     tight bound gives the largest @lambda_max@ and dominates
--     against small consistent bulk shifts; the wider bounds
--     de-fragilize the single warmup-derived @c@ against
--     rare-outlier leaks (a rarely-taken data-dependent branch
--     showing up as occasional slow paths).
--
-- Each component is itself an e-process @E^i_t@ with
-- @E[E^i_0] = 1@; their arithmetic mean @M_t = (1\/K) sum_i E^i_t@
-- is again an e-process with @E[M_0] = 1@, and Ville gives
-- @P(sup_t M_t >= 1\/alpha) <= alpha@. In terms of the
-- @log_wealth = log(K^+ + K^-)@ each component exposes, that
-- rejects when @sup_t log(sum_i (K^{i,+}_t + K^{i,-}_t))@ crosses
-- @log(2K\/alpha) = log(8\/alpha)@ for @K = 4@.
--
-- The latch must be on the mixture's supremum, /not/ on the
-- per-component suprema summed — the latter would use suprema from
-- different times and can be much larger than the mixture ever
-- actually reached, silently inflating the effective alpha.

data HedgeCfg = HedgeCfg
  { hcSign   :: !EPSign.Config
  , hcMagn1  :: !EPMagn.Config
  , hcMagn2  :: !EPMagn.Config
  , hcMagn3  :: !EPMagn.Config
  , hcC1     :: {-# UNPACK #-} !Double  -- tight clip level (base c)
  , hcC2     :: {-# UNPACK #-} !Double  -- 4c
  , hcC3     :: {-# UNPACK #-} !Double  -- 16c
  , hcThresh :: {-# UNPACK #-} !Double  -- log(8 / alpha)
  }

data HedgeState = HedgeState
  { hsSign      :: !EPSign.State
  , hsMagn1     :: !EPMagn.State
  , hsMagn2     :: !EPMagn.State
  , hsMagn3     :: !EPMagn.State
  , hsSupLogMix :: {-# UNPACK #-} !Double
    -- ^ supremum-so-far of @log(sum_i K^i_t)@.
  , hsSupSign   :: {-# UNPACK #-} !Double
    -- ^ supremum-so-far of the sign component's log-wealth.
  , hsSupMagn1  :: {-# UNPACK #-} !Double
    -- ^ ditto, magnitude at @c@.
  , hsSupMagn2  :: {-# UNPACK #-} !Double
    -- ^ ditto, magnitude at @4c@.
  , hsSupMagn3  :: {-# UNPACK #-} !Double
    -- ^ ditto, magnitude at @16c@.
  }

mkHedgeCfg :: Double -> Double -> Either ConfigError HedgeCfg
mkHedgeCfg !alpha !c = do
  let !c2 = 4  * c
      !c3 = 16 * c
  !sc <- EPSign.config 0.5 alpha Newton
  !m1 <- EPMagn.config 0 (negate c)  c  alpha Newton
  !m2 <- EPMagn.config 0 (negate c2) c2 alpha Newton
  !m3 <- EPMagn.config 0 (negate c3) c3 alpha Newton
  pure HedgeCfg
    { hcSign   = sc
    , hcMagn1  = m1
    , hcMagn2  = m2
    , hcMagn3  = m3
    , hcC1     = c
    , hcC2     = c2
    , hcC3     = c3
    , hcThresh = log (8 / alpha)
    }

initialHedge :: HedgeCfg -> HedgeState
initialHedge HedgeCfg{..} = HedgeState
  { hsSign      = EPSign.initial hcSign
  , hsMagn1     = EPMagn.initial hcMagn1
  , hsMagn2     = EPMagn.initial hcMagn2
  , hsMagn3     = EPMagn.initial hcMagn3
  , hsSupLogMix = log 8
    -- ^ each component starts at @log(K^+ + K^-) = log 2@, sum of
    --   four exp's = 8.
  , hsSupSign   = log 2
  , hsSupMagn1  = log 2
  , hsSupMagn2  = log 2
  , hsSupMagn3  = log 2
  }

updateHedge :: HedgeCfg -> HedgeState -> Double -> HedgeState
updateHedge HedgeCfg{..} HedgeState{..} !d =
  let !s'  = if d == 0
             then hsSign
             else EPSign.update hcSign hsSign (d > 0)
      !m1' = EPMagn.update hcMagn1 hsMagn1 (clipD hcC1 d)
      !m2' = EPMagn.update hcMagn2 hsMagn2 (clipD hcC2 d)
      !m3' = EPMagn.update hcMagn3 hsMagn3 (clipD hcC3 d)
      !lwS = EPSign.log_wealth s'
      !lw1 = EPMagn.log_wealth m1'
      !lw2 = EPMagn.log_wealth m2'
      !lw3 = EPMagn.log_wealth m3'
      !logMix = logSumExp4 lwS lw1 lw2 lw3
  in  HedgeState
        { hsSign      = s'
        , hsMagn1     = m1'
        , hsMagn2     = m2'
        , hsMagn3     = m3'
        , hsSupLogMix = max hsSupLogMix logMix
        , hsSupSign   = max hsSupSign lwS
        , hsSupMagn1  = max hsSupMagn1 lw1
        , hsSupMagn2  = max hsSupMagn2 lw2
        , hsSupMagn3  = max hsSupMagn3 lw3
        }

clipD :: Double -> Double -> Double
clipD !lim !x
  | abs x > lim = if x > 0 then lim else negate lim
  | otherwise   = x
{-# INLINE clipD #-}

logSumExp4 :: Double -> Double -> Double -> Double -> Double
logSumExp4 !a !b !c !d =
  let !m = max (max a b) (max c d)
  in  m + log ( exp (a - m) + exp (b - m)
              + exp (c - m) + exp (d - m) )
{-# INLINE logSumExp4 #-}

-- The order-bit PRNG. Deterministic splitmix64 (from "Censor.Rng")
-- threaded from a resolved seed. The conditional-null guarantee
-- needs the per-pair order bit to be independent of the target's
-- timing distribution; the seed itself only needs to be independent
-- of that distribution too, which fresh entropy (default) or a
-- user-supplied constant both satisfy.

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

-- Warmup collects per-pair @|d|@ (for the bound estimate) and the
-- flat list of per-class readings (for the meter-resolution
-- diagnosis). The order-bit randomisation matches runCT's main
-- loop so the p99 estimate doesn't inherit a parity-locked nuisance
-- bias.
--
-- Returns @(c, meterResolves)@ where @c = max 1 (2 * p99(|d|))@ is
-- the clipping bound and @meterResolves@ is True iff
-- @p99(readings) > 0@ (i.e., the meter is producing nonzero readings
-- often enough that at least the upper tail is resolved). If
-- @meterResolves@ is False the meter's quantum is swallowing at
-- least 99% of per-target timings and the caller throws
-- 'WarmupZeroDuration' with the "raise cfgBatch" hint. All-zero
-- /differences/ with well-resolved readings are the PMU-on-truly-CT
-- happy path — we fall back to @c = 1@ and let the main loop
-- proceed; under continued zero differences the wealth process
-- stays flat and the run passes at budget.
warmup :: Meter -> Config -> Hypothesis a -> Word64 -> IO (Double, Bool)
warmup meter cfg hyp seed = do
  let go !i !rng !accD !accR
        | i >= cfgWarmup cfg = pure (accD, accR)
        | otherwise = do
            let (!rng', !flipOrder) = nextOrderBit rng
            (!x, !y) <- runPair meter cfg hyp flipOrder
            let !dx = fromIntegral x :: Double
                !dy = fromIntegral y :: Double
                !da = abs (dx - dy)
            go (i + 1) rng' (da : accD) (x : y : accR)
  (diffs, readings) <- go 0 seed [] []
  let !p99d = orderStat99 diffs
      !p99r = orderStat99 readings
  -- "p99" is fuzzy: at default cfgWarmup = 200 (m = 200 diffs, m'
  -- = 400 readings) idx lands at ~99.5% empirical rank. At small
  -- warmup sizes (cfgWarmup <= 100 for diffs) idx = m - 1, so p99
  -- degenerates to the sample max — surfaced in the cfgWarmup
  -- docstring.
  --
  -- 2x the order statistic on |d| gives headroom for occasional
  -- outliers; observations above are clipped during the main pass.
  -- The @max 1@ floor keeps @c > 0@ so the e-process configs accept
  -- it even when warmup differences are identically zero (PMU on
  -- truly CT code).
  pure (max 1 (2 * p99d), p99r > 0)

-- Empirical @~p99@ order statistic. Sorts and picks index
-- @(m * 99) \/ 100@; degenerates to the sample max at small @m@
-- (documented on 'cfgWarmup').
orderStat99 :: Ord a => Num a => [a] -> a
orderStat99 xs =
  let !sorted = sort xs
      !m      = length sorted
      !idx    = max 0 (min (m - 1) ((m * 99) `div` 100))
  in  case drop idx sorted of
        (x : _) -> x
        []      -> 0
{-# INLINE orderStat99 #-}
