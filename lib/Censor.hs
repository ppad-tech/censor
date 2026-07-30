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
-- Declare a t'Hypothesis' (an @IO@ action plus two input samplers);
-- 'runCT' measures paired class-A \/ class-B timings, streams them
-- through an anytime-valid two-sample test, and either rejects (a
-- leak is detected) or exhausts its budget.
--
-- The null is /conditional exchangeability/ of the measured pair
-- @(ta, tb)@: given the past, swapping the two class labels leaves
-- the joint law unchanged. This is what the randomised per-pair
-- A\/B order actually delivers, and it is implied by (but weaker
-- than) the informal target property \"the two input classes induce
-- the same timing distribution\".
--
-- Every component of the test is an /antisymmetric functional/ of
-- the pair — a statistic @g@ with @g(ta, tb) = -g(tb, ta)@ — and
-- each such functional has a conditionally symmetric distribution
-- under the null, hence conditional mean zero. The test is a
-- convex-hedge mixture of e-processes over a family of them: a
-- sign test on @sign d@ where @d = ta - tb@, three bounded-mean
-- tests on @d@ clipped to progressively wider intervals, and a set
-- of CDF-indicator tests on @1[ta <= q] - 1[tb <= q]@ at
-- warmup-fixed cut points @q@ (see 'runCT' for the shape). Each
-- component's wealth process is a nonnegative supermartingale
-- under the null (a notional bankroll that bets on the data and
-- grows only when the null is wrong; see "Numeric.Eproc.Bounded"
-- for the mechanics). The convex combination is delegated to
-- "Numeric.Eproc.Mixture": the mixture is itself an e-process, and
-- the test is valid under optional stopping — 'runCT' halts as
-- soon as the mixture e-value crosses @1 \/ alpha@, with type-I
-- error controlled at @alpha@ regardless of when sampling stops.
-- Alongside the verdict, the driver reports the mixture's
-- anytime-valid p-value and per-component peak evidence on the
-- calibrated e-value scale.
--
-- The alternatives the test can see are exactly those that move at
-- least one functional in the family off zero. That family is
-- deliberately not exhaustive: no fixed finite set of functionals
-- detects every departure from exchangeability. In particular a
-- distributional difference confined to regions between the
-- warmup-fixed cut points, and leaving @d@ symmetric, is invisible
-- to this test — see 'runCT' for the standing caveat.

module Censor (
    -- * Hypothesis
    Hypothesis(..)
  , fixVsRandom
  , fixVsRandomCtx

    -- * Configuration
  , Config(..)
  , defaultConfig

    -- * Result
  , Result(..)

    -- * Advisories
  , Advisory(..)
  , LeakShape(..)
  , diagnose
  , leakShape

    -- * Errors
  , CensorError(..)

    -- * Running
  , runCT
  , runCTWith
  , Frame(..)
  , runCTAttributed

    -- * Batch calibration
  , calibrateBatch
  , CalibReport(..)
  , calibrateBatchReport
  , Noise(..)

    -- * A\/A and B\/B negative controls
  , toAA
  , runAA
  , toBB
  , runBB
  , Attribution(..)
  , ControlVerdict(..)
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
  , randomBytes
  ) where

import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (unless)
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort, sortBy)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (IOMode(ReadMode), withBinaryFile)
import qualified Numeric.Eproc.Bernoulli.TwoSided as EPSign
import qualified Numeric.Eproc.Bounded as EPMagn
import qualified Numeric.Eproc.ConfSeq as EPCS
import qualified Numeric.Eproc.Mixture as EPMix
import Numeric.Eproc.Bounded (Bettor(Newton), ConfigError)

import Censor.Meter
import Censor.Rng (Rng, mkRng, nextWord, randomBytes, splitMix)

-- | A constant-time hypothesis.
--
--   * @target@: the @IO@ action under test, parameterised on input.
--   * @prepare@: a per-pair prologue, run once before either
--     sampler and outside the timed region. 'pure' @()@ for an
--     ordinary hypothesis; 'fixVsRandomCtx' uses it to draw one
--     fresh public context that both classes are then built
--     around.
--   * @sampleA@, @sampleB@: samplers for two input classes
--     (typically a \"fixed\" class and a \"random\" class), drawn
--     after @prepare@ and independently of each other given it.
--
--   The null asserts that the measured pair @(ta, tb)@ is
--   conditionally exchangeable — swapping the class labels leaves
--   the conditional joint law unchanged. This is what the
--   randomised A\/B order delivers, and it is what the driver
--   tests, via a convex-hedge mixture of anytime-valid e-processes
--   over a family of antisymmetric functionals of the pair (a sign
--   test on @sign d@, bounded-mean tests on @d = ta - tb@ clipped
--   to @c@, @4c@, and @16c@, and CDF-indicator tests at
--   warmup-fixed cut points).
--
--   The detected alternative class is the union over that family:
--   first-moment shifts, sign-asymmetric shape leaks, rare-outlier
--   leaks, and dispersion or general CDF differences all move at
--   least one component off the null. It is a union, not a
--   complete cover — a departure orthogonal to every functional in
--   the family passes. Read a 'Pass' as \"none of these channels
--   found evidence\", not as proof of constant time.
data Hypothesis a = Hypothesis
  { target  :: !(a -> IO ())
  , prepare :: !(IO ())
  , sampleA :: !(IO a)
  , sampleB :: !(IO a)
  }

-- | Construct a dudect-style /fix-vs-random/ t'Hypothesis' from a
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
    , prepare = pure ()
    , sampleA = do
        !_ <- sec        -- draw and discard: balance class B
        embed fix
    , sampleB = do
        !s <- sec
        embed s
    }

-- | A /fix-vs-random/ t'Hypothesis' in which both classes are built
--   around one shared, freshly drawn public context.
--
--   'fixVsRandom' runs its completion independently for each class,
--   so per-pair public inputs (message, nonce, associated data,
--   modulus) are either independently random across the pair —
--   injecting nuisance variance into @d@ that has nothing to do
--   with the secret — or held fixed for the whole run, which tests
--   only one public input. This constructor takes the third option:
--   draw one fresh context per pair in 'prepare', then build both
--   class A and class B around /that same/ context.
--
--   The tested axis is then secret dependence conditional on
--   identical fresh public input. Conditioning on the context
--   removes it as a source of paired variance, which sharpens
--   power, and makes the intended axis explicit rather than
--   implicit in how the completion was written.
--
--   The null is unaffected: under H_0 the timing law does not
--   depend on the secret, so given the context the two readings are
--   conditionally i.i.d. and the pair stays exchangeable.
--
--   > hyp <- fixVsRandomCtx
--   >   (\(k, m) -> () <$ evaluate (mac k m))  -- target
--   >   (randomBytes g 32)                     -- secret sampler
--   >   (randomBytes g 256)                    -- public context
--   >   (\msg key -> pure (key, msg))          -- build the input
fixVsRandomCtx
  :: (a -> IO ())      -- ^ target
  -> IO s              -- ^ secret sampler
  -> IO c              -- ^ public-context sampler (once per pair)
  -> (c -> s -> IO a)  -- ^ build an input from context and secret
  -> IO (Hypothesis a)
fixVsRandomCtx tgt sec ctx embed = do
  !fix <- sec
  -- seeded with a real draw so the ref is total; 'prepare'
  -- overwrites it before the first pair is ever measured.
  !c0  <- ctx
  ref  <- newIORef c0
  pure Hypothesis
    { target  = tgt
    , prepare = do
        !c <- ctx
        writeIORef ref c
    , sampleA = do
        !_ <- sec        -- draw and discard: balance class B
        !c <- readIORef ref
        embed c fix
    , sampleB = do
        !s <- sec
        !c <- readIORef ref
        embed c s
    }

-- | Driver configuration.
data Config = Config
  { cfgAlpha  :: !Double  -- ^ significance level
  , cfgBudget :: !Int     -- ^ max sample pairs to consume
  , cfgBatch  :: !Int     -- ^ target repetitions per timed region.
                          --   The meter repeats the /same/ @target a@
                          --   action on one pre-drawn sample (see
                          --   'Censor.Meter.rep'). This only multiplies
                          --   real work when each repetition genuinely
                          --   recomputes -- a foreign (FFI) call, or a
                          --   target that forces fresh work every time.
                          --   For a pure Haskell target
                          --   @\\a -> evaluate (f a)@ the @f a@ thunk is
                          --   shared across the batch and forced once;
                          --   the remaining repetitions are no-ops on an
                          --   already-evaluated value, so a per-batch
                          --   reading divided by @cfgBatch@ is /not/ a
                          --   per-call figure. Use @cfgBatch = 1@ for
                          --   pure targets and let the target's own cost
                          --   clear the timer quantum; if it is
                          --   sub-quantum, widen the work rather than
                          --   batching.
  , cfgWarmup :: !Int     -- ^ warmup pairs. The clip bound is set
                          --   from the @~p99@ of @cfgWarmup@ per-pair
                          --   absolute differences @|d|@; at small
                          --   warmup sizes the order-statistic
                          --   discretisation degenerates to the
                          --   sample maximum (specifically, any
                          --   @cfgWarmup <= 100@ picks the max).
  , cfgSeed   :: !(Maybe Word64)
                          -- ^ seed for the per-pair A\/B order bit.
                          --   'Nothing' (the default) draws a fresh
                          --   seed from OS entropy
                          --   (@\/dev\/urandom@) per run, so the
                          --   conditional independence of the order
                          --   bit from any periodic ambient noise
                          --   (timer interrupts, scheduler quanta)
                          --   is a fact rather than an assumption.
                          --   @Just s@ pins the seed instead.
                          --
                          --   Whichever way it resolves, the seed
                          --   actually used is reported as
                          --   'resSeed', so a randomised run can be
                          --   replayed exactly by feeding that value
                          --   back as @Just@. Keep this seed
                          --   distinct from any seed driving the
                          --   input samplers: the order bit must be
                          --   independent of the sampled timing
                          --   sequence for the conditional null to
                          --   hold.
  } deriving Show

-- | Defaults: @alpha = 1e-6@, budget @100000@ pairs, batch 1 (no
--   inner repetition), warmup 200 pairs, order-bit seed drawn from
--   OS entropy per run.
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
--   All evidence fields are reported on the calibrated log e-value
--   scale: a fresh run sits at @0@, and the mixture rejects when
--   its supremum-so-far ('resPeakLogW') crosses @log(1 \/ alpha)@.
--   'resPValue' is the same evidence expressed as an anytime-valid
--   p-value: monotone nonincreasing, valid at any data-dependent
--   stopping time, and at or below @alpha@ exactly when the verdict
--   is 'Reject'.
--
--   A clip count that is a substantial fraction of 'resPairs'
--   indicates the warmup bound 'resBound' was too tight for the
--   tight component; raise 'cfgWarmup', enlarge 'cfgBatch', or
--   treat the run with suspicion (the wider (@4c@, @16c@) magnitude
--   components in the hedge still see the pre-clip @d@, but their
--   @lambda_max@ is proportionally smaller).
--
--   The per-component peaks identify which evidence channel drove a
--   rejection: 'resPeakSign' growing points at an asymmetry of @d@
--   itself (@P(d > 0) \/= 1\/2@), 'resPeakMagn' at a small
--   consistent shift, 'resPeakMagn16' at rare large outliers, and
--   'resPeakCdf' at a difference between the two classes' CDFs at
--   one of the warmup-fixed cut points (dispersion or general shape,
--   including leaks that leave @d@ symmetric and so are invisible to
--   every other channel). Each is a supremum over its own (possibly
--   different) time, so the peaks are diagnostic only -- rejection
--   latches on the mixture's supremum, never on per-component peaks.
data Result
  = Reject
      { resPairs      :: {-# UNPACK #-} !Int
        -- ^ sample pairs consumed.
      , resPeakLogW   :: {-# UNPACK #-} !Double
        -- ^ peak (supremum-so-far) log e-value of the mixture;
        --   starts at @0@, crosses @log(1 \/ alpha)@ on rejection.
      , resPValue     :: {-# UNPACK #-} !Double
        -- ^ the anytime-valid p-value @min 1 (exp -resPeakLogW)@;
        --   at or below @cfgAlpha@ iff the verdict is 'Reject'.
      , resEffect     :: !(Double, Double)
        -- ^ anytime-valid @1 - cfgAlpha@ confidence interval for
        --   the mean of @d@ clipped to @[-16c, 16c]@, in meter
        --   units per batch (divide by 'cfgBatch' for per-call).
        --   Covers zero on a truly constant-time target; brackets
        --   the shift magnitude of the clipped difference on a
        --   leaky one. Valid at the data-dependent stopping time,
        --   so it may be wide when a strong leak halts the run
        --   early. On long runs the evidence can pin the mean
        --   below the estimation grid's resolution (the grid is
        --   exhausted; see 'Numeric.Eproc.ConfSeq.interval'); the
        --   interval reported is then the last resolvable one --
        --   at most a few grid steps wide, and still covering the
        --   true mean under the same time-uniform guarantee, since
        --   the sequence's intervals are nested. The interval's
        --   coverage and the verdict's type-I control are separate
        --   marginal guarantees, each at 'cfgAlpha'.
      , resClipped    :: {-# UNPACK #-} !Int
        -- ^ pairs whose @|d|@ exceeded 'resBound' and was clipped
        --   before reaching the tight magnitude component.
      , resBound      :: {-# UNPACK #-} !Double
        -- ^ the tight clip bound @c@ estimated during warmup
        --   (@max 1 (2 * p99(|d|))@).
      , resPeakSign   :: {-# UNPACK #-} !Double
        -- ^ peak log e-value of the sign component.
      , resPeakMagn   :: {-# UNPACK #-} !Double
        -- ^ peak log e-value of the magnitude component at @c@.
      , resPeakMagn4  :: {-# UNPACK #-} !Double
        -- ^ peak log e-value of the magnitude component at @4c@.
      , resPeakMagn16 :: {-# UNPACK #-} !Double
        -- ^ peak log e-value of the magnitude component at @16c@.
      , resPeakCdf    :: {-# UNPACK #-} !Double
        -- ^ largest peak log e-value across the CDF-indicator
        --   components (@0@ when warmup resolved no cut points).
      , resCutPoints  :: ![Double]
        -- ^ the warmup-fixed cut points @q@ backing the
        --   CDF-indicator components, ascending and deduplicated.
        --   Empty when warmup readings were degenerate.
      , resSeed       :: {-# UNPACK #-} !Word64
        -- ^ the order-bit seed the run actually used, whether
        --   supplied via 'cfgSeed' or drawn from OS entropy.
        --   Feeding it back as @'cfgSeed' = 'Just' s@ replays the
        --   same A\/B order sequence.
      }
  | Pass
      { resPairs      :: {-# UNPACK #-} !Int
      , resPeakLogW   :: {-# UNPACK #-} !Double
      , resPValue     :: {-# UNPACK #-} !Double
      , resEffect     :: !(Double, Double)
      , resClipped    :: {-# UNPACK #-} !Int
      , resBound      :: {-# UNPACK #-} !Double
      , resPeakSign   :: {-# UNPACK #-} !Double
      , resPeakMagn   :: {-# UNPACK #-} !Double
      , resPeakMagn4  :: {-# UNPACK #-} !Double
      , resPeakMagn16 :: {-# UNPACK #-} !Double
      , resPeakCdf    :: {-# UNPACK #-} !Double
      , resCutPoints  :: ![Double]
      , resSeed       :: {-# UNPACK #-} !Word64
      }
  deriving Show

-- | An actionable observation about a t'Result', produced by
--   'diagnose'. Each constructor identifies a specific pattern in
--   the report and points to the corresponding remedy in prose you
--   would otherwise have to reconstruct from the 'Result' docstring.
data Advisory
  = HighClipRate !Double
    -- ^ Fraction @resClipped \/ resPairs@ exceeded @0.05@: the tight
    --   magnitude component saw its input truncated on a nontrivial
    --   share of pairs, hurting power against small consistent
    --   shifts. Raise 'cfgWarmup' for a better @c@ estimate, enlarge
    --   'cfgBatch', or treat the run with suspicion.
  | LowPower !Double
    -- ^ On 'Pass' only: the effect-interval half-width exceeded the
    --   tight clip bound @c@ (the ratio is carried here, in units of
    --   @c@). Leaks with a mean shift below that scale would not have
    --   been resolved by this run; raise 'cfgBudget' to sharpen the
    --   interval.
  | RejectionDriver !LeakShape
    -- ^ On 'Reject' only: which channel of the K=4 hedge drove the
    --   detection, i.e. what shape of leak was found.
  deriving (Eq, Show)

-- | The qualitative shape of a leak, inferred from the per-component
--   peaks of the hedge. Reported by 'RejectionDriver' and available
--   standalone via 'leakShape'.
data LeakShape
  = BulkShift
    -- ^ 'resPeakMagn' (tight magnitude) dominates: a small consistent
    --   mean shift in @d@.
  | ShapeLeak
    -- ^ 'resPeakSign' dominates: an equal-mean asymmetry of the
    --   difference itself (@P(d > 0) \/= 1\/2@).
  | RareOutlier
    -- ^ 'resPeakMagn4' or 'resPeakMagn16' dominates: rare-tail
    --   outliers (e.g. a rarely-taken data-dependent branch).
  | CdfShift
    -- ^ 'resPeakCdf' dominates: the two classes' CDFs differ at a
    --   warmup-fixed cut point. Typical of a dispersion (variance)
    --   leak, which leaves @d@ symmetric and so registers on no
    --   other channel.
  | MixedShape
    -- ^ No single component peak dominates by @>= 1.5x@; the leak
    --   registers on several channels comparably.
  deriving (Eq, Show)

-- | Interpret a t'Result' and emit advisories highlighting the
--   patterns the report exposes: heavy clipping (the tight component
--   is starved), a low-power 'Pass' (the effect interval remains
--   wider than the tight bound), and, on 'Reject', which channel of
--   the K=4 hedge drove the detection.
--
--   The output is stable in order — clip, then power, then driver —
--   so downstream tooling can pattern-match by position.
diagnose :: Result -> [Advisory]
diagnose r =
  let !frac = if resPairs r == 0
                then 0
                else fromIntegral (resClipped r)
                     / (fromIntegral (resPairs r) :: Double)
      clipA = [HighClipRate frac | frac > 0.05]
      powerA = case r of
        Pass{} ->
          let !hw = (snd (resEffect r) - fst (resEffect r)) / 2
              !c  = resBound r
          in  [LowPower (hw / c) | c > 0 && hw > c]
        Reject{} -> []
      drvA = case r of
        Reject{} -> [RejectionDriver (leakShape r)]
        Pass{}   -> []
  in  clipA ++ powerA ++ drvA

-- | Identify which channel of the hedge has the largest peak log
--   e-value in a t'Result', binning the two wider magnitude
--   components together as 'RareOutlier' and the CDF-indicator
--   components together as 'CdfShift'. Returns 'MixedShape' when no
--   single peak exceeds the next-largest by a factor of @1.5@.
--
--   Only meaningful on a 'Reject' — on a 'Pass' every peak sits near
--   the calibrated floor of @0@ and the returned shape is not
--   informative.
leakShape :: Result -> LeakShape
leakShape r =
  let !sig  = resPeakSign r
      !mag  = resPeakMagn r
      !tl   = max (resPeakMagn4 r) (resPeakMagn16 r)
      !cdf  = resPeakCdf r
      ranked = sortBy (\a b -> compare (fst b) (fst a))
                 [ (sig, ShapeLeak)
                 , (mag, BulkShift)
                 , (tl,  RareOutlier)
                 , (cdf, CdfShift)
                 ]
  in  case ranked of
        (p1, s1) : (p2, _) : _
          | p1 > 1.5 * max 0 p2 -> s1
          | otherwise           -> MixedShape
        _ -> MixedShape

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
    -- ^ a t'Config' field was outside its admissible range. The
    --   'String' names which field and why.
  | InvalidEprocConfig !ConfigError
    -- ^ the underlying e-process rejected the test configuration
    --   derived from 'cfgAlpha' and the warmup-estimated bound. In
    --   practice this is reachable only via a 'cfgAlpha' outside
    --   @(0, 1)@; adjust 'cfgAlpha' to the standard significance
    --   range.
  deriving (Eq, Show)

instance Exception CensorError

-- | A per-pair snapshot of the driver's state, delivered to the
--   observer passed to 'runCTWith' after every main-phase pair. All
--   quantities are on the same scale as the corresponding 'Result'
--   fields; recording the stream of frames reconstructs the wealth
--   trajectory without reimplementing the driver.
data Frame = Frame
  { frPair    :: {-# UNPACK #-} !Int
    -- ^ pair index (1-based; excludes warmup).
  , frDiff    :: {-# UNPACK #-} !Double
    -- ^ the raw per-pair difference @d = ta - tb@, pre-clip.
  , frLogW    :: {-# UNPACK #-} !Double
    -- ^ the mixture's current log e-value at this pair.
  , frLogWSup :: {-# UNPACK #-} !Double
    -- ^ its supremum-so-far (the quantity the reject latches on).
  , frLo      :: {-# UNPACK #-} !Double
    -- ^ effect-interval lower endpoint (meter units per batch).
  , frHi      :: {-# UNPACK #-} !Double
    -- ^ effect-interval upper endpoint.
  , frClipped :: !Bool
    -- ^ whether @|d|@ exceeded the warmup bound and was clipped.
  } deriving Show

-- | Run a constant-time test, delivering a t'Frame' to the observer
--   after every main-phase pair. 'runCT' is @runCTWith@ with a
--   no-op observer; a recording observer reconstructs the full
--   trajectory (see "Censor.Runner"'s @record@) without duplicating
--   the driver.
--
--   The observer runs outside the timed region — between one pair's
--   measurement and the next — so it does not perturb measurement
--   hygiene. It is handed the frame lazily: under the 'runCT'
--   no-op observer nothing in the frame is forced, so the plain
--   test path pays nothing for the hook.
--
--   Phases:
--
--   1. /Warmup/ — take @cfgWarmup@ paired measurements and fix two
--      calibrations from them: an upper bound @c@ on the magnitude
--      of the per-pair difference @|d| = |ta - tb|@, and the cut
--      points @q@ for the CDF-indicator components (order
--      statistics of the /pooled/ per-class readings, so a cut
--      point carries no class information). Both are frozen before
--      the main phase begins.
--   2. /Main/ — for each pair, choose A-first or B-first uniformly
--      at random, sample and measure both classes, then stream
--      every hedge component's functional of @(ta, tb)@ into its
--      e-process: @sign d@, @d@ clipped to @[-c, c]@, @[-4c, 4c]@,
--      @[-16c, 16c]@, and @1[ta <= q] - 1[tb <= q]@ at each cut
--      point. Halt as soon as the mixture crosses threshold, or
--      when the budget is exhausted.
--
--   The main phase also feeds @d@ clipped to the widest hedge
--   interval @[-16c, 16c]@ into an anytime-valid confidence
--   sequence ("Numeric.Eproc.ConfSeq") for its mean, reported as
--   'resEffect': the effect size of a detected leak, in meter
--   units. Warmup pairs are excluded (they fix the bounds).
--
--   Because the order bit is randomised per pair, @(ta, tb)@ is
--   conditionally exchangeable under H_0, so every antisymmetric
--   functional of it is conditionally symmetric about zero — not
--   merely conditional mean zero. Symmetric clipping is odd and so
--   preserves that symmetry. Betting on @d@ directly — rather than
--   on the raw per-class readings — lets @lambda_max@ scale with
--   the noise magnitude @p99(|d|)@ rather than with the target's
--   baseline runtime @p99(t)@, which for narrow-noise
--   heavy-baseline targets can be orders of magnitude better for
--   power at the same alpha.
--
--   /Standing caveat./ The functional family is finite, so the test
--   is not an omnibus test of distributional equality. A departure
--   that leaves @d@ symmetric /and/ leaves @F_A - F_B@ zero at
--   every cut point passes. Widening 'cfgWarmup' sharpens the cut
--   points but does not close the gap in principle. Read a 'Pass'
--   as the absence of evidence on these channels.
runCTWith
  :: Meter -> Config -> Hypothesis a -> (Frame -> IO ()) -> IO Result
runCTWith meter cfg hyp observe = do
  validateConfig cfg
  (!root, !wSeed, !mSeed) <- resolveSeeds (cfgSeed cfg)
  (!c, !qs, !meterResolves) <- warmup meter cfg hyp wSeed
  unless meterResolves $ throwIO WarmupZeroDuration
  !hcfg <- case mkHedgeCfg (cfgAlpha cfg) c qs of
    Left  e -> throwIO (InvalidEprocConfig e)
    Right x -> pure x
  !cscfg <- case EPCS.config (negate (hcC3 hcfg)) (hcC3 hcfg)
                   (cfgAlpha cfg) effectGrid of
    Left  e -> throwIO (InvalidEprocConfig e)
    Right x -> pure x
  let !mixc = hcMix hcfg
      finish ctor !n !st !iv !nclip = ctor
        n (EPMix.log_evalue_sup mixc (hsMix st))
        (EPMix.p_value mixc (hsMix st))
        iv
        nclip c
        (EPSign.log_evalue_sup (hsSign st))
        (EPMagn.log_evalue_sup (hsMagn1 st))
        (EPMagn.log_evalue_sup (hsMagn2 st))
        (EPMagn.log_evalue_sup (hsMagn3 st))
        (peakCdf (hsCdf st))
        qs
        root
      -- the effect interval is latched: once the evidence pins the
      -- mean below the grid's resolution the survivor set empties,
      -- and the last resolvable interval (nested within all earlier
      -- ones, so still covered by the time-uniform guarantee) is
      -- what finish reports.
      go !n !st !cs !iv !rng !nclip
        | n >= cfgBudget cfg =
            pure $! finish Pass n st iv nclip
        | otherwise = do
            let (!rng', !flipOrder) = nextOrderBit rng
            (!ta, !tb) <- runPair meter cfg hyp flipOrder
            let !xa      = fromIntegral ta :: Double
                !xb      = fromIntegral tb :: Double
                !d       = xa - xb
                !clipped = abs d > c
                !st'     = updateHedge hcfg st d xa xb
                !cs'     = EPCS.update cscfg cs (clipD (hcC3 hcfg) d)
                !iv'     = case EPCS.interval cscfg cs' of
                             Just x  -> x
                             Nothing -> iv
                !nclip'  = if clipped then nclip + 1 else nclip
                !n'      = n + 1
            -- lazy: the frame's fields (mixture reads) are forced
            -- only if the observer forces them, so runCT pays nothing.
            observe (Frame n' d
                       (EPMix.log_evalue mixc (hsMix st'))
                       (EPMix.log_evalue_sup mixc (hsMix st'))
                       (fst iv') (snd iv') clipped)
            case EPMix.decide mixc (hsMix st') of
              EPMix.Reject -> pure $! finish Reject n' st' iv' nclip'
              _            -> go n' st' cs' iv' rng' nclip'
      !iv0 = (negate (hcC3 hcfg), hcC3 hcfg)
  go 0 (initialHedge hcfg) (EPCS.initial cscfg) iv0 mSeed 0

-- | Run a constant-time test. See 'runCTWith' for the phase-by-phase
--   description; this is that driver with the per-pair observer
--   discarded.
runCT :: Meter -> Config -> Hypothesis a -> IO Result
runCT meter cfg hyp = runCTWith meter cfg hyp (\_ -> pure ())

-- | Run a constant-time test and, if it rejects, qualify the
--   rejection with the A\/A and B\/B negative controls
--   ('attribute'). A 'Pass' returns 'Nothing' (no controls needed);
--   a 'Reject' returns @'Just' att@ reporting whether either
--   control convicted the harness. The controls re-run the test
--   twice, so this costs up to three runs on a rejection.
runCTAttributed
  :: Meter -> Config -> Hypothesis a -> IO (Result, Maybe Attribution)
runCTAttributed meter cfg hyp = do
  r <- runCT meter cfg hyp
  case r of
    Reject{} -> do
      att <- attribute meter cfg hyp
      pure (r, Just att)
    Pass{}   -> pure (r, Nothing)

-- | Estimate a 'cfgBatch' for a hypothesis under a meter, so the
--   timed region sits well above the meter's resolution. Measures the
--   target on a class-A sample at geometrically increasing batch
--   sizes until a nonzero reading resolves, then returns the batch
--   that brings the per-batch reading up to a target magnitude
--   (roughly a batch of @1@ for microsecond targets, thousands for
--   nanosecond ones).
--
--   > b <- calibrateBatch meter hyp
--   > r <- runCT meter cfg { cfgBatch = b } hyp
--
--   Returns @1@ if the meter cannot resolve the target even at a
--   large batch (the subsequent 'runCT' then throws
--   'WarmupZeroDuration'). This is a heuristic — it reads the median
--   of a few samples per probe to resist a single quantum-straddling
--   measurement — and the batch it picks is meter-specific; pin
--   'cfgBatch' explicitly when you need identical batches across
--   meters for comparability.
calibrateBatch :: Meter -> Hypothesis a -> IO Int
calibrateBatch meter hyp = fmap crBatch (calibrateBatchReport meter hyp)

-- | The probe history behind a 'calibrateBatchReport' run: which
--   batches were tried, what the meter reported at each, whether the
--   cap 'terminated the search, the target reading magnitude used
--   for the refinement, and the meter's noise floor at the selected
--   batch.
--
--   Useful when 'calibrateBatch' picks a batch that surprises you
--   (or falls back to @1@ on an unresolvable meter): the probe list
--   shows exactly what the meter reported at each geometric step.
data CalibReport = CalibReport
  { crBatch     :: {-# UNPACK #-} !Int
    -- ^ the batch size the calibrator selected.
  , crProbes    :: ![(Int, Word64)]
    -- ^ @(batch, median reading)@ pairs, oldest first.
  , crCapHit    :: !Bool
    -- ^ 'True' when the meter never reached 'crTargetMag' before
    --   the internal cap. 'crBatch' will then be @1@ and a
    --   subsequent 'runCT' will throw 'WarmupZeroDuration'.
  , crTargetMag :: {-# UNPACK #-} !Double
    -- ^ the target per-batch reading magnitude used for refinement.
  , crNoise     :: !(Maybe Noise)
    -- ^ the meter's noise floor at 'crBatch', sampled right after
    --   selection; 'Nothing' on a cap-hit.
  } deriving Show

-- | The meter's noise floor at a calibrated batch size: dispersion
--   of repeated readings of one fixed class-A input, in per-batch
--   meter units (the scale of the driver's pair differences and of
--   'resBound' \/ 'resEffect').
--
--   A run-time record of measurement conditions: a 'Pass' obtained
--   where 'nsIqr' rivals the effect sizes of interest carries less
--   weight than one from a quiet machine. Deterministic meters
--   (instructions, branches) read an IQR of @0@.
data Noise = Noise
  { nsMedian :: {-# UNPACK #-} !Word64
    -- ^ median reading at the selected batch.
  , nsIqr    :: {-# UNPACK #-} !Word64
    -- ^ interquartile range of readings at the selected batch.
  } deriving (Eq, Show)

-- | 'calibrateBatch' with a t'CalibReport'. Same probe loop; useful
--   for understanding /why/ the calibrator picked what it picked
--   (particularly when the answer is @1@).
calibrateBatchReport :: Meter -> Hypothesis a -> IO CalibReport
calibrateBatchReport meter hyp = do
  !x <- sampleA hyp
  let !act = target hyp x
      -- grow the batch until the reading itself reaches the target
      -- magnitude, then refine down from that measurement. Estimating
      -- from a small probe would inflate per-call cost with the
      -- meter's fixed per-measurement overhead (e.g. the clock reads);
      -- at the resolved batch that overhead is amortised.
      -- a cap-hit means the median per-call reading is well below
      -- one meter unit at the largest probed batch, which no
      -- functioning meter produces (the batch loop alone costs more);
      -- return 1 so the subsequent run fails fast in warmup rather
      -- than grinding through zero readings at a million calls per
      -- measurement.
      probe !b !ps
        | b > calibCap =
            pure $! finish 1 True Nothing ps
        | otherwise = do
            !r <- probeReading meter act b
            let !ps' = (b, r) : ps
            if fromIntegral r < calibTarget
              then probe (b * 4) ps'
              else do
                let !b' = ceiling
                       (fromIntegral b * calibTarget / fromIntegral r)
                    !bf = max 1 (min calibCap b')
                !ns <- noiseReading meter act bf
                pure $! finish bf False (Just ns) ps'
      finish !bf !cap !ns !ps = CalibReport
        { crBatch     = bf
        , crProbes    = reverse ps
        , crCapHit    = cap
        , crTargetMag = calibTarget
        , crNoise     = ns
        }
  probe 1 []

-- median reading over a few measurements at batch @b@: robust to a
-- single 0 (fast target straddling the quantum) or a single spike
-- (a scheduler hiccup on the wall clock).
probeReading :: Meter -> IO () -> Int -> IO Word64
probeReading meter act b = fmap median (collect calibProbes)
  where
    collect 0 = pure []
    collect i = do
      !r <- measure meter b act
      fmap (r :) (collect (i - 1 :: Int))

median :: [Word64] -> Word64
median xs = case drop (length xs `div` 2) (sort xs) of
  (m : _) -> m
  []      -> 0

-- dispersion of repeated readings at the selected batch: median and
-- interquartile range (nearest-rank quantiles), in per-batch meter
-- units. This is the meter's noise floor at the scale the driver
-- will observe, recorded as a snapshot of measurement conditions.
noiseReading :: Meter -> IO () -> Int -> IO Noise
noiseReading meter act b = do
  rs <- collect noiseSamples
  let !srt = sort rs
      !n   = length srt
      !med = nth (n `div` 2) srt
      !q1  = nth (n `div` 4) srt
      !q3  = nth (3 * n `div` 4) srt
  pure Noise { nsMedian = med, nsIqr = q3 - q1 }
  where
    collect 0 = pure []
    collect i = do
      !r <- measure meter b act
      fmap (r :) (collect (i - 1 :: Int))

nth :: Int -> [Word64] -> Word64
nth k xs = case drop k xs of
  (v : _) -> v
  []      -> 0

noiseSamples :: Int
noiseSamples = 15

-- Target per-batch reading magnitude (meter units) and probe budget.
-- 65536 matches the known-good hand-tuned batch for nanosecond-scale
-- targets (batch ~2000 at ~30 ns/call); erring high favours power,
-- since an underpowered default that misses a real leak is the
-- dangerous failure mode for a constant-time tester.
calibTarget :: Double
calibTarget = 65536

calibCap :: Int
calibCap = 1000000

calibProbes :: Int
calibProbes = 5

-- | Convert a t'Hypothesis' into its /A\/A diagnostic/ form: 'sampleB'
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

-- | Convert a t'Hypothesis' into its /B\/B diagnostic/ form:
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

-- | The outcome of the A\/A and B\/B /negative controls/.
--
--   These are controls, not an attribution mechanism: they can
--   convict the harness, but they cannot acquit it. A\/A compares
--   'sampleA' against itself and B\/B compares 'sampleB' against
--   itself, so an asymmetry that exists only /between/ the two
--   samplers is invisible to both. If, say, 'sampleA' returns a
--   fully forced input while 'sampleB' leaves a thunk that the
--   target forces inside the timed region, the primary run rejects
--   on that harness artefact while both controls pass.
data ControlVerdict
  = ControlsPass
    -- ^ Neither control rejected. No within-class harness asymmetry
    --   was found — which is weaker than \"the harness is
    --   symmetric\", and weaker still than \"the target leaks\".
    --   A 'Reject' from 'runCT' alongside this verdict is
    --   /consistent with/ a target leak; establishing that it /is/
    --   one needs harness symmetry argued by construction, or an
    --   independent harness.
  | SampleAAsymmetry
    -- ^ The A\/A control rejected: 'sampleA' itself induces a
    --   timing asymmetry, and the primary verdict is not
    --   informative until it is fixed.
  | SampleBAsymmetry
    -- ^ The B\/B control rejected: as above, for 'sampleB'.
  | PervasiveAsymmetry
    -- ^ Both controls rejected: the harness asymmetry is not
    --   specific to either sampler.
  deriving (Eq, Show)

-- | The full result of running the negative controls: the verdict
--   plus both control runs in their entirety, so a caller can
--   serialise or inspect the evidence rather than trusting a label.
data Attribution = Attribution
  { attVerdict :: !ControlVerdict
    -- ^ the combined outcome.
  , attAA      :: !Result
    -- ^ the complete A\/A control run.
  , attBB      :: !Result
    -- ^ the complete B\/B control run.
  } deriving Show

-- | Run the A\/A and B\/B negative controls and combine their
--   outcomes. Only meaningful after 'runCT' has rejected: on a
--   symmetric harness both controls pass whether or not the target
--   leaks, so 'ControlsPass' qualifies an existing rejection rather
--   than detecting anything by itself.
attribute :: Meter -> Config -> Hypothesis a -> IO Attribution
attribute meter cfg hyp = do
  raa <- runAA meter cfg hyp
  rbb <- runBB meter cfg hyp
  let !v = case (raa, rbb) of
        (Pass{},   Pass{})   -> ControlsPass
        (Reject{}, Pass{})   -> SampleAAsymmetry
        (Pass{},   Reject{}) -> SampleBAsymmetry
        (Reject{}, Reject{}) -> PervasiveAsymmetry
  pure $! Attribution
    { attVerdict = v
    , attAA      = raa
    , attBB      = rbb
    }

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
--
--   The hypothesis's 'prepare' prologue runs first, before either
--   sampler and outside the timed region, so anything it
--   establishes (a shared public context) is common to both classes
--   and its cost is not charged to whichever class happens to run
--   first.
runPair
  :: Meter -> Config -> Hypothesis a -> Bool -> IO (Word64, Word64)
runPair meter cfg hyp flipOrder = do
  prepare hyp
  if flipOrder
    then do
      !b <- sampleB hyp
      !y <- measure meter (cfgBatch cfg) (target hyp b)
      !a <- sampleA hyp
      !x <- measure meter (cfgBatch cfg) (target hyp a)
      pure (x, y)
    else do
      !a <- sampleA hyp
      !x <- measure meter (cfgBatch cfg) (target hyp a)
      !b <- sampleB hyp
      !y <- measure meter (cfgBatch cfg) (target hyp b)
      pure (x, y)
{-# INLINE runPair #-}

-- The hedge composition ----------------------------------------------------

-- A family of e-processes, each betting on a different
-- antisymmetric functional of the measured pair @(ta, tb)@,
-- combined via "Numeric.Eproc.Mixture" (a uniform convex hedge).
-- Antisymmetry (@g(ta, tb) = -g(tb, ta)@) is what makes each
-- component's input conditionally symmetric about zero under
-- exchangeability, hence a valid null for a two-sided mean-zero
-- test:
--
--   * /sign/: a two-sided Bernoulli test at @p_0 = 1\/2@ on
--     @sign d@, @d = ta - tb@ (ties skipped, valid because non-tie
--     steps are conditionally fair under symmetric @d@). No bounds,
--     no warmup calibration; unbeatable against shape leaks that
--     move @P(d > 0)@ off @1\/2@ and heavy-tail scenarios where a
--     small bulk shift is swamped by tail variance in the
--     magnitude channel.
--
--   * /magn(c), magn(4c), magn(16c)/: three two-sided bounded-mean
--     tests on @d@ clipped to progressively wider intervals. The
--     tight bound gives the largest @lambda_max@ and dominates
--     against small consistent bulk shifts; the wider bounds
--     de-fragilize the single warmup-derived @c@ against
--     rare-outlier leaks (a rarely-taken data-dependent branch
--     showing up as occasional slow paths).
--
--   * /cdf(q)/: one two-sided bounded-mean test on
--     @1[ta <= q] - 1[tb <= q]@ per warmup-fixed cut point @q@.
--     Each has conditional mean @F_A(q) - F_B(q)@, so together they
--     see any difference between the two classes' CDFs at the cut
--     points — including dispersion leaks, which leave @d@
--     symmetric and are therefore invisible to every other
--     component. The summand lies in @{-1, 0, 1}@ by construction,
--     so the bound needs no calibration.
--
-- The driver steps each component itself (they are heterogeneous)
-- and feeds the vector of their calibrated log e-values to
-- 'EPMix.update' once per pair, upholding the mixture's lockstep
-- precondition: on a tie the sign component's e-value is simply
-- unchanged, which is still its current value at that step. The
-- mixture owns the rejection latch (the supremum of the /mixture's/
-- log e-value, never per-component suprema, which peak at different
-- times) and the threshold @log(K \/ alpha)@ on the component sum.
-- @K@ is @4 + n@ for @n@ resolved cut points, so the hedging price
-- is @log K@ nats — against @log(1 \/ alpha) = 13.8@ at the default
-- alpha, adding the CDF channel costs well under a nat.

data HedgeCfg = HedgeCfg
  { hcSign  :: !EPSign.Config
  , hcMagn1 :: !EPMagn.Config
  , hcMagn2 :: !EPMagn.Config
  , hcMagn3 :: !EPMagn.Config
  , hcCdf   :: !EPMagn.Config          -- shared by every cut point
  , hcQs    :: ![Double]               -- the cut points themselves
  , hcMix   :: !EPMix.Config
  , hcC1    :: {-# UNPACK #-} !Double  -- tight clip level (base c)
  , hcC2    :: {-# UNPACK #-} !Double  -- 4c
  , hcC3    :: {-# UNPACK #-} !Double  -- 16c
  }

data HedgeState = HedgeState
  { hsSign  :: !EPSign.State
  , hsMagn1 :: !EPMagn.State
  , hsMagn2 :: !EPMagn.State
  , hsMagn3 :: !EPMagn.State
  , hsCdf   :: ![EPMagn.State]         -- aligned with 'hcQs'
  , hsMix   :: !EPMix.State
  }

mkHedgeCfg
  :: Double -> Double -> [Double] -> Either ConfigError HedgeCfg
mkHedgeCfg !alpha !c qs = do
  let !c2 = 4  * c
      !c3 = 16 * c
  !sc <- EPSign.config 0.5 alpha Newton
  !m1 <- EPMagn.config 0 (negate c)  c  alpha Newton
  !m2 <- EPMagn.config 0 (negate c2) c2 alpha Newton
  !m3 <- EPMagn.config 0 (negate c3) c3 alpha Newton
  !cd <- EPMagn.config 0 (negate 1)  1  alpha Newton
  !mx <- EPMix.config (4 + length qs) alpha
  pure HedgeCfg
    { hcSign  = sc
    , hcMagn1 = m1
    , hcMagn2 = m2
    , hcMagn3 = m3
    , hcCdf   = cd
    , hcQs    = qs
    , hcMix   = mx
    , hcC1    = c
    , hcC2    = c2
    , hcC3    = c3
    }

initialHedge :: HedgeCfg -> HedgeState
initialHedge HedgeCfg{..} = HedgeState
  { hsSign  = EPSign.initial hcSign
  , hsMagn1 = EPMagn.initial hcMagn1
  , hsMagn2 = EPMagn.initial hcMagn2
  , hsMagn3 = EPMagn.initial hcMagn3
  , hsCdf   = map (const (EPMagn.initial hcCdf)) hcQs
  , hsMix   = EPMix.initial hcMix
  }

updateHedge
  :: HedgeCfg -> HedgeState -> Double -> Double -> Double -> HedgeState
updateHedge HedgeCfg{..} HedgeState{..} !d !ta !tb =
  let !s'  = if d == 0
             then hsSign
             else EPSign.update hcSign hsSign (d > 0)
      !m1' = EPMagn.update hcMagn1 hsMagn1 (clipD hcC1 d)
      !m2' = EPMagn.update hcMagn2 hsMagn2 (clipD hcC2 d)
      !m3' = EPMagn.update hcMagn3 hsMagn3 (clipD hcC3 d)
      !cd' = stepCdf hcCdf hcQs hsCdf ta tb
      !x'  = EPMix.update hcMix hsMix
               ( EPSign.log_evalue s'
               : EPMagn.log_evalue m1'
               : EPMagn.log_evalue m2'
               : EPMagn.log_evalue m3'
               : map EPMagn.log_evalue cd'
               )
  in  HedgeState
        { hsSign  = s'
        , hsMagn1 = m1'
        , hsMagn2 = m2'
        , hsMagn3 = m3'
        , hsCdf   = cd'
        , hsMix   = x'
        }

-- Step every CDF-indicator component. Spine- and element-strict so
-- the per-pair states don't accumulate thunks across the budget.
stepCdf
  :: EPMagn.Config -> [Double] -> [EPMagn.State] -> Double -> Double
  -> [EPMagn.State]
stepCdf !cfg = go
  where
    go (q : qs) (s : ss) !ta !tb =
      let !s'  = EPMagn.update cfg s (cdfDiff q ta tb)
          !ss' = go qs ss ta tb
      in  s' : ss'
    go _ _ _ _ = []

-- The antisymmetric indicator functional at cut point @q@:
-- @1[ta <= q] - 1[tb <= q]@, valued in @{-1, 0, 1}@, with
-- conditional mean @F_A(q) - F_B(q)@.
cdfDiff :: Double -> Double -> Double -> Double
cdfDiff !q !ta !tb = ind ta - ind tb
  where
    ind !t | t <= q    = 1
           | otherwise = 0
{-# INLINE cdfDiff #-}

-- Largest peak log e-value across the CDF components; @0@ (the
-- calibrated floor) when there are none.
peakCdf :: [EPMagn.State] -> Double
peakCdf = foldr (\s acc -> max (EPMagn.log_evalue_sup s) acc) 0

clipD :: Double -> Double -> Double
clipD !lim !x
  | abs x > lim = if x > 0 then lim else negate lim
  | otherwise   = x
{-# INLINE clipD #-}

-- The effect-estimation grid: interior candidate means over
-- @[-16c, 16c]@ for the confidence sequence behind 'resEffect'.
-- 300 candidates resolve the interval endpoints to ~0.1c,
-- comparable to the shift scale the tight magnitude component can
-- detect; the per-pair update cost is O(live candidates), falls as
-- evidence rejects candidates, and sits outside the timed region
-- (and is order-symmetric), so measurement hygiene is unaffected.
effectGrid :: Int
effectGrid = 300

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

-- Resolve the (root, warmup, main) seed triple. The root is the
-- user-supplied constant, or a fresh draw from OS entropy; we
-- splitmix off it to derive the warmup and main-phase slots. The
-- root is returned so the driver can report it as 'resSeed' and a
-- randomised run stays exactly replayable.
--
-- OS entropy rather than the monotonic clock: a clock reading is
-- not independent entropy, and the conditional-null argument needs
-- the order bit independent of the nuisance process — which may
-- itself be periodic in that very clock. 'getMonotonicTimeNSec' is
-- kept only as a fallback for hosts without a readable
-- /dev/urandom, where it is better than a constant.
resolveSeeds :: Maybe Word64 -> IO (Word64, Word64, Word64)
resolveSeeds mseed = do
  !base <- case mseed of
    Just s  -> pure s
    Nothing -> do
      mw <- osEntropy
      case mw of
        Just w  -> pure w
        Nothing -> getMonotonicTimeNSec
  let (!s1, !w1) = splitMix base
      (_ , !w2) = splitMix s1
  pure (base, w1, w2)

-- Eight bytes from /dev/urandom, big-endian. 'Nothing' on any IO
-- failure (absent device, sandbox, exhausted descriptors).
osEntropy :: IO (Maybe Word64)
osEntropy = fmap toWord (try (withBinaryFile urandom ReadMode grab))
  where
    urandom = "/dev/urandom"
    grab h  = BS.hGet h 8
    toWord (Left e)   = const Nothing (e :: IOException)
    toWord (Right bs)
      | BS.length bs == 8 = Just $! BS.foldl' step 0 bs
      | otherwise         = Nothing
    step !acc !w = acc * 256 + fromIntegral w

-- Warmup collects per-pair @|d|@ (for the bound estimate) and the
-- flat list of per-class readings (for the meter-resolution
-- diagnosis). The order-bit randomisation matches runCT's main
-- loop so the p99 estimate doesn't inherit a parity-locked nuisance
-- bias.
--
-- Returns @(c, qs, meterResolves)@ where @c = max 1 (2 * p99(|d|))@
-- is the clipping bound, @qs@ are the CDF-indicator cut points, and
-- @meterResolves@ is True iff @p99(readings) > 0@ (i.e., the meter
-- is producing nonzero readings often enough that at least the
-- upper tail is resolved). If @meterResolves@ is False the meter's
-- quantum is swallowing at least 99% of per-target timings and the
-- caller throws 'WarmupZeroDuration' with the "raise cfgBatch"
-- hint. All-zero /differences/ with well-resolved readings are the
-- PMU-on-truly-CT happy path — we fall back to @c = 1@ and let the
-- main loop proceed; under continued zero differences the wealth
-- process stays flat and the run passes at budget.
--
-- The cut points are order statistics of the /pooled/ readings, so
-- they are fixed before the main phase and identical for both
-- classes — which is what keeps the indicator functional
-- antisymmetric, and hence the null intact. Pooling also means a
-- cut point carries no class information. Duplicates are dropped:
-- on a discrete meter several levels can land on the same reading,
-- and a repeated cut point would only inflate @K@.
warmup
  :: Meter -> Config -> Hypothesis a -> Word64
  -> IO (Double, [Double], Bool)
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
  let !sortedR = sort readings
      !p99d    = orderStat99 diffs
      !p99r    = orderStat99 readings
      !qs      = cutPoints sortedR
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
  pure (max 1 (2 * p99d), qs, p99r > 0)

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

-- The levels at which the CDF-indicator components cut the pooled
-- warmup readings. Spread across the bulk rather than the tails:
-- a cut point out at the extremes leaves both indicators equal on
-- nearly every pair, contributing no evidence while still costing
-- its share of @log K@. The topmost level is deliberately below
-- the p99 that sets the clip bound, so the two calibrations do not
-- collapse onto the same statistic.
cdfLevels :: [Double]
cdfLevels = [0.10, 0.25, 0.50, 0.75, 0.90]

-- Nearest-rank order statistics of a sorted reading list at
-- 'cdfLevels', ascending and deduplicated. Empty when warmup
-- collected nothing.
cutPoints :: [Word64] -> [Double]
cutPoints sorted
  | m == 0    = []
  | otherwise = dedupAsc (map pick cdfLevels)
  where
    !m = length sorted
    pick !p =
      let !idx = max 0 (min (m - 1) (floor (p * fromIntegral m)))
      in  case drop idx sorted of
            (x : _) -> fromIntegral x
            []      -> 0

-- Drop repeats from an ascending list.
dedupAsc :: [Double] -> [Double]
dedupAsc (x : y : rest)
  | x == y    = dedupAsc (y : rest)
  | otherwise = x : dedupAsc (y : rest)
dedupAsc xs = xs
