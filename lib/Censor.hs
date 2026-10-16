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
-- 'runCT' measures paired class-A \/ class-B timings in random order,
-- streams them through an anytime-valid test, and either rejects (a
-- leak is detected) or exhausts its budget. Type-I error is
-- controlled at 'cfgAlpha' whenever the run stops.
--
-- = The null
--
-- The null is /conditional exchangeability/ of the measured pair
-- @(ta, tb)@: given the past, swapping the class labels leaves the
-- joint law unchanged. The randomised per-pair order supplies it,
-- provided the nuisance dynamics within a pair (cache and predictor
-- state, frequency) are blind to the class labels; randomisation
-- cannot undo a nuisance process that reacts to the class itself.
--
-- = The test
--
-- Every component bets on an /antisymmetric functional/ of the pair
-- (@g(ta, tb) = -g(tb, ta)@), which is conditionally symmetric about
-- zero under the null. The test is a uniform mixture of e-processes
-- ("Numeric.Eproc.Mixture") over a family of them, with
-- @d = ta - tb@:
--
--   * /sign/: @sign d@. Catches an asymmetry of @d@ itself,
--     including bulk shifts that heavy tails hide from the
--     magnitude components.
--   * /magnitude/: @d@ clipped to @[-c, c]@, @[-4c, 4c]@, and
--     @[-16c, 16c]@, for a warmup-estimated @c@. The tight bound is
--     the most powerful against small consistent shifts; the wide
--     ones keep rare outliers.
--   * /CDF/: @1[ta <= q] - 1[tb <= q]@ at warmup-fixed cut points
--     @q@. Catches dispersion leaks, which leave @d@ symmetric.
--
-- The family is finite, so the test is not exhaustive: a departure
-- that moves none of these functionals off zero passes. Read a
-- 'Pass' as \"no evidence on these channels\", not as proof of
-- constant time.

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
  , resPeakCdf

    -- * Advisories
  , Advisory(..)
  , ClipRates(..)
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

    -- * Split runs
  , Split(..)
  , runSplit
  , runSplitWith
  , attributeSplit

    -- * Batch calibration
  , calibrateBatch
  , CalibReport(..)
  , calibrateBatchReport
  , Noise(..)

    -- * Baseline probe
  , baselineReading

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
  , SplitMeter(..)
  , withSplitMeter

    -- * Sampler support (re-exports)
  , Rng
  , mkRng
  , nextWord
  , reseed
  , randomBytes
  ) where

import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (unless)
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort, sortBy)
import Data.Primitive.SmallArray
  (SmallArray, indexSmallArray, smallArrayFromListN)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (IOMode(ReadMode), withBinaryFile)
import qualified Numeric.Eproc.Bernoulli.TwoSided as EPSign
import qualified Numeric.Eproc.Bounded as EPMagn
import qualified Numeric.Eproc.ConfSeq as EPCS
import qualified Numeric.Eproc.Mixture as EPMix
import Numeric.Eproc.Bounded (Bettor(Newton), ConfigError)

import Censor.Meter
import Censor.Rng
  (Rng, mkRng, nextWord, randomBytes, reseed, splitMix)

-- | A constant-time hypothesis.
--
--   * @target@: the @IO@ action under test.
--   * @prepare@: a per-pair prologue, run before either sampler and
--     outside the timed region; @pure ()@ unless the classes share
--     per-pair state (see 'fixVsRandomCtx').
--   * @sampleA@, @sampleB@: samplers for the two input classes,
--     typically \"fixed\" and \"random\".
--
--   Only @target@ is timed, so any asymmetry in how the samplers
--   prepare inputs shows up as a class difference. Return inputs in
--   normal form, build the fixed class by the same code path as the
--   random one (not as a top-level constant), and keep the samplers
--   symmetric in allocation and in where the secret is read from.
--   'fixVsRandom' does this for the usual case.
data Hypothesis a = Hypothesis
  { target  :: !(a -> IO ())
  , prepare :: !(IO ())
  , sampleA :: !(IO a)
  , sampleB :: !(IO a)
  }

-- | A dudect-style /fix-vs-random/ t'Hypothesis', from a target, a
--   secret sampler, a re-materialisation function, and a completion
--   building a full input around a secret.
--
--   The fixed secret is drawn once from the sampler, so it is an
--   ordinary heap value like the random class's. Each sample, class
--   A draws and discards a fresh secret and class B keeps its draw,
--   so the samplers allocate alike. Both classes then re-materialise
--   the pinned secret: class A completes around the copy, class B
--   discards it. With a real copy (for a 'BS.ByteString',
--   @\\s -> evaluate (BS.copy s)@), neither class is alone in reading
--   one long-lived, cache-hot value, a locality asymmetry that the
--   A\/A and B\/B controls cannot see. Pass 'pure' only for types
--   with no cheap copy.
--
--   The completion runs identically in both classes, so per-pair
--   public inputs stay fresh on both sides. The sampler,
--   re-materialiser, and completion must return values in normal
--   form.
--
--   > hyp <- fixVsRandom
--   >   (\x -> () <$ evaluate (inv x))   -- target
--   >   (randomMont g)                   -- secret sampler
--   >   pure                             -- no re-materialiser
--   >   pure                             -- input is just the secret
fixVsRandom
  :: (a -> IO ())   -- ^ target
  -> IO s           -- ^ secret sampler
  -> (s -> IO s)    -- ^ re-materialise a secret into a fresh value
  -> (s -> IO a)    -- ^ complete an input around a secret
  -> IO (Hypothesis a)
fixVsRandom tgt sec dup embed = do
  !fix <- sec
  pure Hypothesis
    { target  = tgt
    , prepare = pure ()
    , sampleA = do
        !_ <- sec        -- draw and discard: balance class B
        !s <- dup fix
        embed s
    , sampleB = do
        !s <- sec
        !_ <- dup fix    -- copy and discard: balance class A
        embed s
    }

-- | A /fix-vs-random/ t'Hypothesis' whose two classes share one
--   freshly drawn public context per pair.
--
--   'fixVsRandom' completes each class independently, so per-pair
--   public inputs (message, nonce, modulus) either differ across the
--   pair, adding nuisance variance to @d@, or are pinned for the
--   whole run, testing a single public input. Here 'prepare' draws
--   one context per pair and both classes are built around it, so
--   the test is of secret dependence given identical fresh public
--   input. The null is unaffected: given the context, the two
--   readings are still exchangeable under H_0.
--
--   The re-materialisation function plays the same role as in
--   'fixVsRandom'.
--
--   > hyp <- fixVsRandomCtx
--   >   (\(k, m) -> () <$ evaluate (mac k m))  -- target
--   >   (randomBytes g 32)                     -- secret sampler
--   >   (\k -> evaluate (BS.copy k))           -- re-materialise
--   >   (randomBytes g 256)                    -- public context
--   >   (\msg key -> pure (key, msg))          -- build the input
fixVsRandomCtx
  :: (a -> IO ())      -- ^ target
  -> IO s              -- ^ secret sampler
  -> (s -> IO s)       -- ^ re-materialise a secret into a fresh value
  -> IO c              -- ^ public-context sampler (once per pair)
  -> (c -> s -> IO a)  -- ^ build an input from context and secret
  -> IO (Hypothesis a)
fixVsRandomCtx tgt sec dup ctx embed = do
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
        !s <- dup fix
        !c <- readIORef ref
        embed c s
    , sampleB = do
        !s <- sec
        !_ <- dup fix    -- copy and discard: balance class A
        !c <- readIORef ref
        embed c s
    }

-- | Driver configuration.
data Config = Config
  { cfgAlpha  :: !Double
    -- ^ significance level.
  , cfgBudget :: !Int
    -- ^ maximum sample pairs to consume.
  , cfgBatch  :: !Int
    -- ^ target repetitions per timed region, all on one pre-drawn
    --   sample. This multiplies real work only when each repetition
    --   recomputes (an FFI call, mutable state). For a pure target
    --   @\\a -> evaluate (f a)@ the thunk is forced once and the rest
    --   are no-ops, so use @1@, and widen the work if it is
    --   sub-quantum.
  , cfgWarmup :: !Int
    -- ^ warmup pairs, from which the clip bound @c@ (twice the
    --   @~p99@ of @|d|@) and the CDF cut points are fixed. At @100@
    --   or fewer, the @p99@ is the sample maximum.
  , cfgSeed   :: !(Maybe Word64)
    -- ^ seed for the per-pair A\/B order bit. 'Nothing' (the
    --   default) draws one from @\/dev\/urandom@, falling back to the
    --   monotonic clock if that is unreadable. The seed used is
    --   reported as 'resSeed' for replay. Keep it independent of any
    --   sampler seed: the null needs the order bit independent of the
    --   timings.
  , cfgMargin :: !(Maybe Double)
    -- ^ relative interval-null margin. 'Nothing' (the default) tests
    --   the sharp null. @Just m@ resolves at warmup to an absolute
    --   margin @delta = m * median@ of the pooled per-batch readings
    --   and tests @|E d_clipped| <= delta@: the magnitude components
    --   become interval-null tests and alone gate the verdict, while
    --   the sign and CDF components still report peaks as
    --   diagnostics. Relative, because the channels that motivate it
    --   (frequency, interrupts, scheduling) scale with the
    --   timed-region duration. Meant for the wall meter on
    --   frequency-scaled hosts (see the README); keep it 'Nothing'
    --   under the PMU meters. The resolved margin must land in
    --   @(0, c)@, else the run throws 'InvalidConfig'; it is reported
    --   as 'resMargin'.
  } deriving Show

-- | Defaults: @alpha = 1e-6@, budget @100000@ pairs, batch 1 (no
--   inner repetition), warmup 200 pairs, order-bit seed drawn from
--   OS entropy per run, no margin (sharp null).
defaultConfig :: Config
defaultConfig = Config
  { cfgAlpha  = 1.0e-6
  , cfgBudget = 100000
  , cfgBatch  = 1
  , cfgWarmup = 200
  , cfgSeed   = Nothing
  , cfgMargin = Nothing
  }

-- | Test outcome: 'Reject' (a leak was detected) or 'Pass' (none
--   within budget), each carrying the same run report.
--
--   Evidence is on the log e-value scale: a run starts at @0@ and
--   rejects when the mixture's running supremum ('resPeakLogW')
--   crosses @log(1 \/ alpha)@. The per-component peaks say which
--   channel found the evidence. They are diagnostic only: each is a
--   supremum at its own time, and the verdict latches on the
--   mixture.
data Result
  = Reject
      { resPairs      :: {-# UNPACK #-} !Int
        -- ^ sample pairs consumed.
      , resPeakLogW   :: {-# UNPACK #-} !Double
        -- ^ peak (supremum-so-far) log e-value of the mixture;
        --   starts at @0@, crosses @log(1 \/ alpha)@ on rejection.
      , resPValue     :: {-# UNPACK #-} !Double
        -- ^ the anytime-valid p-value @min 1 (exp -resPeakLogW)@;
        --   at or below 'cfgAlpha' iff the verdict is 'Reject'.
      , resEffect     :: !(Double, Double)
        -- ^ anytime-valid @1 - cfgAlpha@ confidence interval for the
        --   mean of @d@ clipped to @[-16c, 16c]@, in meter units per
        --   batch. Covers zero on a constant-time target. It can still
        --   be wide after an early 'Reject': detection outpaces
        --   estimation. Once the evidence pins the mean below the
        --   estimation grid's resolution, the last resolvable interval
        --   is reported. 'resClipped16' says how often the clipping
        --   bit.
      , resClipped    :: {-# UNPACK #-} !Int
        -- ^ pairs whose @|d|@ exceeded 'resBound' and was clipped for
        --   the tight magnitude component. A large share of
        --   'resPairs' means the bound was too tight: raise
        --   'cfgWarmup' or 'cfgBatch'.
      , resClipped4   :: {-# UNPACK #-} !Int
        -- ^ pairs whose @|d|@ exceeded @4 * 'resBound'@.
      , resClipped16  :: {-# UNPACK #-} !Int
        -- ^ pairs whose @|d|@ exceeded @16 * 'resBound'@, the bound
        --   behind the widest magnitude component and 'resEffect'.
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
      , resCdf        :: ![(Double, Double)]
        -- ^ the CDF channel: each warmup-fixed cut point (ascending)
        --   with its component's peak log e-value, so a CDF-driven
        --   rejection names the threshold that separated the classes.
        --   Empty when warmup readings were degenerate.
      , resSeed       :: {-# UNPACK #-} !Word64
        -- ^ the order-bit seed the run used; feed it back as
        --   'cfgSeed' to replay the same A\/B order.
      , resMargin     :: !(Maybe Double)
        -- ^ the /absolute/ margin resolved from 'cfgMargin', in meter
        --   units per batch; 'Nothing' under the sharp null. When
        --   present, a 'Reject' means the mean effect exceeds it, and
        --   a high 'resPeakSign' on a 'Pass' marks a systematic within
        --   tolerance.
      }
  | Pass
      { resPairs      :: {-# UNPACK #-} !Int
      , resPeakLogW   :: {-# UNPACK #-} !Double
      , resPValue     :: {-# UNPACK #-} !Double
      , resEffect     :: !(Double, Double)
      , resClipped    :: {-# UNPACK #-} !Int
      , resClipped4   :: {-# UNPACK #-} !Int
      , resClipped16  :: {-# UNPACK #-} !Int
      , resBound      :: {-# UNPACK #-} !Double
      , resPeakSign   :: {-# UNPACK #-} !Double
      , resPeakMagn   :: {-# UNPACK #-} !Double
      , resPeakMagn4  :: {-# UNPACK #-} !Double
      , resPeakMagn16 :: {-# UNPACK #-} !Double
      , resCdf        :: ![(Double, Double)]
      , resSeed       :: {-# UNPACK #-} !Word64
      , resMargin     :: !(Maybe Double)
      }
  deriving Show

-- | The largest peak log e-value across the CDF-indicator
--   components — @0@ when warmup resolved no cut points. A maximum
--   over 'resCdf', which keeps the per-cut-point detail.
resPeakCdf :: Result -> Double
resPeakCdf = foldr (\(_, p) acc -> max p acc) 0 . resCdf

-- | An actionable observation about a t'Result', produced by
--   'diagnose'.
data Advisory
  = HighClipRate !ClipRates
    -- ^ More than 5% of pairs were clipped at @c@, starving the tight
    --   magnitude component: raise 'cfgWarmup' or 'cfgBatch', or
    --   treat the run with suspicion. Carries the rate at all three
    --   bounds; a nonzero rate at @16c@ means 'resEffect' describes a
    --   truncated variable.
  | LowPower !Double
    -- ^ On 'Pass' only: the effect-interval half-width exceeded the
    --   tight clip bound @c@ (the ratio is carried here, in units of
    --   @c@). Leaks with a mean shift below that scale would not have
    --   been resolved by this run; raise 'cfgBudget' to sharpen the
    --   interval.
  | RejectionDriver !LeakShape
    -- ^ On 'Reject' only: which channel of the hedge drove the
    --   detection, i.e. what shape of leak was found.
  deriving (Eq, Show)

-- | The share of pairs clipped at each of the hedge's three
--   magnitude bounds, carried by 'HighClipRate'. Nested by
--   construction: @clipWidest <= clipWide <= clipTight@, since
--   @|d| > 16c@ implies @|d| > 4c@ implies @|d| > c@.
data ClipRates = ClipRates
  { clipTight  :: {-# UNPACK #-} !Double
    -- ^ fraction of pairs with @|d| > c@.
  , clipWide   :: {-# UNPACK #-} !Double
    -- ^ fraction of pairs with @|d| > 4c@.
  , clipWidest :: {-# UNPACK #-} !Double
    -- ^ fraction of pairs with @|d| > 16c@ — the bound behind
    --   'resEffect'.
  } deriving (Eq, Show)

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

-- | Interpret a t'Result': heavy clipping, a low-power 'Pass' (the
--   effect interval is wider than the tight bound), and, on
--   'Reject', which channel of the hedge drove the detection. Always
--   in that order, so tooling can match by position.
diagnose :: Result -> [Advisory]
diagnose r =
  let rate n = if resPairs r == 0
                 then 0
                 else fromIntegral n / (fromIntegral (resPairs r) :: Double)
      !rates = ClipRates (rate (resClipped r)) (rate (resClipped4 r))
                         (rate (resClipped16 r))
      -- the counts nest, so the tight rate is the largest of the
      -- three and one trigger covers the profile
      clipA = [HighClipRate rates | clipTight rates > 0.05]
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
--
--   Under a margin-mode run ('resMargin' present) only the gating
--   magnitude components are ranked: the sign and CDF peaks did not
--   drive the verdict there, and a sub-margin systematic routinely
--   inflates them.
leakShape :: Result -> LeakShape
leakShape r =
  let !sig  = resPeakSign r
      !mag  = resPeakMagn r
      !tl   = max (resPeakMagn4 r) (resPeakMagn16 r)
      !cdf  = resPeakCdf r
      candidates = case resMargin r of
        Just _  -> [ (mag, BulkShift)
                   , (tl,  RareOutlier)
                   ]
        Nothing -> [ (sig, ShapeLeak)
                   , (mag, BulkShift)
                   , (tl,  RareOutlier)
                   , (cdf, CdfShift)
                   ]
      ranked = sortBy (\a b -> compare (fst b) (fst a)) candidates
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

-- | Run a constant-time test, handing a t'Frame' to the observer after
--   every main-phase pair. The observer runs outside the timed
--   region and gets the frame lazily, so the no-op observer of
--   'runCT' costs nothing; a recording one reconstructs the
--   trajectory (see @record@ in "Censor.Runner").
--
--   1. /Warmup/: @cfgWarmup@ pairs fix the clip bound @c@ and the
--      CDF cut points (order statistics of the pooled readings, so
--      they carry no class information).
--   2. /Main/: each pair runs in random order, and every
--      component's functional of @(ta, tb)@ feeds its e-process. The
--      run halts when the mixture crosses @1 \/ alpha@ or the budget
--      runs out. @d@ clipped to @[-16c, 16c]@ also feeds the
--      confidence sequence behind 'resEffect'.
--
--   With 'cfgMargin' set, warmup also resolves the absolute margin,
--   and only the magnitude components gate the verdict.
--
--   Betting on @d@ rather than on the raw readings lets the bet size
--   scale with the noise, @p99(|d|)@, rather than with the target's
--   runtime: a large gain for slow targets with little jitter.
runCTWith
  :: Meter -> Config -> Hypothesis a -> (Frame -> IO ()) -> IO Result
runCTWith meter cfg hyp observe = do
  validateConfig cfg
  (!root, !wSeed, !mSeed) <- resolveSeeds (cfgSeed cfg)
  let !samplers = orderedSamplers hyp
      pair      = runPair meter cfg hyp samplers
  ws  <- warmup cfg pair wSeed
  !ch <- channel cfg [ (fromIntegral x, fromIntegral y) | (x, y) <- ws ]
  let go !n !st !rng
        | n >= cfgBudget cfg = pure $! chanResult ch root n st
        | otherwise = do
            let (!rng', !bit) = nextOrderBit rng
            (!ta, !tb) <- pair bit
            let !xa  = fromIntegral ta :: Double
                !xb  = fromIntegral tb :: Double
                !st' = stepChan ch st xa xb
                !n'  = n + 1
            -- lazy: the frame's fields (mixture reads) are forced
            -- only if the observer forces them, so runCT pays nothing.
            observe (chanFrame ch n' (xa - xb) st')
            if chanRejects ch st'
              then pure $! chanResult ch root n' st'
              else go n' st' rng'
  go 0 (chanInitial ch) mSeed

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

-- | One value per channel of a split run ('runSplitWith'): a
--   t'Result' each, or the controls' t'Attribution' each.
data Split a = Split
  { splitCycles :: !a
    -- ^ the cycle count: whether the code's execution depends on
    --   the class. The verdict is this channel's.
  , splitPeriod :: !a
    -- ^ wall time per cycle, in femtoseconds: whether the clock the
    --   cycles ran at does.
  , splitWall   :: !a
    -- ^ wall time, in nanoseconds: the two together, as elapsed
    --   time shows them to an observer.
  } deriving (Eq, Show)

instance Functor Split where
  fmap f (Split c p w) = Split (f c) (f p) (f w)

-- | Run a constant-time test under a t'SplitMeter', handing the
--   cycles channel's t'Frame' to the observer after every
--   main-phase pair.
--
--   Every pair is read once, as cycles and wall time, and tested on
--   three channels: cycles, the clock period (wall time per cycle),
--   and wall time. Each channel is a full test as 'runCTWith'
--   describes, with its own clip bound and cut points fixed by the
--   same warmup pairs, and each holds its own type-I error at
--   'cfgAlpha'. The cycles channel decides when the run stops: on
--   its rejection, or at the budget. That stop is a stopping time
--   for the readings of all three, and the order bit is independent
--   of all three, so the other two channels' guarantees hold where
--   it falls.
--
--   The cycles channel answers the constant-time question. The
--   period channel answers whether the clock moved with the class,
--   as it does when the core's frequency follows data-dependent
--   power; wall time answers whether elapsed time moved, by either
--   route. A wall rejection beside a cycles pass, with the period
--   rejecting too, is a frequency effect rather than a code path.
--
--   The period reads cleanly only beside a cycles pass: the counted
--   region carries a fixed overhead (the clock reads), so wall time
--   per cycle falls as cycles grow, and a cycles difference
--   registers on the period as well.
runSplitWith
  :: SplitMeter -> Config -> Hypothesis a -> (Frame -> IO ())
  -> IO (Split Result)
runSplitWith meter cfg hyp observe = do
  validateConfig cfg
  (!root, !wSeed, !mSeed) <- resolveSeeds (cfgSeed cfg)
  let !samplers = orderedSamplers hyp
      pair      = runSplitPair meter cfg hyp samplers
  ws <- warmup cfg pair wSeed
  let rs = fmap splitReadings ws
  !cc <- channel cfg (fmap splitCycles rs)
  !cp <- channel cfg (fmap splitPeriod rs)
  !cw <- channel cfg (fmap splitWall rs)
  let finish !n !sc !sp !sw = Split
        (chanResult cc root n sc)
        (chanResult cp root n sp)
        (chanResult cw root n sw)
      go !n !sc !sp !sw !rng
        | n >= cfgBudget cfg = pure $! finish n sc sp sw
        | otherwise = do
            let (!rng', !bit) = nextOrderBit rng
            !r <- pair bit
            case splitReadings r of
              Split (!ca, !cb) (!pa, !pb) (!wa, !wb) -> do
                let !sc' = stepChan cc sc ca cb
                    !sp' = stepChan cp sp pa pb
                    !sw' = stepChan cw sw wa wb
                    !n'  = n + 1
                observe (chanFrame cc n' (ca - cb) sc')
                if chanRejects cc sc'
                  then pure $! finish n' sc' sp' sw'
                  else go n' sc' sp' sw' rng'
  go 0 (chanInitial cc) (chanInitial cp) (chanInitial cw) mSeed

-- | 'runSplitWith' with the per-pair observer discarded.
runSplit :: SplitMeter -> Config -> Hypothesis a -> IO (Split Result)
runSplit meter cfg hyp = runSplitWith meter cfg hyp (\_ -> pure ())

-- | The A\/A and B\/B controls under a t'SplitMeter': each control
--   is one split run, and each channel's verdict comes from that
--   channel's readings of the two. Only meaningful for a channel
--   that rejected, as for 'attribute'.
attributeSplit
  :: SplitMeter -> Config -> Hypothesis a -> IO (Split Attribution)
attributeSplit meter cfg hyp = do
  Split ac ap aw <- runSplit meter cfg (toAA hyp)
  Split bc bp bw <- runSplit meter cfg (toBB hyp)
  pure $! Split (attribution ac bc) (attribution ap bp)
                (attribution aw bw)

-- | Pick a 'cfgBatch' that lifts the per-batch reading well above the
--   meter's resolution: probe a class-A sample at geometrically
--   growing batches until the reading reaches a target magnitude,
--   then scale to it. Only meaningful for targets that recompute on
--   every repetition (see 'cfgBatch').
--
--   > b <- calibrateBatch meter hyp
--   > r <- runCT meter cfg { cfgBatch = b } hyp
--
--   Consumes one 'prepare' and one class-A draw; build a fresh
--   hypothesis for the run if its sampler stream must start
--   undisturbed. Returns @1@ if the meter never resolves the target
--   (the run then throws 'WarmupZeroDuration'). The result is
--   meter-specific; pin 'cfgBatch' to compare meters.
calibrateBatch :: Meter -> Hypothesis a -> IO Int
calibrateBatch meter hyp = fmap crBatch (calibrateBatchReport meter hyp)

-- | The probe history behind 'calibrateBatchReport', for when the
--   chosen batch surprises.
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

-- | Dispersion of meter readings, in per-batch meter units (the scale
--   of 'resBound' and 'resEffect'). Deterministic meters read an IQR
--   of @0@.
data Noise = Noise
  { nsMedian :: {-# UNPACK #-} !Word64
    -- ^ median reading at the selected batch.
  , nsIqr    :: {-# UNPACK #-} !Word64
    -- ^ interquartile range of readings at the selected batch.
  } deriving (Eq, Show)

-- | 'calibrateBatch' with its t'CalibReport'.
calibrateBatchReport :: Meter -> Hypothesis a -> IO CalibReport
calibrateBatchReport meter hyp = do
  prepare hyp
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
probeReading meter act b =
  fmap (nsMedian . dispersion) (samples calibProbes (measure meter b act))

-- dispersion of repeated readings at the selected batch: median and
-- interquartile range (nearest-rank quantiles), in per-batch meter
-- units. This is the meter's noise floor at the scale the driver
-- will observe, recorded as a snapshot of measurement conditions.
noiseReading :: Meter -> IO () -> Int -> IO Noise
noiseReading meter act b =
  fmap dispersion (samples noiseSamples (measure meter b act))

-- run a reading action @n@ times, forcing each reading as it lands.
samples :: Int -> IO Word64 -> IO [Word64]
samples n act = go n
  where
    go 0 = pure []
    go i = do
      !r <- act
      fmap (r :) (go (i - 1))

-- median and nearest-rank interquartile range of a reading list.
dispersion :: [Word64] -> Noise
dispersion rs =
  let !srt = sort rs
      !n   = length srt
      !med = nth (n `div` 2) srt
      !q1  = nth (n `div` 4) srt
      !q3  = nth (3 * n `div` 4) srt
  in  Noise { nsMedian = med, nsIqr = q3 - q1 }

-- | Baseline cost of the target: median and IQR of single
--   measurements on fresh class-A draws (each after a 'prepare'), at
--   'cfgBatch', in the units of 'resEffect'. Unlike 'crNoise', which
--   remeasures one applied action (no-ops after the first, for a pure
--   target), every measurement here is genuine work. Give it its own
--   hypothesis instance if the run's sampler stream must start
--   undisturbed.
baselineReading :: Meter -> Config -> Hypothesis a -> IO Noise
baselineReading meter cfg hyp =
  fmap dispersion (samples baselineSamples fresh)
  where
    fresh = do
      prepare hyp
      !x <- sampleA hyp
      measure meter (cfgBatch cfg) (target hyp x)

baselineSamples :: Int
baselineSamples = 15

-- the @k@th element of a sorted list (nearest-rank order statistic);
-- @0@ past the end.
nth :: Num a => Int -> [a] -> a
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

-- | The /A\/A control/: 'sampleB' replaced by 'sampleA'. The
--   hypothesis is then H_0 by construction, so a 'Reject' indicts
--   the harness (unbalanced samplers, RTS state, order effects)
--   rather than the target. Pair with 'toBB'.
toAA :: Hypothesis a -> Hypothesis a
toAA h = h { sampleB = sampleA h }
{-# INLINE toAA #-}

-- | @'runCT' meter cfg ('toAA' hyp)@.
runAA :: Meter -> Config -> Hypothesis a -> IO Result
runAA meter cfg hyp = runCT meter cfg (toAA hyp)

-- | The /B\/B control/: 'sampleA' replaced by 'sampleB', catching
--   asymmetries specific to 'sampleB' that 'toAA' cannot see.
toBB :: Hypothesis a -> Hypothesis a
toBB h = h { sampleA = sampleB h }
{-# INLINE toBB #-}

-- | @'runCT' meter cfg ('toBB' hyp)@.
runBB :: Meter -> Config -> Hypothesis a -> IO Result
runBB meter cfg hyp = runCT meter cfg (toBB hyp)

-- | The outcome of the A\/A and B\/B controls. They can convict the
--   harness but not acquit it: each compares a sampler against
--   itself, so an asymmetry only /between/ the samplers (say, one
--   returns a thunk that the target forces while timed) is invisible
--   to both.
data ControlVerdict
  = ControlsPass
    -- ^ Neither control rejected. A primary 'Reject' is then
    --   consistent with a target leak, not proof of one.
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

-- | The control verdict with both control runs in full, so the
--   evidence can be inspected rather than taken on trust.
data Attribution = Attribution
  { attVerdict :: !ControlVerdict
    -- ^ the combined outcome.
  , attAA      :: !Result
    -- ^ the complete A\/A control run.
  , attBB      :: !Result
    -- ^ the complete B\/B control run.
  } deriving Show

-- | Run the A\/A and B\/B controls. Only meaningful after a
--   'Reject': on a symmetric harness both pass whether or not the
--   target leaks.
attribute :: Meter -> Config -> Hypothesis a -> IO Attribution
attribute meter cfg hyp = do
  raa <- runAA meter cfg hyp
  rbb <- runBB meter cfg hyp
  pure $! attribution raa rbb

-- the controls' combined verdict, carrying both runs.
attribution :: Result -> Result -> Attribution
attribution raa rbb =
  let !v = case (raa, rbb) of
        (Pass{},   Pass{})   -> ControlsPass
        (Reject{}, Pass{})   -> SampleAAsymmetry
        (Pass{},   Reject{}) -> SampleBAsymmetry
        (Reject{}, Reject{}) -> PervasiveAsymmetry
  in  Attribution
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
  | Just m <- cfgMargin
  , not (m > 0 && not (isNaN m) && not (isInfinite m)) =
      throwIO $! InvalidConfig
        "cfgMargin must be a positive finite fraction"
  | otherwise      = pure ()

-- | The two samplers, indexed by the per-pair order word: slot 0
--   holds 'sampleA', slot 1 'sampleB'. Built once per run so the
--   driver's pair loop selects a sampler by indexing — a
--   data-dependent load — rather than by branching on the order
--   bit. Both elements live in one cache line, so the selection's
--   microarchitectural footprint is as small as it can be made.
orderedSamplers :: Hypothesis a -> SmallArray (IO a)
orderedSamplers hyp =
  let !sa = sampleA hyp
      !sb = sampleB hyp
  in  smallArrayFromListN 2 [sa, sb]

-- | Run one (sample, measure, sample, measure) pair in the order
--   given by the order word (@0@ = A first, @1@ = B first) and return
--   (A's reading, B's reading). 'prepare' runs first, outside the
--   timed region.
--
--   The order bit is data only: one code path, the sampler chosen by
--   array index, the labels recovered by mask arithmetic. A branch
--   on the bit would give the two orders distinct code paths and
--   couple the bit into the readings through microarchitectural
--   state, breaking exchangeability (see @issues/handled/ISSUE2.md@).
runPair
  :: Meter -> Config -> Hypothesis a -> SmallArray (IO a) -> Word64
  -> IO (Word64, Word64)
runPair meter cfg hyp samplers !bit = do
  prepare hyp
  let !iFst = fromIntegral bit :: Int
      !iSnd = 1 - iFst
  !u  <- indexSmallArray samplers iFst
  !t1 <- measure meter (cfgBatch cfg) (target hyp u)
  !v  <- indexSmallArray samplers iSnd
  !t2 <- measure meter (cfgBatch cfg) (target hyp v)
  let (!x, !y) = unscramble bit t1 t2
  pure (x, y)
{-# INLINE runPair #-}

-- | 'runPair' under a t'SplitMeter': (A's cycles, A's wall time,
--   B's cycles, B's wall time), each reading unscrambled as there.
runSplitPair
  :: SplitMeter -> Config -> Hypothesis a -> SmallArray (IO a) -> Word64
  -> IO (Word64, Word64, Word64, Word64)
runSplitPair meter cfg hyp samplers !bit = do
  prepare hyp
  let !iFst = fromIntegral bit :: Int
      !iSnd = 1 - iFst
  !u <- indexSmallArray samplers iFst
  (!c1, !w1) <- measureSplit meter (cfgBatch cfg) (target hyp u)
  !v <- indexSmallArray samplers iSnd
  (!c2, !w2) <- measureSplit meter (cfgBatch cfg) (target hyp v)
  let (!ca, !cb) = unscramble bit c1 c2
      (!wa, !wb) = unscramble bit w1 w2
  pure (ca, wa, cb, wb)
{-# INLINE runSplitPair #-}

-- branchless unscramble: swap iff bit = 1, via an all-ones mask. The
-- first result is class A's reading (slot 1 when the bit is 0, slot
-- 2 when it is 1), the second class B's.
unscramble :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
unscramble !bit !t1 !t2 =
  let !m = negate bit
      !s = (t1 `B.xor` t2) B..&. m
  in  (t1 `B.xor` s, t2 `B.xor` s)
{-# INLINE unscramble #-}

-- Each channel's (A, B) readings from a split pair. The period is
-- wall time per cycle in femtoseconds, fine enough that the clip
-- bound's floor of 1 sits far below its noise. A batch always
-- retires cycles; the floor on the divisor only keeps the ratio
-- total.
splitReadings :: (Word64, Word64, Word64, Word64) -> Split (Double, Double)
splitReadings (!ca, !wa, !cb, !wb) = Split
  (fromIntegral ca, fromIntegral cb)
  (period wa ca, period wb cb)
  (fromIntegral wa, fromIntegral wb)
  where
    period !w !c = fromIntegral w * 1.0e6 / fromIntegral (max 1 c)

-- The hedge composition ----------------------------------------------------

-- The components are heterogeneous, so the driver steps each one
-- itself and hands their log e-values to 'EPMix.update' once per
-- pair. On a tie the sign component is unchanged, which is still its
-- current value, so the mixture's lockstep precondition holds. The
-- mixture owns the rejection latch and the threshold
-- @log(K \/ alpha)@, with @K = 4 + n@ for @n@ cut points: the
-- hedging price is @log K@ nats against @log(1 \/ alpha) = 13.8@ at
-- the default alpha. Under a margin only the three magnitude
-- components are mixed (@K = 3@); the sign and CDF ones are stepped
-- for their diagnostic peaks.

data HedgeCfg = HedgeCfg
  { hcSign   :: !EPSign.Config
  , hcMagn1  :: !EPMagn.Config
  , hcMagn2  :: !EPMagn.Config
  , hcMagn3  :: !EPMagn.Config
  , hcCdf    :: !EPMagn.Config          -- shared by every cut point
  , hcQs     :: ![Double]               -- the cut points themselves
  , hcMix    :: !EPMix.Config
  , hcC1     :: {-# UNPACK #-} !Double  -- tight clip level (base c)
  , hcC2     :: {-# UNPACK #-} !Double  -- 4c
  , hcC3     :: {-# UNPACK #-} !Double  -- 16c
  , hcMargin :: !(Maybe Double)         -- absolute margin; Nothing =
                                        -- sharp null
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
  :: Double -> Double -> [Double] -> Maybe Double
  -> Either ConfigError HedgeCfg
mkHedgeCfg !alpha !c qs mmargin = do
  let !c2 = 4  * c
      !c3 = 16 * c
      -- a magnitude component on d clipped to [-b, b]: mean-zero
      -- under the sharp null, anchored at ±delta under a margin.
      magn !b = case mmargin of
        Nothing -> EPMagn.config 0 (negate b) b alpha Newton
        Just d  ->
          EPMagn.configInterval (negate d) d (negate b) b alpha Newton
      !k = case mmargin of
        Nothing -> 4 + length qs
        Just _  -> 3
  !sc <- EPSign.config 0.5 alpha Newton
  !m1 <- magn c
  !m2 <- magn c2
  !m3 <- magn c3
  !cd <- EPMagn.config 0 (negate 1)  1  alpha Newton
  !mx <- EPMix.config k alpha
  pure HedgeCfg
    { hcSign   = sc
    , hcMagn1  = m1
    , hcMagn2  = m2
    , hcMagn3  = m3
    , hcCdf    = cd
    , hcQs     = qs
    , hcMix    = mx
    , hcC1     = c
    , hcC2     = c2
    , hcC3     = c3
    , hcMargin = mmargin
    }

-- One reading stream under test: its hedge and the confidence
-- sequence behind 'resEffect', both resting on the calibration
-- 'channel' fixes from the warmup pairs. A run tests one channel per
-- quantity it reads.
data Chan = Chan
  { chHedge :: !HedgeCfg
  , chSeq   :: !EPCS.Config
  }

data ChanState = ChanState
  { csHedge :: !HedgeState
  , csSeq   :: !EPCS.State
  , csIv    :: !(Double, Double)  -- the latched effect interval
  , csClips :: !Clips
  }

-- Calibrate a channel from its warmup (A, B) readings, resolving the
-- margin against them. Throws as 'runCTWith' documents.
channel :: Config -> [(Double, Double)] -> IO Chan
channel cfg ws = do
  let (!c, !qs, !meterResolves, !medR) = calibrate ws
  unless meterResolves $ throwIO WarmupZeroDuration
  !mdelta <- case cfgMargin cfg of
    Nothing  -> pure Nothing
    Just rel -> do
      let !delta = rel * medR
      unless (delta > 0) $ throwIO $! InvalidConfig
        "cfgMargin resolves to a zero absolute margin (warmup \
        \median reading is 0); raise cfgBatch"
      unless (delta < c) $ throwIO $! InvalidConfig
        "cfgMargin resolves to an absolute margin at or above the \
        \warmup clip bound; lower cfgMargin (the tolerance exceeds \
        \the pair-noise scale the test bets against)"
      pure (Just delta)
  !hcfg <- case mkHedgeCfg (cfgAlpha cfg) c qs mdelta of
    Left  e -> throwIO (InvalidEprocConfig e)
    Right x -> pure x
  !cscfg <- case EPCS.config (negate (hcC3 hcfg)) (hcC3 hcfg)
                   (cfgAlpha cfg) effectGrid of
    Left  e -> throwIO (InvalidEprocConfig e)
    Right x -> pure x
  pure (Chan hcfg cscfg)

chanInitial :: Chan -> ChanState
chanInitial Chan{..} = ChanState
  { csHedge = initialHedge chHedge
  , csSeq   = EPCS.initial chSeq
  , csIv    = (negate (hcC3 chHedge), hcC3 chHedge)
  , csClips = noClips
  }

-- Step a channel on one pair's (A, B) readings. The effect interval
-- is latched: once the evidence pins the mean below the grid's
-- resolution the survivor set empties, and the last resolvable
-- interval (nested within all earlier ones, so still covered by the
-- time-uniform guarantee) is what 'chanResult' reports.
stepChan :: Chan -> ChanState -> Double -> Double -> ChanState
stepChan Chan{..} ChanState{..} !xa !xb =
  let !d   = xa - xb
      !h'  = updateHedge chHedge csHedge d xa xb
      !s'  = EPCS.update chSeq csSeq (clipD (hcC3 chHedge) d)
      !iv' = case EPCS.interval chSeq s' of
               Just x  -> x
               Nothing -> csIv
      !cl' = bumpClips chHedge csClips d
  in  ChanState h' s' iv' cl'

-- Whether the channel's mixture has crossed its threshold; latched.
chanRejects :: Chan -> ChanState -> Bool
chanRejects Chan{..} ChanState{..} =
  case EPMix.decide (hcMix chHedge) (hsMix csHedge) of
    EPMix.Reject -> True
    _            -> False

-- The channel's frame after pair @n@, whose difference was @d@.
chanFrame :: Chan -> Int -> Double -> ChanState -> Frame
chanFrame Chan{..} !n !d ChanState{..} = Frame n d
  (EPMix.log_evalue mixc (hsMix csHedge))
  (EPMix.log_evalue_sup mixc (hsMix csHedge))
  (fst csIv) (snd csIv)
  (abs d > hcC1 chHedge)
  where
    mixc = hcMix chHedge

-- The channel's t'Result' after @n@ pairs, under the run's root seed.
chanResult :: Chan -> Word64 -> Int -> ChanState -> Result
chanResult ch@Chan{..} !root !n st@ChanState{..} = ctor
  n (EPMix.log_evalue_sup mixc (hsMix csHedge))
  (EPMix.p_value mixc (hsMix csHedge))
  csIv
  (clAtC csClips) (clAt4C csClips) (clAt16C csClips) (hcC1 chHedge)
  (EPSign.log_evalue_sup (hsSign csHedge))
  (EPMagn.log_evalue_sup (hsMagn1 csHedge))
  (EPMagn.log_evalue_sup (hsMagn2 csHedge))
  (EPMagn.log_evalue_sup (hsMagn3 csHedge))
  (cdfPeaks (hcQs chHedge) (hsCdf csHedge))
  root
  (hcMargin chHedge)
  where
    mixc = hcMix chHedge
    ctor | chanRejects ch st = Reject
         | otherwise         = Pass

-- Running clipped-pair tallies at the hedge's three magnitude
-- bounds. Kept as one strict record rather than three accumulator
-- arguments so the driver loop's arity does not grow with them.
data Clips = Clips
  { clAtC   :: {-# UNPACK #-} !Int
  , clAt4C  :: {-# UNPACK #-} !Int
  , clAt16C :: {-# UNPACK #-} !Int
  }

noClips :: Clips
noClips = Clips 0 0 0

-- Tally one pair's difference against each bound. Sits outside the
-- timed region, alongside the e-process updates.
bumpClips :: HedgeCfg -> Clips -> Double -> Clips
bumpClips HedgeCfg{..} (Clips !a !b !c) !d = Clips
  (bump hcC1 a) (bump hcC2 b) (bump hcC3 c)
  where
    !m = abs d
    bump !lim !n = if m > lim then n + 1 else n
{-# INLINE bumpClips #-}

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
      -- under a margin only the magnitude components gate; the
      -- sign and CDF components are stepped above for their
      -- diagnostic peaks but stay out of the mixture.
      !evs = case hcMargin of
        Just _  ->
          [ EPMagn.log_evalue m1'
          , EPMagn.log_evalue m2'
          , EPMagn.log_evalue m3'
          ]
        Nothing ->
          ( EPSign.log_evalue s'
          : EPMagn.log_evalue m1'
          : EPMagn.log_evalue m2'
          : EPMagn.log_evalue m3'
          : map EPMagn.log_evalue cd'
          )
      !x'  = EPMix.update hcMix hsMix evs
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

-- Pair each cut point with its component's peak log e-value. The
-- state list is aligned with 'hcQs' by construction, so the zip is
-- total on both.
cdfPeaks :: [Double] -> [EPMagn.State] -> [(Double, Double)]
cdfPeaks qs = zip qs . map EPMagn.log_evalue_sup

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

-- The order bit: splitmix64 from the resolved seed, carried as a
-- @Word64@ in @{0, 1}@ and consumed arithmetically, never by a case
-- (see 'runPair').

nextOrderBit :: Word64 -> (Word64, Word64)
nextOrderBit !s =
  let (!s', !w) = splitMix s
  in  (s', w B..&. 1)
{-# INLINE nextOrderBit #-}

-- Resolve the (root, warmup, main) seed triple: the root is the
-- user's seed or fresh OS entropy, reported as 'resSeed'; warmup and
-- main derive from it. Entropy rather than a clock reading, since a
-- periodic nuisance process may be periodic in that very clock; the
-- clock is only a fallback where @\/dev\/urandom@ is unreadable.
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

-- Warmup: run @cfgWarmup@ randomly ordered pairs, returning each
-- pair's readings for 'channel' to calibrate from.
warmup :: Config -> (Word64 -> IO r) -> Word64 -> IO [r]
warmup cfg pair = go 0 []
  where
    go !i !acc !rng
      | i >= cfgWarmup cfg = pure acc
      | otherwise = do
          let (!rng', !bit) = nextOrderBit rng
          !r <- pair bit
          go (i + 1) (r : acc) rng'

-- Calibrate from warmup (A, B) readings, returning
-- @(c, qs, meterResolves, medReading)@:
--
--   * @c = max 1 (2 * p99(|d|))@, the clip bound; the floor keeps it
--     positive when a PMU meter on constant-time code reads zero
--     differences, which is fine (the run then passes at budget).
--   * @qs@, the CDF cut points: order statistics of the pooled
--     readings, identical for both classes (which keeps the
--     indicator antisymmetric), deduplicated so a discrete meter
--     does not inflate @K@.
--   * @meterResolves@, whether @p99@ of the readings is nonzero;
--     if not, the meter's quantum swallows the target and the caller
--     throws 'WarmupZeroDuration'.
--   * @medReading@, the pooled median, which resolves a relative
--     'cfgMargin'.
calibrate :: [(Double, Double)] -> (Double, [Double], Bool, Double)
calibrate ws =
  let !readings = concat [ [x, y] | (x, y) <- ws ]
      !sortedR  = sort readings
      !p99d     = orderStat99 [ abs (x - y) | (x, y) <- ws ]
      !p99r     = orderStat99 readings
      !qs       = cutPoints sortedR
      !medR     = nth (length sortedR `div` 2) sortedR
  -- twice the order statistic leaves headroom for occasional
  -- outliers; larger differences are clipped in the main phase.
  in  (max 1 (2 * p99d), qs, p99r > 0, medR)

-- Empirical @~p99@ order statistic. Sorts and picks index
-- @(m * 99) \/ 100@; degenerates to the sample max at small @m@
-- (documented on 'cfgWarmup').
orderStat99 :: Ord a => Num a => [a] -> a
orderStat99 xs =
  let !sorted = sort xs
  in  nth ((length sorted * 99) `div` 100) sorted
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
cutPoints :: [Double] -> [Double]
cutPoints sorted
  | m == 0    = []
  | otherwise = dedupAsc (map pick cdfLevels)
  where
    !m = length sorted
    pick !p = nth (max 0 (min (m - 1) (floor (p * fromIntegral m)))) sorted

-- Drop repeats from an ascending list.
dedupAsc :: [Double] -> [Double]
dedupAsc (x : y : rest)
  | x == y    = dedupAsc (y : rest)
  | otherwise = x : dedupAsc (y : rest)
dedupAsc xs = xs
