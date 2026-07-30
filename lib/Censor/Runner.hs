{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Censor.Runner
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Batteries-included harness support for "Censor": meter selection
-- from a command-line token, trace recording (built on
-- 'Censor.runCTWith', so it does not reimplement the driver),
-- auto-attributing case batteries, and parameter sweeps.

module Censor.Runner (
    -- * Meter selection
    MeterChoice(..)
  , parseMeter
  , meterName
  , withMeterArg

    -- * Recording traces
  , Trace(..)
  , record
  , thin

    -- * Batteries
  , Case(..)
  , runBattery

    -- * Sweeps
  , sweep
  , partitionVerdicts

    -- * Verdicts and rendering
  , verdictName
  , shapeName
  , summary
  , clipText
  , resultToJSON
  ) where

import Control.Exception (try)
import Control.Monad (forM)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.List (intercalate)
import Numeric (showHex)
import System.Exit (die)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import Censor

-- meter selection -------------------------------------------------------------

-- | A meter chosen by name. The PMU variants carry their display
--   label alongside the 'Counter'.
data MeterChoice
  = MeterWall
  | MeterPMU !Counter !String

-- | Parse a meter name: @wall@, @instructions@, @cycles@, or
--   @branches@.
parseMeter :: String -> Maybe MeterChoice
parseMeter s = case s of
  "wall"         -> Just MeterWall
  "instructions" -> Just (MeterPMU InstructionsRetired "instructions")
  "cycles"       -> Just (MeterPMU Cycles "cycles")
  "branches"     -> Just (MeterPMU BranchesRetired "branches")
  _              -> Nothing

-- | The display name of a chosen meter.
meterName :: MeterChoice -> String
meterName MeterWall         = "wall"
meterName (MeterPMU _ name)  = name

-- | Parse a meter token (defaulting to wall-clock when absent) and
--   run the callback with the resolved t'Meter' and its name,
--   bracketing a PMU counter for its lifetime and dying with a clear
--   message if it cannot be opened (e.g. the PMU meters off Linux, or
--   an unknown name). Handles the whole "wall everywhere, PMU
--   Linux-only, hard error otherwise" dance in one place.
withMeterArg :: Maybe String -> (String -> Meter -> IO a) -> IO a
withMeterArg marg k = do
  choice <- case marg of
    Nothing -> pure MeterWall
    Just s  -> case parseMeter s of
      Just c  -> pure c
      Nothing -> die $ "unknown meter " ++ show s
        ++ " (expected wall|instructions|cycles|branches)"
  case choice of
    MeterWall         -> k "wall" wallClock
    MeterPMU cnt name -> do
      r <- try (withCounter cnt (k name))
      case r of
        Right a -> pure a
        Left e  -> die $ name ++ " meter unavailable: "
          ++ show (e :: MeterError)

-- recording -------------------------------------------------------------------

-- | A recorded run: the final 'Result' plus the per-pair t'Frame'
--   trajectory.
data Trace = Trace
  { traceName   :: !String
  , traceMeter  :: !String
  , traceResult :: !Result
  , traceFrames :: ![Frame]
  } deriving Show

-- | Record a run, capturing every t'Frame' via 'runCTWith'. The
--   observer strictly accumulates frames; the driver is 'runCTWith'
--   itself, not a reimplementation of it.
record :: String -> String -> Meter -> Config -> Hypothesis a -> IO Trace
record name mname meter cfg hyp = do
  ref <- newIORef []
  res <- runCTWith meter cfg hyp $ \ !fr -> modifyIORef' ref (fr :)
  frs <- readIORef ref
  pure $! Trace name mname res (reverse frs)

-- | Downsample a frame list for compact plotting: keep all of the
--   first 150 frames (the early motion), then a uniform stride, then
--   the final frame.
thin :: [Frame] -> [Frame]
thin frs =
  let !m      = length frs
      !stride = max 1 (m `div` 450)
      keep fr = frPair fr <= 150
             || frPair fr `mod` stride == 0
             || frPair fr == m
  in  filter keep frs

-- batteries -------------------------------------------------------------------

-- | A named hypothesis to run. The hypothesis is built in @IO@ so each
--   case can seed its own sampler.
data Case a = Case
  { caseName :: !String
  , caseHyp  :: !(IO (Hypothesis a))
  }

-- | Run a battery of cases under one meter, auto-attributing every
--   rejection ('runCTAttributed'). Prints a concise verdict line per
--   case and returns the results.
runBattery
  :: Meter
  -> Config
  -> [Case a]
  -> IO [(String, Result, Maybe Attribution)]
runBattery meter cfg cs = forM cs $ \(Case name mkH) -> do
  putStr (name ++ " ... ") >> hFlush stdout
  h <- mkH
  (r, ma) <- runCTAttributed meter cfg h
  putStrLn $ verdictName r ++ " after " ++ show (resPairs r) ++ " pairs"
    ++ maybe "" (\att -> "  [" ++ attribLabel att ++ "]") ma
  pure (name, r, ma)

attribLabel :: Attribution -> String
attribLabel a = case attVerdict a of
  ControlsPass         -> "controls pass"
  SampleAAsymmetry     -> "sampleA asymmetry"
  SampleBAsymmetry     -> "sampleB asymmetry"
  PervasiveAsymmetry   -> "harness asymmetry"

-- sweeps ----------------------------------------------------------------------

-- | Run a parameterised family of hypotheses under one meter and
--   return each parameter's result. The recurring "control vs swept
--   input" pattern (a mismatch position, a length, ...); pair with
--   'partitionVerdicts' to read off where the verdict flips.
sweep
  :: Meter -> Config -> (p -> IO (Hypothesis a)) -> [p] -> IO [(p, Result)]
sweep meter cfg mk ps = forM ps $ \p -> do
  h <- mk p
  r <- runCT meter cfg h
  pure (p, r)

-- | Split swept results into (passing params, rejecting params) — the
--   boundary of a leak's structure (e.g. same-lane pass vs cross-lane
--   reject).
partitionVerdicts :: [(p, Result)] -> ([p], [p])
partitionVerdicts = foldr step ([], [])
  where
    step (p, Pass{})   (ps, rs) = (p : ps, rs)
    step (p, Reject{}) (ps, rs) = (ps, p : rs)

-- verdicts and rendering ------------------------------------------------------

-- | @\"REJECT\"@ or @\"PASS\"@.
verdictName :: Result -> String
verdictName Reject{} = "REJECT"
verdictName Pass{}   = "PASS"

-- | A one-line human-readable render of a t'Result', suitable for
--   logging or embedding in a larger status line. Includes the
--   verdict, pairs consumed, evidence (peak log e-value, p-value),
--   effect interval, and clip count; on 'Reject' it also names the
--   driver channel of the hedge ('leakShape').
--
--   > REJECT after 342 pairs; driver=bulk peakLogW=18.42 p=1.0e-8
--   > effect=[-101.5,-98.4] clipped=6/342
summary :: Result -> String
summary r =
  let !effLo = fst (resEffect r)
      !effHi = snd (resEffect r)
      base = printf "peakLogW=%.2f effect=[%.1f,%.1f] %s"
               (resPeakLogW r) effLo effHi (clipText r)
  in  case r of
        Reject{} ->
          printf "REJECT after %d pairs; driver=%s %s p=%.2g"
            (resPairs r) (shapeName (leakShape r))
            (base :: String) (resPValue r)
        Pass{} ->
          printf "PASS at %d pairs; %s"
            (resPairs r) (base :: String)

-- | Render a t'Result'\'s clipping as one field. The tight count is
--   always shown; the wider bounds are appended only when clipping
--   actually reached them, so the common case stays as terse as it
--   was and a run whose effect interval saw truncated input says so.
--
--   > clipped=6/342
--   > clipped=129/138 (4c 44, 16c 3)
clipText :: Result -> String
clipText r
  | resClipped4 r == 0 && resClipped16 r == 0 = tight
  | otherwise = tight ++ printf " (4c %d, 16c %d)"
      (resClipped4 r) (resClipped16 r)
  where
    tight = printf "clipped=%d/%d" (resClipped r) (resPairs r) :: String

-- | Serialise a t'Result' as a stable JSON object. Includes verdict,
--   pairs, mixture peak log e-value, anytime-valid p-value, effect
--   interval, clip counts at each of the three magnitude bounds,
--   warmup bound, per-component peaks, and the 'diagnose'
--   advisories.
--
--   The object is a single line; pipe multiple runs into a
--   line-oriented store and diff or filter with @jq@.
resultToJSON :: Result -> String
resultToJSON r = concat
  [ "{\"verdict\":",    jstr (verdictName r)
  , ",\"pairs\":",      show (resPairs r)
  , printf ",\"peakLogW\":%.6g" (resPeakLogW r)
  , printf ",\"pvalue\":%.6g"   (resPValue r)
  , printf ",\"effect\":[%.6g,%.6g]"
      (fst (resEffect r)) (snd (resEffect r))
  , ",\"clipped\":",    show (resClipped r)
  , ",\"clipped4\":",   show (resClipped4 r)
  , ",\"clipped16\":",  show (resClipped16 r)
  , printf ",\"bound\":%.6g" (resBound r)
  , ",\"seed\":",       jstr ("0x" ++ showHex (resSeed r) "")
  , ",\"cutPoints\":["
  , intercalate "," (map (printf "%.6g") (resCutPoints r))
  , "]"
  , ",\"peaks\":{"
  ,       printf "\"sign\":%.6g"    (resPeakSign r)
  , printf ",\"magn\":%.6g"         (resPeakMagn r)
  , printf ",\"magn4\":%.6g"        (resPeakMagn4 r)
  , printf ",\"magn16\":%.6g"       (resPeakMagn16 r)
  , printf ",\"cdf\":%.6g"          (resPeakCdf r)
  , "}"
  , ",\"advisories\":["
  , intercalate "," (map advisoryJSON (diagnose r))
  , "]}"
  ]

advisoryJSON :: Advisory -> String
advisoryJSON a = case a of
  HighClipRate cr -> printf
    ("{\"kind\":\"HighClipRate\",\"clipFraction\":%.4g"
      ++ ",\"clipFraction4\":%.4g,\"clipFraction16\":%.4g}")
    (clipTight cr) (clipWide cr) (clipWidest cr)
  LowPower h -> printf
    "{\"kind\":\"LowPower\",\"widthOverBound\":%.4g}" h
  RejectionDriver s -> concat
    [ "{\"kind\":\"RejectionDriver\",\"shape\":"
    , jstr (shapeName s), "}"
    ]

-- | The short label for a 'LeakShape', as it appears on a case row
--   and in the @advisories@ block of 'resultToJSON'.
shapeName :: LeakShape -> String
shapeName s = case s of
  BulkShift   -> "bulk"
  ShapeLeak   -> "shape"
  RareOutlier -> "tail"
  CdfShift    -> "cdf"
  MixedShape  -> "mixed"

jstr :: String -> String
jstr s = "\"" ++ s ++ "\""
