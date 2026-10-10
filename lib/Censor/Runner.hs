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
-- 'Censor.runCTWith', so it does not reimplement the driver), and
-- parameter sweeps.

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

    -- * Sweeps
  , sweep

    -- * Verdicts and rendering
  , verdictName
  , shapeName
  , summary
  , clipText
  ) where

import Control.Exception (try)
import Control.Monad (forM)
import Data.IORef (newIORef, modifyIORef', readIORef)
import System.Exit (die)
import Text.Printf (printf)

import Censor

-- meter selection -------------------------------------------------------------

-- | A meter chosen by name. The PMU variants carry their display
--   label alongside the 'Counter'.
data MeterChoice
  = MeterWall
  | MeterPMU !Counter !String

-- | Parse a meter name: @wall@, @instructions@, @cycles@,
--   @branches@, @ref-cycles@, @task-clock@, @branch-misses@, or
--   @cache-misses@.
parseMeter :: String -> Maybe MeterChoice
parseMeter s = case s of
  "wall"          -> Just MeterWall
  "instructions"  -> Just (MeterPMU InstructionsRetired "instructions")
  "cycles"        -> Just (MeterPMU Cycles "cycles")
  "branches"      -> Just (MeterPMU BranchesRetired "branches")
  "ref-cycles"    -> Just (MeterPMU RefCycles "ref-cycles")
  "task-clock"    -> Just (MeterPMU TaskClock "task-clock")
  "branch-misses" -> Just (MeterPMU BranchMisses "branch-misses")
  "cache-misses"  -> Just (MeterPMU CacheMisses "cache-misses")
  _               -> Nothing

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
        ++ " (expected wall|instructions|cycles|branches|ref-cycles"
        ++ "|task-clock|branch-misses|cache-misses)"
  case choice of
    MeterWall         -> k "wall" wallClock
    MeterPMU cnt name -> do
      r <- try (withCounter cnt (k name))
      case r of
        Right a -> pure a
        Left e  -> die $ name ++ " meter unavailable: "
          ++ show (e :: MeterError) ++ hint e
  where
    -- EPERM/EACCES means the kernel refused a counter that exists,
    -- which for an unprivileged process is the paranoid level.
    -- task-clock counts kernel time and so needs a lower one than
    -- the rest; see Censor.Meter.
    hint (OpenFailed n)
      | n == 1 || n == 13 =
          " (check kernel.perf_event_paranoid: task-clock needs 1"
            ++ " or lower, the other counters 2 or lower)"
    hint _ = ""

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

-- sweeps ----------------------------------------------------------------------

-- | Run a parameterised family of hypotheses under one meter and
--   return each parameter's result: the recurring "control vs swept
--   input" pattern (a mismatch position, a length, ...).
sweep
  :: Meter -> Config -> (p -> IO (Hypothesis a)) -> [p] -> IO [(p, Result)]
sweep meter cfg mk ps = forM ps $ \p -> do
  h <- mk p
  r <- runCT meter cfg h
  pure (p, r)

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

-- | The short label for a 'LeakShape', as it appears on a case row
--   and in the @advisories@ block of the JSON report.
shapeName :: LeakShape -> String
shapeName s = case s of
  BulkShift   -> "bulk"
  ShapeLeak   -> "shape"
  RareOutlier -> "tail"
  CdfShift    -> "cdf"
  MixedShape  -> "mixed"
