{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Censor.Runner.Env
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Host-environment capture.
--
-- An t'Env' is a point-in-time snapshot of the machine a censor run
-- executed on: platform identity (OS, architecture, kernel release,
-- CPU model, core count), the timing-relevant tuning knobs visible
-- under Linux sysfs (cpufreq governor, turbo\/boost state, SMT
-- control, @perf_event_paranoid@), the load average, and a UTC
-- timestamp.
--
-- Capture is best-effort: fields whose sources are absent on the
-- host (e.g. the sysfs knobs on Darwin) are 'Nothing'. Conditions
-- affect the /power/ of a run, not its validity — the paired,
-- order-randomised design cancels common-mode drift — so the
-- snapshot exists to make reports interpretable and provenanced,
-- not to gate execution.

module Censor.Runner.Env (
    -- * Environment snapshot
    Env(..)
  , captureEnv
  ) where

import Control.Exception (try, IOException)
import qualified Data.ByteString.Char8 as B8
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar, CDouble, CInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr)
import GHC.Conc (getNumProcessors)
import qualified System.Info as Info

foreign import ccall unsafe "censor_env_loadavg"
  c_env_loadavg :: Ptr CDouble -> IO CInt

foreign import ccall unsafe "censor_env_kernel"
  c_env_kernel :: Ptr CChar -> CSize -> IO CInt

foreign import ccall unsafe "censor_env_time"
  c_env_time :: Ptr CChar -> CSize -> IO CInt

foreign import ccall unsafe "censor_env_cpu"
  c_env_cpu :: Ptr CChar -> CSize -> IO CInt

foreign import ccall unsafe "censor_env_cores"
  c_env_cores :: IO CInt

-- | A host-environment snapshot, taken once at run start.
--
--   The Linux knob fields ('envGovernor', 'envBoost', 'envSmt',
--   'envParanoid') hold the raw sysfs\/procfs file contents; they
--   are 'Nothing' wherever the file does not exist (in particular,
--   everywhere but Linux).
data Env = Env
  { envOs       :: !String
    -- ^ operating system, per @System.Info.os@.
  , envArch     :: !String
    -- ^ architecture, per @System.Info.arch@.
  , envKernel   :: !(Maybe String)
    -- ^ kernel release, per @uname(2)@.
  , envCpu      :: !(Maybe String)
    -- ^ CPU model string (Darwin @sysctl@, or the first @model name@
    --   row of Linux @\/proc\/cpuinfo@; absent on most aarch64 Linux
    --   kernels).
  , envCores    :: !Int
    -- ^ logical core count.
  , envGovernor :: !(Maybe String)
    -- ^ Linux cpufreq scaling governor of cpu0.
  , envBoost    :: !(Maybe String)
    -- ^ Linux turbo\/boost state, recorded verbatim as @key=value@
    --   (@boost=1@ is boost-enabled; @no_turbo=1@ is boost-disabled)
    --   to avoid interpreting the two opposing conventions.
  , envSmt      :: !(Maybe String)
    -- ^ Linux SMT control state (@on@ \/ @off@ \/ ...).
  , envParanoid :: !(Maybe String)
    -- ^ Linux @kernel.perf_event_paranoid@ level.
  , envLoad     :: !(Maybe (Double, Double, Double))
    -- ^ 1\/5\/15-minute load averages at capture time.
  , envTime     :: !(Maybe String)
    -- ^ capture time, UTC ISO-8601.
  } deriving (Eq, Show)

-- | Snapshot the host environment. Cheap (a few syscalls and file
--   reads); call once at run start.
captureEnv :: IO Env
captureEnv = do
  cores <- captureCores
  kern  <- cString 256 c_env_kernel
  cpu   <- captureCpu
  gov   <- readKnob
    "/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"
  boost <- captureBoost
  smt   <- readKnob "/sys/devices/system/cpu/smt/control"
  para  <- readKnob "/proc/sys/kernel/perf_event_paranoid"
  load  <- captureLoad
  time  <- cString 64 c_env_time
  pure Env
    { envOs       = Info.os
    , envArch     = Info.arch
    , envKernel   = kern
    , envCpu      = cpu
    , envCores    = cores
    , envGovernor = gov
    , envBoost    = boost
    , envSmt      = smt
    , envParanoid = para
    , envLoad     = load
    , envTime     = time
    }

-- run a C fill-buffer shim, returning the trimmed NUL-terminated
-- contents on success and Nothing on failure.
cString :: Int -> (Ptr CChar -> CSize -> IO CInt) -> IO (Maybe String)
cString n fill = allocaBytes n $ \buf -> do
  rc <- fill buf (fromIntegral n)
  if rc == 0
    then fmap nonEmpty (peekCString buf)
    else pure Nothing

-- sysconf via the shim, because GHC's getNumProcessors reports 1 on
-- the non-threaded RTS; that stays as the fallback.
captureCores :: IO Int
captureCores = do
  rc <- c_env_cores
  if rc >= 1
    then pure (fromIntegral rc)
    else getNumProcessors

captureLoad :: IO (Maybe (Double, Double, Double))
captureLoad = allocaArray 3 $ \p -> do
  rc <- c_env_loadavg p
  if rc == 3
    then do
      vals <- peekArray 3 p
      case map realToFrac vals of
        [l1, l5, l15] -> pure (Just (l1, l5, l15))
        _             -> pure Nothing
    else pure Nothing

captureCpu :: IO (Maybe String)
captureCpu = do
  s <- cString 256 c_env_cpu
  case s of
    Just _  -> pure s
    Nothing -> cpuFromProcinfo

-- first "model name" row of /proc/cpuinfo (x86 Linux; most aarch64
-- kernels have no such row and the field stays Nothing).
cpuFromProcinfo :: IO (Maybe String)
cpuFromProcinfo = do
  mc <- readKnob "/proc/cpuinfo"
  pure $ case mc of
    Nothing -> Nothing
    Just s  ->
      case filter ("model name" `isPrefixOf`) (lines s) of
        (l : _) -> nonEmpty (drop 1 (dropWhile (/= ':') l))
        []      -> Nothing

captureBoost :: IO (Maybe String)
captureBoost = do
  b <- readKnob "/sys/devices/system/cpu/cpufreq/boost"
  case b of
    Just v  -> pure (Just ("boost=" ++ v))
    Nothing -> do
      t <- readKnob "/sys/devices/system/cpu/intel_pstate/no_turbo"
      pure (fmap ("no_turbo=" ++) t)

-- read a small sysfs/procfs file, trimmed; Nothing when absent,
-- unreadable, or empty. Strict read so no handle outlives the call.
readKnob :: FilePath -> IO (Maybe String)
readKnob p = do
  er <- try (B8.readFile p) :: IO (Either IOException B8.ByteString)
  pure $ case er of
    Left _  -> Nothing
    Right b -> nonEmpty (B8.unpack b)

nonEmpty :: String -> Maybe String
nonEmpty s =
  let t = dropWhileEnd isSpace (dropWhile isSpace s)
  in  if null t then Nothing else Just t
