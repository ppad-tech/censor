{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- |
-- Module: Censor.Meter
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Measurement abstraction.
--
-- A 'Meter' wraps an @IO@ action and returns a 'Word64' reading.
-- 'wallClock' is available everywhere, backed by
-- @GHC.Clock.getMonotonicTimeNSec@. On Linux, 'withCounter' provides
-- hardware performance-counter meters (retired instructions, retired
-- branches, or CPU cycles) via @perf_event_open@. The instruction and
-- branch counters are deterministic properties of the executed code,
-- immune to the frequency-scaling, cache, and scheduling noise that
-- limits 'wallClock' on sub-microsecond targets; the cycle counter is
-- additionally sensitive to data-dependent latency within identical
-- instruction streams (e.g. variable-time divider units, cache
-- misses), at the cost of run-to-run microarchitectural noise.
--
-- Resolution caveat ('wallClock'): on Apple Silicon, the user-mode
-- monotonic counter ticks at roughly 24 MHz (~42 ns per tick). Targets
-- faster than a microsecond should be measured under batching (see
-- 'Censor.cfgBatch'). On Linux x86, the underlying counter is
-- typically cycle-resolved.

module Censor.Meter (
    -- * Meter type
    Meter(..)

    -- * Implementations
  , wallClock

    -- * Hardware performance-counter meters (Linux)
  , Counter(..)
  , MeterError(..)
  , withCounter
  ) where

import Control.Exception (Exception, throwIO)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
#if defined(linux_HOST_OS)
import Control.Concurrent (runInBoundThread, rtsSupportsBoundThreads)
import Control.Exception (finally)
import Foreign.C.Types (CInt(..))
#endif

-- | A measurement device.
--
--   @'measure' m k act@ runs @act@ @k@ times back-to-back and returns
--   a single 'Word64' reading for the whole batch. The unit and
--   semantics depend on the meter (nanoseconds for 'wallClock';
--   retired instructions, retired branches, or CPU cycles for a
--   'withCounter' meter).
--
--   Bundling the batch count into the meter API lets each meter
--   decide where the inner loop lives -- the wall-clock meter samples
--   the monotonic clock once at each end of the batch; the PMU meters
--   arm and disarm the counter once per batch -- and avoids
--   per-pair closure allocation in the driver wrapping the loop.
newtype Meter = Meter
  { measure :: Int -> IO () -> IO Word64
  }

-- | Wall-clock meter. Reads the monotonic clock before and after the
--   @k@-fold batch and returns the elapsed nanoseconds.
--
--   Backed by @GHC.Clock.getMonotonicTimeNSec@, which dispatches to
--   @mach_absolute_time@ on Darwin and @clock_gettime(CLOCK_MONOTONIC)@
--   on Linux.
--
--   Resolution caveat: on Apple Silicon the user-mode monotonic
--   counter ticks at roughly 24 MHz (~42 ns per tick), so
--   sub-microsecond targets need batching (see 'Censor.cfgBatch').
--   On Linux x86 the underlying counter is typically cycle-resolved.
wallClock :: Meter
wallClock = Meter measureWall

measureWall :: Int -> IO () -> IO Word64
measureWall !k !act = do
  !t0 <- getMonotonicTimeNSec
  rep k act
  !t1 <- getMonotonicTimeNSec
  pure $! t1 - t0
{-# INLINE measureWall #-}

-- shared inner loop. INLINE so it specialises at each meter call site
-- and the recursion compiles to a join-point with no per-iteration
-- closure allocation.
rep :: Int -> IO () -> IO ()
rep !k !act = go k
  where
    go 0 = pure ()
    go i = act >> go (i - 1)
{-# INLINE rep #-}

-- | A hardware event to count.
data Counter =
    InstructionsRetired  -- ^ @PERF_COUNT_HW_INSTRUCTIONS@; deterministic.
  | BranchesRetired      -- ^ @PERF_COUNT_HW_BRANCH_INSTRUCTIONS@; deterministic.
  | Cycles               -- ^ @PERF_COUNT_HW_CPU_CYCLES@; sensitive to
                         --   data-dependent latency and microarch state,
                         --   not deterministic.
  deriving (Eq, Show)

-- | Why a 'withCounter' meter could not be opened.
data MeterError =
    Unsupported     -- ^ not a Linux build; no hardware-counter support
  | OpenFailed !Int -- ^ @perf_event_open@ failed with this @errno@
                    --   (commonly @EACCES@\/@EPERM@ when
                    --   @kernel.perf_event_paranoid@ is too high)
  deriving (Eq, Show)

instance Exception MeterError

#if defined(linux_HOST_OS)

foreign import ccall unsafe "censor_perf_open"
  c_perf_open :: CInt -> IO CInt
foreign import ccall unsafe "censor_perf_begin"
  c_perf_begin :: CInt -> IO ()
foreign import ccall unsafe "censor_perf_end"
  c_perf_end :: CInt -> IO Word64
foreign import ccall unsafe "censor_perf_close"
  c_perf_close :: CInt -> IO ()

counterCode :: Counter -> CInt
counterCode InstructionsRetired = 0
counterCode BranchesRetired     = 1
counterCode Cycles              = 2

-- | Acquire a hardware performance-counter 'Meter' for the duration of
--   the callback, then release it.
--
--   The counter is opened in user-space-only mode
--   (@exclude_kernel@), which unprivileged processes may do when
--   @kernel.perf_event_paranoid@ is at most @2@. Because a
--   @perf_event@ counter is bound to the thread that opened it, the
--   callback runs on a single OS thread -- under the threaded runtime
--   via 'runInBoundThread', otherwise inline on the sole OS thread --
--   so that the open and every 'measure' share one thread.
--
--   Throws 'MeterError' if the counter cannot be opened. For
--   'InstructionsRetired' and 'BranchesRetired' the reading is a
--   deterministic property of the executed code, so a constant-time
--   target yields identical readings across input classes regardless
--   of operand values, cache state, or CPU frequency. For 'Cycles' the
--   reading additionally reflects data-dependent latency and
--   microarchitectural state, so equality across classes is not
--   guaranteed even for CT code.
withCounter :: Counter -> (Meter -> IO a) -> IO a
withCounter counter k = onOneThread $ do
  fd <- c_perf_open (counterCode counter)
  if fd < 0
    then throwIO (OpenFailed (negate (fromIntegral fd)))
    else k (meterFor fd) `finally` c_perf_close fd
  where
    -- the perf fd is thread-affine; keep open + measures on one OS
    -- thread. runInBoundThread requires the threaded RTS, so fall back
    -- to running inline when there is only the single OS thread anyway.
    onOneThread
      | rtsSupportsBoundThreads = runInBoundThread
      | otherwise               = id

meterFor :: CInt -> Meter
meterFor !fd = Meter $ \k act -> do
  c_perf_begin fd
  rep k act
  c_perf_end fd
{-# INLINE meterFor #-}

#else

-- | Hardware performance-counter meters are only available on Linux;
--   on other platforms this always throws 'Unsupported'.
withCounter :: Counter -> (Meter -> IO a) -> IO a
withCounter _ _ = throwIO Unsupported

#endif
