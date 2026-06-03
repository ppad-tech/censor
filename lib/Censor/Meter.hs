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
-- hardware performance-counter meters (retired instructions or
-- branches) via @perf_event_open@; these count a deterministic
-- property of the executed code rather than elapsed time, so they are
-- immune to the frequency-scaling, cache, and scheduling noise that
-- limits 'wallClock' on sub-microsecond targets.
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
--   @'measure' m act@ runs @act@ and returns a 'Word64' reading. The
--   unit and semantics depend on the meter (nanoseconds for
--   'wallClock'; retired instructions or branches for a 'withCounter'
--   meter).
newtype Meter = Meter
  { measure :: IO () -> IO Word64
  }

-- | Wall-clock meter. Reads the monotonic clock before and after the
--   action and returns the elapsed nanoseconds.
--
--   Backed by @GHC.Clock.getMonotonicTimeNSec@, which dispatches to
--   @mach_absolute_time@ on Darwin and @clock_gettime(CLOCK_MONOTONIC)@
--   on Linux.
wallClock :: Meter
wallClock = Meter measureWall

measureWall :: IO () -> IO Word64
measureWall act = do
  !t0 <- getMonotonicTimeNSec
  act
  !t1 <- getMonotonicTimeNSec
  pure $! t1 - t0
{-# INLINE measureWall #-}

-- | A hardware event to count.
data Counter =
    InstructionsRetired  -- ^ @PERF_COUNT_HW_INSTRUCTIONS@
  | BranchesRetired      -- ^ @PERF_COUNT_HW_BRANCH_INSTRUCTIONS@
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
--   Throws 'MeterError' if the counter cannot be opened. Counts a
--   deterministic instruction\/branch tally, so a constant-time target
--   yields identical readings across input classes regardless of
--   operand values, cache state, or CPU frequency.
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
meterFor !fd = Meter $ \act -> do
  c_perf_begin fd
  act
  c_perf_end fd
{-# INLINE meterFor #-}

#else

-- | Hardware performance-counter meters are only available on Linux;
--   on other platforms this always throws 'Unsupported'.
withCounter :: Counter -> (Meter -> IO a) -> IO a
withCounter _ _ = throwIO Unsupported

#endif
