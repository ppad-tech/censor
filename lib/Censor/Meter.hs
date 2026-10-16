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
-- A t'Meter' wraps an @IO@ action and returns a 'Word64' reading.
-- 'wallClock' is available everywhere. On Linux, 'withCounter'
-- provides performance-counter meters via @perf_event_open@: the
-- retired-instruction and retired-branch counts are deterministic
-- properties of the executed code, immune to the frequency, cache,
-- and scheduling noise that limits the wall clock on fast targets,
-- while the other counters help attribute a difference to a channel
-- (see t'Counter'). 'withSplitMeter' reads the cycle counter and the
-- wall clock over the same execution, splitting wall time into the
-- cycles the code took and the clock period they ran at.

module Censor.Meter (
    -- * Meter type
    Meter(..)

    -- * Implementations
  , wallClock

    -- * Hardware performance-counter meters (Linux)
  , Counter(..)
  , MeterError(..)
  , withCounter

    -- * Wall time split into cycles and period (Linux)
  , SplitMeter(..)
  , withSplitMeter
  ) where

import Control.Exception (Exception, throwIO)
import Data.Word (Word64)
#if !defined(darwin_HOST_OS)
import GHC.Clock (getMonotonicTimeNSec)
#endif
#if defined(linux_HOST_OS)
import Control.Concurrent (runInBoundThread, rtsSupportsBoundThreads)
import Control.Exception (finally)
import Foreign.C.Types (CInt(..))
#endif

-- | A measurement device. @'measure' m k act@ runs @act@ @k@ times
--   back to back and returns one reading for the batch: nanoseconds
--   for 'wallClock', counted events for a 'withCounter' meter. Each
--   meter owns its inner loop, reading the clock or arming the
--   counter once per batch.
newtype Meter = Meter
  { measure :: Int -> IO () -> IO Word64
  }

-- | A meter reading two quantities over one execution of the batch:
--   @'measureSplit' m k act@ returns @(cycles, nanoseconds)@. Wall
--   time is cycles times the clock period, and a secret can move it
--   through either factor: through cycles when the code's execution
--   depends on it, through the period when the core's frequency
--   does (data-dependent power under frequency scaling). Reading
--   both on the same execution tells the two apart.
newtype SplitMeter = SplitMeter
  { measureSplit :: Int -> IO () -> IO (Word64, Word64)
  }

-- | Wall-clock meter: elapsed nanoseconds over the batch.
--
--   On Darwin, backed by @clock_gettime_nsec_np(CLOCK_UPTIME_RAW)@,
--   which resolves the hardware tick (~42 ns on Apple Silicon, so
--   sub-microsecond targets need batching; see 'Censor.cfgBatch').
--   Not @GHC.Clock.getMonotonicTimeNSec@, which macOS quantises to
--   microseconds, putting CDF cut points on quantisation boundaries
--   (see @issues\/handled\/ISSUE2.md@). Elsewhere, backed by
--   @GHC.Clock.getMonotonicTimeNSec@.
wallClock :: Meter
wallClock = Meter measureWall

#if defined(darwin_HOST_OS)
foreign import ccall unsafe "censor_monotonic_ns"
  monotonicNS :: IO Word64
#else
monotonicNS :: IO Word64
monotonicNS = getMonotonicTimeNSec
{-# INLINE monotonicNS #-}
#endif

measureWall :: Int -> IO () -> IO Word64
measureWall !k !act = do
  !t0 <- monotonicNS
  rep k act
  !t1 <- monotonicNS
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

-- | A hardware (or, for 'TaskClock', software) event to count.
--
--   With 'wallClock' the counters form a ladder in which each
--   adjacent gap isolates one cause of a class difference:
--   instructions (code path) -> cycles (microarchitectural latency)
--   -> ref-cycles (frequency) -> task-clock (kernel and interrupt
--   time) -> wall (scheduling gaps).
data Counter =
    InstructionsRetired  -- ^ @PERF_COUNT_HW_INSTRUCTIONS@; deterministic.
  | BranchesRetired      -- ^ @PERF_COUNT_HW_BRANCH_INSTRUCTIONS@;
                         --   deterministic.
  | Cycles               -- ^ @PERF_COUNT_HW_CPU_CYCLES@; also sensitive
                         --   to data-dependent latency.
  | RefCycles            -- ^ @PERF_COUNT_HW_REF_CPU_CYCLES@: cycles at
                         --   the constant reference rate, so a
                         --   difference here with clean 'Cycles' is a
                         --   frequency effect. Not available on every
                         --   microarchitecture.
  | TaskClock            -- ^ @PERF_COUNT_SW_TASK_CLOCK@: on-CPU
                         --   nanoseconds, including kernel time on the
                         --   task's behalf. A software counter, so
                         --   available in VMs, but counting kernel time
                         --   needs @kernel.perf_event_paranoid@ at most
                         --   @1@.
  | BranchMisses         -- ^ @PERF_COUNT_HW_BRANCH_MISSES@: catches a
                         --   secret-dependent branch direction when
                         --   retired branch counts balance.
  | CacheMisses          -- ^ @PERF_COUNT_HW_CACHE_MISSES@: attributes a
                         --   cycles-level difference to memory access
                         --   patterns.
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
counterCode RefCycles           = 3
counterCode TaskClock           = 4
counterCode BranchMisses        = 5
counterCode CacheMisses         = 6

-- | Acquire a performance-counter t'Meter' for the duration of the
--   callback, then release it. Throws 'MeterError' if the counter
--   cannot be opened.
--
--   Counters count user space only, which unprivileged processes may
--   open at @kernel.perf_event_paranoid@ @2@ or lower; 'TaskClock'
--   deliberately includes kernel time and needs @1@ or lower, failing
--   with @'OpenFailed' 13@ otherwise rather than silently degrading
--   to a user-only clock. A counter is bound to the thread that
--   opened it, so the callback runs on one OS thread.
withCounter :: Counter -> (Meter -> IO a) -> IO a
withCounter counter k = withPerf counter (k . meterFor)

-- | Acquire a t'SplitMeter' for the duration of the callback, then
--   release it. Throws 'MeterError' if the cycle counter cannot be
--   opened.
--
--   The wall clock is read inside the counted region, so both
--   readings cover the batch and the clock reads; the overhead is
--   the same in both classes. As with 'withCounter', the callback
--   runs on one OS thread.
withSplitMeter :: (SplitMeter -> IO a) -> IO a
withSplitMeter k = withPerf Cycles (k . splitFor)

-- open a counter on one OS thread for the callback, closing it after.
withPerf :: Counter -> (CInt -> IO a) -> IO a
withPerf counter k = onOneThread $ do
  fd <- c_perf_open (counterCode counter)
  if fd < 0
    then throwIO (OpenFailed (negate (fromIntegral fd)))
    else k fd `finally` c_perf_close fd
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

splitFor :: CInt -> SplitMeter
splitFor !fd = SplitMeter $ \k act -> do
  c_perf_begin fd
  !t0 <- monotonicNS
  rep k act
  !t1 <- monotonicNS
  !cy <- c_perf_end fd
  pure (cy, t1 - t0)
{-# INLINE splitFor #-}

#else

-- | Hardware performance-counter meters are only available on Linux;
--   on other platforms this always throws 'Unsupported'.
withCounter :: Counter -> (Meter -> IO a) -> IO a
withCounter _ _ = throwIO Unsupported

-- | The split meter needs the cycle counter, so it is only available
--   on Linux; on other platforms this always throws 'Unsupported'.
withSplitMeter :: (SplitMeter -> IO a) -> IO a
withSplitMeter _ = throwIO Unsupported

#endif
