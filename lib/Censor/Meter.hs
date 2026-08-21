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
-- 'wallClock' is available everywhere: on Darwin it is backed by
-- @clock_gettime_nsec_np(CLOCK_UPTIME_RAW)@ (the
-- @mach_absolute_time@ counter), elsewhere by
-- @GHC.Clock.getMonotonicTimeNSec@. On Linux, 'withCounter' provides
-- performance-counter meters (retired instructions, retired
-- branches, CPU cycles, constant-rate reference cycles, on-CPU task
-- clock, branch misses, or cache misses) via @perf_event_open@. The
-- instruction and branch counters are deterministic properties of
-- the executed code, immune to the frequency-scaling, cache, and
-- scheduling noise that limits 'wallClock' on sub-microsecond
-- targets; the cycle counter is additionally sensitive to
-- data-dependent latency within identical instruction streams (e.g.
-- variable-time divider units, cache misses), at the cost of
-- run-to-run microarchitectural noise. The remaining counters exist
-- to /attribute/ a difference to a channel — see t'Counter' for the
-- ladder they form with 'wallClock'.
--
-- Resolution caveat ('wallClock'): on Apple Silicon, the hardware
-- counter ticks at roughly 24 MHz (~42 ns per tick). Targets
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
#if !defined(darwin_HOST_OS)
import GHC.Clock (getMonotonicTimeNSec)
#endif
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
--   On Darwin, backed by
--   @clock_gettime_nsec_np(CLOCK_UPTIME_RAW)@ — the
--   @mach_absolute_time@ counter, resolved at its hardware tick
--   rate. Not @GHC.Clock.getMonotonicTimeNSec@, whose
--   @CLOCK_MONOTONIC@ macOS quantises to /microseconds/: a
--   microsecond-atomised reading distribution puts warmup-fixed
--   CDF cut points exactly on quantisation boundaries, where the
--   indicator channel is maximally sensitive to nanosecond-scale
--   harness perturbations (see @issues\/ISSUE2.md@). On other
--   platforms, backed by @GHC.Clock.getMonotonicTimeNSec@
--   (@clock_gettime(CLOCK_MONOTONIC)@ on Linux).
--
--   Resolution caveat: on Apple Silicon the hardware counter ticks
--   at roughly 24 MHz (~42 ns per tick), so sub-microsecond targets
--   need batching (see 'Censor.cfgBatch'). On Linux x86 the
--   underlying counter is typically cycle-resolved.
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

-- | A hardware (or, for 'TaskClock', kernel software) event to
--   count.
--
--   Together with 'wallClock' the counters form a ladder in which
--   each adjacent gap isolates one cause of a class difference:
--   instructions (code path) -> cycles (microarchitectural
--   latency) -> ref-cycles (frequency) -> task-clock (kernel \/
--   interrupt time) -> wall (scheduling gaps). A rejection that
--   first appears at a given rung localises the difference to that
--   rung's channel.
data Counter =
    InstructionsRetired  -- ^ @PERF_COUNT_HW_INSTRUCTIONS@; deterministic.
  | BranchesRetired      -- ^ @PERF_COUNT_HW_BRANCH_INSTRUCTIONS@; deterministic.
  | Cycles               -- ^ @PERF_COUNT_HW_CPU_CYCLES@; sensitive to
                         --   data-dependent latency and microarch state,
                         --   not deterministic.
  | RefCycles            -- ^ @PERF_COUNT_HW_REF_CPU_CYCLES@: cycles at
                         --   the /constant/ reference rate, over the
                         --   same scheduled user-mode interval as
                         --   'Cycles'. A class difference here with
                         --   clean 'Cycles' is a frequency (DVFS \/
                         --   power) effect by construction.
                         --   Availability is microarchitecture-
                         --   dependent; 'withCounter' throws
                         --   'OpenFailed' where unsupported.
  | TaskClock            -- ^ @PERF_COUNT_SW_TASK_CLOCK@: nanoseconds
                         --   the task spent on-CPU, /including/
                         --   kernel time on its behalf (the
                         --   exclusion bits are deliberately not
                         --   set). Wall minus task-clock isolates
                         --   descheduled time. A software counter:
                         --   needs no PMU access, so it works in
                         --   VMs and containers where the hardware
                         --   counters cannot be opened.
  | BranchMisses         -- ^ @PERF_COUNT_HW_BRANCH_MISSES@: catches a
                         --   secret-dependent branch /direction/
                         --   even when retired branch counts
                         --   balance (one branch, two directions,
                         --   same total). Not deterministic.
  | CacheMisses          -- ^ @PERF_COUNT_HW_CACHE_MISSES@: attributes
                         --   a cycles-level difference to memory
                         --   access patterns (the table-lookup
                         --   signature). Not deterministic.
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

-- | Acquire a hardware performance-counter t'Meter' for the duration of
--   the callback, then release it.
--
--   The counter is opened in user-space-only mode
--   (@exclude_kernel@), which unprivileged processes may do when
--   @kernel.perf_event_paranoid@ is at most @2@ — except
--   'TaskClock', which deliberately counts kernel time on the
--   task's behalf (see t'Counter'). Because a @perf_event@ counter
--   is bound to the thread that opened it, the callback runs on a
--   single OS thread -- under the threaded runtime via
--   'runInBoundThread', otherwise inline on the sole OS thread --
--   so that the open and every 'measure' share one thread.
--
--   Throws 'MeterError' if the counter cannot be opened. For
--   'InstructionsRetired' and 'BranchesRetired' the reading is a
--   deterministic property of the executed code, so a constant-time
--   target yields identical readings across input classes regardless
--   of operand values, cache state, or CPU frequency. For the other
--   counters the reading additionally reflects data-dependent
--   latency, microarchitectural state, frequency ('RefCycles'), or
--   kernel-time attribution ('TaskClock'), so equality across
--   classes is not guaranteed even for CT code — class-conditional
--   /differences/ are the signal.
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
