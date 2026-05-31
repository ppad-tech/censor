{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Censor.Meter
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Measurement abstraction.
--
-- A 'Meter' wraps an @IO@ action and returns a 'Word64' reading. In
-- this release the only implementation is 'wallClock', backed by
-- @GHC.Clock.getMonotonicTimeNSec@. A future release will add PMU
-- hardware-counter meters on Linux; the API is designed so any new
-- meter is a drop-in replacement.
--
-- Resolution caveat: on Apple Silicon, the user-mode monotonic
-- counter ticks at roughly 24 MHz (~42 ns per tick). Targets faster
-- than a microsecond should be measured under batching (see
-- 'Censor.cfgBatch'). On Linux x86, the underlying counter is
-- typically cycle-resolved.

module Censor.Meter (
    -- * Meter type
    Meter(..)

    -- * Implementations
  , wallClock
  ) where

import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

-- | A measurement device.
--
--   @'measure' m act@ runs @act@ and returns a 'Word64' reading.
--   The unit and semantics depend on the meter (nanoseconds for
--   'wallClock'; retired instructions for a future PMU meter).
newtype Meter = Meter
  { measure :: IO () -> IO Word64
  }

-- | Wall-clock meter. Reads the monotonic clock before and after
--   the action and returns the elapsed nanoseconds.
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
