{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Censor.Rng
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- A minimal splitmix64 PRNG for input samplers.
--
-- Samplers feeding 'Censor.runCT' want a pseudorandom source that is
-- deterministic under a fixed seed, cheap, and light on allocation,
-- so that sampling work does not itself perturb the measurement (see
-- the measurement-hygiene notes in "Censor"). This module provides
-- exactly that and nothing more. It is /not/ a cryptographic
-- generator; the sampled inputs only need to be independent of the
-- target's timing distribution, not unpredictable.

module Censor.Rng (
    -- * Stateful generator
    Rng
  , mkRng
  , nextWord

    -- * Pure step
  , splitMix
  ) where

import qualified Data.Bits as B
import Data.IORef
import Data.Word (Word64)

-- | splitmix64 state threaded through an 'IORef'.
newtype Rng = Rng (IORef Word64)

-- | Create a generator from a seed. Equal seeds yield equal streams.
mkRng :: Word64 -> IO Rng
mkRng s = Rng <$> newIORef s
{-# INLINE mkRng #-}

-- | Draw the next 'Word64' from the generator.
nextWord :: Rng -> IO Word64
nextWord (Rng ref) = atomicModifyIORef' ref splitMix
{-# INLINE nextWord #-}

-- | The pure splitmix64 step: from a state, produce the successor
--   state and an output word.
splitMix :: Word64 -> (Word64, Word64)
splitMix !s =
  let !s' = s + 0x9E3779B97F4A7C15
      !z0 = (s' `B.xor` (s' `B.shiftR` 30)) * 0xBF58476D1CE4E5B9
      !z1 = (z0 `B.xor` (z0 `B.shiftR` 27)) * 0x94D049BB133111EB
      !z2 =  z1 `B.xor` (z1 `B.shiftR` 31)
  in  (s', z2)
{-# INLINE splitMix #-}
