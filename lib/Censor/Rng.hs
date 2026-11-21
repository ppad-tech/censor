{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Censor.Rng
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- A minimal PRNG for input samplers: xoshiro256++, seeded from a
-- 64-bit seed via splitmix64. Its stream for a seed is that of
-- ppad-random's Crypto.PRNG; censor keeps its own copy so that it
-- adds nothing to a harness's dependency closure that a target might
-- also depend on (ppad-random depends on ppad-chacha).
--
-- Samplers feeding 'Censor.runCT' want a pseudorandom source that is
-- deterministic under a fixed seed, cheap, and light on allocation,
-- so that sampling work does not itself perturb the measurement (see
-- the measurement-hygiene notes in "Censor"). This module provides
-- exactly that and nothing more. It is /not/ a cryptographic
-- generator; the sampled inputs only need to be independent of the
-- target's timing distribution, not unpredictable.
--
-- A generator is not thread-safe: use one per thread.

module Censor.Rng (
    -- * Stateful generator
    Rng
  , mkRng
  , nextWord
  , reseed

    -- * Byte fills
  , randomBytes
  , fillRandom

    -- * Pure step
  , splitMix
  ) where

import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.Primitive.PrimArray as PA
import Data.Word (Word8, Word64)
import GHC.Exts (RealWorld)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)

-- | A xoshiro256++ generator: its four words of state, mutable so
--   that it can be reseeded in place.
newtype Rng = Rng (PA.MutablePrimArray RealWorld Word64)

-- | Create a generator from a seed. Equal seeds yield equal streams.
mkRng :: Word64 -> IO Rng
mkRng s = do
  g <- Rng <$> PA.newPrimArray 4
  reseed g s
  pure g
{-# INLINE mkRng #-}

-- | Draw the next 'Word64' from the generator.
nextWord :: Rng -> IO Word64
nextWord (Rng a) = do
  !s0 <- PA.readPrimArray a 0
  !s1 <- PA.readPrimArray a 1
  !s2 <- PA.readPrimArray a 2
  !s3 <- PA.readPrimArray a 3
  let !t  = s1 `B.shiftL` 17
      !a2 = s2 `B.xor` s0
      !a3 = s3 `B.xor` s1
  PA.writePrimArray a 0 (s0 `B.xor` a3)
  PA.writePrimArray a 1 (s1 `B.xor` a2)
  PA.writePrimArray a 2 (a2 `B.xor` t)
  PA.writePrimArray a 3 (a3 `B.rotateL` 45)
  pure $! ((s0 + s3) `B.rotateL` 23) + s0
{-# INLINE nextWord #-}

-- | Reset a generator to a seed in place: after @reseed g s@ the
--   generator draws exactly the stream of a fresh @'mkRng' s@.
--
--   Lets one long-lived generator replay a pinned stream, or adopt
--   a fresh one, without allocating per use — so the two classes
--   of a fix-vs-random sampler can share a single generator object
--   and differ only in the seed value written into it.
reseed :: Rng -> Word64 -> IO ()
reseed (Rng a) !x = do
  let !(x0, s0) = splitMix x
      !(x1, s1) = splitMix x0
      !(x2, s2) = splitMix x1
      !(_ , s3) = splitMix x2
  PA.writePrimArray a 0 s0
  PA.writePrimArray a 1 s1
  PA.writePrimArray a 2 s2
  PA.writePrimArray a 3 s3
{-# INLINE reseed #-}

-- | The pure splitmix64 step: from a state, produce the successor
--   state and an output word. An 'Rng' expands its seed with it,
--   and "Censor" derives a run's seeds and its order bits with it.
splitMix :: Word64 -> (Word64, Word64)
splitMix !s =
  let !s' = s + 0x9E3779B97F4A7C15
      !z0 = (s' `B.xor` (s' `B.shiftR` 30)) * 0xBF58476D1CE4E5B9
      !z1 = (z0 `B.xor` (z0 `B.shiftR` 27)) * 0x94D049BB133111EB
      !z2 =  z1 `B.xor` (z1 `B.shiftR` 31)
  in  (s', z2)
{-# INLINE splitMix #-}

-- | Draw @n@ pseudorandom bytes as a strict 'BS.ByteString'.
--
--   The result is a fresh, fully-materialised heap value -- no
--   thunks for the meter to pay inside the timed region -- so a
--   sampler can return it directly.
randomBytes :: Rng -> Int -> IO BS.ByteString
randomBytes !g !n
  | n <= 0    = pure BS.empty
  | otherwise = BI.create n $ \p -> fillRandom g p n

-- | Fill @n@ bytes at the pointer with pseudorandom bytes drawn
--   from the generator.
fillRandom :: Rng -> Ptr Word8 -> Int -> IO ()
fillRandom !g !dst !n = go 0
  where
    go !i
      | i >= n    = pure ()
      | otherwise = do
          !w <- nextWord g
          let !left  = n - i
              !chunk = if left > 8 then 8 else left
          writeWord dst i w chunk
          go (i + chunk)

-- write the low @lim@ bytes of a 'Word64', little-endian, at the
-- given offset.
writeWord :: Ptr Word8 -> Int -> Word64 -> Int -> IO ()
writeWord !dst !off !w !lim = go 0
  where
    go !j
      | j >= lim  = pure ()
      | otherwise = do
          let !byte = fromIntegral (w `B.shiftR` (8 * j)) :: Word8
          pokeByteOff dst (off + j) byte
          go (j + 1)
