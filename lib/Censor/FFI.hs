{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Censor.FFI
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Buffer-allocating helpers for testing foreign-function targets.
--
-- A foreign-function constant-time test typically wants three pieces
-- of memory: two input buffers (one per input class, or two halves of
-- a single input record) and an output buffer the target writes to.
-- This module gives you a single pinned workspace, large enough to
-- hold all three, allocated once outside the timed region and reused
-- for every measurement pair.
--
-- The samplers receive a base pointer and fill the workspace in
-- place; the target receives the same base pointer and consumes
-- whatever the sampler wrote. Layout within the workspace -- which
-- offsets hold which inputs, where the output lives -- is the user's
-- responsibility (typically a few @plusPtr@ calls).
--
-- = Hygiene
--
-- The timed region wraps only @ffiTarget@, so any work done in the
-- samplers (RNG, marshalling, format conversion) is excluded from
-- the measurement. Keep the target action minimal: it should be a
-- single FFI call and nothing else. Allocation, @withForeignPtr@,
-- @ByteString@ packing, etc. inside the target appear as A\/B noise
-- the same way that in-Haskell thunk evaluation would.
--
-- = Example
--
-- > -- C side: void ct_mul(const uint8_t *a, const uint8_t *b,
-- > --                     uint8_t *out);  -- 32-byte operands.
-- > foreign import ccall unsafe "ct_mul"
-- >   c_mul :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
-- >
-- > mulHyp :: Rng -> FFIHypothesis
-- > mulHyp g = FFIHypothesis
-- >   { ffiWorkspaceBytes = 96
-- >   , ffiSampleA = \p -> do
-- >       fillRandom g (p `plusPtr` 0) 32    -- draw and discard,
-- >       fillBytes (p `plusPtr` 0) 0 32     -- then fix: input a
-- >       fillRandom g (p `plusPtr` 32) 32   -- input b: random
-- >   , ffiSampleB = \p -> do
-- >       fillRandom g (p `plusPtr` 0)  32
-- >       fillRandom g (p `plusPtr` 32) 32
-- >   , ffiTarget = \p ->
-- >       c_mul p (p `plusPtr` 32) (p `plusPtr` 64)
-- >   }
-- >
-- > result <- withFFIHypothesis (mulHyp g) $ \h ->
-- >   runCT wallClock defaultConfig h
--
-- @Rng@ and 'fillRandom' come from "Censor.Rng" ('fillRandom' is
-- re-exported here); @fillBytes@ is
-- 'Foreign.Marshal.Utils.fillBytes'. The discarded 'fillRandom' in
-- @ffiSampleA@ keeps the two samplers' generator work symmetric
-- before the fixed operand overwrites it.

module Censor.FFI (
    -- * Foreign hypothesis
    FFIHypothesis(..)

    -- * Bridging to runCT
  , withFFIHypothesis

    -- * Workspace fills (re-exports)
  , fillRandom
  ) where

import Censor.Rng (fillRandom)
import Censor (Hypothesis(..))
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)

-- | A foreign-function constant-time hypothesis.
--
--   The workspace is a single pinned byte buffer of @ffiWorkspaceBytes@
--   bytes, allocated once outside the timed region. Samplers fill it
--   in place; the target reads (and optionally writes) it.
data FFIHypothesis = FFIHypothesis
  { ffiWorkspaceBytes :: !Int
    -- ^ size of the pinned workspace, in bytes.
  , ffiPrepare :: !(IO ())
    -- ^ per-pair prologue, run before either sampler and outside
    --   the timed region. @pure ()@ for an ordinary hypothesis; a
    --   shared-context hypothesis uses it to refresh a scratch
    --   buffer that both samplers then overlay into the workspace.
  , ffiSampleA :: !(Ptr Word8 -> IO ())
    -- ^ populate the workspace for an input drawn from class A.
  , ffiSampleB :: !(Ptr Word8 -> IO ())
    -- ^ populate the workspace for an input drawn from class B.
  , ffiTarget  :: !(Ptr Word8 -> IO ())
    -- ^ the foreign action under test. Timed by the meter; should be
    --   a single FFI call with no in-Haskell preparation.
  }

-- | Acquire the workspace, present a regular t'Hypothesis' that the
--   sequential driver ('Censor.runCT') can consume, and release the
--   workspace when the continuation returns.
--
--   The same workspace pointer is handed to every sample\/measure
--   pair; samplers should establish the buffer state for their class
--   each call rather than relying on residual contents.
withFFIHypothesis
  :: FFIHypothesis
  -> (Hypothesis (Ptr Word8) -> IO a)
  -> IO a
withFFIHypothesis h k =
  allocaBytes (ffiWorkspaceBytes h) $ \p -> k Hypothesis
    { target  = ffiTarget h
    , prepare = ffiPrepare h
    , sampleA = ffiSampleA h p >> pure p
    , sampleB = ffiSampleB h p >> pure p
    }
