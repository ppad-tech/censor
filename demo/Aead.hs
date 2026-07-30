{-# LANGUAGE BangPatterns #-}

-- censor-aead: record censor runs of ppad-aead's ChaCha20-Poly1305
-- surface. Fix-vs-random on the 256-bit key across the encrypt and
-- decrypt accept paths, plus a mismatch-position independence probe
-- of the decrypt reject path.
--
-- Usage: censor-aead [--format pretty|json|plain]
--                    [wall|instructions|cycles|branches] [OUT.json]
--
-- Everything besides the hypotheses and the case list is handled
-- by 'Censor.Runner.Report.runTraceSuite': batch is calibrated per
-- case, and any 'Reject' verdict is automatically attributed via
-- A/A + B/B diagnostics.

module Main where

import Control.Exception (evaluate)
import qualified Censor as C
import qualified Censor.Runner.Report as R
import qualified Crypto.AEAD.ChaCha20Poly1305 as AEAD
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.Word (Word64)

fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- alpha 1e-6 is censor's default rejection level (the earlier 1e-3
-- was inherited from ATTACK132's first run; no reason to keep it).
aeadCfg :: C.Config
aeadCfg = C.defaultConfig
  { C.cfgAlpha  = 1.0e-6
  , C.cfgBudget = 50000
  , C.cfgWarmup = 500
  }

random32, random12 :: C.Rng -> IO BS.ByteString
random32 g = C.randomBytes g 32
random12 g = C.randomBytes g 12

-- encrypt: fix-vs-random on the key; nonce, AAD, plaintext public.
posAeadEncrypt
  :: IO (C.Hypothesis
           (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString))
posAeadEncrypt = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!key, !nonce, !aad, !pt) -> do
        r <- evaluate (AEAD.encrypt aad key nonce pt)
        case r of
          Right (!_, !_) -> pure ()
          Left _         -> pure ())
    (random32 g)
    (\key -> do
        !nonce <- random12 g
        !aad   <- random32 g
        !pt    <- C.randomBytes g 256
        pure (key, nonce, aad, pt))

-- decrypt accept path: (cip, mac) precomputed at sample time, so
-- the timed region exercises keystream regen, Poly1305 recompute,
-- and the constant-time tag comparison.
posAeadDecrypt
  :: IO (C.Hypothesis
           ( BS.ByteString, BS.ByteString, BS.ByteString
           , BS.ByteString, BS.ByteString ))
posAeadDecrypt = do
  g <- C.mkRng fixedSeed
  let mkSample !key = do
        !nonce <- random12 g
        !aad   <- random32 g
        !pt    <- C.randomBytes g 256
        case AEAD.encrypt aad key nonce pt of
          Left _ -> error "censor-aead: encrypt failed during sample setup"
          Right (cip, mac) -> do
            !c <- evaluate cip
            !m <- evaluate mac
            pure (key, nonce, aad, c, m)
  C.fixVsRandom
    (\(!key, !nonce, !aad, !cip, !mac) -> do
        r <- evaluate (AEAD.decrypt aad key nonce (cip, mac))
        case r of
          Right !_ -> pure ()
          Left _   -> pure ())
    (random32 g)
    mkSample

-- flip one byte (XOR 0xff) of a bytestring at the given index, so
-- a valid tag no longer matches. Index assumed in range for the
-- 16-byte Poly1305 tag.
flipByte :: Int -> BS.ByteString -> BS.ByteString
flipByte i bs = BS.pack
  [ if j == i then B.xor w 0xff else w
  | (j, w) <- zip [0 :: Int ..] (BS.unpack bs) ]

-- decrypt reject path: mismatch-position independence of the tag
-- comparison. Both classes submit a tag corrupted at a fixed byte
-- (class A at i0, class B at i1) so decrypt rejects; the timed
-- region runs the full Poly1305 recompute and ct_eq's full-length
-- scan. Tests that reject timing does not depend on where the first
-- mismatch lies. Both samplers do identical work, differing only
-- in the flip index.
posAeadDecryptReject
  :: (Int, Int)
  -> IO (C.Hypothesis
           ( BS.ByteString, BS.ByteString, BS.ByteString
           , BS.ByteString, BS.ByteString ))
posAeadDecryptReject (i0, i1) = do
  g <- C.mkRng fixedSeed
  let mk pos = do
        !key   <- random32 g
        !nonce <- random12 g
        !aad   <- random32 g
        !pt    <- C.randomBytes g 256
        case AEAD.encrypt aad key nonce pt of
          Left _ -> error "censor-aead: encrypt failed during sample setup"
          Right (cip, mac) -> do
            !c <- evaluate cip
            !m <- evaluate (flipByte pos mac)
            pure (key, nonce, aad, c, m)
  pure C.Hypothesis
    { C.target  = \(!key, !nonce, !aad, !cip, !mac) -> do
        r <- evaluate (AEAD.decrypt aad key nonce (cip, mac))
        case r of
          Right !_ -> pure ()   -- unreachable: tag is corrupted
          Left _   -> pure ()
    , C.prepare = pure ()
    , C.sampleA = mk i0
    , C.sampleB = mk i1
    }

main :: IO ()
main = R.runTraceSuite R.TraceSuite
  { R.tsTarget = "ppad-aead ChaCha20-Poly1305 (secret-key fix-vs-random)"
  , R.tsConfig = aeadCfg
  , R.tsSeed   = Just fixedSeed
  , R.tsNote   = Just "pure targets: batch=1 (no inner repetition)"
  , R.tsCases  =
      [ R.TraceCase "AEAD.encrypt (256 B)" R.SingleShot posAeadEncrypt
      , R.TraceCase "AEAD.decrypt accept (256 B)" R.SingleShot posAeadDecrypt
      , R.TraceCase "AEAD.decrypt reject (tag mismatch @0 vs @15)" R.SingleShot
                    (posAeadDecryptReject (0, 15))
      ]
  }
