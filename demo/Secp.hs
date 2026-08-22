{-# LANGUAGE BangPatterns #-}

-- censor-secp: record censor runs of ppad-secp256k1's secret-scalar
-- operations. Fix-vs-random on the secret scalar across the
-- constant-time multipliers and the signing / ECDH surfaces.
--
-- Usage: censor-secp [--format pretty|json|plain]
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
import qualified Crypto.Curve.Secp256k1 as Secp
import qualified Data.ByteString as BS
import Data.Word (Word64)
import qualified Data.Word.Wider as W
import Data.Word.Wider (Wider)

fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- matches ppad-secp256k1's ATTACK134 censor config.
secpCfg :: C.Config
secpCfg = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgBudget = 50000
  , C.cfgWarmup = 500
  }

randomWider :: C.Rng -> IO Wider
randomWider g = do
  !a <- C.nextWord g
  !b <- C.nextWord g
  !c <- C.nextWord g
  !d <- C.nextWord g
  pure $! W.wider (fromIntegral a) (fromIntegral b)
                  (fromIntegral c) (fromIntegral d)

random32 :: C.Rng -> IO BS.ByteString
random32 g = C.randomBytes g 32

-- constant-time variable-base multiplication of the generator by
-- the secret scalar (the double-and-add ladder).
posMul :: IO (C.Hypothesis Wider)
posMul = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\s -> () <$ evaluate (Secp.mul Secp._CURVE_G s))
    (randomWider g)
    pure
    pure

-- timing-safe wNAF multiplication under a precomputed context.
posMulWNAF :: Secp.Context -> IO (C.Hypothesis Wider)
posMulWNAF tex = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\s -> () <$ evaluate (Secp.mul_wnaf tex s))
    (randomWider g)
    pure
    pure

-- ECDH shared secret against a fixed peer public key.
posEcdh :: Secp.Pub -> IO (C.Hypothesis Wider)
posEcdh peer = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\sec -> () <$ evaluate (Secp.ecdh peer sec))
    (randomWider g)
    pure
    pure

-- public-key derivation from the secret scalar (mul _CURVE_G).
posDerivePub :: IO (C.Hypothesis Wider)
posDerivePub = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\sec -> () <$ evaluate (Secp.derive_pub sec))
    (randomWider g)
    pure
    pure

-- RFC6979 ECDSA signing (default, ladder-based mul _CURVE_G). Secret
-- key fixed vs random; the message is held FIXED and public across
-- both classes and every sample, so the RFC6979 nonce
-- (k = f(key, msg)) is deterministic in the fixed class and the
-- fix-vs-random axis controls the whole declared secret (key + nonce)
-- rather than only the key.
posSignECDSA
  :: BS.ByteString -> IO (C.Hypothesis (Wider, BS.ByteString))
posSignECDSA msg = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!sec, !m) -> () <$ evaluate (Secp.sign_ecdsa sec m))
    (randomWider g)
    pure
    (\sec -> pure (sec, msg))

-- RFC6979 ECDSA signing under a precomputed context (wNAF mul_wnaf).
posSignECDSA'
  :: Secp.Context -> BS.ByteString
  -> IO (C.Hypothesis (Wider, BS.ByteString))
posSignECDSA' tex msg = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!sec, !m) -> () <$ evaluate (Secp.sign_ecdsa' tex sec m))
    (randomWider g)
    pure
    (\sec -> pure (sec, msg))

-- BIP0340 Schnorr signing (default, ladder-based mul _CURVE_G). Secret
-- key fixed vs random; message and aux held fixed and public (see
-- 'posSignECDSA').
posSignSchnorr
  :: BS.ByteString -> BS.ByteString
  -> IO (C.Hypothesis (Wider, BS.ByteString, BS.ByteString))
posSignSchnorr msg aux = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!sec, !m, !a) ->
       () <$ evaluate (Secp.sign_schnorr sec m a))
    (randomWider g)
    pure
    (\sec -> pure (sec, msg, aux))

-- BIP0340 Schnorr signing under a precomputed context (wNAF).
posSignSchnorr'
  :: Secp.Context -> BS.ByteString -> BS.ByteString
  -> IO (C.Hypothesis (Wider, BS.ByteString, BS.ByteString))
posSignSchnorr' tex msg aux = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!sec, !m, !a) ->
       () <$ evaluate (Secp.sign_schnorr' tex sec m a))
    (randomWider g)
    pure
    (\sec -> pure (sec, msg, aux))

peerScalar :: Wider
peerScalar = W.wider 0xa1b2c3d4e5f60718 0x1122334455667788
                     0x99aabbccddeeff00 0x0f1e2d3c4b5a6978

main :: IO ()
main = do
  let !tex = Secp.precompute
  -- one fixed, public message and aux, drawn once and shared across
  -- both classes and every sample of the signing cases (see
  -- 'posSignECDSA' for why).
  gpub <- C.mkRng 0x0FED0FED0FED0FED
  !fixedMsg <- random32 gpub
  !fixedAux <- random32 gpub
  peer <- case Secp.derive_pub peerScalar of
    Just p  -> evaluate p
    Nothing -> error "censor-secp: peer derive failed"
  R.runTraceSuite R.TraceSuite
    { R.tsTarget = "ppad-secp256k1 secret-scalar surfaces"
    , R.tsConfig = secpCfg
    , R.tsSeed   = Just fixedSeed
    , R.tsNote   = Nothing
    , R.tsCases  =
        [ R.TraceCase "Secp.mul (constant-time, variable-base)"
                      R.SingleShot posMul
        , R.TraceCase "Secp.mul_wnaf (wNAF point multiplication)" R.SingleShot
                      (posMulWNAF tex)
        , R.TraceCase "Secp.sign_ecdsa (RFC6979)" R.SingleShot
                      (posSignECDSA fixedMsg)
        , R.TraceCase "Secp.sign_ecdsa' (RFC6979)" R.SingleShot
                      (posSignECDSA' tex fixedMsg)
        , R.TraceCase "Secp.sign_schnorr (BIP0340)" R.SingleShot
                      (posSignSchnorr fixedMsg fixedAux)
        , R.TraceCase "Secp.sign_schnorr' (BIP0340)" R.SingleShot
                      (posSignSchnorr' tex fixedMsg fixedAux)
        , R.TraceCase "Secp.ecdh (shared secret)" R.SingleShot (posEcdh peer)
        , R.TraceCase "Secp.derive_pub (public key)" R.SingleShot posDerivePub
        ]
    }
