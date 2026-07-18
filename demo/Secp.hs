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

-- constant-time variable-base multiplication of a fixed public
-- point by the secret scalar.
posMul :: IO (C.Hypothesis Wider)
posMul = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\s -> () <$ evaluate (Secp.mul Secp._CURVE_G s))
    (randomWider g)
    pure

-- timing-safe wNAF multiplication under a precomputed context.
posMulWNAF :: Secp.Context -> IO (C.Hypothesis Wider)
posMulWNAF tex = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\s -> () <$ evaluate (Secp.mul_wnaf tex s))
    (randomWider g)
    pure

-- ECDH shared secret against a fixed peer public key.
posEcdh :: Secp.Pub -> IO (C.Hypothesis Wider)
posEcdh peer = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\sec -> () <$ evaluate (Secp.ecdh peer sec))
    (randomWider g)
    pure

-- RFC6979 ECDSA signing; secret key fixed vs random, message public.
posSignECDSA
  :: Secp.Context -> IO (C.Hypothesis (Wider, BS.ByteString))
posSignECDSA tex = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!sec, !msg) -> () <$ evaluate (Secp.sign_ecdsa' tex sec msg))
    (randomWider g)
    (\sec -> do !msg <- random32 g; pure (sec, msg))

-- BIP0340 Schnorr signing; secret key fixed vs random, message +
-- aux public.
posSignSchnorr
  :: Secp.Context
  -> IO (C.Hypothesis (Wider, BS.ByteString, BS.ByteString))
posSignSchnorr tex = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!sec, !msg, !aux) ->
       () <$ evaluate (Secp.sign_schnorr' tex sec msg aux))
    (randomWider g)
    (\sec -> do !msg <- random32 g; !aux <- random32 g; pure (sec, msg, aux))

peerScalar :: Wider
peerScalar = W.wider 0xa1b2c3d4e5f60718 0x1122334455667788
                     0x99aabbccddeeff00 0x0f1e2d3c4b5a6978

main :: IO ()
main = do
  let !tex = Secp.precompute
  peer <- case Secp.derive_pub peerScalar of
    Just p  -> evaluate p
    Nothing -> error "censor-secp: peer derive failed"
  R.runTraceSuite R.TraceSuite
    { R.tsTarget = "ppad-secp256k1 secret-scalar surfaces"
    , R.tsConfig = secpCfg
    , R.tsSeed   = Just fixedSeed
    , R.tsNote   = Just "batch auto-calibrated"
    , R.tsCases  =
        [ R.TraceCase "Secp.mul (constant-time, variable-base)" posMul
        , R.TraceCase "Secp.mul_wnaf (wNAF point multiplication)"
                      (posMulWNAF tex)
        , R.TraceCase "Secp.ecdh (shared secret)" (posEcdh peer)
        , R.TraceCase "Secp.sign_ecdsa' (RFC6979)" (posSignECDSA tex)
        , R.TraceCase "Secp.sign_schnorr' (BIP0340)"
                      (posSignSchnorr tex)
        ]
    }
