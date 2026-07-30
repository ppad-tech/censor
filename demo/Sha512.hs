{-# LANGUAGE BangPatterns #-}

-- censor-sha512: record censor runs of ppad-sha512's constant-time
-- HMAC-tag comparison under a selectable meter, dumping per-pair
-- trajectories as JSON for plotting.
--
-- Usage: censor-sha512 [--format pretty|json|plain]
--                      [wall|instructions|cycles|branches] [OUT.json]
--
-- Everything besides the hypothesis and the case list is handled
-- by 'Censor.Runner.Report.runTraceSuite'.

module Main where

import Control.Exception (evaluate)
import qualified Censor as C
import qualified Censor.Runner.Report as R
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.Word (Word64)

fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- Both classes compare a fresh 64-byte tag against a copy differing
-- in exactly one byte, class A flipping byte i0 and class B flipping
-- byte i1. Same terminal result (False); the classes differ only in
-- mismatch position.
posCase :: (Int, Int) -> IO (C.Hypothesis (SHA512.MAC, SHA512.MAC))
posCase (i0, i1) = do
  g <- C.mkRng fixedSeed
  let flipAt i bs = BS.pack
        [ if j == i then B.xor w 0xFF else w
        | (j, w) <- zip [0 :: Int ..] (BS.unpack bs) ]
      mk i = do
        !k <- C.randomBytes g 32
        !m <- C.randomBytes g 256
        let SHA512.MAC raw = SHA512.hmac k m
        !t <- evaluate (SHA512.MAC raw)
        !u <- evaluate (SHA512.MAC (flipAt i raw))
        pure (t, u)
  pure C.Hypothesis
    { C.target  = \(!a, !b) -> () <$ evaluate (a == b)
    , C.sampleA = mk i0
    , C.sampleB = mk i1
    }

main :: IO ()
main = R.runTraceSuite R.TraceSuite
  { R.tsTarget = "ppad-sha512 HMAC MAC == (mismatch-position independence)"
  , R.tsConfig = C.defaultConfig
      { C.cfgAlpha  = 1.0e-3
      , C.cfgWarmup = 500
      , C.cfgBudget = 50000
      }
  , R.tsSeed   = Just fixedSeed
  , R.tsNote   = Just "pure targets: batch=1 (no inner repetition)"
  , R.tsCases  =
      [ R.TraceCase "H0 control @0 vs @0" R.SingleShot (posCase (0, 0))
      , R.TraceCase "position @0 vs @7" R.SingleShot   (posCase (0, 7))
      , R.TraceCase "position @0 vs @31" R.SingleShot  (posCase (0, 31))
      , R.TraceCase "position @0 vs @63" R.SingleShot  (posCase (0, 63))
      ]
  }
