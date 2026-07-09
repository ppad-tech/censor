{-# LANGUAGE BangPatterns #-}

-- censor-poly: record censor runs of ppad-poly1305's constant-time
-- MAC comparison under a selectable meter, dumping per-pair
-- trajectories as JSON for plotting.
--
-- Usage: censor-poly [--format pretty|json|plain]
--                    [wall|instructions|cycles|branches] [OUT.json]
--
-- Everything besides the hypothesis and the case list is handled
-- by 'Censor.Runner.Report.runTraceDemo'.

module Main where

import Control.Exception (evaluate)
import qualified Censor as C
import qualified Censor.Runner.Report as R
import qualified Crypto.MAC.Poly1305 as Poly1305
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.Word (Word64)
import System.Exit (die)

fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- Both classes compare a fresh 16-byte tag against a copy differing
-- in exactly one byte, class A flipping byte i0 and class B flipping
-- byte i1. Same terminal result (False); the classes differ only in
-- mismatch position, relative to the comparison's two 64-bit lanes
-- ([0-7] and [8-15]).
posCase :: (Int, Int) -> IO (C.Hypothesis (Poly1305.MAC, Poly1305.MAC))
posCase (i0, i1) = do
  g <- C.mkRng fixedSeed
  let flipAt i bs = BS.pack
        [ if j == i then B.xor w 0xFF else w
        | (j, w) <- zip [0 :: Int ..] (BS.unpack bs) ]
      mk i = do
        !k   <- C.randomBytes g 32
        !m   <- C.randomBytes g 256
        !raw <- case Poly1305.mac k m of
          Just (Poly1305.MAC r) -> evaluate r
          Nothing               -> die "censor-poly: poly1305 mac failed"
        !t <- evaluate (Poly1305.MAC raw)
        !u <- evaluate (Poly1305.MAC (flipAt i raw))
        pure (t, u)
  pure C.Hypothesis
    { C.target  = \(!a, !b) -> () <$ evaluate (a == b)
    , C.sampleA = mk i0
    , C.sampleB = mk i1
    }

main :: IO ()
main = R.runTraceDemo R.TraceDemo
  { R.tdSuite   = "censor-poly"
  , R.tdVersion = "0.2.0"
  , R.tdTarget  = "ppad-poly1305 MAC == (mismatch-position independence)"
  , R.tdConfig  = C.defaultConfig
      { C.cfgAlpha  = 1.0e-3
      , C.cfgWarmup = 500
      , C.cfgBudget = 50000
      }
  , R.tdSeed    = Just fixedSeed
  , R.tdNote    = Just "batch auto-calibrated"
  , R.tdBuild   = posCase
  , R.tdCases   =
      [ ("H0 control @0 vs @0",  (0, 0))
      , ("same-lane @0 vs @7",   (0, 7))
      , ("cross-lane @0 vs @15", (0, 15))
      ]
  }
