{-# LANGUAGE BangPatterns #-}

-- censor-poly: record censor runs of ppad-poly1305's constant-time
-- MAC comparison under a selectable meter, dumping per-pair
-- trajectories as JSON for plotting.
--
-- Usage: censor-poly [--format pretty|json|plain]
--                    [wall|instructions|cycles|branches] [OUT.json]
--
-- 'Censor.Runner' supplies meter selection, trace recording (via
-- 'Censor.runCTWith', not a reimplemented driver), and the trace-
-- JSON rendering; 'Censor.Runner.Report' owns the human-facing
-- report; 'Censor.calibrateBatch' picks the inner-loop batch. What
-- remains here is the hypothesis and the case list.

module Main where

import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Censor as C
import Censor.Runner
import qualified Censor.Runner.Report as R
import qualified Crypto.MAC.Poly1305 as Poly1305
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified System.Environment as Env
import System.Exit (die)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import Text.Printf (printf)

cfg :: C.Config
cfg = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgWarmup = 500
  , C.cfgBudget = 50000
  }

fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- a Poly1305 MAC comparison hypothesis: both classes compare a fresh
-- 16-byte tag against a copy differing in exactly one byte, class A
-- flipping byte i0 and class B flipping byte i1. Same terminal result
-- (False); the classes differ only in mismatch position, relative to
-- the comparison's two 64-bit lanes ([0-7] and [8-15]).
posCase :: Int -> Int -> IO (C.Hypothesis (Poly1305.MAC, Poly1305.MAC))
posCase i0 i1 = do
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

cases :: [(String, Int, Int)]
cases =
  [ ("H0 control @0 vs @0",  0, 0)
  , ("same-lane @0 vs @7",   0, 7)
  , ("cross-lane @0 vs @15", 0, 15)
  ]

version :: String
version = "0.2.0"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  raw <- Env.getArgs
  let (farg, rest) = R.takeFormatArg raw
      (marg, out) = case rest of
        []      -> (Nothing, Nothing)
        [m]     -> (Just m,  Nothing)
        (m:o:_) -> (Just m,  Just o)
  rnd <- R.resolveFormat farg
  rc  <- R.initReportCfg
  withMeterArg marg $ \mname meter -> do
    h0 <- posCase 0 0
    b  <- C.calibrateBatch meter h0
    let c   = cfg { C.cfgBatch = b }
        hdr = R.ReportHeader
          { R.rhSuite   = "censor-poly"
          , R.rhVersion = version
          , R.rhMeter   = mname
          , R.rhConfig  = c
          , R.rhSeed    = Just fixedSeed
          , R.rhTarget  =
              Just "ppad-poly1305 MAC == (mismatch-position independence)"
          , R.rhNote    = Just "batch auto-calibrated"
          }
        path = fromMaybe ("censor-poly-" ++ mname ++ ".json") out
    _ <- R.withReport rc rnd hdr $ \emit -> do
      traces <- forM cases $ \(name, i0, i1) -> do
        h <- posCase i0 i1
        t <- record name mname meter c h
        emit (R.CaseReport name (traceResult t) Nothing)
        pure t
      writeFile path $ concat
        [ "{\"meter\":\"", mname, "\""
        , printf ",\"alpha\":%.1g"  (C.cfgAlpha c)
        , printf ",\"thresh\":%.6f" (log (1 / C.cfgAlpha c))
        , ",\"cases\":[", intercalate "," (map renderTrace traces), "]}"
        ]
      pure (Just path)
    pure ()
