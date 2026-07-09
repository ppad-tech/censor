{-# LANGUAGE BangPatterns #-}

-- censor-sha: record censor runs of ppad-sha256's constant-time
-- HMAC-tag comparison under a selectable meter, dumping per-pair
-- trajectories as JSON for plotting.
--
-- Usage: censor-sha [--format pretty|json|plain]
--                   [wall|instructions|cycles|branches] [OUT.json]
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
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import qualified System.Environment as Env
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

-- a Poly1305/SHA-256 MAC comparison hypothesis: both classes compare
-- a fresh 32-byte tag against a copy differing in exactly one byte,
-- class A flipping byte i0 and class B flipping byte i1. Same terminal
-- result (False); the classes differ only in mismatch position.
posCase :: Int -> Int -> IO (C.Hypothesis (SHA256.MAC, SHA256.MAC))
posCase i0 i1 = do
  g <- C.mkRng fixedSeed
  let flipAt i bs = BS.pack
        [ if j == i then B.xor w 0xFF else w
        | (j, w) <- zip [0 :: Int ..] (BS.unpack bs) ]
      mk i = do
        !k <- C.randomBytes g 32
        !m <- C.randomBytes g 256
        let SHA256.MAC raw = SHA256.hmac k m
        !t <- evaluate (SHA256.MAC raw)
        !u <- evaluate (SHA256.MAC (flipAt i raw))
        pure (t, u)
  pure C.Hypothesis
    { C.target  = \(!a, !b) -> () <$ evaluate (a == b)
    , C.sampleA = mk i0
    , C.sampleB = mk i1
    }

cases :: [(String, Int, Int)]
cases =
  [ ("H0 control @0 vs @0", 0, 0)
  , ("position @0 vs @7",   0, 7)
  , ("position @0 vs @15",  0, 15)
  , ("position @0 vs @31",  0, 31)
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
    -- auto-batch: calibrate once on the target under this meter.
    h0 <- posCase 0 0
    b  <- C.calibrateBatch meter h0
    let c   = cfg { C.cfgBatch = b }
        hdr = R.ReportHeader
          { R.rhSuite   = "censor-sha"
          , R.rhVersion = version
          , R.rhMeter   = mname
          , R.rhConfig  = c
          , R.rhSeed    = Just fixedSeed
          , R.rhTarget  =
              Just "ppad-sha256 HMAC MAC == (mismatch-position independence)"
          , R.rhNote    = Just "batch auto-calibrated"
          }
        path = fromMaybe ("censor-sha-" ++ mname ++ ".json") out
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
