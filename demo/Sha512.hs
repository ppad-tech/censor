{-# LANGUAGE BangPatterns #-}

-- censor-sha512: record censor runs of ppad-sha512's constant-time
-- HMAC-tag comparison under a selectable meter, dumping per-pair
-- trajectories as JSON for plotting.
--
-- Usage: censor-sha512 [wall|instructions|cycles|branches] [OUT.json]
--
-- This is the whole tool: 'Censor.Runner' supplies meter selection,
-- trace recording (via Censor.runCTWith, not a reimplemented driver),
-- and the JSON rendering; Censor.calibrateBatch picks the batch. What
-- remains here is the hypothesis and the case list.

module Main where

import Control.Exception (evaluate)
import qualified Censor as C
import Censor.Runner
import qualified Crypto.Hash.SHA512 as SHA512
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

-- a SHA-512 MAC comparison hypothesis: both classes compare a fresh
-- 64-byte tag against a copy differing in exactly one byte, class A
-- flipping byte i0 and class B flipping byte i1. Same terminal result
-- (False); the classes differ only in mismatch position.
posCase :: Int -> Int -> IO (C.Hypothesis (SHA512.MAC, SHA512.MAC))
posCase i0 i1 = do
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

cases :: [(String, Int, Int)]
cases =
  [ ("H0 control @0 vs @0", 0, 0)
  , ("position @0 vs @7",   0, 7)
  , ("position @0 vs @31",  0, 31)
  , ("position @0 vs @63",  0, 63)
  ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- Env.getArgs
  let (marg, out) = case args of
        []      -> (Nothing, Nothing)
        [m]     -> (Just m,  Nothing)
        (m:o:_) -> (Just m,  Just o)
  withMeterArg marg $ \mname meter -> do
    h0 <- posCase 0 0
    b  <- C.calibrateBatch meter h0
    let c = cfg { C.cfgBatch = b }
    printf "meter: %s, batch=%d (auto), budget=%d\n" mname b (C.cfgBudget c)
    traces <- mapM (recordCase mname meter c) cases
    let path = fromMaybe ("censor-sha512-" ++ mname ++ ".json") out
    writeFile path $ concat
      [ "{\"meter\":\"", mname, "\""
      , printf ",\"alpha\":%.1g" (C.cfgAlpha c)
      , printf ",\"thresh\":%.6f" (log (1 / C.cfgAlpha c))
      , ",\"cases\":[", intercalate "," (map renderTrace traces), "]}"
      ]
    putStrLn ("wrote " ++ path)
  where
    recordCase mname meter c (name, i0, i1) = do
      h <- posCase i0 i1
      t <- record name mname meter c h
      printf "  %-24s %s after %d pairs\n"
        name (verdictName (traceResult t)) (C.resPairs (traceResult t))
      pure t
