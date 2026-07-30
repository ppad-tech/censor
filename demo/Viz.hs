{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

-- censor-viz: record real runs of the censor construction and dump
-- per-pair trajectories as JSON, for the ppad.tech product-page
-- animation.
--
-- The driver is Censor's own: Censor.Runner.record runs runCTWith
-- and keeps the per-pair Frame it hands back, so the trajectory is
-- the real construction and cannot drift from it. Measurements are
-- real wall-clock timings of real targets; nothing in the emitted
-- trace is synthesized.
--
-- Usage: censor-viz [OUTPUT.json]

module Main where

import Control.Exception (evaluate)
import qualified Censor as C
import qualified Censor.Runner as R
import qualified Crypto.Curve.Secp256k1 as Secp
import qualified Data.Choice as Ch
import Data.List (intercalate)
import Data.Word (Word64)
import qualified Data.Word.Wider as W
import Data.Word.Wider (Wider)
import qualified System.Environment as Env
import System.Exit (die)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- parameters ------------------------------------------------------------------

alpha :: Double
alpha = 1.0e-3

nWarmup :: Int
nWarmup = 500

fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- seed chosen so the leaky case's pinned scalar has Hamming weight
-- 129, a single set bit above the binomial mean of the random
-- class. the leak's mean-shift component is then one point
-- addition per call (well under 1us of a ~300us operation), a far
-- subtler signal than a typical pinned draw's ~8-bit deviation
-- would produce.
leakySeed :: Word64
leakySeed = 0x5EED000000000033

-- trajectory ------------------------------------------------------------------

-- one recorded frame: pair index, mixture log e-value (current and
-- supremum), and the effect interval in meter units per batch.
data Point = Point
  { pN   :: !Int
  , pCur :: !Double
  , pSup :: !Double
  , pLo  :: !Double
  , pHi  :: !Double
  }

data Trace = Trace
  { tName    :: !String
  , tBatch   :: !Int
  , tVerdict :: !String
  , tPairs   :: !Int
  , tPValue  :: !Double
  , tBound   :: !Double
  , tLo      :: !Double
  , tHi      :: !Double
  , tPoints  :: ![Point]
  }

-- the recording driver. This *is* Censor's driver: runCTWith
-- delivers a Frame after every main-phase pair, which carries
-- exactly the trajectory this animation needs. Reimplementing the
-- construction here would only guarantee it drifts out of step with
-- the library -- as it did when the CDF-indicator components joined
-- the hedge.
runTrace
  :: String -> Int -> Int -> C.Hypothesis a -> IO Trace
runTrace name batch budget hyp = do
  let cfg = C.defaultConfig
        { C.cfgAlpha  = alpha
        , C.cfgBudget = budget
        , C.cfgWarmup = nWarmup
        , C.cfgBatch  = batch
        , C.cfgSeed   = Just 0xCAFE
        }
  t <- R.record name "wall" C.wallClock cfg hyp
  let r = R.traceResult t
  pure Trace
    { tName    = name
    , tBatch   = batch
    , tVerdict = R.verdictName r
    , tPairs   = C.resPairs r
    , tPValue  = C.resPValue r
    , tBound   = C.resBound r
    , tLo      = fst (C.resEffect r)
    , tHi      = snd (C.resEffect r)
    , tPoints  = map toPoint (R.thin (R.traceFrames t))
    }

toPoint :: C.Frame -> Point
toPoint fr = Point
  { pN   = C.frPair fr
  , pCur = C.frLogW fr
  , pSup = C.frLogWSup fr
  , pLo  = C.frLo fr
  , pHi  = C.frHi fr
  }

-- cases -----------------------------------------------------------------------

-- a random 256-bit scalar in normal form, as in censor-demo
randomWider :: C.Rng -> IO Wider
randomWider g = do
  !a <- C.nextWord g
  !b <- C.nextWord g
  !c <- C.nextWord g
  !d <- C.nextWord g
  pure $! W.wider (fromIntegral a) (fromIntegral b)
                  (fromIntegral c) (fromIntegral d)

-- a busted scalar mul over the library's exported group
-- operations: fixed-length double-and-add whose loop always runs
-- 256 iterations, so the scalar's bit length does not leak -- but
-- the per-bit add is branched on the bit value rather than
-- constant-time selected:
--
--   time = const + 256 c_dbl + popcount(s) c_add
--
-- an isolated Hamming-weight leak, far subtler than an
-- early-terminating variable-time mul.
leakyMul :: Wider -> Secp.Projective
leakyMul = loop (256 :: Int) Secp._CURVE_ZERO Secp._CURVE_G
  where
    loop !i !r !d !m
      | i == 0    = r
      | otherwise =
          let !nd = Secp.double d
              !(# nm, lsb #) = W.shr1_c m
              !nr | Ch.decide lsb = Secp.add_proj r d
                  | otherwise     = r
          in  loop (i - 1) nr nd nm

-- deliberately leaky: fix-vs-random on the scalar pins class A's
-- popcount while class B's spreads binomially around 128, so the
-- add count depends on the secret and the mixture hunts down the
-- Hamming-weight signal.
leaky :: IO (C.Hypothesis Wider)
leaky = do
  g <- C.mkRng leakySeed
  C.fixVsRandom
    (\s -> do
        !_ <- evaluate (leakyMul s)
        pure ())
    (randomWider g)
    pure

-- constant-time: the same operation done properly, via the ladder
-- ppad-secp256k1 uses for secret scalars.
ct :: IO (C.Hypothesis Wider)
ct = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\s -> do
        !_ <- evaluate (Secp.mul Secp._CURVE_G s)
        pure ())
    (randomWider g)
    pure

-- json ------------------------------------------------------------------------

renderTrace :: Trace -> String
renderTrace t = concat
  [ "{\"name\":\"", tName t, "\""
  , ",\"batch\":", show (tBatch t)
  , ",\"verdict\":\"", tVerdict t, "\""
  , ",\"pairs\":", show (tPairs t)
  , printf ",\"pvalue\":%.6g" (tPValue t)
  , printf ",\"bound\":%.6g" (tBound t)
  , printf ",\"effect\":[%.1f,%.1f]" (tLo t) (tHi t)
  , ",\"trace\":["
  , intercalate ","
      [ printf "[%d,%.4f,%.4f,%.1f,%.1f]"
          (pN p) (pCur p) (pSup p) (pLo p) (pHi p)
      | p <- tPoints t ]
  , "]}"
  ]

main :: IO ()
main = do
  args <- Env.getArgs
  let out = case args of
        [path] -> path
        _      -> "censor-trace.json"
  -- the busted mul must still compute the right points
  g0 <- C.mkRng 0xB0B
  s0 <- randomWider g0
  case Secp.mul Secp._CURVE_G s0 of
    Just q | show (Secp.affine (leakyMul s0)) == show (Secp.affine q)
      -> pure ()
    _ -> die "censor-viz: leakyMul disagrees with Secp.mul"
  putStrLn "recording leaky case..." >> hFlush stdout
  hl <- leaky
  tl <- runTrace "branchy double-and-add" 20 20000 hl
  printf "  %s after %d pairs\n" (tVerdict tl) (tPairs tl)
  putStrLn "recording constant-time case..." >> hFlush stdout
  hc <- ct
  tc <- runTrace "Secp256k1.mul" 20 50000 hc
  printf "  %s after %d pairs\n" (tVerdict tc) (tPairs tc)
  writeFile out $ concat
    [ "{\"alpha\":", printf "%.1g" alpha
    , ",\"thresh\":", printf "%.6f" (log (1 / alpha))
    , ",\"cases\":[", renderTrace tl, ",", renderTrace tc, "]}"
    ]
  putStrLn ("wrote " ++ out)
