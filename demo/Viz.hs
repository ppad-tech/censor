{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

-- censor-viz: record real runs of the censor construction and dump
-- per-pair trajectories as JSON, for the ppad.tech product-page
-- animation.
--
-- The driver mirrors Censor.runCT — same components, same
-- warmup-derived clip bound, same K=4 mixture and effect-size
-- confidence sequence, built from ppad-eproc's public API — but
-- records the state trajectory after every pair, which runCT
-- deliberately does not expose. Measurements are real wall-clock
-- timings of real targets; nothing in the emitted trace is
-- synthesized.
--
-- Usage: censor-viz [OUTPUT.json]

module Main where

import Control.Exception (evaluate)
import qualified Censor as C
import qualified Crypto.Curve.Secp256k1 as Secp
import qualified Data.Bits as B
import qualified Data.Choice as Ch
import Data.List (intercalate, sort)
import Data.Word (Word64)
import qualified Data.Word.Wider as W
import Data.Word.Wider (Wider)
import qualified Numeric.Eproc.Bernoulli.TwoSided as EPSign
import qualified Numeric.Eproc.Bounded as EPMagn
import qualified Numeric.Eproc.ConfSeq as EPCS
import qualified Numeric.Eproc.Mixture as EPMix
import Numeric.Eproc.Bounded (Bettor(Newton))
import qualified System.Environment as Env
import System.Exit (die)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- parameters ------------------------------------------------------------------

alpha :: Double
alpha = 1.0e-3

nWarmup :: Int
nWarmup = 500

effectGrid :: Int
effectGrid = 300

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

-- the recording driver. mirrors Censor.runCT's construction:
-- warmup estimates the clip bound c from ~p99(|d|), the main loop
-- randomizes A/B order per pair and streams d through the K=4
-- hedge (delegating combination to Numeric.Eproc.Mixture) plus the
-- [-16c, 16c] confidence sequence, halting on rejection or budget.
runTrace
  :: String -> Int -> Int -> C.Hypothesis a -> IO Trace
runTrace name batch budget hyp = do
  g <- C.mkRng 0xCAFE
  let pair flipOrder
        | flipOrder = do
            !b <- C.sampleB hyp
            !y <- C.measure C.wallClock batch (C.target hyp b)
            !a <- C.sampleA hyp
            !x <- C.measure C.wallClock batch (C.target hyp a)
            pure (x, y)
        | otherwise = do
            !a <- C.sampleA hyp
            !x <- C.measure C.wallClock batch (C.target hyp a)
            !b <- C.sampleB hyp
            !y <- C.measure C.wallClock batch (C.target hyp b)
            pure (x, y)
      orderBit = do
        !w <- C.nextWord g
        pure (B.testBit w 0)

  -- warmup: clip bound from ~p99 of |d|, as in runCT
  let warm !i !acc
        | i >= nWarmup = pure acc
        | otherwise = do
            o <- orderBit
            (!x, !y) <- pair o
            let !d = abs (fromIntegral x - fromIntegral y) :: Double
            warm (i + 1) (d : acc)
  diffs <- warm 0 []
  let !c = max 1 (2 * orderStat99 diffs)
      !c3 = 16 * c

  sc  <- confok (EPSign.config 0.5 alpha Newton)
  m1c <- confok (EPMagn.config 0 (negate c)      c       alpha Newton)
  m2c <- confok (EPMagn.config 0 (negate (4*c))  (4*c)   alpha Newton)
  m3c <- confok (EPMagn.config 0 (negate c3)     c3      alpha Newton)
  mxc <- confok (EPMix.config 4 alpha)
  csc <- confok (EPCS.config (negate c3) c3 alpha effectGrid)

  let clip !lim !x | abs x > lim = if x > 0 then lim else negate lim
                   | otherwise   = x
      go !n !s !m1 !m2 !m3 !mx !cs !iv !acc
        | n >= budget =
            pure (finish "PASS" n mx iv acc)
        | otherwise = do
            o <- orderBit
            (!x, !y) <- pair o
            let !d   = fromIntegral x - fromIntegral y :: Double
                !s'  = if d == 0 then s
                       else EPSign.update sc s (d > 0)
                !m1' = EPMagn.update m1c m1 (clip c d)
                !m2' = EPMagn.update m2c m2 (clip (4*c) d)
                !m3' = EPMagn.update m3c m3 (clip c3 d)
                !mx' = EPMix.update mxc mx
                         [ EPSign.log_evalue s'
                         , EPMagn.log_evalue m1'
                         , EPMagn.log_evalue m2'
                         , EPMagn.log_evalue m3'
                         ]
                !cs' = EPCS.update csc cs (clip c3 d)
                !iv' = case EPCS.interval csc cs' of
                         Just v  -> v
                         Nothing -> iv
                !n'  = n + 1
                !pt  = Point n'
                         (EPMix.log_evalue mxc mx')
                         (EPMix.log_evalue_sup mxc mx')
                         (fst iv') (snd iv')
            case EPMix.decide mxc mx' of
              EPMix.Reject ->
                pure (finish "REJECT" n' mx' iv' (pt : acc))
              _ ->
                go n' s' m1' m2' m3' mx' cs' iv' (pt : acc)
      finish verdict n mx iv acc = Trace
        { tName    = name
        , tBatch   = batch
        , tVerdict = verdict
        , tPairs   = n
        , tPValue  = EPMix.p_value mxc mx
        , tBound   = c
        , tLo      = fst iv
        , tHi      = snd iv
        , tPoints  = thin (reverse acc)
        }
  go 0 (EPSign.initial sc)
       (EPMagn.initial m1c) (EPMagn.initial m2c) (EPMagn.initial m3c)
       (EPMix.initial mxc) (EPCS.initial csc)
       (negate c3, c3)
       []

confok :: Show e => Either e a -> IO a
confok = either (die . show) pure

-- keep the animation asset small: all of the first 150 frames (the
-- early motion), then a uniform stride, then the final frame.
thin :: [Point] -> [Point]
thin pts =
  let !m = length pts
      !stride = max 1 (m `div` 450)
      keep p = pN p <= 150 || pN p `mod` stride == 0 || pN p == m
  in  filter keep pts

-- ~p99 order statistic, as in Censor's warmup
orderStat99 :: [Double] -> Double
orderStat99 xs =
  let !sorted = sort xs
      !m      = length sorted
      !idx    = max 0 (min (m - 1) ((m * 99) `div` 100))
  in  case drop idx sorted of
        (x : _) -> x
        []      -> 0

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
