{-# LANGUAGE BangPatterns #-}

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
import Control.Monad (replicateM_)
import qualified Censor as C
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.List (intercalate, sort)
import Data.Word (Word64)
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

-- deliberately leaky: work proportional to the secret's popcount.
-- fix-vs-random on the secret gives class A a pinned popcount and
-- class B the binomial spread around 32, so timing depends on the
-- secret and the mixture races to a rejection.
leaky :: IO (C.Hypothesis Word64)
leaky = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\w -> replicateM_ (B.popCount w) (evaluate ()))
    (C.nextWord g)
    pure

-- constant-time: HMAC-SHA256 under a pinned vs random key, fresh
-- 256-byte messages in both classes. mirrors the censor-demo case.
ct :: IO (C.Hypothesis (BS.ByteString, BS.ByteString))
ct = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(k, m) -> do
        !_ <- evaluate (SHA256.hmac k m)
        pure ())
    (C.randomBytes g 32)
    (\k -> do
        !m <- C.randomBytes g 256
        pure (k, m))

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
  putStrLn "recording leaky case..." >> hFlush stdout
  hl <- leaky
  tl <- runTrace "popcount-driven loop" 20 20000 hl
  printf "  %s after %d pairs\n" (tVerdict tl) (tPairs tl)
  putStrLn "recording constant-time case..." >> hFlush stdout
  hc <- ct
  tc <- runTrace "SHA256.hmac" 50 50000 hc
  printf "  %s after %d pairs\n" (tVerdict tc) (tPairs tc)
  writeFile out $ concat
    [ "{\"alpha\":", printf "%.1g" alpha
    , ",\"thresh\":", printf "%.6f" (log (1 / alpha))
    , ",\"cases\":[", renderTrace tl, ",", renderTrace tc, "]}"
    ]
  putStrLn ("wrote " ++ out)
