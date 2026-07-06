{-# LANGUAGE BangPatterns #-}

-- censor-poly: record censor runs of ppad-poly1305's constant-time
-- MAC comparison under a selectable meter, dumping per-pair
-- trajectories as JSON for plotting.
--
-- Usage: censor-poly [wall|instructions|cycles|branches] [OUT.json]
--
-- The meter is chosen by the first argument (default wall). The PMU
-- meters are Linux-only; asking for one elsewhere is a hard error
-- rather than a silent fallback. The output path defaults to
-- censor-poly-<meter>.json.
--
-- The recording driver mirrors Censor.runCT -- same K=4 mixture,
-- warmup-derived clip bound, and effect-size confidence sequence,
-- all from ppad-eproc's public API -- but records the state
-- trajectory after every pair, which runCT does not expose.
--
-- Three cases are recorded per run, all comparing 16-byte Poly1305
-- tags that differ in exactly one byte (same terminal result,
-- False), varying only where the mismatch sits relative to the
-- comparison's two 64-bit lanes ([0-7] and [8-15]):
--
--   * H0 control  (@0 vs @0):   identical classes; must PASS.
--   * same-lane   (@0 vs @7):   both mismatches in lane 0.
--   * cross-lane  (@0 vs @15):  mismatches straddle the lanes.
--
-- The unrolled comparison is constant in instruction and branch
-- counts regardless of position, so under the deterministic PMU
-- meters every case should PASS; any separation between the cases
-- is an analog, operand-value-dependent effect visible only to the
-- wall-clock and cycle meters.

module Main where

import Control.Exception (evaluate, try)
import qualified Censor as C
import qualified Crypto.MAC.Poly1305 as Poly1305
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

-- a ~4 ns comparison sits well below the ~42 ns Apple wall-clock
-- quantum, so batch the timed region as the censor-demo MAC cases
-- do.
batch :: Int
batch = 2000

budget :: Int
budget = 50000

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
  , tVerdict :: !String
  , tPairs   :: !Int
  , tPValue  :: !Double
  , tBound   :: !Double
  , tLo      :: !Double
  , tHi      :: !Double
  , tPoints  :: ![Point]
  }

-- the recording driver. mirrors Censor.runCT's construction under
-- the supplied meter: warmup estimates the clip bound c from
-- ~p99(|d|), the main loop randomizes A/B order per pair and streams
-- d through the K=4 hedge plus the [-16c, 16c] confidence sequence,
-- halting on rejection or budget.
runTrace :: String -> C.Meter -> C.Hypothesis a -> IO Trace
runTrace name meter hyp = do
  g <- C.mkRng 0xCAFE
  let pair flipOrder
        | flipOrder = do
            !b <- C.sampleB hyp
            !y <- C.measure meter batch (C.target hyp b)
            !a <- C.sampleA hyp
            !x <- C.measure meter batch (C.target hyp a)
            pure (x, y)
        | otherwise = do
            !a <- C.sampleA hyp
            !x <- C.measure meter batch (C.target hyp a)
            !b <- C.sampleB hyp
            !y <- C.measure meter batch (C.target hyp b)
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

-- keep the asset small: all of the first 150 frames (the early
-- motion), then a uniform stride, then the final frame.
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

-- a Poly1305 MAC comparison hypothesis: both classes compare a fresh
-- tag against a copy differing in exactly one byte, class A flipping
-- byte i0 and class B flipping byte i1. Same terminal result (False)
-- on both sides; the classes differ only in mismatch position.
posCase :: Int -> Int -> IO (C.Hypothesis (Poly1305.MAC, Poly1305.MAC))
posCase i0 i1 = do
  g <- C.mkRng fixedSeed
  let flipAt i bs = BS.pack
        [ if j == i then B.xor w 0xFF else w
        | (j, w) <- zip [0 :: Int ..] (BS.unpack bs) ]
      mk i = do
        !k <- C.randomBytes g 32
        !m <- C.randomBytes g 256
        !raw <- case Poly1305.mac k m of
          Just (Poly1305.MAC r) -> evaluate r
          Nothing               -> die "censor-poly: poly1305 mac failed"
        !t <- evaluate (Poly1305.MAC raw)
        !u <- evaluate (Poly1305.MAC (flipAt i raw))
        pure (t, u)
  pure C.Hypothesis
    { C.target  = \(!a, !b) -> do
        !_ <- evaluate (a == b)
        pure ()
    , C.sampleA = mk i0
    , C.sampleB = mk i1
    }

cases :: [(String, Int, Int)]
cases =
  [ ("H0 control @0 vs @0",  0, 0)
  , ("same-lane @0 vs @7",   0, 7)
  , ("cross-lane @0 vs @15", 0, 15)
  ]

-- meter selection -------------------------------------------------------------

data MeterChoice
  = MeterWall
  | MeterPMU !C.Counter !String

parseMeter :: String -> Maybe MeterChoice
parseMeter s = case s of
  "wall"         -> Just MeterWall
  "instructions" -> Just (MeterPMU C.InstructionsRetired "instructions")
  "cycles"       -> Just (MeterPMU C.Cycles "cycles")
  "branches"     -> Just (MeterPMU C.BranchesRetired "branches")
  _              -> Nothing

meterLabel :: MeterChoice -> String
meterLabel MeterWall           = "wall"
meterLabel (MeterPMU _ label)  = label

-- run the callback with the chosen meter, bracketing the PMU counter
-- and failing hard if it cannot be opened (e.g. off Linux).
withChosenMeter :: MeterChoice -> (C.Meter -> IO ()) -> IO ()
withChosenMeter MeterWall k = k C.wallClock
withChosenMeter (MeterPMU counter label) k = do
  r <- try (C.withCounter counter k) :: IO (Either C.MeterError ())
  case r of
    Left e   -> die $ "censor-poly: " ++ label
      ++ " meter unavailable: " ++ show e
    Right () -> pure ()

-- json ------------------------------------------------------------------------

renderTrace :: Trace -> String
renderTrace t = concat
  [ "{\"name\":\"", tName t, "\""
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
  (choice, out) <- case args of
    []               -> pure (MeterWall, defaultOut MeterWall)
    [m]              -> withParsed m $ \c -> (c, defaultOut c)
    [m, path]        -> withParsed m $ \c -> (c, path)
    _                -> usage
  putStrLn $ "meter: " ++ meterLabel choice
    ++ ", batch=" ++ show batch ++ ", budget=" ++ show budget
  hFlush stdout
  withChosenMeter choice $ \meter -> do
    traces <- mapM (record meter) cases
    writeFile out $ concat
      [ "{\"meter\":\"", meterLabel choice, "\""
      , ",\"batch\":", show batch
      , ",\"alpha\":", printf "%.1g" alpha
      , ",\"thresh\":", printf "%.6f" (log (1 / alpha))
      , ",\"cases\":[", intercalate "," (map renderTrace traces), "]}"
      ]
    putStrLn ("wrote " ++ out)
  where
    withParsed m f = case parseMeter m of
      Just c  -> pure (f c)
      Nothing -> usage
    usage = die "usage: censor-poly [wall|instructions|cycles|branches] [OUT.json]"
    defaultOut c = "censor-poly-" ++ meterLabel c ++ ".json"
    record meter (name, i0, i1) = do
      putStrLn ("recording " ++ name ++ "...") >> hFlush stdout
      h <- posCase i0 i1
      t <- runTrace name meter h
      printf "  %s after %d pairs\n" (tVerdict t) (tPairs t)
      pure t
