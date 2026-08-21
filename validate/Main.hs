{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- censor-validate: empirical calibration sweep for ppad-censor.
--
-- Drives runCT under synthetic meters with known H_0 / H_1
-- distributions, counts rejections across N independent trials per
-- cell, and reports Wilson 95% binomial CIs against each cell's
-- alpha bound. Bonferroni-adjusts confidence across the cell grid
-- so a family-wise PASS / FAIL / INCONCLUSIVE verdict means
-- something.
--
-- Use case: pre-release calibration check. A full N = 10000 sweep
-- takes ~10 minutes; pass a smaller N as the first argument
-- (e.g. `censor-validate 1000`) for a fast pass while iterating.
--
-- `censor-validate --power-gain [METER]` runs a different job: the
-- host power-coupling probe, a cycle-identical but maximally
-- power-asymmetric positive control measured on the *real* host
-- (default meter: wall). See the probe section below.

module Main where

import qualified Censor as C
import qualified Censor.Runner as R
import Control.Monad (forM, forM_, unless)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeElemOff)
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as IO
import Text.Printf (printf)
import Text.Read (readMaybe)

-- splitmix64 -----------------------------------------------------------------

-- Internal PRNG for the synthetic distributions, from Censor.Rng.
-- A separate generator is used for every stream; deterministic
-- seeding lets failures reproduce.

-- uniform integer in @[lo, hi]@, inclusive.
uniformInt :: C.Rng -> Word64 -> Word64 -> IO Word64
uniformInt !rng !lo !hi
  | hi <= lo  = pure lo
  | otherwise = do
      !w <- C.nextWord rng
      let !range = hi - lo + 1
      pure $! lo + (w `mod` range)

-- Wilson CI + verdict --------------------------------------------------------

-- two-sided z critical value, ladder. Rounds *up* so the resulting
-- CI is conservatively wide and verdicts are cautious.
zCritical :: Double -> Double
zCritical !conf
  | conf > 0.99999 = 4.892
  | conf > 0.9999  = 4.417
  | conf > 0.999   = 3.890
  | conf > 0.99    = 3.291
  | conf > 0.95    = 2.576
  | otherwise      = 1.960

-- Wilson score interval for a binomial proportion. Has better
-- coverage than the naive normal approximation when p is near 0,
-- which is exactly our regime.
wilsonCI :: Int -> Int -> Double -> (Double, Double)
wilsonCI !k !n !z
  | n <= 0    = (0, 1)
  | otherwise =
      let !nn  = fromIntegral n
          !kk  = fromIntegral k
          !p   = kk / nn
          !z2  = z * z
          !den = 1 + z2 / nn
          !ctr = (p + z2 / (2 * nn)) / den
          !rad = (z * sqrt (p * (1 - p) / nn + z2 / (4 * nn * nn))) / den
      in  (max 0 (ctr - rad), min 1 (ctr + rad))

data Verdict = VPass | VFail | VInconclusive
  deriving (Eq, Show)

renderVerdict :: Verdict -> String
renderVerdict VPass         = "PASS"
renderVerdict VFail         = "FAIL"
renderVerdict VInconclusive = "INCONC"

-- A cell PASSes iff the CI's upper bound is below alpha (rejection
-- rate is comfortably under the type-I bound); FAILs iff the lower
-- bound is above alpha (rejection rate exceeds the bound — a real
-- statistical violation); INCONCLUSIVE if the CI straddles alpha
-- (need more trials to resolve).
verdictFor :: Double -> (Double, Double) -> Verdict
verdictFor !alpha (!lo, !hi)
  | lo > alpha = VFail
  | hi < alpha = VPass
  | otherwise  = VInconclusive

-- synthetic meter ------------------------------------------------------------

-- The "class" being measured this call. The hypothesis's samplers
-- pass it through the IORef the meter reads.
data Class = ClassA | ClassB

-- Constructs a (Meter, Hypothesis) pair that draws per-pair
-- measurements from two user-supplied streams. The samplers route
-- the class tag through an IORef the meter reads after running the
-- (no-op) target k times, so the meter knows which stream to pull
-- from. cfgBatch is honoured for fidelity to the real driver — the
-- target action runs k times — but only one stream element is
-- consumed per measure call.
mkSyntheticMeter
  :: IO Word64
  -> IO Word64
  -> IO (C.Meter, C.Hypothesis Class)
mkSyntheticMeter !nextA !nextB = do
  ref <- newIORef ClassA
  let !meter = C.Meter $ \k act -> do
        let go !0 = pure ()
            go !i = act >> go (i - 1)
        go k
        c <- readIORef ref
        case c of
          ClassA -> nextA
          ClassB -> nextB
      !hyp = C.Hypothesis
        { C.target  = writeIORef ref
        , C.prepare = pure ()
        , C.sampleA = pure ClassA
        , C.sampleB = pure ClassB
        }
  pure (meter, hyp)

-- scenarios ------------------------------------------------------------------

-- A scenario is a stream factory: each trial calls scInit afresh,
-- producing two new per-class streams (and any state they share).
-- scInit takes the Config so scenarios that need to coordinate
-- with cfgWarmup (the pathological-clipping case) can do so.
data Scenario = Scenario
  { scName :: !String
  , scInit :: !(C.Config -> IO (IO Word64, IO Word64))
  }

scConstant :: Scenario
scConstant = Scenario "constant" $ \_ ->
  pure (pure 100, pure 100)

scSymUniform :: Word64 -> Scenario
scSymUniform !seed = Scenario "sym-uniform-iid" $ \_ -> do
  ra <- C.mkRng seed
  rb <- C.mkRng (seed + 1)
  pure (uniformInt ra 0 200, uniformInt rb 0 200)

-- A uniform over a wide range, B uniform over a narrow centred
-- range. Both have mean 100; the only asymmetry is the variance.
--
-- This is an H_1 scenario, not H_0. Both marginals are symmetric
-- about 100, so d = ta - tb is symmetric about zero and the sign
-- and magnitude channels see nothing. The class CDFs differ
-- everywhere but the centre, though, so the indicator components
-- detect it. It is the cell that distinguishes a null of
-- "d is symmetric" from a null of "the pair is exchangeable".
scAsymUniform :: Word64 -> Scenario
scAsymUniform !seed = Scenario "asym-uniform-equal-mean" $ \_ -> do
  ra <- C.mkRng seed
  rb <- C.mkRng (seed + 1)
  pure (uniformInt ra 0 200, uniformInt rb 50 150)

-- 95% uniform[50, 150] + 5% uniform[5000, 10000] for both classes.
-- Heavy upper tail exercises the clipping branch heavily; equal
-- distributions mean equal means, so H_0 holds.
scHeavyTail :: Word64 -> Scenario
scHeavyTail !seed = Scenario "heavy-tail" $ \_ -> do
  ra <- C.mkRng seed
  rb <- C.mkRng (seed + 1)
  let draw rng = do
        u <- C.nextWord rng
        if u `mod` 20 == 0
          then uniformInt rng 5000 10000
          else uniformInt rng 50   150
  pure (draw ra, draw rb)

-- Per-pair shared deterministic drift. Both classes' measurements
-- shift upward by 10 every pair; per-class noise is i.i.d. uniform
-- around zero. Conditional mean of the difference is exactly zero
-- throughout (the drift cancels), but the marginal expectation is
-- highly non-stationary — exercising the conditional-null regime
-- the order-bit randomisation is supposed to handle.
scDriftingBaseline :: Word64 -> Scenario
scDriftingBaseline !seed = Scenario "drifting-baseline" $ \_ -> do
  ctr <- newIORef (0 :: Int)
  noiseA <- C.mkRng seed
  noiseB <- C.mkRng (seed + 1)
  let draw rng = do
        n <- atomicModifyIORef' ctr (\x -> (x + 1, x))
        let !base = 100 + fromIntegral (n `div` 2) * 10
        u <- C.nextWord rng
        let !noise = fromIntegral (u `mod` 21) - 10
        pure $! fromIntegral (base + noise :: Int)
  pure (draw noiseA, draw noiseB)

-- Heavy main-loop clipping. Each class returns a small reading for
-- the first cfgWarmup calls (the warmup phase, where it is sampled
-- once per pair), then a huge reading thereafter. The warmup-derived
-- bound is small; every main-phase reading exceeds it and is
-- clipped to that bound. Both classes share the same stream, so
-- conditional mean of the difference stays zero — the test is that
-- clipping doesn't *itself* generate spurious rejections.
scAllClipping :: Word64 -> Scenario
scAllClipping !seed = Scenario "pathological-clipping" $ \cfg -> do
  ctrA <- newIORef (0 :: Int)
  ctrB <- newIORef (0 :: Int)
  let !cutover = C.cfgWarmup cfg
      draw ref = do
        n <- atomicModifyIORef' ref (\x -> (x + 1, x))
        if n < cutover then pure 100 else pure 1000000
  let _ = seed  -- reserved for future randomised variants
  pure (draw ctrA, draw ctrB)

-- Readings concentrated right against the upper end of the warmup
-- bound. Tests behaviour at the clipping boundary.
scNearBound :: Word64 -> Scenario
scNearBound !seed = Scenario "near-bound" $ \_ -> do
  ra <- C.mkRng seed
  rb <- C.mkRng (seed + 1)
  pure (uniformInt ra 180 200, uniformInt rb 180 200)

-- Mid-run regime change: warmup and the early main phase see a
-- narrow noise band, after which the noise widens. Conditional mean
-- of the difference stays at zero (both classes shift regime
-- together), but the warmup bound is no longer representative of
-- the main phase — stresses the assumption that warmup calibration
-- characterises the whole run.
scRegimeChange :: Word64 -> Scenario
scRegimeChange !seed = Scenario "regime-change" $ \cfg -> do
  ctr <- newIORef (0 :: Int)
  ra <- C.mkRng seed
  rb <- C.mkRng (seed + 1)
  let !switchAt = 2 * C.cfgWarmup cfg + C.cfgBudget cfg
      draw rng = do
        n <- atomicModifyIORef' ctr (\x -> (x + 1, x))
        if n < switchAt
          then uniformInt rng 50 150
          else uniformInt rng 0  200
  pure (draw ra, draw rb)

-- High baseline with narrow i.i.d. jitter per class. Concretely
-- readings sit at 10000 +/- 10; @|d|@ stays on the noise scale (~20)
-- while readings themselves stay on the baseline scale (~10000).
-- This is the regime where betting on the clipped difference (rather
-- than on the clipped readings) pays off dramatically: @lambda_max@
-- is set by @|d|@ and stays ~500x higher than it would be if it
-- scaled with reading magnitude.
scNarrowJitter :: Word64 -> Scenario
scNarrowJitter !seed = Scenario "narrow-jitter" $ \_ -> do
  ra <- C.mkRng seed
  rb <- C.mkRng (seed + 1)
  pure (uniformInt ra 9990 10010, uniformInt rb 9990 10010)

-- Rare-outlier H_0: both classes are 99% uniform[50,150] + 1%
-- uniform[900,1100]. Independent draws, identical distributions,
-- so the null holds. Included to verify that the multi-clip hedge
-- doesn't spuriously reject under fat-tailed-but-symmetric structure
-- (symmetric clipping preserves symmetry across all three magnitude
-- components, so it shouldn't).
scRareOutlier :: Word64 -> Scenario
scRareOutlier !seed = Scenario "rare-outlier" $ \_ -> do
  ra <- C.mkRng seed
  rb <- C.mkRng (seed + 1)
  let draw rng = do
        u <- C.nextWord rng
        if u `mod` 100 == 0
          then uniformInt rng 900 1100
          else uniformInt rng 50  150
  pure (draw ra, draw rb)

-- Rare-outlier H_1: class A is pure bulk (uniform[50,150]); class B
-- is 99% bulk + 1% spike (uniform[900,1100]). Both classes match in
-- bulk, so the sign test and tight-clip magnitude see almost no
-- signal (the 1% spikes shift @P(d > 0)@ by ~0.005 and get clipped
-- away at bound @c@ by @magn(c)@). Only @magn(16c)@ preserves the
-- spike signal — this is the scenario that exercises the multi-clip
-- rationale for the hedge.
scRareOutlierAsym :: Word64 -> Scenario
scRareOutlierAsym !seed = Scenario "rare-outlier-asym" $ \_ -> do
  ra <- C.mkRng seed
  rb <- C.mkRng (seed + 1)
  let drawA = uniformInt ra 50 150
      drawB = do
        u <- C.nextWord rb
        if u `mod` 100 == 0
          then uniformInt rb 900 1100
          else uniformInt rb 50  150
  pure (drawA, drawB)

-- All H_0 calibration scenarios.
h0Scenarios :: [Scenario]
h0Scenarios =
  [ scConstant
  , scSymUniform        0x5EED01
  , scHeavyTail         0x5EED03
  , scDriftingBaseline  0x5EED04
  , scNearBound         0x5EED05
  , scAllClipping       0x5EED06
  , scRegimeChange      0x5EED07
  , scNarrowJitter      0x5EED08
  , scRareOutlier       0x5EED09
  ]

-- A power scenario adds a positive shift to class B (so
-- E[ta - tb] = -shift). Used to characterise detection rate as a
-- function of effect size.
addShift :: Scenario -> Word64 -> Scenario
addShift !base !shift = base
  { scName = scName base ++ "+shift=" ++ show shift
  , scInit = \cfg -> do
      (a, b) <- scInit base cfg
      pure (a, fmap (+ shift) b)
  }

-- H_1 scenarios for the power table. Pick a couple of base
-- distributions and sweep the shift / budget grid against them.
-- narrow-jitter is included specifically to exhibit the bet-on-|d|
-- power gain: under bet-on-readings the ~500x smaller lambda_max
-- would leave every one of these cells at 0% detection.
powerBases :: [Scenario]
powerBases =
  [ scSymUniform    0xB055ED01
  , scHeavyTail     0xB055ED02
  , scNarrowJitter  0xB055ED03
  ]

powerShifts :: [Word64]
powerShifts = [1, 5, 10, 50]

powerBudgets :: [Int]
powerBudgets = [1000, 10000]

-- cells ----------------------------------------------------------------------

data Cell = Cell
  { cellScenario :: !Scenario
  , cellCfg      :: !C.Config
  , cellGate     :: !(Maybe Double)
    -- ^ alpha bound for PASS/FAIL gating; @Nothing@ means
    --   characterisation only (power cells).
  }

baseCfg :: Int -> Int -> Double -> C.Config
baseCfg !budget !batch !alpha = C.defaultConfig
  { C.cfgAlpha  = alpha
  , C.cfgBudget = budget
  , C.cfgBatch  = batch
  , C.cfgWarmup = 200
  , C.cfgSeed   = Just 0xBA5E11EDBA5E11ED
    -- Pin the order-bit seed so the baseline CSV is reproducible
    -- across runs. Production use of censor should leave cfgSeed at
    -- the default 'Nothing' (fresh entropy per run).
  }

-- Gated cells: alphas where it is feasible to PASS at our N. A
-- PASS at alpha = a requires the Wilson upper bound to fall below
-- a, which (at family confidence) needs roughly N > 3.3^2 / a even
-- with zero observed rejections. So 1e-2 and 1e-3 are gateable at
-- N = 10000; 1e-4 and 1e-6 are characterisation-only (CI reported
-- but no PASS/FAIL).
gatedAlphas, ungatedAlphas :: [Double]
gatedAlphas   = [1.0e-2, 1.0e-3]
ungatedAlphas = [1.0e-4, 1.0e-6]

calibrationCells :: [Cell]
calibrationCells = gated ++ characterised
  where
    gated =
      [ Cell sc (baseCfg 10000 1 a) (Just a)
      | sc <- h0Scenarios, a <- gatedAlphas
      ]
    characterised =
      [ Cell sc (baseCfg 10000 1 a) Nothing
      | sc <- h0Scenarios, a <- ungatedAlphas
      ]

-- Edge-case cells: parameter regimes where the alpha guarantee
-- could plausibly degrade beyond what the calibration sweep
-- exercises. Kept small because most natural variations (cfgBatch,
-- cfgWarmup size) don't materially affect statistical behaviour in
-- synthetic-meter mode.
edgeCaseCells :: [Cell]
edgeCaseCells =
  [ Cell sc (baseCfg 10000 1 1e-3) { C.cfgWarmup = 20 } (Just 1e-3)
  , Cell sc (baseCfg 10000 100 1e-3) (Just 1e-3)
  ]
  where
    sc = scSymUniform 0xED6E01

-- margin (interval-null) cells -----------------------------------------------

-- Cells exercising cfgMargin. A relative margin resolves against
-- the warmup median pooled reading, so each base names the reading
-- scale it was chosen for: narrow-jitter readings sit at ~10000,
-- where rel 1e-3 lands the absolute margin at ~10 against a clip
-- bound of ~40+; heavy-tail readings have median ~100, where rel
-- 5e-2 lands it at ~5 against a clip bound in the thousands.
--
-- The calibration cells shift class B by LESS than the resolved
-- margin, so the interval null |E d_clipped| <= delta is true
-- while the sharp null is false: they validate margin mode's
-- type-I claim under exactly the sub-tolerance systematic the
-- margin exists to absorb. The sharp-contrast power cell runs the
-- same sub-margin shift without a margin and should reject at
-- ~100%, exhibiting the behaviour being ruled out of scope.
marginCfg :: Double -> Int -> Double -> C.Config
marginCfg !rel !budget !alpha =
  (baseCfg budget 1 alpha) { C.cfgMargin = Just rel }

withMarginName :: Double -> Scenario -> Scenario
withMarginName !rel sc =
  sc { scName = scName sc ++ "+margin=" ++ showSci rel }

marginH0Bases :: [(Scenario, Double)]
marginH0Bases =
  [ (scNarrowJitter 0x3A6C01,              1.0e-3)  -- no systematic
  , (addShift (scNarrowJitter 0x3A6C02) 2, 1.0e-3)  -- 2 < delta ~10
  , (addShift (scNarrowJitter 0x3A6C03) 8, 1.0e-3)  -- 8 < delta ~10
  , (addShift (scHeavyTail    0x3A6C04) 2, 5.0e-2)  -- 2 < delta ~5
  ]

marginCalibrationCells :: [Cell]
marginCalibrationCells =
  [ Cell (withMarginName rel sc) (marginCfg rel 10000 a) (Just a)
  | (sc, rel) <- marginH0Bases
  , a         <- gatedAlphas
  ]

marginPowerCells :: [Cell]
marginPowerCells = beyond ++ contrast
  where
    -- shifts beyond the resolved margin (~10) must still be caught.
    beyond =
      [ Cell (withMarginName 1.0e-3
               (addShift (scNarrowJitter 0x3A6C05) s))
             (marginCfg 1.0e-3 budget 1e-3) Nothing
      | s <- [20, 50], budget <- powerBudgets
      ]
    -- the sub-margin shift from the calibration cells, sharp mode.
    contrast =
      [ Cell (addShift (scNarrowJitter 0x3A6C06) 2)
             (baseCfg 10000 1 1e-3) Nothing
      ]

powerCells :: [Cell]
powerCells = shiftedCells ++ asymCells ++ varianceCells
  where
    shiftedCells =
      [ Cell (addShift base shift) (baseCfg budget 1 1e-3) Nothing
      | base   <- powerBases
      , shift  <- powerShifts
      , budget <- powerBudgets
      ]
    -- The rare-outlier asymmetric H_1 encodes its leak in the tail
    -- (spike rate), not in a scalar shift, so it doesn't ride
    -- 'addShift'. Just two budget cells.
    asymCells =
      [ Cell (scRareOutlierAsym 0xB055ED04) (baseCfg budget 1 1e-3)
             Nothing
      | budget <- powerBudgets
      ]
    -- Likewise the variance-only H_1: its leak is a dispersion
    -- difference at equal means, reachable only through the
    -- CDF-indicator channel.
    varianceCells =
      [ Cell (scAsymUniform 0xB055ED05) (baseCfg budget 1 1e-3)
             Nothing
      | budget <- powerBudgets
      ]

-- runner ---------------------------------------------------------------------

runTrial :: Scenario -> C.Config -> IO C.Result
runTrial !sc !cfg = do
  (a, b)        <- scInit sc cfg
  (meter, hyp)  <- mkSyntheticMeter a b
  C.runCT meter cfg hyp

countRejects :: Scenario -> C.Config -> Int -> IO Int
countRejects !sc !cfg !trials = go trials 0
  where
    go !0 !acc = pure acc
    go !n !acc = do
      r <- runTrial sc cfg
      let !inc = case r of
            C.Reject{} -> 1
            C.Pass{}   -> 0
      go (n - 1) (acc + inc)

data CellResult = CellResult
  { crCell    :: !Cell
  , crTrials  :: !Int
  , crRejects :: !Int
  , crCI      :: !(Double, Double)
  , crVerdict :: !(Maybe Verdict)
    -- ^ Nothing for power (characterisation) cells.
  }

runCell :: Int -> Double -> Cell -> IO CellResult
runCell !trials !familyConf cell@Cell{..} = do
  rejects <- countRejects cellScenario cellCfg trials
  let !z   = zCritical familyConf
      !ci  = wilsonCI rejects trials z
      !ver = fmap (`verdictFor` ci) cellGate
  pure CellResult
    { crCell = cell, crTrials = trials, crRejects = rejects
    , crCI = ci, crVerdict = ver
    }

-- reporter -------------------------------------------------------------------

showSci :: Double -> String
showSci x
  | x >= 0.01 = printf "%g" x
  | otherwise = printf "%.0e" x

rate :: Int -> Int -> Double
rate !k !n
  | n <= 0    = 0
  | otherwise = fromIntegral k / fromIntegral n

reportCell :: CellResult -> String
reportCell CellResult{..} =
  let Cell{..} = crCell
      !alpha   = C.cfgAlpha cellCfg
      !budget  = C.cfgBudget cellCfg
      !batch   = C.cfgBatch cellCfg
      !(lo, hi) = crCI
      !verCol  = maybe "-" renderVerdict crVerdict
  in  printf "%-36s %-9s %7d %5d %5d %9.5f  [%7.5f, %7.5f]  %-6s"
        (scName cellScenario) (showSci alpha) budget batch
        crRejects (rate crRejects crTrials) lo hi verCol

reportHeader :: String
reportHeader = printf "%-36s %-9s %7s %5s %5s %9s  %18s  %-6s"
  ("scenario" :: String) ("alpha" :: String) ("budget" :: String)
  ("batch" :: String) ("rej" :: String) ("rate" :: String)
  ("CI" :: String) ("verdict" :: String)

reportSection :: String -> Int -> Double -> [CellResult] -> IO ()
reportSection title trials cellConf results = do
  putStrLn ""
  putStrLn $ title ++ " (N=" ++ show trials
          ++ ", per-cell conf=" ++ printf "%.4f%%" (cellConf * 100)
          ++ ")"
  putStrLn $ replicate (length reportHeader) '='
  putStrLn reportHeader
  putStrLn $ replicate (length reportHeader) '-'
  mapM_ (putStrLn . reportCell) results

countVerdicts :: [CellResult] -> (Int, Int, Int)
countVerdicts = foldr step (0, 0, 0)
  where
    step CellResult{crVerdict = Just VPass} (!p, !f, !i) = (p+1, f, i)
    step CellResult{crVerdict = Just VFail} (!p, !f, !i) = (p, f+1, i)
    step CellResult{crVerdict = Just VInconclusive}
                                             (!p, !f, !i) = (p, f, i+1)
    step CellResult{crVerdict = Nothing}     acc          = acc

reportSummary :: [CellResult] -> IO ()
reportSummary results = do
  let (!p, !f, !i) = countVerdicts results
  putStrLn ""
  printf "calibration summary: %d PASS / %d FAIL / %d INCONCLUSIVE\n"
         p f i
  unless (f == 0) $
    putStrLn "FAIL: empirical rejection rate exceeded alpha; investigate."

-- CSV output -----------------------------------------------------------------

csvHeader :: String
csvHeader = intercalate ","
  [ "scenario", "alpha", "budget", "batch", "trials"
  , "rejects", "rate", "ci_lo", "ci_hi", "verdict"
  ]

csvRow :: CellResult -> String
csvRow CellResult{..} = intercalate ","
  [ scName (cellScenario crCell)
  , printf "%g" (C.cfgAlpha (cellCfg crCell))
  , show (C.cfgBudget (cellCfg crCell))
  , show (C.cfgBatch  (cellCfg crCell))
  , show crTrials
  , show crRejects
  , printf "%g" (rate crRejects crTrials)
  , printf "%g" (fst crCI)
  , printf "%g" (snd crCI)
  , maybe "-" renderVerdict crVerdict
  ]

writeCsv :: FilePath -> [CellResult] -> IO ()
writeCsv !path results = do
  let body = csvHeader : map csvRow results
  writeFile path (unlines body)
  putStrLn $ "csv written to " ++ path

-- main -----------------------------------------------------------------------

data Opts = Opts
  { optTrials :: !Int
  , optCsv    :: !(Maybe FilePath)
  }

defaultOpts :: Opts
defaultOpts = Opts { optTrials = 10000, optCsv = Nothing }

parseArgs :: [String] -> Opts
parseArgs = go defaultOpts
  where
    go !o []                 = o
    go !o ("--csv":p:rest)   = go (o { optCsv = Just p }) rest
    go !o (s:rest)
      | Just n <- readMaybe s = go (o { optTrials = n }) rest
      | otherwise             = go o rest

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  args <- Env.getArgs
  if "--power-gain" `elem` args
    then powerGainMain (filter (/= "--power-gain") args)
    else sweepMain args

sweepMain :: [String] -> IO ()
sweepMain args = do
  let Opts{..} = parseArgs args
      cells    = calibrationCells ++ edgeCaseCells
                   ++ marginCalibrationCells
                   ++ powerCells ++ marginPowerCells
      gatedCount = length [ c | c <- calibrationCells ++ edgeCaseCells
                                       ++ marginCalibrationCells
                              , isJust (cellGate c) ]
      familyConf = 1 - 0.05 / fromIntegral gatedCount
        -- Bonferroni over the gated cells. Ungated calibration and
        -- power cells don't contribute to family-wise FAIL risk, so
        -- they don't pay into the correction.

  putStrLn "censor-validate: empirical calibration sweep"
  printf "  trials per cell : %d\n" optTrials
  printf "  total cells     : %d\n" (length cells)
  printf "  gated cells     : %d\n" gatedCount
  printf "  per-cell conf   : %.4f%% (Bonferroni)\n" (familyConf * 100)
  putStrLn ""

  results <- forM cells $ \c -> do
    let !ungated = case cellGate c of { Nothing -> True; _ -> False }
        !conf    = if ungated then 0.95 else familyConf
    printf "  ... %s\n" (scName (cellScenario c))
    runCell optTrials conf c

  let (calibR, rest1) = splitAt (length calibrationCells)       results
      (edgeR,  rest2) = splitAt (length edgeCaseCells)          rest1
      (mcalR,  rest3) = splitAt (length marginCalibrationCells) rest2
      (powerR, mpowR) = splitAt (length powerCells)             rest3

  reportSection "calibration"        optTrials familyConf calibR
  reportSection "edge cases"         optTrials familyConf edgeR
  reportSection "margin calibration" optTrials familyConf mcalR
  reportSection "power"              optTrials 0.95       powerR
  reportSection "margin power"       optTrials 0.95       mpowR
  reportSummary (calibR ++ edgeR ++ mcalR)

  forM_ optCsv $ \p -> writeCsv p results

  let (_, fails, _) = countVerdicts (calibR ++ edgeR ++ mcalR)
  if fails > 0
    then Exit.exitWith (Exit.ExitFailure 1)
    else Exit.exitSuccess

-- host power-gain probe ------------------------------------------------------

-- A maximally power-asymmetric, cycle-identical positive control,
-- measured on the real host rather than a synthetic meter. Both
-- classes run the identical instruction stream -- two full rewrites
-- of a 64 KiB buffer per call -- and differ only in the data
-- written: class A writes zeros twice, so after the first call no
-- stored bit ever changes; class B alternates the all-01 and all-10
-- patterns, so every bit of the buffer and the store datapath
-- toggles on each rewrite. Any class difference a timing meter
-- resolves is therefore carried by a power-mediated channel
-- (frequency scaling being the usual transducer), not by the
-- executed code. The probe measures the host's power-to-time
-- coupling gain at maximal switching activity -- an upper bound on
-- what any real fix-vs-random data asymmetry can induce through
-- that channel, and hence a principled floor for cfgMargin on this
-- host. Only the deterministic counters -- instructions, branches
-- -- are guaranteed to read clean, and their doing so is what
-- certifies the two classes really do execute the same code. A
-- cycle-meter rejection is a result rather than a malfunction: an
-- identical instruction stream can still cost different cycles
-- when the data changes memory-system behaviour, so cycles
-- rejecting while instructions stay clean localises the coupling
-- to microarchitectural latency instead of frequency.
--
-- The A/A control (zeros vs zeros) runs first: it shares the
-- harness shape but not the content asymmetry, so a rejection
-- there indicts ambient instability rather than the power channel,
-- and the gain estimate is flagged as confounded.

powerGainWords :: Int
powerGainWords = 8192   -- 64 KiB of Word64

fillBuf :: Ptr Word64 -> Word64 -> IO ()
fillBuf !p !w = go 0
  where
    go !i
      | i >= powerGainWords = pure ()
      | otherwise           = pokeElemOff p i w >> go (i + 1)

powerGainHyp :: IO (C.Hypothesis (Word64, Word64))
powerGainHyp = do
  fp <- mallocForeignPtrBytes (powerGainWords * 8)
  pure C.Hypothesis
    { C.target  = \(!w1, !w2) -> withForeignPtr fp $ \p -> do
        fillBuf p w1
        fillBuf p w2
    , C.prepare = pure ()
    , C.sampleA = pure (0x0000000000000000, 0x0000000000000000)
    , C.sampleB = pure (0x5555555555555555, 0xAAAAAAAAAAAAAAAA)
    }

data GainOpts = GainOpts
  { goMeter  :: !(Maybe String)
  , goBudget :: !Int
  , goBatch  :: !Int
  }

parseGainArgs :: [String] -> GainOpts
parseGainArgs =
  go GainOpts { goMeter = Nothing, goBudget = 100000, goBatch = 1 }
  where
    go !o [] = o
    go !o ("--budget":v:rest)
      | Just n <- readMaybe v = go o { goBudget = n } rest
    go !o ("--batch":v:rest)
      | Just n <- readMaybe v = go o { goBatch = n } rest
    go !o (s:rest)
      | take 2 s /= "--" = go o { goMeter = Just s } rest
      | otherwise        = go o rest

powerGainMain :: [String] -> IO ()
powerGainMain args = do
  let GainOpts{..} = parseGainArgs args
      -- alpha 1e-3, not the driver default 1e-6: the probe is a
      -- measurement instrument, not a gate, and resEffect is a
      -- 1 - alpha confidence sequence -- the looser alpha buys a
      -- usefully tighter coupling bound at a confidence that is
      -- ample for host calibration.
      cfg = C.defaultConfig
        { C.cfgAlpha  = 1.0e-3
        , C.cfgBudget = goBudget
        , C.cfgWarmup = 500
        , C.cfgBatch  = goBatch
        }
  R.withMeterArg goMeter $ \mname meter -> do
    putStrLn "censor-validate: host power-coupling probe"
    printf "  meter %s . buffer %d KiB . batch %d . budget %d . alpha %.1g\n"
      mname (powerGainWords * 8 `div` 1024) goBatch goBudget
      (C.cfgAlpha cfg)
    putStrLn ""
    hypC <- powerGainHyp
    ctrl <- C.runAA meter cfg hypC
    printf "  control (zeros vs zeros)      : %s\n" (R.summary ctrl)
    hypP <- powerGainHyp
    base <- C.baselineReading meter cfg hypP
    probe <- C.runCT meter cfg hypP
    printf "  probe   (zeros vs max-toggle) : %s\n" (R.summary probe)
    putStrLn ""
    let !med        = fromIntegral (C.nsMedian base) :: Double
        (!elo, !ehi) = C.resEffect probe
        !mag        = max (abs elo) (abs ehi)
    printf "  baseline: med=%d iqr=%d (meter units per batch)\n"
      (C.nsMedian base) (C.nsIqr base)
    if med <= 0
      then putStrLn
        "  baseline median is zero; raise --batch until it resolves"
      else do
      -- the effect interval covers the true coupling with
      -- probability 1 - alpha, so its largest endpoint magnitude is
      -- a valid upper bound on |coupling| at that level.
        printf
          "  coupling bound: |effect| <= %.1f units/batch (%.2g of the reading)\n"
          mag (mag / med)
        let !rel4 = 4 * mag / med
        if mname /= "wall"
          then putStrLn $
               "  no margin suggested: the counters are meant to support\n"
            ++ "  the sharp null, where a tolerance only gives away\n"
            ++ "  power. Read the bound as this meter's residual\n"
            ++ "  class-difference sensitivity instead."
          else if rel4 < 0.05
            then printf
              "  suggested wall margin, 4x headroom: --margin %.1g\n" rel4
            else putStrLn $
                 "  bound too loose to derive a useful margin. The effect\n"
              ++ "  interval bottoms out near a tenth of the clip bound,\n"
              ++ "  which wall pair-noise sets, and more --budget barely\n"
              ++ "  moves it -- the lever is a quieter host. A bigger\n"
              ++ "  --batch does not tighten the bound either, but it is\n"
              ++ "  what resolves the channel at all on a noisy or\n"
              ++ "  virtualised host (see the README's batching note)."
    case probe of
      C.Reject{} -> do
        putStrLn ""
        putStrLn $ "  the meter resolved the power channel: this host converts\n"
          ++ "  data switching activity into measurable timing differences."
        unless (C.resPairs probe >= goBudget) $ putStrLn $
             "  (halted early at " ++ show (C.resPairs probe)
          ++ " pairs; detection outpaces estimation, so the bound\n"
          ++ "  above is loose -- repeat runs or a larger budget sharpen it.)"
      C.Pass{} -> do
        putStrLn ""
        putStrLn $ "  no coupling resolved at this budget; the bound above is\n"
          ++ "  what the run can exclude."
    case ctrl of
      C.Reject{} -> do
        putStrLn ""
        putStrLn $ "  WARNING: the A/A control rejected. The harness or host is\n"
          ++ "  unstable and the gain estimate above is confounded; quiet the\n"
          ++ "  host and re-run."
      C.Pass{} -> pure ()
