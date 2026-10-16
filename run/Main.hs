{-# LANGUAGE BangPatterns #-}

-- censor: dlopen-based sequential CT runner. See 'usage' for the
-- command line.
--
-- Every sample starts from a workspace of fresh pseudorandom bytes
-- (independent per-class streams), then overlays the declared ranges:
--
--   * --secret  ranges are refilled each sample in BOTH classes
--               through one reseed-and-fill path: class A from a
--               seed pinned at setup, class B from a fresh seed. This
--               is the fix-vs-random axis; no long-lived secret
--               buffer exists for one class alone to read (see
--               issues/handled/ISSUE4.md).
--   * --public  ranges are copied in BOTH classes from one buffer
--               drawn at setup, pinned for the whole run.
--   * --context ranges are copied in BOTH classes from a buffer
--               redrawn once per pair: one fresh public input shared
--               by the pair.
--
-- --seed pins the input samplers only; the A/B order bit is seeded
-- separately by --order-seed (default: fresh OS entropy, reported for
-- replay), since the conditional null needs the two independent.

module Main where

import Control.Applicative ((<|>))
import Control.Exception (catch)
import Control.Monad (forM_, when)
import Data.Bits (xor)
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr
  (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import qualified System.Environment as Env
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import qualified Censor as C
import qualified Censor.FFI as F
import qualified Censor.Runner as R
import qualified Censor.Runner.DL as DL
import qualified Censor.Runner.Manifest as M
import qualified Censor.Runner.Report as Rep
import Censor.Runner.Manifest (Range(..))

-- args ------------------------------------------------------------------------

-- Command-line flags as given. 'parseArgs' validates them and
-- resolves the run mode.
data Args = Args
  { argLib        :: !(Maybe FilePath)
  , argTarget     :: !(Maybe String)
  , argWorkspace  :: !(Maybe Int)
  , argSecret     :: ![Range]
  , argPublic     :: ![Range]
  , argContext    :: ![Range]
  , argMeter      :: !(Maybe String)
  , argAlpha      :: !(Maybe Double)
  , argBudget     :: !(Maybe Int)
  , argWarmup     :: !(Maybe Int)
  , argBatch      :: !(Maybe Int)
  , argMargin     :: !(Maybe Double)
  , argSeed       :: !(Maybe Word64)
  , argOrderSeed  :: !(Maybe Word64)
  , argFormat     :: !(Maybe String)
  , argTrace      :: !(Maybe FilePath)
  , argAttribute  :: !Bool
  , argManifest   :: !(Maybe FilePath)
  }

emptyArgs :: Args
emptyArgs = Args
  { argLib       = Nothing
  , argTarget    = Nothing
  , argWorkspace = Nothing
  , argSecret    = []
  , argPublic    = []
  , argContext   = []
  , argMeter     = Nothing
  , argAlpha     = Nothing
  , argBudget    = Nothing
  , argWarmup    = Nothing
  , argBatch     = Nothing
  , argMargin    = Nothing
  , argSeed      = Nothing
  , argOrderSeed = Nothing
  , argFormat    = Nothing
  , argTrace     = Nothing
  , argAttribute = False
  , argManifest  = Nothing
  }

-- A single target with its workspace size, or a manifest battery.
data Mode
  = Single !Int
  | Battery !FilePath

-- exit codes ------------------------------------------------------------------

exitReject, exitUsage, exitLoad :: Int
exitReject = 1
exitUsage  = 2
exitLoad   = 3

usage :: String
usage = unlines
  [ "usage: censor <libpath> --workspace BYTES [--target NAME]"
  , "              [--secret off:len]... [--public off:len]..."
  , "              [--context off:len]..."
  , "              [--meter wall|instructions|cycles|branches"
  , "                      |ref-cycles|task-clock|branch-misses"
  , "                      |cache-misses]"
  , "              [--alpha FLOAT] [--budget INT] [--warmup INT]"
  , "              [--batch INT] [--margin FLOAT]"
  , "              [--seed HEX] [--order-seed HEX]"
  , "              [--format pretty|json|plain]"
  , "              [--trace PATH] [--attribute]"
  , "   or: censor <libpath> --manifest PATH [--format FMT]"
  , "              [--attribute] [run-wide overrides]"
  , ""
  , "--manifest runs a whole battery from one file and emits a"
  , "single report; per-case layout flags are then rejected."
  , ""
  , "--secret pins class A only (fix vs. random axis)."
  , "--public pins BOTH classes for the whole run."
  , "--context refreshes BOTH classes together, once per pair."
  , ""
  , "--margin M tests the interval null |mean effect| <= delta,"
  , "with delta = M x the warmup median reading (see cfgMargin)."
  , ""
  , "--seed pins the input samplers; --order-seed pins the A/B"
  , "order bit (default: fresh OS entropy, reported on the run)."
  , ""
  , "Exit codes: 0 Pass, 1 Reject, 2 usage error, 3 load/symbol error."
  ]

die2 :: String -> IO a
die2 msg = do
  hPutStrLn stderr msg
  hPutStrLn stderr ""
  hPutStrLn stderr usage
  exitWith (ExitFailure exitUsage)

-- parsing ---------------------------------------------------------------------

parseArgs :: [String] -> IO (Args, FilePath, Mode)
parseArgs xs = go emptyArgs xs >>= finish

go :: Args -> [String] -> IO Args
go a []            = pure a
go _ ("-h":_)      = printHelp
go _ ("--help":_)  = printHelp
go a (x:xs)        = case x of
  "--target"     -> withVal x xs $ \v ys ->
    once x (argTarget a) $ go a { argTarget = Just v } ys
  "--workspace"  -> withVal x xs $ \v ys -> do
    n <- parseInt x v
    once x (argWorkspace a) $ go a { argWorkspace = Just n } ys
  "--secret"     -> withVal x xs $ \v ys -> do
    r <- parseRange x v
    go a { argSecret = r : argSecret a } ys
  "--public"     -> withVal x xs $ \v ys -> do
    r <- parseRange x v
    go a { argPublic = r : argPublic a } ys
  "--context"    -> withVal x xs $ \v ys -> do
    r <- parseRange x v
    go a { argContext = r : argContext a } ys
  "--meter"      -> withVal x xs $ \v ys ->
    once x (argMeter a) $ go a { argMeter = Just v } ys
  "--alpha"      -> withVal x xs $ \v ys -> do
    d <- parseDouble x v
    go a { argAlpha = Just d } ys
  "--budget"     -> withVal x xs $ \v ys -> do
    n <- parseInt x v
    go a { argBudget = Just n } ys
  "--warmup"     -> withVal x xs $ \v ys -> do
    n <- parseInt x v
    go a { argWarmup = Just n } ys
  "--batch"      -> withVal x xs $ \v ys -> do
    n <- parseInt x v
    go a { argBatch = Just n } ys
  "--margin"     -> withVal x xs $ \v ys -> do
    d <- parseDouble x v
    go a { argMargin = Just d } ys
  "--seed"       -> withVal x xs $ \v ys -> do
    w <- parseHex x v
    go a { argSeed = Just w } ys
  "--order-seed" -> withVal x xs $ \v ys -> do
    w <- parseHex x v
    go a { argOrderSeed = Just w } ys
  "--format"     -> withVal x xs $ \v ys ->
    once x (argFormat a) $ go a { argFormat = Just v } ys
  "--trace"      -> withVal x xs $ \v ys ->
    once x (argTrace a) $ go a { argTrace = Just v } ys
  "--attribute"  -> go a { argAttribute = True } xs
  "--manifest"   -> withVal x xs $ \v ys ->
    once x (argManifest a) $ go a { argManifest = Just v } ys
  _
    | take 2 x == "--" -> die2 ("censor: unknown flag " ++ x)
    | otherwise -> case argLib a of
        Nothing -> go a { argLib = Just x } xs
        Just _  -> die2 ("censor: unexpected positional argument " ++ x)

printHelp :: IO a
printHelp = do
  putStr usage
  exitWith ExitSuccess

withVal :: String -> [String] -> (String -> [String] -> IO a) -> IO a
withVal name []     _ = die2 ("censor: " ++ name ++ " expects a value")
withVal _    (v:xs) k = k v xs

once :: String -> Maybe b -> IO a -> IO a
once name (Just _) _ = die2 ("censor: " ++ name ++ " given twice")
once _    Nothing  k = k

parseInt :: String -> String -> IO Int
parseInt name v = case reads v of
  [(n, "")] | n >= 0 -> pure n
  _ -> die2 ("censor: " ++ name ++ " expects a non-negative integer,"
             ++ " got " ++ show v)

parseDouble :: String -> String -> IO Double
parseDouble name v = case reads v of
  [(d, "")] -> pure d
  _ -> die2 ("censor: " ++ name ++ " expects a floating-point value,"
             ++ " got " ++ show v)

parseHex :: String -> String -> IO Word64
parseHex name v = case M.readSeed v of
  Just w  -> pure w
  Nothing -> die2 ("censor: " ++ name ++ " expects a hex Word64,"
                   ++ " got " ++ show v)

parseRange :: String -> String -> IO Range
parseRange name v = case M.readRange v of
  Just r  -> pure r
  Nothing -> die2 ("censor: " ++ name ++ " expects off:len with"
                   ++ " off >= 0, len > 0, got " ++ show v)

finish :: Args -> IO (Args, FilePath, Mode)
finish a = do
  lib <- case argLib a of
    Just p  -> pure p
    Nothing -> die2 "censor: missing <libpath>"
  -- under --manifest the workspace layout is per case, so the
  -- flags that describe a single case are rejected rather than
  -- silently ignored.
  mode <- case argManifest a of
    Just p -> do
      forM_ perCaseFlags $ \(name, given) ->
        when given $ die2 ("censor: " ++ name ++ " is per-case under"
                           ++ " --manifest; declare it on the case")
      pure (Battery p)
    Nothing -> case argWorkspace a of
      Just n | n > 0 -> pure (Single n)
      Just _         -> die2 "censor: --workspace must be positive"
      Nothing        -> die2 "censor: missing --workspace BYTES"
  let ws = case mode of
        Single n  -> n
        Battery _ -> 0
  -- the same validator a manifest case goes through, so the two
  -- entry points cannot accept different layouts.
  case M.checkLayout ws (argSecret a) (argPublic a) (argContext a) of
    Just msg -> die2 ("censor: " ++ msg)
    Nothing  -> pure ()
  let a' = a { argSecret  = reverse (argSecret a)
             , argPublic  = reverse (argPublic a)
             , argContext = reverse (argContext a) }
  pure (a', lib, mode)
  where
    perCaseFlags =
      [ ("--workspace", isJust (argWorkspace a))
      , ("--target",    isJust (argTarget a))
      , ("--secret",    not (null (argSecret a)))
      , ("--public",    not (null (argPublic a)))
      , ("--context",   not (null (argContext a)))
      , ("--trace",     isJust (argTrace a))
      ]

-- hypothesis construction -----------------------------------------------------

-- fallback seed when the user does not pass --seed. Not
-- cryptographically meaningful; just a stable default so runs are
-- reproducible without a flag.
defaultSeed :: Word64
defaultSeed = 0xC0FFEE5EED0BEEF7

-- setup: derive the pinned secret seed and draw the fixed --public
-- overlay buffer once from a seeded RNG; keep independent
-- per-class RNGs for the random fill of everything else. Context
-- ranges live in a second buffer that the prologue redraws once
-- per pair, so both classes see the same fresh public bytes there.
-- Returns an FFIHypothesis ready to hand to 'withFFIHypothesis'.
--
-- Secret ranges are deliberately not overlaid from a long-lived
-- buffer: a buffer that only class A reads, every sample, is the
-- FFI form of the copy-source locality asymmetry described in
-- issues/handled/ISSUE4.md. Instead both classes refill their secret
-- ranges in place through one shared generator, reseeded before
-- every use: class A to the pinned seed, class B to a word drawn
-- from its own stream. The classes run identical code and differ
-- only in the seed value written into the generator state.
--
-- The two buffers are ForeignPtrs held by the sampler closures,
-- so they live exactly as long as the hypothesis does and are
-- reclaimed with it. Each phase of a case builds its own hypothesis;
-- raw mallocs would strand a workspace apiece for the length of a
-- battery.
buildHypothesis
  :: Int                 -- ^ workspace bytes
  -> [Range]             -- ^ secret ranges (class A pinned)
  -> [Range]             -- ^ public ranges (both classes, pinned)
  -> [Range]             -- ^ context ranges (both classes, per pair)
  -> Word64              -- ^ sampler seed
  -> (Ptr Word8 -> IO ())
  -> IO F.FFIHypothesis
buildHypothesis ws secrets publics contexts seed target = do
  fixPub <- mallocForeignPtrBytes ws
  ctxBuf <- mallocForeignPtrBytes ws
  gFix <- C.mkRng seed
  !s0 <- C.nextWord gFix
  withForeignPtr fixPub $ \p -> F.fillRandom gFix p ws
  ga  <- C.mkRng (seed `xor` 0x0A0A0A0A0A0A0A0A)
  gb  <- C.mkRng (seed `xor` 0x0B0B0B0B0B0B0B0B)
  gCtx <- C.mkRng (seed `xor` 0x0C0C0C0C0C0C0C0C)
  rSec <- C.mkRng s0
  let refresh
        | null contexts = pure ()
        | otherwise = withForeignPtr ctxBuf $ \p ->
            mapM_ (\(Range o l) ->
              F.fillRandom gCtx (p `plusPtr` o) l) contexts
      -- both classes draw one word from their own stream and
      -- reseed the shared secret generator -- class A pins the
      -- seed ('const' s0) and discards its draw, class B seeds
      -- with its draw ('id') -- then fill the secret ranges in
      -- place. Short-circuits on no secret ranges, like 'overlay'.
      fillSecrets !g seedOf !p
        | null secrets = pure ()
        | otherwise = do
            !w <- C.nextWord g
            C.reseed rSec (seedOf w)
            mapM_ (\(Range o l) ->
              F.fillRandom rSec (p `plusPtr` o) l) secrets
  refresh
  pure F.FFIHypothesis
    { F.ffiWorkspaceBytes = ws
    , F.ffiPrepare = refresh
      -- role ranges are validated pairwise disjoint, so the write
      -- order below is immaterial: no two of them touch the same
      -- byte.
    , F.ffiSampleA = \p -> do
        F.fillRandom ga p ws
        overlay p ctxBuf contexts
        overlay p fixPub publics
        fillSecrets ga (const s0) p
    , F.ffiSampleB = \p -> do
        F.fillRandom gb p ws
        overlay p ctxBuf contexts
        overlay p fixPub publics
        fillSecrets gb id p
    , F.ffiTarget = target
    }

-- Copy the declared ranges out of a backing buffer into the
-- workspace. Short-circuits on an empty range list, so a role a
-- hypothesis does not use costs nothing per sample.
overlay :: Ptr Word8 -> ForeignPtr Word8 -> [Range] -> IO ()
overlay _   _   [] = pure ()
overlay dst src rs = withForeignPtr src $ \p ->
  mapM_ (\(Range o l) ->
    copyBytes (dst `plusPtr` o) (p `plusPtr` o) l) rs

-- driver ----------------------------------------------------------------------

main :: IO ()
main = do
  (args, lib, mode) <- Env.getArgs >>= parseArgs
  let run = case mode of
        Single ws -> runSingle args lib ws
        Battery p -> runManifest args lib p
  run `catch` \e -> case e of
    DL.DLOpenFailed p m ->
      dieLoad ("censor: dlopen " ++ p ++ " failed: " ++ m)
    DL.DLSymFailed s m ->
      dieLoad ("censor: dlsym " ++ s ++ " failed: " ++ m)

dieLoad :: String -> IO a
dieLoad msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure exitLoad)

-- exit 0 when every case passed, 1 when any rejected.
exitVerdicts :: [Rep.CaseReport] -> IO a
exitVerdicts cases
  | any (isReject . Rep.crResult) cases = exitWith (ExitFailure exitReject)
  | otherwise                           = exitWith ExitSuccess

isReject :: C.Result -> Bool
isReject C.Reject{} = True
isReject C.Pass{}   = False

-- The sampler seed. Distinct from the order-bit seed, which the
-- driver resolves from OS entropy unless --order-seed pins it.
samplerSeed :: Args -> Word64
samplerSeed args = fromMaybe defaultSeed (argSeed args)

-- CLI flag beats manifest directive beats built-in default.
pick :: Maybe a -> Maybe a -> a -> a
pick cli mf def = fromMaybe def (cli <|> mf)

runConfig :: Args -> M.Defaults -> C.Config
runConfig args d = C.defaultConfig
  { C.cfgAlpha  = pick (argAlpha  args) (M.dfAlpha  d)
                       (C.cfgAlpha  C.defaultConfig)
  , C.cfgBudget = pick (argBudget args) (M.dfBudget d)
                       (C.cfgBudget C.defaultConfig)
  , C.cfgWarmup = pick (argWarmup args) (M.dfWarmup d)
                       (C.cfgWarmup C.defaultConfig)
  , C.cfgBatch  = 1  -- resolved per case
  , C.cfgMargin = argMargin args <|> M.dfMargin d
  , C.cfgSeed   = argOrderSeed args <|> M.dfOrderSeed d
    -- deliberately not argSeed: the order bit must be independent of
    -- the sampler stream.
  }

-- One case: a baseline probe, the run (recording its frames under
-- --trace), and, on a rejection under --attribute, the negative
-- controls. Each phase gets a fresh hypothesis instance, so the run
-- starts from the same sampler state whether or not a probe ran.
runCase
  :: Args -> String -> C.Meter -> C.Config -> String
  -> IO F.FFIHypothesis -> IO Rep.CaseReport
runCase args mname meter cfg name mk = do
  base <- withHyp $ \h -> C.baselineReading meter cfg h
  (result, mfrs) <- withHyp $ \h -> case argTrace args of
    Nothing -> do
      r <- C.runCT meter cfg h
      pure (r, Nothing)
    Just _ -> do
      t <- R.record name mname meter cfg h
      pure (R.traceResult t, Just (R.traceFrames t))
  mat <- case (argAttribute args, result) of
    (True, C.Reject{}) -> withHyp $ \h -> Just <$> C.attribute meter cfg h
    _                  -> pure Nothing
  pure Rep.CaseReport
    { Rep.crName        = name
    , Rep.crResult      = result
    , Rep.crAttribution = mat
    , Rep.crBatch       = Nothing
    , Rep.crNoise       = Nothing
    , Rep.crBaseline    = Just base
    , Rep.crTrace       = mfrs
    , Rep.crChannels    = []
    }
  where
    withHyp k = mk >>= \fh -> F.withFFIHypothesis fh k

-- single target ---------------------------------------------------------------

runSingle :: Args -> FilePath -> Int -> IO ()
runSingle args lib ws = DL.withLibrary lib $ \lib' -> do
  let name = fromMaybe "censor_target" (argTarget args)
      seed = samplerSeed args
      cfg  = (runConfig args M.emptyDefaults)
        { C.cfgBatch = fromMaybe 1 (argBatch args) }
  target <- DL.resolveTarget lib' name
  rnd    <- Rep.resolveFormat (argFormat args)
  rc     <- Rep.initReportCfg
  R.withMeterArg (argMeter args) $ \mname meter -> do
    env <- Rep.captureEnv
    let hdr = Rep.ReportHeader
          { Rep.rhMeter        = mname
          , Rep.rhConfig       = cfg
          , Rep.rhSeed         = Just seed
          , Rep.rhTarget       = Just name
          , Rep.rhNote         = Just ("library " ++ lib)
          , Rep.rhEnv          = Just env
          , Rep.rhPerCaseBatch = False
          }
    cases <- Rep.withReport rc rnd hdr $ \emit -> do
      cr <- runCase args mname meter cfg name $
        buildHypothesis ws (argSecret args) (argPublic args)
          (argContext args) seed target
      emit cr
      -- the trace artefact is the same censor/report-v3 object the
      -- json renderer emits, with the frame trajectory embedded.
      forM_ (argTrace args) $ \tp -> writeFile tp (Rep.reportJSON hdr [cr])
      pure (argTrace args)
    exitVerdicts cases

-- manifest runner ------------------------------------------------------------

-- A case's batch: --batch overrides everything, then the case's own
-- key, then the manifest default, then 1. Defaulting to 1 rather
-- than 'auto' keeps a silently-calibrated batch out of a report
-- nobody asked to calibrate.
caseBatch :: Args -> M.Defaults -> M.ManifestCase -> M.BatchSpec
caseBatch args d mc = case argBatch args of
  Just n  -> M.BatchFixed n
  Nothing -> fromMaybe (M.BatchFixed 1) (M.mcBatch mc <|> M.dfBatch d)

-- Expand a case into one entry per replicate, each with its own
-- sampler seed so it draws an independent fixed secret. A single
-- replicate keeps the bare case name; several get a suffix.
data Cell = Cell
  { cellName :: !String
  , cellCase :: !M.ManifestCase
  , cellSeed :: {-# UNPACK #-} !Word64
  }

expand :: Word64 -> M.ManifestCase -> [Cell]
expand base mc
  | n <= 1    = [Cell (M.mcName mc) mc s0]
  | otherwise =
      [ Cell (M.mcName mc ++ " #" ++ show i) mc (replicaSeed s0 i)
      | i <- [1 .. n]
      ]
  where
    n  = M.mcReplicates mc
    s0 = caseSeed base (M.mcId mc)

-- Mix the run-wide seed with the case identity, so each case draws
-- its own fixed secret. Sharing one seed across every case would
-- start them all from identical bytes, which makes a battery far
-- less independent than its case count suggests.
--
-- Deliberately 'mcId', not 'mcName': a label= override is a
-- presentation change, and must not silently re-roll the secret the
-- case tests against.
caseSeed :: Word64 -> String -> Word64
caseSeed base name = base `xor` fnv1a name

fnv1a :: String -> Word64
fnv1a = foldl' step 0xCBF29CE484222325
  where
    step !h ch = (h `xor` fromIntegral (fromEnum ch)) * 0x100000001B3

-- Replicate #1 *is* the base cell: raising 'replicates' must extend
-- a battery, never redefine the cell that was already in it.
-- Later indices decorrelate with the golden-ratio odd constant
-- rather than a small addend, so nearby indices do not produce
-- nearby generator states.
replicaSeed :: Word64 -> Int -> Word64
replicaSeed base 1 = base
replicaSeed base i = base `xor` (fromIntegral i * 0x9E3779B97F4A7C15)

runManifest :: Args -> FilePath -> FilePath -> IO ()
runManifest args lib path = do
  src <- readFile path
  mf  <- case M.parseManifest src of
    Left err -> do
      hPutStrLn stderr ("censor: " ++ err)
      exitWith (ExitFailure exitUsage)
    Right x  -> pure x
  let d     = M.mfDefaults mf
      seed  = pick (argSeed args) (M.dfSeed d) defaultSeed
      cells = concatMap (expand seed) (M.mfCases mf)
      cfg0  = runConfig args d
      -- 'family-alpha on' turns the reported union bound into an
      -- actual correction: each of the M cells runs at alpha / M, so
      -- "any cell rejects" is a test at alpha family-wise.
      cfg   = case M.dfFamilyAlpha d of
        Just True -> cfg0
          { C.cfgAlpha = C.cfgAlpha cfg0
              / fromIntegral (max 1 (length cells)) }
        _ -> cfg0
  rnd <- Rep.resolveFormat (argFormat args)
  rc  <- Rep.initReportCfg
  DL.withLibrary lib $ \lib' ->
    R.withMeterArg (argMeter args <|> M.dfMeter d) $ \mname meter -> do
      env <- Rep.captureEnv
      let hdr = Rep.ReportHeader
            { Rep.rhMeter        = mname
            , Rep.rhConfig       = cfg
            , Rep.rhSeed         = Just seed
            , Rep.rhTarget       = Just lib
            , Rep.rhNote         = Just ("manifest " ++ path)
            , Rep.rhEnv          = Just env
            , Rep.rhPerCaseBatch = True
            }
      cases <- Rep.withReport rc rnd hdr $ \emit -> do
        forM_ cells $ \cell ->
          runCell args d mname meter cfg lib' cell >>= emit
        pure Nothing
      exitVerdicts cases

runCell
  :: Args -> M.Defaults -> String -> C.Meter -> C.Config -> DL.Library
  -> Cell -> IO Rep.CaseReport
runCell args d mname meter cfg lib cell = do
  let mc = cellCase cell
  target <- DL.resolveTarget lib (M.mcTarget mc)
  let mk = buildHypothesis (M.mcWorkspace mc) (M.mcSecret mc)
             (M.mcPublic mc) (M.mcContext mc) (cellSeed cell) target
  (b, mnoise) <- case caseBatch args d mc of
    M.BatchFixed n -> pure (n, Nothing)
    M.BatchAuto    -> do
      hCal <- mk
      F.withFFIHypothesis hCal $ \h -> do
        cb <- C.calibrateBatchReport meter h
        pure (C.crBatch cb, C.crNoise cb)
  cr <- runCase args mname meter cfg { C.cfgBatch = b } (cellName cell) mk
  pure cr { Rep.crBatch = Just b, Rep.crNoise = mnoise }
