{-# LANGUAGE BangPatterns #-}

-- censor: dlopen-based sequential CT runner.
--
--   censor <libpath> --workspace BYTES [--target NAME]
--     [--secret off:len]... [--public off:len]...
--   censor <libpath> --manifest PATH
--     [--meter wall|instructions|cycles|branches]
--     [--alpha FLOAT] [--budget INT] [--warmup INT] [--batch INT]
--     [--seed HEX] [--order-seed HEX] [--format pretty|json|plain]
--     [--trace PATH] [--attribute]
--
-- --seed pins the input samplers only. The per-pair A/B order bit
-- is seeded separately by --order-seed, defaulting to fresh OS
-- entropy; the value it resolves to is reported, so a randomised
-- run stays exactly replayable. Keeping the two independent is
-- what the conditional null requires.
--
-- Workspace layout: every pair starts with a workspace of --workspace
-- bytes filled with fresh pseudorandom bytes (independent per-class
-- streams).
--
--   * --secret off:len  ranges are overwritten in class A only with
--                       bytes from a single fixed buffer drawn at
--                       setup. This is the fix-vs-random test axis.
--   * --public off:len  ranges are overwritten in BOTH classes with
--                       bytes from a shared fixed buffer drawn at
--                       setup (common public context: message, IV,
--                       modulus, ...).
--
-- Bytes outside any pinned range are random per pair, per class.
--
-- --manifest runs a whole battery from a declarative file: one
-- dlopen, one report carrying every case, its controls, the host
-- environment, and the family-wise alpha. The per-case layout flags
-- (--workspace/--target/--secret/--public/--trace) are then rejected
-- rather than silently ignored; see Censor.Runner.Manifest for the
-- format.

module Main where

import Control.Exception (catch)
import Control.Monad (forM_)
import Data.Bits (xor)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Maybe (isJust)
import Data.Word (Word8, Word64)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Numeric (readHex)
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

data Args = Args
  { argLib        :: !FilePath
  , argTarget     :: !String
  , argWorkspace  :: !Int
  , argSecret     :: ![Range]
  , argPublic     :: ![Range]
  , argMeter      :: !(Maybe String)
  , argAlpha      :: !(Maybe Double)
  , argBudget     :: !(Maybe Int)
  , argWarmup     :: !(Maybe Int)
  , argBatch      :: !(Maybe Int)
  , argSeed       :: !(Maybe Word64)
  , argOrderSeed  :: !(Maybe Word64)
  , argFormat     :: !(Maybe String)
  , argTrace      :: !(Maybe FilePath)
  , argAttribute  :: !Bool
  , argManifest   :: !(Maybe FilePath)
  } deriving Show

data ArgAcc = ArgAcc
  { accLib        :: !(Maybe FilePath)
  , accTarget     :: !(Maybe String)
  , accWorkspace  :: !(Maybe Int)
  , accSecret     :: ![Range]
  , accPublic     :: ![Range]
  , accMeter      :: !(Maybe String)
  , accAlpha      :: !(Maybe Double)
  , accBudget     :: !(Maybe Int)
  , accWarmup     :: !(Maybe Int)
  , accBatch      :: !(Maybe Int)
  , accSeed       :: !(Maybe Word64)
  , accOrderSeed  :: !(Maybe Word64)
  , accFormat     :: !(Maybe String)
  , accTrace      :: !(Maybe FilePath)
  , accAttribute  :: !Bool
  , accManifest   :: !(Maybe FilePath)
  }

emptyAcc :: ArgAcc
emptyAcc = ArgAcc Nothing Nothing Nothing [] [] Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing False Nothing

-- exit codes ------------------------------------------------------------------

exitPass, exitReject, exitUsage, exitLoad :: Int
exitPass   = 0
exitReject = 1
exitUsage  = 2
exitLoad   = 3

usage :: String
usage = unlines
  [ "usage: censor <libpath> --workspace BYTES [--target NAME]"
  , "              [--secret off:len]... [--public off:len]..."
  , "              [--meter wall|instructions|cycles|branches]"
  , "              [--alpha FLOAT] [--budget INT] [--warmup INT]"
  , "              [--batch INT] [--seed HEX] [--order-seed HEX]"
  , "              [--format pretty|json|plain]"
  , "              [--trace PATH] [--attribute]"
  , "   or: censor <libpath> --manifest PATH [--format FMT]"
  , "              [--attribute] [run-wide overrides]"
  , ""
  , "--manifest runs a whole battery from one file and emits a"
  , "single report; per-case layout flags are then rejected."
  , ""
  , "--secret pins class A only (fix vs. random axis)."
  , "--public pins BOTH classes (shared public context)."
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

parseArgs :: [String] -> IO Args
parseArgs xs = go emptyAcc xs >>= finish

go :: ArgAcc -> [String] -> IO ArgAcc
go acc []            = pure acc
go _   ("-h":_)      = printHelp
go _   ("--help":_)  = printHelp
go acc (x:xs)        = case x of
  "--target"     -> withVal x xs $ \v ys ->
    onceStr x (accTarget acc) $
      go acc { accTarget = Just v } ys
  "--workspace"  -> withVal x xs $ \v ys -> do
    n <- parseInt x v
    onceInt x (accWorkspace acc) $
      go acc { accWorkspace = Just n } ys
  "--secret"     -> withVal x xs $ \v ys -> do
    r <- parseRange x v
    go acc { accSecret = r : accSecret acc } ys
  "--public"     -> withVal x xs $ \v ys -> do
    r <- parseRange x v
    go acc { accPublic = r : accPublic acc } ys
  "--meter"      -> withVal x xs $ \v ys ->
    onceStr x (accMeter acc) $
      go acc { accMeter = Just v } ys
  "--alpha"      -> withVal x xs $ \v ys -> do
    d <- parseDouble x v
    go acc { accAlpha = Just d } ys
  "--budget"     -> withVal x xs $ \v ys -> do
    n <- parseInt x v
    go acc { accBudget = Just n } ys
  "--warmup"     -> withVal x xs $ \v ys -> do
    n <- parseInt x v
    go acc { accWarmup = Just n } ys
  "--batch"      -> withVal x xs $ \v ys -> do
    n <- parseInt x v
    go acc { accBatch = Just n } ys
  "--seed"       -> withVal x xs $ \v ys -> do
    w <- parseHex x v
    go acc { accSeed = Just w } ys
  "--order-seed" -> withVal x xs $ \v ys -> do
    w <- parseHex x v
    go acc { accOrderSeed = Just w } ys
  "--format"     -> withVal x xs $ \v ys ->
    onceStr x (accFormat acc) $
      go acc { accFormat = Just v } ys
  "--trace"      -> withVal x xs $ \v ys ->
    onceStr x (accTrace acc) $
      go acc { accTrace = Just v } ys
  "--attribute"  -> go acc { accAttribute = True } xs
  "--manifest"   -> withVal x xs $ \v ys ->
    onceStr x (accManifest acc) $
      go acc { accManifest = Just v } ys
  _
    | take 2 x == "--" -> die2 ("censor: unknown flag " ++ x)
    | otherwise -> case accLib acc of
        Nothing -> go acc { accLib = Just x } xs
        Just _  -> die2 ("censor: unexpected positional argument " ++ x)

printHelp :: IO a
printHelp = do
  putStr usage
  exitWith ExitSuccess

withVal :: String -> [String] -> (String -> [String] -> IO a) -> IO a
withVal name []     _ = die2 ("censor: " ++ name ++ " expects a value")
withVal _    (v:xs) k = k v xs

onceStr :: String -> Maybe String -> IO a -> IO a
onceStr name (Just _) _ = die2 ("censor: " ++ name ++ " given twice")
onceStr _    Nothing  k = k

onceInt :: String -> Maybe Int -> IO a -> IO a
onceInt name (Just _) _ = die2 ("censor: " ++ name ++ " given twice")
onceInt _    Nothing  k = k

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
parseHex name v =
  let s = case v of
        '0':'x':rest -> rest
        '0':'X':rest -> rest
        _            -> v
  in  case readHex s of
        [(w, "")] -> pure w
        _ -> die2 ("censor: " ++ name ++ " expects a hex Word64,"
                   ++ " got " ++ show v)

parseRange :: String -> String -> IO Range
parseRange name v = case break (== ':') v of
  (off, ':':len) -> case (reads off, reads len) of
    ([(o, "")], [(l, "")]) | o >= 0 && l > 0 -> pure (Range o l)
    _ -> bad
  _ -> bad
  where
    bad = die2 ("censor: " ++ name ++ " expects off:len with off >= 0,"
                ++ " len > 0, got " ++ show v)

finish :: ArgAcc -> IO Args
finish acc = do
  lib <- case accLib acc of
    Just p  -> pure p
    Nothing -> die2 "censor: missing <libpath>"
  -- under --manifest the workspace layout is per case, so the
  -- flags that describe a single case are rejected rather than
  -- silently ignored.
  ws <- case accManifest acc of
    Just _ -> do
      forM_ perCaseFlags $ \(name, given) ->
        if given
          then die2 ("censor: " ++ name ++ " is per-case under"
                     ++ " --manifest; declare it on the case")
          else pure ()
      pure 0
    Nothing -> case accWorkspace acc of
      Just n | n > 0 -> pure n
      Just _         -> die2 "censor: --workspace must be positive"
      Nothing        -> die2 "censor: missing --workspace BYTES"
  mapM_ (checkRange ws "--secret") (accSecret acc)
  mapM_ (checkRange ws "--public") (accPublic acc)
  pure Args
    { argLib       = lib
    , argTarget    = maybe "censor_target" id (accTarget acc)
    , argWorkspace = ws
    , argSecret    = reverse (accSecret acc)
    , argPublic    = reverse (accPublic acc)
    , argMeter     = accMeter acc
    , argAlpha     = accAlpha acc
    , argBudget    = accBudget acc
    , argWarmup    = accWarmup acc
    , argBatch     = accBatch acc
    , argSeed      = accSeed acc
    , argOrderSeed = accOrderSeed acc
    , argFormat    = accFormat acc
    , argTrace     = accTrace acc
    , argAttribute = accAttribute acc
    , argManifest  = accManifest acc
    }

  where
    perCaseFlags =
      [ ("--workspace", isJust (accWorkspace acc))
      , ("--target",    isJust (accTarget acc))
      , ("--secret",    not (null (accSecret acc)))
      , ("--public",    not (null (accPublic acc)))
      , ("--trace",     isJust (accTrace acc))
      ]

checkRange :: Int -> String -> Range -> IO ()
checkRange ws name (Range o l)
  | o + l > ws =
      die2 ("censor: " ++ name ++ " " ++ show o ++ ":" ++ show l
            ++ " overflows workspace of " ++ show ws ++ " bytes")
  | otherwise = pure ()

-- hypothesis construction -----------------------------------------------------

-- fallback seed when the user does not pass --seed. Not
-- cryptographically meaningful; just a stable default so runs are
-- reproducible without a flag.
defaultSeed :: Word64
defaultSeed = 0xC0FFEE5EED0BEEF7

-- setup: draw the fixed --secret and --public overlay buffers once
-- from a seeded RNG; keep independent per-class RNGs for the random
-- fill of everything else. Returns an FFIHypothesis ready to hand to
-- 'withFFIHypothesis'.
buildHypothesis
  :: Int                 -- ^ workspace bytes
  -> [Range]             -- ^ secret ranges (class A only)
  -> [Range]             -- ^ public ranges (both classes)
  -> Word64              -- ^ sampler seed
  -> (Ptr Word8 -> IO ())
  -> IO F.FFIHypothesis
buildHypothesis ws secrets publics seed target = do
  fixSec <- mallocBytes ws
  fixPub <- mallocBytes ws
  gFix <- C.mkRng seed
  F.fillRandom gFix fixSec ws
  F.fillRandom gFix fixPub ws
  ga <- C.mkRng (seed `xor` 0x0A0A0A0A0A0A0A0A)
  gb <- C.mkRng (seed `xor` 0x0B0B0B0B0B0B0B0B)
  pure F.FFIHypothesis
    { F.ffiWorkspaceBytes = ws
    , F.ffiPrepare = pure ()
    , F.ffiSampleA = \p -> do
        F.fillRandom ga p ws
        overlay p fixPub publics
        overlay p fixSec secrets
    , F.ffiSampleB = \p -> do
        F.fillRandom gb p ws
        overlay p fixPub publics
    , F.ffiTarget = target
    }

overlay :: Ptr Word8 -> Ptr Word8 -> [Range] -> IO ()
overlay dst src = mapM_ $ \(Range o l) ->
  copyBytes (dst `plusPtr` o) (src `plusPtr` o) l

-- driver ----------------------------------------------------------------------

buildConfig :: Args -> C.Config
buildConfig args = C.defaultConfig
  { C.cfgAlpha  = maybe (C.cfgAlpha  C.defaultConfig) id (argAlpha args)
  , C.cfgBudget = maybe (C.cfgBudget C.defaultConfig) id (argBudget args)
  , C.cfgWarmup = maybe (C.cfgWarmup C.defaultConfig) id (argWarmup args)
  , C.cfgBatch  = maybe (C.cfgBatch  C.defaultConfig) id (argBatch args)
  , C.cfgSeed   = argOrderSeed args
    -- deliberately NOT argSeed: the order bit must be independent
    -- of the sampler stream. Nothing here means fresh OS entropy,
    -- and the resolved value comes back as 'resSeed'.
  }

verdictExit :: C.Result -> Int
verdictExit C.Pass{}   = exitPass
verdictExit C.Reject{} = exitReject

-- main ------------------------------------------------------------------------

main :: IO ()
main = do
  args <- Env.getArgs >>= parseArgs
  let run = case argManifest args of
        Just p  -> runManifest args p
        Nothing -> runWith args
  run `catch` \e -> case e of
    DL.DLOpenFailed p m ->
      dieLoad ("censor: dlopen " ++ p ++ " failed: " ++ m)
    DL.DLSymFailed s m ->
      dieLoad ("censor: dlsym " ++ s ++ " failed: " ++ m)

dieLoad :: String -> IO a
dieLoad msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure exitLoad)

runWith :: Args -> IO ()
runWith args = DL.withLibrary (argLib args) $ \lib -> do
  target <- DL.resolveTarget lib (argTarget args)
  hyp    <- buildHypothesis (argWorkspace args) (argSecret args)
              (argPublic args) (samplerSeed args) target
  rnd    <- Rep.resolveFormat (argFormat args)
  rc     <- Rep.initReportCfg
  let cfg  = buildConfig args
      seed = samplerSeed args
  R.withMeterArg (argMeter args) $ \mname meter -> do
    env <- Rep.captureEnv
    let hdr = Rep.ReportHeader
          { Rep.rhMeter     = mname
          , Rep.rhConfig    = cfg
          , Rep.rhSeed      = Just seed
          , Rep.rhTarget    = Just (argTarget args)
          , Rep.rhNote      = Just ("library " ++ argLib args)
          , Rep.rhEnv          = Just env
          , Rep.rhPerCaseBatch = False
          }
    cases <- Rep.withReport rc rnd hdr $ \emit -> do
      (result, mfrs) <- F.withFFIHypothesis hyp $ \h ->
        case argTrace args of
          Nothing -> do
            r <- C.runCT meter cfg h
            pure (r, Nothing)
          Just _ -> do
            t <- R.record (argTarget args) mname meter cfg h
            pure (R.traceResult t, Just (R.traceFrames t))
      -- run the A/A and B/B diagnostics if the user asked and the
      -- primary run rejected; otherwise skip.
      mat <- case (argAttribute args, result) of
        (True, C.Reject{}) -> F.withFFIHypothesis hyp $ \h ->
          Just <$> C.attribute meter cfg h
        _                  -> pure Nothing
      let cr = Rep.CaseReport
            { Rep.crName        = argTarget args
            , Rep.crResult      = result
            , Rep.crAttribution = mat
            , Rep.crBatch       = Nothing
            , Rep.crNoise       = Nothing
            , Rep.crTrace       = mfrs
            }
      emit cr
      -- the trace artefact is the same censor/report-v2 object the
      -- json renderer emits, with the frame trajectory embedded.
      case argTrace args of
        Just tp -> writeFile tp (Rep.reportJSON hdr [cr])
        Nothing -> pure ()
      pure (argTrace args)
    let code = case cases of
          [cr] -> verdictExit (Rep.crResult cr)
          _    -> exitPass  -- unreachable: exactly one case emitted
    exitWith (if code == 0 then ExitSuccess else ExitFailure code)

-- manifest runner ------------------------------------------------------------

-- The sampler seed. Distinct from the order-bit seed, which the
-- driver resolves from OS entropy unless --order-seed pins it.
samplerSeed :: Args -> Word64
samplerSeed args = maybe defaultSeed id (argSeed args)

-- CLI flag beats manifest directive beats built-in default.
pick :: Maybe a -> Maybe a -> a -> a
pick cli mf def = case cli of
  Just x  -> x
  Nothing -> maybe def id mf

manifestConfig :: Args -> M.Defaults -> C.Config
manifestConfig args d = C.defaultConfig
  { C.cfgAlpha  = pick (argAlpha  args) (M.dfAlpha  d)
                       (C.cfgAlpha  C.defaultConfig)
  , C.cfgBudget = pick (argBudget args) (M.dfBudget d)
                       (C.cfgBudget C.defaultConfig)
  , C.cfgWarmup = pick (argWarmup args) (M.dfWarmup d)
                       (C.cfgWarmup C.defaultConfig)
  , C.cfgBatch  = 1  -- resolved per case
  , C.cfgSeed   = case argOrderSeed args of
      Just w  -> Just w
      Nothing -> M.dfOrderSeed d
  }

-- A case's batch: --batch overrides everything, then the case's own
-- key, then the manifest default, then 1. Defaulting to 1 rather
-- than 'auto' keeps a silently-calibrated batch out of a report
-- nobody asked to calibrate.
caseBatch :: Args -> M.Defaults -> M.ManifestCase -> M.BatchSpec
caseBatch args d mc = case argBatch args of
  Just n  -> M.BatchFixed n
  Nothing -> case M.mcBatch mc of
    Just b  -> b
    Nothing -> maybe (M.BatchFixed 1) id (M.dfBatch d)

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
  | n <= 1    = [Cell (M.mcName mc) mc base]
  | otherwise =
      [ Cell (M.mcName mc ++ " #" ++ show i) mc (replicaSeed base i)
      | i <- [1 .. n]
      ]
  where
    n = M.mcReplicates mc

-- Decorrelate replicate seeds with the golden-ratio odd constant
-- rather than a small addend, so nearby indices do not produce
-- nearby generator states.
replicaSeed :: Word64 -> Int -> Word64
replicaSeed base i = base `xor` (fromIntegral i * 0x9E3779B97F4A7C15)

runManifest :: Args -> FilePath -> IO ()
runManifest args path = do
  src <- readFile path
  mf  <- case M.parseManifest src of
    Left err -> do
      hPutStrLn stderr ("censor: " ++ err)
      exitWith (ExitFailure exitUsage)
    Right x  -> pure x
  let d     = M.mfDefaults mf
      seed  = pick (argSeed args) (M.dfSeed d) defaultSeed
      cells = concatMap (expand seed) (M.mfCases mf)
      cfg   = manifestConfig args d
      mname = case argMeter args of
        Just m  -> Just m
        Nothing -> M.dfMeter d
  rnd <- Rep.resolveFormat (argFormat args)
  rc  <- Rep.initReportCfg
  DL.withLibrary (argLib args) $ \lib ->
    R.withMeterArg mname $ \meterName meter -> do
      env <- Rep.captureEnv
      let hdr = Rep.ReportHeader
            { Rep.rhMeter        = meterName
            , Rep.rhConfig       = cfg
            , Rep.rhSeed         = Just seed
            , Rep.rhTarget       = Just (argLib args)
            , Rep.rhNote         = Just ("manifest " ++ path)
            , Rep.rhEnv          = Just env
            , Rep.rhPerCaseBatch = True
            }
      ref <- newIORef ([] :: [Rep.CaseReport])
      _ <- Rep.withReport rc rnd hdr $ \emit -> do
        forM_ cells $ \cell -> do
          cr <- runCell args d meter cfg lib cell
          modifyIORef' ref (cr :)
          emit cr
        pure Nothing
      cases <- fmap reverse (readIORef ref)
      let anyReject = any (isReject . Rep.crResult) cases
      exitWith (if anyReject then ExitFailure exitReject else ExitSuccess)

isReject :: C.Result -> Bool
isReject C.Reject{} = True
isReject C.Pass{}   = False

runCell
  :: Args -> M.Defaults -> C.Meter -> C.Config -> DL.Library -> Cell
  -> IO Rep.CaseReport
runCell args d meter cfg lib cell = do
  let mc = cellCase cell
  target <- DL.resolveTarget lib (M.mcTarget mc)
  let mk = buildHypothesis (M.mcWorkspace mc) (M.mcSecret mc)
             (M.mcPublic mc) (cellSeed cell) target
  hyp <- mk
  (b, mnoise) <- F.withFFIHypothesis hyp $ \h ->
    case caseBatch args d mc of
      M.BatchFixed n -> pure (n, Nothing)
      M.BatchAuto    -> do
        cb <- C.calibrateBatchReport meter h
        pure (C.crBatch cb, C.crNoise cb)
  let cfg' = cfg { C.cfgBatch = b }
  result <- F.withFFIHypothesis hyp $ \h -> C.runCT meter cfg' h
  mat <- case (argAttribute args, result) of
    (True, C.Reject{}) -> do
      h2 <- mk
      F.withFFIHypothesis h2 $ \h -> Just <$> C.attribute meter cfg' h
    _ -> pure Nothing
  pure Rep.CaseReport
    { Rep.crName        = cellName cell
    , Rep.crResult      = result
    , Rep.crAttribution = mat
    , Rep.crBatch       = Just b
    , Rep.crNoise       = mnoise
    , Rep.crTrace       = Nothing
    }
