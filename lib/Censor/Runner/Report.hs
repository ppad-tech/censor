{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Censor.Runner.Report
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Uniform report rendering and suite scaffolding for censor
-- executables. Three concentric layers:
--
-- * The lowest layer is a header\/case\/footer data model streamed
--   through a 'Renderer'. Three renderers are provided —
--   'pretty' (the ppad terminal aesthetic: bold brand line, dim
--   key column, middle-dot separators, ANSI-colored verdicts on a
--   TTY), 'json' (one line-oriented @censor\/report-v1@ object per
--   run, deterministic key order), and 'plain' (ASCII-only,
--   grep-safe).
-- * 'withReport' streams cases through the chosen renderer and
--   returns the accumulated list so callers can drive exit codes.
-- * At the top, t'Suite' and t'TraceSuite' package up the entire
--   censor test-suite shape (arg parse, meter selection, header
--   assembly, calibration, per-case loop, optional trace dump)
--   into one call, so a suite's @Main.hs@ contains only the
--   hypotheses and case list.

module Censor.Runner.Report (
    -- * Report config
    ReportCfg(..)
  , initReportCfg

    -- * Data model
  , ReportHeader(..)
  , CaseReport(..)
  , BatteryTail(..)

    -- * Renderers
  , Renderer(..)
  , pretty
  , json
  , plain

    -- * Driver
  , withReport

    -- * Format flags
  , Format(..)
  , parseFormat
  , rendererFor
  , takeFormatArg
  , resolveFormat

    -- * Standard suite arg surface
  , SuiteArgs(..)
  , parseSuiteArgs

    -- * Suite scaffolds
  , TraceSuite(..)
  , runTraceSuite
  , Suite(..)
  , runSuite
  ) where

import Control.Monad (forM)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Data.Word (Word64)
import Numeric (showHex)
import qualified System.Environment as Env
import System.Exit (die)
import System.IO (BufferMode(..), hIsTerminalDevice, hSetBuffering, stdout)
import Text.Printf (printf)

import Censor
import Censor.Runner
  ( traceResult, record, renderTracesJSON
  , verdictName, resultToJSON, withMeterArg
  )
import qualified Paths_ppad_censor as Paths

-- | Package name used in the report brand line (@ppad censor@) and
--   the @tool@ field of the JSON envelope. All censor executables
--   render as this string — individual suites do not carry their own
--   brand.
brand :: String
brand = "censor"

-- | Package version, sourced from the autogen'd @Paths_ppad_censor@
--   module so it always tracks the cabal file.
brandVersion :: String
brandVersion = showVersion Paths.version

-- report config -------------------------------------------------------------

-- | Runtime knobs for the renderer.
--
--   'rcColor' is set once at startup based on whether stdout is a
--   terminal. All ANSI helpers short-circuit to identity when it is
--   'False'.
data ReportCfg = ReportCfg
  { rcColor :: !Bool
  } deriving (Eq, Show)

-- | Initialise a 'ReportCfg' by probing whether stdout is a TTY.
--   Call once at the start of @main@ and thread the result through
--   the renderer callbacks.
initReportCfg :: IO ReportCfg
initReportCfg = do
  !isTty <- hIsTerminalDevice stdout
  pure ReportCfg { rcColor = isTty }

-- data model ---------------------------------------------------------------

-- | Static header emitted once, before any case row. Populate this
--   at 'main' entry with the run's own metadata; the brand line
--   (@ppad censor v\<VER\>@) is fixed and does not appear here.
data ReportHeader = ReportHeader
  { rhMeter  :: !String
    -- ^ meter name (@wall@ \/ @instructions@ \/ ...).
  , rhConfig :: !Config
    -- ^ the driver 'Config' the run used (surfaced on the header line).
  , rhSeed   :: !(Maybe Word64)
    -- ^ user-facing sampler seed (distinct from 'cfgSeed', which
    --   only seeds the per-pair A\/B order bit).
  , rhTarget :: !(Maybe String)
    -- ^ short target description, e.g. @\"Poly1305 MAC ==\"@.
  , rhNote   :: !(Maybe String)
    -- ^ optional one-line caveat under the header block.
  } deriving Show

-- | A single case row.
data CaseReport = CaseReport
  { crName        :: !String
    -- ^ case name displayed on the row.
  , crResult      :: !Result
    -- ^ the driver's verdict + evidence.
  , crAttribution :: !(Maybe Attribution)
    -- ^ optional A\/A + B\/B attribution result, surfaced under the
    --   case row on 'Reject'. 'Nothing' when the tool did not run
    --   attribution.
  } deriving Show

-- | The tail passed to a renderer's 'rndClose'. Carries the
--   accumulated case list and, optionally, the path to a companion
--   artefact (a trace JSON, a log) that the tool wrote alongside.
data BatteryTail = BatteryTail
  { btCases  :: ![CaseReport]
  , btOutput :: !(Maybe FilePath)
  } deriving Show

-- renderer interface -------------------------------------------------------

-- | A rendering strategy: one callback per lifecycle event.
--
--   The lifecycle is @rndOpen@ (once), then @rndCase@ per case (in
--   order), then @rndClose@ (once). Streaming renderers ('pretty',
--   'plain') emit at each callback; buffered renderers ('json') use
--   the case callback as a no-op and emit the full object at close.
data Renderer = Renderer
  { rndOpen  :: ReportCfg -> ReportHeader -> IO ()
  , rndCase  :: ReportCfg -> CaseReport   -> IO ()
  , rndClose :: ReportCfg -> ReportHeader -> BatteryTail -> IO ()
  }

-- format flag --------------------------------------------------------------

-- | Selectable output format.
data Format = FmtPretty | FmtJson | FmtPlain
  deriving (Eq, Show)

-- | Parse a @--format@ argument: @pretty@, @json@, or @plain@.
parseFormat :: String -> Maybe Format
parseFormat s = case s of
  "pretty" -> Just FmtPretty
  "json"   -> Just FmtJson
  "plain"  -> Just FmtPlain
  _        -> Nothing

-- | The 'Renderer' for a 'Format'.
rendererFor :: Format -> Renderer
rendererFor FmtPretty = pretty
rendererFor FmtJson   = json
rendererFor FmtPlain  = plain

-- | Position-insensitively pluck a @--format FMT@ pair out of an
--   argv list. Returns the format token if present and the argv with
--   that pair removed. Handy for demos whose remaining arguments are
--   positional.
takeFormatArg :: [String] -> (Maybe String, [String])
takeFormatArg = go []
  where
    go pre ("--format":v:rest) = (Just v, reverse pre ++ rest)
    go pre (x:xs)              = go (x:pre) xs
    go pre []                  = (Nothing, reverse pre)

-- | Resolve an optional format token to a 'Renderer', dying with a
--   clear message on unknown values. Defaults to 'pretty' when the
--   token is absent.
resolveFormat :: Maybe String -> IO Renderer
resolveFormat Nothing  = pure pretty
resolveFormat (Just s) = case parseFormat s of
  Just f  -> pure (rendererFor f)
  Nothing -> die $ "unknown --format " ++ show s
    ++ " (expected pretty|json|plain)"

-- driver -------------------------------------------------------------------

-- | Stream a battery through a renderer.
--
--   The body callback is handed an @emit@ action that records one
--   case row; call it after each hypothesis completes. The body may
--   return the path of a companion artefact (e.g. a trace JSON) to
--   surface in the footer. The accumulated case list is returned so
--   callers can drive an exit code off the verdicts.
withReport
  :: ReportCfg
  -> Renderer
  -> ReportHeader
  -> ((CaseReport -> IO ()) -> IO (Maybe FilePath))
  -> IO [CaseReport]
withReport rc rnd hdr body = do
  ref <- newIORef ([] :: [CaseReport])
  rndOpen rnd rc hdr
  out <- body $ \cr -> do
    rndCase rnd rc cr
    modifyIORef' ref (cr :)
  cases <- fmap reverse (readIORef ref)
  rndClose rnd rc hdr (BatteryTail cases out)
  pure cases

-- ANSI helpers -------------------------------------------------------------

ansiReset, ansiBold, ansiDim, ansiRed, ansiGreen :: String
ansiReset  = "\ESC[0m"
ansiBold   = "\ESC[1m"
ansiDim    = "\ESC[2m"
ansiRed    = "\ESC[31m"
ansiGreen  = "\ESC[32m"

withColor :: Bool -> String -> String -> String
withColor False _    t = t
withColor True  code t = code ++ t ++ ansiReset

cBold, cDim, cRed, cGreen :: ReportCfg -> String -> String
cBold  c = withColor (rcColor c) ansiBold
cDim   c = withColor (rcColor c) ansiDim
cRed   c = withColor (rcColor c) ansiRed
cGreen c = withColor (rcColor c) ansiGreen

-- pretty renderer ----------------------------------------------------------

-- | The ppad terminal aesthetic: bold brand line, a dim key column,
--   middle-dot (@·@) separators, ANSI-colored verdicts on a TTY.
pretty :: Renderer
pretty = Renderer
  { rndOpen  = prettyOpen
  , rndCase  = prettyCase
  , rndClose = prettyClose
  }

prettyOpen :: ReportCfg -> ReportHeader -> IO ()
prettyOpen rc hdr = do
  putStrLn $ "  " ++ cBold rc ("ppad " ++ brand)
    ++ " " ++ cDim rc ("v" ++ brandVersion)
  putStr (prettyHeaderBlock rc hdr)
  putStrLn ""

prettyHeaderBlock :: ReportCfg -> ReportHeader -> String
prettyHeaderBlock rc hdr =
  let entries = concat
        [ [("meter", meterLine hdr)]
        , maybe [] (\t -> [("target", t)]) (rhTarget hdr)
        , maybe [] (\w -> [("seed",   seedHex w)])   (rhSeed hdr)
        , maybe [] (\n -> [("note",   n)])           (rhNote hdr)
        ]
      keyW = maximum (1 : map (length . fst) entries)
      row (k, v) = "  " ++ cDim rc (padR keyW k) ++ "  " ++ v ++ "\n"
  in  concatMap row entries

meterLine :: ReportHeader -> String
meterLine hdr =
  let c = rhConfig hdr
  in  intercalate " \183 "
        [ rhMeter hdr
        , printf "alpha=%.1g"       (cfgAlpha c) :: String
        , "warmup=" ++ show (cfgWarmup c)
        , "budget=" ++ show (cfgBudget c)
        , "batch="  ++ show (cfgBatch c)
        ]

-- Row layout: verdict + pairs + driver in fixed-width columns on the
-- left, evidence in the middle, case name after a dim separator on
-- the right. Case-name column being last (unbounded) means the
-- numeric columns align across arbitrarily long names.
prettyCase :: ReportCfg -> CaseReport -> IO ()
prettyCase rc cr = do
  let r       = crResult cr
      verdict = case r of
        Reject{} -> cRed   rc "REJECT"
        Pass{}   -> cGreen rc "PASS  "
      pairs   = padL 8 (show (resPairs r))
      driver  = padR 6 $ case r of
        Reject{} -> shapeName (leakShape r)
        Pass{}   -> ""
      evid    = printf "peakLogW=%5.2f  p=%.2g"
                  (resPeakLogW r) (resPValue r) :: String
      sep     = cDim rc "\183"
  putStrLn $ "    " ++ verdict
    ++ "  " ++ cDim rc pairs
    ++ "  " ++ driver
    ++ "  " ++ evid
    ++ "  " ++ sep
    ++ "  " ++ crName cr
  case r of
    Reject{} -> do
      let (elo, ehi) = resEffect r
          detail = printf "effect=[%.1f,%.1f]  clipped=%d/%d"
                     elo ehi (resClipped r) (resPairs r) :: String
      putStrLn $ replicate detailIndent ' ' ++ cDim rc detail
    Pass{} -> pure ()
  case crAttribution cr of
    Just a  -> putStrLn $ replicate detailIndent ' '
                          ++ cDim rc (attributionText a)
    Nothing -> pure ()

-- Indent under the evidence column: 4 (row indent) + 6 (verdict)
-- + 2 + 8 (pairs) + 2 + 6 (driver) + 2 = 30.
detailIndent :: Int
detailIndent = 30

prettyClose :: ReportCfg -> ReportHeader -> BatteryTail -> IO ()
prettyClose rc _ tl = do
  let cs   = btCases tl
      nrej = length (filter (isReject . crResult) cs)
      npas = length cs - nrej
      pairs = sum (map (resPairs . crResult) cs)
      parts = [ colorCount cRed   rc nrej " reject"
              , colorCount cGreen rc npas " pass"
              , show pairs ++ " pairs" ]
      parts' = case btOutput tl of
        Just p  -> parts ++ ["wrote " ++ p]
        Nothing -> parts
  putStrLn ""
  putStrLn $ "  " ++ cDim rc (intercalate " \183 " parts')

colorCount
  :: (ReportCfg -> String -> String) -> ReportCfg -> Int -> String -> String
colorCount col rc n suf
  | n > 0     = col rc (show n) ++ suf
  | otherwise = show n ++ suf

-- json renderer ------------------------------------------------------------

-- | Emit one line-oriented JSON object per battery, deterministic
--   key order, with a @\"schema\":\"censor\/report-v1\"@ tag. Header
--   and per-case callbacks are no-ops; the whole object materialises
--   at close.
json :: Renderer
json = Renderer
  { rndOpen  = \_ _   -> pure ()
  , rndCase  = \_ _   -> pure ()
  , rndClose = jsonClose
  }

jsonClose :: ReportCfg -> ReportHeader -> BatteryTail -> IO ()
jsonClose _ hdr tl = putStrLn $ concat
  [ "{\"schema\":\"censor/report-v1\""
  , ",\"tool\":",    jstr brand
  , ",\"version\":", jstr brandVersion
  , ",\"meter\":",   jstr (rhMeter hdr)
  , ",\"config\":",  configJSON (rhConfig hdr)
  , maybe "" (\w -> ",\"seed\":"   ++ jstr (seedHex w)) (rhSeed hdr)
  , maybe "" (\t -> ",\"target\":" ++ jstr t)           (rhTarget hdr)
  , maybe "" (\n -> ",\"note\":"   ++ jstr n)           (rhNote hdr)
  , ",\"cases\":["
  , intercalate "," (map caseJSON (btCases tl))
  , "]"
  , maybe "" (\p -> ",\"output\":" ++ jstr p) (btOutput tl)
  , ",\"totals\":", totalsJSON (btCases tl)
  , "}"
  ]

configJSON :: Config -> String
configJSON c = concat
  [ "{\"alpha\":",  showF (cfgAlpha c)
  , ",\"budget\":", show (cfgBudget c)
  , ",\"batch\":",  show (cfgBatch c)
  , ",\"warmup\":", show (cfgWarmup c)
  , maybe "" (\w -> ",\"seed\":" ++ jstr (seedHex w)) (cfgSeed c)
  , "}"
  ]

caseJSON :: CaseReport -> String
caseJSON cr = concat
  [ "{\"name\":",   jstr (crName cr)
  , ",\"result\":", resultToJSON (crResult cr)
  , maybe "" (\a -> ",\"attribution\":" ++ jstr (attribJSONLabel a))
      (crAttribution cr)
  , "}"
  ]

totalsJSON :: [CaseReport] -> String
totalsJSON cs =
  let n  = length cs
      nr = length (filter (isReject . crResult) cs)
      np = n - nr
      pr = sum (map (resPairs . crResult) cs)
  in  concat
        [ "{\"cases\":",  show n
        , ",\"reject\":", show nr
        , ",\"pass\":",   show np
        , ",\"pairs\":",  show pr
        , "}"
        ]

attribJSONLabel :: Attribution -> String
attribJSONLabel a = case a of
  TargetLeak           -> "target leak"
  SampleAAsymmetry _   -> "sampleA asymmetry"
  SampleBAsymmetry _   -> "sampleB asymmetry"
  PervasiveAsymmetry{} -> "pervasive asymmetry"

-- plain renderer -----------------------------------------------------------

-- | ASCII-only, no ANSI, no unicode. Suitable for CI logs and grep
--   pipelines that dislike middle dots.
plain :: Renderer
plain = Renderer
  { rndOpen  = plainOpen
  , rndCase  = plainCase
  , rndClose = plainClose
  }

plainOpen :: ReportCfg -> ReportHeader -> IO ()
plainOpen _ hdr = do
  putStrLn $ "ppad " ++ brand ++ " v" ++ brandVersion
  let entries = concat
        [ [("meter", plainMeterLine hdr)]
        , maybe [] (\t -> [("target", t)]) (rhTarget hdr)
        , maybe [] (\w -> [("seed",   seedHex w)])   (rhSeed hdr)
        , maybe [] (\n -> [("note",   n)])           (rhNote hdr)
        ]
      keyW = maximum (1 : map (length . fst) entries)
  mapM_ (\(k, v) -> putStrLn $ padR keyW k ++ "  " ++ v) entries
  putStrLn ""

plainMeterLine :: ReportHeader -> String
plainMeterLine hdr =
  let c = rhConfig hdr
  in  intercalate ", "
        [ rhMeter hdr
        , printf "alpha=%.1g"       (cfgAlpha c) :: String
        , "warmup=" ++ show (cfgWarmup c)
        , "budget=" ++ show (cfgBudget c)
        , "batch="  ++ show (cfgBatch c)
        ]

plainCase :: ReportCfg -> CaseReport -> IO ()
plainCase _ cr = do
  let r  = crResult cr
      hd = printf "%s %s pairs=%d peakLogW=%.2f p=%.2g"
             (verdictName r) (crName cr) (resPairs r)
             (resPeakLogW r) (resPValue r) :: String
      drv = case r of
        Reject{} -> " driver=" ++ shapeName (leakShape r)
        Pass{}   -> ""
  putStrLn (hd ++ drv)
  case r of
    Reject{} -> do
      let (elo, ehi) = resEffect r
      putStrLn $ printf "  effect=[%.1f,%.1f] clipped=%d/%d"
        elo ehi (resClipped r) (resPairs r)
    Pass{} -> pure ()
  case crAttribution cr of
    Just a  -> putStrLn ("  " ++ attributionText a)
    Nothing -> pure ()

plainClose :: ReportCfg -> ReportHeader -> BatteryTail -> IO ()
plainClose _ _ tl = do
  let cs = btCases tl
      nr = length (filter (isReject . crResult) cs)
      np = length cs - nr
      pr = sum (map (resPairs . crResult) cs)
      wrote = case btOutput tl of
        Just p  -> " wrote=" ++ p
        Nothing -> ""
  putStrLn ""
  putStrLn $ "summary: reject=" ++ show nr
    ++ " pass=" ++ show np
    ++ " pairs=" ++ show pr
    ++ wrote

-- shared helpers -----------------------------------------------------------

isReject :: Result -> Bool
isReject Reject{} = True
isReject Pass{}   = False

shapeName :: LeakShape -> String
shapeName s = case s of
  BulkShift   -> "bulk"
  ShapeLeak   -> "shape"
  RareOutlier -> "tail"
  MixedShape  -> "mixed"

attributionText :: Attribution -> String
attributionText a = case a of
  TargetLeak ->
    "attribution: target leak (A/A and B/B both pass)"
  SampleAAsymmetry r ->
    "attribution: sampleA asymmetry (A/A rejected at "
    ++ show (resPairs r) ++ ")"
  SampleBAsymmetry r ->
    "attribution: sampleB asymmetry (B/B rejected at "
    ++ show (resPairs r) ++ ")"
  PervasiveAsymmetry ra rb ->
    "attribution: pervasive harness asymmetry (A/A at "
    ++ show (resPairs ra) ++ ", B/B at " ++ show (resPairs rb) ++ ")"

seedHex :: Word64 -> String
seedHex w = "0x" ++ showHex w ""

showF :: Double -> String
showF = printf "%.6g"

jstr :: String -> String
jstr s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c    = [c]

padR, padL :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
padL n s = replicate (max 0 (n - length s)) ' ' ++ s

-- shared suite arg surface -------------------------------------------------

-- | The parsed suite argv. All censor suites share the shape
--   @[--format FMT] [METER] [EXTRA...]@; each runner interprets
--   @saExtra@ in its own way (trace suites take the first extra as
--   an output path; plain suites reject them).
data SuiteArgs = SuiteArgs
  { saRenderer :: !Renderer
    -- ^ resolved from @--format@; defaults to 'pretty'.
  , saMeter    :: !(Maybe String)
    -- ^ meter name (@wall@, @instructions@, ...); 'Nothing' means
    --   accept the runner's default.
  , saExtra    :: ![String]
    -- ^ positional arguments left after meter, in order.
  }

-- | Parse the standard suite argv from 'Env.getArgs'. Dies with a
--   clear message if @--format@ is malformed. The extras list is
--   handed to the runner for further interpretation.
parseSuiteArgs :: IO SuiteArgs
parseSuiteArgs = do
  raw <- Env.getArgs
  let (farg, rest) = takeFormatArg raw
  rnd <- resolveFormat farg
  let (marg, extra) = case rest of
        []       -> (Nothing, [])
        (m:more) -> (Just m,  more)
  pure SuiteArgs
    { saRenderer = rnd
    , saMeter    = marg
    , saExtra    = extra
    }

-- trace-recording suite scaffold -------------------------------------------

-- | A trace-recording suite: iterate a parameterised hypothesis
--   over a case list under a chosen meter, record the per-pair
--   frame trajectory of each run, and dump the whole battery as a
--   plot-artifact JSON alongside the human-facing report.
--
--   The tool's @Main.hs@ builds one of these and calls
--   'runTraceSuite'; everything else — arg parse, meter selection,
--   'calibrateBatch', header assembly, per-case 'record' loop,
--   'renderTracesJSON' envelope write, 'withReport' driver — is
--   done inside.
data TraceSuite p a = TraceSuite
  { tsTarget :: !String
    -- ^ one-line target description shown in the header block.
  , tsConfig :: !Config
    -- ^ base driver config. 'calibrateBatch' overrides @cfgBatch@
    --   using the first case as its probe.
  , tsSeed   :: !(Maybe Word64)
    -- ^ user-facing sampler seed shown in the header block.
  , tsNote   :: !(Maybe String)
    -- ^ optional one-line note under the header block.
  , tsCases  :: ![(String, p)]
    -- ^ case list: display name paired with the hypothesis
    --   parameter.
  , tsBuild  :: !(p -> IO (Hypothesis a))
    -- ^ hypothesis builder invoked once per case.
  }

-- | Drive a t'TraceSuite' end to end. The trace JSON is written to
--   the first positional argument after the meter, defaulting to
--   @\<progName\>-\<meter\>.json@.
runTraceSuite :: TraceSuite p a -> IO ()
runTraceSuite ts = do
  hSetBuffering stdout LineBuffering
  prog <- Env.getProgName
  sa   <- parseSuiteArgs
  rc   <- initReportCfg
  probe <- case tsCases ts of
    []         -> die (prog ++ ": no cases configured")
    ((_,p):_)  -> pure p
  let out = case saExtra sa of
        (o:_) -> Just o
        _     -> Nothing
  withMeterArg (saMeter sa) $ \mname meter -> do
    h0 <- tsBuild ts probe
    b  <- calibrateBatch meter h0
    let c    = (tsConfig ts) { cfgBatch = b }
        hdr  = ReportHeader
          { rhMeter  = mname
          , rhConfig = c
          , rhSeed   = tsSeed ts
          , rhTarget = Just (tsTarget ts)
          , rhNote   = tsNote ts
          }
        path = fromMaybe (prog ++ "-" ++ mname ++ ".json") out
    _ <- withReport rc (saRenderer sa) hdr $ \emit -> do
      traces <- forM (tsCases ts) $ \(name, p) -> do
        h <- tsBuild ts p
        t <- record name mname meter c h
        emit (CaseReport name (traceResult t) Nothing)
        pure t
      writeFile path (renderTracesJSON mname (cfgAlpha c) traces)
      pure (Just path)
    pure ()

-- plain suite scaffold -----------------------------------------------------

-- | A censor test suite: a fixed set of heterogeneous hypotheses
--   (each with its own type and possibly its own 'Config'), no
--   trace recording, no per-case parameter passing. The tool
--   supplies a callback that runs the whole battery under the
--   chosen meter and emits each result; 'runSuite' handles the
--   surrounding scaffolding.
data Suite = Suite
  { suTarget :: !String
  , suConfig :: !Config
    -- ^ base driver config (shown in the header). Individual cases
    --   may run under different configs internally; the header only
    --   surfaces this one.
  , suSeed   :: !(Maybe Word64)
  , suNote   :: !(Maybe String)
  , suRun    :: !(String -> Meter -> (CaseReport -> IO ()) -> IO ())
    -- ^ battery driver. Receives the meter name, the meter, and an
    --   @emit@ action that records one 'CaseReport'.
  }

-- | Drive a t'Suite' end to end. Rejects positional arguments past
--   the meter with a usage error — plain suites do not take an
--   output path.
runSuite :: Suite -> IO ()
runSuite su = do
  hSetBuffering stdout LineBuffering
  prog <- Env.getProgName
  sa   <- parseSuiteArgs
  case saExtra sa of
    [] -> pure ()
    xs -> die $ prog ++ ": unexpected positional arg(s) "
            ++ show xs
            ++ " (usage: " ++ prog
            ++ " [--format pretty|json|plain] "
            ++ "[wall|instructions|cycles|branches])"
  rc <- initReportCfg
  withMeterArg (saMeter sa) $ \mname meter -> do
    let hdr = ReportHeader
          { rhMeter  = mname
          , rhConfig = suConfig su
          , rhSeed   = suSeed su
          , rhTarget = Just (suTarget su)
          , rhNote   = suNote su
          }
    _ <- withReport rc (saRenderer sa) hdr $ \emit -> do
      suRun su mname meter emit
      pure Nothing
    pure ()
