{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

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
--   TTY), 'json' (one @censor\/report-v2@ object per run,
--   deterministic key order, per-case trace arrays when present),
--   and 'plain' (ASCII-only, grep-safe).
-- * 'withReport' streams cases through the chosen renderer and
--   returns the accumulated list so callers can drive exit codes.
-- * At the top, t'Suite' and t'TraceSuite' package up the entire
--   censor test-suite shape (arg parse, meter selection, header
--   assembly, per-case calibration and attribution, trace record,
--   JSON artefact write) into one call, so a suite's @Main.hs@
--   contains only the hypotheses and case list.

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

    -- * Host environment (re-exports)
  , Env(..)
  , captureEnv

    -- * Suite scaffolds
  , BatchPolicy(..)
  , TraceCase(..)
  , TraceSuite(..)
  , runTraceSuite
  , Suite(..)
  , runSuite

    -- * JSON serialisation
  , reportJSON
  ) where

import Control.Monad (forM_)
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

import Censor hiding (crBatch, crNoise)
import qualified Censor as C
import Censor.Runner
  ( traceResult, traceFrames, record, thin
  , verdictName, shapeName, resultToJSON, withMeterArg
  )
import Censor.Runner.Env (Env(..), captureEnv)
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
  { rhMeter     :: !String
    -- ^ meter name (@wall@ \/ @instructions@ \/ ...).
  , rhConfig    :: !Config
    -- ^ the driver 'Config' the run used (surfaced on the header line).
  , rhSeed      :: !(Maybe Word64)
    -- ^ user-facing sampler seed (distinct from 'cfgSeed', which
    --   only seeds the per-pair A\/B order bit).
  , rhTarget    :: !(Maybe String)
    -- ^ short target description, e.g. @\"Poly1305 MAC ==\"@.
  , rhNote      :: !(Maybe String)
    -- ^ optional one-line caveat under the header block.
  , rhEnv       :: !(Maybe Env)
    -- ^ host-environment snapshot taken at run start. The suite
    --   scaffolds ('runTraceSuite', 'runSuite') always populate it;
    --   'Nothing' when the caller assembled the header by hand
    --   without capturing one.
  , rhPerCaseBatch :: !Bool
    -- ^ 'True' when each case picks its own 'cfgBatch' from its
    --   'BatchPolicy' (see 'runTraceSuite'). Header renders
    --   @batch=per-case@ and the config JSON emits
    --   @\"batch\":\"per-case\"@; the resolved batch for each case
    --   appears on that case's row via 'crBatch'. 'False' means the
    --   run used a single 'cfgBatch' from 'rhConfig' throughout,
    --   rendered as usual.
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
  , crBatch       :: !(Maybe Int)
    -- ^ the 'cfgBatch' the case actually ran under. Populated by
    --   'runTraceSuite' from per-case 'calibrateBatch'; 'Nothing'
    --   when the case ran under a shared 'rhConfig' batch.
  , crNoise       :: !(Maybe Noise)
    -- ^ the meter's noise floor at the calibrated batch, from the
    --   case's t'CalibReport'. 'Nothing' when the case was not
    --   calibrated per-case.
  , crTrace       :: !(Maybe [Frame])
    -- ^ per-pair 'Frame' trajectory, when the runner recorded one
    --   (via 'record'). Surfaces in the JSON artefact as a thinned
    --   array; the human renderers ignore it.
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
        , maybe [] (envEntries " \183 ") (rhEnv hdr)
        , maybe [] (\n -> [("note",   n)])           (rhNote hdr)
        ]
      keyW = maximum (1 : map (length . fst) entries)
      row (k, v) = "  " ++ cDim rc (padR keyW k) ++ "  " ++ v ++ "\n"
  in  concatMap row entries

-- header rows for a host-environment snapshot: a system line
-- (os/kernel/arch, CPU model, cores), a knob line when any Linux
-- tuning knob resolved, the load average, and the capture time.
envEntries :: String -> Env -> [(String, String)]
envEntries sep e = concat
  [ [("system", intercalate sep (systemSegs e))]
  , case knobSegs e of
      [] -> []
      ks -> [("env", intercalate sep ks)]
  , maybe [] (\(l1, l5, l15) ->
      [("load", printf "%.2f %.2f %.2f" l1 l5 l15)]) (envLoad e)
  , maybe [] (\t -> [("time", t)]) (envTime e)
  ]

systemSegs :: Env -> [String]
systemSegs e = concat
  [ [unwords (concat
      [ [envOs e], maybe [] pure (envKernel e), [envArch e] ])]
  , maybe [] pure (envCpu e)
  , [show (envCores e) ++ " cores"]
  ]

knobSegs :: Env -> [String]
knobSegs e = concat
  [ maybe [] (\v -> ["governor=" ++ v]) (envGovernor e)
  , maybe [] pure (envBoost e)  -- already key=value
  , maybe [] (\v -> ["smt="      ++ v]) (envSmt e)
  , maybe [] (\v -> ["paranoid=" ++ v]) (envParanoid e)
  ]

meterLine :: ReportHeader -> String
meterLine hdr =
  let c = rhConfig hdr
  in  intercalate " \183 "
        [ rhMeter hdr
        , printf "alpha=%.1g"       (cfgAlpha c) :: String
        , "warmup=" ++ show (cfgWarmup c)
        , "budget=" ++ show (cfgBudget c)
        , "batch="  ++ batchText hdr
        ]

batchText :: ReportHeader -> String
batchText hdr
  | rhPerCaseBatch hdr = "per-case"
  | otherwise          = show (cfgBatch (rhConfig hdr))

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
  case calibText cr of
    Just t  -> putStrLn $ replicate detailIndent ' ' ++ cDim rc t
    Nothing -> pure ()
  case crAttribution cr of
    Just a  -> putStrLn $ replicate detailIndent ' '
                          ++ cDim rc (attributionText a)
    Nothing -> pure ()

-- the calibration detail line under a case row: resolved batch and,
-- when recorded, the meter's noise floor at that batch.
calibText :: CaseReport -> Maybe String
calibText cr = case segs of
  [] -> Nothing
  xs -> Just (unwords xs)
  where
    segs = concat
      [ maybe [] (\b -> ["batch=" ++ show b]) (crBatch cr)
      , maybe [] (\n -> [ printf "noise med=%d iqr=%d"
                            (nsMedian n) (nsIqr n) ])
          (crNoise cr)
      ]

-- Indent under the evidence column: 4 (row indent) + 6 (verdict)
-- + 2 + 8 (pairs) + 2 + 6 (driver) + 2 = 30.
detailIndent :: Int
detailIndent = 30

prettyClose :: ReportCfg -> ReportHeader -> BatteryTail -> IO ()
prettyClose rc hdr tl = do
  let cs   = btCases tl
      nrej = length (filter (isReject . crResult) cs)
      npas = length cs - nrej
      pairs = sum (map (resPairs . crResult) cs)
      parts = concat
        [ [ colorCount cRed   rc nrej " reject"
          , colorCount cGreen rc npas " pass"
          , show pairs ++ " pairs" ]
        , familySeg hdr cs
        , maybe [] (\p -> ["wrote " ++ p]) (btOutput tl)
        ]
  putStrLn ""
  putStrLn $ "  " ++ cDim rc (intercalate " \183 " parts)

-- Family-wise alpha, shown only for multi-case batteries (for a
-- single case it is just alpha, and saying so adds noise).
familySeg :: ReportHeader -> [CaseReport] -> [String]
familySeg hdr cs
  | n <= 1    = []
  | otherwise = [printf "family alpha<=%.1g" (familyAlpha alpha n)]
  where
    n     = length cs
    alpha = cfgAlpha (rhConfig hdr)

colorCount
  :: (ReportCfg -> String -> String) -> ReportCfg -> Int -> String -> String
colorCount col rc n suf
  | n > 0     = col rc (show n) ++ suf
  | otherwise = show n ++ suf

-- json renderer ------------------------------------------------------------

-- | Emit one @censor\/report-v2@ JSON object per battery,
--   deterministic key order. Header and per-case callbacks are
--   no-ops; the whole object materialises at close, exactly as
--   'reportJSON' would render it.
json :: Renderer
json = Renderer
  { rndOpen  = \_ _   -> pure ()
  , rndCase  = \_ _   -> pure ()
  , rndClose = jsonClose
  }

jsonClose :: ReportCfg -> ReportHeader -> BatteryTail -> IO ()
jsonClose _ hdr tl = putStrLn (reportJSON hdr (btCases tl))

-- | Serialise a header + case list as the canonical
--   @censor\/report-v2@ JSON object. Deterministic key order; the
--   per-case block carries 'crBatch' and 'crTrace' as optional
--   fields, present exactly when the runner recorded them.
--
--   The 'json' renderer and 'runTraceSuite' both call this so
--   stdout output and the on-disk artefact are byte-identical.
reportJSON :: ReportHeader -> [CaseReport] -> String
reportJSON hdr cases = concat
  [ "{\"schema\":\"censor/report-v2\""
  , ",\"tool\":",    jstr brand
  , ",\"version\":", jstr brandVersion
  , ",\"meter\":",   jstr (rhMeter hdr)
  , ",\"config\":",  configJSON hdr
  , maybe "" (\e -> ",\"env\":"    ++ envJSON e)        (rhEnv hdr)
  , maybe "" (\w -> ",\"seed\":"   ++ jstr (seedHex w)) (rhSeed hdr)
  , maybe "" (\t -> ",\"target\":" ++ jstr t)           (rhTarget hdr)
  , maybe "" (\n -> ",\"note\":"   ++ jstr n)           (rhNote hdr)
  , ",\"cases\":["
  , intercalate "," (map caseJSON cases)
  , "]"
  , ",\"totals\":", totalsJSON (cfgAlpha (rhConfig hdr)) cases
  , "}"
  ]

configJSON :: ReportHeader -> String
configJSON hdr =
  let c = rhConfig hdr
      batchField
        | rhPerCaseBatch hdr = ",\"batch\":\"per-case\""
        | otherwise          = ",\"batch\":" ++ show (cfgBatch c)
  in  concat
        [ "{\"alpha\":",  showF (cfgAlpha c)
        , ",\"budget\":", show (cfgBudget c)
        , ",\"warmup\":", show (cfgWarmup c)
        , batchField
        , maybe "" (\w -> ",\"seed\":" ++ jstr (seedHex w)) (cfgSeed c)
        , "}"
        ]

-- optional keys are omitted (rather than emitted null) when the
-- corresponding Env field did not resolve on the host.
envJSON :: Env -> String
envJSON e = concat
  [ "{\"os\":",    jstr (envOs e)
  , ",\"arch\":",  jstr (envArch e)
  , maybe "" (\v -> ",\"kernel\":"   ++ jstr v) (envKernel e)
  , maybe "" (\v -> ",\"cpu\":"      ++ jstr v) (envCpu e)
  , ",\"cores\":", show (envCores e)
  , maybe "" (\v -> ",\"governor\":" ++ jstr v) (envGovernor e)
  , maybe "" (\v -> ",\"boost\":"    ++ jstr v) (envBoost e)
  , maybe "" (\v -> ",\"smt\":"      ++ jstr v) (envSmt e)
  , maybe "" (\v -> ",\"paranoid\":" ++ jstr v) (envParanoid e)
  , maybe "" (\(l1, l5, l15) -> ",\"load\":["
      ++ intercalate "," (map showF [l1, l5, l15]) ++ "]")
      (envLoad e)
  , maybe "" (\v -> ",\"time\":"     ++ jstr v) (envTime e)
  , "}"
  ]

caseJSON :: CaseReport -> String
caseJSON cr = concat
  [ "{\"name\":",   jstr (crName cr)
  , maybe "" (\b -> ",\"batch\":" ++ show b) (crBatch cr)
  , maybe "" (\n -> ",\"noise\":{\"med\":" ++ show (nsMedian n)
      ++ ",\"iqr\":" ++ show (nsIqr n) ++ "}") (crNoise cr)
  , ",\"result\":", resultToJSON (crResult cr)
  , maybe "" (\a -> ",\"controls\":" ++ controlsJSON a)
      (crAttribution cr)
  , maybe "" (\frs -> ",\"trace\":" ++ traceArrayJSON frs) (crTrace cr)
  , "}"
  ]

traceArrayJSON :: [Frame] -> String
traceArrayJSON frs =
  "[" ++ intercalate "," (map frameJSON (thin frs)) ++ "]"
  where
    frameJSON fr = printf "[%d,%.4f,%.4f,%.1f,%.1f]"
      (frPair fr) (frLogW fr) (frLogWSup fr) (frLo fr) (frHi fr)

totalsJSON :: Double -> [CaseReport] -> String
totalsJSON alpha cs =
  let n  = length cs
      nr = length (filter (isReject . crResult) cs)
      np = n - nr
      pr = sum (map (resPairs . crResult) cs)
  in  concat
        [ "{\"cases\":",  show n
        , ",\"reject\":", show nr
        , ",\"pass\":",   show np
        , ",\"pairs\":",  show pr
        , ",\"alpha\":",  showF alpha
        , ",\"familyAlpha\":", showF (familyAlpha alpha n)
        , "}"
        ]

-- Censor's type-I guarantee is per run. A battery that calls
-- failure when /any/ of its M cells rejects has a family-wise false
-- alarm probability bounded by M * alpha (union bound), not alpha.
-- Reported so a reader does not have to reconstruct it; to actually
-- test at alpha family-wise, run each cell at alpha / M.
familyAlpha :: Double -> Int -> Double
familyAlpha !alpha !n = min 1 (alpha * fromIntegral (max 1 n))

-- The controls block: the combined verdict plus both control runs
-- in full. Serialising the runs (rather than reducing them to a
-- label) is deliberate — 'ControlsPass' is an absence of evidence,
-- and a reader should be able to see how much evidence was
-- actually gathered before accepting it.
controlsJSON :: Attribution -> String
controlsJSON a = concat
  [ "{\"verdict\":", jstr (controlLabel (attVerdict a))
  , ",\"aa\":",      resultToJSON (attAA a)
  , ",\"bb\":",      resultToJSON (attBB a)
  , "}"
  ]

controlLabel :: ControlVerdict -> String
controlLabel v = case v of
  ControlsPass       -> "controls pass"
  SampleAAsymmetry   -> "sampleA asymmetry"
  SampleBAsymmetry   -> "sampleB asymmetry"
  PervasiveAsymmetry -> "pervasive asymmetry"

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
        , maybe [] (envEntries ", ") (rhEnv hdr)
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
        , "batch="  ++ batchText hdr
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
  case calibText cr of
    Just t  -> putStrLn ("  " ++ t)
    Nothing -> pure ()
  case crAttribution cr of
    Just a  -> putStrLn ("  " ++ attributionText a)
    Nothing -> pure ()

plainClose :: ReportCfg -> ReportHeader -> BatteryTail -> IO ()
plainClose _ hdr tl = do
  let cs = btCases tl
      nr = length (filter (isReject . crResult) cs)
      np = length cs - nr
      pr = sum (map (resPairs . crResult) cs)
      fam = case familySeg hdr cs of
        (s:_) -> " " ++ s
        []    -> ""
      wrote = case btOutput tl of
        Just p  -> " wrote=" ++ p
        Nothing -> ""
  putStrLn ""
  putStrLn $ "summary: reject=" ++ show nr
    ++ " pass=" ++ show np
    ++ " pairs=" ++ show pr
    ++ fam
    ++ wrote

-- shared helpers -----------------------------------------------------------

isReject :: Result -> Bool
isReject Reject{} = True
isReject Pass{}   = False

attributionText :: Attribution -> String
attributionText a = case attVerdict a of
  ControlsPass ->
    "controls: pass (A/A " ++ show (resPairs (attAA a))
    ++ ", B/B " ++ show (resPairs (attBB a))
    ++ " pairs; consistent with a target leak, not proof of one)"
  SampleAAsymmetry ->
    "controls: sampleA asymmetry (A/A rejected at "
    ++ show (resPairs (attAA a)) ++ ")"
  SampleBAsymmetry ->
    "controls: sampleB asymmetry (B/B rejected at "
    ++ show (resPairs (attBB a)) ++ ")"
  PervasiveAsymmetry ->
    "controls: pervasive harness asymmetry (A/A at "
    ++ show (resPairs (attAA a)) ++ ", B/B at "
    ++ show (resPairs (attBB a)) ++ ")"

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
--   @[--format FMT] [--batch N] [METER] [EXTRA...]@; each runner
--   interprets @saExtra@ in its own way (trace suites take the
--   first extra as an output path; plain suites reject them).
data SuiteArgs = SuiteArgs
  { saRenderer :: !Renderer
    -- ^ resolved from @--format@; defaults to 'pretty'.
  , saMeter    :: !(Maybe String)
    -- ^ meter name (@wall@, @instructions@, ...); 'Nothing' means
    --   accept the runner's default.
  , saBatch    :: !(Maybe Int)
    -- ^ fixed batch from @--batch@; 'Nothing' means the runner's
    --   default (trace suites auto-calibrate). Pure Haskell
    --   targets share the applied thunk across a batch, so reps
    --   beyond the first are no-ops — @--batch 1@ is the honest
    --   setting for them (mirrors the dlopen CLI's flag).
  , saExtra    :: ![String]
    -- ^ positional arguments left after meter, in order.
  }

-- | Parse the standard suite argv from 'Env.getArgs'. Dies with a
--   clear message if @--format@ or @--batch@ is malformed. The
--   extras list is handed to the runner for further interpretation.
parseSuiteArgs :: IO SuiteArgs
parseSuiteArgs = do
  raw <- Env.getArgs
  let (farg, rest0) = takeFormatArg raw
      (barg, rest)  = takeBatchArg rest0
  rnd <- resolveFormat farg
  bat <- case barg of
    Nothing -> pure Nothing
    Just s  -> case reads s of
      [(n, "")] | n >= 1 -> pure (Just n)
      _ -> die $ "unknown --batch " ++ show s
        ++ " (expected a positive integer)"
  let (marg, extra) = case rest of
        []       -> (Nothing, [])
        (m:more) -> (Just m,  more)
  pure SuiteArgs
    { saRenderer = rnd
    , saMeter    = marg
    , saBatch    = bat
    , saExtra    = extra
    }

-- | Position-insensitively pluck a @--batch N@ pair out of an argv
--   list, mirroring 'takeFormatArg'.
takeBatchArg :: [String] -> (Maybe String, [String])
takeBatchArg = go []
  where
    go pre ("--batch":v:rest) = (Just v, reverse pre ++ rest)
    go pre (x:xs)             = go (x:pre) xs
    go pre []                 = (Nothing, reverse pre)

-- trace-recording suite scaffold -------------------------------------------

-- | How a case's 'cfgBatch' is chosen. The distinction is a
--   /repeatability contract/ the case author asserts about the
--   target, and it cannot be inferred: batching only multiplies
--   real work when each repetition genuinely recomputes.
data BatchPolicy
  = SingleShot
    -- ^ One execution per timed region. The correct policy for a
    --   pure Haskell target such as @\\a -> () <$ evaluate (f a)@:
    --   the @f a@ thunk is shared across the batch and forced once,
    --   so repetitions beyond the first are no-ops on an
    --   already-evaluated value and a calibrated batch of @N@ would
    --   describe one call plus @N-1@ no-ops. If the target is
    --   sub-quantum under the meter, widen the work rather than
    --   batching.
  | RepeatableAuto
    -- ^ Each repetition genuinely recomputes, so calibrate the
    --   batch with 'calibrateBatchReport'. Appropriate for foreign
    --   (FFI) calls and targets driving mutable state — anything
    --   that cannot be memoised by a thunk.
  | FixedBatch !Int
    -- ^ Pin the batch outright, bypassing calibration. Use when
    --   batches must match across meters for comparability.
  deriving (Eq, Show)

-- | One case in a t'TraceSuite': a display name, its 'BatchPolicy',
--   and an @IO@ action producing the hypothesis. Existentially
--   quantified over the hypothesis input type so a suite can hold
--   cases with different payload shapes in one list.
--
--   >>> TraceCase "encrypt" SingleShot     posAeadEncrypt
--   >>> TraceCase "mul"     RepeatableAuto posFfiMul
--
--   The action is re-executed for the calibration probe (under
--   'RepeatableAuto'), the main run, and (on 'Reject') the negative
--   controls, so the builder should be inexpensive and side-effect
--   free.
data TraceCase =
  forall a. TraceCase !String !BatchPolicy !(IO (Hypothesis a))

-- | A trace-recording suite: iterate a list of hypotheses under a
--   chosen meter, record each run's per-pair frame trajectory, and
--   write the whole battery as a single @censor\/report-v2@ JSON
--   artefact alongside the human-facing report.
--
--   The tool's @Main.hs@ builds one of these and calls
--   'runTraceSuite'; everything else — arg parse, meter selection,
--   per-case 'calibrateBatch', 'record', auto-attribution on
--   'Reject', header assembly, @withReport@ driver, JSON write —
--   is done inside.
data TraceSuite = TraceSuite
  { tsTarget :: !String
    -- ^ one-line target description shown in the header block.
  , tsConfig :: !Config
    -- ^ base driver config. 'runTraceSuite' calibrates 'cfgBatch'
    --   per case (or pins it from @--batch@), so the value of
    --   @cfgBatch@ here is ignored; 'cfgAlpha', 'cfgWarmup',
    --   'cfgBudget', and 'cfgSeed' are applied verbatim.
  , tsSeed   :: !(Maybe Word64)
    -- ^ user-facing sampler seed shown in the header block.
  , tsNote   :: !(Maybe String)
    -- ^ optional one-line note under the header block.
  , tsCases  :: ![TraceCase]
    -- ^ heterogeneous case list.
  }

-- | Drive a t'TraceSuite' end to end. The JSON artefact is written
--   to the first positional argument after the meter, defaulting to
--   @\<progName\>-\<meter\>.json@; it carries the same content the
--   'json' renderer emits to stdout under @--format json@.
--
--   A host-environment snapshot ('captureEnv') is taken at run start
--   and rides on the header. Per case: 'calibrateBatchReport' picks
--   the batch and records the meter's noise floor at it (both
--   surface on the case row and in the JSON) unless @--batch N@
--   pins the batch (no probe, no noise floor — the honest setting
--   for pure Haskell targets is @--batch 1@), 'record' streams the
--   run and keeps its frames, and on 'Reject' 'attribute' runs the
--   A\/A + B\/B diagnostics automatically (two extra runs).
runTraceSuite :: TraceSuite -> IO ()
runTraceSuite ts = do
  hSetBuffering stdout LineBuffering
  prog <- Env.getProgName
  sa   <- parseSuiteArgs
  rc   <- initReportCfg
  case tsCases ts of
    [] -> die (prog ++ ": no cases configured")
    _  -> pure ()
  let out = case saExtra sa of
        (o:_) -> Just o
        _     -> Nothing
  withMeterArg (saMeter sa) $ \mname meter -> do
    env <- captureEnv
    let hdr = ReportHeader
          { rhMeter     = mname
          , rhConfig    = (tsConfig ts)
              { cfgBatch =
                  fromMaybe (cfgBatch (tsConfig ts)) (saBatch sa) }
          , rhSeed      = tsSeed ts
          , rhTarget    = Just (tsTarget ts)
          , rhNote      = tsNote ts
          , rhEnv       = Just env
          , rhPerCaseBatch = maybe True (const False) (saBatch sa)
          }
        path = fromMaybe (prog ++ "-" ++ mname ++ ".json") out
    ref <- newIORef ([] :: [CaseReport])
    _ <- withReport rc (saRenderer sa) hdr $ \emit -> do
      forM_ (tsCases ts) $ \(TraceCase name policy mkH) -> do
        h  <- mkH
        -- --batch overrides every case's policy; otherwise the
        -- case's own declared policy governs, and only
        -- RepeatableAuto runs a calibration probe.
        (b, mnoise) <- case maybe policy FixedBatch (saBatch sa) of
          SingleShot     -> pure (1, Nothing)
          FixedBatch n   -> pure (n, Nothing)
          RepeatableAuto -> do
            cb <- calibrateBatchReport meter h
            pure (C.crBatch cb, C.crNoise cb)
        let c = (tsConfig ts) { cfgBatch = b }
        t <- record name mname meter c h
        let r = traceResult t
        mat <- case r of
          Reject{} -> do
            hAtt <- mkH
            Just <$> attribute meter c hAtt
          Pass{}   -> pure Nothing
        let cr = CaseReport
              { crName        = name
              , crResult      = r
              , crAttribution = mat
              , crBatch       = Just b
              , crNoise       = mnoise
              , crTrace       = Just (traceFrames t)
              }
        modifyIORef' ref (cr :)
        emit cr
      cases <- fmap reverse (readIORef ref)
      writeFile path (reportJSON hdr cases)
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
  case saBatch sa of
    Nothing -> pure ()
    Just _  -> die $ prog ++ ": --batch is a trace-suite flag; "
            ++ "plain suites run each case under its own config"
  rc <- initReportCfg
  withMeterArg (saMeter sa) $ \mname meter -> do
    env <- captureEnv
    let hdr = ReportHeader
          { rhMeter     = mname
          , rhConfig    = suConfig su
          , rhSeed      = suSeed su
          , rhTarget    = Just (suTarget su)
          , rhNote      = suNote su
          , rhEnv       = Just env
          , rhPerCaseBatch = False
          }
    _ <- withReport rc (saRenderer sa) hdr $ \emit -> do
      suRun su mname meter emit
      pure Nothing
    pure ()
