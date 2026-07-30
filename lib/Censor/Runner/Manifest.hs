{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Censor.Runner.Manifest
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- A declarative battery description for the @censor@ foreign
-- runner: a list of cases, each naming a target symbol and the
-- workspace layout to drive it with, plus run-wide defaults.
--
-- Without one, auditing a library means one @censor@ process per
-- symbol and a shell loop to stitch the single-case reports back
-- together — which duplicates the tool version and config by hand
-- in every script, and produces no single artefact carrying the
-- whole battery. With one, a single invocation opens the library
-- once and emits one @censor\/report-v2@ object containing every
-- case, its controls, the host environment, and the family-wise
-- alpha.
--
-- The format is line-oriented. Blank lines and @#@ comments are
-- ignored; every other line is either a run-wide directive
-- (@key value@) or a case (@case NAME key=value...@):
--
-- > # ppad-secp256k1 scalar surfaces
-- > meter          wall
-- > alpha          1e-6
-- > budget         20000
-- > warmup         500
-- > target-prefix  censor_target_
-- > workspace      128
-- >
-- > case mul       secret=0:32
-- > case sign      workspace=96 secret=0:32 public=32:32
-- > case inv       secret=0:32 replicates=3
--
-- A case's target symbol is @target-prefix@ ++ its name unless
-- @target=@ overrides it outright.

module Censor.Runner.Manifest (
    -- * Types
    Manifest(..)
  , Defaults(..)
  , ManifestCase(..)
  , Range(..)
  , BatchSpec(..)

    -- * Parsing
  , parseManifest
  , emptyDefaults
  ) where

import Data.Char (isSpace)
import Data.Word (Word64)
import Numeric (readHex)

-- | A byte range within the workspace: @off:len@.
data Range = Range
  { rangeOff :: {-# UNPACK #-} !Int
  , rangeLen :: {-# UNPACK #-} !Int
  } deriving (Eq, Show)

-- | How a case picks its @cfgBatch@. Foreign targets genuinely
--   recompute on every repetition, so 'BatchAuto' is meaningful
--   here in a way it is not for a pure Haskell target.
data BatchSpec
  = BatchFixed !Int
    -- ^ pin the batch.
  | BatchAuto
    -- ^ calibrate it per case.
  deriving (Eq, Show)

-- | Run-wide directives. Every field is optional; 'Nothing' defers
--   to the runner's own default (or a command-line override).
data Defaults = Defaults
  { dfMeter     :: !(Maybe String)
  , dfAlpha     :: !(Maybe Double)
  , dfBudget    :: !(Maybe Int)
  , dfWarmup    :: !(Maybe Int)
  , dfBatch     :: !(Maybe BatchSpec)
  , dfSeed      :: !(Maybe Word64)
    -- ^ seeds the input samplers.
  , dfOrderSeed :: !(Maybe Word64)
    -- ^ seeds the A\/B order bit. Left unset in a checked-in
    --   manifest: the default is fresh OS entropy per run, and the
    --   resolved value is reported for replay.
  , dfPrefix    :: !(Maybe String)
  , dfWorkspace :: !(Maybe Int)
  } deriving (Eq, Show)

-- | All directives unset.
emptyDefaults :: Defaults
emptyDefaults = Defaults
  { dfMeter     = Nothing
  , dfAlpha     = Nothing
  , dfBudget    = Nothing
  , dfWarmup    = Nothing
  , dfBatch     = Nothing
  , dfSeed      = Nothing
  , dfOrderSeed = Nothing
  , dfPrefix    = Nothing
  , dfWorkspace = Nothing
  }

-- | One case: a display name, the symbol to resolve, the workspace
--   layout, and how many times to replicate it.
data ManifestCase = ManifestCase
  { mcName       :: !String
  , mcTarget     :: !String
  , mcWorkspace  :: !Int
  , mcSecret     :: ![Range]
  , mcPublic     :: ![Range]
  , mcBatch      :: !(Maybe BatchSpec)
  , mcReplicates :: {-# UNPACK #-} !Int
    -- ^ how many independent fixed secrets to run this case
    --   against, each an ordinary case row of its own.
    --
    --   A dudect-style fix-vs-random cell pins /one/ fixed secret,
    --   so it answers "does this secret time differently from
    --   random", not "does any secret". Replicating guards against
    --   a single unlucky (or unluckily typical) draw. Each
    --   replicate counts toward the family-wise alpha, as it
    --   should.
  } deriving (Eq, Show)

-- | A parsed manifest.
data Manifest = Manifest
  { mfDefaults :: !Defaults
  , mfCases    :: ![ManifestCase]
  } deriving (Eq, Show)

-- | Parse a manifest. Returns a message naming the offending line
--   on failure.
--
--   Case order is preserved. A manifest with no cases is an error:
--   it is far more likely a typo than an intent.
parseManifest :: String -> Either String Manifest
parseManifest src = do
  mf <- foldl step (Right (Manifest emptyDefaults [])) numbered
  case mfCases mf of
    [] -> Left "manifest: no cases declared"
    cs -> Right mf { mfCases = reverse cs }
  where
    numbered = zip [1 :: Int ..] (lines src)
    step acc (n, raw) = do
      mf <- acc
      case words (strip raw) of
        []           -> Right mf
        ("case" : r) -> fmap (\c -> mf { mfCases = c : mfCases mf })
                             (parseCase n (mfDefaults mf) r)
        [k, v]       -> fmap (\d -> mf { mfDefaults = d })
                             (parseDirective n (mfDefaults mf) k v)
        ws           -> Left (at n ("expected 'key value' or "
                        ++ "'case NAME ...', got " ++ show (unwords ws)))

at :: Int -> String -> String
at n msg = "manifest line " ++ show n ++ ": " ++ msg

-- drop a trailing comment and surrounding whitespace. '#' only
-- starts a comment at the beginning of a token, so a value may
-- contain one.
strip :: String -> String
strip = dropWhile isSpace . go
  where
    go ('#' : _)              = []
    go (c : '#' : _) | isSpace c = []
    go (c : cs)               = c : go cs
    go []                     = []

parseDirective :: Int -> Defaults -> String -> String -> Either String Defaults
parseDirective n d k v = case k of
  "meter"         -> Right d { dfMeter     = Just v }
  "target-prefix" -> Right d { dfPrefix    = Just v }
  "alpha"         -> fmap (\x -> d { dfAlpha     = Just x }) (num "alpha")
  "budget"        -> fmap (\x -> d { dfBudget    = Just x }) (nat "budget")
  "warmup"        -> fmap (\x -> d { dfWarmup    = Just x }) (nat "warmup")
  "workspace"     -> fmap (\x -> d { dfWorkspace = Just x }) (pos "workspace")
  "batch"         -> fmap (\x -> d { dfBatch     = Just x }) (batch n v)
  "seed"          -> fmap (\x -> d { dfSeed      = Just x }) (hex n "seed" v)
  "order-seed"    -> fmap (\x -> d { dfOrderSeed = Just x })
                          (hex n "order-seed" v)
  _ -> Left (at n ("unknown directive " ++ show k))
  where
    num nm = case reads v of
      [(x, "")] -> Right (x :: Double)
      _ -> Left (at n (nm ++ " expects a number, got " ++ show v))
    nat nm = case reads v of
      [(x, "")] | x >= 0 -> Right (x :: Int)
      _ -> Left (at n (nm ++ " expects a non-negative integer, got "
                       ++ show v))
    pos nm = case reads v of
      [(x, "")] | x > 0 -> Right (x :: Int)
      _ -> Left (at n (nm ++ " expects a positive integer, got " ++ show v))

batch :: Int -> String -> Either String BatchSpec
batch _ "auto" = Right BatchAuto
batch n v = case reads v of
  [(x, "")] | x >= 1 -> Right (BatchFixed x)
  _ -> Left (at n ("batch expects a positive integer or 'auto', got "
                   ++ show v))

hex :: Int -> String -> String -> Either String Word64
hex n nm v =
  let s = case v of
        '0' : 'x' : r -> r
        '0' : 'X' : r -> r
        _             -> v
  in  case readHex s of
        [(w, "")] -> Right w
        _ -> Left (at n (nm ++ " expects a hex Word64, got " ++ show v))

parseCase
  :: Int -> Defaults -> [String] -> Either String ManifestCase
parseCase n d toks = case toks of
  [] -> Left (at n "case needs a name")
  (name : kvs) -> do
    c0 <- Right ManifestCase
      { mcName       = name
      , mcTarget     = maybe "" id (dfPrefix d) ++ name
      , mcWorkspace  = maybe 0 id (dfWorkspace d)
      , mcSecret     = []
      , mcPublic     = []
      , mcBatch      = Nothing
      , mcReplicates = 1
      }
    c <- foldl (\acc kv -> acc >>= \x -> applyKV n x kv) (Right c0) kvs
    validateCase n c { mcSecret = reverse (mcSecret c)
                     , mcPublic = reverse (mcPublic c) }

applyKV :: Int -> ManifestCase -> String -> Either String ManifestCase
applyKV n c kv = case break (== '=') kv of
  (k, '=' : v) -> case k of
    "target"     -> Right c { mcTarget = v }
    "label"      -> Right c { mcName   = v }
    "secret"     -> fmap (\r -> c { mcSecret = r : mcSecret c })
                         (range n "secret" v)
    "public"     -> fmap (\r -> c { mcPublic = r : mcPublic c })
                         (range n "public" v)
    "batch"      -> fmap (\b -> c { mcBatch = Just b }) (batch n v)
    "workspace"  -> case reads v of
      [(x, "")] | x > 0 -> Right c { mcWorkspace = x }
      _ -> Left (at n ("workspace expects a positive integer, got "
                       ++ show v))
    "replicates" -> case reads v of
      [(x, "")] | x >= 1 -> Right c { mcReplicates = x }
      _ -> Left (at n ("replicates expects a positive integer, got "
                       ++ show v))
    _ -> Left (at n ("unknown case key " ++ show k))
  _ -> Left (at n ("expected key=value, got " ++ show kv))

range :: Int -> String -> String -> Either String Range
range n nm v = case break (== ':') v of
  (off, ':' : len) -> case (reads off, reads len) of
    ([(o, "")], [(l, "")]) | o >= 0 && l > 0 -> Right (Range o l)
    _ -> bad
  _ -> bad
  where
    bad = Left (at n (nm ++ " expects off:len with off >= 0, len > 0,"
                      ++ " got " ++ show v))

validateCase :: Int -> ManifestCase -> Either String ManifestCase
validateCase n c
  | mcWorkspace c <= 0 =
      Left (at n ("case " ++ mcName c ++ " has no workspace; set one "
                  ++ "on the case or a run-wide 'workspace' default"))
  | null (mcSecret c) =
      Left (at n ("case " ++ mcName c ++ " declares no secret range; "
                  ++ "there is no fix-vs-random axis without one"))
  | otherwise = case filter (overflows (mcWorkspace c)) rs of
      (Range o l : _) ->
        Left (at n ("case " ++ mcName c ++ " range " ++ show o ++ ":"
                    ++ show l ++ " overflows its workspace of "
                    ++ show (mcWorkspace c) ++ " bytes"))
      [] -> Right c
  where
    rs = mcSecret c ++ mcPublic c
    overflows ws (Range o l) = o + l > ws
