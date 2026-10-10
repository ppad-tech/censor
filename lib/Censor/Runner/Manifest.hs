{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Censor.Runner.Manifest
-- Copyright: (c) 2026 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- A declarative battery for the @censor@ CLI: run-wide defaults and
-- a list of cases, each naming a target symbol and the workspace
-- layout to drive it with. One invocation opens the library once and
-- emits one @censor\/report-v3@ object covering every case.
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
-- > case ecdh      secret=0:32 context=32:32
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
  , readRange
  , readSeed

    -- * Layout validation
  , checkLayout
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
  , dfMargin    :: !(Maybe Double)
    -- ^ relative interval-null margin (see 'Censor.cfgMargin').
  , dfSeed      :: !(Maybe Word64)
    -- ^ seeds the input samplers.
  , dfOrderSeed :: !(Maybe Word64)
    -- ^ seeds the A\/B order bit. Left unset in a checked-in
    --   manifest: the default is fresh OS entropy per run, and the
    --   resolved value is reported for replay.
  , dfPrefix    :: !(Maybe String)
  , dfWorkspace :: !(Maybe Int)
  , dfFamilyAlpha :: !(Maybe Bool)
    -- ^ when set, run every expanded cell at @alpha \/ M@ (M being
    --   the number of cells) so the /battery/ tests at @alpha@
    --   family-wise, rather than merely reporting the union bound.
  } deriving (Eq, Show)

-- | All directives unset.
emptyDefaults :: Defaults
emptyDefaults = Defaults
  { dfMeter     = Nothing
  , dfAlpha     = Nothing
  , dfBudget    = Nothing
  , dfWarmup    = Nothing
  , dfBatch     = Nothing
  , dfMargin    = Nothing
  , dfSeed      = Nothing
  , dfOrderSeed = Nothing
  , dfPrefix    = Nothing
  , dfWorkspace = Nothing
  , dfFamilyAlpha = Nothing
  }

-- | One case: a display name, the symbol to resolve, the workspace
--   layout, and how many times to replicate it.
data ManifestCase = ManifestCase
  { mcId         :: !String
    -- ^ the case token as written in the manifest, before any
    --   @label=@ override. This is the case's /identity/: the
    --   runner mixes it into the run-wide seed to give each case
    --   its own fixed secret.
    --
    --   Kept apart from 'mcName' so that renaming a case for
    --   presentation does not change the experiment it runs.
  , mcName       :: !String
    -- ^ the display name: 'mcId', or whatever @label=@ set. Used
    --   for rendering only.
  , mcTarget     :: !String
  , mcWorkspace  :: !Int
  , mcSecret     :: ![Range]
  , mcPublic     :: ![Range]
  , mcContext    :: ![Range]
    -- ^ ranges redrawn once per /pair/ and written identically into
    --   both classes: a fresh public input shared by the pair (see
    --   'Censor.fixVsRandomCtx').
  , mcBatch      :: !(Maybe BatchSpec)
  , mcReplicates :: {-# UNPACK #-} !Int
    -- ^ how many independent fixed secrets to run this case
    --   against, each its own case row. One pinned secret only asks
    --   whether /that/ secret times differently from random;
    --   replicating guards against an unlucky draw.
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

-- Drop a comment and leading whitespace. '#' starts a comment
-- anywhere on the line: no field here (symbol, range, number) has
-- any use for one, so there is nothing to escape.
strip :: String -> String
strip = dropWhile isSpace . takeWhile (/= '#')

parseDirective :: Int -> Defaults -> String -> String -> Either String Defaults
parseDirective n d k v = case k of
  "meter"         -> Right d { dfMeter     = Just v }
  "target-prefix" -> Right d { dfPrefix    = Just v }
  "alpha"         -> fmap (\x -> d { dfAlpha     = Just x }) (num "alpha")
  "budget"        -> fmap (\x -> d { dfBudget    = Just x }) (nat "budget")
  "warmup"        -> fmap (\x -> d { dfWarmup    = Just x }) (nat "warmup")
  "workspace"     -> fmap (\x -> d { dfWorkspace = Just x }) (pos "workspace")
  "batch"         -> fmap (\x -> d { dfBatch     = Just x }) (batch n v)
  "margin"        -> fmap (\x -> d { dfMargin    = Just x }) posnum
  "seed"          -> fmap (\x -> d { dfSeed      = Just x }) (hex n "seed" v)
  "order-seed"    -> fmap (\x -> d { dfOrderSeed = Just x })
                          (hex n "order-seed" v)
  "family-alpha"  -> fmap (\x -> d { dfFamilyAlpha = Just x })
                          (flag n "family-alpha" v)
  _ -> Left (at n ("unknown directive " ++ show k))
  where
    num nm = case reads v of
      [(x, "")] -> Right (x :: Double)
      _ -> Left (at n (nm ++ " expects a number, got " ++ show v))
    posnum = case reads v of
      [(x, "")] | x > 0 -> Right (x :: Double)
      _ -> Left (at n ("margin expects a positive number, got "
                       ++ show v))
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

flag :: Int -> String -> String -> Either String Bool
flag _ _ "on"  = Right True
flag _ _ "off" = Right False
flag n nm v = Left (at n (nm ++ " expects 'on' or 'off', got " ++ show v))

hex :: Int -> String -> String -> Either String Word64
hex n nm v = case readSeed v of
  Just w  -> Right w
  Nothing -> Left (at n (nm ++ " expects a hex Word64, got " ++ show v))

-- | Read a hex 'Word64', with or without a @0x@ prefix.
readSeed :: String -> Maybe Word64
readSeed v =
  let s = case v of
        '0' : 'x' : r -> r
        '0' : 'X' : r -> r
        _             -> v
  in  case readHex s of
        [(w, "")] -> Just w
        _         -> Nothing

parseCase
  :: Int -> Defaults -> [String] -> Either String ManifestCase
parseCase n d toks = case toks of
  [] -> Left (at n "case needs a name")
  (name : kvs) -> do
    let c0 = ManifestCase
          { mcId         = name
          , mcName       = name
          , mcTarget     = maybe "" id (dfPrefix d) ++ name
          , mcWorkspace  = maybe 0 id (dfWorkspace d)
          , mcSecret     = []
          , mcPublic     = []
          , mcContext    = []
          , mcBatch      = Nothing
          , mcReplicates = 1
          }
    c <- foldl (\acc kv -> acc >>= \x -> applyKV n x kv) (Right c0) kvs
    validateCase n c { mcSecret  = reverse (mcSecret c)
                     , mcPublic  = reverse (mcPublic c)
                     , mcContext = reverse (mcContext c) }

applyKV :: Int -> ManifestCase -> String -> Either String ManifestCase
applyKV n c kv = case break (== '=') kv of
  (k, '=' : v) -> case k of
    "target"     -> Right c { mcTarget = v }
    "label"      -> Right c { mcName   = v }
    "secret"     -> fmap (\r -> c { mcSecret = r : mcSecret c })
                         (range n "secret" v)
    "public"     -> fmap (\r -> c { mcPublic = r : mcPublic c })
                         (range n "public" v)
    "context"    -> fmap (\r -> c { mcContext = r : mcContext c })
                         (range n "context" v)
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
range n nm v = case readRange v of
  Just r  -> Right r
  Nothing -> Left (at n (nm ++ " expects off:len with off >= 0, len > 0,"
                         ++ " got " ++ show v))

-- | Read an @off:len@ byte range, with @off >= 0@ and @len > 0@.
readRange :: String -> Maybe Range
readRange v = case break (== ':') v of
  (off, ':' : len) -> case (reads off, reads len) of
    ([(o, "")], [(l, "")]) | o >= 0 && l > 0 -> Just (Range o l)
    _ -> Nothing
  _ -> Nothing

validateCase :: Int -> ManifestCase -> Either String ManifestCase
validateCase n c
  | mcWorkspace c <= 0 =
      Left (at n ("case " ++ mcName c ++ " has no workspace; set one "
                  ++ "on the case or a run-wide 'workspace' default"))
  | null (mcSecret c) =
      Left (at n ("case " ++ mcName c ++ " declares no secret range; "
                  ++ "there is no fix-vs-random axis without one"))
  | otherwise =
      case checkLayout (mcWorkspace c) (mcSecret c) (mcPublic c)
             (mcContext c) of
        Just msg -> Left (at n ("case " ++ mcName c ++ " " ++ msg))
        Nothing  -> Right c

-- | Check a workspace layout: every declared range must fit inside
--   the workspace, and the three roles must be pairwise disjoint.
--   'Nothing' when the layout is sound; otherwise a message naming
--   the first problem, for the caller to place in context.
--
--   Both entry points validate through this — a manifest case and
--   the single-case command line — so the two cannot drift on what
--   they accept.
--
--   Overlapping roles are rejected rather than resolved by
--   precedence. A byte is pinned in class A only ('mcSecret'),
--   pinned in both for the run ('mcPublic'), refreshed per pair in
--   both ('mcContext'), or freshly random in each class; asking for
--   two of those at once has no coherent reading, and whichever
--   overlay happened to land last would silently pick one. That is
--   how a public range could mask the context it overlapped,
--   turning a per-pair refresh into a constant without saying so.
checkLayout
  :: Int      -- ^ workspace bytes
  -> [Range]  -- ^ secret ranges
  -> [Range]  -- ^ public ranges
  -> [Range]  -- ^ context ranges
  -> Maybe String
checkLayout ws secrets publics contexts =
  case filter (overflows . snd) tagged of
    ((role, r) : _) -> Just
      (role ++ " " ++ showRange r ++ " overflows the workspace of "
       ++ show ws ++ " bytes")
    [] -> case clashes of
      ((ra, r, rb) : _) -> Just
        (ra ++ " " ++ showRange r ++ " overlaps a " ++ rb ++ " range")
      [] -> Nothing
  where
    tagged = concat
      [ [("secret",  r) | r <- secrets]
      , [("public",  r) | r <- publics]
      , [("context", r) | r <- contexts]
      ]
    clashes =
      [ (ra, a, rb)
      | (ra, as, rb, bs) <-
          [ ("secret",  secrets,  "public",  publics)
          , ("secret",  secrets,  "context", contexts)
          , ("context", contexts, "public",  publics)
          ]
      , a <- as, b <- bs, overlaps a b
      ]
    overflows (Range o l) = o + l > ws
    overlaps (Range o1 l1) (Range o2 l2) =
      o1 < o2 + l2 && o2 < o1 + l1

showRange :: Range -> String
showRange (Range o l) = show o ++ ":" ++ show l
