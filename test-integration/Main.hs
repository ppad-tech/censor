{-# LANGUAGE BangPatterns #-}

-- End-to-end integration test for the dlopen runner path.
--
-- Compiles a small C shim exporting two @void(uint8_t *)@ targets
-- (one constant-time, one linear in @ws[0]@), loads it through
-- 'Censor.Runner.DL', and drives fix-vs-random on byte offset 0..32
-- through the sequential driver. Asserts Pass on the CT target and
-- Reject on the leaky one.

module Main where

import qualified Censor as C
import qualified Censor.FFI as F
import qualified Censor.Rng as R
import qualified Censor.Runner.DL as DL
import Data.Bits (xor)
import Data.Word (Word8, Word64)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr)
import System.Environment (lookupEnv)
import System.Info (os)
import System.Process (callProcess)
import Test.Tasty
import Test.Tasty.HUnit

-- inline shim source; the build produces a shared library from this
-- string at test startup, so the suite doesn't depend on any file
-- outside the cabal target.
shimSource :: String
shimSource = unlines
  [ "#include <stdint.h>"
  , ""
  , "void ct_countdown(uint8_t *ws) {"
  , "  volatile uint64_t sink = 0;"
  , "  uint64_t n = 25500;"
  , "  (void)ws;"
  , "  while (n--) sink += n;"
  , "}"
  , ""
  , "void leaky_countdown(uint8_t *ws) {"
  , "  volatile uint64_t sink = 0;"
  , "  uint64_t n = (uint64_t)ws[0] * 100;"
  , "  while (n--) sink += n;"
  , "}"
  ]

sharedExt :: String
sharedExt = case os of
  "darwin" -> "dylib"
  _        -> "so"

tmpDir :: IO FilePath
tmpDir = maybe "/tmp" id `fmap` lookupEnv "TMPDIR"

buildShim :: IO FilePath
buildShim = do
  dir <- tmpDir
  let src = dir ++ "/censor-integration-shim.c"
      lib = dir ++ "/censor-integration-shim." ++ sharedExt
  writeFile src shimSource
  callProcess "cc" ["-O2", "-shared", "-fPIC", "-o", lib, src]
  pure lib

main :: IO ()
main = do
  lib <- buildShim
  defaultMain (tests lib)

tests :: FilePath -> TestTree
tests lib = testGroup "censor-integration" [
    testCase "ct_countdown passes within budget" $
      runCase lib "ct_countdown" ExpectPass

  , testCase "leaky_countdown rejects within budget" $
      runCase lib "leaky_countdown" ExpectReject
  ]

data Expected = ExpectPass | ExpectReject

runCase :: FilePath -> String -> Expected -> Assertion
runCase lib sym expected = DL.withLibrary lib $ \h -> do
  target <- DL.resolveTarget h sym
  hyp    <- buildHyp target
  res    <- F.withFFIHypothesis hyp (C.runCT C.wallClock cfg)
  case (expected, res) of
    (ExpectPass, C.Pass{}) ->
      pure ()
    (ExpectReject, C.Reject{}) ->
      pure ()
    (ExpectPass, C.Reject { C.resPairs = n, C.resPeakLogW = w }) ->
      assertFailure $
        "CT target falsely rejected at n=" ++ show n
        ++ " logW=" ++ show w
    (ExpectReject, C.Pass { C.resPairs = n, C.resPeakLogW = w }) ->
      assertFailure $
        "leaky target not detected; passed at n=" ++ show n
        ++ " logW=" ++ show w
  where
    cfg = C.defaultConfig
      { C.cfgAlpha  = 1.0e-3
      , C.cfgBudget = 4000
      , C.cfgWarmup = 100
      , C.cfgBatch  = 1
      }

-- workspace layout matches the manual smoke: 64 bytes, --secret 0:32,
-- --public empty. class A pins bytes 0..32 to a shared random-seeded
-- fixed buffer; class B fills the whole workspace at random. so ws[0]
-- is constant across class-A pairs and uniform across class-B pairs.
wsBytes :: Int
wsBytes = 64

secretLen :: Int
secretLen = 32

buildHyp :: (Ptr Word8 -> IO ()) -> IO F.FFIHypothesis
buildHyp target = do
  let seed = 0xC0FFEE5EED0BEEF7 :: Word64
  fixSec <- mallocBytes wsBytes
  gFix <- R.mkRng seed
  F.fillRandom gFix fixSec wsBytes
  ga <- R.mkRng (seed `xor` 0x0A0A0A0A0A0A0A0A)
  gb <- R.mkRng (seed `xor` 0x0B0B0B0B0B0B0B0B)
  pure F.FFIHypothesis
    { F.ffiWorkspaceBytes = wsBytes
    , F.ffiSampleA = \p -> do
        F.fillRandom ga p wsBytes
        copyBytes p fixSec secretLen
    , F.ffiSampleB = \p ->
        F.fillRandom gb p wsBytes
    , F.ffiTarget = target
    }
