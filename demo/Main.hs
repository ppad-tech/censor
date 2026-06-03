{-# LANGUAGE BangPatterns #-}

-- censor-demo: exercise the sequential CT tester against a few
-- targets drawn from ppad-fixed. We adopt the dudect-style
-- "fix vs random" protocol: class A always supplies a canonical
-- fixed input, class B supplies fresh pseudorandom inputs. A
-- timing-leaky target should produce a measurable distributional
-- difference between classes; a CT target should not.

module Main where

import Control.Exception (evaluate)
import Control.Monad (replicateM_)
import qualified Data.Bits as B
import Data.IORef
import Data.Word (Word64)
import System.IO (hFlush, hSetBuffering, stdout, BufferMode(..))

import qualified Censor as C
import qualified Data.Word.Wider as W
import qualified Numeric.Montgomery.Secp256k1.Scalar as MS
import Data.Word.Wider (Wider)

-- splitmix64 -----------------------------------------------------------------

type Rng = IORef Word64

mkRng :: Word64 -> IO Rng
mkRng = newIORef

nextWord :: Rng -> IO Word64
nextWord ref = atomicModifyIORef' ref $ \s ->
  let !s'  = s + 0x9E3779B97F4A7C15
      !z0  = (s' `B.xor` (s' `B.shiftR` 30)) * 0xBF58476D1CE4E5B9
      !z1  = (z0 `B.xor` (z0 `B.shiftR` 27)) * 0x94D049BB133111EB
      !z2  =  z1 `B.xor` (z1 `B.shiftR` 31)
  in  (s', z2)

randomWider :: Rng -> IO Wider
randomWider g = do
  !a <- nextWord g
  !b <- nextWord g
  !c <- nextWord g
  !d <- nextWord g
  pure $! W.wider (fromIntegral a) (fromIntegral b)
                  (fromIntegral c) (fromIntegral d)

randomMont :: Rng -> IO MS.Montgomery
randomMont g = MS.to <$> randomWider g

-- hypotheses -----------------------------------------------------------------

-- positive control: Montgomery scalar multiplication.
-- fully-unrolled limb arithmetic; should be CT.
posScalarMul :: Rng -> Rng -> IO (C.Hypothesis (MS.Montgomery, MS.Montgomery))
posScalarMul ga gb = do
  let !fixA = MS.one
  pure C.Hypothesis
    { C.target  = \(!x, !y) -> do
        !_ <- evaluate (MS.mul x y)
        pure ()
    , C.sampleA = do
        !y <- randomMont ga
        pure (fixA, y)
    , C.sampleB = do
        !x <- randomMont gb
        !y <- randomMont gb
        pure (x, y)
    }

-- positive control: Montgomery modular inversion (addition chain).
-- 256 squarings + a fixed number of multiplications; should be CT.
posScalarInv :: Rng -> Rng -> IO (C.Hypothesis MS.Montgomery)
posScalarInv _ga gb = do
  pure C.Hypothesis
    { C.target  = \(!x) -> do
        !_ <- evaluate (MS.inv x)
        pure ()
    , C.sampleA = pure MS.one
    , C.sampleB = randomMont gb
    }

-- positive control: Montgomery exponentiation (constant-iteration
-- ladder driven by select#). Should be CT in the exponent.
posScalarExp :: Rng -> Rng -> IO (C.Hypothesis (MS.Montgomery, Wider))
posScalarExp _ga gb = do
  let !fixedExp = W.wider 1 0 0 0
  pure C.Hypothesis
    { C.target  = \(!b, !e) -> do
        !_ <- evaluate (MS.exp b e)
        pure ()
    , C.sampleA = do
        !b <- randomMont gb
        pure (b, fixedExp)
    , C.sampleB = do
        !b <- randomMont gb
        !e <- randomWider gb
        pure (b, e)
    }

-- negative control: a deliberately leaky target whose work scales
-- with the popcount of the value. A CT tester worth its salt must
-- reject this.
negLeakyPopcount :: Rng -> Rng -> IO (C.Hypothesis Wider)
negLeakyPopcount _ga gb = do
  pure C.Hypothesis
    { C.target  = \(!w) -> do
        let !n = B.popCount (W.from_vartime w :: Integer)
        replicateM_ (10 * n) (evaluate ())
    , C.sampleA = pure (W.wider 0 0 0 0)
    , C.sampleB = randomWider gb
    }

-- negative control: Wider -> Integer conversion is by name
-- variable-time. Class A always converts zero, class B random.
negFromVartime :: Rng -> Rng -> IO (C.Hypothesis Wider)
negFromVartime _ga gb = do
  pure C.Hypothesis
    { C.target  = \(!w) -> do
        !_ <- evaluate (W.from_vartime w :: Integer)
        pure ()
    , C.sampleA = pure (W.wider 0 0 0 0)
    , C.sampleB = randomWider gb
    }

-- driver ---------------------------------------------------------------------

-- looser alpha than the default, bigger budget than the test suite,
-- and a small inner-loop batch so micro-targets clear the wall-clock
-- counter resolution.
cfg :: C.Config
cfg = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgBudget = 200000
  , C.cfgWarmup = 500
  , C.cfgBatch  = 50
  }

-- bigger batch to defeat 42ns wall-clock quantisation for the
-- fast single-mul case.
cfgBigBatch :: C.Config
cfgBigBatch = cfg { C.cfgBatch = 2000 }

runCase :: String -> IO (C.Hypothesis a) -> IO ()
runCase = runCaseWith cfg

runCaseWith :: C.Config -> String -> IO (C.Hypothesis a) -> IO ()
runCaseWith c name mkH = do
  putStrLn $ "-- " ++ name
  hFlush stdout
  h <- mkH
  r <- C.runCT C.wallClock c h
  case r of
    C.Reject n w -> putStrLn $
      "  REJECT after " ++ show n ++ " pairs, logW=" ++ show w
    C.Pass   n w -> putStrLn $
      "  PASS    after " ++ show n ++ " pairs, logW=" ++ show w
  hFlush stdout

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ga <- mkRng 0xCAFEBABE
  gb <- mkRng 0xDEADBEEF
  putStrLn "ppad-censor demo: dudect-style fix-vs-random against ppad-fixed"
  putStrLn $ "config: " ++ show cfg
  putStrLn ""
  runCase "positive: Scalar.mul (batch=50)"
          (posScalarMul ga gb)
  runCaseWith cfgBigBatch
          "positive: Scalar.mul (batch=2000)"
          (posScalarMul ga gb)
  runCase "positive: Scalar.inv (addition chain)"
          (posScalarInv ga gb)
  runCase "positive: Scalar.exp (constant-iteration ladder)"
          (posScalarExp ga gb)
  runCase "negative: deliberately leaky popcount-driven loop"
          (negLeakyPopcount ga gb)
  runCase "negative: Wider.from_vartime (Integer conversion)"
          (negFromVartime ga gb)
