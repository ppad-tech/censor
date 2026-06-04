{-# LANGUAGE BangPatterns #-}

-- censor-demo: exercise the sequential CT tester against a few
-- targets drawn from ppad-fixed and ppad-secp256k1. We adopt the
-- dudect-style "fix vs random" protocol: class A always supplies one
-- fixed input, class B supplies fresh pseudorandom inputs. A
-- timing-leaky target should produce a measurable distributional
-- difference between classes; a CT target should not.
--
-- Two measurement-hygiene rules keep the comparison honest (see
-- Censor's "Measurement hygiene" notes): inputs are returned in normal
-- form, so no lazy conversion is paid inside the timed region, and the
-- fixed input is an ordinary heap value built by the same sampler as
-- the random class -- not a top-level CAF, whose distinct access path
-- a sensitive meter would otherwise flag as a spurious difference.
--
-- On Linux the demo runs each case under the deterministic
-- instructions-retired PMU meter and again under the cycles meter
-- (which is additionally sensitive to data-dependent latency and
-- microarchitectural state), falling back to wall-clock if perf is
-- unavailable.

module Main where

import Control.Exception (evaluate, try)
import Control.Monad (replicateM, replicateM_)
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word8, Word64)
import System.IO (hFlush, hSetBuffering, stdout, BufferMode(..))

import qualified Censor as C
import qualified Crypto.Curve.Secp256k1 as Secp
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

-- a random Montgomery value, forced to normal form: the to# conversion
-- is performed here, not left as a thunk for the meter to pay inside
-- the timed region.
randomMont :: Rng -> IO MS.Montgomery
randomMont g = do
  !w <- randomWider g
  evaluate (MS.to w)

-- a random 32-byte ByteString in NF. used to build messages and aux
-- random data for the signing hypotheses. constructed at sample time,
-- outside the timed region; replicateM is justified here -- it builds
-- a fixed four-element list of pre-forced Word64s, immediately
-- flattened into a strict ByteString that 'evaluate' fully forces.
random32 :: Rng -> IO BS.ByteString
random32 g = do
  ws <- replicateM 4 (nextWord g)
  evaluate $! BS.pack
    [ fromIntegral (w `B.shiftR` (8 * i)) :: Word8
    | w <- ws, i <- [0 .. 7 :: Int]
    ]

-- seed for the fixed class. drawing the fixed input from a dedicated
-- generator (rather than naming a top-level constant) yields a
-- reproducible but heap-allocated value, reached by the same code as
-- the random class.
fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- hypotheses -----------------------------------------------------------------

-- positive control: Montgomery scalar multiplication.
-- fully-unrolled limb arithmetic; should be CT.
posScalarMul :: Rng -> Rng -> IO (C.Hypothesis (MS.Montgomery, MS.Montgomery))
posScalarMul ga gb = do
  gFix  <- mkRng fixedSeed
  !fixX <- randomMont gFix
  pure C.Hypothesis
    { C.target  = \(!x, !y) -> do
        !_ <- evaluate (MS.mul x y)
        pure ()
    , C.sampleA = do
        !y <- randomMont ga
        pure (fixX, y)
    , C.sampleB = do
        !x <- randomMont gb
        !y <- randomMont gb
        pure (x, y)
    }

-- positive control: Montgomery modular inversion (addition chain).
-- 256 squarings + a fixed number of multiplications; should be CT.
posScalarInv :: Rng -> Rng -> IO (C.Hypothesis MS.Montgomery)
posScalarInv _ga gb = do
  gFix  <- mkRng fixedSeed
  !fixX <- randomMont gFix
  pure C.Hypothesis
    { C.target  = \(!x) -> do
        !_ <- evaluate (MS.inv x)
        pure ()
    , C.sampleA = pure fixX
    , C.sampleB = randomMont gb
    }

-- positive control: Montgomery exponentiation (constant-iteration
-- ladder driven by select#). Should be CT in the exponent.
posScalarExp :: Rng -> Rng -> IO (C.Hypothesis (MS.Montgomery, Wider))
posScalarExp _ga gb = do
  gFix  <- mkRng fixedSeed
  !fixE <- randomWider gFix
  pure C.Hypothesis
    { C.target  = \(!b, !e) -> do
        !_ <- evaluate (MS.exp b e)
        pure ()
    , C.sampleA = do
        !b <- randomMont gb
        pure (b, fixE)
    , C.sampleB = do
        !b <- randomMont gb
        !e <- randomWider gb
        pure (b, e)
    }

-- positive control: wNAF scalar multiplication of the secp256k1
-- generator. The CT property of interest is that the timing of
-- @mul_wnaf tex s@ should not depend on the scalar @s@.
posMulWNAF
  :: Secp.Context -> Rng -> Rng -> IO (C.Hypothesis Wider)
posMulWNAF tex _ga gb = do
  gFix  <- mkRng fixedSeed
  !fixS <- randomWider gFix
  pure C.Hypothesis
    { C.target  = \(!s) -> do
        !_ <- evaluate (Secp.mul_wnaf tex s)
        pure ()
    , C.sampleA = pure fixS
    , C.sampleB = randomWider gb
    }

-- positive control: BIP0340 Schnorr signing under a precomputed
-- context. The secret-key path runs through wNAF mul; messages and
-- aux randomness are public, so a leak (if any) shows up as
-- fix-vs-random on the secret.
posSignSchnorr
  :: Secp.Context
  -> Rng -> Rng
  -> IO (C.Hypothesis (Wider, BS.ByteString, BS.ByteString))
posSignSchnorr tex _ga gb = do
  gFix    <- mkRng fixedSeed
  !fixSec <- randomWider gFix
  pure C.Hypothesis
    { C.target = \(!sec, !msg, !aux) -> do
        !_ <- evaluate (Secp.sign_schnorr' tex sec msg aux)
        pure ()
    , C.sampleA = do
        !msg <- random32 gb
        !aux <- random32 gb
        pure (fixSec, msg, aux)
    , C.sampleB = do
        !sec <- randomWider gb
        !msg <- random32 gb
        !aux <- random32 gb
        pure (sec, msg, aux)
    }

-- positive control: RFC6979 ECDSA signing under a precomputed
-- context. RFC6979 derives the nonce deterministically from the
-- secret + message, then signs via wNAF mul.
posSignECDSA
  :: Secp.Context
  -> Rng -> Rng
  -> IO (C.Hypothesis (Wider, BS.ByteString))
posSignECDSA tex _ga gb = do
  gFix    <- mkRng fixedSeed
  !fixSec <- randomWider gFix
  pure C.Hypothesis
    { C.target = \(!sec, !msg) -> do
        !_ <- evaluate (Secp.sign_ecdsa' tex sec msg)
        pure ()
    , C.sampleA = do
        !msg <- random32 gb
        pure (fixSec, msg)
    , C.sampleB = do
        !sec <- randomWider gb
        !msg <- random32 gb
        pure (sec, msg)
    }

-- negative control: a deliberately leaky target whose work scales
-- with the popcount of the value. A CT tester worth its salt must
-- reject this.
negLeakyPopcount :: Rng -> Rng -> IO (C.Hypothesis Wider)
negLeakyPopcount _ga gb = do
  !fixZero <- evaluate (W.wider 0 0 0 0)
  pure C.Hypothesis
    { C.target  = \(!w) -> do
        let !n = B.popCount (W.from_vartime w :: Integer)
        replicateM_ (10 * n) (evaluate ())
    , C.sampleA = pure fixZero
    , C.sampleB = randomWider gb
    }

-- negative control: Wider -> Integer conversion is by name
-- variable-time. Class A always converts zero, class B random.
negFromVartime :: Rng -> Rng -> IO (C.Hypothesis Wider)
negFromVartime _ga gb = do
  !fixZero <- evaluate (W.wider 0 0 0 0)
  pure C.Hypothesis
    { C.target  = \(!w) -> do
        !_ <- evaluate (W.from_vartime w :: Integer)
        pure ()
    , C.sampleA = pure fixZero
    , C.sampleB = randomWider gb
    }

-- driver ---------------------------------------------------------------------

-- looser alpha than the default, bigger budget than the test suite,
-- and a small inner-loop batch so micro-targets clear the wall-clock
-- counter resolution. The CT controls now (correctly) run to the full
-- budget before passing, so the budget is kept modest.
cfg :: C.Config
cfg = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgBudget = 50000
  , C.cfgWarmup = 500
  , C.cfgBatch  = 50
  }

-- Scalar.mul is fast enough that wall-clock readings on macOS (~42ns
-- quantum) are dominated by per-call cache and allocator asymmetries
-- between the fixed and random classes rather than mul itself. A
-- larger inner-loop batch amortizes the fixed cost across enough muls
-- that the per-call asymmetries average out. Irrelevant under the
-- deterministic PMU meter, but harmless there.
cfgBigBatch :: C.Config
cfgBigBatch = cfg { C.cfgBatch = 2000 }

-- Secp targets are far slower per call than the Montgomery scalar
-- arithmetic (microseconds rather than nanoseconds), so a per-call
-- batch of 1 is already well above any wall-clock quantum and keeps
-- total run-time modest.
cfgHeavy :: C.Config
cfgHeavy = cfg { C.cfgBatch = 1 }

runCaseWith :: C.Meter -> C.Config -> String -> IO (C.Hypothesis a) -> IO ()
runCaseWith meter c name mkH = do
  putStrLn $ "-- " ++ name
  hFlush stdout
  h <- mkH
  r <- C.runCT meter c h
  case r of
    C.Reject n w -> putStrLn $
      "  REJECT after " ++ show n ++ " pairs, logW=" ++ show w
    C.Pass   n w -> putStrLn $
      "  PASS    after " ++ show n ++ " pairs, logW=" ++ show w
  hFlush stdout

-- run every case under the given meter.
runAll :: Secp.Context -> C.Meter -> IO ()
runAll tex meter = do
  ga <- mkRng 0xCAFEBABE
  gb <- mkRng 0xDEADBEEF
  let rc = runCaseWith meter cfg
      rh = runCaseWith meter cfgHeavy
  runCaseWith meter cfgBigBatch
     "positive: Scalar.mul (single multiply, batch=2000)"  (posScalarMul ga gb)
  rc "positive: Scalar.inv (addition chain)"               (posScalarInv ga gb)
  rc "positive: Scalar.exp (constant-iteration ladder)"    (posScalarExp ga gb)
  rh "positive: Secp.mul_wnaf (wNAF point multiplication)" (posMulWNAF tex ga gb)
  rh "positive: Secp.sign_schnorr' (BIP0340)"              (posSignSchnorr tex ga gb)
  rh "positive: Secp.sign_ecdsa' (RFC6979)"                (posSignECDSA tex ga gb)
  rc "negative: deliberately leaky popcount-driven loop"   (negLeakyPopcount ga gb)
  rc "negative: Wider.from_vartime (Integer conversion)"   (negFromVartime ga gb)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "ppad-censor demo: dudect-style fix-vs-random \
           \against ppad-fixed and ppad-secp256k1"
  putStrLn $ "config: " ++ show cfg
  -- one-shot secp256k1 precomputation, shared across all secp targets
  -- and across every meter.
  let !tex = Secp.precompute
  -- Try each PMU meter in turn: instructions_retired (deterministic
  -- code-path tally) and then cycles (catches data-dependent latency
  -- too). Fall back to wall-clock only if perf_event_open is
  -- unavailable (non-Linux, or kernel.perf_event_paranoid > 2).
  ri <- tryMeter tex C.InstructionsRetired "instructions_retired"
  rc <- tryMeter tex C.Cycles              "cycles"
  case (ri, rc) of
    (Left err, Left _) -> do
      putStrLn $ "meter: wall-clock (PMU unavailable: "
              ++ show err ++ ")\n"
      runAll tex C.wallClock
    _ -> pure ()
  where
    tryMeter tex counter label =
      try (C.withCounter counter $ \meter -> do
             putStrLn $ "\nmeter: " ++ label
             runAll tex meter) :: IO (Either C.MeterError ())
