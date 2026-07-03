{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- censor-demo-ffi: exercise the FFI bridge against the bundled
-- Rust staticlib (rs-demo). Three groups of targets:
--
--   * a controlled pair of 32-byte inequality witnesses, one
--     constant-time and one deliberately leaky, as a
--     positive/negative control;
--   * fastcrypto's secp256r1 production sign path, whose
--     underlying table-indexed scalar multiplication leaks the
--     secret scalar via timing -- censor is expected to reject
--     it under PMU on x86-64;
--   * a Bernstein-Yang Fr inverter vendored from halo2curves
--     with the `core::hint::black_box(mask)` guard stripped
--     from `cond_add`/`cond_neg`. On aarch64 the compiled code
--     is genuinely branch-free in the secret; on x86-64 LLVM
--     inserts a secret-dependent conditional jump and censor
--     should reject under PMU. See
--     ppad.tech/articles/introducing-substratum.md.
--
-- The meter is chosen by the first command-line argument (wall,
-- instructions, branches, or cycles), defaulting to wall-clock.
-- instructions_retired catches control-flow leaks (different code
-- paths retire different counts) but is blind to data-dependent
-- latency from cache misses, so it can pass a target that
-- nonetheless leaks via table-indexed loads; cycles counts stall
-- cycles too and is the right meter for table-lookup-style leaks.
-- The PMU meters are Linux-only; asking for one elsewhere is a
-- hard error rather than a silent fallback.

module Main where

import Control.Exception (try)
import Control.Monad (when)
import Data.Word (Word8, Word64)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Alloc (allocaBytes, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke)
import qualified System.Environment as Env
import System.Exit (die)
import System.IO (hFlush, hSetBuffering, stdout, BufferMode(..))
import Text.Printf (printf)

import qualified Censor as C
import qualified Censor.FFI as F

-- foreign imports ------------------------------------------------------------

-- rs-demo
foreign import ccall unsafe "censor_ct_neq32"
  c_ct_neq32    :: Ptr Word8 -> Ptr Word8 -> IO Word8

foreign import ccall unsafe "censor_naive_neq32"
  c_naive_neq32 :: Ptr Word8 -> Ptr Word8 -> IO Word8

-- rs-fastcrypto
foreign import ccall unsafe "h_secp256r1_keypair_from_bytes"
  h_kp_from_bytes :: Ptr Word8 -> IO (Ptr ())

foreign import ccall unsafe "h_secp256r1_sign"
  h_sign          :: Ptr () -> Ptr Word8 -> CSize -> IO ()

-- rs-demo: bn254 Fr Bernstein-Yang inverter, black_box removed
foreign import ccall unsafe "h_byinv_fr_invert"
  h_byinv_fr_invert :: Ptr Word8 -> Ptr Word8 -> IO ()

fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- build a fastcrypto Secp256r1KeyPair from a 32-byte secret drawn
-- from the supplied RNG. errors on invalid scalar (vanishingly
-- improbable).
mkKeyPair :: C.Rng -> IO (Ptr ())
mkKeyPair g = allocaBytes 32 $ \seedPtr -> do
  F.fillRandom g seedPtr 32
  kp <- h_kp_from_bytes seedPtr
  when (kp == nullPtr) $
    error "fastcrypto keypair_from_bytes returned null"
  pure kp

-- hypotheses -----------------------------------------------------------------

-- workspace layout: 64 bytes = [a (32B) | b (32B)].
workspaceBytes :: Int
workspaceBytes = 64

aOff, bOff :: Int
aOff = 0
bOff = 32

-- positive control: the CT XOR-fold inequality witness. Class A
-- writes the same 32 bytes into both halves of the workspace
-- (target returns 0, walks all bytes); class B writes two
-- independent random 32-byte regions (target returns nonzero,
-- still walks all bytes). Timing should be identical.
posCTNeq32 :: C.Rng -> C.Rng -> F.FFIHypothesis
posCTNeq32 ga gb = F.FFIHypothesis
  { F.ffiWorkspaceBytes = workspaceBytes
  , F.ffiSampleA = \p -> do
      F.fillRandom ga (p `plusPtr` aOff) 32
      copyBytes (p `plusPtr` bOff) (p `plusPtr` aOff) 32
  , F.ffiSampleB = \p -> do
      F.fillRandom gb (p `plusPtr` aOff) 32
      F.fillRandom gb (p `plusPtr` bOff) 32
  , F.ffiTarget = \p -> do
      !_ <- c_ct_neq32 (p `plusPtr` aOff) (p `plusPtr` bOff)
      pure ()
  }

-- negative control: the short-circuiting naive variant. Class A's
-- identical halves force the full 32-byte walk and a 0 return;
-- class B's independent halves trip the early-exit on (typically)
-- the very first byte, so the call is many times faster. Censor
-- must reject this.
negNaiveNeq32 :: C.Rng -> C.Rng -> F.FFIHypothesis
negNaiveNeq32 ga gb = F.FFIHypothesis
  { F.ffiWorkspaceBytes = workspaceBytes
  , F.ffiSampleA = \p -> do
      F.fillRandom ga (p `plusPtr` aOff) 32
      copyBytes (p `plusPtr` bOff) (p `plusPtr` aOff) 32
  , F.ffiSampleB = \p -> do
      F.fillRandom gb (p `plusPtr` aOff) 32
      F.fillRandom gb (p `plusPtr` bOff) 32
  , F.ffiTarget = \p -> do
      !_ <- c_naive_neq32 (p `plusPtr` aOff) (p `plusPtr` bOff)
      pure ()
  }

-- fastcrypto secp256r1 sign. workspace layout:
--
--   [keypair_ptr (8B) | msg (32B)]   = 40B total
--
-- both classes share a single keypair built once at hypothesis
-- setup, and the leak is driven by the message alone. RFC 6979
-- makes the nonce k = F(sk, H(msg)), and the leaking
-- WindowedScalarMultiplier is indexed by k's bits — so pinning the
-- message in class A pins k, which pins the table walk; class B
-- draws a random message per pair, giving a random k and a random
-- table walk. If the class A sampler drew a fresh message the
-- distribution of leaking indices would match class B and the test
-- would have zero power by construction (a real bug we shipped in
-- an earlier version).
--
-- Sharing the keypair also eliminates the per-pair keypair Box
-- allocation the previous class B path incurred, tightening the
-- harness symmetry.
--
-- censor should reject this decisively under PMU.
negFastcryptoSign
  :: C.Rng -> C.Rng -> IO F.FFIHypothesis
negFastcryptoSign _ga gb = do
  gFix    <- C.mkRng fixedSeed
  sharedKp <- mkKeyPair gFix
  -- Build the fixed message on the heap the same way random
  -- messages are built, then freeze; using a top-level CAF would
  -- be reached by different code and could show up as an
  -- instruction-count difference under PMU.
  fixMsgPtr <- mallocBytes 32
  F.fillRandom gFix fixMsgPtr 32
  pure F.FFIHypothesis
    { F.ffiWorkspaceBytes = 8 + 32
    , F.ffiSampleA = \p -> do
        poke (castPtr p) sharedKp
        copyBytes (p `plusPtr` 8) fixMsgPtr 32
    , F.ffiSampleB = \p -> do
        poke (castPtr p) sharedKp
        F.fillRandom gb (p `plusPtr` 8) 32
    , F.ffiTarget = \p -> do
        kp <- peek (castPtr p) :: IO (Ptr ())
        h_sign kp (p `plusPtr` 8) 32
    }

-- bn254 Fr Bernstein-Yang inverter with black_box removed from
-- cond_add/cond_neg. workspace layout:
--
--   [input scalar (32B) | output buffer (32B)]   = 64B total
--
-- class A pins a fixed scalar prepared once at setup; class B
-- draws a fresh random scalar every pair. Both samplers consume
-- 32 random bytes from their RNG to keep nursery occupancy
-- symmetric -- class A then overwrites with the fixed buffer.
--
-- on aarch64 (LLVM's `csel` lowering) per-call instruction
-- counts are identical across the two classes and the e-process
-- should exhaust budget cleanly. on x86-64 LLVM collapses the
-- masked add into a secret-dependent conditional jump (the
-- `jbe LBB1_5` site documented by substratum) and per-call
-- instructions retired diverge by ~1 per invert -- small in
-- absolute terms, but deterministic, so the wealth process
-- accumulates fast under PMU.
negBYInvFr :: C.Rng -> C.Rng -> IO F.FFIHypothesis
negBYInvFr ga gb = do
  fixBuf <- mallocBytes 32
  gFix   <- C.mkRng fixedSeed
  F.fillRandom gFix fixBuf 32
  pure F.FFIHypothesis
    { F.ffiWorkspaceBytes = 64
    , F.ffiSampleA = \p -> do
        F.fillRandom ga (p `plusPtr` 0) 32     -- draw, discard for symmetry
        copyBytes (p `plusPtr` 0) fixBuf 32    -- overwrite with fixed
        F.fillRandom ga (p `plusPtr` 32) 32    -- output region: random pad
    , F.ffiSampleB = \p -> do
        F.fillRandom gb (p `plusPtr` 0) 32
        F.fillRandom gb (p `plusPtr` 32) 32
    , F.ffiTarget = \p ->
        h_byinv_fr_invert p (p `plusPtr` 32)
    }

-- driver ---------------------------------------------------------------------

cfg :: C.Config
cfg = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgBudget = 50000
  , C.cfgWarmup = 500
  , C.cfgBatch  = 1000
  }

-- a config tuned for the slower fastcrypto sign target. The
-- windowed-multiplier table-lookup leak is small in absolute
-- cycle terms (a handful of L1/L2 misses per call out of ~90k
-- total cycles, i.e. fractions of a percent), so we need both a
-- big budget and a sizeable inner-loop batch to push the wealth
-- process past the rejection threshold cleanly. batch=50
-- amortizes per-measurement scheduler noise; budget=50000 gives
-- the bettor plenty of pairs to bank.
cfgSign :: C.Config
cfgSign = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgBudget = 50000
  , C.cfgWarmup = 200
  , C.cfgBatch  = 50
  }

renderResult :: C.Result -> IO ()
renderResult r = do
  let verdict = case r of
        C.Reject{} -> "REJECT after "
        C.Pass{}   -> "PASS    after "
  putStrLn $ "  " ++ verdict ++ show (C.resPairs r)
    ++ " pairs, logE=" ++ show (C.resPeakLogW r)
    ++ ", clipped=" ++ show (C.resClipped r)
    ++ ", bound=" ++ show (C.resBound r)
  printf "    evidence: p=%.3g  peaks: sign=%.2f magn(c)=%.2f \
         \magn(4c)=%.2f magn(16c)=%.2f\n"
    (C.resPValue r)
    (C.resPeakSign r) (C.resPeakMagn r)
    (C.resPeakMagn4 r) (C.resPeakMagn16 r)
  let (elo, ehi) = C.resEffect r
  printf "    effect: mean clipped d in [%.1f, %.1f] per batch\n"
    elo ehi

renderAttribution :: C.Attribution -> String
renderAttribution att = case att of
  C.TargetLeak ->
    "A/A and B/B both PASS — target leak confirmed"
  C.SampleAAsymmetry r ->
    "A/A REJECT after " ++ show (C.resPairs r)
    ++ " — sampleA asymmetry, not a target leak"
  C.SampleBAsymmetry r ->
    "B/B REJECT after " ++ show (C.resPairs r)
    ++ " — sampleB asymmetry, not a target leak"
  C.PervasiveAsymmetry ra rb ->
    "A/A and B/B both REJECT (nA=" ++ show (C.resPairs ra)
    ++ ", nB=" ++ show (C.resPairs rb)
    ++ ") — pervasive harness asymmetry"

runCase :: C.Meter -> C.Config -> String -> F.FFIHypothesis -> IO ()
runCase meter c name fh = do
  putStrLn $ "-- " ++ name
  hFlush stdout
  result <- F.withFFIHypothesis fh $ \h -> C.runCT meter c h
  renderResult result
  case result of
    C.Reject{} -> do
      -- A/A and B/B diagnostics: each collapses one sampler onto
      -- itself and should always Pass under a symmetric harness;
      -- 'attribute' combines their verdicts.
      putStrLn "    (running A/A and B/B diagnostics to attribute the reject...)"
      hFlush stdout
      att <- F.withFFIHypothesis fh $ \h -> C.attribute meter c h
      putStrLn $ "    " ++ renderAttribution att
    C.Pass{} -> pure ()
  hFlush stdout

runAll :: C.Meter -> IO ()
runAll meter = do
  ga <- C.mkRng 0xCAFEBABE
  gb <- C.mkRng 0xDEADBEEF
  runCase meter cfg "positive: rust censor_ct_neq32 (XOR fold, 32B)"
    (posCTNeq32 ga gb)
  runCase meter cfg "negative: rust censor_naive_neq32 (short-circuit)"
    (negNaiveNeq32 ga gb)
  signHyp <- negFastcryptoSign ga gb
  runCase meter cfgSign
    "negative: fastcrypto::secp256r1 sign (WindowedScalarMultiplier)"
    signHyp
  byHyp <- negBYInvFr ga gb
  runCase meter cfgSign
    "negative: halo2curves BYInverter::invert bn254 Fr (no black_box)"
    byHyp

-- meter selection: first CLI argument, defaulting to wall-clock.
-- The PMU meters are Linux-only; an unavailable counter is a hard
-- error rather than a silent fallback.
data MeterChoice
  = MeterWall
  | MeterPMU !C.Counter !String

parseMeter :: [String] -> Maybe MeterChoice
parseMeter args = case args of
  []               -> Just MeterWall
  ["wall"]         -> Just MeterWall
  ["instructions"] ->
    Just (MeterPMU C.InstructionsRetired "instructions_retired")
  ["branches"]     ->
    Just (MeterPMU C.BranchesRetired "branches_retired")
  ["cycles"]       ->
    Just (MeterPMU C.Cycles "cycles")
  _                -> Nothing

withMeter :: String -> MeterChoice -> (String -> C.Meter -> IO ()) -> IO ()
withMeter _ MeterWall k = k "wall-clock" C.wallClock
withMeter prog (MeterPMU counter label) k = do
  r <- try (C.withCounter counter (k label))
        :: IO (Either C.MeterError ())
  case r of
    Left e   -> die $ prog ++ ": PMU meter unavailable: " ++ show e
    Right () -> pure ()

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- Env.getArgs
  choice <- case parseMeter args of
    Just m  -> pure m
    Nothing -> die
      "usage: censor-demo-ffi [wall|instructions|branches|cycles]"
  putStrLn "ppad-censor demo-ffi: dudect-style fix-vs-random \
           \against rust 32-byte inequality witnesses, \
           \fastcrypto secp256r1 sign, and halo2curves BY \
           \inverter (black_box stripped)"
  putStrLn $ "cfg:     " ++ show cfg
  putStrLn $ "cfgSign: " ++ show cfgSign
  withMeter "censor-demo-ffi" choice $ \label meter -> do
    putStrLn $ "\nmeter: " ++ label
    runAll meter
