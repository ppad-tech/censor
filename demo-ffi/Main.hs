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

module Main where

import Control.Exception (try)
import Control.Monad (when)
import qualified Data.Bits as B
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Alloc (allocaBytes, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, pokeByteOff)
import System.IO (hFlush, hSetBuffering, stdout, BufferMode(..))

import qualified Censor as C
import qualified Censor.FFI as F

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

-- write n random bytes from the RNG into a destination pointer.
fillRandom :: Rng -> Ptr Word8 -> Int -> IO ()
fillRandom !g !dst !n = go 0
  where
    go !i
      | i >= n    = pure ()
      | otherwise = do
          !w <- nextWord g
          let !rem = n - i
              !lim = if rem > 8 then 8 else rem
          writeWord dst i w lim
          go (i + lim)

writeWord :: Ptr Word8 -> Int -> Word64 -> Int -> IO ()
writeWord !dst !off !w !lim = go 0
  where
    go !j
      | j >= lim  = pure ()
      | otherwise = do
          let !byte = fromIntegral (w `B.shiftR` (8 * j)) :: Word8
          pokeByteOff dst (off + j) byte
          go (j + 1)

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
mkKeyPair :: Rng -> IO (Ptr ())
mkKeyPair g = allocaBytes 32 $ \seedPtr -> do
  fillRandom g seedPtr 32
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
posCTNeq32 :: Rng -> Rng -> F.FFIHypothesis
posCTNeq32 ga gb = F.FFIHypothesis
  { F.ffiWorkspaceBytes = workspaceBytes
  , F.ffiSampleA = \p -> do
      fillRandom ga (p `plusPtr` aOff) 32
      copyBytes (p `plusPtr` bOff) (p `plusPtr` aOff) 32
  , F.ffiSampleB = \p -> do
      fillRandom gb (p `plusPtr` aOff) 32
      fillRandom gb (p `plusPtr` bOff) 32
  , F.ffiTarget = \p -> do
      !_ <- c_ct_neq32 (p `plusPtr` aOff) (p `plusPtr` bOff)
      pure ()
  }

-- negative control: the short-circuiting naive variant. Class A's
-- identical halves force the full 32-byte walk and a 0 return;
-- class B's independent halves trip the early-exit on (typically)
-- the very first byte, so the call is many times faster. Censor
-- must reject this.
negNaiveNeq32 :: Rng -> Rng -> F.FFIHypothesis
negNaiveNeq32 ga gb = F.FFIHypothesis
  { F.ffiWorkspaceBytes = workspaceBytes
  , F.ffiSampleA = \p -> do
      fillRandom ga (p `plusPtr` aOff) 32
      copyBytes (p `plusPtr` bOff) (p `plusPtr` aOff) 32
  , F.ffiSampleB = \p -> do
      fillRandom gb (p `plusPtr` aOff) 32
      fillRandom gb (p `plusPtr` bOff) 32
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
  :: Rng -> Rng -> IO F.FFIHypothesis
negFastcryptoSign _ga gb = do
  gFix    <- mkRng fixedSeed
  sharedKp <- mkKeyPair gFix
  -- Build the fixed message on the heap the same way random
  -- messages are built, then freeze; using a top-level CAF would
  -- be reached by different code and could show up as an
  -- instruction-count difference under PMU.
  fixMsgPtr <- mallocBytes 32
  fillRandom gFix fixMsgPtr 32
  pure F.FFIHypothesis
    { F.ffiWorkspaceBytes = 8 + 32
    , F.ffiSampleA = \p -> do
        poke (castPtr p) sharedKp
        copyBytes (p `plusPtr` 8) fixMsgPtr 32
    , F.ffiSampleB = \p -> do
        poke (castPtr p) sharedKp
        fillRandom gb (p `plusPtr` 8) 32
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
negBYInvFr :: Rng -> Rng -> IO F.FFIHypothesis
negBYInvFr ga gb = do
  fixBuf <- mallocBytes 32
  gFix   <- mkRng fixedSeed
  fillRandom gFix fixBuf 32
  pure F.FFIHypothesis
    { F.ffiWorkspaceBytes = 64
    , F.ffiSampleA = \p -> do
        fillRandom ga (p `plusPtr` 0) 32       -- draw, discard for symmetry
        copyBytes (p `plusPtr` 0) fixBuf 32    -- overwrite with fixed
        fillRandom ga (p `plusPtr` 32) 32      -- output region: random pad
    , F.ffiSampleB = \p -> do
        fillRandom gb (p `plusPtr` 0) 32
        fillRandom gb (p `plusPtr` 32) 32
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

runCase :: C.Meter -> C.Config -> String -> F.FFIHypothesis -> IO ()
runCase meter c name fh = do
  putStrLn $ "-- " ++ name
  hFlush stdout
  result <- F.withFFIHypothesis fh $ \h -> C.runCT meter c h
  case result of
    C.Reject n w nc -> do
      putStrLn $
        "  REJECT after " ++ show n ++ " pairs, logW=" ++ show w
        ++ ", clipped=" ++ show nc
      -- A/A and B/B diagnostics: each collapses one sampler onto
      -- itself and should always Pass under a symmetric harness. A
      -- Reject on either indicts an asymmetry specific to that
      -- sampler rather than a target leak. Running both attributes
      -- the reject to (a) target, (b) sampleA-specific harness bug,
      -- or (c) sampleB-specific harness bug.
      putStrLn "    (running A/A and B/B diagnostics to attribute the reject...)"
      hFlush stdout
      rAA <- F.withFFIHypothesis fh $ \h -> C.runAA meter c h
      rBB <- F.withFFIHypothesis fh $ \h -> C.runBB meter c h
      case (rAA, rBB) of
        (C.Pass{}, C.Pass{}) -> putStrLn
          "    A/A and B/B both PASS — target leak confirmed"
        (C.Reject nA _ _, C.Pass{}) -> putStrLn $
          "    A/A REJECT after " ++ show nA
          ++ " — sampleA asymmetry, not a target leak"
        (C.Pass{}, C.Reject nB _ _) -> putStrLn $
          "    B/B REJECT after " ++ show nB
          ++ " — sampleB asymmetry, not a target leak"
        (C.Reject nA _ _, C.Reject nB _ _) -> putStrLn $
          "    A/A and B/B both REJECT (nA=" ++ show nA
          ++ ", nB=" ++ show nB ++ ") — pervasive harness asymmetry"
    C.Pass   n w nc -> putStrLn $
      "  PASS    after " ++ show n ++ " pairs, logW=" ++ show w
      ++ ", clipped=" ++ show nc
  hFlush stdout

runAll :: C.Meter -> IO ()
runAll meter = do
  ga <- mkRng 0xCAFEBABE
  gb <- mkRng 0xDEADBEEF
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "ppad-censor demo-ffi: dudect-style fix-vs-random \
           \against rust 32-byte inequality witnesses, \
           \fastcrypto secp256r1 sign, and halo2curves BY \
           \inverter (black_box stripped)"
  putStrLn $ "cfg:     " ++ show cfg
  putStrLn $ "cfgSign: " ++ show cfgSign
  -- Try each PMU meter in turn. instructions_retired catches
  -- control-flow leaks (different code paths retire different
  -- counts) but is blind to data-dependent latency from cache
  -- misses, so it can pass a target that nonetheless leaks via
  -- table-indexed loads. cycles counts stall cycles too and is
  -- the right meter for table-lookup-style leaks. Fall back to
  -- wall-clock only if perf_event_open is unavailable (non-Linux,
  -- or kernel.perf_event_paranoid > 2).
  ri <- tryMeter C.InstructionsRetired "instructions_retired"
  rc <- tryMeter C.Cycles              "cycles"
  case (ri, rc) of
    (Left err, Left _) -> do
      putStrLn $ "meter: wall-clock (PMU unavailable: "
              ++ show err ++ ")\n"
      runAll C.wallClock
    _ -> pure ()
  where
    tryMeter counter label =
      try (C.withCounter counter $ \meter -> do
             putStrLn $ "\nmeter: " ++ label
             runAll meter) :: IO (Either C.MeterError ())
