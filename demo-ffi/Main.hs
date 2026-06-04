{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- censor-demo-ffi: exercise the FFI bridge against a small Rust
-- library exposing two 32-byte inequality witnesses -- one
-- constant-time and one deliberately leaky. Both consume the same
-- 64-byte workspace layout:
--
--   workspace = [a (32B) | b (32B)]
--
-- and the same fix-vs-random protocol is applied: class A places
-- two byte-identical 32-byte regions in the workspace; class B
-- places two independent random 32-byte regions. The CT primitive
-- walks all bytes either way; the naive primitive short-circuits
-- on the first mismatch and is detectably faster on class B.

module Main where

import Control.Exception (try)
import qualified Data.Bits as B
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
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

foreign import ccall unsafe "censor_ct_neq32"
  c_ct_neq32    :: Ptr Word8 -> Ptr Word8 -> IO Word8

foreign import ccall unsafe "censor_naive_neq32"
  c_naive_neq32 :: Ptr Word8 -> Ptr Word8 -> IO Word8

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

-- driver ---------------------------------------------------------------------

cfg :: C.Config
cfg = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgBudget = 50000
  , C.cfgWarmup = 500
  , C.cfgBatch  = 1000
  }

runCase :: C.Meter -> String -> F.FFIHypothesis -> IO ()
runCase meter name fh = do
  putStrLn $ "-- " ++ name
  hFlush stdout
  result <- F.withFFIHypothesis fh $ \h -> C.runCT meter cfg h
  case result of
    C.Reject n w -> putStrLn $
      "  REJECT after " ++ show n ++ " pairs, logW=" ++ show w
    C.Pass   n w -> putStrLn $
      "  PASS    after " ++ show n ++ " pairs, logW=" ++ show w
  hFlush stdout

runAll :: C.Meter -> IO ()
runAll meter = do
  ga <- mkRng 0xCAFEBABE
  gb <- mkRng 0xDEADBEEF
  runCase meter "positive: rust censor_ct_neq32 (XOR fold, 32B)"
    (posCTNeq32 ga gb)
  runCase meter "negative: rust censor_naive_neq32 (short-circuit)"
    (negNaiveNeq32 ga gb)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "ppad-censor demo-ffi: dudect-style fix-vs-random \
           \against rust constant-time and naive 32-byte inequality"
  putStrLn $ "config: " ++ show cfg
  -- Try the deterministic instructions_retired PMU meter; fall
  -- back to wall-clock on non-Linux or when perf_event_paranoid
  -- blocks unprivileged access.
  ri <- try (C.withCounter C.InstructionsRetired $ \meter -> do
               putStrLn "\nmeter: instructions_retired"
               runAll meter) :: IO (Either C.MeterError ())
  case ri of
    Left err -> do
      putStrLn $ "meter: wall-clock (PMU unavailable: "
              ++ show err ++ ")\n"
      runAll C.wallClock
    Right () -> pure ()
