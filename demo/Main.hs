{-# LANGUAGE BangPatterns #-}

-- censor-demo: exercise the sequential CT tester against a few
-- targets drawn from ppad-fixed, ppad-secp256k1, ppad-aead,
-- ppad-sha256, ppad-sha512, and ppad-bip32. We adopt the
-- dudect-style "fix vs random" protocol: class A pins one secret
-- input, class B supplies fresh pseudorandom secrets. A
-- timing-leaky target should produce a measurable distributional
-- difference between classes; a CT target should not.
--
-- Most hypotheses are built with Censor's 'fixVsRandom', which
-- encodes the measurement-hygiene rules (the fixed input is a heap
-- value built by the same code path as the random class, and class
-- A draws and discards a fresh secret per pair so the samplers stay
-- symmetric in allocation count, size, and order). The samplers
-- here uphold their side of the contract by returning inputs in
-- normal form. The MAC-equality case (equal-vs-unequal tags rather
-- than fix-vs-random) and the deliberately leaky negative controls
-- (adversarial all-zero fixed inputs) are wired by hand.
--
-- The meter is chosen by the first command-line argument (wall,
-- instructions, branches, or cycles), defaulting to wall-clock.
-- The PMU meters are Linux-only; asking for one elsewhere is a
-- hard error rather than a silent fallback.

module Main where

import Control.Exception (evaluate, try)
import Control.Monad (replicateM, replicateM_)
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32, Word64)
import qualified System.Environment as Env
import System.Exit (die)
import System.IO (hFlush, hSetBuffering, stdout, BufferMode(..))
import Text.Printf (printf)

import qualified Censor as C
import qualified Crypto.AEAD.ChaCha20Poly1305 as AEAD
import qualified Crypto.Curve.Secp256k1 as Secp
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.HDKey.BIP32 as BIP32
import qualified Data.Word.Wider as W
import qualified Numeric.Montgomery.Secp256k1.Scalar as MS
import Data.Word.Wider (Wider)

-- samplers --------------------------------------------------------------------

randomWider :: C.Rng -> IO Wider
randomWider g = do
  !a <- C.nextWord g
  !b <- C.nextWord g
  !c <- C.nextWord g
  !d <- C.nextWord g
  pure $! W.wider (fromIntegral a) (fromIntegral b)
                  (fromIntegral c) (fromIntegral d)

-- a random Montgomery value, forced to normal form: the to# conversion
-- is performed here, not left as a thunk for the meter to pay inside
-- the timed region.
randomMont :: C.Rng -> IO MS.Montgomery
randomMont g = do
  !w <- randomWider g
  evaluate (MS.to w)

-- a random n-byte ByteString in NF. used to build keys, nonces,
-- messages, AAD, plaintexts for the cryptographic hypotheses.
-- constructed at sample time, outside the timed region; replicateM
-- is justified -- it builds a fixed-length list of pre-forced
-- Word64s, immediately flattened into a strict ByteString that
-- 'evaluate' fully forces.
randomBytes :: C.Rng -> Int -> IO BS.ByteString
randomBytes g n = do
  let !nws = (n + 7) `quot` 8
  ws <- replicateM nws (C.nextWord g)
  evaluate $! BS.take n $ BS.pack
    [ fromIntegral (w `B.shiftR` (8 * i)) :: Word8
    | w <- ws, i <- [0 .. 7 :: Int]
    ]

random32 :: C.Rng -> IO BS.ByteString
random32 g = randomBytes g 32

random12 :: C.Rng -> IO BS.ByteString
random12 g = randomBytes g 12

-- every hypothesis seeds its own generator with this, so the pinned
-- secret (the stream's first draw) is reproducible across runs.
fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- hypotheses -----------------------------------------------------------------

-- positive control: Montgomery scalar multiplication.
-- fully-unrolled limb arithmetic; should be CT.
--
-- the pinned secret is the raw Wider; the completion converts it
-- to Montgomery form per pair, so both classes' operands are
-- nursery-fresh allocations built by identical code. Pinning the
-- Montgomery object itself would give class A a long-lived operand
-- and class B a fresh one -- a heap-locality asymmetry (not a
-- property of mul) that the sign component resolves at nanosecond
-- scale under batch=2000.
posScalarMul :: IO (C.Hypothesis (MS.Montgomery, MS.Montgomery))
posScalarMul = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!x, !y) -> do
        !_ <- evaluate (MS.mul x y)
        pure ())
    (randomWider g)
    (\w -> do
        !x <- evaluate (MS.to w)
        !y <- randomMont g
        pure (x, y))

-- positive control: Montgomery modular inversion (addition chain).
-- 256 squarings + a fixed number of multiplications; should be CT.
posScalarInv :: IO (C.Hypothesis MS.Montgomery)
posScalarInv = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\x -> do
        !_ <- evaluate (MS.inv x)
        pure ())
    (randomMont g)
    pure

-- positive control: Montgomery exponentiation (constant-iteration
-- ladder driven by select#). Should be CT in the exponent.
posScalarExp :: IO (C.Hypothesis (MS.Montgomery, Wider))
posScalarExp = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!b, !e) -> do
        !_ <- evaluate (MS.exp b e)
        pure ())
    (randomWider g)
    (\e -> do
        !b <- randomMont g
        pure (b, e))

-- positive control: wNAF scalar multiplication of the secp256k1
-- generator. The CT property of interest is that the timing of
-- @mul_wnaf tex s@ should not depend on the scalar @s@.
posMulWNAF :: Secp.Context -> IO (C.Hypothesis Wider)
posMulWNAF tex = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\s -> do
        !_ <- evaluate (Secp.mul_wnaf tex s)
        pure ())
    (randomWider g)
    pure

-- positive control: BIP0340 Schnorr signing under a precomputed
-- context. The secret-key path runs through wNAF mul; messages and
-- aux randomness are public, so a leak (if any) shows up as
-- fix-vs-random on the secret.
posSignSchnorr
  :: Secp.Context
  -> IO (C.Hypothesis (Wider, BS.ByteString, BS.ByteString))
posSignSchnorr tex = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!sec, !msg, !aux) -> do
        !_ <- evaluate (Secp.sign_schnorr' tex sec msg aux)
        pure ())
    (randomWider g)
    (\sec -> do
        !msg <- random32 g
        !aux <- random32 g
        pure (sec, msg, aux))

-- positive control: RFC6979 ECDSA signing under a precomputed
-- context. RFC6979 derives the nonce deterministically from the
-- secret + message, then signs via wNAF mul.
posSignECDSA
  :: Secp.Context -> IO (C.Hypothesis (Wider, BS.ByteString))
posSignECDSA tex = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!sec, !msg) -> do
        !_ <- evaluate (Secp.sign_ecdsa' tex sec msg)
        pure ())
    (randomWider g)
    (\sec -> do
        !msg <- random32 g
        pure (sec, msg))

-- positive control: AEAD-ChaCha20-Poly1305 encrypt. The secret is
-- the 256-bit key; AAD, nonce, plaintext are public per call. Both
-- ChaCha20 keystream generation and Poly1305 MAC computation have
-- key-independent control flow; CT should hold.
posAEADEncrypt
  :: IO (C.Hypothesis
           (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString))
posAEADEncrypt = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!key, !nonce, !aad, !pt) -> do
        r <- evaluate (AEAD.encrypt aad key nonce pt)
        case r of
          Right (!_, !_) -> pure ()
          Left _         -> pure ())
    (random32 g)
    (\key -> do
        !nonce <- random12 g
        !aad   <- random32 g
        !pt    <- randomBytes g 256
        pure (key, nonce, aad, pt))

-- positive control: AEAD-ChaCha20-Poly1305 decrypt on a valid
-- ciphertext + MAC. The (cip, mac) pair is pre-computed at sample
-- time using the chosen key, so the timed region exercises the
-- full success path: keystream regeneration, Poly1305 recompute,
-- and the ct_eq tag comparison. CT in the key should hold.
posAEADDecrypt
  :: IO (C.Hypothesis
           ( BS.ByteString, BS.ByteString, BS.ByteString
           , BS.ByteString, BS.ByteString ))
posAEADDecrypt = do
  g <- C.mkRng fixedSeed
  let mkSample !key = do
        !nonce <- random12 g
        !aad   <- random32 g
        !pt    <- randomBytes g 256
        case AEAD.encrypt aad key nonce pt of
          Left _ -> error
            "censor-demo: AEAD encrypt failed during sample setup"
          Right (cip, mac) -> do
            !c <- evaluate cip
            !m <- evaluate mac
            pure (key, nonce, aad, c, m)
  C.fixVsRandom
    (\(!key, !nonce, !aad, !cip, !mac) -> do
        r <- evaluate (AEAD.decrypt aad key nonce (cip, mac))
        case r of
          Right !_ -> pure ()
          Left _   -> pure ())
    (random32 g)
    mkSample

-- positive control: HMAC-SHA256 with a secret key. The inner+outer
-- block transformations are key-independent in control flow; the
-- only length-dependent work is in the message, which we hold
-- constant across pairs.
posHMACSHA256 :: IO (C.Hypothesis (BS.ByteString, BS.ByteString))
posHMACSHA256 = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!key, !msg) -> do
        !_ <- evaluate (SHA256.hmac key msg)
        pure ())
    (random32 g)
    (\key -> do
        !msg <- randomBytes g 256
        pure (key, msg))

-- positive control: HMAC-SHA512 with a secret key. Same structure
-- as HMAC-SHA256.
posHMACSHA512 :: IO (C.Hypothesis (BS.ByteString, BS.ByteString))
posHMACSHA512 = do
  g <- C.mkRng fixedSeed
  C.fixVsRandom
    (\(!key, !msg) -> do
        !_ <- evaluate (SHA512.hmac key msg)
        pure ())
    (random32 g)
    (\key -> do
        !msg <- randomBytes g 256
        pure (key, msg))

-- positive control: constant-time MAC equality. The == on 'MAC'
-- walks all 32 bytes regardless of where (or whether) a mismatch
-- occurs. Class A compares two byte-identical tags (full walk,
-- returns True); class B compares two independently-derived tags
-- (full walk, returns False with overwhelming probability). A
-- non-CT implementation would early-exit on B and be measurably
-- faster. Equal-vs-unequal rather than fix-vs-random, so wired by
-- hand.
posMACEq :: IO (C.Hypothesis (SHA256.MAC, SHA256.MAC))
posMACEq = do
  g <- C.mkRng fixedSeed
  pure C.Hypothesis
    { C.target = \(!a, !b) -> do
        !_ <- evaluate (a == b)
        pure ()
    , C.sampleA = do
        -- A: two byte-equal but structurally distinct tags, built
        -- by hmac'ing the same key/message twice in the same
        -- draw/hmac order as class B (the second key/message draw
        -- is discarded). '==' walks two fresh buffers, as in class
        -- B, and returns True. Returning the *same object* twice
        -- would give class A a one-buffer cache footprint -- a
        -- same-object-vs-distinct-objects asymmetry, rather than
        -- the equal-vs-unequal walk under test.
        !k    <- random32 g
        !m    <- randomBytes g 256
        !tagX <- evaluate $! SHA256.hmac k m
        !_    <- random32 g
        !_    <- randomBytes g 256
        !tagY <- evaluate $! SHA256.hmac k m
        pure (tagX, tagY)
    , C.sampleB = do
        -- B: build two independent tags; identical only with
        -- negligible probability, so '==' walks all bytes and
        -- returns False.
        !k1   <- random32 g
        !m1   <- randomBytes g 256
        !tag1 <- evaluate $! SHA256.hmac k1 m1
        !k2   <- random32 g
        !m2   <- randomBytes g 256
        !tag2 <- evaluate $! SHA256.hmac k2 m2
        pure (tag1, tag2)
    }

-- positive control: BIP32 child key derivation. The secret is the
-- parent xpriv. Derivation runs HMAC-SHA512 keyed on the chain
-- code with the parent key + child index as data, then a scalar
-- add on the secp256k1 group order. CT in the parent secret.
posBIP32Derive :: IO (C.Hypothesis (BIP32.HDKey, Word32))
posBIP32Derive = do
  g <- C.mkRng fixedSeed
  let sampleHD = do
        !seed <- randomBytes g 64
        case BIP32.master seed of
          Just h  -> evaluate h
          Nothing -> error
            "censor-demo: BIP32 master failed on random seed"
  C.fixVsRandom
    (\(!hd, !i) -> do
        !_ <- evaluate (BIP32.derive_child_priv hd i)
        pure ())
    sampleHD
    (\hd -> do
        !w <- C.nextWord g
        let !i = fromIntegral w :: Word32
        pure (hd, i))

-- negative control: a deliberately leaky target whose work scales
-- with the popcount of the value. A CT tester worth its salt must
-- reject this. The fixed input is adversarially all-zero (maximum
-- popcount contrast), so it is wired by hand rather than drawn from
-- the sampler.
negLeakyPopcount :: IO (C.Hypothesis Wider)
negLeakyPopcount = do
  g <- C.mkRng fixedSeed
  !fixZero <- evaluate (W.wider 0 0 0 0)
  pure C.Hypothesis
    { C.target  = \(!w) -> do
        let !n = B.popCount (W.from_vartime w :: Integer)
        replicateM_ (10 * n) (evaluate ())
    , C.sampleA = pure fixZero
    , C.sampleB = randomWider g
    }

-- negative control: Wider -> Integer conversion is by name
-- variable-time. Class A always converts zero, class B random.
negFromVartime :: IO (C.Hypothesis Wider)
negFromVartime = do
  g <- C.mkRng fixedSeed
  !fixZero <- evaluate (W.wider 0 0 0 0)
  pure C.Hypothesis
    { C.target  = \(!w) -> do
        !_ <- evaluate (W.from_vartime w :: Integer)
        pure ()
    , C.sampleA = pure fixZero
    , C.sampleB = randomWider g
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

-- Nanosecond-scale targets (Scalar.mul, the MAC comparison) are fast
-- enough that wall-clock readings on macOS (~42ns quantum) are
-- dominated by per-call cache and allocator asymmetries between the
-- two classes rather than the target itself. A larger inner-loop
-- batch amortizes those per-call asymmetries across enough calls
-- that they average out. Irrelevant under the deterministic PMU
-- meter, but harmless there.
cfgBigBatch :: C.Config
cfgBigBatch = cfg { C.cfgBatch = 2000 }

-- Secp targets are far slower per call than the Montgomery scalar
-- arithmetic (microseconds rather than nanoseconds), so a per-call
-- batch of 1 is already well above any wall-clock quantum and keeps
-- total run-time modest.
cfgHeavy :: C.Config
cfgHeavy = cfg { C.cfgBatch = 1 }

renderResult :: C.Result -> IO ()
renderResult r = do
  let verdict = case r of
        C.Reject{} -> "REJECT after "
        C.Pass{}   -> "PASS    after "
  putStrLn $ "  " ++ verdict ++ show (C.resPairs r)
    ++ " pairs, logW=" ++ show (C.resPeakLogW r)
    ++ ", clipped=" ++ show (C.resClipped r)
    ++ ", bound=" ++ show (C.resBound r)
  printf "    peaks: sign=%.2f magn(c)=%.2f magn(4c)=%.2f \
         \magn(16c)=%.2f\n"
    (C.resPeakSign r) (C.resPeakMagn r)
    (C.resPeakMagn4 r) (C.resPeakMagn16 r)

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

runCaseWith :: C.Meter -> C.Config -> String -> IO (C.Hypothesis a) -> IO ()
runCaseWith meter c name mkH = do
  putStrLn $ "-- " ++ name
  hFlush stdout
  h <- mkH
  r <- C.runCT meter c h
  renderResult r
  case r of
    C.Reject{} -> do
      -- A/A and B/B diagnostics: each collapses one sampler onto
      -- itself and should always Pass under a symmetric harness;
      -- 'attribute' combines their verdicts.
      putStrLn "    (running A/A and B/B diagnostics to attribute the reject...)"
      hFlush stdout
      hD <- mkH
      att <- C.attribute meter c hD
      putStrLn $ "    " ++ renderAttribution att
    C.Pass{} -> pure ()
  hFlush stdout

-- run every case under the given meter.
runAll :: Secp.Context -> C.Meter -> IO ()
runAll tex meter = do
  let rc = runCaseWith meter cfg
      rh = runCaseWith meter cfgHeavy
  runCaseWith meter cfgBigBatch
     "positive: Scalar.mul (single multiply, batch=2000)"  posScalarMul
  rc "positive: Scalar.inv (addition chain)"               posScalarInv
  rc "positive: Scalar.exp (constant-iteration ladder)"    posScalarExp
  rh "positive: Secp.mul_wnaf (wNAF point multiplication)" (posMulWNAF tex)
  rh "positive: Secp.sign_schnorr' (BIP0340)"              (posSignSchnorr tex)
  rh "positive: Secp.sign_ecdsa' (RFC6979)"                (posSignECDSA tex)
  rc "positive: AEAD.encrypt (ChaCha20-Poly1305, 256 B)"   posAEADEncrypt
  rc "positive: AEAD.decrypt (ChaCha20-Poly1305, 256 B)"   posAEADDecrypt
  rh "positive: SHA256.hmac (256 B msg, secret key)"       posHMACSHA256
  rh "positive: SHA512.hmac (256 B msg, secret key)"       posHMACSHA512
  runCaseWith meter cfgBigBatch
     "positive: MAC == (constant-time tag compare, batch=2000)" posMACEq
  rh "positive: BIP32 derive_child_priv (secret parent)"   posBIP32Derive
  rc "negative: deliberately leaky popcount-driven loop"   negLeakyPopcount
  rc "negative: Wider.from_vartime (Integer conversion)"   negFromVartime

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
    Nothing -> die "usage: censor-demo [wall|instructions|branches|cycles]"
  putStrLn "ppad-censor demo: dudect-style fix-vs-random against \
           \ppad-fixed, ppad-secp256k1, ppad-aead, ppad-sha256, \
           \ppad-sha512, and ppad-bip32"
  putStrLn $ "config: " ++ show cfg
  -- one-shot secp256k1 precomputation, shared across all secp targets.
  let !tex = Secp.precompute
  withMeter "censor-demo" choice $ \label meter -> do
    putStrLn $ "\nmeter: " ++ label
    runAll tex meter
