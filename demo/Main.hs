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
-- Usage: censor-demo [--format pretty|json|plain]
--                    [wall|instructions|cycles|branches]
--
-- The meter is chosen by the first positional argument, defaulting
-- to wall-clock. The PMU meters are Linux-only; asking for one
-- elsewhere is a hard error rather than a silent fallback.

module Main where

import Control.Exception (evaluate)
import Control.Monad (replicateM_)
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)

import qualified Censor as C
import qualified Censor.Runner.Report as R
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

random32 :: C.Rng -> IO BS.ByteString
random32 g = C.randomBytes g 32

random12 :: C.Rng -> IO BS.ByteString
random12 g = C.randomBytes g 12

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
        !pt    <- C.randomBytes g 256
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
        !pt    <- C.randomBytes g 256
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
        !msg <- C.randomBytes g 256
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
        !msg <- C.randomBytes g 256
        pure (key, msg))

-- positive control: constant-time MAC equality. The == on 'MAC'
-- walks all 32 bytes regardless of where (or whether) a mismatch
-- occurs. Class A compares two byte-identical tags (full walk,
-- returns True); class B compares two independently-derived tags
-- (full walk, returns False with overwhelming probability). A
-- non-CT implementation would early-exit on B and be measurably
-- faster. Equal-vs-unequal rather than fix-vs-random, so wired by
-- hand.
--
-- Note that the comparison /result/ here correlates exactly with
-- the class label, and materialising it as a Bool ends in a branch
-- whose direction is that result -- so under wall clock this case
-- typically REJECTs on the batched taken-vs-not-taken front-end
-- asymmetry of a perfectly-predicted branch. That branch reveals
-- only the accept/reject bit, which MAC verification makes public
-- by protocol; it is not an exploitable leak, and no Bool-returning
-- equality can avoid it. Expect PASS under the PMU instruction and
-- branch meters (both directions retire identical counts). The
-- mismatch-position case below tests the property that early-exit
-- comparisons actually violate.
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
        !m    <- C.randomBytes g 256
        !tagX <- evaluate $! SHA256.hmac k m
        !_    <- random32 g
        !_    <- C.randomBytes g 256
        !tagY <- evaluate $! SHA256.hmac k m
        pure (tagX, tagY)
    , C.sampleB = do
        -- B: build two independent tags; identical only with
        -- negligible probability, so '==' walks all bytes and
        -- returns False.
        !k1   <- random32 g
        !m1   <- C.randomBytes g 256
        !tag1 <- evaluate $! SHA256.hmac k1 m1
        !k2   <- random32 g
        !m2   <- C.randomBytes g 256
        !tag2 <- evaluate $! SHA256.hmac k2 m2
        pure (tag1, tag2)
    }

-- positive control: MAC equality, mismatch-position independence.
-- Both classes compare unequal tags (same terminal result, False),
-- differing only in where the single mismatching byte sits: first
-- byte (class A) vs last byte (class B). An early-exit comparison
-- would be ~32x faster on class A; the full-walk XOR fold should
-- show no position dependence. Unlike the equal-vs-unequal case,
-- the (public) comparison result does not correlate with the class
-- label here, so this directly tests the security-relevant
-- property.
posMACPos :: IO (C.Hypothesis (SHA256.MAC, SHA256.MAC))
posMACPos = do
  g <- C.mkRng fixedSeed
  let flipAt i bs = BS.pack
        [ if j == i then B.xor w 0xFF else w
        | (j, w) <- zip [0 :: Int ..] (BS.unpack bs) ]
      mk i = do
        !k <- random32 g
        !m <- C.randomBytes g 256
        let SHA256.MAC raw = SHA256.hmac k m
        !t <- evaluate (SHA256.MAC raw)
        !u <- evaluate (SHA256.MAC (flipAt i raw))
        pure (t, u)
  pure C.Hypothesis
    { C.target  = \(!a, !b) -> do
        !_ <- evaluate (a == b)
        pure ()
    , C.sampleA = mk 0
    , C.sampleB = mk 31
    }

-- positive control: BIP32 child key derivation. The secret is the
-- parent xpriv. Derivation runs HMAC-SHA512 keyed on the chain
-- code with the parent key + child index as data, then a scalar
-- add on the secp256k1 group order. CT in the parent secret.
posBIP32Derive :: IO (C.Hypothesis (BIP32.HDKey, Word32))
posBIP32Derive = do
  g <- C.mkRng fixedSeed
  let sampleHD = do
        !seed <- C.randomBytes g 64
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

-- Run one case: execute the hypothesis, and on 'Reject' re-run the
-- A/A + B/B attribution to distinguish target leaks from harness
-- asymmetries.
runCase
  :: (R.CaseReport -> IO ())
  -> C.Meter -> C.Config -> String -> IO (C.Hypothesis a) -> IO ()
runCase emit meter c name mkH = do
  h <- mkH
  r <- C.runCT meter c h
  mat <- case r of
    C.Reject{} -> do
      hD <- mkH
      Just <$> C.attribute meter c hD
    C.Pass{}   -> pure Nothing
  emit (R.CaseReport name r mat)

-- run every case under the given meter.
runAll
  :: Secp.Context -> C.Meter -> (R.CaseReport -> IO ()) -> IO ()
runAll tex meter emit = do
  let rc = runCase emit meter cfg
      rh = runCase emit meter cfgHeavy
      rb = runCase emit meter cfgBigBatch
  rb "positive: Scalar.mul (single multiply, batch=2000)"        posScalarMul
  rc "positive: Scalar.inv (addition chain)"                     posScalarInv
  rc "positive: Scalar.exp (constant-iteration ladder)"          posScalarExp
  rh "positive: Secp.mul_wnaf (wNAF point multiplication)"       (posMulWNAF tex)
  rh "positive: Secp.sign_schnorr' (BIP0340)"                    (posSignSchnorr tex)
  rh "positive: Secp.sign_ecdsa' (RFC6979)"                      (posSignECDSA tex)
  rc "positive: AEAD.encrypt (ChaCha20-Poly1305, 256 B)"         posAEADEncrypt
  rc "positive: AEAD.decrypt (ChaCha20-Poly1305, 256 B)"         posAEADDecrypt
  rh "positive: SHA256.hmac (256 B msg, secret key)"             posHMACSHA256
  rh "positive: SHA512.hmac (256 B msg, secret key)"             posHMACSHA512
  rb "positive: MAC == (constant-time tag compare, batch=2000)"  posMACEq
  rb "positive: MAC == (mismatch@0 vs mismatch@31, batch=2000)"  posMACPos
  rh "positive: BIP32 derive_child_priv (secret parent)"         posBIP32Derive
  rc "negative: deliberately leaky popcount-driven loop"         negLeakyPopcount
  rc "negative: Wider.from_vartime (Integer conversion)"         negFromVartime

main :: IO ()
main = do
  let !tex = Secp.precompute
  R.runSuite R.Suite
    { R.suTarget = "dudect-style fix-vs-random against ppad-* and controls"
    , R.suConfig = cfg
    , R.suSeed   = Just fixedSeed
    , R.suNote   = Just "batch varies per case: 50 (base), \
                        \1 (secp), 2000 (nanosecond targets)"
    , R.suRun    = \_ meter emit -> runAll tex meter emit
    }
