{-# LANGUAGE BangPatterns #-}

-- censor-poly: record censor runs of ppad-poly1305's constant-time
-- MAC comparison under a selectable meter, dumping per-pair
-- trajectories as JSON for plotting.
--
-- Usage: censor-poly [--format pretty|json|plain]
--                    [wall|instructions|cycles|branches] [OUT.json]
--
-- Everything besides the hypothesis and the case list is handled
-- by 'Censor.Runner.Report.runTraceSuite'.

module Main where

import Control.Exception (evaluate)
import qualified Censor as C
import qualified Censor.Runner.Report as R
import qualified Crypto.MAC.Poly1305 as Poly1305
import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.Word (Word64)
import System.Exit (die)

fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- Both classes compare a fresh 16-byte tag against a copy differing
-- in exactly one byte, class A flipping byte i0 and class B flipping
-- byte i1. Same terminal result (False); the classes differ only in
-- mismatch position, relative to the comparison's two 64-bit lanes
-- ([0-7] and [8-15]).
posCase :: (Int, Int) -> IO (C.Hypothesis (Poly1305.MAC, Poly1305.MAC))
posCase (i0, i1) = do
  g <- C.mkRng fixedSeed
  let flipAt i bs = BS.pack
        [ if j == i then B.xor w 0xFF else w
        | (j, w) <- zip [0 :: Int ..] (BS.unpack bs) ]
      mk i = do
        !k   <- C.randomBytes g 32
        !m   <- C.randomBytes g 256
        !raw <- case Poly1305.mac k m of
          Just (Poly1305.MAC r) -> evaluate r
          Nothing               -> die "censor-poly: poly1305 mac failed"
        !t <- evaluate (Poly1305.MAC raw)
        !u <- evaluate (Poly1305.MAC (flipAt i raw))
        pure (t, u)
  pure C.Hypothesis
    { C.target  = \(!a, !b) -> () <$ evaluate (a == b)
    , C.prepare = pure ()
    , C.sampleA = mk i0
    , C.sampleB = mk i1
    }

-- Both classes compute a full MAC over inputs of fixed lengths: a
-- 32-byte key and an n-byte message. Class A holds the varied
-- input's bytes at a fixed value; class B redraws them each sample.
-- The embed copies the varied input into a fresh buffer per draw in
-- both classes, so the target always reads freshly allocated memory
-- and the classes differ only in the bytes' values, not in buffer
-- identity or cache history. Poly1305's timing may depend on the
-- (public) lengths; it must not depend on key or message contents.
--
-- Lengths in use: 245 bytes is 15 full 16-byte blocks plus a 5-byte
-- final chunk (set_hi's wide-shift arm on the Haskell path; the ARM
-- kernel's scalar blocks and padded final chunk, under its 256-byte
-- NEON threshold). 320 bytes is 20 exact blocks: on NEON-capable ARM
-- hosts the kernel's 4-way NEON path engages and consumes all of it
-- in five iterations, with no scalar tail and no partial block.
data Vary = VaryKey | VaryMsg

macCase :: Vary -> Int -> IO (C.Hypothesis (BS.ByteString, BS.ByteString))
macCase v n = do
  g <- C.mkRng fixedSeed
  !k0 <- C.randomBytes g 32
  !m0 <- C.randomBytes g n
  let tgt (!k, !m) = case Poly1305.mac k m of
        Just (Poly1305.MAC r) -> () <$ evaluate r
        Nothing               -> die "censor-poly: poly1305 mac failed"
  case v of
    VaryKey ->
      C.fixVsRandom tgt (C.randomBytes g 32)
        (\k -> evaluate (BS.copy k))
        (\k -> do !k' <- evaluate (BS.copy k); pure (k', m0))
    VaryMsg ->
      C.fixVsRandom tgt (C.randomBytes g n)
        (\m -> evaluate (BS.copy m))
        (\m -> do !m' <- evaluate (BS.copy m); pure (k0, m'))

main :: IO ()
main = R.runTraceSuite R.TraceSuite
  { R.tsTarget = "ppad-poly1305 MAC == (mismatch-position independence)"
  , R.tsConfig = C.defaultConfig
      { C.cfgAlpha  = 1.0e-6
      , C.cfgWarmup = 500
      , C.cfgBudget = 50000
      }
  , R.tsSeed   = Just fixedSeed
  , R.tsNote   = Just "pure targets: batch=1 (no inner repetition)"
  , R.tsCases  =
      [ R.TraceCase "H0 control @0 vs @0" R.SingleShot  (posCase (0, 0))
      , R.TraceCase "same-lane @0 vs @7" R.SingleShot   (posCase (0, 7))
      , R.TraceCase "cross-lane @0 vs @15" R.SingleShot (posCase (0, 15))
      , R.TraceCase "mac key fix-vs-random" R.SingleShot
          (macCase VaryKey 245)
      , R.TraceCase "mac msg fix-vs-random" R.SingleShot
          (macCase VaryMsg 245)
      , R.TraceCase "mac key fix-vs-random 320B" R.SingleShot
          (macCase VaryKey 320)
      ]
  }
