{-# LANGUAGE BangPatterns #-}

-- censor-chacha: record censor runs of ppad-chacha's ChaCha20
-- cipher and block functions under a selectable meter, dumping
-- per-pair trajectories as JSON for plotting.
--
-- Usage: censor-chacha [--format pretty|json|plain]
--                      [wall|instructions|cycles|branches] [OUT.json]
--
-- Everything besides the hypothesis and the case list is handled
-- by 'Censor.Runner.Report.runTraceSuite'.

module Main where

import Control.Exception (evaluate)
import qualified Censor as C
import qualified Censor.Runner.Report as R
import qualified Crypto.Cipher.ChaCha20 as ChaCha20
import qualified Data.ByteString as BS
import Data.Word (Word64)
import System.Exit (die)

fixedSeed :: Word64
fixedSeed = 0x5EED5EED5EED5EED

-- Both classes encrypt a fixed n-byte plaintext under a fixed
-- 96-bit nonce and counter. Class A holds the 32-byte key at a
-- fixed value; class B redraws it each sample. The embed copies
-- the key into a fresh buffer per draw in both classes, so the
-- target always reads freshly allocated memory and the classes
-- differ only in the key bytes' values, not in buffer identity or
-- cache history. ChaCha20 is a data-oblivious ARX cipher: timing
-- may depend on the (public) plaintext length; it must not depend
-- on the key.
--
-- Lengths in use: 245 bytes is 3 full 64-byte keystream blocks
-- plus a 53-byte trailing partial block (the ARM kernel's
-- per-block loop and its byte-wise tail; the Haskell path's
-- analogues). 320 bytes is 5 exact blocks: no partial tail on
-- either path.
cipherCase :: Int -> IO (C.Hypothesis (BS.ByteString, BS.ByteString))
cipherCase n = do
  g <- C.mkRng fixedSeed
  !non <- C.randomBytes g 12
  !msg <- C.randomBytes g n
  let tgt (!k, !m) = case ChaCha20.cipher k 1 non m of
        Right r -> () <$ evaluate r
        Left e  -> die ("censor-chacha: cipher failed: " <> show e)
  C.fixVsRandom tgt (C.randomBytes g 32)
    (\k -> do !k' <- evaluate (BS.copy k); pure (k', msg))

-- Same discipline for the block function: one 64-byte keystream
-- block from the 32-byte key, fixed counter and nonce. Timing
-- must not depend on the key.
blockCase :: IO (C.Hypothesis BS.ByteString)
blockCase = do
  g <- C.mkRng fixedSeed
  !non <- C.randomBytes g 12
  let tgt !k = case ChaCha20.block k 1 non of
        Right r -> () <$ evaluate r
        Left e  -> die ("censor-chacha: block failed: " <> show e)
  C.fixVsRandom tgt (C.randomBytes g 32)
    (\k -> do !k' <- evaluate (BS.copy k); pure k')

main :: IO ()
main = R.runTraceSuite R.TraceSuite
  { R.tsTarget = "ppad-chacha ChaCha20 (key independence)"
  , R.tsConfig = C.defaultConfig
      { C.cfgAlpha  = 1.0e-6
      , C.cfgWarmup = 500
      , C.cfgBudget = 50000
      }
  , R.tsSeed   = Just fixedSeed
  , R.tsNote   = Just "pure targets: batch=1 (no inner repetition)"
  , R.tsCases  =
      [ R.TraceCase "cipher key fix-vs-random" R.SingleShot
          (cipherCase 245)
      , R.TraceCase "cipher key fix-vs-random 320B" R.SingleShot
          (cipherCase 320)
      , R.TraceCase "block key fix-vs-random" R.SingleShot
          blockCase
      ]
  }
