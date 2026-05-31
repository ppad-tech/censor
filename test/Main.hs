{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (replicateM_)
import Data.IORef
import qualified Censor as C
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "ppad-censor" [
    sanityTests
  , leakDetectionTests
  ]

-- Test config tuned for fast feedback: less strict alpha and small
-- budget, since both fixtures produce strong signals (CT: identical
-- work, leaky: 20x work imbalance).
testConfig :: C.Config
testConfig = C.defaultConfig
  { C.cfgAlpha  = 1.0e-3
  , C.cfgBudget = 5000
  , C.cfgWarmup = 100
  , C.cfgBatch  = 1
  }

sanityTests :: TestTree
sanityTests = testGroup "sanity" [
    testCase "constant-time target passes within budget" $ do
      ref <- newIORef (0 :: Int)
      let hyp = C.Hypothesis
            { C.target  = \_ ->
                replicateM_ 1000 (modifyIORef' ref (+ 1))
            , C.sampleA = pure (2000 :: Int)
            , C.sampleB = pure (100  :: Int)
            }
      result <- C.runCT C.wallClock testConfig hyp
      case result of
        C.Pass _ _ -> pure ()
        C.Reject n w -> assertFailure $
          "CT target falsely rejected at n=" ++ show n
          ++ " logW=" ++ show w
  ]

leakDetectionTests :: TestTree
leakDetectionTests = testGroup "leak detection" [
    testCase "leaky target rejected within budget" $ do
      ref <- newIORef (0 :: Int)
      let hyp = C.Hypothesis
            { C.target  = \n ->
                replicateM_ n (modifyIORef' ref (+ 1))
            , C.sampleA = pure (2000 :: Int)
            , C.sampleB = pure (100  :: Int)
            }
      result <- C.runCT C.wallClock testConfig hyp
      case result of
        C.Reject _ _ -> pure ()
        C.Pass n w -> assertFailure $
          "leaky target not detected; passed n=" ++ show n
          ++ " logW=" ++ show w
  ]
