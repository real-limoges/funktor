module Audio.OscillatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty.QuickCheck (testProperty, forAll, choose)

import Funktor.Audio.Oscillator
import Test.Utils.Arbitrary ()

samples :: Waveform -> [Double]
samples wf = [ oscillate wf (fromIntegral i / 1024) (1 / 1024) | i <- [0 .. 1023 :: Int] ]

tests :: TestTree
tests = testGroup "Audio.Oscillator"
  [ testProperty "nextPhase wraps into [0,1)" $
        forAll (choose (0, 0.999999)) $ \phase ->
        forAll (choose (0, 0.999999)) $ \inc ->
        let p' = nextPhase phase inc
        in p' >= 0 && p' < 1

  , testProperty "polyBLEP is zero when dt is tiny" $
        forAll (choose (0, 0.999999)) $ \phase ->
        polyBLEP phase 1e-12 == 0

  , testCase "sine waveform stays in [-1,1]" $
        assertBool "in range" (all (\x -> x >= -1 && x <= 1) (samples Sine))

  , testCase "triangle waveform stays in [-1,1]" $
        assertBool "in range" (all (\x -> x >= -1 && x <= 1) (samples Triangle))

  , testCase "sawBLEP continuous near wrap" $ do
        let v1 = sawBLEP 0.999999 0.01
            v2 = sawBLEP 0.000001 0.01
        assertBool "close at wrap" (abs (v1 - v2) < 1e-2)
  ]
