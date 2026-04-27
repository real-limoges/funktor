module Audio.OscillatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Audio.Oscillator
import Funktor.Core.Types
import Test.Utils.Arbitrary ()

-- Helper to generate a phase within [0,1)
phaseInRange :: Beat -> Bool
phaseInRange (Beat b) = b >= 0 && b < 1

samples :: Waveform -> [Double]
samples wf = [ oscillate wf (fromIntegral i / 1024) 0 | i <- [0..1023] ]

tests :: TestTree
tests = testGroup "Audio.Oscillator"
  [ testProperty "nextPhase modular arithmetic" $ \phase inc ->
        let p = Beat phase
            i = Beat inc
            p' = nextPhase p i
            Beat expected = Beat ((phase + inc) - fromIntegral (floor (phase + inc)))
        in p' == Beat expected

  , testProperty "waveforms stay in [-1,1]" $ \wf ->
        all (\x -> x >= -1 && x <= 1) (samples wf)

  , testProperty "polyBLEP zero dt" $ \phase ->
        polyBLEP phase (1e-12) == 0

  , testCase "sawBLEP continuity at wrap" $ do
        let v1 = sawBLEP 0.999999 0.01
            v2 = sawBLEP 0.000001 0.01
        assertBool "close at wrap" (abs (v1 - v2) < 1e-6)

  , testCase "squareBLEP continuity at wrap" $ do
        let v1 = squareBLEP 0.999999 0.01
            v2 = squareBLEP 0.000001 0.01
        assertBool "close at wrap" (abs (v1 - v2) < 1e-6)
  ]
