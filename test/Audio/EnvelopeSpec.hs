module Audio.EnvelopeSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Audio.Envelope
import Funktor.Core.Types
import Test.Utils.Arbitrary ()

-- Helper: compute envelope amplitude for a given time with optional note‑off
ampAt :: EnvelopeParams -> Double -> Maybe Double -> Double -> Double
ampAt = envelopeAmplitude

tests :: TestTree
tests = testGroup "Audio.Envelope"
  [ testProperty "attack monotonic rise" $ \params noteOn t ->
        let attackEnd = noteOn + envAttack params
        in t >= noteOn && t <= attackEnd &&
           let a1 = ampAt params noteOn Nothing t
               a2 = ampAt params noteOn Nothing (t + 1e-6)
           in a2 >= a1

  , testProperty "decay monotonic fall" $ \params noteOn t ->
        let attackEnd = noteOn + envAttack params
            decayEnd  = attackEnd + envDecay params
        in t >= attackEnd && t <= decayEnd &&
           let a1 = ampAt params noteOn Nothing t
               a2 = ampAt params noteOn Nothing (t + 1e-6)
           in a2 <= a1

  , testProperty "sustain plateau (no release)" $ \params noteOn t ->
        let decayEnd = noteOn + envAttack params + envDecay params
        in t >= decayEnd &&
           ampAt params noteOn Nothing t == envSustain params

  , testProperty "release monotonic fall" $ \params noteOn noteOff t ->
        let relStart = max noteOff (noteOn + envAttack params + envDecay params)
            relEnd   = relStart + envRelease params
        in t >= relStart && t <= relEnd &&
           let a1 = ampAt params noteOn (Just noteOff) t
               a2 = ampAt params noteOn (Just noteOff) (t + 1e-6)
           in a2 <= a1

  , testProperty "zero after release" $ \params noteOn noteOff ->
        let relEnd = max noteOff (noteOn + envAttack params + envDecay params) + envRelease params + 0.1
        in ampAt params noteOn (Just noteOff) relEnd == 0

  , testCase "never negative" $ do
        let params = defaultEnvelope
        let times = [0,0.01..5]
        assertBool "no negative values" (all (>= 0) [ ampAt params 0 Nothing t | t <- times])
  ]
