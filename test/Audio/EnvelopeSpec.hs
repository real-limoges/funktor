{-# LANGUAGE ScopedTypeVariables #-}

module Audio.EnvelopeSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Audio.Envelope
import Test.Utils.Arbitrary ()

ampAt :: EnvelopeParams -> Double -> Maybe Double -> Double -> Double
ampAt = envelopeAmplitude

tests :: TestTree
tests =
    testGroup
        "Audio.Envelope"
        [ testProperty "amplitude before noteOn is zero" $ \(params :: EnvelopeParams) ->
            ampAt params 1.0 Nothing 0.5 == 0
        , testProperty "sustain plateau (no release)" $ \(params :: EnvelopeParams) ->
            let onT = 0.0
                decayEnd = onT + envAttack params + envDecay params
                t = decayEnd + 1.0
             in ampAt params onT Nothing t == envSustain params
        , testProperty "zero after release window" $ \(params :: EnvelopeParams) ->
            let onT = 0.0
                offT = onT + envAttack params + envDecay params + 0.1
                relEnd =
                    max offT (onT + envAttack params + envDecay params)
                        + envRelease params
                        + 0.5
             in ampAt params onT (Just offT) relEnd == 0
        , testCase "never negative for default envelope" $ do
            let params = defaultEnvelope
                times = [0, 0.01 .. 5 :: Double]
            assertBool
                "no negative values"
                (all (>= 0) [ampAt params 0 Nothing t | t <- times])
        , testCase "amplitude reaches 1 at end of attack" $
            let p = defaultEnvelope
                amp = ampAt p 0 Nothing (envAttack p)
             in assertBool ("got " ++ show amp) (abs (amp - 1.0) < 1e-9)
        , testProperty "monotonic non-decreasing during attack" $ \(params :: EnvelopeParams) ->
            let onT = 0.0
                a = envAttack params
                ts = [0, a / 20 .. a]
                amps = [ampAt params onT Nothing t | t <- ts]
             in and (zipWith (<=) amps (drop 1 amps))
        , testProperty "monotonic non-increasing during release" $ \(params :: EnvelopeParams) ->
            let onT = 0.0
                offT = onT + envAttack params + envDecay params + 0.1
                relStart = max offT (onT + envAttack params + envDecay params)
                r = envRelease params
                ts = [relStart, relStart + r / 20 .. relStart + r]
                amps = [ampAt params onT (Just offT) t | t <- ts]
             in and (zipWith (>=) amps (drop 1 amps))
        ]
