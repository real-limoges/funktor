{-# LANGUAGE ScopedTypeVariables #-}

module Audio.VoiceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Data.Maybe (isJust)
import Data.Vector qualified as V
import Funktor.Audio.Envelope (EnvelopeParams (..), defaultEnvelope)
import Funktor.Audio.Oscillator (Waveform (..))
import Funktor.Audio.Timbre (Timbre (..), defaultTimbre)
import Funktor.Audio.Voice
import Funktor.Core.Types (Pitch (..), Velocity (..), midiToFreq)
import Test.Utils.Arbitrary ()

makeVoice :: Double -> Pitch -> Velocity -> Int -> Voice
makeVoice t p v age =
    Voice
        { pitch = p
        , freq = midiToFreq p
        , phase = 0
        , velocity = v
        , waveform = Sine
        , cutoffHz = 20000
        , envelope = defaultEnvelope
        , lowPassPrev = 0
        , noteOnAt = t
        , noteOffAt = Nothing
        , age = age
        }

fullPool :: VoicePool
fullPool =
    VoicePool
        { voices =
            V.generate
                maxVoices
                (\i -> Just (makeVoice 0 (Pitch (60 + i)) (Velocity 0.5) i))
        , nextAge = maxVoices
        }

tests :: TestTree
tests =
    testGroup
        "Audio.Voice"
        [ testCase "findSlot picks oldest when full" $
            assertBool "slot is 0" (findSlot fullPool == 0)
        , testProperty "freq matches midiToFreq" $ \p ->
            let v = makeVoice 0 p (Velocity 1) 0
             in abs (v.freq - midiToFreq p) < 1e-9
        , testCase "poolNoteOff records first off-time" $ do
            let pool = poolNoteOn 0 (Pitch 60) (Velocity 0.5) defaultTimbre emptyPool
                pool' = poolNoteOff 1 (Pitch 60) pool
                pool'' = poolNoteOff 2 (Pitch 60) pool'
                firstVoice = V.find isJust pool''.voices
            case firstVoice of
                Just (Just v) -> assertBool "noteOff was set" (isJust v.noteOffAt)
                _ -> assertBool "voice present" False
        , testProperty "isVoiceDone after release window" $ \(params :: EnvelopeParams) ->
            let onT = 0.0 :: Double
                offT = 1.0 :: Double
                v =
                    (makeVoice onT (Pitch 60) (Velocity 0.5) 0)
                        { noteOffAt = Just offT
                        , envelope = params
                        }
                t = offT + params.release + 0.2
             in isVoiceDone t v
        , testCase "cleanupVoices removes done voices" $ do
            let params = defaultEnvelope
                offT = 1.0 :: Double
                vDone = (makeVoice 0 (Pitch 60) (Velocity 0.5) 0){noteOffAt = Just offT}
                vAlive = makeVoice 0 (Pitch 61) (Velocity 0.5) 1
                vs =
                    V.fromList [Just vDone, Just vAlive]
                        V.++ V.replicate (maxVoices - 2) Nothing
                pool = VoicePool{voices = vs, nextAge = 2}
                cleaned = cleanupVoices (offT + params.release + 0.2) pool
                remain = V.filter isJust cleaned.voices
            assertBool "only alive remains" (V.length remain == 1)
        , testCase "cleanupVoices preserves slot count" $
            -- Regression: V.filter would shrink the vector and break findSlot's
            -- index assumption. Slots must be cleared in place.
            let params = defaultEnvelope
                offT = 1.0 :: Double
                vDone = (makeVoice 0 (Pitch 60) (Velocity 0.5) 0){noteOffAt = Just offT}
                vs = V.fromList [Just vDone] V.++ V.replicate (maxVoices - 1) Nothing
                pool = VoicePool{voices = vs, nextAge = 1}
                cleaned = cleanupVoices (offT + params.release + 0.2) pool
             in assertBool "length preserved" (V.length cleaned.voices == maxVoices)
        , testCase "findSlot picks 0 on empty pool" $
            assertBool "slot 0" (findSlot emptyPool == 0)
        , testCase "poolNoteOn increments nextAge" $
            let p = poolNoteOn 0 (Pitch 60) (Velocity 0.5) defaultTimbre emptyPool
             in assertBool "age incremented" (p.nextAge == emptyPool.nextAge + 1)
        , testCase "poolNoteOn copies waveform from timbre" $
            let tim = Timbre Sawtooth 20000 defaultEnvelope
                p = poolNoteOn 0 (Pitch 60) (Velocity 0.5) tim emptyPool
                firstVoice = V.head p.voices
             in case firstVoice of
                    Just v -> assertBool "got sawtooth" (v.waveform == Sawtooth)
                    Nothing -> assertBool "voice present" False
        ]
