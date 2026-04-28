{-# LANGUAGE ScopedTypeVariables #-}
module Audio.VoiceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Audio.Voice
import Funktor.Audio.Envelope (EnvelopeParams(..), defaultEnvelope)
import Funktor.Core.Types (Pitch(..), Velocity(..), midiToFreq)
import Test.Utils.Arbitrary ()
import Data.Maybe (isJust)
import qualified Data.Vector as V

makeVoice :: Double -> Pitch -> Velocity -> Int -> Voice
makeVoice t p v age =
    Voice { voicePitch     = p
          , voiceFreq      = midiToFreq p
          , voicePhase     = 0
          , voiceVelocity  = v
          , voiceNoteOnAt  = t
          , voiceNoteOffAt = Nothing
          , voiceAge       = age
          }

fullPool :: VoicePool
fullPool = VoicePool
    { poolVoices  = V.generate maxVoices
                      (\i -> Just (makeVoice 0 (Pitch (60 + i)) (Velocity 0.5) i))
    , poolNextAge = maxVoices
    }

tests :: TestTree
tests = testGroup "Audio.Voice"
  [ testCase "findSlot picks oldest when full" $
        assertBool "slot is 0" (findSlot fullPool == 0)

  , testProperty "voiceFreq matches midiToFreq" $ \p ->
        let v = makeVoice 0 p (Velocity 1) 0
        in abs (voiceFreq v - midiToFreq p) < 1e-9

  , testCase "poolNoteOff records first off-time" $ do
        let pool   = poolNoteOn 0 (Pitch 60) (Velocity 0.5) emptyPool
            pool'  = poolNoteOff 1 (Pitch 60) pool
            pool'' = poolNoteOff 2 (Pitch 60) pool'
            firstVoice = V.find isJust (poolVoices pool'')
        case firstVoice of
          Just (Just v) -> assertBool "noteOff was set" (voiceNoteOffAt v /= Nothing)
          _             -> assertBool "voice present" False

  , testProperty "isVoiceDone after release window" $ \(params :: EnvelopeParams) ->
        let onT  = 0.0 :: Double
            offT = 1.0 :: Double
            v    = (makeVoice onT (Pitch 60) (Velocity 0.5) 0)
                     { voiceNoteOffAt = Just offT }
            t    = offT + envRelease params + 0.2
        in isVoiceDone params t v

  , testCase "cleanupVoices removes done voices" $ do
        let params  = defaultEnvelope
            offT    = 1.0 :: Double
            vDone   = (makeVoice 0 (Pitch 60) (Velocity 0.5) 0) { voiceNoteOffAt = Just offT }
            vAlive  = makeVoice 0 (Pitch 61) (Velocity 0.5) 1
            voices  = V.fromList [Just vDone, Just vAlive]
                    V.++ V.replicate (maxVoices - 2) Nothing
            pool    = VoicePool { poolVoices = voices, poolNextAge = 2 }
            cleaned = cleanupVoices params (offT + envRelease params + 0.2) pool
            remain  = V.filter isJust (poolVoices cleaned)
        assertBool "only alive remains" (V.length remain == 1)
  ]
