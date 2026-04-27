module Audio.VoiceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Audio.Voice
import Funktor.Core.Types
import Test.Utils.Arbitrary ()
import qualified Data.Vector as V

-- Helper to create a full pool with increasing ages
fullPool :: VoicePool
fullPool = VoicePool { poolVoices = V.generate maxVoices (\i -> Just $ Voice (Pitch (60+i)) (midiToFreq (Pitch (60+i))) 0 (Velocity 0.5) 0 Nothing i)
                    , poolNextAge = maxVoices }

tests :: TestTree
tests = testGroup "Audio.Voice"
  [ testProperty "findSlot picks oldest when full" $ \
        let slot = findSlot fullPool
        in slot == 0

  , testProperty "makeVoice frequency matches midi" $ \p ->
        let v = makeVoice 0 p (Velocity 1) 0
        in abs (voiceFreq v - midiToFreq p) < 1e-9

  , testCase "poolNoteOff idempotent" $ do
        let pool = poolNoteOn 0 (Pitch 60) (Velocity 0.5) emptyPool
            pool' = poolNoteOff 1 (Pitch 60) pool
            pool'' = poolNoteOff 2 (Pitch 60) pool'
        let voice = V.head (poolVoices pool'')
        case voice of
          Just v -> assertBool "noteOff unchanged" (voiceNoteOffAt v == Just 1)
          Nothing -> assertBool "voice exists" False

  , testProperty "isVoiceDone after release" $ \params noteOn noteOff ->
        let v = makeVoice noteOn (Pitch 60) (Velocity 0.5) 0
            v' = v { voiceNoteOffAt = Just noteOff }
            t = noteOff + envRelease params + 0.11
        in isVoiceDone params t v'

  , testCase "cleanupVoices removes done voices" $ do
        let params = defaultEnvelope
            noteOn = 0
            noteOff = 1
            vDone = (makeVoice noteOn (Pitch 60) (Velocity 0.5) 0) { voiceNoteOffAt = Just noteOff }
            vAlive = makeVoice noteOn (Pitch 61) (Velocity 0.5) 1
            pool = VoicePool { poolVoices = V.fromList [Just vDone, Just vAlive] V.++ V.replicate (maxVoices-2) Nothing
                             , poolNextAge = 2 }
            cleaned = cleanupVoices params (noteOff + envRelease params + 0.2) pool
            remaining = V.filter (/= Nothing) (poolVoices cleaned)
        assertBool "only alive remains" (V.length remaining == 1)
  ]
