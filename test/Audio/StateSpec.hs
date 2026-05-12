module Audio.StateSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Data.Maybe (isNothing)
import Data.Vector qualified as V
import Funktor.Audio.Envelope (defaultEnvelope)
import Funktor.Audio.State
import Funktor.Audio.Voice (VoicePool (..), maxVoices)

tests :: TestTree
tests =
    testGroup
        "Audio.State"
        [ testCase "createSineAudioState initializes correctly" $ do
            let st = createSineAudioState 440 1.0
            assertBool "pool length" (V.length (poolVoices $ audioPool st) == maxVoices)
            assertBool "all slots empty" (all isNothing (V.toList $ poolVoices $ audioPool st))
            assertBool "default envelope" (audioEnvelope st == defaultEnvelope)
            assertBool "time zero" (audioTime st == 0)
        , testCase "constants" $ do
            assertBool "sampleRate" (sampleRate == 44100)
            assertBool "bufferSize" (bufferSize == 512)
        ]
