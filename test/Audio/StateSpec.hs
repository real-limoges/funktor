module Audio.StateSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Data.Maybe (isNothing)
import Data.Vector qualified as V
import Funktor.Audio.State
import Funktor.Audio.Voice (VoicePool (..), maxVoices)

tests :: TestTree
tests =
    testGroup
        "Audio.State"
        [ testCase "createAudioState initializes correctly" $ do
            let st = createAudioState
            assertBool "pool length" (V.length st.pool.voices == maxVoices)
            assertBool "all slots empty" (all isNothing (V.toList st.pool.voices))
            assertBool "time zero" (st.time == 0)
        , testCase "constants" $ do
            assertBool "sampleRate" (sampleRate == 44100)
            assertBool "bufferSize" (bufferSize == 512)
        ]
