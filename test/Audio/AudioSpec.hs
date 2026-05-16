module Audio.AudioSpec (tests) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Vector qualified as V
import Funktor.Audio (AudioState (..))
import Funktor.Audio.State (createSineAudioState)
import Funktor.Audio.Voice (Voice (..), VoicePool (..), poolNoteOff, poolNoteOn)
import Funktor.Core.Types (Pitch (..), Velocity (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Audio (top-level)"
        [ testCase "TVar noteOn places a voice with the requested pitch" $ do
            -- Re-implements the noteOn body here to avoid opening an SDL
            -- device in the test suite.
            stateVar <- newTVarIO (createSineAudioState 440 0.5)
            atomically $ modifyTVar' stateVar $ \s ->
                s{pool = poolNoteOn s.time (Pitch 60) (Velocity 0.5) s.pool}
            st <- readTVarIO stateVar
            let pitches = [v.pitch | Just v <- V.toList st.pool.voices]
            assertBool "voice present with pitch 60" (Pitch 60 `elem` pitches)
        , testCase "TVar noteOff marks the voice for release" $ do
            stateVar <- newTVarIO (createSineAudioState 440 0.5)
            atomically $ modifyTVar' stateVar $ \s ->
                s{pool = poolNoteOn s.time (Pitch 60) (Velocity 0.5) s.pool}
            atomically $ modifyTVar' stateVar $ \s ->
                s{pool = poolNoteOff s.time (Pitch 60) s.pool}
            st <- readTVarIO stateVar
            let offs = [v.noteOffAt | Just v <- V.toList st.pool.voices, v.pitch == Pitch 60]
            assertBool "noteOffAt recorded" (any (/= Nothing) offs)
        ]
