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
            -- We re-implement the noteOn body here to avoid opening an SDL device
            -- in the test suite. The body is the same one Funktor.Audio.noteOn uses.
            stateVar <- newTVarIO (createSineAudioState 440 0.5)
            atomically $ modifyTVar' stateVar $ \s ->
                s{audioPool = poolNoteOn (audioTime s) (Pitch 60) (Velocity 0.5) (audioPool s)}
            st <- readTVarIO stateVar
            let pitches = [voicePitch v | Just v <- V.toList (poolVoices (audioPool st))]
            assertBool "voice present with pitch 60" (Pitch 60 `elem` pitches)
        , testCase "TVar noteOff marks the voice for release" $ do
            stateVar <- newTVarIO (createSineAudioState 440 0.5)
            atomically $ modifyTVar' stateVar $ \s ->
                s{audioPool = poolNoteOn (audioTime s) (Pitch 60) (Velocity 0.5) (audioPool s)}
            atomically $ modifyTVar' stateVar $ \s ->
                s{audioPool = poolNoteOff (audioTime s) (Pitch 60) (audioPool s)}
            st <- readTVarIO stateVar
            let offs = [voiceNoteOffAt v | Just v <- V.toList (poolVoices (audioPool st)), voicePitch v == Pitch 60]
            assertBool "noteOffAt recorded" (any (/= Nothing) offs)
        ]
