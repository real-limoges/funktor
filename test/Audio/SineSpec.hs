module Audio.SineSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as VM
import Funktor.Audio.Sine (sineCallback)
import Funktor.Audio.State (createSineAudioState, sampleRate, bufferSize)
import Funktor.Audio.Voice (Voice(..), VoicePool(..), maxVoices, emptyPool)
import Funktor.Core.Types (Pitch(..), Velocity(..), midiToFreq, velocityToAmplitude)
import qualified SDL

setupAudio :: IO (VM.IOVector Float, VM.IOVector Float, VM.IOVector Float, VM.IOVector Float, VM.IOVector Float, VM.IOVector Float, VM.IOVector Float, VM.IOVector Float, VM.IOVector Float, VM.IOVector Float)
setupAudio = do
  let voice = Voice { voicePitch = Pitch 60
                    , voiceFreq = midiToFreq (Pitch 60)
                    , voicePhase = 0
                    , voiceVelocity = Velocity 0.8
                    , voiceNoteOnAt = 0
                    , voiceNoteOffAt = Nothing
                    , voiceAge = 0
                    }
      pool = VoicePool { poolVoices = V.singleton (Just voice)
                       , poolNextAge = 1 }
      st = createSineAudioState 440 1.0 { audioPool = pool }
  var <- newTVarIO st
  buf <- VM.replicate bufferSize (0 :: Float)
  pure buf

tests :: TestTree
tests = testGroup "Audio.Sine"
  [ testCase "sineCallback produces bounded samples and advances phase" $ do
        buf <- VM.replicate bufferSize (0 :: Float)
        var <- newTVarIO $ createSineAudioState 440 1.0
        sineCallback var SDL.FloatingLEAudio buf
        samples <- V.freeze buf
        let inBounds = V.all (\x -> x >= -1 && x <= 1) samples
        assertBool "samples stay in [-1,1]" inBounds
        -- check that phase advanced
        st <- readTVarIO var
        let voice = V.head (poolVoices $ audioPool st)
        case voice of
          Just v -> let expected = (voicePhase v) `mod'` 1
                     in assertBool "phase advanced" (abs (voicePhase v - expected) < 1e-9)
          Nothing -> assertBool "voice present" False
  ]
