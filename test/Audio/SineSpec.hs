module Audio.SineSpec (tests) where

import Control.Concurrent.STM (newTVarIO)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VM
import Funktor.Audio (sineCallback)
import Funktor.Audio.State (bufferSize, createSineAudioState)
import SDL qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Audio.Sine"
        [ testCase "sineCallback fills buffer with bounded samples" $ do
            buf <- VM.replicate bufferSize (0 :: Float)
            var <- newTVarIO $ createSineAudioState 440 1.0
            sineCallback var SDL.FloatingLEAudio buf
            samples <- VS.freeze buf
            assertBool "samples stay in [-1,1]" (VS.all (\x -> x >= -1 && x <= 1) samples)
        ]
