module Audio.SineSpec (tests) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VM
import Funktor.Audio.Sine (sineCallback)
import Funktor.Audio.State (AudioState (..), bufferSize, createAudioState)
import Funktor.Audio.Timbre (defaultTimbre)
import Funktor.Audio.Voice (poolNoteOn)
import Funktor.Core.Types (Pitch (..), Velocity (..))
import SDL qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Audio.Sine"
        [ testCase "sineCallback fills buffer with bounded samples" $ do
            buf <- VM.replicate bufferSize (0 :: Float)
            var <- newTVarIO createAudioState
            atomically $ modifyTVar' var $ \s ->
                s{pool = poolNoteOn s.time (Pitch 69) (Velocity 1.0) defaultTimbre s.pool}
            sineCallback var SDL.FloatingLEAudio buf
            samples <- VS.freeze buf
            assertBool "samples stay in [-1,1]" (VS.all (\x -> x >= -1 && x <= 1) samples)
        ]
