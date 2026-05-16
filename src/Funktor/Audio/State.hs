module Funktor.Audio.State (
    AudioState (..),
    sampleRate,
    bufferSize,
    createAudioState,
) where

import Funktor.Audio.Voice (VoicePool, emptyPool)

sampleRate :: Double
sampleRate = 44100

bufferSize :: Int
bufferSize = 512

{- | Mutable audio engine state. The envelope was historically global; voices
now carry their own envelope via 'Funktor.Audio.Timbre.Timbre', so only the
voice pool and wall-clock cursor live here.
-}
data AudioState = AudioState
    { time :: !Double
    , pool :: !VoicePool
    }
    deriving (Show)

createAudioState :: AudioState
createAudioState =
    AudioState
        { time = 0.0
        , pool = emptyPool
        }
