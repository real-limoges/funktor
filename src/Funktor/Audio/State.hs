module Funktor.Audio.State (
    AudioState (..),
    OscState (..),
    sampleRate,
    bufferSize,
    createSineAudioState,
) where

import Funktor.Audio.Envelope (EnvelopeParams, defaultEnvelope)
import Funktor.Audio.Voice (VoicePool, emptyPool)

sampleRate :: Double
sampleRate = 44100

bufferSize :: Int
bufferSize = 512

data OscState = OscState
    { freq :: !Double
    , phase :: !Double
    , amplitude :: !Double
    }
    deriving (Show)

data AudioState = AudioState
    { osc :: !OscState
    , envelope :: !EnvelopeParams
    , time :: !Double
    , pool :: !VoicePool
    }
    deriving (Show)

createSineAudioState :: Double -> Double -> AudioState
createSineAudioState f amp =
    AudioState
        { osc = OscState f 0.0 amp
        , envelope = defaultEnvelope
        , time = 0.0
        , pool = emptyPool
        }
