{-# LANGUAGE RecordWildCards #-}
module Funktor.Audio.State
    ( AudioState(..)
    , OscState(..)
    , sampleRate
    , bufferSize
    , createSineAudioState
    ) where

import Funktor.Audio.Voice (VoicePool, emptyPool)
import Funktor.Audio.Envelope (EnvelopeParams, defaultEnvelope)

sampleRate :: Double
sampleRate = 44100

bufferSize :: Int
bufferSize = 512

data OscState = OscState
    { oscFreq      :: !Double
    , oscPhase     :: !Double
    , oscAmplitude :: !Double
    } deriving (Show)

data AudioState = AudioState
    { audioPool      :: !VoicePool
    , audioEnvelope :: !EnvelopeParams
    , audioTime     :: !Double
    } deriving (Show)

createSineAudioState :: Double -> Double -> AudioState
createSineAudioState _freq _amp =
    AudioState
    { audioPool = emptyPool
    , audioEnvelope = defaultEnvelope
    , audioTime = 0.0
    }
