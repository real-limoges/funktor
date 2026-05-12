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
    { oscFreq :: !Double
    , oscPhase :: !Double
    , oscAmplitude :: !Double
    }
    deriving (Show)

data AudioState = AudioState
    { audioOsc :: !OscState
    , audioEnvelope :: !EnvelopeParams
    , audioNoteOnAt :: !(Maybe Double)
    , audioNoteOffAt :: !(Maybe Double)
    , audioTime :: !Double
    , audioPool :: !VoicePool
    }
    deriving (Show)

createSineAudioState :: Double -> Double -> AudioState
createSineAudioState freq amp =
    AudioState
        { audioOsc = OscState freq 0.0 amp
        , audioEnvelope = defaultEnvelope
        , audioNoteOnAt = Nothing
        , audioNoteOffAt = Nothing
        , audioTime = 0.0
        , audioPool = emptyPool
        }
