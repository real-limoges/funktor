module Funktor.Audio.Timbre (
    Timbre (..),
    defaultTimbre,
) where

import Funktor.Audio.Envelope (EnvelopeParams, defaultEnvelope)
import Funktor.Audio.Oscillator (Waveform (..))

{- | Per-voice synth parameters attached to each note-on. The scheduler
defaults to 'defaultTimbre' when running a 'Stream Note'; richer timbres
can be supplied by calling 'Funktor.Audio.Scheduler.enqueueImmediate'
directly with a 'SchedNoteOn' that carries a custom 'Timbre'.
-}
data Timbre = Timbre
    { waveform :: !Waveform
    , cutoffHz :: !Double
    , envelope :: !EnvelopeParams
    }
    deriving (Eq, Show)

defaultTimbre :: Timbre
defaultTimbre =
    Timbre
        { waveform = Sine
        , cutoffHz = 20000
        , envelope = defaultEnvelope
        }
