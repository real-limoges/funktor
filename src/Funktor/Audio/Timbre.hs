{- | Per-voice synth parameters attached to each note-on. Refers to a SynthDef
loaded into scsynth by name (see @synthdefs/funktor.scd@); 'params' are
additional name/value pairs sent with @/s_new@, overriding the SynthDef's
defaults for that single voice.

The scheduler defaults to 'defaultTimbre' when running a 'Stream Note'; per-event
timbres are supplied by calling 'Funktor.Audio.Scheduler.enqueueImmediate'
directly with a 'SchedNoteOn' that carries a custom 'Timbre'.
-}
module Funktor.Audio.Timbre (
    Timbre (..),
    defaultTimbre,
    Waveform (..),
    waveformParam,
    adsr,
) where

data Timbre = Timbre
    { synthDef :: !String
    , params :: ![(String, Float)]
    }
    deriving (Eq, Show)

defaultTimbre :: Timbre
defaultTimbre = Timbre "funktor_note" []

data Waveform = Sine | Sawtooth | Square | Triangle
    deriving (Eq, Show, Enum, Bounded)

waveformParam :: Waveform -> (String, Float)
waveformParam w = ("wave", fromIntegral (fromEnum w))

adsr :: Double -> Double -> Double -> Double -> [(String, Float)]
adsr a d s r =
    [ ("attack", realToFrac a)
    , ("decay", realToFrac d)
    , ("sustain", realToFrac s)
    , ("release", realToFrac r)
    ]
