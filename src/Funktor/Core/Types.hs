module Funktor.Core.Types where

newtype Beat = Beat {unBeat :: Rational}
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Fractional)

type Beats = Rational

newtype Duration = Duration {unDuration :: Beats}
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Fractional)

newtype Tempo = Tempo {unTempo :: Double}
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Fractional)

beatsToSeconds :: Tempo -> Beats -> Double
beatsToSeconds (Tempo bpm) beats =
    (fromRational beats * 60) / bpm

secondsToBeats :: Tempo -> Double -> Beats
secondsToBeats (Tempo bpm) secs =
    toRational (secs * bpm / 60)

-- | MIDI pitch number; 60 = middle C, 69 = A440.
newtype Pitch = Pitch {unPitch :: Int}
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Enum)

type Octave = Int

midiToFreq :: Pitch -> Double
midiToFreq (Pitch p) = 440 * (2 ** (fromIntegral (p - 69) / 12))

freqToMidi :: Double -> Pitch
freqToMidi freq = Pitch $ round $ 69 + 12 * logBase 2 (freq / 440)

newtype Velocity = Velocity {unVelocity :: Double}
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Fractional)

velocityToAmplitude :: Velocity -> Double
velocityToAmplitude (Velocity v) = v * v

data Event a = Event
    { beat :: !Beat
    , value :: !a
    }
    deriving (Eq, Ord, Show, Functor)

mapEventTime :: (Beat -> Beat) -> Event a -> Event a
mapEventTime f (Event t v) = Event (f t) v

mapEventValue :: (a -> b) -> Event a -> Event b
mapEventValue = fmap

data Note = Note
    { pitch :: !Pitch
    , duration :: !Duration
    , velocity :: !Velocity
    }
    deriving (Eq, Show)

newtype ScaleDegree = ScaleDegree {unScaleDegree :: Int}
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Enum)

newtype Scale = Scale {scaleIntervals :: [Int]}
    deriving (Eq, Show)

data ChordQuality
    = Major7
    | Minor7
    | Dominant7
    | HalfDiminished
    | Diminished7
    | Minor7Flat5
    | Augmented
    | Sus4
    | Sus2
    deriving (Eq, Ord, Show, Enum, Bounded)

data ChordSymbol = ChordSymbol
    { chordRoot :: !Pitch
    , chordQuality :: !ChordQuality
    }
    deriving (Eq, Ord, Show)
