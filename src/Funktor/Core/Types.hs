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

{- | A half-open time interval @[start, end)@ in beats. Used as the query
window for 'Funktor.Core.Stream.Stream' and as both the @whole@ and @part@
extents of an 'Event'.
-}
data Arc = Arc
    { start :: !Beat
    , end :: !Beat
    }
    deriving (Eq, Ord, Show)

arcLength :: Arc -> Beat
arcLength a = a.end - a.start

shiftArc :: Beat -> Arc -> Arc
shiftArc o (Arc s e) = Arc (s + o) (e + o)

scaleArc :: Rational -> Arc -> Arc
scaleArc k (Arc (Beat s) (Beat e)) = Arc (Beat (s * k)) (Beat (e * k))

{- | A timed value with Tidal-style @whole@/@part@ arcs. @whole@ is the
event's true extent (e.g. note on→off); @part@ is the slice returned by the
current 'Stream' query, possibly cropped to a smaller window. For uncropped
events the two are equal.
-}
data Event a = Event
    { whole :: !Arc
    , part :: !Arc
    , value :: !a
    }
    deriving (Eq, Ord, Show, Functor)

-- | Build an 'Event' whose @whole@ and @part@ are equal — the common case.
event :: Arc -> a -> Event a
event a v = Event a a v

shiftEvent :: Beat -> Event a -> Event a
shiftEvent o (Event w p v) = Event (shiftArc o w) (shiftArc o p) v

mapEventValue :: (a -> b) -> Event a -> Event b
mapEventValue = fmap

{- | A pitched note with velocity. Duration lives on the containing
'Event' (via @whole@), not on the note itself — note-shape stays
independent of how long it's held.
-}
data Note = Note
    { pitch :: !Pitch
    , velocity :: !Velocity
    }
    deriving (Eq, Ord, Show)

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
