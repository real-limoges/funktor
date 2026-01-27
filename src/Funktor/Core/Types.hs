module Funktor.Core.Types where

------------------
--    Time
------------------
newtype Beat = Beat { unBeat :: Rational }
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Fractional)

-- durations measured in beats
type Beats = Rational

-- how long something lasts in beats
newtype Duration = Duration { unDuration :: Beats }
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Fractional)

newtype Tempo = Tempo { unTempo :: Double }
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Fractional)

----------------------------
--    Time Conversions
----------------------------

beatsToSeconds :: Tempo -> Beats -> Double
beatsToSeconds (Tempo bpm) beats =
    (fromRational beats * 60) / bpm

secondsToBeats :: Tempo -> Double -> Beats
secondsToBeats (Tempo bpm) secs =
    toRational (secs * bpm / 60)


-----------------------
--      Pitch
-----------------------

-- Middle C is at 60
newtype Pitch = Pitch { unPitch :: Int }
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Enum)

type Octave = Int

-- MIDI 69 = 440 Hz
midiToFreq :: Pitch -> Double
midiToFreq (Pitch p) = 440 * (2 ** (fromIntegral (p - 69) / 12))

freqToMidi :: Double -> Pitch
freqToMidi freq = Pitch $ round $ 69 + 12 * logBase 2 (freq / 440)

-----------------------
--      Velocity
-----------------------
newtype Velocity = Velocity { unVelocity :: Double }
    deriving (Eq, Ord, Show)
    deriving newtype (Num, Fractional)

velocityToAmplitude :: Velocity -> Double
velocityToAmplitude (Velocity v) = v * v

-----------------------
--      Event
-----------------------

data Event a = Event
    { eventBeat :: !Beat
    , eventValue :: !a
    } deriving (Eq, Ord, Show, Functor)

mapEventTime :: (Beat -> Beat) -> Event a -> Event a
mapEventTime f (Event t v) = Event (f t) v

mapEventValue :: (a -> b) -> Event a -> Event b
mapEventValue = fmap

data Note = Note
    { notePitch :: !Pitch
    , noteDuration :: !Duration
    , noteVelocity :: !Velocity
    } deriving (Eq, Show)
