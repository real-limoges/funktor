module Funktor.Audio.Voice (
    Voice (..),
    VoicePool (..),
    maxVoices,
    emptyPool,
    isVoiceDone,
    poolNoteOn,
    poolNoteOff,
    cleanupVoices,
    findSlot,
) where

import Data.Maybe (isNothing)
import Data.Vector qualified as V
import Funktor.Audio.Envelope (EnvelopeParams (..))
import Funktor.Core.Types (Pitch, Velocity, midiToFreq)

data Voice = Voice
    { pitch :: !Pitch
    , freq :: !Double
    , phase :: !Double
    , velocity :: !Velocity
    , noteOnAt :: !Double
    , noteOffAt :: !(Maybe Double)
    , age :: !Int
    }
    deriving (Show)

data VoicePool = VoicePool
    { voices :: !(V.Vector (Maybe Voice))
    , nextAge :: !Int
    }
    deriving (Show)

maxVoices :: Int
maxVoices = 8

emptyPool :: VoicePool
emptyPool = VoicePool (V.replicate maxVoices Nothing) 0

{- | A voice is reclaimable once it has been released for the full envelope
release plus a 100ms tail. The tail keeps a slot reserved across one or two
extra audio buffers so the release curve isn't truncated by wall-clock jitter
between the scheduler tick and the next callback.
-}
isVoiceDone :: EnvelopeParams -> Double -> Voice -> Bool
isVoiceDone params currentTime voice = case voice.noteOffAt of
    Nothing -> False
    Just t -> (currentTime - t) >= params.release + 0.1

poolNoteOn :: Double -> Pitch -> Velocity -> VoicePool -> VoicePool
poolNoteOn currentTime p v pool =
    pool
        { voices = pool.voices V.// [(findSlot pool, Just newVoice)]
        , nextAge = pool.nextAge + 1
        }
  where
    newVoice =
        Voice
            { pitch = p
            , freq = midiToFreq p
            , phase = 0
            , velocity = v
            , noteOnAt = currentTime
            , noteOffAt = Nothing
            , age = pool.nextAge
            }

poolNoteOff :: Double -> Pitch -> VoicePool -> VoicePool
poolNoteOff currentTime p pool =
    pool{voices = V.map (fmap markOff) pool.voices}
  where
    markOff voice
        | voice.pitch == p = voice{noteOffAt = Just currentTime}
        | otherwise = voice

findSlot :: VoicePool -> Int
findSlot pool = case V.findIndex isNothing pool.voices of
    Just i -> i
    Nothing -> oldestSlot pool.voices

oldestSlot :: V.Vector (Maybe Voice) -> Int
oldestSlot vs = snd $ V.ifoldl' step (maxBound, 0) vs
  where
    step acc@(minAge, _) i (Just v)
        | v.age < minAge = (v.age, i)
        | otherwise = acc
    step acc _ Nothing = acc

cleanupVoices :: EnvelopeParams -> Double -> VoicePool -> VoicePool
cleanupVoices params currentTime pool =
    pool{voices = V.map clear pool.voices}
  where
    clear (Just v) | isVoiceDone params currentTime v = Nothing
    clear mv = mv
