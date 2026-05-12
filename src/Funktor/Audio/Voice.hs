module Funktor.Audio.Voice (
    Voice (..),
    VoicePool (..),
    maxVoices,
    emptyPool,
    isVoiceDone,
    poolNoteOn,
    poolNoteOff,
    nextAge,
    cleanupVoices,
    findSlot,
) where

import Data.Maybe (isNothing)
import Data.Vector qualified as V
import Funktor.Audio.Envelope (EnvelopeParams (..))
import Funktor.Core.Types (Pitch, Velocity, midiToFreq)

data Voice = Voice
    { voicePitch :: !Pitch
    , voiceFreq :: !Double
    , voicePhase :: !Double
    , voiceVelocity :: !Velocity
    , voiceNoteOnAt :: !Double
    , voiceNoteOffAt :: !(Maybe Double)
    , voiceAge :: !Int
    }
    deriving (Show)

data VoicePool = VoicePool
    { poolVoices :: !(V.Vector (Maybe Voice))
    , poolNextAge :: !Int
    }
    deriving (Show)

maxVoices :: Int
maxVoices = 8

emptyPool :: VoicePool
emptyPool = VoicePool (V.replicate maxVoices Nothing) 0

isVoiceDone :: EnvelopeParams -> Double -> Voice -> Bool
isVoiceDone params currentTime voice = case voiceNoteOffAt voice of
    Nothing -> False
    Just t -> (currentTime - t) >= envRelease params + 0.1

poolNoteOn :: Double -> Pitch -> Velocity -> VoicePool -> VoicePool
poolNoteOn currentTime pitch velocity pool =
    pool
        { poolVoices = poolVoices pool V.// [(findSlot pool, Just newVoice)]
        , poolNextAge = poolNextAge pool + 1
        }
  where
    newVoice =
        Voice
            { voicePitch = pitch
            , voiceFreq = midiToFreq pitch
            , voicePhase = 0
            , voiceVelocity = velocity
            , voiceNoteOnAt = currentTime
            , voiceNoteOffAt = Nothing
            , voiceAge = poolNextAge pool
            }

poolNoteOff :: Double -> Pitch -> VoicePool -> VoicePool
poolNoteOff currentTime pitch pool =
    pool{poolVoices = V.map (fmap markOff) (poolVoices pool)}
  where
    markOff voice
        | voicePitch voice == pitch = voice{voiceNoteOffAt = Just currentTime}
        | otherwise = voice

findSlot :: VoicePool -> Int
findSlot pool = case V.findIndex isNothing (poolVoices pool) of
    Just i -> i
    Nothing -> oldestSlot (poolVoices pool)

oldestSlot :: V.Vector (Maybe Voice) -> Int
oldestSlot voices = snd $ V.ifoldl' step (maxBound, 0) voices
  where
    step acc@(minAge, _) i (Just v)
        | voiceAge v < minAge = (voiceAge v, i)
        | otherwise = acc
    step acc _ Nothing = acc

cleanupVoices :: EnvelopeParams -> Double -> VoicePool -> VoicePool
cleanupVoices params currentTime pool =
    pool{poolVoices = V.map clear (poolVoices pool)}
  where
    clear (Just v) | isVoiceDone params currentTime v = Nothing
    clear mv = mv

nextAge :: VoicePool -> Int
nextAge = poolNextAge
