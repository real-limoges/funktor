module Funktor.Audio.Voice
    ( Voice (..)
    , VoicePool (..)
    , maxVoices
    , emptyPool
    , isVoiceDone
    , poolNoteOn
    , poolNoteOff
    , nextAge
    , cleanupVoices
    , findSlot
    ) where

import Funktor.Core.Types (Pitch, Velocity, midiToFreq)
import Funktor.Audio.Envelope (EnvelopeParams (..))
import qualified Data.Vector as V

data Voice = Voice
    { voicePitch     :: !Pitch
    , voiceFreq      :: !Double
    , voicePhase     :: !Double
    , voiceVelocity  :: !Velocity
    , voiceNoteOnAt  :: !Double
    , voiceNoteOffAt :: !(Maybe Double)
    , voiceAge       :: !Int
    } deriving (Show)

data VoicePool = VoicePool
    { poolVoices  :: !(V.Vector (Maybe Voice))
    , poolNextAge :: !Int
    } deriving (Show)

maxVoices :: Int
maxVoices = 8

emptyPool :: VoicePool
emptyPool = VoicePool
    { poolVoices = V.replicate maxVoices Nothing
    , poolNextAge = 0
    }

isVoiceDone :: EnvelopeParams -> Double -> Voice -> Bool
isVoiceDone params currentTime voice =
    case voiceNoteOffAt voice of
        Nothing -> False
        Just t -> (currentTime - t) >= envRelease params + 0.1

poolNoteOn :: Double -> Pitch -> Velocity -> VoicePool -> VoicePool
poolNoteOn currentTime pitch velocity pool =
    let slot = findSlot pool
        newVoice = Just (makeVoice currentTime pitch velocity (poolNextAge pool))
        newVoices = V.update (poolVoices pool) (V.fromList [(slot, newVoice)])
    in pool { poolVoices = newVoices, poolNextAge = poolNextAge pool + 1 }

makeVoice :: Double -> Pitch -> Velocity -> Int -> Voice
makeVoice currentTime pitch velocity age =
    Voice pitch (midiToFreq pitch) 0 velocity currentTime Nothing age

poolNoteOff :: Double -> Pitch -> VoicePool -> VoicePool
poolNoteOff currentTime pitch pool =
    pool { poolVoices = V.map (updateVoice pitch currentTime) (poolVoices pool) }

updateVoice :: Pitch -> Double -> Maybe Voice -> Maybe Voice
updateVoice pitch currentTime maybeVoice =
    case maybeVoice of
        Nothing -> Nothing
        Just voice ->
            if voicePitch voice == pitch
                then Just voice { voiceNoteOffAt = Just currentTime }
                else maybeVoice

findSlot :: VoicePool -> Int
findSlot pool =
    case findFirstEmpty (poolVoices pool) 0 of
        Just i -> i
        Nothing -> findOldest (poolVoices pool) 0 maxBound 0

findFirstEmpty :: V.Vector (Maybe Voice) -> Int -> Maybe Int
findFirstEmpty voices i =
    if i >= V.length voices then Nothing
    else case V.unsafeIndex voices i of
        Nothing -> Just i
        Just _ -> findFirstEmpty voices (i + 1)

findOldest :: V.Vector (Maybe Voice) -> Int -> Int -> Int -> Int
findOldest voices i minAge minIdx =
    if i >= V.length voices then minIdx
    else case V.unsafeIndex voices i of
        Nothing -> findOldest voices (i + 1) minAge minIdx
        Just v ->
            let age = voiceAge v
            in if age < minAge
                then findOldest voices (i + 1) age i
                else findOldest voices (i + 1) minAge minIdx

cleanupVoices :: EnvelopeParams -> Double -> VoicePool -> VoicePool
cleanupVoices params currentTime pool =
    let isNotDone maybeVoice = case maybeVoice of
            Nothing -> True
            Just voice -> not (isVoiceDone params currentTime voice)
    in pool { poolVoices = V.filter isNotDone (poolVoices pool) }

nextAge :: VoicePool -> Int
nextAge = poolNextAge
