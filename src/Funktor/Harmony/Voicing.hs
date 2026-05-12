module Funktor.Harmony.Voicing (
    VoicingType (..),
    PitchRange (..),
    Voicing,
    inversions,
    applyDrop,
    inRange,
    allVoicings,
    voiceLeadingCost,
    bestVoicing,
    voiceLead,
    voicingToNotes,
) where

import Funktor.Core.Types

data VoicingType = ClosePosition | Drop2 | Drop3
    deriving (Eq, Show, Enum, Bounded)

data PitchRange = PitchRange
    { rangeLow :: !Pitch
    , rangeHigh :: !Pitch
    }
    deriving (Eq, Show)

type Voicing = [Pitch]

inversions :: [Pitch] -> [[Pitch]]
inversions = undefined

applyDrop :: VoicingType -> [Pitch] -> [Pitch]
applyDrop = undefined

inRange :: PitchRange -> Voicing -> Bool
inRange = undefined

allVoicings :: PitchRange -> ChordSymbol -> [Voicing]
allVoicings = undefined

voiceLeadingCost :: Voicing -> Voicing -> Int
voiceLeadingCost = undefined

bestVoicing :: PitchRange -> Voicing -> ChordSymbol -> Voicing
bestVoicing = undefined

voiceLead :: PitchRange -> [ChordSymbol] -> [Voicing]
voiceLead = undefined

voicingToNotes :: Duration -> Velocity -> Voicing -> [Note]
voicingToNotes = undefined
