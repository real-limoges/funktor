module Funktor.Harmony.Analysis (
    NamedScale (..),
    jazzScales,
    scalesForChord,
    scalesForChordLoose,
    chordsFromScale,
    classifyIntervals,
) where

import Funktor.Core.Types

-- | A scale with a human-readable name.
data NamedScale = NamedScale
    { scaleName :: String
    , scaleDefinition :: Scale
    }
    deriving (Eq, Show)

jazzScales :: [NamedScale]
jazzScales = undefined

scalesForChord :: ChordSymbol -> [(NamedScale, Scale)]
scalesForChord = undefined

scalesForChordLoose :: ChordSymbol -> [(NamedScale, Scale)]
scalesForChordLoose = undefined

chordsFromScale :: Scale -> Pitch -> [ChordSymbol]
chordsFromScale = undefined

classifyIntervals :: Int -> Int -> Int -> ChordQuality
classifyIntervals = undefined
