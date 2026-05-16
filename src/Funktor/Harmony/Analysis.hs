module Funktor.Harmony.Analysis (
    NamedScale (..),
    jazzScales,
    scalesForChord,
    scalesForChordLoose,
    chordsFromScale,
    classifyIntervals,
) where

import Data.List (sort)
import Funktor.Core.Types
import Funktor.Harmony (chordTones)

-- | A scale with a human-readable name.
data NamedScale = NamedScale
    { scaleName :: String
    , scaleDefinition :: Scale
    }
    deriving (Eq, Show)

{- | A small canon of jazz-friendly scales. Each row is the scale relative
to the tonic (semitone offsets in @[0..11]@).
-}
jazzScales :: [NamedScale]
jazzScales =
    [ NamedScale "Ionian" (Scale [0, 2, 4, 5, 7, 9, 11])
    , NamedScale "Dorian" (Scale [0, 2, 3, 5, 7, 9, 10])
    , NamedScale "Phrygian" (Scale [0, 1, 3, 5, 7, 8, 10])
    , NamedScale "Lydian" (Scale [0, 2, 4, 6, 7, 9, 11])
    , NamedScale "Mixolydian" (Scale [0, 2, 4, 5, 7, 9, 10])
    , NamedScale "Aeolian" (Scale [0, 2, 3, 5, 7, 8, 10])
    , NamedScale "Locrian" (Scale [0, 1, 3, 5, 6, 8, 10])
    , NamedScale "MelodicMinor" (Scale [0, 2, 3, 5, 7, 9, 11])
    , NamedScale "LydianDominant" (Scale [0, 2, 4, 6, 7, 9, 10])
    , NamedScale "Altered" (Scale [0, 1, 3, 4, 6, 8, 10])
    , NamedScale "HalfWholeDiminished" (Scale [0, 1, 3, 4, 6, 7, 9, 10])
    , NamedScale "WholeTone" (Scale [0, 2, 4, 6, 8, 10])
    , NamedScale "BebopDominant" (Scale [0, 2, 4, 5, 7, 9, 10, 11])
    , NamedScale "BebopMajor" (Scale [0, 2, 4, 5, 7, 8, 9, 11])
    ]

-- | Pitch-class set of a chord (intervals from root mod 12, sorted).
chordPCs :: ChordSymbol -> [Int]
chordPCs cs =
    sort
        [ (p - r) `mod` 12
        | let Pitch r = cs.chordRoot
        , Pitch p <- chordTones cs
        ]

-- | Pitch-class set of a scale relative to the root (sorted, deduped via 'sort' + dedupe).
scalePCs :: Scale -> [Int]
scalePCs (Scale is) = sort [i `mod` 12 | i <- is]

{- | Scales that contain every tone of the chord exactly. Returned alongside
the chord-rooted 'Scale' (the named scale's intervals shifted so the chord
root is the tonic), useful for soloing.
-}
scalesForChord :: ChordSymbol -> [(NamedScale, Scale)]
scalesForChord cs =
    let chordSet = chordPCs cs
     in [ (ns, ns.scaleDefinition)
        | ns <- jazzScales
        , let setS = scalePCs ns.scaleDefinition
        , all (`elem` setS) chordSet
        ]

{- | Looser match: keep the chord's third, fifth, and seventh but ignore the
root and any extensions beyond the basic 7th. Useful when the chord
symbol carries alterations the named scale doesn't list verbatim.
-}
scalesForChordLoose :: ChordSymbol -> [(NamedScale, Scale)]
scalesForChordLoose cs =
    let trimmed = take 3 (drop 1 (chordPCs cs))
     in [ (ns, ns.scaleDefinition)
        | ns <- jazzScales
        , let setS = scalePCs ns.scaleDefinition
        , all (`elem` setS) trimmed
        ]

{- | Walk each scale degree and read off the seventh chord built by stacking
thirds within the scale. The returned list has one chord per scale degree.
-}
chordsFromScale :: Scale -> Pitch -> [ChordSymbol]
chordsFromScale (Scale is) (Pitch root) =
    [ ChordSymbol (Pitch (root + (is !! i))) (classifyAt i)
    | i <- [0 .. length is - 1]
    , length is >= 7 -- need at least a heptatonic scale for thirds-stacking
    ]
  where
    classifyAt i =
        let n = length is
            r = is !! i
            third = (is !! ((i + 2) `mod` n)) - r
            fifth = (is !! ((i + 4) `mod` n)) - r
            seventh = (is !! ((i + 6) `mod` n)) - r
            norm x = x `mod` 12
         in classifyIntervals (norm third) (norm fifth) (norm seventh)

{- | Bucket an @(third, fifth, seventh)@ semitone triple into a chord quality.
Falls back to @Sus4@ when nothing else matches; callers needing precision
should switch on the triple themselves.
-}
classifyIntervals :: Int -> Int -> Int -> ChordQuality
classifyIntervals 4 7 11 = Major7
classifyIntervals 3 7 10 = Minor7
classifyIntervals 4 7 10 = Dominant7
classifyIntervals 3 6 10 = Minor7Flat5
classifyIntervals 3 6 9 = Diminished7
classifyIntervals 4 8 11 = Augmented
classifyIntervals 5 7 _ = Sus4
classifyIntervals 2 7 _ = Sus2
classifyIntervals 3 6 _ = HalfDiminished
classifyIntervals _ _ _ = Sus4
