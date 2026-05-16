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

import Data.List (minimumBy, sort)
import Data.Ord (comparing)
import Funktor.Core.Types
import Funktor.Harmony (chordTones)

data VoicingType = ClosePosition | Drop2 | Drop3
    deriving (Eq, Show, Enum, Bounded)

data PitchRange = PitchRange
    { rangeLow :: !Pitch
    , rangeHigh :: !Pitch
    }
    deriving (Eq, Show)

type Voicing = [Pitch]

{- | All inversions of a chord: each rotation drops the bottom voice up an
octave to the top. A 4-note chord has 4 inversions (the original + 3 rotations).
-}
inversions :: [Pitch] -> [[Pitch]]
inversions ps = take (length ps) (iterate invert ps)
  where
    invert (Pitch p : rest) = rest ++ [Pitch (p + 12)]
    invert [] = []

{- | Drop voicings — move a voice down an octave. Drop2 moves the second-
highest voice; Drop3 moves the third-highest. 'ClosePosition' is identity.
-}
applyDrop :: VoicingType -> [Pitch] -> [Pitch]
applyDrop ClosePosition ps = ps
applyDrop Drop2 ps = dropNthFromTop 2 ps
applyDrop Drop3 ps = dropNthFromTop 3 ps

dropNthFromTop :: Int -> [Pitch] -> [Pitch]
dropNthFromTop n ps
    | n <= 0 || n > length ps = ps
    | otherwise = case splitAt (length sorted - n) sorted of
        (before, Pitch v : after) -> sort (before ++ Pitch (v - 12) : after)
        _ -> ps
  where
    sorted = sort ps

-- | Whether every voice falls inside the closed range @[low, high]@.
inRange :: PitchRange -> Voicing -> Bool
inRange (PitchRange lo hi) = all (\p -> p >= lo && p <= hi)

{- | All inversions × drop variants for the chord that fit inside @range@.
Octave transpositions of the whole voicing are also tried so that the
shape can land anywhere inside the range.
-}
allVoicings :: PitchRange -> ChordSymbol -> [Voicing]
allVoicings range cs =
    [ candidate
    | inv <- inversions (chordTones cs)
    , drop_ <- [ClosePosition, Drop2, Drop3]
    , octShift <- [-2 .. 2]
    , let candidate = map (transpose (octShift * 12)) (applyDrop drop_ inv)
    , inRange range candidate
    ]
  where
    transpose semis (Pitch p) = Pitch (p + semis)

{- | Sum of absolute semitone moves between voicings of the same arity.
Mismatched arity falls back to 'maxBound' so 'minimumBy' never selects it.
-}
voiceLeadingCost :: Voicing -> Voicing -> Int
voiceLeadingCost a b
    | length a /= length b = maxBound
    | otherwise = sum (zipWith voiceMove (sort a) (sort b))
  where
    voiceMove (Pitch x) (Pitch y) = abs (x - y)

{- | The voicing of @target@ within @range@ closest by total voice movement
to @previous@. Falls back to the first voicing in @range@ if none score
better (e.g. arity mismatch).
-}
bestVoicing :: PitchRange -> Voicing -> ChordSymbol -> Voicing
bestVoicing range previous target = case allVoicings range target of
    [] -> chordTones target
    vs -> minimumBy (comparing (voiceLeadingCost previous)) vs

{- | Smooth voice-leading over a chord progression. The first chord uses its
'chordTones' as the seed @previous@; subsequent chords pick the voicing
closest to the one before it.
-}
voiceLead :: PitchRange -> [ChordSymbol] -> [Voicing]
voiceLead _ [] = []
voiceLead range (c : cs) =
    let seed = case allVoicings range c of
            (v : _) -> v
            [] -> chordTones c
     in scanl (bestVoicing range) seed cs

{- | Materialise a voicing as a list of simultaneously-struck notes. Note
duration lives on the containing 'Event' under the new DSL, so this no
longer takes a 'Duration' argument.
-}
voicingToNotes :: Velocity -> Voicing -> [Note]
voicingToNotes vel = map (`Note` vel)
