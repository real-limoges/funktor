module Funktor.Harmony (
    qualityIntervals,
    chordTones,
    scaleTones,
) where

import Funktor.Core.Types

{- | Semitone intervals above the root for each chord quality.

Read each list as: root (0) plus these offsets.
Example: Major7 = [0,4,7,11] means root, major 3rd (+4), perfect 5th (+7),
major 7th (+11).
-}
qualityIntervals :: ChordQuality -> [Int]
qualityIntervals Major7 = [0, 4, 7, 11]
qualityIntervals Minor7 = [0, 3, 7, 10]
qualityIntervals Dominant7 = [0, 4, 7, 10]
qualityIntervals HalfDiminished = [0, 3, 6, 10]
qualityIntervals Diminished7 = [0, 3, 6, 9]
qualityIntervals Minor7Flat5 = [0, 3, 6, 10]
qualityIntervals Augmented = [0, 4, 8]
qualityIntervals Sus4 = [0, 5, 7]
qualityIntervals Sus2 = [0, 2, 7]

{- | All chord tones for a given chord symbol.

>>> chordTones (ChordSymbol (Pitch 60) Major7)
[Pitch 60, Pitch 64, Pitch 67, Pitch 71]
-}
chordTones :: ChordSymbol -> [Pitch]
chordTones (ChordSymbol root quality) =
    [root + Pitch i | i <- qualityIntervals quality]

{- | All scale tones for a root pitch and scale definition.

>>> scaleTones (Pitch 60) (Scale [0,2,4,5,7,9,11])
[Pitch 60, Pitch 62, Pitch 64, Pitch 65, Pitch 67, Pitch 69, Pitch 71]
-}
scaleTones :: Pitch -> Scale -> [Pitch]
scaleTones root (Scale intervals) =
    [root + Pitch i | i <- intervals]
