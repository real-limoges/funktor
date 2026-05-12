module HarmonySpec (tests) where

import Funktor.Core.Types (ChordQuality (..), ChordSymbol (..), Pitch (..), Scale (..))
import Funktor.Harmony
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- Example intervals for each quality (simplified)
expectedIntervals :: ChordQuality -> [Int]
expectedIntervals Major7 = [0, 4, 7, 11]
expectedIntervals Minor7 = [0, 3, 7, 10]
expectedIntervals Dominant7 = [0, 4, 7, 10]
expectedIntervals HalfDiminished = [0, 3, 6, 10]
expectedIntervals Diminished7 = [0, 3, 6, 9]
expectedIntervals Minor7Flat5 = [0, 3, 6, 10]
expectedIntervals Augmented = [0, 4, 8]
expectedIntervals Sus4 = [0, 5, 7]
expectedIntervals Sus2 = [0, 2, 7]

tests :: TestTree
tests =
    testGroup
        "Harmony"
        [ testCase "quality intervals match expectations" $ do
            let allQuals = [minBound .. maxBound] :: [ChordQuality]
            mapM_
                ( \q ->
                    let got = qualityIntervals q
                        expc = expectedIntervals q
                     in assertBool ("intervals for " ++ show q) (got == expc)
                )
                allQuals
        , testCase "chordTones transposes correctly" $
            chordTones (ChordSymbol (Pitch 60) Major7) @?= [Pitch 60, Pitch 64, Pitch 67, Pitch 71]
        , testCase "chordTones for a different root" $
            chordTones (ChordSymbol (Pitch 62) Minor7) @?= [Pitch 62, Pitch 65, Pitch 69, Pitch 72]
        , testCase "scaleTones for C major" $
            scaleTones (Pitch 60) (Scale [0, 2, 4, 5, 7, 9, 11])
                @?= [Pitch 60, Pitch 62, Pitch 64, Pitch 65, Pitch 67, Pitch 69, Pitch 71]
        ]
