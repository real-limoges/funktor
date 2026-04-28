module HarmonySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Funktor.Harmony
import Funktor.Core.Types (ChordQuality(..))

-- Example intervals for each quality (simplified)
expectedIntervals :: ChordQuality -> [Int]
expectedIntervals Major7       = [0,4,7,11]
expectedIntervals Minor7       = [0,3,7,10]
expectedIntervals Dominant7    = [0,4,7,10]
expectedIntervals HalfDiminished= [0,3,6,10]
expectedIntervals Diminished7  = [0,3,6,9]
expectedIntervals Minor7Flat5 = [0,3,6,10]
expectedIntervals Augmented    = [0,4,8]
expectedIntervals Sus4         = [0,5,7]
expectedIntervals Sus2         = [0,2,7]

tests :: TestTree
tests = testGroup "Harmony"
  [ testCase "quality intervals match expectations" $ do
        let allQuals = [minBound .. maxBound] :: [ChordQuality]
        mapM_ (\q -> let got  = qualityIntervals q
                         expc = expectedIntervals q
                     in assertBool ("intervals for " ++ show q) (got == expc)) allQuals
  ]
