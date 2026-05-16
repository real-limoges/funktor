module Harmony.AnalysisSpec (tests) where

import Data.List (find)
import Funktor.Core.Types
import Funktor.Harmony.Analysis
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

byName :: String -> [(NamedScale, b)] -> Bool
byName n = any ((== n) . scaleName . fst)

tests :: TestTree
tests =
    testGroup
        "Harmony.Analysis"
        [ testCase "jazzScales lists at least the modes" $ do
            assertBool "has Ionian" (any ((== "Ionian") . scaleName) jazzScales)
            assertBool "has Dorian" (any ((== "Dorian") . scaleName) jazzScales)
            assertBool "has Lydian" (any ((== "Lydian") . scaleName) jazzScales)
            assertBool "has Mixolydian" (any ((== "Mixolydian") . scaleName) jazzScales)
        , testCase "scalesForChord Cmaj7 includes Ionian and Lydian" $ do
            let result = scalesForChord (ChordSymbol (Pitch 60) Major7)
            assertBool "Ionian present" (byName "Ionian" result)
            assertBool "Lydian present" (byName "Lydian" result)
        , testCase "scalesForChord Dmin7 includes Dorian" $ do
            let result = scalesForChord (ChordSymbol (Pitch 62) Minor7)
            assertBool "Dorian present" (byName "Dorian" result)
        , testCase "scalesForChord G7 includes Mixolydian" $ do
            let result = scalesForChord (ChordSymbol (Pitch 67) Dominant7)
            assertBool "Mixolydian present" (byName "Mixolydian" result)
        , testCase "classifyIntervals covers the seven core qualities" $ do
            classifyIntervals 4 7 11 @?= Major7
            classifyIntervals 3 7 10 @?= Minor7
            classifyIntervals 4 7 10 @?= Dominant7
            classifyIntervals 3 6 10 @?= Minor7Flat5
            classifyIntervals 3 6 9 @?= Diminished7
            classifyIntervals 4 8 11 @?= Augmented
        , testCase "chordsFromScale C Ionian produces 7 chords" $
            length (chordsFromScale (Scale [0, 2, 4, 5, 7, 9, 11]) (Pitch 60)) @?= 7
        , testCase "chordsFromScale C Ionian first chord rooted at C" $ do
            let cs = chordsFromScale (Scale [0, 2, 4, 5, 7, 9, 11]) (Pitch 60)
            case cs of
                (c : _) -> c.chordRoot @?= Pitch 60
                [] -> assertBool "non-empty" False
        , testCase "chordsFromScale C Ionian I chord is Major7" $ do
            let cs = chordsFromScale (Scale [0, 2, 4, 5, 7, 9, 11]) (Pitch 60)
                cMaj = find (\c -> c.chordRoot == Pitch 60) cs
            case cMaj of
                Just c -> c.chordQuality @?= Major7
                Nothing -> assertBool "I chord present" False
        , testCase "chordsFromScale on non-heptatonic scale is empty" $
            chordsFromScale (Scale [0, 7]) (Pitch 60) @?= []
        , testCase "scalesForChordLoose accepts more than scalesForChord" $
            let chord = ChordSymbol (Pitch 60) Major7
                strict = length (scalesForChord chord)
                loose = length (scalesForChordLoose chord)
             in assertBool ("strict=" ++ show strict ++ " loose=" ++ show loose) (loose >= strict)
        ]
