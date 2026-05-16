module Harmony.VoicingSpec (tests) where

import Funktor.Core.Types
import Funktor.Harmony (chordTones)
import Funktor.Harmony.Voicing
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

cMaj7 :: ChordSymbol
cMaj7 = ChordSymbol (Pitch 60) Major7

dMin7 :: ChordSymbol
dMin7 = ChordSymbol (Pitch 62) Minor7

g7 :: ChordSymbol
g7 = ChordSymbol (Pitch 67) Dominant7

wideRange :: PitchRange
wideRange = PitchRange (Pitch 36) (Pitch 96)

tests :: TestTree
tests =
    testGroup
        "Harmony.Voicing"
        [ testCase "inversions length equals chord arity" $
            length (inversions (chordTones cMaj7)) @?= 4
        , testCase "first inversion in 'inversions' is the input" $
            head (inversions (chordTones cMaj7)) @?= chordTones cMaj7
        , testCase "ClosePosition leaves the voicing unchanged" $
            applyDrop ClosePosition (chordTones cMaj7) @?= chordTones cMaj7
        , testCase "Drop2 lowers exactly one voice by an octave" $
            let original = chordTones cMaj7
                dropped = applyDrop Drop2 original
                origSum = sum [p | Pitch p <- original]
                dropSum = sum [p | Pitch p <- dropped]
             in (origSum - dropSum) @?= 12
        , testCase "voiceLeadingCost of a voicing with itself is 0" $
            voiceLeadingCost (chordTones cMaj7) (chordTones cMaj7) @?= 0
        , testCase "voiceLeadingCost is symmetric" $
            voiceLeadingCost (chordTones cMaj7) (chordTones dMin7)
                @?= voiceLeadingCost (chordTones dMin7) (chordTones cMaj7)
        , testCase "voiceLeadingCost with mismatched arity is maxBound" $
            voiceLeadingCost (chordTones cMaj7) [Pitch 60] @?= maxBound
        , testCase "inRange accepts an in-range voicing" $
            assertBool "in range" (inRange wideRange (chordTones cMaj7))
        , testCase "inRange rejects an out-of-range voicing" $
            let tight = PitchRange (Pitch 60) (Pitch 62)
             in assertBool "out of range" (not (inRange tight (chordTones cMaj7)))
        , testCase "allVoicings produces only in-range candidates" $
            let tight = PitchRange (Pitch 48) (Pitch 84)
                vs = allVoicings tight cMaj7
             in assertBool "all in range" (all (inRange tight) vs)
        , testCase "bestVoicing picks chord tones when previous is itself" $
            let prev = chordTones cMaj7
                v = bestVoicing wideRange prev cMaj7
             in v @?= prev
        , testCase "voiceLead returns one voicing per chord" $
            length (voiceLead wideRange [cMaj7, dMin7, g7, cMaj7]) @?= 4
        , testCase "voiceLead ii-V-I has small total movement" $
            let vs = voiceLead wideRange [dMin7, g7, cMaj7]
                cost = sum (zipWith voiceLeadingCost vs (drop 1 vs))
             in assertBool ("cost was " ++ show cost) (cost <= 12)
        , testCase "voicingToNotes produces a note per voice" $
            length (voicingToNotes (Duration 1) (Velocity 0.7) (chordTones cMaj7)) @?= 4
        ]
