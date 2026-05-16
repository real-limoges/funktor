module Audio.TimbreSpec (tests) where

import Funktor.Audio.Timbre
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Audio.Timbre"
        [ testCase "defaultTimbre points at funktor_note" $
            defaultTimbre.synthDef @?= "funktor_note"
        , testCase "defaultTimbre carries no overrides" $
            defaultTimbre.params @?= []
        , testCase "waveformParam encodes each waveform as its enum index" $ do
            waveformParam Sine @?= ("wave", 0)
            waveformParam Sawtooth @?= ("wave", 1)
            waveformParam Square @?= ("wave", 2)
            waveformParam Triangle @?= ("wave", 3)
        , testCase "adsr builds the four named params" $
            (fst <$> adsr 0.01 0.1 0.7 0.3)
                @?= ["attack", "decay", "sustain", "release"]
        , testCase "adsr keeps numeric values in order" $
            (snd <$> adsr 0.01 0.1 0.7 0.3)
                @?= [0.01, 0.1, 0.7, 0.3]
        ]
