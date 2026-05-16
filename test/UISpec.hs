module UISpec (tests) where

import Funktor.Core.Types
import Funktor.UI
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "UI"
        [ testCase "Tick is identity" $
            applyEvent Tick initialUIState @?= initialUIState
        , testCase "SetTempo updates tempo" $
            (applyEvent (SetTempo (Tempo 140)) initialUIState).uiTempo @?= Tempo 140
        , testCase "SetBeat updates beat" $
            (applyEvent (SetBeat (Beat 5)) initialUIState).uiCurrentBeat @?= Beat 5
        , testCase "SetPlaying toggles transport" $ do
            let s = applyEvent (SetPlaying True) initialUIState
            s.uiPlaying @?= True
            (applyEvent (SetPlaying False) s).uiPlaying @?= False
        , testCase "MoveCursor clamps to 0..7" $ do
            let s1 = applyEvent (MoveCursor (-3) 99) initialUIState
            s1.uiCursorRow @?= 0
            s1.uiCursorCol @?= 7
        , testCase "renderUI starts with the title line" $ do
            let ls = renderUI initialUIState
            assertBool "has title" (any (\l -> take 7 l == "Funktor") ls)
        , testCase "renderUI Stopped vs Playing" $ do
            let playing = applyEvent (SetPlaying True) initialUIState
            assertBool "playing title" (any (\l -> drop (length "Funktor — ") l == "Playing") (renderUI playing))
            assertBool "stopped title" (any (\l -> drop (length "Funktor — ") l == "Stopped") (renderUI initialUIState))
        , testCase "renderUI emits one line per grid row plus header lines" $
            -- 3 header lines (title, transport, blank) + 8 grid rows
            length (renderUI initialUIState) @?= 11
        ]
