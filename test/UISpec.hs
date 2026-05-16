module UISpec (tests) where

import Data.List (isInfixOf)
import Funktor.Core.Types
import Funktor.Grid (Color (..), Pad (..), PadAction (..), setPad)
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
        , testCase "renderUI paints each non-Off color through colorChar" $ do
            -- Paint one pad of each non-Off color so every colorChar
            -- alternative fires at least once. Place them off the cursor
            -- (cursor lives at (0,0) in initialUIState) so the symbol
            -- renders without [] framing and we can scan for ' X '.
            let colors = [Red, Green, Yellow, Blue, Purple, Cyan, White]
                placed =
                    foldr
                        (\(c, col) g -> setPad c 1 (Pad NoAction col) g)
                        initialUIState.uiGrid
                        (zip [0 :: Int ..] colors)
                rendered = unlines (renderUI initialUIState{uiGrid = placed})
            mapM_
                (\sym -> assertBool (sym ++ " present") ((" " ++ sym ++ " ") `isInfixOf` rendered))
                ["R", "G", "Y", "B", "P", "C", "W"]
        , testCase "renderUI brackets the cursor cell" $ do
            -- The (0,0) cursor on initialUIState renders as `[.]` (Off + cursor).
            let rendered = unlines (renderUI initialUIState)
            assertBool "[.] framed" ("[.]" `isInfixOf` rendered)
        ]
