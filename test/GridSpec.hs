module GridSpec (tests) where

import Funktor.Grid
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Grid"
        [ testCase "setPad out-of-bounds does not alter grid" $ do
            let g = emptyGrid 4 4
                g' = setPad (-1) (-1) (Pad NoAction Red) g
            assertBool "grid unchanged" (g == g')
        , testCase "setPad stores pad correctly" $ do
            let g = emptyGrid 4 4
                pad = Pad NoAction Cyan
                g' = setPad 1 2 pad g
            getPad 1 2 g' @?= Just pad
        , testCase "getPad out-of-bounds yields Nothing" $
            getPad 10 10 (emptyGrid 4 4) @?= Nothing
        , testCase "emptyGrid records its dimensions" $ do
            gridWidth (emptyGrid 5 3) @?= 5
            gridHeight (emptyGrid 5 3) @?= 3
        , testCase "emptyGrid pads are all NoAction/Off" $
            let g = emptyGrid 4 4
                pads = concat (gridPads g)
             in assertBool "all default" (all (== Pad NoAction Off) pads)
        , testCase "setPad then getPad round-trips" $
            let pad = Pad (TriggerPattern 3) Blue
                g = setPad 2 1 pad (emptyGrid 4 4)
             in getPad 2 1 g @?= Just pad
        ]
