module Generative.CellularAutomataSpec (tests) where

import Data.Vector.Unboxed qualified as V
import Funktor.Core.Stream (query)
import Funktor.Core.Types
import Funktor.Generative.CellularAutomata
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

oneCycle :: Beat -> Arc
oneCycle d = Arc (Beat 0) d

tests :: TestTree
tests =
    testGroup
        "Generative.CellularAutomata"
        [ testCase "rule30 truth table" $ do
            -- Rule 30 = 00011110 binary, so the live patterns are
            -- (l,c,r) indexed by 4l + 2c + r:
            applyRule rule30 False False False @?= False -- 0
            applyRule rule30 False False True @?= True -- 1
            applyRule rule30 False True False @?= True -- 2
            applyRule rule30 False True True @?= True -- 3
            applyRule rule30 True False False @?= True -- 4
            applyRule rule30 True False True @?= False -- 5
            applyRule rule30 True True False @?= False -- 6
            applyRule rule30 True True True @?= False -- 7
        , testCase "centerSeed places a single true cell at n/2" $ do
            let s = centerSeed 7
            V.toList s @?= [False, False, False, True, False, False, False]
        , testCase "centerSeed 0 is empty" $
            centerSeed 0 @?= V.empty
        , testCase "evolve rule30 over one step matches known output" $ do
            -- Starting from single live cell, rule 30 produces 1 1 1 surrounding it.
            let g1 = evolve rule30 (centerSeed 5)
            V.toList g1 @?= [False, True, True, True, False]
        , testCase "generations rule90 step-by-step matches sierpinski" $ do
            -- Rule 90 produces the Sierpinski triangle pattern.
            let g = generations rule90 3 (centerSeed 7)
                rows = map V.toList g
            length g @?= 3
            -- Generation 0 is the seed
            rows !! 0 @?= [False, False, False, True, False, False, False]
            -- Generation 1: neighbours of the center fire
            rows !! 1 @?= [False, False, True, False, True, False, False]
        , testCase "rowToStream emits one event per live cell" $ do
            let row = V.fromList [True, False, True, False]
                s = rowToStream (Note (Pitch 60) 1.0) row
                evs = query s (oneCycle (Beat 4))
            length evs @?= 2
        , testCase "columnDensity counts live cells per column" $ do
            let rs =
                    [ V.fromList [True, False, True]
                    , V.fromList [True, True, False]
                    , V.fromList [False, False, True]
                    ]
            columnDensity rs @?= [2, 1, 2]
        , testCase "caRhythm rule30 5 has at least one event in one period" $ do
            let evs = query (caRhythm rule30 5) (oneCycle (Beat 5))
            assertBool "non-empty" (not (null evs))
        , testCase "caSequence returns one stream per generation" $
            length (caSequence rule30 4 5) @?= 4
        , testCase "caStream with empty pitches is silence" $
            query (caStream rule30 3 5 []) (oneCycle (Beat 15)) @?= []
        ]
