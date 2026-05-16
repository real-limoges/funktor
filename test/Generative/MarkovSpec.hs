module Generative.MarkovSpec (tests) where

import Data.Map.Strict qualified as Map
import Funktor.Core.Stream (query)
import Funktor.Core.Types
import Funktor.Generative.Markov
import System.Random (mkStdGen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

simpleChain :: MarkovChain Int
simpleChain =
    MarkovChain $
        Map.fromList
            [ (0, [(1.0, 1)])
            , (1, [(1.0, 2)])
            , (2, [(1.0, 0)])
            ]

tests :: TestTree
tests =
    testGroup
        "Generative.Markov"
        [ testCase "weightedChoice picks the only option" $
            let (x, _) = weightedChoice [(1.0, 'a')] (mkStdGen 0)
             in x @?= 'a'
        , testCase "weightedChoice with extreme weight always picks heavy option" $
            let outcomes = [fst (weightedChoice [(0.001, 'a'), (999.0, 'b')] (mkStdGen i)) | i <- [1 .. 50]]
             in assertBool "biased toward b" (length (filter (== 'b') outcomes) >= 49)
        , testCase "step on terminal state stays put" $
            let chain = MarkovChain (Map.fromList [(0 :: Int, [])])
                (s', _) = step chain 0 (mkStdGen 1)
             in s' @?= 0
        , testCase "generate produces deterministic walk with seed" $
            let walk1 = take 20 (generate simpleChain 0 (mkStdGen 42))
                walk2 = take 20 (generate simpleChain 0 (mkStdGen 42))
             in walk1 @?= walk2
        , testCase "generate cycles through 3-state deterministic chain" $
            let walk = take 7 (generate simpleChain 0 (mkStdGen 1))
             in walk @?= [0, 1, 2, 0, 1, 2, 0]
        , testCase "runChain materialises events at stepDur beats" $
            let s = runChain simpleChain (Duration 2) 0 (mkStdGen 1)
                evs = query s (Arc (Beat 0) (Beat 8))
             in map (.part.start) evs @?= [Beat 0, Beat 2, Beat 4, Beat 6]
        , testCase "jazzBluesChain includes ii-V-I motion" $
            let i7 = ChordSymbol (Pitch 60) Dominant7
                walk = take 100 (generate jazzBluesChain i7 (mkStdGen 7))
                pairs = zip walk (drop 1 walk)
                ii7 = ChordSymbol (Pitch 62) Minor7
                v7 = ChordSymbol (Pitch 67) Dominant7
             in assertBool "contains ii->V transition" ((ii7, v7) `elem` pairs)
        ]
