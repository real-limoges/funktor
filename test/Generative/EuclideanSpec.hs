module Generative.EuclideanSpec (tests) where

import Funktor.Core.Pattern (Pattern (..), duration)
import Funktor.Core.Types
import Funktor.Generative.Euclidean
import Test.QuickCheck (Positive (..), (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "Generative.Euclidean"
        [ testCase "tresillo: bjorklund 3 8" $
            bjorklund 3 8 @?= [True, False, False, True, False, False, True, False]
        , testCase "cinquillo: bjorklund 5 8" $
            bjorklund 5 8 @?= [True, False, True, True, False, True, True, False]
        , testCase "even 4 16 distributes one pulse every four steps" $
            bjorklund 4 16
                @?= [ True
                    , False
                    , False
                    , False
                    , True
                    , False
                    , False
                    , False
                    , True
                    , False
                    , False
                    , False
                    , True
                    , False
                    , False
                    , False
                    ]
        , testCase "k <= 0 yields all rests" $
            bjorklund 0 5 @?= replicate 5 False
        , testCase "k >= n yields all pulses" $
            bjorklund 7 5 @?= replicate 5 True
        , testProperty "length of bjorklund k n equals n" $
            \(Positive k) (Positive n) ->
                length (bjorklund k n) == n
        , testProperty "pulse count is min k n for k >= 0" $
            \(Positive k) (Positive n) ->
                length (filter id (bjorklund k n)) == min k n
        , testCase "euclidean 3 8 has 3 events over duration 8" $ do
            let p = euclidean 3 8
            length p.events @?= 3
            p.duration @?= Duration 8
        , testCase "euclideanWith respects the caller-supplied note" $ do
            let n_ = Note (Pitch 67) 2 0.5
                p = euclideanWith n_ 2 4
            map (.value) p.events @?= replicate 2 n_
        , testCase "rotateEuclidean shifts the first event" $ do
            let p = rotateEuclidean 2 3 8
            case p.events of
                (Event b _ : _) -> b @?= Beat 2
                [] -> error "rotateEuclidean produced empty events"
        , testProperty "polyEuclidean event count is sum of layer ks" $
            \(Positive a) (Positive b) (Positive c) ->
                (a + b + c) <= 64 ==>
                    let pat = polyEuclidean [(a, 8, Pitch 60), (b, 8, Pitch 67), (c, 8, Pitch 72)]
                     in length pat.events == min a 8 + min b 8 + min c 8
        ]
