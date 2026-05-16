module Generative.EuclideanSpec (tests) where

import Funktor.Core.Stream (query)
import Funktor.Core.Types
import Funktor.Generative.Euclidean
import Test.QuickCheck (Positive (..), (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

oneCycle :: Beat -> Arc
oneCycle d = Arc (Beat 0) d

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
        , testCase "euclidean 3 8 emits 3 events per period" $ do
            let evs = query (euclidean 3 8) (oneCycle (Beat 8))
            length evs @?= 3
        , testCase "euclideanWith respects the caller-supplied note" $ do
            let n_ = Note (Pitch 67) 0.5
                evs = query (euclideanWith n_ 2 4) (oneCycle (Beat 4))
            map (.value) evs @?= replicate 2 n_
        , testCase "rotateEuclidean shifts events forward in time" $ do
            -- Query starting at the rotation offset so we skip the wrapped-in
            -- copy from the previous (negative-index) cycle. Original pulses
            -- at 0/3/6 become 2/5/8 after shifting forward by 2.
            let evs = query (rotateEuclidean 2 3 8) (Arc (Beat 2) (Beat 10))
                starts = map (.part.start) evs
            starts @?= [Beat 2, Beat 5, Beat 8]
        , testProperty "polyEuclidean event count is sum of layer ks" $
            \(Positive a) (Positive b) (Positive c) ->
                (a + b + c) <= 64 ==>
                    let s = polyEuclidean [(a, 8, Pitch 60), (b, 8, Pitch 67), (c, 8, Pitch 72)]
                        evs = query s (oneCycle (Beat 8))
                     in length evs == min a 8 + min b 8 + min c 8
        ]
