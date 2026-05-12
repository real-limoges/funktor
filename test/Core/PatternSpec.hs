{-# LANGUAGE ScopedTypeVariables #-}

module Core.PatternSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Data.List (sort)
import Funktor.Core.Pattern
import Funktor.Core.Types
import Test.QuickCheck (NonNegative (..))
import Test.Utils.Arbitrary ()

tests :: TestTree
tests =
    testGroup
        "Core.Pattern"
        [ testProperty "duration of append" $ \(p :: Pattern Int) (q :: Pattern Int) ->
            duration (append p q) == duration p + duration q
        , testProperty "shift inverse" $ \(d :: Beat) (p :: Pattern Int) ->
            shift d (shift (-d) p) == p
        , testCase "empty is empty" $
            isEmpty (empty :: Pattern Int) @?= True
        , testProperty "singleton not empty" $ \(dur :: Duration) (x :: Int) ->
            not (isEmpty (singleton dur x))
        , testProperty "pattern_ sorts events" $ \(dur :: Duration) (evs :: [Event Int]) ->
            let beats = map eventBeat (patternEvents (pattern_ dur evs))
             in and (zipWith (<=) beats (drop 1 beats))
        , testProperty "repeat_ 1 is identity" $ \(p :: Pattern Int) ->
            repeat_ 1 p == p
        , testCase "repeat_ 0 yields empty" $
            repeat_ 0 (singleton 1 (1 :: Int)) @?= empty
        , testProperty "stack duration is max" $ \(p :: Pattern Int) (q :: Pattern Int) ->
            duration (stack p q) == max (duration p) (duration q)
        , testProperty "stack event set is union" $ \(p :: Pattern Int) (q :: Pattern Int) ->
            sort (patternEvents (stack p q)) == sort (patternEvents p ++ patternEvents q)
        , testProperty "scale 1 is identity" $ \(p :: Pattern Int) ->
            scale 1 p == p
        , testProperty "repeat_ duration scales" $ \(NonNegative n) (p :: Pattern Int) ->
            duration (repeat_ n p) == fromIntegral n * duration p
        , testCase "pentatonic 4 pitches" $
            let evs = patternEvents (pentatonic 4)
                pitches = [unPitch (notePitch (eventValue e)) | e <- evs]
             in pitches @?= [60, 63, 65, 67, 70]
        , testProperty "mapEvents identity preserves events" $ \(p :: Pattern Int) ->
            mapEvents id p == p
        , testProperty "filterEvents (const True) is identity" $ \(p :: Pattern Int) ->
            filterEvents (const True) p == p
        , testProperty "filterEvents (const False) removes events" $ \(p :: Pattern Int) ->
            isEmpty (filterEvents (const False) p)
        ]
