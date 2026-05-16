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
            (append p q).duration == p.duration + q.duration
        , testProperty "shift inverse" $ \(d :: Beat) (p :: Pattern Int) ->
            shift d (shift (-d) p) == p
        , testCase "empty is empty" $
            isEmpty (empty :: Pattern Int) @?= True
        , testProperty "singleton not empty" $ \(dur :: Duration) (x :: Int) ->
            not (isEmpty (singleton dur x))
        , testProperty "pattern_ sorts events" $ \(dur :: Duration) (evs :: [Event Int]) ->
            let beats = map (.beat) (pattern_ dur evs).events
             in and (zipWith (<=) beats (drop 1 beats))
        , testProperty "repeat_ 1 is identity" $ \(p :: Pattern Int) ->
            repeat_ 1 p == p
        , testCase "repeat_ 0 yields empty" $
            repeat_ 0 (singleton 1 (1 :: Int)) @?= empty
        , testProperty "stack duration is max" $ \(p :: Pattern Int) (q :: Pattern Int) ->
            (stack p q).duration == max p.duration q.duration
        , testProperty "stack event set is union" $ \(p :: Pattern Int) (q :: Pattern Int) ->
            sort (stack p q).events == sort (p.events ++ q.events)
        , testProperty "scale 1 is identity" $ \(p :: Pattern Int) ->
            scale 1 p == p
        , testProperty "repeat_ duration scales" $ \(NonNegative n) (p :: Pattern Int) ->
            (repeat_ n p).duration == fromIntegral n * p.duration
        , testCase "pentatonic 4 pitches" $
            let evs = (pentatonic 4).events
                pitches = [unPitch e.value.pitch | e <- evs]
             in pitches @?= [60, 63, 65, 67, 70]
        , testProperty "mapEvents identity preserves events" $ \(p :: Pattern Int) ->
            mapEvents id p == p
        , testProperty "filterEvents (const True) is identity" $ \(p :: Pattern Int) ->
            filterEvents (const True) p == p
        , testProperty "filterEvents (const False) removes events" $ \(p :: Pattern Int) ->
            isEmpty (filterEvents (const False) p)
        , testProperty "fast 1 is identity" $ \(p :: Pattern Int) ->
            fast 1 p == p
        , testProperty "slow 1 is identity" $ \(p :: Pattern Int) ->
            slow 1 p == p
        , testCase "fast 2 halves duration" $
            (fast 2 (singleton 4 (0 :: Int))).duration @?= Duration 2
        , testCase "slow 2 doubles duration" $
            (slow 2 (singleton 4 (0 :: Int))).duration @?= Duration 8
        , testProperty "every 1 f equals f" $ \(p :: Pattern Int) ->
            every 1 (scale 2) p == scale 2 p
        , testProperty "every n id equals repeat_ n" $ \(NonNegative k) (p :: Pattern Int) ->
            let n = k + 1
             in every n id p == repeat_ n p
        , testCase "every n with duration-preserving f equals repeat_ n in duration" $
            let p = singleton 1 (0 :: Int)
             in (every 4 (shift (Beat 0)) p).duration @?= Duration 4
        , testCase "chunk 1 f equals f" $
            let p = singleton 1 (0 :: Int)
             in chunk 1 (scale 2) p @?= scale 2 p
        , testCase "chunk n id has duration n * pat.duration" $
            let p = singleton 1 (0 :: Int)
             in (chunk 3 id p).duration @?= Duration 3
        , testCase "sliceWindow restricts and shifts" $
            let p = pattern_ 4 [Event 0 (0 :: Int), Event 1 1, Event 2 2, Event 3 3]
                w = sliceWindow 1 3 p
             in (w.duration, map (.beat) w.events) @?= (Duration 2, [Beat 0, Beat 1])
        ]
