{-# LANGUAGE ScopedTypeVariables #-}

{-# HLINT ignore "Functor law" #-}

module Core.TypesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Core.Types
import Test.Utils.Arbitrary ()

tests :: TestTree
tests =
    testGroup
        "Core.Types"
        [ testProperty "midi <-> freq round-trip" $ \(p :: Pitch) ->
            freqToMidi (midiToFreq p) == p
        , testProperty "Event Functor identity" $ \(e :: Event Int) ->
            fmap id e == e
        , testProperty "Event Functor composition" $ \(e :: Event Int) ->
            let f = (+ 1) :: Int -> Int
                g = (* 2) :: Int -> Int
             in fmap (f . g) e == (fmap f . fmap g) e
        , testCase "mapEventTime shifts beat" $ do
            let ev = Event (Beat 1) "x"
                ev' = mapEventTime (+ Beat 2) ev
            eventBeat ev' @?= Beat 3
        , testCase "mapEventValue applies" $ do
            let ev = Event (Beat 0) (5 :: Int)
                ev' = mapEventValue (+ 1) ev
            eventValue ev' @?= 6
        , testCase "beatsToSeconds at 120 BPM" $
            -- 1 beat at 120 BPM = 0.5s
            beatsToSeconds (Tempo 120) 1 @?= 0.5
        , testCase "secondsToBeats inverts beatsToSeconds" $
            -- Round-trip: convert beats -> seconds -> beats
            let t = Tempo 100
                b = 2.5 :: Rational
                back = secondsToBeats t (beatsToSeconds t b)
             in abs (back - b) < 1e-9 @?= True
        , testCase "velocityToAmplitude maps to [0,1]" $
            let amps = [velocityToAmplitude (Velocity v) | v <- [0, 0.25, 0.5, 0.75, 1.0]]
             in all (\a -> a >= 0 && a <= 1) amps @?= True
        ]
