module Core.TypesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Core.Types
import Test.Utils.Arbitrary ()

tests :: TestTree
tests = testGroup "Core.Types"
  [ testProperty "beats ↔ seconds round‑trip" $ \(tempo :: Tempo) (beat :: Beat) ->
        let secs = beatsToSeconds tempo beat
            beat' = secondsToBeats tempo secs
        in beat' == beat

  , testProperty "midi ↔ freq round‑trip" $ \(p :: Pitch) ->
        let f = midiToFreq p
            p' = freqToMidi f
        in p' == p

  , testProperty "velocity to amplitude monotonic" $ \v1 v2 ->
        (velocityToAmplitude v2 > velocityToAmplitude v1) ==> (v2 > v1)

  , testProperty "Event Functor identity" $ \(e :: Event Int) ->
        fmap id e == e

  , testProperty "Event Functor composition" $ \(f :: Int -> Int) (g :: Int -> Int) (e :: Event Int) ->
        fmap (f . g) e == (fmap f . fmap g) e

  , testCase "mapEventTime shifts beat" $ do
        let ev = Event (Beat 1) "x"
            ev' = mapEventTime (+ Beat 2) ev
        eventBeat ev' @?= Beat 3

  , testCase "mapEventValue applies" $ do
        let ev = Event (Beat 0) (5 :: Int)
            ev' = mapEventValue (+1) ev
        eventValue ev' @?= 6
  ]
