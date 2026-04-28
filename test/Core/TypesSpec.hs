{-# LANGUAGE ScopedTypeVariables #-}
module Core.TypesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Core.Types
import Test.Utils.Arbitrary ()

tests :: TestTree
tests = testGroup "Core.Types"
  [ testProperty "midi <-> freq round-trip" $ \(p :: Pitch) ->
        freqToMidi (midiToFreq p) == p

  , testProperty "Event Functor identity" $ \(e :: Event Int) ->
        fmap id e == e

  , testProperty "Event Functor composition" $ \(e :: Event Int) ->
        let f = (+ 1) :: Int -> Int
            g = (* 2) :: Int -> Int
        in fmap (f . g) e == (fmap f . fmap g) e

  , testCase "mapEventTime shifts beat" $ do
        let ev  = Event (Beat 1) "x"
            ev' = mapEventTime (+ Beat 2) ev
        eventBeat ev' @?= Beat 3

  , testCase "mapEventValue applies" $ do
        let ev  = Event (Beat 0) (5 :: Int)
            ev' = mapEventValue (+ 1) ev
        eventValue ev' @?= 6
  ]
