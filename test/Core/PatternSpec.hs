{-# LANGUAGE ScopedTypeVariables #-}
module Core.PatternSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Core.Pattern
import Funktor.Core.Types
import Test.Utils.Arbitrary ()

tests :: TestTree
tests = testGroup "Core.Pattern"
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
        (repeat_ 0 (singleton 1 (1 :: Int))) @?= empty
  ]
