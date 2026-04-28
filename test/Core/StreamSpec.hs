{-# LANGUAGE ScopedTypeVariables #-}
module Core.StreamSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Core.Stream
import Funktor.Core.Pattern (Pattern)
import Funktor.Core.Types
import Test.Utils.Arbitrary ()
import Data.List (sort)

tests :: TestTree
tests = testGroup "Core.Stream"
  [ testProperty "runStream events fall within window" $
      \(p :: Pattern Int) (Beat b0) (Beat b1) ->
        let (lo, hi) = if b0 <= b1 then (b0, b1) else (b1, b0)
            evs     = runStream (fromPattern p) (Beat lo) (Beat hi)
        in all (\(Event (Beat b) _) -> b >= lo && b < hi) evs

  , testProperty "shiftStream composes additively" $
      \(p :: Pattern Int) (d1 :: Beat) (d2 :: Beat) (t0 :: Beat) (t1 :: Beat) ->
        let s     = fromPattern p
            left  = runStream (shiftStream (d1 + d2) s) t0 t1
            right = runStream (shiftStream d2 (shiftStream d1 s)) t0 t1
        in left == right

  , testProperty "merge is commutative on event sets" $
      \(p :: Pattern Int) (q :: Pattern Int) (t0 :: Beat) (t1 :: Beat) ->
        let s1  = fromPattern p
            s2  = fromPattern q
            ev1 = runStream (merge s1 s2) t0 t1
            ev2 = runStream (merge s2 s1) t0 t1
        in sort ev1 == sort ev2
  ]
