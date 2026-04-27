module Core.StreamSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Core.Stream
import Funktor.Core.Pattern
import Funktor.Core.Types
import Test.Utils.Arbitrary ()

-- Helper to generate a random Pattern and wrap it as a Stream
fromPatternArb :: Pattern Int -> Stream Int
fromPatternArb = fromPattern

tests :: TestTree
tests = testGroup "Core.Stream"
  [ testProperty "runStream respects bounds" $ \p (t0 :: Beat) (t1 :: Beat) ->
        let s = fromPatternArb p
            (Beat b0, Beat b1) = (t0, t1)
            (lo, hi) = if b0 <= b1 then (b0, b1) else (b1, b0)
            evs = runStream s (Beat lo) (Beat hi)
        in all (\(Event (Beat b) _) -> b >= lo && b < hi) evs

  , testProperty "looping equivalence" $ \(p :: Pattern Int) n ->
        n >= 0 &&
        let dur = unDuration $ duration p
            s = fromPatternArb p
            start = Beat (fromIntegral n * dur)
            end   = Beat (fromIntegral (n+1) * dur)
            evs1 = runStream s start end
            evs2 = map (\(Event (Beat b) v) -> Event (Beat (b - fromIntegral n * dur)) v) (runStream s (Beat 0) (Beat dur))
        in evs1 == evs2

  , testProperty "mapStream identity" $ \p (t0 :: Beat) (t1 :: Beat) ->
        let s = fromPatternArb p
            evs1 = runStream (mapStream id s) t0 t1
            evs2 = runStream s t0 t1
        in evs1 == evs2

  , testProperty "shiftStream composition" $ \p (d1 :: Beat) (d2 :: Beat) t0 t1 ->
        let s = fromPatternArb p
            left  = runStream (shiftStream (d1 + d2) s) t0 t1
            right = runStream (shiftStream d2 (shiftStream d1 s)) t0 t1
        in left == right

  , testProperty "merge commutative" $ \p q t0 t1 ->
        let s1 = fromPatternArb p
            s2 = fromPatternArb q
            ev1 = runStream (merge s1 s2) t0 t1
            ev2 = runStream (merge s2 s1) t0 t1
        in ev1 == ev2
  ]
