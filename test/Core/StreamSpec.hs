{-# LANGUAGE ScopedTypeVariables #-}

module Core.StreamSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.List (sort)
import Funktor.Core.Pattern (Pattern)
import Funktor.Core.Stream
import Funktor.Core.Types
import System.Random (mkStdGen)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Utils.Arbitrary ()

tests :: TestTree
tests =
    testGroup
        "Core.Stream"
        [ testProperty "runStream events fall within window" $
            \(p :: Pattern Int) (Beat b0) (Beat b1) ->
                let (lo, hi) = if b0 <= b1 then (b0, b1) else (b1, b0)
                    evs = runStream (fromPattern p) (Beat lo) (Beat hi)
                 in all (\(Event (Beat b) _) -> b >= lo && b < hi) evs
        , testProperty "shiftStream composes additively" $
            \(p :: Pattern Int) (d1 :: Beat) (d2 :: Beat) (t0 :: Beat) (t1 :: Beat) ->
                let s = fromPattern p
                    left = runStream (shiftStream (d1 + d2) s) t0 t1
                    right = runStream (shiftStream d2 (shiftStream d1 s)) t0 t1
                 in left == right
        , testProperty "merge is commutative on event sets" $
            \(p :: Pattern Int) (q :: Pattern Int) (t0 :: Beat) (t1 :: Beat) ->
                let s1 = fromPattern p
                    s2 = fromPattern q
                    ev1 = runStream (merge s1 s2) t0 t1
                    ev2 = runStream (merge s2 s1) t0 t1
                 in sort ev1 == sort ev2
        , testCase "silence yields no events" $
            runStream (silence :: Stream Int) (Beat 0) (Beat 100) @?= []
        , testProperty "mergeMany [] equals silence" $ \(t0 :: Beat) (t1 :: Beat) ->
            runStream (mergeMany ([] :: [Stream Int])) t0 t1 == []
        , testProperty "mergeMany [s] preserves events (mod order)" $
            \(p :: Pattern Int) (t0 :: Beat) (t1 :: Beat) ->
                let s = fromPattern p
                 in sort (runStream (mergeMany [s]) t0 t1) == sort (runStream s t0 t1)
        , testProperty "fromList returns events in beat order" $
            \(evs :: [Event Int]) (t0 :: Beat) (t1 :: Beat) ->
                let out = runStream (fromList evs) t0 t1
                    beats = map (.beat) out
                 in and (zipWith (<=) beats (drop 1 beats))
        , testCase "sometimes p=0 leaves stream unchanged" $
            let s = fromList [Event (Beat 0) (1 :: Int)]
                t = sometimes 0 (mkStdGen 1) (shiftStream (Beat 10)) s
             in runStream t (Beat 0) (Beat 1) @?= [Event (Beat 0) 1]
        , testCase "sometimes p=1 always applies f" $
            let s = fromList [Event (Beat 0) (1 :: Int)]
                t = sometimes 1 (mkStdGen 1) (shiftStream (Beat 5)) s
             in runStream t (Beat 0) (Beat 1) @?= []
        , testCase "everyN 1 f equals f" $
            let s = fromList [Event (Beat 0) (1 :: Int), Event (Beat 2) 2]
                t = everyN 1 (shiftStream (Beat 100)) s
             in runStream t (Beat 0) (Beat 10) @?= []
        , testCase "everyN n=2 keeps events on cycle 0 transformed" $
            let s = fromList [Event (Beat 0) (1 :: Int), Event (Beat 1) 2]
                -- 0.0 is in cycle 0 (multiple of 2), so it's transformed (dropped from window)
                -- 1.0 is in cycle 1, untouched
                t = everyN 2 (fmap (* 10)) s
                out = runStream t (Beat 0) (Beat 2)
             in map (.value) out @?= [10, 2]
        ]
