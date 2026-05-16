{-# LANGUAGE ScopedTypeVariables #-}

module Core.StreamSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.List (sort)
import Funktor.Core.Stream
import Funktor.Core.Types
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Utils.Arbitrary ()

singleEv :: Beat -> Beat -> a -> Event a
singleEv s e v = event (Arc s e) v

sampleArc :: Beat -> Beat -> Arc
sampleArc = Arc

tests :: TestTree
tests =
    testGroup
        "Core.Stream"
        [ testProperty "query events have part.start inside [t0, t1)" $
            \(s :: Stream Int) (Beat b0) (Beat b1) ->
                let (lo, hi) = if b0 <= b1 then (b0, b1) else (b1, b0)
                    evs = s.query (sampleArc (Beat lo) (Beat hi))
                    inWindow e =
                        let p = unBeat e.part.start
                         in p >= lo && p < hi
                 in all inWindow evs
        , testProperty "shiftStream composes additively" $
            \(s :: Stream Int) (d1 :: Beat) (d2 :: Beat) (a :: Arc) ->
                let left = (shiftStream (d1 + d2) s).query a
                    right = (shiftStream d2 (shiftStream d1 s)).query a
                 in left == right
        , testProperty "merge is commutative on event sets" $
            \(s1 :: Stream Int) (s2 :: Stream Int) (a :: Arc) ->
                sort ((merge s1 s2).query a) == sort ((merge s2 s1).query a)
        , testCase "silence yields no events" $
            (silence :: Stream Int).query (sampleArc (Beat 0) (Beat 100)) @?= []
        , testProperty "mergeMany [] equals silence" $ \(a :: Arc) ->
            (mergeMany ([] :: [Stream Int])).query a == []
        , testProperty "stack is alias for mergeMany" $
            \(ss :: [Stream Int]) (a :: Arc) ->
                sort ((stack ss).query a) == sort ((mergeMany ss).query a)
        , testProperty "fromList returns events in part.start order" $
            \(evs :: [Event Int]) (a :: Arc) ->
                let out = (fromList evs).query a
                    starts = map (unBeat . (.start) . (.part)) out
                 in and (zipWith (<=) starts (drop 1 starts))
        , testCase "periodic loops events every period" $
            let evs = [singleEv (Beat 0) (Beat 1) (1 :: Int)]
                s = periodic (Duration 2) evs
                -- query 4 beats expects events at 0, 2 (two cycles)
                starts = map (unBeat . (.part.start)) (s.query (sampleArc (Beat 0) (Beat 4)))
             in starts @?= [0, 2]
        , testCase "periodic with period 0 is silence" $
            (periodic (Duration 0) [singleEv 0 1 (1 :: Int)]).query (sampleArc 0 10) @?= []
        , testCase "singleton plays its event once per period" $
            let s = singleton (Duration 2) (5 :: Int)
                out = s.query (sampleArc 0 6)
             in map (.value) out @?= [5, 5, 5]
        , testCase "slow 2 doubles event extents" $
            let s = singleton (Duration 1) (1 :: Int)
                -- s emits an event at beat 0 with whole=Arc 0 1, looping every 1 beat
                -- slow 2 makes it loop every 2 beats; first event has whole=Arc 0 2
                slowed = slow 2 s
                out = slowed.query (sampleArc 0 2)
             in case out of
                    [e] -> (e.whole.start, e.whole.end) @?= (Beat 0, Beat 2)
                    _ -> error ("expected one event, got " ++ show (length out))
        , testCase "fast 2 halves the period" $
            let s = singleton (Duration 4) (1 :: Int)
                fastened = fast 2 s
                out = fastened.query (sampleArc 0 4)
             in length out @?= 2
        , testCase "cat alternates streams in time" $
            let s1 = fromList [singleEv (Beat 0) (Beat 1) (1 :: Int)]
                s2 = fromList [singleEv (Beat 0) (Beat 1) 2]
                c = cat (Duration 2) [s1, s2]
                out = c.query (sampleArc 0 4)
                vals = map (.value) out
             in vals @?= [1, 2]
        , testCase "sometimes p=0 leaves stream unchanged" $
            let s = fromList [singleEv (Beat 0) (Beat 1) (1 :: Int)]
                t = sometimes 0 1 (shiftStream (Beat 10)) s
             in t.query (sampleArc 0 1) @?= [singleEv (Beat 0) (Beat 1) 1]
        , testCase "sometimes p=1 always applies f" $
            let s = fromList [singleEv (Beat 0) (Beat 1) (1 :: Int)]
                t = sometimes 1 1 (shiftStream (Beat 5)) s
             in t.query (sampleArc 0 1) @?= []
        , testCase "sometimes re-rolls per cycle: different cycles may differ" $
            -- Use 'const silence' as the f branch so cycles that pick f
            -- drop their event entirely. With prob 0.5 over 20 cycles we
            -- expect a partial result, not all-or-nothing.
            let s = periodic (Duration 1) [event (Arc 0 1) (1 :: Int)]
                t = sometimes 0.5 42 (const silence) s
                evs = t.query (sampleArc 0 20)
             in assertBool
                    ("expected partial filter, got " ++ show (length evs) ++ " events")
                    (length evs < 20 && length evs > 0)
        , testCase "everyN 1 f equals f" $
            let s = fromList [singleEv (Beat 0) (Beat 1) (1 :: Int)]
                t = everyN 1 (shiftStream (Beat 100)) s
             in t.query (sampleArc 0 10) @?= []
        , testCase "everyN 2 keeps events on cycle 0 transformed" $
            let s = fromList [singleEv (Beat 0) (Beat 1) (1 :: Int), singleEv (Beat 1) (Beat 2) 2]
                t = everyN 2 (fmap (* 10)) s
                out = t.query (sampleArc 0 2)
             in map (.value) out @?= [10, 2]
        , testCase "pentatonic 4 pitches" $
            let evs = (pentatonic 4).query (sampleArc 0 5)
                pitches = [unPitch e.value.pitch | e <- evs]
             in pitches @?= [60, 63, 65, 67, 70]
        , testCase "runStream legacy alias matches query" $
            let s = fromList [singleEv (Beat 0) (Beat 1) (1 :: Int)]
             in runStream s (Beat 0) (Beat 10) @?= s.query (sampleArc 0 10)
        , assertSilent
        ]
  where
    assertSilent =
        testCase "fast 0 collapses to silence" $
            assertBool "no events" (null ((fast 0 (singleton (Duration 1) (1 :: Int))).query (sampleArc 0 10)))
