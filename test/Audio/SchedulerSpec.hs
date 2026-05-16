module Audio.SchedulerSpec (tests) where

import Data.List (sort)
import Funktor.Audio.Scheduler
import Funktor.Core.Stream (Stream, fromList)
import Funktor.Core.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

noteEv :: Beat -> Beat -> Pitch -> Velocity -> Event Note
noteEv s e p v = event (Arc s e) (Note p v)

oneNoteStream :: Stream Note
oneNoteStream = fromList [noteEv (Beat 1) (Beat 1.5) (Pitch 60) (Velocity 0.8)]

twoNoteStream :: Stream Note
twoNoteStream =
    fromList
        [ noteEv (Beat 0) (Beat 0.5) (Pitch 60) (Velocity 0.8)
        , noteEv (Beat 2) (Beat 2.5) (Pitch 64) (Velocity 0.8)
        ]

tests :: TestTree
tests =
    testGroup
        "Audio.Scheduler"
        [ testGroup
            "initialSchedulerState"
            [ testCase "carries its inputs" $
                let st = initialSchedulerState oneNoteStream (Tempo 120) 0
                 in do
                        st.tempo @?= Tempo 120
                        st.startTime @?= 0
                        st.beat @?= Beat 0
                        assertBool "no pending events" (null st.pending)
            , testCase "lookahead default is 100ms" $
                (initialSchedulerState oneNoteStream (Tempo 120) 0).lookAhead @?= 0.1
            ]
        , testGroup
            "step"
            [ testCase "advances beat by lookahead per call" $
                -- 120 BPM, 100ms lookahead = 0.2 beats per tick. secondsToBeats
                -- routes through Double, so the Rational result is the float
                -- approximation of 0.2, not exactly 1/5.
                let s0 = initialSchedulerState oneNoteStream (Tempo 120) 0
                    (s1, _) = step 0 s0
                    delta = abs (fromRational (unBeat s1.beat) - 0.2 :: Double)
                 in assertBool ("beat ~ 0.2, was " ++ show s1.beat) (delta < 1e-9)
            , testCase "first tick at t=0 finds nothing due" $
                let s0 = initialSchedulerState oneNoteStream (Tempo 120) 0
                    (_, due) = step 0 s0
                 in due @?= []
            , testCase "events that have passed wall-clock time become due" $
                let s0 = (initialSchedulerState twoNoteStream (Tempo 120) 0){lookAhead = 1.0}
                    (s1, _) = step 0 s0
                    (_, due) = step 0.01 s1
                    pitches = [p | ScheduledEvent _ (SchedNoteOn p _ _) <- due]
                 in pitches @?= [Pitch 60]
            , testCase "pending events stay sorted by time" $
                let s0 = (initialSchedulerState twoNoteStream (Tempo 120) 0){lookAhead = 2.0}
                    (s1, _) = step 0 s0
                    times = (.time) <$> s1.pending
                 in times @?= sort times
            ]
        ]
