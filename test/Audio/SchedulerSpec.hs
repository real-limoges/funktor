module Audio.SchedulerSpec (tests) where

import Funktor.Audio.Scheduler
import Funktor.Core.Stream (Stream, fromList)
import Funktor.Core.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

oneNoteStream :: Stream Note
oneNoteStream = fromList [Event (Beat 1) (Note (Pitch 60) (Duration 0.5) (Velocity 0.8))]

tests :: TestTree
tests =
    testGroup
        "Audio.Scheduler"
        [ testCase "initialSchedulerState carries its inputs" $
            let st = initialSchedulerState oneNoteStream (Tempo 120) 0
             in do
                    schedTempo st @?= Tempo 120
                    schedStartTime st @?= 0
                    schedBeat st @?= Beat 0
                    assertBool "no pending events" (null (schedPending st))
        , testCase "lookahead default is 100ms" $
            schedLookAhead (initialSchedulerState oneNoteStream (Tempo 120) 0) @?= 0.1
        ]
