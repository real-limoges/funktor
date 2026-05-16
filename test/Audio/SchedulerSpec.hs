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
                    st.tempo @?= Tempo 120
                    st.startTime @?= 0
                    st.beat @?= Beat 0
                    assertBool "no pending events" (null st.pending)
        , testCase "lookahead default is 100ms" $
            (initialSchedulerState oneNoteStream (Tempo 120) 0).lookAhead @?= 0.1
        ]
