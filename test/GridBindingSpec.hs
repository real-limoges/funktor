{-# LANGUAGE ScopedTypeVariables #-}

module GridBindingSpec (tests) where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Funktor.Audio.Scheduler (
    ScheduledEvent (..),
    SchedulerAction (..),
    SchedulerState (..),
    initialSchedulerState,
 )
import Funktor.Audio.State (createSineAudioState)
import Funktor.Core.Stream (Stream, runStream, silence)
import Funktor.Core.Types (
    Beat (..),
    Duration (..),
    Event (..),
    Note (..),
    Pitch (..),
    Tempo (..),
    Velocity (..),
 )
import Funktor.Grid (Color (..), Pad (..), gridPads)
import Funktor.Grid.Binding (
    AudioEngine (..),
    GridMode (..),
    InstrumentConfig (..),
    SequencerState (..),
    defaultInstrumentConfig,
    gridForMode,
    newAudioEngine,
    padToPitch,
    pressPad,
    releasePad,
    sequencerStream,
    setMode,
    setStep,
    topRowModeSwitch,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Grid.Binding"
        [ modeSwitchTests
        , sequencerStreamTests
        , gridForModeTests
        , padToPitchTests
        , setStepTests
        , pressPadSmokeTests
        ]

modeSwitchTests :: TestTree
modeSwitchTests =
    testGroup
        "topRowModeSwitch"
        [ testCase "col 0 picks Sequencer" $
            isSequencer (topRowModeSwitch 0) @?= True
        , testCase "col 1 picks Instrument" $
            isInstrument (topRowModeSwitch 1) @?= True
        , testCase "col 2 picks Scene" $
            isScene (topRowModeSwitch 2) @?= True
        , testCase "col 3 is unbound" $
            isNothingMode (topRowModeSwitch 3) @?= True
        , testCase "col 7 is unbound" $
            isNothingMode (topRowModeSwitch 7) @?= True
        ]
  where
    isSequencer (Just (SequencerMode _)) = True
    isSequencer _ = False
    isInstrument (Just (InstrumentMode _)) = True
    isInstrument _ = False
    isScene (Just (SceneMode _)) = True
    isScene _ = False
    isNothingMode Nothing = True
    isNothingMode _ = False

sequencerStreamTests :: TestTree
sequencerStreamTests =
    testGroup
        "sequencerStream"
        [ testCase "two toggled cells fire at expected beats and pitches" $
            let st =
                    SequencerState
                        { seqSteps =
                            V.fromList
                                [ V.fromList [True, False]
                                , V.fromList [False, True]
                                ]
                        , seqRoot = Pitch 60
                        , seqScale = [0, 2]
                        , seqStepDur = Duration 1
                        }
                evts = sample (sequencerStream st) (Beat 0) (Beat 2)
                pitchAt b = eventValue <$> lookup b [(eventBeat e, e) | e <- evts]
             in (notePitch <$> pitchAt (Beat 0), notePitch <$> pitchAt (Beat 1))
                    @?= (Just (Pitch 60), Just (Pitch 62))
        , testCase "all-off grid produces silence over query window" $
            let st =
                    SequencerState
                        { seqSteps = V.replicate 4 (V.replicate 4 False)
                        , seqRoot = Pitch 60
                        , seqScale = [0, 2, 4, 5]
                        , seqStepDur = Duration 1
                        }
             in sample (sequencerStream st) (Beat 0) (Beat 4) @?= []
        , testCase "empty step grid is silence" $
            sample (sequencerStream emptySeq) (Beat 0) (Beat 4) @?= []
        ]
  where
    sample :: Stream Note -> Beat -> Beat -> [Event Note]
    sample = runStream
    emptySeq =
        SequencerState
            { seqSteps = V.empty
            , seqRoot = Pitch 60
            , seqScale = []
            , seqStepDur = Duration 1
            }

gridForModeTests :: TestTree
gridForModeTests =
    testGroup
        "gridForMode"
        [ testCase "Instrument paints every pad Cyan" $
            allColors (gridForMode (InstrumentMode defaultInstrumentConfig))
                @?= replicate 64 Cyan
        , testCase "empty Sequencer leaves everything Off" $
            allColors
                ( gridForMode
                    (SequencerMode (defaultSeq{seqSteps = V.replicate 8 (V.replicate 8 False)}))
                )
                @?= replicate 64 Off
        , testCase "Sequencer with one toggled step lights one Yellow" $
            let st = defaultSeq{seqSteps = oneToggle 2 3}
                colors = allColors (gridForMode (SequencerMode st))
                yellowCount = length (filter (== Yellow) colors)
             in yellowCount @?= 1
        , testCase "Scene with two armed pads lights two Yellow" $
            let m = Map.fromList [((0, 0), silence), ((4, 5), silence)]
                colors = allColors (gridForMode (SceneMode m))
             in length (filter (== Yellow) colors) @?= 2
        ]
  where
    allColors g = [padColor p | row <- gridPads g, p <- row]
    defaultSeq =
        SequencerState
            { seqSteps = V.replicate 8 (V.replicate 8 False)
            , seqRoot = Pitch 60
            , seqScale = [0, 2, 4, 5, 7, 9, 11, 12]
            , seqStepDur = Duration 1
            }
    oneToggle x y =
        V.generate 8 $ \r ->
            V.generate 8 $ \c -> r == y && c == x

padToPitchTests :: TestTree
padToPitchTests =
    testGroup
        "padToPitch"
        [ testCase "instrument geometry combines column + row offsets" $
            padToPitch (InstrumentConfig (Pitch 36) 1 5) 3 2 @?= Pitch (36 + 3 + 10)
        , testCase "origin returns root" $
            padToPitch defaultInstrumentConfig 0 0 @?= Pitch 36
        ]

setStepTests :: TestTree
setStepTests =
    testGroup
        "setStep"
        [ testCase "toggling twice returns to original state" $
            let st = SequencerState (V.fromList [V.fromList [False, False]]) (Pitch 60) [0] (Duration 1)
                st' = setStep 0 0 (setStep 0 0 st)
             in seqSteps st' @?= seqSteps st
        , testCase "out-of-bounds is a no-op" $
            let st = SequencerState (V.fromList [V.fromList [False]]) (Pitch 60) [0] (Duration 1)
             in seqSteps (setStep 99 99 st) @?= seqSteps st
        ]

pressPadSmokeTests :: TestTree
pressPadSmokeTests =
    testGroup
        "pressPad / releasePad smoke"
        [ testCase "InstrumentMode press enqueues SchedNoteOn" $ do
            engine <- newEngine
            setMode engine (InstrumentMode defaultInstrumentConfig)
            pressPad 2 0 engine (Velocity 0.8)
            sched <- readTVarIO (engineSchedVar engine)
            let pending = map schedAction (schedPending sched)
            assertBool "expected a SchedNoteOn for column 2 of default instrument" $
                SchedNoteOn (Pitch 38) (Velocity 0.8) `elem` pending
        , testCase "InstrumentMode release enqueues SchedNoteOff" $ do
            engine <- newEngine
            setMode engine (InstrumentMode defaultInstrumentConfig)
            releasePad 0 1 engine
            sched <- readTVarIO (engineSchedVar engine)
            let pending = map schedAction (schedPending sched)
            assertBool "expected a SchedNoteOff for (0,1) of default instrument" $
                SchedNoteOff (Pitch 41) `elem` pending
        ]
  where
    newEngine = do
        audioVar <- newTVarIO (createSineAudioState 440 0)
        schedVar <- newTVarIO (initialSchedulerState silence (Tempo 120) 0)
        newAudioEngine audioVar schedVar
