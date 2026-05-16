{-# LANGUAGE ScopedTypeVariables #-}

module LaunchpadSpec (tests) where

import Funktor.Core.Types (Pitch (..), Velocity (..))
import Funktor.Grid (Color (..), emptyGrid)
import Funktor.Hardware.Launchpad (
    LaunchpadEvent (..),
    colorToRGB,
    defaultMk3Config,
    gridLedSysEx,
    gridToNote,
    ledSysEx,
    liveModeSysEx,
    midiToLaunchpadEvent,
    noteToGrid,
    programmerModeSysEx,
 )
import Funktor.Hardware.MIDI (MidiMessage (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (choose, forAll, testProperty)

tests :: TestTree
tests =
    testGroup
        "Hardware.Launchpad"
        [ noteGridTests
        , colorTests
        , sysexTests
        , eventTests
        ]

noteGridTests :: TestTree
noteGridTests =
    testGroup
        "note/grid coords"
        [ testProperty "noteToGrid . gridToNote == Just over [0..7]^2" $
            forAll (choose (0, 7)) $ \x ->
                forAll (choose (0, 7)) $ \y ->
                    noteToGrid (gridToNote x y) == Just (x, y)
        , testCase "bottom-left main pad is note 11" $
            gridToNote 0 0 @?= 11
        , testCase "top-right main pad is note 88" $
            gridToNote 7 7 @?= 88
        , testCase "top-row leftmost is note 91" $
            gridToNote 0 8 @?= 91
        , testCase "noteToGrid 0 is Nothing" $
            noteToGrid 0 @?= Nothing
        , testCase "noteToGrid 10 (column zero) is Nothing" $
            noteToGrid 10 @?= Nothing
        , testCase "noteToGrid 9 (row zero col 9) is Nothing" $
            noteToGrid 9 @?= Nothing
        , testCase "noteToGrid 100 (row beyond grid) is Nothing" $
            noteToGrid 100 @?= Nothing
        , testCase "noteToGrid 19 is right-column bottom" $
            noteToGrid 19 @?= Just (8, 0)
        , testCase "noteToGrid 99 is top-right logo position" $
            noteToGrid 99 @?= Just (8, 8)
        ]

colorTests :: TestTree
colorTests =
    testGroup
        "colorToRGB"
        [ testCase "Off is zeroes" $ colorToRGB Off @?= (0, 0, 0)
        , testCase "Red is full red" $ colorToRGB Red @?= (127, 0, 0)
        , testCase "Green is full green" $ colorToRGB Green @?= (0, 127, 0)
        , testCase "Yellow blends red+green" $ colorToRGB Yellow @?= (127, 127, 0)
        , testCase "Blue is full blue" $ colorToRGB Blue @?= (0, 0, 127)
        , testCase "Purple blends red+blue" $ colorToRGB Purple @?= (127, 0, 127)
        , testCase "Cyan blends green+blue" $ colorToRGB Cyan @?= (0, 127, 127)
        , testCase "White is full all" $ colorToRGB White @?= (127, 127, 127)
        ]

sysexTests :: TestTree
sysexTests =
    testGroup
        "SysEx payloads"
        [ testCase "programmerMode payload" $
            programmerModeSysEx defaultMk3Config
                @?= [0x00, 0x20, 0x29, 0x02, 0x0D, 0x0E, 0x01]
        , testCase "liveMode payload" $
            liveModeSysEx defaultMk3Config
                @?= [0x00, 0x20, 0x29, 0x02, 0x0D, 0x0E, 0x00]
        , testCase "single LED payload encodes RGB" $
            ledSysEx defaultMk3Config 11 Red
                @?= [0x00, 0x20, 0x29, 0x02, 0x0D, 0x03, 0x03, 11, 127, 0, 0]
        , testCase "gridLedSysEx on empty 8x8 grid has header + command + 64 zero LED specs" $
            length (gridLedSysEx defaultMk3Config (emptyGrid 8 8))
                @?= 5 + 1 + 64 * 5
        ]

eventTests :: TestTree
eventTests =
    testGroup
        "midiToLaunchpadEvent"
        [ testCase "NoteOn at note 11 -> PadDown (0,0)" $
            midiToLaunchpadEvent defaultMk3Config (NoteOn 0 (Pitch 11) (Velocity 1))
                @?= Just (PadDown (0, 0) (Velocity 1))
        , testCase "NoteOn at note 91 -> PadDown (0,8) (top row)" $
            midiToLaunchpadEvent defaultMk3Config (NoteOn 0 (Pitch 91) (Velocity 0.5))
                @?= Just (PadDown (0, 8) (Velocity 0.5))
        , testCase "NoteOff at note 88 -> PadUp (7,7)" $
            midiToLaunchpadEvent defaultMk3Config (NoteOff 0 (Pitch 88) (Velocity 0))
                @?= Just (PadUp (7, 7))
        , testCase "PolyAftertouch -> PadAftertouch" $
            midiToLaunchpadEvent defaultMk3Config (PolyAftertouch 0 (Pitch 44) 90)
                @?= Just (PadAftertouch (3, 3) 90)
        , testCase "ControlChange drops" $
            midiToLaunchpadEvent defaultMk3Config (ControlChange 0 7 100)
                @?= Nothing
        , testCase "Off-grid NoteOn drops" $
            midiToLaunchpadEvent defaultMk3Config (NoteOn 0 (Pitch 0) (Velocity 1))
                @?= Nothing
        , testCase "SysEx drops" $
            midiToLaunchpadEvent defaultMk3Config (SysEx [0x01]) @?= Nothing
        ]
