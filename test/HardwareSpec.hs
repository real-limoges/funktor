{-# LANGUAGE ScopedTypeVariables #-}

module HardwareSpec (tests) where

import Data.Bits (shiftL, (.|.))
import Data.Word (Word8)
import Foreign.C.Types (CLong)
import Funktor.Audio.Scheduler (SchedulerAction (..))
import Funktor.Audio.Timbre (defaultTimbre)
import Funktor.Core.Types (Pitch (..), Velocity (..))
import Funktor.Hardware.MIDI (
    MidiMessage (..),
    RxState (..),
    encodeMidiMessage,
    initialRxState,
    midiToSchedAction,
    parseMidiMessage,
    stepRx,
 )
import Sound.PortMidi qualified as PM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (
    Arbitrary (..),
    Gen,
    choose,
    listOf,
    oneof,
    testProperty,
    (===),
 )

tests :: TestTree
tests =
    testGroup
        "Hardware.MIDI"
        [ parseTests
        , encodeTests
        , stepRxTests
        , routerTests
        ]

routerTests :: TestTree
routerTests =
    testGroup
        "midiToSchedAction"
        [ testCase "NoteOn routes to SchedNoteOn" $
            midiToSchedAction (NoteOn 0 (Pitch 60) (Velocity 1))
                @?= Just (SchedNoteOn (Pitch 60) (Velocity 1) defaultTimbre)
        , testCase "NoteOff routes to SchedNoteOff" $
            midiToSchedAction (NoteOff 5 (Pitch 64) (Velocity 0))
                @?= Just (SchedNoteOff (Pitch 64))
        , testCase "Channel is ignored (mono collapse)" $
            midiToSchedAction (NoteOn 7 (Pitch 60) (Velocity 0.5))
                @?= Just (SchedNoteOn (Pitch 60) (Velocity 0.5) defaultTimbre)
        , testCase "ControlChange drops to Nothing" $
            midiToSchedAction (ControlChange 0 7 100) @?= Nothing
        , testCase "PitchBend drops to Nothing" $
            midiToSchedAction (PitchBend 0 8192) @?= Nothing
        , testCase "SysEx drops to Nothing" $
            midiToSchedAction (SysEx [0x01, 0x02]) @?= Nothing
        , testCase "PolyAftertouch drops to Nothing" $
            midiToSchedAction (PolyAftertouch 0 (Pitch 60) 90) @?= Nothing
        , testCase "ProgramChange drops to Nothing" $
            midiToSchedAction (ProgramChange 0 12) @?= Nothing
        , testCase "Unknown drops to Nothing" $
            midiToSchedAction (Unknown [0x00]) @?= Nothing
        ]

parseTests :: TestTree
parseTests =
    testGroup
        "parseMidiMessage"
        [ testCase "NoteOn channel 0 max velocity" $
            parseMidiMessage [0x90, 60, 127]
                @?= NoteOn 0 (Pitch 60) (Velocity 1)
        , testCase "NoteOn channel 5 mid velocity" $
            parseMidiMessage [0x95, 64, 64]
                @?= NoteOn 5 (Pitch 64) (Velocity (64 / 127))
        , testCase "NoteOn vel=0 normalises to NoteOff" $
            parseMidiMessage [0x90, 60, 0]
                @?= NoteOff 0 (Pitch 60) (Velocity 0)
        , testCase "NoteOff channel 15" $
            parseMidiMessage [0x8F, 60, 64]
                @?= NoteOff 15 (Pitch 60) (Velocity (64 / 127))
        , testCase "ControlChange" $
            parseMidiMessage [0xB1, 7, 100]
                @?= ControlChange 1 7 100
        , testCase "PitchBend center" $
            parseMidiMessage [0xE0, 0x00, 0x40]
                @?= PitchBend 0 8192
        , testCase "PitchBend max" $
            parseMidiMessage [0xE0, 0x7F, 0x7F]
                @?= PitchBend 0 16383
        , testCase "PitchBend zero" $
            parseMidiMessage [0xE0, 0x00, 0x00]
                @?= PitchBend 0 0
        , testCase "PolyAftertouch" $
            parseMidiMessage [0xA2, 60, 90]
                @?= PolyAftertouch 2 (Pitch 60) 90
        , testCase "ChannelAftertouch" $
            parseMidiMessage [0xD3, 64]
                @?= ChannelAftertouch 3 64
        , testCase "ProgramChange" $
            parseMidiMessage [0xC4, 12]
                @?= ProgramChange 4 12
        , testCase "SysEx strips F0/F7 framing" $
            parseMidiMessage [0xF0, 0x7E, 0x7F, 0x06, 0x01, 0xF7]
                @?= SysEx [0x7E, 0x7F, 0x06, 0x01]
        , testCase "SysEx without terminator accepts payload" $
            parseMidiMessage [0xF0, 0x7E, 0x7F]
                @?= SysEx [0x7E, 0x7F]
        , testCase "Empty input is Unknown" $
            parseMidiMessage [] @?= Unknown []
        , testCase "Unrecognised status is Unknown" $
            parseMidiMessage [0x00, 0x01]
                @?= Unknown [0x00, 0x01]
        , testCase "Truncated NoteOn is Unknown" $
            parseMidiMessage [0x90, 60]
                @?= Unknown [0x90, 60]
        , testCase "Channel nibble masked from status byte" $
            parseMidiMessage [0x9F, 60, 100]
                @?= NoteOn 15 (Pitch 60) (Velocity (100 / 127))
        ]

encodeTests :: TestTree
encodeTests =
    testGroup
        "encodeMidiMessage"
        [ testCase "NoteOn round" $
            encodeMidiMessage (NoteOn 0 (Pitch 60) (Velocity 1))
                @?= [0x90, 60, 127]
        , testCase "NoteOff channel 5" $
            encodeMidiMessage (NoteOff 5 (Pitch 64) (Velocity 0))
                @?= [0x85, 64, 0]
        , testCase "ControlChange round" $
            encodeMidiMessage (ControlChange 1 7 100) @?= [0xB1, 7, 100]
        , testCase "PitchBend center encodes" $
            encodeMidiMessage (PitchBend 0 8192) @?= [0xE0, 0x00, 0x40]
        , testCase "PitchBend max encodes" $
            encodeMidiMessage (PitchBend 0 16383) @?= [0xE0, 0x7F, 0x7F]
        , testCase "SysEx encodes with framing" $
            encodeMidiMessage (SysEx [0x7E, 0x7F])
                @?= [0xF0, 0x7E, 0x7F, 0xF7]
        , testCase "Unknown round-trips raw bytes" $
            encodeMidiMessage (Unknown [0x00, 0x01])
                @?= [0x00, 0x01]
        , testProperty "parse . encode == id (encodable subset)" $ \msg ->
            parseMidiMessage (encodeMidiMessage msg) === msg
        ]

stepRxTests :: TestTree
stepRxTests =
    testGroup
        "stepRx"
        [ testCase "single-event sysex in one packet" $
            let evs = [event [0xF0, 0x7E, 0x06, 0xF7]]
                (msgs, st) = stepRx initialRxState evs
             in (msgs, st.inSysEx) @?= ([SysEx [0x7E, 0x06]], False)
        , testCase "multi-packet sysex reassembles in order" $
            let evs =
                    [ event [0xF0, 0x7E, 0x06, 0x01]
                    , event [0x02, 0x03, 0xF7, 0x00]
                    ]
                (msgs, st) = stepRx initialRxState evs
             in (msgs, st.inSysEx)
                    @?= ([SysEx [0x7E, 0x06, 0x01, 0x02, 0x03]], False)
        , testCase "real-time status interleaved mid-sysex" $
            let evs =
                    [ event [0xF0, 0x7E, 0x06, 0x01]
                    , event [0xFE, 0x00, 0x00, 0x00] -- active sensing
                    , event [0x02, 0x03, 0xF7, 0x00]
                    ]
                (msgs, _) = stepRx initialRxState evs
             in msgs
                    @?= [ Unknown [0xFE]
                        , SysEx [0x7E, 0x06, 0x01, 0x02, 0x03]
                        ]
        , testCase "short channel-voice decode" $
            let evs =
                    [ event [0x90, 60, 100, 0]
                    , event [0x80, 60, 64, 0]
                    ]
                (msgs, _) = stepRx initialRxState evs
             in msgs
                    @?= [ NoteOn 0 (Pitch 60) (Velocity (100 / 127))
                        , NoteOff 0 (Pitch 60) (Velocity (64 / 127))
                        ]
        , testCase "two-byte ProgramChange strips padding" $
            let evs = [event [0xC4, 12, 0, 0]]
                (msgs, _) = stepRx initialRxState evs
             in msgs @?= [ProgramChange 4 12]
        , testCase "sysex spanning three packets" $
            let evs =
                    [ event [0xF0, 0x01, 0x02, 0x03]
                    , event [0x04, 0x05, 0x06, 0x07]
                    , event [0x08, 0xF7, 0x00, 0x00]
                    ]
                (msgs, _) = stepRx initialRxState evs
             in msgs
                    @?= [SysEx [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]]
        , testCase "empty event list yields no messages" $
            stepRx initialRxState [] @?= ([], initialRxState)
        ]

-- | Pack up to 4 bytes little-endian into a PMEvent's message word.
event :: [Word8] -> PM.PMEvent
event bs = PM.PMEvent (packBytes bs) 0
  where
    packBytes :: [Word8] -> CLong
    packBytes = foldr (.|.) 0 . zipWith packByte [0, 8, 16, 24]
    packByte :: Int -> Word8 -> CLong
    packByte shift b = fromIntegral b `shiftL` shift

{- | Generator constrained to the encodable, round-trippable subset:
velocities are quantised to k/127, NoteOn vel=0 is excluded (it would
canonicalise to NoteOff), and 'Unknown' / 'PitchBend' out-of-range cases
are skipped.
-}
instance Arbitrary MidiMessage where
    arbitrary =
        oneof
            [ NoteOn <$> chanG <*> pitchG <*> velPosG
            , NoteOff <$> chanG <*> pitchG <*> velG
            , ControlChange <$> chanG <*> sevenG <*> sevenG
            , PitchBend <$> chanG <*> choose (0, 16383)
            , PolyAftertouch <$> chanG <*> pitchG <*> sevenG
            , ChannelAftertouch <$> chanG <*> sevenG
            , ProgramChange <$> chanG <*> sevenG
            , SysEx <$> sysexBytesG
            ]
      where
        chanG = choose (0, 15)
        sevenG = choose (0, 127)
        pitchG = Pitch <$> sevenG
        velPosG = quantisedVel <$> (choose (1, 127) :: Gen Int)
        velG = quantisedVel <$> (choose (0, 127) :: Gen Int)
        quantisedVel k = Velocity (fromIntegral k / 127)
        sysexBytesG = listOf (choose (0, 0x7F) :: Gen Word8)
