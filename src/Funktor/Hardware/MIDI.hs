module Funktor.Hardware.MIDI (
    MidiMessage (..),
    parseMidiMessage,
) where

import Data.Word (Word8)
import Funktor.Core.Types (Pitch, Velocity)

-- | Parsed MIDI message.
data MidiMessage
    = NoteOn !Int !Pitch !Velocity
    | NoteOff !Int !Pitch !Velocity
    | ControlChange !Int !Int !Int
    | PitchBend !Int !Int
    | Unknown [Word8]
    deriving (Show, Eq)

parseMidiMessage :: [Word8] -> MidiMessage
parseMidiMessage = undefined
