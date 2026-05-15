{- | Novation Launchpad Mini Mk3 driver. Pure module: builds SysEx byte
sequences and decodes pad-note coordinates without touching PortMidi
directly. Callers in 'Funktor.Live' open the underlying input/output handles
and dispatch 'sendSysEx' / 'sendMessage'.

In Mk3 Programmer Mode every pad and control button is addressed by a MIDI
note number of the form @(row+1)*10 + (col+1)@, with row 0 at the bottom
and column 0 at the left. The main 8x8 grid spans notes 11..88; the top
"scene" row uses 91..98 and the right column uses 19, 29, .., 89. The very
top-right corner (note 99) is the Novation logo LED.
-}
module Funktor.Hardware.Launchpad (
    LaunchpadConfig (..),
    LaunchpadEvent (..),
    defaultMk3Config,
    noteToGrid,
    gridToNote,
    colorToRGB,
    midiToLaunchpadEvent,
    programmerModeSysEx,
    liveModeSysEx,
    ledSysEx,
    gridLedSysEx,
) where

import Data.Word (Word8)
import Funktor.Core.Types (Pitch (..), Velocity)
import Funktor.Grid (Color (..), Grid (..), Pad (..))
import Funktor.Hardware.MIDI (MidiMessage (..))

{- | A pad press, release, or aftertouch reported as a grid coordinate.
Coordinates use @(x, y)@ with @x@ measured from the left and @y@ from the
bottom, matching the Mk3's row/column numbering. The top control row is
@y == 8@; the right control column is @x == 8@.
-}
data LaunchpadEvent
    = PadDown !(Int, Int) !Velocity
    | PadUp !(Int, Int)
    | PadAftertouch !(Int, Int) !Int
    deriving (Eq, Show)

-- | Configuration for a specific Launchpad model.
data LaunchpadConfig = LaunchpadConfig
    { lpSysExHeader :: ![Word8]
    , lpNoteToGrid :: !(Int -> Maybe (Int, Int))
    , lpGridToNote :: !(Int -> Int -> Int)
    , lpGridWidth :: !Int
    , lpGridHeight :: !Int
    }

{- | Configuration for the Launchpad Mini Mk3 in Programmer Mode. Covers the
full 9x9 addressable surface (main grid + top row + right column); the very
top-right slot (note 99) is the Novation logo, not a button.
-}
defaultMk3Config :: LaunchpadConfig
defaultMk3Config =
    LaunchpadConfig
        { lpSysExHeader = [0x00, 0x20, 0x29, 0x02, 0x0D]
        , lpNoteToGrid = noteToGrid
        , lpGridToNote = gridToNote
        , lpGridWidth = 9
        , lpGridHeight = 9
        }

{- | Decode a Programmer-Mode MIDI note into a grid coordinate. Returns
'Nothing' for notes outside the 9x9 addressable surface (e.g. 0..10, the
column-9 / row-9 strays like 90, 99..127).
-}
noteToGrid :: Int -> Maybe (Int, Int)
noteToGrid n
    | x < 0 || x > 8 || y < 0 || y > 8 = Nothing
    | otherwise = Just (x, y)
  where
    (y1, x1) = n `divMod` 10
    x = x1 - 1
    y = y1 - 1

{- | Encode a grid coordinate as a Programmer-Mode MIDI note. The inverse of
'noteToGrid' over @(0..8) x (0..8)@. Out-of-range coordinates produce
nonsense notes — callers should bounds-check first.
-}
gridToNote :: Int -> Int -> Int
gridToNote x y = (y + 1) * 10 + (x + 1)

{- | Map a 'Color' to a 0..127-per-channel RGB triple, the format the Mk3
expects in its LED-control SysEx frames. Saturated hues at full brightness;
'Off' is all-zero.
-}
colorToRGB :: Color -> (Word8, Word8, Word8)
colorToRGB c = case c of
    Off -> (0, 0, 0)
    Red -> (127, 0, 0)
    Green -> (0, 127, 0)
    Yellow -> (127, 127, 0)
    Blue -> (0, 0, 127)
    Purple -> (127, 0, 127)
    Cyan -> (0, 127, 127)
    White -> (127, 127, 127)

{- | Translate a raw 'MidiMessage' from the Launchpad's input stream into a
'LaunchpadEvent', or 'Nothing' if the message is not a pad event. NoteOn
with velocity 0 is canonicalised to 'PadUp' by the upstream parser.
-}
midiToLaunchpadEvent :: LaunchpadConfig -> MidiMessage -> Maybe LaunchpadEvent
midiToLaunchpadEvent cfg msg = case msg of
    NoteOn _ (Pitch p) v -> fmap (`PadDown` v) (lpNoteToGrid cfg p)
    NoteOff _ (Pitch p) _ -> fmap PadUp (lpNoteToGrid cfg p)
    PolyAftertouch _ (Pitch p) pr -> fmap (`PadAftertouch` pr) (lpNoteToGrid cfg p)
    _ -> Nothing

{- | SysEx payload (no F0/F7 framing) that switches the Mk3 into Programmer
Mode. Send via 'Funktor.Hardware.MIDI.sendSysEx'.
-}
programmerModeSysEx :: LaunchpadConfig -> [Word8]
programmerModeSysEx cfg = lpSysExHeader cfg ++ [0x0E, 0x01]

-- | SysEx payload (no F0/F7 framing) that returns the Mk3 to Live Mode.
liveModeSysEx :: LaunchpadConfig -> [Word8]
liveModeSysEx cfg = lpSysExHeader cfg ++ [0x0E, 0x00]

{- | SysEx payload setting a single pad's LED to the given colour. The pad is
addressed by its Programmer-Mode MIDI note (use 'gridToNote' for grid
coordinates).
-}
ledSysEx :: LaunchpadConfig -> Int -> Color -> [Word8]
ledSysEx cfg note c =
    lpSysExHeader cfg ++ [0x03, 0x03, fromIntegral note, r, g, b]
  where
    (r, g, b) = colorToRGB c

{- | SysEx payload setting every pad in a 'Grid' to its 'padColor' in a single
multi-spec frame. Used to repaint the whole Launchpad on mode change.
-}
gridLedSysEx :: LaunchpadConfig -> Grid -> [Word8]
gridLedSysEx cfg g =
    lpSysExHeader cfg ++ [0x03] ++ concatMap spec coords
  where
    coords =
        [ (x, y, padColor pad)
        | (y, row) <- zip [0 ..] (gridPads g)
        , (x, pad) <- zip [0 ..] row
        ]
    spec (x, y, c) =
        let (r, gC, b) = colorToRGB c
         in [0x03, fromIntegral (gridToNote x y), r, gC, b]
