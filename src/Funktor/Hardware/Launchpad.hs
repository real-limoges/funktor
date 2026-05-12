module Funktor.Hardware.Launchpad (
    LaunchpadConfig (..),
    defaultMk3Config,
    noteToGrid,
    gridToNote,
    colorToRGB,
) where

import Data.Word (Word8)
import Funktor.Grid (Color (..))

-- | Configuration for a specific Launchpad model.
data LaunchpadConfig = LaunchpadConfig
    { lpSysExHeader :: ![Word8]
    , lpNoteToGrid :: !(Int -> Maybe (Int, Int))
    , lpGridToNote :: !(Int -> Int -> Int)
    , lpGridWidth :: !Int
    , lpGridHeight :: !Int
    }

defaultMk3Config :: LaunchpadConfig
defaultMk3Config = undefined

noteToGrid :: Int -> Maybe (Int, Int)
noteToGrid = undefined

gridToNote :: Int -> Int -> Int
gridToNote = undefined

colorToRGB :: Color -> (Word8, Word8, Word8)
colorToRGB = undefined
