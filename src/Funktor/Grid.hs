module Funktor.Grid
    ( Color (..)
    , PadAction (..)
    , Pad (..)
    , Grid (..)
    , emptyGrid
    , setPad
    , getPad
    ) where

import Funktor.Core.Types (Pitch, Velocity)

-- | LED colour for a pad. 'Off' means unlit.
data Color
    = Off
    | Red
    | Green
    | Yellow
    | Blue
    | Purple
    | Cyan
    | White
    deriving (Eq, Show, Enum, Bounded)

-- | What happens when a pad is pressed.
data PadAction
    = PlayNote Pitch Velocity
    | TriggerPattern Int
    | NoAction
    deriving (Eq, Show)

-- | A single pad: an action and a colour.
data Pad = Pad
    { padAction :: PadAction
    , padColor  :: Color
    } deriving (Eq, Show)

-- | An arbitrary-size grid of pads stored in row-major order.
-- 'gridPads !! y !! x' gives the pad at column x, row y.
data Grid = Grid
    { gridPads   :: ![[Pad]]
    , gridWidth  :: !Int
    , gridHeight :: !Int
    } deriving (Eq, Show)

-- | Create a blank grid with every pad set to 'NoAction' and 'Off'.
emptyGrid :: Int -> Int -> Grid
emptyGrid w h = Grid
    { gridPads   = replicate h (replicate w (Pad NoAction Off))
    , gridWidth  = w
    , gridHeight = h
    }

-- | Return a new 'Grid' with the pad at (x, y) replaced.
-- Out-of-bounds writes are silently ignored.
setPad :: Int -> Int -> Pad -> Grid -> Grid
setPad x y pad g
    | x < 0 || y < 0 || x >= gridWidth g || y >= gridHeight g = g
    | otherwise = g { gridPads = newPads }
  where
    newPads =
        [ if row == y
            then [ if col == x then pad else p | (col, p) <- zip [0..] r ]
            else r
        | (row, r) <- zip [0..] (gridPads g)
        ]

-- | Look up the pad at (x, y). Returns 'Nothing' for out-of-bounds indices.
getPad :: Int -> Int -> Grid -> Maybe Pad
getPad x y g
    | x < 0 || y < 0 || x >= gridWidth g || y >= gridHeight g = Nothing
    | otherwise = Just (gridPads g !! y !! x)
