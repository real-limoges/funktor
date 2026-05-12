module Funktor.Grid (
    Color (..),
    PadAction (..),
    Pad (..),
    Grid (..),
    emptyGrid,
    setPad,
    getPad,
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
    , padColor :: Color
    }
    deriving (Eq, Show)

{- | An arbitrary-size grid of pads stored in row-major order.
'gridPads !! y !! x' gives the pad at column x, row y.
-}
data Grid = Grid
    { gridPads :: ![[Pad]]
    , gridWidth :: !Int
    , gridHeight :: !Int
    }
    deriving (Eq, Show)

-- | Create a blank grid with every pad set to 'NoAction' and 'Off'.
emptyGrid :: Int -> Int -> Grid
emptyGrid w h = Grid (replicate h (replicate w (Pad NoAction Off))) w h

setPad :: Int -> Int -> Pad -> Grid -> Grid
setPad x y pad g
    | x < 0 || y < 0 || x >= gridWidth g || y >= gridHeight g = g
    | otherwise = g{gridPads = updateRow y (updateCell x pad) (gridPads g)}

updateCell :: Int -> Pad -> [Pad] -> [Pad]
updateCell x pad = zipWith (\i p -> if i == x then pad else p) [0 ..]

updateRow :: Int -> ([Pad] -> [Pad]) -> [[Pad]] -> [[Pad]]
updateRow y f = zipWith (\i r -> if i == y then f r else r) [0 ..]

getPad :: Int -> Int -> Grid -> Maybe Pad
getPad x y g
    | x < 0 || y < 0 || x >= gridWidth g || y >= gridHeight g = Nothing
    | otherwise = getNested (gridPads g) y x
  where
    getNested :: [[Pad]] -> Int -> Int -> Maybe Pad
    getNested [] _ _ = Nothing
    getNested (r : _) 0 x' = safeIndex r x'
    getNested (_ : rs) i x' = getNested rs (i - 1) x'

    safeIndex :: [b] -> Int -> Maybe b
    safeIndex [] _ = Nothing
    safeIndex (x' : _) 0 = Just x'
    safeIndex (_ : xs) n = safeIndex xs (n - 1)
