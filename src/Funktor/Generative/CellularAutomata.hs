module Funktor.Generative.CellularAutomata (
    Rule (..),
    Row,
    rule30,
    rule90,
    rule110,
    applyRule,
    evolve,
    generations,
    centerSeed,
    rowToPattern,
    columnDensity,
    caPattern,
    caRhythm,
    caSequence,
) where

import Data.Bits (testBit)
import Data.Vector.Unboxed qualified as V
import Data.Word (Word8)

import Funktor.Core.Pattern
import Funktor.Core.Types

-- | A Wolfram elementary CA rule (0-255).
newtype Rule = Rule {unRule :: Word8}
    deriving (Eq, Show)

rule30, rule90, rule110 :: Rule
rule30 = Rule 30
rule90 = Rule 90
rule110 = Rule 110

type Row = V.Vector Bool

{- | Evaluate the Wolfram rule on a 3-cell neighbourhood. The bit pattern of
@unRule@ is indexed by @(l, c, r)@ packed as a 3-bit value with @l@ as the
high bit.
-}
applyRule :: Rule -> Bool -> Bool -> Bool -> Bool
applyRule (Rule w) l c r =
    let idx = (if l then 4 else 0) + (if c then 2 else 0) + (if r then 1 else 0)
     in testBit w idx

-- | One generation of the CA with @False@ boundaries.
evolve :: Rule -> Row -> Row
evolve rule row = V.generate n $ \i ->
    let l = if i == 0 then False else row V.! (i - 1)
        c = row V.! i
        r = if i == n - 1 then False else row V.! (i + 1)
     in applyRule rule l c r
  where
    n = V.length row

-- | @n@ successive generations starting from @seed@ (inclusive of the seed).
generations :: Rule -> Int -> Row -> [Row]
generations rule n seed = take n (iterate (evolve rule) seed)

-- | A row of @n@ cells with only the middle cell alive.
centerSeed :: Int -> Row
centerSeed n
    | n <= 0 = V.empty
    | otherwise = V.generate n (\i -> i == n `div` 2)

{- | Map a row to a pattern of unit-step events. Each @True@ cell fires the
supplied note at its index; @False@ cells produce rests. Result duration
equals row length.
-}
rowToPattern :: Note -> Row -> Pattern Note
rowToPattern n row =
    pattern_
        (Duration (fromIntegral (V.length row)))
        [ Event (Beat (fromIntegral i)) n
        | (i, True) <- zip [0 :: Int ..] (V.toList row)
        ]

-- | Number of live cells per column across a list of equal-length rows.
columnDensity :: [Row] -> [Int]
columnDensity [] = []
columnDensity rs@(r0 : _) =
    [ length [() | row <- rs, row V.! j]
    | j <- [0 .. V.length r0 - 1]
    ]

{- | Build a melodic pattern: evolve the rule for @rows@ generations starting
from a center seed of @cols@ cells, then map each live cell to the pitch
at its column index (cycling through @pitches@).
-}
caPattern :: Rule -> Int -> Int -> [Pitch] -> Pattern Note
caPattern _ _ _ [] = empty
caPattern rule rows cols pitches =
    foldr (append . rowEvents) empty (generations rule rows (centerSeed cols))
  where
    pitchAt j = pitches !! (j `mod` length pitches)
    rowEvents row =
        pattern_
            (Duration (fromIntegral cols))
            [ Event (Beat (fromIntegral j)) (Note (pitchAt j) 1 1.0)
            | (j, True) <- zip [0 :: Int ..] (V.toList row)
            ]

-- | One row of the CA used as a rhythm: middle-C hit on every live cell.
caRhythm :: Rule -> Int -> Pattern Note
caRhythm rule cols =
    rowToPattern (Note (Pitch 60) 1 1.0) (evolve rule (centerSeed cols))

-- | One pattern per generation, useful as scene content.
caSequence :: Rule -> Int -> Int -> [Pattern Note]
caSequence rule rows cols =
    [ rowToPattern (Note (Pitch 60) 1 1.0) row
    | row <- generations rule rows (centerSeed cols)
    ]
