module Funktor.Generative.CellularAutomata
    ( Rule (..)
    , rule30
    , rule90
    , rule110
    , applyRule
    , evolve
    , generations
    , centerSeed
    , rowToPattern
    , columnDensity
    , caPattern
    , caRhythm
    , caSequence
    ) where

import Data.Word (Word8)
import qualified Data.Vector.Unboxed as V

import Funktor.Core.Types
import Funktor.Core.Pattern

-- | A Wolfram elementary CA rule (0-255).
newtype Rule = Rule { unRule :: Word8 }
    deriving (Eq, Show)

rule30, rule90, rule110 :: Rule
rule30  = Rule 30
rule90  = Rule 90
rule110 = Rule 110

type Row = V.Vector Bool

applyRule :: Rule -> Bool -> Bool -> Bool -> Bool
applyRule = undefined

evolve :: Rule -> Row -> Row
evolve = undefined

generations :: Rule -> Int -> Row -> [Row]
generations = undefined

centerSeed :: Int -> Row
centerSeed = undefined

rowToPattern :: Note -> Row -> Pattern Note
rowToPattern = undefined

columnDensity :: [Row] -> [Int]
columnDensity = undefined

caPattern :: Rule -> Int -> Int -> [Pitch] -> Pattern Note
caPattern = undefined

caRhythm :: Rule -> Int -> Pattern Note
caRhythm = undefined

caSequence :: Rule -> Int -> Int -> [Pattern Note]
caSequence = undefined
