module Funktor.Generative.Markov
    ( MarkovChain (..)
    , weightedChoice
    , step
    , generate
    , runChain
    , jazzBluesChain
    ) where

import Funktor.Core.Types
import Funktor.Core.Stream (Stream)

import qualified Data.Map.Strict as Map
import System.Random (StdGen)

-- | A Markov chain: for each state, a list of (weight, next-state) pairs.
newtype MarkovChain a = MarkovChain
    { transitions :: Map.Map a [(Double, a)]
    }

weightedChoice :: [(Double, a)] -> StdGen -> (a, StdGen)
weightedChoice = undefined

step :: MarkovChain a -> a -> StdGen -> (a, StdGen)
step = undefined

generate :: MarkovChain a -> a -> StdGen -> [a]
generate = undefined

runChain :: MarkovChain a -> Duration -> a -> StdGen -> Stream a
runChain = undefined

jazzBluesChain :: MarkovChain ChordSymbol
jazzBluesChain = undefined
