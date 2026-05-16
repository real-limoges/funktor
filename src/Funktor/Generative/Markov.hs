module Funktor.Generative.Markov (
    MarkovChain (..),
    weightedChoice,
    step,
    generate,
    runChain,
    jazzBluesChain,
) where

import Data.List (unfoldr)
import Data.Map.Strict qualified as Map
import Funktor.Core.Stream (Stream, fromList)
import Funktor.Core.Types
import System.Random (StdGen, randomR)

-- | A Markov chain: for each state, a list of (weight, next-state) pairs.
newtype MarkovChain a = MarkovChain
    { transitions :: Map.Map a [(Double, a)]
    }

{- | Pick one element from a weighted list. Weights need not sum to 1; the
roll is scaled to the running total. Empty input is a programmer error —
the function calls 'error' rather than wrap every chain in 'Maybe'.
-}
weightedChoice :: [(Double, a)] -> StdGen -> (a, StdGen)
weightedChoice [] _ = error "weightedChoice: empty weighted list"
weightedChoice ws gen =
    let total = sum (map fst ws)
        (r, gen') = randomR (0, total) gen
     in (pickAt r ws, gen')
  where
    pickAt _ [(_, x)] = x
    pickAt r ((w, x) : rest)
        | r <= w = x
        | otherwise = pickAt (r - w) rest
    pickAt _ [] = error "weightedChoice: unreachable"

{- | One Markov step from the given state. States with no outgoing
transitions stay put.
-}
step :: (Ord a) => MarkovChain a -> a -> StdGen -> (a, StdGen)
step (MarkovChain ts) s gen = case Map.lookup s ts of
    Just outs@(_ : _) -> weightedChoice outs gen
    _ -> (s, gen)

-- | Infinite walk starting at @s@.
generate :: (Ord a) => MarkovChain a -> a -> StdGen -> [a]
generate chain s gen = s : unfoldr go (s, gen)
  where
    go (cur, g) =
        let (nxt, g') = step chain cur g
         in Just (nxt, (nxt, g'))

{- | Materialise a walk as a 'Stream'. Each state occupies @stepDur@ beats,
starting at beat 0; truncate by querying a finite window.
-}
runChain :: (Ord a) => MarkovChain a -> Duration -> a -> StdGen -> Stream a
runChain chain (Duration stepDur) s gen =
    fromList
        [ event (Arc start_ (start_ + Beat stepDur)) v
        | (i, v) <- zip [0 :: Int .. 1024] (generate chain s gen)
        , let start_ = Beat (stepDur * fromIntegral i)
        ]

-- | Classic 12-bar jazz blues skeleton over C7: I7 → IV7 → I7 → V7 ii–V → I7.
jazzBluesChain :: MarkovChain ChordSymbol
jazzBluesChain =
    MarkovChain $
        Map.fromList
            [ (i7, [(0.6, iv7), (0.3, v7), (0.1, ii7)])
            , (iv7, [(0.7, i7), (0.2, iv7), (0.1, ii7)])
            , (v7, [(0.6, i7), (0.3, ii7), (0.1, iv7)])
            , (ii7, [(0.7, v7), (0.3, i7)])
            ]
  where
    i7 = ChordSymbol (Pitch 60) Dominant7
    iv7 = ChordSymbol (Pitch 65) Dominant7
    v7 = ChordSymbol (Pitch 67) Dominant7
    ii7 = ChordSymbol (Pitch 62) Minor7
