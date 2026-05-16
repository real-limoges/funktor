module Funktor.Generative.Euclidean (
    bjorklund,
    euclidean,
    euclideanWith,
    rotateEuclidean,
    polyEuclidean,
) where

import Funktor.Core.Pattern
import Funktor.Core.Types

{- | Bjorklund's algorithm: distribute @k@ pulses across @n@ slots as evenly
as possible. @bjorklund 3 8@ is the Cuban tresillo @[T,F,F,T,F,F,T,F]@.
-}
bjorklund :: Int -> Int -> [Bool]
bjorklund k n
    | k <= 0 = replicate n False
    | k >= n = replicate n True
    | otherwise = concat (loop (replicate k [True]) (replicate (n - k) [False]))
  where
    loop xs ys
        | length ys <= 1 = xs ++ ys
        | length xs <= length ys =
            let (paired, leftover) = splitAt (length xs) ys
             in loop (zipWith (++) xs paired) leftover
        | otherwise =
            let (paired, leftover) = splitAt (length ys) xs
             in loop (zipWith (++) paired ys) leftover

defaultEuclideanNote :: Note
defaultEuclideanNote = Note (Pitch 60) 1 1.0

-- | @euclidean k n@: @k@ unit-duration middle-C notes spread over @n@ steps.
euclidean :: Int -> Int -> Pattern Note
euclidean = euclideanWith defaultEuclideanNote

-- | Like 'euclidean' but the caller picks the note to fire on each pulse.
euclideanWith :: Note -> Int -> Int -> Pattern Note
euclideanWith n_ k n =
    pattern_ (Duration (fromIntegral n)) (eventsFor (bjorklund k n))
  where
    eventsFor bs =
        [ Event (Beat (fromIntegral i)) n_
        | (i, True) <- zip [0 :: Int ..] bs
        ]

-- | @rotateEuclidean offset k n@: 'euclidean' shifted by @offset@ beats.
rotateEuclidean :: Int -> Int -> Int -> Pattern Note
rotateEuclidean offset k n =
    shift (Beat (fromIntegral offset)) (euclidean k n)

{- | Layer one 'euclideanWith' per @(k, n, pitch)@ triple via 'stack'.
Each layer fires its pitch on every pulse; duration of the result is the
longest layer.
-}
polyEuclidean :: [(Int, Int, Pitch)] -> Pattern Note
polyEuclidean = foldr layer empty
  where
    layer (k, n, p) acc =
        stack (euclideanWith (Note p 1 1.0) k n) acc
