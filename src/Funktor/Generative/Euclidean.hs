module Funktor.Generative.Euclidean (
    bjorklund,
    euclidean,
    euclideanWith,
    rotateEuclidean,
    polyEuclidean,
) where

import Funktor.Core.Stream
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
defaultEuclideanNote = Note (Pitch 60) 1.0

-- | @euclidean k n@: @k@ unit-duration middle-C notes spread over @n@ steps.
euclidean :: Int -> Int -> Stream Note
euclidean = euclideanWith defaultEuclideanNote

-- | Like 'euclidean' but the caller picks the note to fire on each pulse.
euclideanWith :: Note -> Int -> Int -> Stream Note
euclideanWith n_ k n =
    periodic (Duration (fromIntegral n)) (eventsFor (bjorklund k n))
  where
    eventsFor bs =
        [ event (Arc s (s + 1)) n_
        | (i, True) <- zip [0 :: Int ..] bs
        , let s = Beat (fromIntegral i)
        ]

-- | @rotateEuclidean offset k n@: 'euclidean' shifted by @offset@ beats.
rotateEuclidean :: Int -> Int -> Int -> Stream Note
rotateEuclidean offset k n =
    shiftStream (Beat (fromIntegral offset)) (euclidean k n)

{- | Layer one 'euclideanWith' per @(k, n, pitch)@ triple via 'merge'.
Each layer fires its pitch on every pulse; the result is a parallel stack
of all layers.
-}
polyEuclidean :: [(Int, Int, Pitch)] -> Stream Note
polyEuclidean = foldr layer silence
  where
    layer (k, n, p) acc =
        merge (euclideanWith (Note p 1.0) k n) acc
