module Funktor.Generative.Euclidean
    ( bjorklund
    , euclidean
    , euclideanWith
    , rotateEuclidean
    , polyEuclidean
    ) where

import Funktor.Core.Types
import Funktor.Core.Pattern

bjorklund :: Int -> Int -> [Bool]
bjorklund = undefined

euclidean :: Int -> Int -> Pattern Note
euclidean = undefined

euclideanWith :: Note -> Int -> Int -> Pattern Note
euclideanWith = undefined

rotateEuclidean :: Int -> Int -> Int -> Pattern Note
rotateEuclidean = undefined

polyEuclidean :: [(Int, Int, Pitch)] -> Pattern Note
polyEuclidean = undefined
