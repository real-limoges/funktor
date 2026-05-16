module Funktor.Core.Pattern
where

import Data.List (sortOn)
import Funktor.Core.Types

data Pattern a = Pattern
    { events :: ![Event a]
    , duration :: !Duration
    }
    deriving (Eq, Show, Functor)

pattern_ :: Duration -> [Event a] -> Pattern a
pattern_ dur evts = Pattern (sortOn (.beat) evts) dur

empty :: Pattern a
empty = Pattern [] 0

singleton :: Duration -> a -> Pattern a
singleton dur a = Pattern [Event 0 a] dur

rest :: Duration -> Pattern a
rest = Pattern []

note :: Pitch -> Duration -> Velocity -> Pattern Note
note p d v = singleton d (Note p d v)

notes :: Duration -> [(Beat, Note)] -> Pattern Note
notes dur pairs = pattern_ dur [Event b n | (b, n) <- pairs]

isEmpty :: Pattern a -> Bool
isEmpty p = null p.events

shift :: Beat -> Pattern a -> Pattern a
shift offset (Pattern evts dur) =
    Pattern (map (mapEventTime (+ offset)) evts) dur

scale :: Rational -> Pattern a -> Pattern a
scale factor (Pattern evts dur) =
    Pattern
        (map (mapEventTime (* Beat factor)) evts)
        (dur * Duration factor)

mapEvents :: (Event a -> Event b) -> Pattern a -> Pattern b
mapEvents f (Pattern evts dur) = Pattern (map f evts) dur

filterEvents :: (Event a -> Bool) -> Pattern a -> Pattern a
filterEvents p (Pattern evts dur) = Pattern (filter p evts) dur

append :: Pattern a -> Pattern a -> Pattern a
append (Pattern evts1 dur1) (Pattern evts2 dur2) =
    Pattern
        (evts1 ++ map (mapEventTime (+ Beat (unDuration dur1))) evts2)
        (dur1 + dur2)

stack :: Pattern a -> Pattern a -> Pattern a
stack (Pattern evts1 dur1) (Pattern evts2 dur2) =
    Pattern (sortOn (.beat) $ evts1 ++ evts2) (max dur1 dur2)

repeat_ :: Int -> Pattern a -> Pattern a
repeat_ n pat
    | n <= 0 = empty
    | n == 1 = pat
    | otherwise = pat `append` repeat_ (n - 1) pat

pentatonicIntervals :: [Int]
pentatonicIntervals = [0, 3, 5, 7, 10]

pentatonic :: Octave -> Pattern Note
pentatonic oct = Pattern (zipWith mkEvent [0 :: Int ..] pentatonicIntervals) 5
  where
    baseMidi = 60 + (oct - 4) * 12
    mkEvent i interval =
        Event (Beat (fromIntegral i)) (Note (Pitch (baseMidi + interval)) 1 0.7)
