{--
Finite Musical ideas with a known length. Repeatable.
Building blocks to form emergent music.
--}

module Funktor.Core.Pattern
where

import Funktor.Core.Types
import Data.List (sortOn)

----------------
--  Patterns
----------------

data Pattern a = Pattern
    { patternEvents :: ![Event a] -- Events sorted by time
    , patternDuration :: !Duration -- length in beats
    }
    deriving (Eq, Show, Functor)

-- sorts events and makes it into a pattern
pattern_ :: Duration -> [Event a] -> Pattern a
pattern_ dur evts = Pattern (sortOn eventBeat evts) dur

----------------------
--   Construction
----------------------

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

----------------------
-- Helper Functions
----------------------

events :: Pattern a -> [Event a]
events = patternEvents

duration :: Pattern a -> Duration
duration = patternDuration

isEmpty :: Pattern a -> Bool
isEmpty = null . patternEvents

------------------
-- Composition
------------------

shift :: Beat -> Pattern a -> Pattern a
shift offset (Pattern evts dur) =
    Pattern (map (mapEventTime (+ offset)) evts) dur

scale :: Rational -> Pattern a -> Pattern a
scale factor (Pattern evts dur) =
    Pattern (map (mapEventTime (* Beat factor)) evts)
            (dur * Duration factor)

mapEvents :: (Event a -> Event b) -> Pattern a -> Pattern b
mapEvents f (Pattern evts dur) = Pattern (map f evts) dur

filterEvents :: (Event a -> Bool) -> Pattern a -> Pattern a
filterEvents p (Pattern evts dur) = Pattern (filter p evts) dur

append :: Pattern a -> Pattern a -> Pattern a
append (Pattern evts1 dur1) (Pattern evts2 dur2) =
    Pattern (evts1 ++ map (mapEventTime (+ Beat (unDuration dur1))) evts2)
            (dur1 + dur2)

stack :: Pattern a -> Pattern a -> Pattern a
stack (Pattern evts1 dur1) (Pattern evts2 dur2) =
    Pattern (sortOn eventBeat $ evts1 ++ evts2) (max dur1 dur2)

-- tail-optimized recursion
repeat_ :: Int -> Pattern a -> Pattern a
repeat_ n pat
    | n <= 0 = empty
    | n == 1 = pat
    | otherwise = pat `append` repeat_ (n - 1) pat

--------------------
-- Common Patterns
--------------------

pentatonic :: Octave -> Pattern Note
pentatonic oct =
    let pitches = [0, 3, 5, 7, 10] -- Minor pentatonic intervals
        baseNote = 60 + (oct -4) * 12 -- C in the octave
        makeNote i p = Event (Beat $ fromIntegral i)
                             (Note (Pitch $ baseNote + p) 1 0.7)
    in Pattern (zipWith makeNote [0..] pitches) 5
