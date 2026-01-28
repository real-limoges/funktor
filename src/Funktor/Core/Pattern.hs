{--
Finite Musical ideas with a known length. Repeatable.
Building blocks to form emergent music.
--}

module Funktor.Core.Pattern
where

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
pattern_ dur events = Pattern (sorton eventBeat evts) dur

----------------------
--   Construction
----------------------

empty :: Pattern a
empty = Pattern [] 0

singleton :: Duration -> a -> Pattern a
singleton dur a = Pattern [Event 0 a] dur

rest :: Duration -> Pattern
rest dur = Pattern [] dur

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

-- Appending
-- Stacking
-- Repeat

--------------------
-- Transformations
--------------------

-- Shift
-- Scale
-- Map & Filter
