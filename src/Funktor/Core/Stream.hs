{- | Streams are query functions @Arc -> [Event a]@. The scheduler asks "what
plays in @[t0, t1)@" each tick and gets back exactly those events — nothing
is materialised until needed.

@periodic period events@ is the bread-and-butter constructor: a finite list
of events placed in @[0, period)@, looped forever. @cat@ sequences streams
in time, @stack@ layers them, @slow@/@fast@ scale time, @shiftStream@
translates.
-}
module Funktor.Core.Stream (
    Stream (..),
    runStream,

    -- * Construction
    silence,
    periodic,
    fromList,
    singleton,

    -- * Composition
    shiftStream,
    merge,
    mergeMany,
    stack,
    cat,

    -- * Time scaling
    slow,
    fast,

    -- * Mapping
    mapStream,

    -- * Randomness
    sometimes,

    -- * Repetition utilities
    everyN,

    -- * Musical helpers
    pentatonic,
) where

import Data.List (sortOn)
import Funktor.Core.Types
import System.Random (mkStdGen, randomR)

newtype Stream a = Stream
    { query :: Arc -> [Event a]
    }

instance Functor Stream where
    fmap f (Stream q) = Stream $ \arc -> map (fmap f) (q arc)

{- | Streams are queries, not data. Show is opaque on purpose so QuickCheck
can mention them in counter-examples without forcing materialisation.
-}
instance Show (Stream a) where
    show _ = "<Stream>"

{- | Backwards-compatibility wrapper. Prefer @stream.query (Arc t0 t1)@ in
new code.
-}
runStream :: Stream a -> Beat -> Beat -> [Event a]
runStream s t0 t1 = s.query (Arc t0 t1)

silence :: Stream a
silence = Stream $ \_ -> []

{- | @periodic period events@ — a stream that plays @events@ (positions in
@[0, period)@) every @period@ beats. The classic finite-pattern-looped
shape, which is what 90% of musical streams are.

Events whose @part.start@ falls outside @[0, period)@ are still queried;
they're shifted to absolute time per cycle the same way as in-range events.
-}
periodic :: Duration -> [Event a] -> Stream a
periodic (Duration d) evts
    | d <= 0 = silence
    | otherwise = Stream $ \(Arc (Beat t0) (Beat t1)) ->
        let startCycle = floor (t0 / d) :: Integer
            endCycle = ceiling (t1 / d) :: Integer
            cycleAt n =
                [ shiftEvent (Beat (fromIntegral n * d)) e
                | e <- evts
                , let absStart = fromIntegral n * d + unBeat e.part.start
                , absStart >= t0
                , absStart < t1
                ]
         in concatMap cycleAt [startCycle .. endCycle - 1]

{- | Finite list of events; no repetition. Events outside the query window
are filtered out; remaining events are sorted by start time.
-}
fromList :: [Event a] -> Stream a
fromList evts = Stream $ \(Arc (Beat t0) (Beat t1)) ->
    [ e
    | e <- sorted
    , let absStart = unBeat e.part.start
    , absStart >= t0
    , absStart < t1
    ]
  where
    sorted = sortOn (unBeat . (.start) . (.part)) evts

{- | A one-event stream looped every @d@ beats, with the event spanning the
full cycle.
-}
singleton :: Duration -> a -> Stream a
singleton (Duration d) a
    | d <= 0 = silence
    | otherwise =
        let arc = Arc 0 (Beat d)
         in periodic (Duration d) [event arc a]

{- | Shift every event's time forward by @offset@. The query window is
back-shifted so the resulting stream returns the same events from the
caller's perspective at the new times.
-}
shiftStream :: Beat -> Stream a -> Stream a
shiftStream offset (Stream q) = Stream $ \arc ->
    map (shiftEvent offset) (q (shiftArc (-offset) arc))

-- | Parallel layering of two streams. Events from both fire in the same window.
merge :: Stream a -> Stream a -> Stream a
merge (Stream q1) (Stream q2) = Stream $ \arc ->
    sortOn (unBeat . (.start) . (.part)) (q1 arc ++ q2 arc)

mergeMany :: [Stream a] -> Stream a
mergeMany = foldr merge silence

-- | Alias for 'mergeMany'. Idiomatic when stacking different voices.
stack :: [Stream a] -> Stream a
stack = mergeMany

{- | Concatenate streams in time. @cat period [s1, s2, s3]@ plays @s1@'s
first @period@ beats, then @s2@'s, then @s3@'s, then repeats forever.
-}
cat :: Duration -> [Stream a] -> Stream a
cat _ [] = silence
cat (Duration d) streams
    | d <= 0 = silence
    | otherwise = Stream $ \arc ->
        let startCycle = floor (unBeat arc.start / d) :: Integer
            endCycle = ceiling (unBeat arc.end / d) :: Integer
            n = length streams
            local = Arc (Beat 0) (Beat d)
            slice i =
                let cycleStart = Beat (fromIntegral i * d)
                    slot = streams !! (fromInteger i `mod` n)
                    shifted = map (shiftEvent cycleStart) (slot.query local)
                 in filter (withinArc arc) shifted
         in concatMap slice [startCycle .. endCycle - 1]
  where
    withinArc q e =
        let s = e.part.start
         in s >= q.start && s < q.end

{- | Stretch a stream by factor @k@. @slow 2 s@ plays at half speed: a
period of 4 beats becomes 8. Non-positive @k@ collapses to 'silence'.
-}
slow :: Rational -> Stream a -> Stream a
slow k (Stream q)
    | k <= 0 = silence
    | otherwise = Stream $ \arc ->
        let invK = 1 / k
            arc' = scaleArc invK arc
            stretchEv (Event w p v) = Event (scaleArc k w) (scaleArc k p) v
         in map stretchEv (q arc')

-- | Compress a stream by factor @k@. @fast 2 s@ plays at double speed.
fast :: Rational -> Stream a -> Stream a
fast k
    | k == 0 = const silence
    | otherwise = slow (1 / k)

mapStream :: (Event a -> Event b) -> Stream a -> Stream b
mapStream f (Stream q) = Stream $ \arc -> map f (q arc)

{- | @sometimes p seed f s@ applies @f@ to events that fall in cycles where
a per-cycle coin flip lands under @p@. The coin uses @seed@ XOR-mixed with
the integer cycle index, so the same @(p, seed)@ gives the same per-cycle
result on every replay, but different cycles can take different branches.
@p <= 0@ is identity; @p >= 1@ always applies @f@.

(Phase 3 rewrite of the original one-shot 'sometimes' that decided once at
construction time and applied f to the whole stream — that variant was
effectively @if r < p then f else id@ with extra steps.)
-}
sometimes :: Double -> Int -> (Stream a -> Stream a) -> Stream a -> Stream a
sometimes prob seed f s
    | prob <= 0 = s
    | prob >= 1 = f s
    | otherwise = Stream $ \arc ->
        let startCycle = floor (unBeat arc.start) :: Int
            endCycle = ceiling (unBeat arc.end) :: Int
            roll c = fst (randomR (0.0 :: Double, 1.0) (mkStdGen (seed * 1_000_003 + c)))
            cycleArc c = Arc (Beat (fromIntegral c)) (Beat (fromIntegral (c + 1)))
            clip c evs =
                let ca = cycleArc c
                    lo = max ca.start arc.start
                    hi = min ca.end arc.end
                 in filter (\e -> e.part.start >= lo && e.part.start < hi) evs
            -- Query each branch once over the whole arc; then per-cycle pick
            -- which branch's events fall into that cycle. Cycle = 1-beat slot.
            fEvs = (f s).query arc
            sEvs = s.query arc
            cycleEvents c = if roll c < prob then clip c fEvs else clip c sEvs
         in sortOn
                (unBeat . (.start) . (.part))
                (concatMap cycleEvents [startCycle .. endCycle - 1])

{- | @everyN n f s@ applies @f@ to events whose integer-beat cycle index is
a multiple of @n@; other events pass through. @everyN 1 f = f@; non-positive
@n@ is the identity.
-}
everyN :: Int -> (Stream a -> Stream a) -> Stream a -> Stream a
everyN n f s
    | n <= 0 = s
    | n == 1 = f s
    | otherwise = Stream $ \arc ->
        let fEvents = filter inCycle ((f s).query arc)
            otherEvents = filter (not . inCycle) (s.query arc)
         in sortOn (unBeat . (.start) . (.part)) (fEvents ++ otherEvents)
  where
    inCycle e =
        let b = unBeat e.part.start
         in (floor b :: Integer) `mod` fromIntegral n == 0

pentatonicIntervals :: [Int]
pentatonicIntervals = [0, 3, 5, 7, 10]

{- | A C-minor pentatonic loop (C, Eb, F, G, Bb), one note per beat, period
of 5 beats. @oct@ shifts the root by octave; @4@ = middle C.
-}
pentatonic :: Octave -> Stream Note
pentatonic oct =
    periodic
        (Duration 5)
        (zipWith mkEv [0 :: Int ..] pentatonicIntervals)
  where
    baseMidi = 60 + (oct - 4) * 12
    mkEv i interval =
        let s = Beat (fromIntegral i)
            a = Arc s (s + 1)
         in event a (Note (Pitch (baseMidi + interval)) 0.7)
