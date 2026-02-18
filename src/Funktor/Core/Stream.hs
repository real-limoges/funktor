module Funktor.Core.Stream where

import Funktor.Core.Types
import Funktor.Core.Pattern (Pattern(..))

import Data.List (sortOn)


-- I'm representing Streams as functions from a time range to events
-- it's better than infinite lists when I only use events in a
-- window

newtype Stream a = Stream
    { runStream :: Beat -> Beat -> [Event a]
    }

instance Functor Stream where
    fmap f (Stream query_) = Stream $ \t0 t1 ->
        map (fmap f) (query_ t0 t1)


-- Construct streams
fromPattern :: Pattern a -> Stream a
fromPattern (Pattern evts (Duration dur))
    | dur <= 0 = silence
    | otherwise = Stream query_
  where
    query_ (Beat t0) (Beat t1) =
        let
            startLoop = floor (t0 / dur) :: Integer
            endLoop = ceiling (t1 / dur ) :: Integer

            loopEvents loopNum = 
                let offset = fromIntegral loopNum * dur
                in [ Event (Beat $ offset + unBeat b) v
                    | Event b v <- evts
                    , let absTime = offset + unBeat b
                    , absTime >= t0
                    , absTime < t1
                    ]
        in concatMap loopEvents [startLoop .. endLoop - 1]


-- create a stream from a list
fromList :: [Event a] -> Stream a
fromList evts = Stream $ \(Beat t0) (Beat t1) ->
    [ e | e@(Event (Beat t) _) <- sortedEvts
    , t >= t0
    , t < t1
    ]
  where
    sortedEvts = sortOn eventBeat evts

silence :: Stream a
silence = Stream $ \_ _ -> []

--------------------
-- Transformations
--------------------

mapStream :: (Event a -> Event b) -> Stream a -> Stream b
mapStream f (Stream query_) = Stream $ \t0 t1 ->
    map f (query_ t0 t1)

shiftStream :: Beat -> Stream a -> Stream a
shiftStream offset (Stream query_) = Stream $ \t0 t1 ->
    map (mapEventTime (+ offset)) $ query_ (t0 - offset) (t1 - offset)

merge :: Stream a -> Stream a -> Stream a
merge (Stream q1) (Stream q2) = Stream $ \t0 t1 ->
    sortOn eventBeat $ q1 t0 t1 ++ q2 t0 t1

mergeMany :: [Stream a] -> Stream a
mergeMany = foldr merge silence