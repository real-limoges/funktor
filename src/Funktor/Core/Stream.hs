module Funktor.Core.Stream where

import Data.List (sortOn)
import Funktor.Core.Pattern (Pattern (..))
import Funktor.Core.Types
import System.Random (StdGen, randomR)

newtype Stream a = Stream
    { runStream :: Beat -> Beat -> [Event a]
    }

instance Functor Stream where
    fmap f (Stream query_) = Stream $ \t0 t1 ->
        map (fmap f) (query_ t0 t1)

fromPattern :: Pattern a -> Stream a
fromPattern (Pattern evts (Duration dur))
    | dur <= 0 = silence
    | otherwise = Stream $ \(Beat t0) (Beat t1) ->
        let startLoop = floor (t0 / dur) :: Integer
            endLoop = ceiling (t1 / dur) :: Integer
            eventsInCycle loopNum =
                [ Event (Beat absTime) v
                | Event b v <- evts
                , let absTime = fromIntegral loopNum * dur + unBeat b
                , absTime >= t0
                , absTime < t1
                ]
         in concatMap eventsInCycle [startLoop .. endLoop - 1]

fromList :: [Event a] -> Stream a
fromList evts = Stream $ \(Beat t0) (Beat t1) ->
    [ e
    | e@(Event (Beat t) _) <- sortedEvts
    , t >= t0
    , t < t1
    ]
  where
    sortedEvts = sortOn (.beat) evts

silence :: Stream a
silence = Stream $ \_ _ -> []

mapStream :: (Event a -> Event b) -> Stream a -> Stream b
mapStream f (Stream query_) = Stream $ \t0 t1 ->
    map f (query_ t0 t1)

shiftStream :: Beat -> Stream a -> Stream a
shiftStream offset (Stream query_) = Stream $ \t0 t1 ->
    map (mapEventTime (+ offset)) $ query_ (t0 - offset) (t1 - offset)

merge :: Stream a -> Stream a -> Stream a
merge (Stream q1) (Stream q2) = Stream $ \t0 t1 ->
    sortOn (.beat) $ q1 t0 t1 ++ q2 t0 t1

mergeMany :: [Stream a] -> Stream a
mergeMany = foldr merge silence

{- | @sometimes p gen f s@ applies @f@ to @s@ with probability @p@, otherwise
returns @s@ unchanged. The decision is made once at construction time using
@gen@; pass a fresh 'StdGen' to re-roll. @p <= 0@ is the identity, @p >= 1@
always applies @f@.
-}
sometimes :: Double -> StdGen -> (Stream a -> Stream a) -> Stream a -> Stream a
sometimes prob gen f s
    | prob <= 0 = s
    | prob >= 1 = f s
    | r < prob = f s
    | otherwise = s
  where
    (r, _) = randomR (0.0 :: Double, 1.0) gen

{- | @everyN n f s@ applies @f@ to events whose integer-beat cycle index is
a multiple of @n@; other events pass through. @everyN 1 f = f@; non-positive
@n@ is the identity.
-}
everyN :: Int -> (Stream a -> Stream a) -> Stream a -> Stream a
everyN n f s
    | n <= 0 = s
    | n == 1 = f s
    | otherwise = Stream $ \t0 t1 ->
        let fEvents = filter inCycle (runStream (f s) t0 t1)
            otherEvents = filter (not . inCycle) (runStream s t0 t1)
         in sortOn (.beat) (fEvents ++ otherEvents)
  where
    inCycle (Event (Beat b) _) = (floor b :: Integer) `mod` fromIntegral n == 0
