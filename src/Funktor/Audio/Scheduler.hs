module Funktor.Audio.Scheduler (
    SchedulerAction (..),
    ScheduledEvent (..),
    SchedulerState (..),
    initialSchedulerState,
    schedulerThread,
    enqueueImmediate,
    hotSwap,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Monad (forever)
import Data.List qualified as L
import Funktor.Audio.State (AudioState (..))
import Funktor.Audio.Voice (poolNoteOff, poolNoteOn)
import Funktor.Core.Stream (Stream, runStream)
import Funktor.Core.Types (Beat (..), Duration (..), Event (..), Note (..), Pitch, Tempo (..), Velocity)
import GHC.Clock (getMonotonicTime)

data SchedulerAction
    = SchedNoteOn !Pitch !Velocity
    | SchedNoteOff !Pitch
    deriving (Eq, Show)

data ScheduledEvent = ScheduledEvent
    { time :: !Double
    , action :: !SchedulerAction
    }
    deriving (Show)

data SchedulerState = SchedulerState
    { tempo :: !Tempo
    , stream :: !(Stream Note)
    , beat :: !Beat
    , startTime :: !Double
    , pending :: ![ScheduledEvent]
    , lookAhead :: !Double
    }

{- | 100ms lookahead absorbs jitter in the scheduler tick (which is best-effort
on a non-realtime OS) without adding latency a human player would feel
between a 'play' / 'hot-swap' call and the resulting audio.
-}
initialSchedulerState :: Stream Note -> Tempo -> Double -> SchedulerState
initialSchedulerState s t start =
    SchedulerState
        { tempo = t
        , stream = s
        , beat = Beat 0
        , startTime = start
        , pending = []
        , lookAhead = 0.1
        }

schedulerThread :: TVar AudioState -> TVar SchedulerState -> IO ()
schedulerThread audioVar schedVar = forever $ do
    now <- getMonotonicTime
    start <- (.startTime) <$> readTVarIO schedVar
    let currentTime = now - start
    atomically $ do
        st <- readTVar schedVar
        let (due, remaining) = L.partition (\e -> e.time <= currentTime) st.pending
            nextBeat = st.beat + Beat (secondsToBeats st.tempo st.lookAhead)
            newEvents = eventsFromStream st st.beat nextBeat
            merged = L.sortOn (.time) (remaining ++ newEvents)
        writeTVar schedVar st{beat = nextBeat, pending = merged}
        mapM_ (applyAction audioVar currentTime) due
    -- 10ms tick: fine enough that quantisation isn't audible at sensible
    -- tempos, coarse enough to leave the CPU alone between batches.
    threadDelay 10000

applyAction :: TVar AudioState -> Double -> ScheduledEvent -> STM ()
applyAction audioVar currentTime event = case event.action of
    SchedNoteOn p vel -> modifyAudioPool $ poolNoteOn currentTime p vel
    SchedNoteOff p -> modifyAudioPool $ poolNoteOff currentTime p
  where
    modifyAudioPool f = modifyTVar' audioVar (\s -> s{pool = f s.pool})

eventsFromStream :: SchedulerState -> Beat -> Beat -> [ScheduledEvent]
eventsFromStream st fromBeat toBeat =
    concatMap (eventToActions st.tempo st.startTime) $
        runStream st.stream fromBeat toBeat

eventToActions :: Tempo -> Double -> Event Note -> [ScheduledEvent]
eventToActions t start (Event b (Note p d v)) =
    [ ScheduledEvent (start + beatToSeconds t (unBeat b)) (SchedNoteOn p v)
    , ScheduledEvent (start + beatToSeconds t (unBeat b + unDuration d)) (SchedNoteOff p)
    ]

beatToSeconds :: Tempo -> Rational -> Double
beatToSeconds (Tempo bpm) beats = fromRational beats * 60 / bpm

secondsToBeats :: Tempo -> Double -> Rational
secondsToBeats (Tempo bpm) secs = toRational (secs * bpm / 60)

{- | Inject a 'SchedulerAction' that fires on the very next scheduler tick.
Used by 'Funktor.Hardware.MIDI' to forward live MIDI note-on/off events
into the audio pipeline without consulting the wall clock. The event is
timestamped at @-Infinity@ so the next 'schedulerThread' iteration sees it
as already due, regardless of clock drift.
-}
enqueueImmediate :: TVar SchedulerState -> SchedulerAction -> STM ()
enqueueImmediate var act =
    modifyTVar' var $ \s ->
        s{pending = ScheduledEvent (-1 / 0) act : s.pending}

{- | Atomically replace the scheduler's stream, restart the beat clock at 0,
and drop any events queued from the previous stream. Used by 'Funktor.Live'
for the GHCi @play@ hot-swap, by 'Funktor.Grid.Binding' to commit Sequencer
toggle changes, and by Scene-mode pad presses to swap whole patterns.
-}
hotSwap :: TVar SchedulerState -> Stream Note -> STM ()
hotSwap var s =
    modifyTVar' var $ \st ->
        st{stream = s, beat = Beat 0, pending = []}
