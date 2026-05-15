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
    { schedTime :: !Double
    , schedAction :: !SchedulerAction
    }
    deriving (Show)

data SchedulerState = SchedulerState
    { schedTempo :: !Tempo
    , schedStream :: !(Stream Note)
    , schedBeat :: !Beat
    , schedStartTime :: !Double
    , schedPending :: ![ScheduledEvent]
    , schedLookAhead :: !Double
    }

initialSchedulerState :: Stream Note -> Tempo -> Double -> SchedulerState
initialSchedulerState stream tempo startTime =
    SchedulerState
        { schedTempo = tempo
        , schedStream = stream
        , schedBeat = Beat 0
        , schedStartTime = startTime
        , schedPending = []
        , schedLookAhead = 0.1
        }

schedulerThread :: TVar AudioState -> TVar SchedulerState -> IO ()
schedulerThread audioVar schedVar = forever $ do
    now <- getMonotonicTime
    startTime <- schedStartTime <$> readTVarIO schedVar
    let currentTime = now - startTime
    atomically $ do
        st <- readTVar schedVar
        let (due, remaining) = L.partition (\e -> schedTime e <= currentTime) (schedPending st)
            nextBeat = schedBeat st + Beat (secondsToBeats (schedTempo st) (schedLookAhead st))
            newEvents = eventsFromStream st (schedBeat st) nextBeat
            merged = L.sortOn schedTime (remaining ++ newEvents)
        writeTVar schedVar st{schedBeat = nextBeat, schedPending = merged}
        mapM_ (applyAction audioVar currentTime) due
    threadDelay 10000

applyAction :: TVar AudioState -> Double -> ScheduledEvent -> STM ()
applyAction audioVar currentTime event = case schedAction event of
    SchedNoteOn pitch vel -> modifyAudioPool $ poolNoteOn currentTime pitch vel
    SchedNoteOff pitch -> modifyAudioPool $ poolNoteOff currentTime pitch
  where
    modifyAudioPool f = modifyTVar' audioVar (\s -> s{audioPool = f (audioPool s)})

eventsFromStream :: SchedulerState -> Beat -> Beat -> [ScheduledEvent]
eventsFromStream st fromBeat toBeat =
    concatMap (eventToActions (schedTempo st) (schedStartTime st)) $
        runStream (schedStream st) fromBeat toBeat

eventToActions :: Tempo -> Double -> Event Note -> [ScheduledEvent]
eventToActions tempo startTime (Event beat (Note pitch duration velocity)) =
    [ ScheduledEvent (startTime + beatToSeconds tempo (unBeat beat)) (SchedNoteOn pitch velocity)
    , ScheduledEvent (startTime + beatToSeconds tempo (unBeat beat + unDuration duration)) (SchedNoteOff pitch)
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
        s{schedPending = ScheduledEvent (-1 / 0) act : schedPending s}

{- | Atomically replace the scheduler's stream, restart the beat clock at 0,
and drop any events queued from the previous stream. Used by 'Funktor.Live'
for the GHCi @play@ hot-swap, by 'Funktor.Grid.Binding' to commit Sequencer
toggle changes, and by Scene-mode pad presses to swap whole patterns.
-}
hotSwap :: TVar SchedulerState -> Stream Note -> STM ()
hotSwap var stream =
    modifyTVar' var $ \s ->
        s{schedStream = stream, schedBeat = Beat 0, schedPending = []}
