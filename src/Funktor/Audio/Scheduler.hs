module Funktor.Audio.Scheduler
    ( SchedulerAction (..)
    , ScheduledEvent (..)
    , SchedulerState (..)
    , play
    , stop
    , schedulerThread
    ) where

import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, STM)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Monad (forever)
import qualified Data.List as L
import Funktor.Core.Types (Beat(..), Tempo(..), Pitch, Velocity, Note(..), Duration(..), Event(..))
import Funktor.Core.Stream (Stream, runStream)
import Funktor.Audio.Voice (VoicePool, poolNoteOn, poolNoteOff)

data SchedulerAction
    = SchedNoteOn  !Pitch !Velocity
    | SchedNoteOff !Pitch
    deriving (Show)

data ScheduledEvent = ScheduledEvent
    { schedTime   :: !Double
    , schedAction :: !SchedulerAction
    } deriving (Show)

data SchedulerState = SchedulerState
    { schedTempo     :: !Tempo
    , schedStream    :: !(Stream Note)
    , schedBeat      :: !Beat
    , schedStartTime :: !Double
    , schedPending   :: ![ScheduledEvent]
    , schedLookAhead :: !Double
    }

play :: TVar VoicePool -> TVar SchedulerState -> IO ThreadId
play poolVar stateVar = do
    let initState = SchedulerState
            { schedTempo = Tempo 120
            , schedStream = error "Stream not provided"
            , schedBeat = Beat 0
            , schedStartTime = 0
            , schedPending = []
            , schedLookAhead = 0.1
            }
    atomically $ writeTVar stateVar initState
    forkIO (schedulerThread poolVar stateVar)

stop :: ThreadId -> IO ()
stop = killThread

schedulerThread :: TVar VoicePool -> TVar SchedulerState -> IO ()
schedulerThread poolVar stateVar = forever $ do
    currentTime <- return 0
    processDueEvents poolVar stateVar currentTime
    scheduleNewEvents poolVar stateVar currentTime
    threadDelay 10000

processDueEvents :: TVar VoicePool -> TVar SchedulerState -> Double -> IO ()
processDueEvents poolVar stateVar currentTime = atomically $ do
    st <- readTVar stateVar
    let (due, remaining) = partitionEvents (schedPending st) currentTime
    writeTVar stateVar st { schedPending = remaining }
    mapM_ (runActionSTM poolVar currentTime) due

partitionEvents :: [ScheduledEvent] -> Double -> ([ScheduledEvent], [ScheduledEvent])
partitionEvents events currentTime = 
    let due = filter (\e -> schedTime e <= currentTime) events
        remaining = filter (\e -> schedTime e > currentTime) events
    in (due, remaining)

runActionSTM :: TVar VoicePool -> Double -> ScheduledEvent -> STM ()
runActionSTM poolVar currentTime event = case schedAction event of
    SchedNoteOn pitch vel -> modifyTVar' poolVar (poolNoteOn currentTime pitch vel)
    SchedNoteOff pitch -> modifyTVar' poolVar (poolNoteOff currentTime pitch)

scheduleNewEvents :: TVar VoicePool -> TVar SchedulerState -> Double -> IO ()
scheduleNewEvents _ stateVar _ = atomically $ do
    st <- readTVar stateVar
    let nextBeat = schedBeat st + Beat (secondsToBeats (schedTempo st) (schedLookAhead st))
        newEvents = eventsToActions (schedTempo st) (schedStartTime st) (schedBeat st) nextBeat (schedStream st)
        merged = sortEvents (schedPending st ++ newEvents)
    writeTVar stateVar st { schedBeat = nextBeat, schedPending = merged }

eventsToActions :: Tempo -> Double -> Beat -> Beat -> Stream Note -> [ScheduledEvent]
eventsToActions tempo startTime fromBeat toBeat stream =
    let events = runStream stream fromBeat toBeat
    in concatMap (eventToActions tempo startTime) events

eventToActions :: Tempo -> Double -> Event Note -> [ScheduledEvent]
eventToActions tempo startTime (Event beat (Note pitch duration velocity)) =
    [ ScheduledEvent (startTime + beatToSeconds tempo (unBeat beat)) (SchedNoteOn pitch velocity)
    , ScheduledEvent (startTime + beatToSeconds tempo (unBeat beat + unDuration duration)) (SchedNoteOff pitch)
    ]

beatToSeconds :: Tempo -> Rational -> Double
beatToSeconds (Tempo bpm) beats = fromRational beats * 60 / bpm

secondsToBeats :: Tempo -> Double -> Rational
secondsToBeats (Tempo bpm) secs = toRational (secs * bpm / 60)

sortEvents :: [ScheduledEvent] -> [ScheduledEvent]
sortEvents = L.sortBy (\a b -> compare (schedTime a) (schedTime b))
