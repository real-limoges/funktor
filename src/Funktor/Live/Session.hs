module Funktor.Live.Session (
    SessionAction (..),
    SessionEvent (..),
    Session (..),
    MaterializedNote (..),
    startRecording,
    stopRecording,
    replaySession,
    materializeSession,
    exportMidi,
) where

import Control.Concurrent.STM (TVar)
import Funktor.Core.Types

import Funktor.Core.Stream (Stream)

-- | An action the user took during a live session.
data SessionAction
    = StreamSwap String (Stream Note)
    | TempoChange Tempo
    | PadPress Int
    | PadRelease Int

-- | A timestamped session event.
data SessionEvent = SessionEvent
    { seTime :: !Double
    , seBeat :: !Beat
    , seAction :: !SessionAction
    }

-- | A recording session.
data Session = Session
    { sessionEvents :: !(TVar [SessionEvent])
    , sessionStart :: !Double
    , sessionActive :: !(TVar Bool)
    }

-- | A fully materialized note for MIDI export.
data MaterializedNote = MaterializedNote
    { mnBeat :: !Beat
    , mnPitch :: !Pitch
    , mnDuration :: !Duration
    , mnVelocity :: !Velocity
    }
    deriving (Show)

startRecording :: IO Session
startRecording = undefined

stopRecording :: Session -> IO [SessionEvent]
stopRecording = undefined

replaySession :: [SessionEvent] -> IO ()
replaySession = undefined

materializeSession :: [SessionEvent] -> [MaterializedNote]
materializeSession = undefined

exportMidi :: FilePath -> [MaterializedNote] -> IO ()
exportMidi = undefined
