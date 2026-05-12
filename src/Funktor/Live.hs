{- | GHCi Live Interface for Funktor
Provides interactive music control from GHCi using atomic stream swapping
-}
module Funktor.Live (
    play,
    stop,
    setTempo,

    -- * Re-exports for convenient GHCi usage
    Stream,
    Note,
    Tempo (..),
    fromPattern,
    pentatonic,
    silence,
    merge,
) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Funktor.Audio (closeDevice, openDevice)
import Funktor.Audio.Scheduler (SchedulerState (..), initialSchedulerState, schedulerThread)
import Funktor.Audio.State (AudioState (..))
import Funktor.Audio.Voice (emptyPool)
import Funktor.Core.Pattern (pentatonic)
import Funktor.Core.Stream (Stream, fromPattern, merge, silence)
import Funktor.Core.Types (Beat (..), Note, Tempo (..))
import GHC.Clock (getMonotonicTime)
import SDL qualified
import System.IO.Unsafe (unsafePerformIO)

-- | Global live session state
{-# NOINLINE globalLive #-}
globalLive :: TVar (Maybe LiveState)
globalLive = unsafePerformIO (newTVarIO Nothing)

-- | Live session state
data LiveState = LiveState
    { liveStream :: Stream Note -- what's currently playing
    , liveTempo :: !Tempo -- current BPM
    , liveSchedVar :: !(TVar SchedulerState) -- scheduler's own state
    , liveAudioVar :: !(TVar AudioState) -- the voice pool
    , liveThreadId :: !(Maybe ThreadId) -- scheduler thread (Nothing = stopped)
    , liveDevice :: SDL.AudioDevice -- audio device handle
    }

{- | Play a stream of notes in the live session
Starts a new session if none exists, or swaps the current stream if one exists
-}
play :: Stream Note -> IO ()
play stream = do
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> bootSession stream
        Just st -> hotSwap st stream

bootSession :: Stream Note -> IO ()
bootSession stream = do
    (dev, audioVar) <- openDevice
    startTime <- getMonotonicTime
    schedVar <- newTVarIO (initialSchedulerState stream (Tempo 120) startTime)
    tid <- forkIO (schedulerThread audioVar schedVar)
    atomically $
        writeTVar globalLive $
            Just
                LiveState
                    { liveStream = stream
                    , liveTempo = Tempo 120
                    , liveSchedVar = schedVar
                    , liveAudioVar = audioVar
                    , liveThreadId = Just tid
                    , liveDevice = dev
                    }

hotSwap :: LiveState -> Stream Note -> IO ()
hotSwap st stream = atomically $ do
    modifyTVar' (liveSchedVar st) $ \s -> s{schedStream = stream, schedBeat = Beat 0, schedPending = []}
    modifyTVar' globalLive $ fmap $ \s -> s{liveStream = stream}

-- | Stop the live session and clean up all resources
stop :: IO ()
stop = do
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> putStrLn "Nothing playing."
        Just st -> do
            mapM_ killThread (liveThreadId st)
            -- Silence all voices
            atomically $ modifyTVar' (liveAudioVar st) silenceAllVoices
            atomically $ writeTVar globalLive Nothing
            closeDevice (liveDevice st)

-- | Set the tempo of the currently playing session
setTempo :: Tempo -> IO ()
setTempo tempo = do
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> putStrLn "Nothing playing. Call play first."
        Just st -> do
            atomically $ modifyTVar' (liveSchedVar st) $ \s ->
                s{schedTempo = tempo}
            atomically $ modifyTVar' globalLive $ fmap $ \s ->
                s{liveTempo = tempo}
            putStrLn $ "Tempo: " ++ show (unTempo tempo) ++ " BPM"

-- | Silence all voices (helper for stop)
silenceAllVoices :: AudioState -> AudioState
silenceAllVoices st = st{audioPool = emptyPool}
