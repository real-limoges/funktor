{-# LANGUAGE CPP #-}

{- | GHCi Live Interface for Funktor
Provides interactive music control from GHCi using atomic stream swapping
-}
module Funktor.Live (
    play,
    stop,
    setTempo,
    startMidi,
    startMidiWith,
    stopMidi,
    listMidiInputs,

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
import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVarIO,
    writeTVar,
 )
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

#ifdef MIDI_ENABLED
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (TQueue, newTQueueIO)
import Control.Monad (when)
import Data.Maybe (isJust)
import Funktor.Hardware.MIDI (
    MidiConfig,
    MidiDeviceInfo (..),
    MidiInputThread,
    MidiMessage,
    defaultMidiConfig,
    listInputDevices,
    midiHandleName,
    openInput,
    startInputThread,
    startMidiRouter,
    stopInputThread,
 )
#endif

-- ---------------------------------------------------------------------------
-- MIDI handle synonyms
--
-- The 'LiveState' record always holds three MIDI slots so its shape is
-- independent of the build flag. With MIDI off the inner types collapse to
-- '()' and the slots are permanently 'Nothing'.
-- ---------------------------------------------------------------------------

#ifdef MIDI_ENABLED
type MidiInputHandle = MidiInputThread
type MidiRouterHandle = Async ()
type MidiQueueHandle = TQueue MidiMessage
#else
type MidiInputHandle = ()
type MidiRouterHandle = ()
type MidiQueueHandle = ()

-- | Opaque stub so 'startMidiWith' has a consistent type signature regardless
-- of build flag. Has no inhabitants the user can construct in this build.
data MidiConfig
#endif

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
    , liveMidi :: !(Maybe MidiInputHandle) -- MIDI input poller (Nothing = not running / disabled)
    , liveMidiRouter :: !(Maybe MidiRouterHandle) -- MIDI -> scheduler router thread
    , liveMidiQueue :: !(Maybe MidiQueueHandle) -- bridge between poller and router
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
                    , liveMidi = Nothing
                    , liveMidiRouter = Nothing
                    , liveMidiQueue = Nothing
                    }

hotSwap :: LiveState -> Stream Note -> IO ()
hotSwap st stream = atomically $ do
    modifyTVar' (liveSchedVar st) $ \s -> s{schedStream = stream, schedBeat = Beat 0, schedPending = []}
    modifyTVar' globalLive $ fmap $ \s -> s{liveStream = stream}

{- | Stop the live session and clean up all resources. MIDI input (if any) is
torn down first so the router doesn't write into a half-shut scheduler.
-}
stop :: IO ()
stop = do
    stopMidi
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> putStrLn "Nothing playing."
        Just st -> do
            mapM_ killThread (liveThreadId st)
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

-- ---------------------------------------------------------------------------
-- MIDI integration
-- ---------------------------------------------------------------------------

#ifdef MIDI_ENABLED

{- | Open the first available MIDI input device and start forwarding incoming
note-on/off events to the audio pipeline. Requires an active session ('play'
must have been called). Idempotent: calling twice is a no-op with a
friendly message.
-}
startMidi :: IO ()
startMidi = startMidiWith defaultMidiConfig

{- | Like 'startMidi' but with a custom 'MidiConfig' (device selector, poll
interval, etc.).
-}
startMidiWith :: MidiConfig -> IO ()
startMidiWith cfg = do
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> putStrLn "Nothing playing. Call play first."
        Just st
            | isJust (liveMidi st) -> putStrLn "MIDI input already running."
            | otherwise -> do
                eh <- openInput cfg
                case eh of
                    Left err -> putStrLn ("MIDI: " ++ err)
                    Right h -> do
                        let name = midiHandleName h
                        q <- newTQueueIO
                        inThread <- startInputThread h q
                        routerAsync <- startMidiRouter q (liveSchedVar st)
                        atomically $ modifyTVar' globalLive $ fmap $ \s ->
                            s
                                { liveMidi = Just inThread
                                , liveMidiRouter = Just routerAsync
                                , liveMidiQueue = Just q
                                }
                        putStrLn ("MIDI input: " ++ name)

{- | Tear down MIDI input. Quietly no-ops if MIDI isn't running. Called
automatically by 'stop'; available standalone for users who want to switch
devices without restarting audio.
-}
stopMidi :: IO ()
stopMidi = do
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> pure ()
        Just st -> do
            let wasRunning = isJust (liveMidi st)
            mapM_ stopInputThread (liveMidi st)
            mapM_ Async.cancel (liveMidiRouter st)
            atomically $ modifyTVar' globalLive $ fmap $ \s ->
                s
                    { liveMidi = Nothing
                    , liveMidiRouter = Nothing
                    , liveMidiQueue = Nothing
                    }
            when wasRunning $ putStrLn "MIDI input stopped."

{- | Print available MIDI input devices to stdout. Use the bracketed index
with 'Funktor.Hardware.MIDI.ByIndex' (or a substring with 'ByNameSubstring')
to target a specific device via 'startMidiWith'.
-}
listMidiInputs :: IO ()
listMidiInputs = do
    devs <- listInputDevices
    case devs of
        [] -> putStrLn "No MIDI input devices found."
        _ -> mapM_ printDev devs
  where
    printDev d =
        putStrLn $
            "  ["
                ++ show (midiDevId d)
                ++ "] "
                ++ midiDevName d
                ++ " ("
                ++ midiDevInterface d
                ++ ")"

#else

-- | Message printed by every MIDI entry point when the library was built
-- without the @midi@ cabal flag.
midiDisabledMsg :: String
midiDisabledMsg =
    "Funktor.Live: MIDI support disabled at build time "
        ++ "(rebuild with 'cabal build --flags=+midi')."

startMidi :: IO ()
startMidi = putStrLn midiDisabledMsg

startMidiWith :: MidiConfig -> IO ()
startMidiWith _ = startMidi

stopMidi :: IO ()
stopMidi = pure ()

listMidiInputs :: IO ()
listMidiInputs = putStrLn midiDisabledMsg

#endif
