{-# LANGUAGE CPP #-}

{- | GHCi Live Interface for Funktor
Provides interactive music control from GHCi using atomic stream swapping,
optional MIDI input, Launchpad-driven grid play, and a file-watcher hook for
live-coded edits.
-}
module Funktor.Live (
    play,
    reload,
    stop,
    setTempo,

    -- * MIDI keyboard
    startMidi,
    startMidiWith,
    stopMidi,
    listMidiInputs,

    -- * Launchpad
    startLaunchpad,
    startLaunchpadWith,
    stopLaunchpad,
    listLaunchpadDevices,
    setGridMode,

    -- * Re-exports for convenient GHCi usage
    Stream,
    Note,
    Tempo (..),
    fromPattern,
    pentatonic,
    silence,
    merge,
    GridMode (..),
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
import Funktor.Audio.Scheduler qualified as Scheduler
import Funktor.Audio.State (AudioState (..))
import Funktor.Audio.Voice (emptyPool)
import Funktor.Core.Pattern (pentatonic)
import Funktor.Core.Stream (Stream, fromPattern, merge, silence)
import Funktor.Core.Types (Note, Tempo (..))
import Funktor.Grid.Binding (GridMode (..))
import Funktor.Live.Reload (persistAt, startWatcher, stopWatcher)
import GHC.Clock (getMonotonicTime)
import SDL qualified
import System.IO.Unsafe (unsafePerformIO)

import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async

#ifdef MIDI_ENABLED
import Control.Concurrent.STM (TQueue, newTQueueIO, readTQueue)
import Control.Monad (forever, when)
import Data.Maybe (isJust)
import Funktor.Grid.Binding (
    AudioEngine,
    engineMode,
    newAudioEngine,
    pressPad,
    releasePad,
    setMode,
    topRowModeSwitch,
    gridForMode,
 )
import Funktor.Hardware.Launchpad (
    LaunchpadConfig,
    LaunchpadEvent (..),
    defaultMk3Config,
    gridLedSysEx,
    gridToNote,
    ledSysEx,
    liveModeSysEx,
    midiToLaunchpadEvent,
    programmerModeSysEx,
 )
import Funktor.Grid (Color (Green, Off))
import Funktor.Hardware.MIDI (
    DeviceSelector (..),
    MidiConfig (..),
    MidiDeviceInfo (..),
    MidiHandle,
    MidiInputThread,
    MidiMessage,
    closeMidi,
    defaultMidiConfig,
    listInputDevices,
    listOutputDevices,
    midiHandleName,
    openInput,
    openOutput,
    sendSysEx,
    startInputThread,
    startMidiRouter,
    stopInputThread,
 )
#endif

-- ---------------------------------------------------------------------------
-- Handle synonyms
--
-- 'LiveState' always carries every concurrency slot so its shape is
-- independent of the build flag. With MIDI off the inner types collapse to
-- '()' and the slots are permanently 'Nothing'.
-- ---------------------------------------------------------------------------

#ifdef MIDI_ENABLED
type MidiInputHandle = MidiInputThread
type MidiRouterHandle = Async ()
type MidiQueueHandle = TQueue MidiMessage
type LaunchpadInHandle = MidiInputThread
type LaunchpadOutHandle = MidiHandle
type LaunchpadRouterHandle = Async ()
type LaunchpadQueueHandle = TQueue MidiMessage
type LaunchpadEngineHandle = AudioEngine
#else
type MidiInputHandle = ()
type MidiRouterHandle = ()
type MidiQueueHandle = ()
type LaunchpadInHandle = ()
type LaunchpadOutHandle = ()
type LaunchpadRouterHandle = ()
type LaunchpadQueueHandle = ()
type LaunchpadEngineHandle = ()

-- | Opaque stub so 'startMidiWith' / 'startLaunchpadWith' keep their
-- signatures whether or not the @midi@ flag is on. Has no inhabitants the
-- user can construct in this build.
data MidiConfig
data LaunchpadConfig
#endif

{- | Module-private 'TVar' holding the running session. Wrapped in 'persistAt'
so the value survives GHCi @:reload@ — without this, every reload would
drop the audio thread on the floor. Slot 0 is reserved for this handle;
do not reuse it for other 'persistAt' calls.
-}
{-# NOINLINE globalLive #-}
globalLive :: TVar (Maybe LiveState)
globalLive = unsafePerformIO (persistAt 0 (newTVarIO Nothing))

-- | Live session state
data LiveState = LiveState
    { liveStream :: Stream Note
    , liveTempo :: !Tempo
    , liveSchedVar :: !(TVar SchedulerState)
    , liveAudioVar :: !(TVar AudioState)
    , liveThreadId :: !(Maybe ThreadId)
    , liveDevice :: SDL.AudioDevice
    , liveMidi :: !(Maybe MidiInputHandle)
    , liveMidiRouter :: !(Maybe MidiRouterHandle)
    , liveMidiQueue :: !(Maybe MidiQueueHandle)
    , liveLaunchpadIn :: !(Maybe LaunchpadInHandle)
    , liveLaunchpadOut :: !(Maybe LaunchpadOutHandle)
    , liveLaunchpadRouter :: !(Maybe LaunchpadRouterHandle)
    , liveLaunchpadQueue :: !(Maybe LaunchpadQueueHandle)
    , liveEngine :: !(Maybe LaunchpadEngineHandle)
    , liveWatcher :: !(Maybe (Async ()))
    }

{- | Play a stream of notes in the live session. Starts a new session if
none exists; otherwise atomically swaps the current stream. Also spawns
the live-reload file watcher on first boot.
-}
play :: Stream Note -> IO ()
play stream = do
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> bootSession stream
        Just st -> do
            hotSwap st stream
            ensureWatcher

{- | Alias for 'play'. Provided so users get muscle-memory parity with the
@reload@ command they re-issue after a GHCi @:reload@.
-}
reload :: Stream Note -> IO ()
reload = play

bootSession :: Stream Note -> IO ()
bootSession stream = do
    (dev, audioVar) <- openDevice
    startTime <- getMonotonicTime
    schedVar <- newTVarIO (initialSchedulerState stream (Tempo 120) startTime)
    tid <- forkIO (schedulerThread audioVar schedVar)
    watcher <- startWatcher "."
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
                    , liveLaunchpadIn = Nothing
                    , liveLaunchpadOut = Nothing
                    , liveLaunchpadRouter = Nothing
                    , liveLaunchpadQueue = Nothing
                    , liveEngine = Nothing
                    , liveWatcher = Just watcher
                    }

ensureWatcher :: IO ()
ensureWatcher = do
    mst <- readTVarIO globalLive
    case mst of
        Just st | Nothing <- liveWatcher st -> do
            w <- startWatcher "."
            atomically $ modifyTVar' globalLive $ fmap $ \s -> s{liveWatcher = Just w}
        _ -> pure ()

hotSwap :: LiveState -> Stream Note -> IO ()
hotSwap st stream = atomically $ do
    Scheduler.hotSwap (liveSchedVar st) stream
    modifyTVar' globalLive $ fmap $ \s -> s{liveStream = stream}

{- | Stop the live session and clean up all resources. Teardown order
preserves the invariants of each subsystem: the watcher dies first (so its
log line doesn't reference a half-shut session), then the Launchpad (its
LED-reset SysEx needs the output handle still open), then keyboard MIDI
(so its router doesn't race a half-shut scheduler), then audio.
-}
stop :: IO ()
stop = do
    stopWatcherIfRunning
    stopLaunchpad
    stopMidi
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> putStrLn "Nothing playing."
        Just st -> do
            mapM_ killThread (liveThreadId st)
            atomically $ modifyTVar' (liveAudioVar st) silenceAllVoices
            atomically $ writeTVar globalLive Nothing
            closeDevice (liveDevice st)

stopWatcherIfRunning :: IO ()
stopWatcherIfRunning = do
    mst <- readTVarIO globalLive
    case mst of
        Just st | Just w <- liveWatcher st -> do
            stopWatcher w
            atomically $ modifyTVar' globalLive $ fmap $ \s -> s{liveWatcher = Nothing}
        _ -> pure ()

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

-- ---------------------------------------------------------------------------
-- Launchpad integration
-- ---------------------------------------------------------------------------

defaultLaunchpadConfig :: MidiConfig
defaultLaunchpadConfig =
    defaultMidiConfig{midiCfgDeviceSelector = ByNameSubstring "Launchpad"}

{- | Open the first Launchpad device, switch it into Programmer Mode, and
spawn an input + router pair that drives 'Funktor.Grid.Binding'. Requires
an active session.
-}
startLaunchpad :: IO ()
startLaunchpad =
    startLaunchpadWith defaultMk3Config defaultLaunchpadConfig defaultLaunchpadConfig

{- | Open in and out devices matching the provided 'MidiConfig' selectors,
configure the Launchpad described by 'LaunchpadConfig', and wire the input
into 'Funktor.Grid.Binding'.
-}
startLaunchpadWith :: LaunchpadConfig -> MidiConfig -> MidiConfig -> IO ()
startLaunchpadWith lpCfg inCfg outCfg = do
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> putStrLn "Nothing playing. Call play first."
        Just st
            | isJust (liveLaunchpadIn st) -> putStrLn "Launchpad already running."
            | otherwise -> do
                ein <- openInput inCfg
                case ein of
                    Left err -> putStrLn ("Launchpad in: " ++ err ++ " — plug in before starting GHCi.")
                    Right inH -> do
                        eout <- openOutput outCfg
                        case eout of
                            Left err -> do
                                closeMidi inH
                                putStrLn ("Launchpad out: " ++ err ++ " — plug in before starting GHCi.")
                            Right outH -> do
                                sendSysEx outH (programmerModeSysEx lpCfg)
                                engine <- newAudioEngine (liveAudioVar st) (liveSchedVar st)
                                initialMode <- readTVarIO (engineMode engine)
                                sendSysEx outH (gridLedSysEx lpCfg (gridForMode initialMode))
                                q <- newTQueueIO
                                inThread <- startInputThread inH q
                                routerAsync <- startLaunchpadRouter lpCfg q engine outH
                                atomically $ modifyTVar' globalLive $ fmap $ \s ->
                                    s
                                        { liveLaunchpadIn = Just inThread
                                        , liveLaunchpadOut = Just outH
                                        , liveLaunchpadRouter = Just routerAsync
                                        , liveLaunchpadQueue = Just q
                                        , liveEngine = Just engine
                                        }
                                putStrLn ("Launchpad: " ++ midiHandleName inH)

-- | Tear down Launchpad I/O. Quietly no-ops if nothing is running. Sends a
-- LED-off reset SysEx so the device returns to its default appearance.
stopLaunchpad :: IO ()
stopLaunchpad = do
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> pure ()
        Just st -> do
            let wasRunning = isJust (liveLaunchpadIn st)
            mapM_ Async.cancel (liveLaunchpadRouter st)
            mapM_ stopInputThread (liveLaunchpadIn st)
            mapM_ resetAndClose (liveLaunchpadOut st)
            atomically $ modifyTVar' globalLive $ fmap $ \s ->
                s
                    { liveLaunchpadIn = Nothing
                    , liveLaunchpadOut = Nothing
                    , liveLaunchpadRouter = Nothing
                    , liveLaunchpadQueue = Nothing
                    , liveEngine = Nothing
                    }
            when wasRunning $ putStrLn "Launchpad stopped."
  where
    resetAndClose h = do
        sendSysEx h (liveModeSysEx defaultMk3Config)
        closeMidi h

-- | Switch the Launchpad's dispatch mode and repaint the whole grid.
setGridMode :: GridMode -> IO ()
setGridMode mode = do
    mst <- readTVarIO globalLive
    case mst of
        Just st
            | Just engine <- liveEngine st
            , Just outH <- liveLaunchpadOut st -> do
                setMode engine mode
                sendSysEx outH (gridLedSysEx defaultMk3Config (gridForMode mode))
        _ -> putStrLn "Launchpad not running. Call startLaunchpad first."

listLaunchpadDevices :: IO ()
listLaunchpadDevices = do
    ins <- listInputDevices
    outs <- listOutputDevices
    putStrLn "Inputs:"
    mapM_ printDev ins
    putStrLn "Outputs:"
    mapM_ printDev outs
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

{- | Drain the Launchpad input queue and dispatch pad events. Top-row
presses switch mode (and repaint); body presses go to 'pressPad' /
'releasePad' and trigger a press-echo LED change. The whole-grid redraw
on mode change is the boring move — diff tracking is forbidden by house
style.
-}
startLaunchpadRouter
    :: LaunchpadConfig
    -> TQueue MidiMessage
    -> AudioEngine
    -> MidiHandle
    -> IO (Async ())
startLaunchpadRouter cfg q engine outH = Async.async $ forever $ do
    msg <- atomically (readTQueue q)
    case midiToLaunchpadEvent cfg msg of
        Just (PadDown (x, y) vel)
            | y == 8 -> handleTopRow x
            | otherwise -> do
                pressPad x y engine vel
                sendSysEx outH (ledSysEx cfg (gridToNote x y) Green)
        Just (PadUp (x, y))
            | y < 8 -> do
                releasePad x y engine
                sendSysEx outH (ledSysEx cfg (gridToNote x y) Off)
        _ -> pure ()
  where
    handleTopRow col = case topRowModeSwitch col of
        Just newMode -> do
            setMode engine newMode
            sendSysEx outH (gridLedSysEx cfg (gridForMode newMode))
        Nothing -> pure ()

#else

-- | Message printed by every MIDI/Launchpad entry point when the library was
-- built without the @midi@ cabal flag.
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

startLaunchpad :: IO ()
startLaunchpad = putStrLn midiDisabledMsg

startLaunchpadWith :: LaunchpadConfig -> MidiConfig -> MidiConfig -> IO ()
startLaunchpadWith _ _ _ = startLaunchpad

stopLaunchpad :: IO ()
stopLaunchpad = pure ()

listLaunchpadDevices :: IO ()
listLaunchpadDevices = putStrLn midiDisabledMsg

setGridMode :: GridMode -> IO ()
setGridMode _ = putStrLn midiDisabledMsg

#endif
