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
    AudioEngine (..),
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

-- 'LiveState' carries every concurrency slot whether or not the @midi@
-- flag is on. Without MIDI the inner types collapse to '()' and the slots
-- are permanently 'Nothing'.

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

data LiveState = LiveState
    { stream :: Stream Note
    , tempo :: !Tempo
    , schedVar :: !(TVar SchedulerState)
    , audioVar :: !(TVar AudioState)
    , threadId :: !(Maybe ThreadId)
    , device :: SDL.AudioDevice
    , midi :: !(Maybe MidiInputHandle)
    , midiRouter :: !(Maybe MidiRouterHandle)
    , midiQueue :: !(Maybe MidiQueueHandle)
    , launchpadIn :: !(Maybe LaunchpadInHandle)
    , launchpadOut :: !(Maybe LaunchpadOutHandle)
    , launchpadRouter :: !(Maybe LaunchpadRouterHandle)
    , launchpadQueue :: !(Maybe LaunchpadQueueHandle)
    , engine :: !(Maybe LaunchpadEngineHandle)
    , watcher :: !(Maybe (Async ()))
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
                    { stream = stream
                    , tempo = Tempo 120
                    , schedVar = schedVar
                    , audioVar = audioVar
                    , threadId = Just tid
                    , device = dev
                    , midi = Nothing
                    , midiRouter = Nothing
                    , midiQueue = Nothing
                    , launchpadIn = Nothing
                    , launchpadOut = Nothing
                    , launchpadRouter = Nothing
                    , launchpadQueue = Nothing
                    , engine = Nothing
                    , watcher = Just watcher
                    }

ensureWatcher :: IO ()
ensureWatcher = do
    mst <- readTVarIO globalLive
    case mst of
        Just st | Nothing <- st.watcher -> do
            w <- startWatcher "."
            atomically $ modifyTVar' globalLive $ fmap $ \s -> s{watcher = Just w}
        _ -> pure ()

hotSwap :: LiveState -> Stream Note -> IO ()
hotSwap st stream = atomically $ do
    Scheduler.hotSwap (st.schedVar) stream
    modifyTVar' globalLive $ fmap $ \s -> s{stream = stream}

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
            mapM_ killThread (st.threadId)
            atomically $ modifyTVar' (st.audioVar) silenceAllVoices
            atomically $ writeTVar globalLive Nothing
            closeDevice (st.device)

stopWatcherIfRunning :: IO ()
stopWatcherIfRunning = do
    mst <- readTVarIO globalLive
    case mst of
        Just st | Just w <- st.watcher -> do
            stopWatcher w
            atomically $ modifyTVar' globalLive $ fmap $ \s -> s{watcher = Nothing}
        _ -> pure ()

-- | Set the tempo of the currently playing session
setTempo :: Tempo -> IO ()
setTempo t = do
    mst <- readTVarIO globalLive
    case mst of
        Nothing -> putStrLn "Nothing playing. Call play first."
        Just st -> do
            atomically $ modifyTVar' st.schedVar (setSchedTempo t)
            atomically $ modifyTVar' globalLive (fmap (setLiveTempo t))
            putStrLn $ "Tempo: " ++ show (unTempo t) ++ " BPM"

setSchedTempo :: Tempo -> SchedulerState -> SchedulerState
setSchedTempo t s = s{tempo = t}

setLiveTempo :: Tempo -> LiveState -> LiveState
setLiveTempo t s = s{tempo = t}

-- | Silence all voices (helper for stop)
silenceAllVoices :: AudioState -> AudioState
silenceAllVoices st = st{pool = emptyPool}

#ifdef MIDI_ENABLED

{- | Open the first available MIDI input device and forward note-on/off
events into the audio pipeline. Requires an active session. Idempotent.
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
            | isJust (st.midi) -> putStrLn "MIDI input already running."
            | otherwise -> do
                eh <- openInput cfg
                case eh of
                    Left err -> putStrLn ("MIDI: " ++ err)
                    Right h -> do
                        let name = midiHandleName h
                        q <- newTQueueIO
                        inThread <- startInputThread h q
                        routerAsync <- startMidiRouter q (st.schedVar)
                        atomically $ modifyTVar' globalLive $ fmap $ \s ->
                            s
                                { midi = Just inThread
                                , midiRouter = Just routerAsync
                                , midiQueue = Just q
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
            let wasRunning = isJust (st.midi)
            mapM_ stopInputThread (st.midi)
            mapM_ Async.cancel (st.midiRouter)
            atomically $ modifyTVar' globalLive $ fmap $ \s ->
                s
                    { midi = Nothing
                    , midiRouter = Nothing
                    , midiQueue = Nothing
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
                ++ show d.devId
                ++ "] "
                ++ d.name
                ++ " ("
                ++ d.interface
                ++ ")"

defaultLaunchpadConfig :: MidiConfig
defaultLaunchpadConfig =
    defaultMidiConfig{deviceSelector = ByNameSubstring "Launchpad"}

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
            | isJust (st.launchpadIn) -> putStrLn "Launchpad already running."
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
                                engine <- newAudioEngine (st.audioVar) (st.schedVar)
                                initialMode <- readTVarIO engine.mode
                                sendSysEx outH (gridLedSysEx lpCfg (gridForMode initialMode))
                                q <- newTQueueIO
                                inThread <- startInputThread inH q
                                routerAsync <- startLaunchpadRouter lpCfg q engine outH
                                atomically $ modifyTVar' globalLive $ fmap $ \s ->
                                    s
                                        { launchpadIn = Just inThread
                                        , launchpadOut = Just outH
                                        , launchpadRouter = Just routerAsync
                                        , launchpadQueue = Just q
                                        , engine = Just engine
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
            let wasRunning = isJust (st.launchpadIn)
            mapM_ Async.cancel (st.launchpadRouter)
            mapM_ stopInputThread (st.launchpadIn)
            mapM_ resetAndClose (st.launchpadOut)
            atomically $ modifyTVar' globalLive $ fmap $ \s ->
                s
                    { launchpadIn = Nothing
                    , launchpadOut = Nothing
                    , launchpadRouter = Nothing
                    , launchpadQueue = Nothing
                    , engine = Nothing
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
            | Just engine <- st.engine
            , Just outH <- st.launchpadOut -> do
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
                ++ show d.devId
                ++ "] "
                ++ d.name
                ++ " ("
                ++ d.interface
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
