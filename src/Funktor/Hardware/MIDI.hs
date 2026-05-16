module Funktor.Hardware.MIDI (
    -- * Pure data
    MidiMessage (..),
    parseMidiMessage,
    encodeMidiMessage,

    -- * Device discovery
    MidiDeviceInfo (..),
    listInputDevices,
    listOutputDevices,

    -- * Handles
    MidiHandle,
    midiHandleName,
    MidiConfig (..),
    DeviceSelector (..),
    defaultMidiConfig,
    openInput,
    openOutput,
    closeMidi,

    -- * Sending
    sendMessage,
    sendSysEx,

    -- * Input thread + SysEx reassembly
    RxState (..),
    initialRxState,
    stepRx,
    MidiInputThread,
    startInputThread,
    stopInputThread,

    -- * Scheduler routing
    midiToSchedAction,
    startMidiRouter,

    -- * Library lifecycle
    initializeMidi,
    terminateMidi,
    withMidi,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (TQueue, TVar, atomically, readTQueue, writeTQueue)
import Control.Exception (bracket_)
import Control.Monad (forever, void)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, toLower)
import Data.List (isInfixOf)
import Data.Word (Word8)
import Foreign.C.Types (CLong)
import Funktor.Audio.Scheduler (SchedulerAction (..), SchedulerState, enqueueImmediate)
import Funktor.Core.Types (Pitch (..), Velocity (..))
import Sound.PortMidi qualified as PM
import System.IO.Unsafe (unsafePerformIO)

{- | Parsed MIDI message. Channel fields are 0..15. Velocity is normalised to
[0, 1] from the 7-bit MIDI range. PitchBend is the assembled 14-bit value
(8192 = center). SysEx payloads exclude the F0/F7 framing bytes.
-}
data MidiMessage
    = NoteOn !Int !Pitch !Velocity
    | NoteOff !Int !Pitch !Velocity
    | ControlChange !Int !Int !Int
    | PitchBend !Int !Int
    | PolyAftertouch !Int !Pitch !Int
    | ChannelAftertouch !Int !Int
    | ProgramChange !Int !Int
    | SysEx ![Word8]
    | Unknown ![Word8]
    deriving (Eq, Show)

{- | Parse a single MIDI message from raw bytes. Recognises channel-voice
messages (status nibble 0x80..0xE0) and SysEx frames (0xF0..0xF7).
Everything else (system real-time, malformed, or truncated inputs) becomes
'Unknown'.
-}
parseMidiMessage :: [Word8] -> MidiMessage
parseMidiMessage bs = case bs of
    [] -> Unknown []
    status : rest
        | status == 0xF0 -> parseSysEx bs
        | otherwise -> parseChannelVoice status rest bs

parseChannelVoice :: Word8 -> [Word8] -> [Word8] -> MidiMessage
parseChannelVoice status rest original = case status .&. 0xF0 of
    0x80 -> case rest of
        [p, v] -> NoteOff ch (Pitch (fromIntegral p)) (vel v)
        _ -> Unknown original
    0x90 -> case rest of
        [p, 0] -> NoteOff ch (Pitch (fromIntegral p)) (Velocity 0)
        [p, v] -> NoteOn ch (Pitch (fromIntegral p)) (vel v)
        _ -> Unknown original
    0xA0 -> case rest of
        [p, pr] -> PolyAftertouch ch (Pitch (fromIntegral p)) (fromIntegral pr)
        _ -> Unknown original
    0xB0 -> case rest of
        [c, v] -> ControlChange ch (fromIntegral c) (fromIntegral v)
        _ -> Unknown original
    0xC0 -> case rest of
        [p] -> ProgramChange ch (fromIntegral p)
        _ -> Unknown original
    0xD0 -> case rest of
        [pr] -> ChannelAftertouch ch (fromIntegral pr)
        _ -> Unknown original
    0xE0 -> case rest of
        [lsb, msb] ->
            PitchBend ch ((fromIntegral msb `shiftL` 7) .|. fromIntegral lsb)
        _ -> Unknown original
    _ -> Unknown original
  where
    ch = fromIntegral (status .&. 0x0F)
    vel v = Velocity (fromIntegral v / 127)

parseSysEx :: [Word8] -> MidiMessage
parseSysEx bs = case bs of
    0xF0 : rest -> SysEx (takeWhile (/= 0xF7) rest)
    _ -> Unknown bs

{- | Serialise a 'MidiMessage' back to raw bytes. Inverse of 'parseMidiMessage'
on the subset of messages whose fields are in range. Out-of-range integer
fields are clamped to 7 bits (or 14 for PitchBend); velocities outside [0,1]
are clamped to [0,127] after scaling.
-}
encodeMidiMessage :: MidiMessage -> [Word8]
encodeMidiMessage m = case m of
    NoteOff c p v -> [0x80 .|. ch7 c, pByte p, vByte v]
    NoteOn c p v -> [0x90 .|. ch7 c, pByte p, vByte v]
    PolyAftertouch c p pr -> [0xA0 .|. ch7 c, pByte p, clamp7 pr]
    ControlChange c k v -> [0xB0 .|. ch7 c, clamp7 k, clamp7 v]
    ProgramChange c p -> [0xC0 .|. ch7 c, clamp7 p]
    ChannelAftertouch c pr -> [0xD0 .|. ch7 c, clamp7 pr]
    PitchBend c v ->
        let v14 = max 0 (min 16383 v)
         in [ 0xE0 .|. ch7 c
            , fromIntegral (v14 .&. 0x7F)
            , fromIntegral ((v14 `shiftR` 7) .&. 0x7F)
            ]
    SysEx bs -> 0xF0 : bs ++ [0xF7]
    Unknown bs -> bs
  where
    ch7 c = fromIntegral (c .&. 0x0F)
    pByte (Pitch p) = fromIntegral (p .&. 0x7F)
    vByte (Velocity v) =
        let scaled = round (v * 127) :: Int
         in fromIntegral (max 0 (min 127 scaled))
    clamp7 x = fromIntegral (max 0 (min 127 x))

data MidiDeviceInfo = MidiDeviceInfo
    { devId :: !Int
    , name :: !String
    , interface :: !String
    , input :: !Bool
    , output :: !Bool
    }
    deriving (Eq, Show)

{- | All input-capable devices visible to PortMidi. PortMidi snapshots devices
at 'initializeMidi' and 0.2 exposes no refresh primitive, so plugging in a
device after this call won't make it appear; restart the process.
-}
listInputDevices :: IO [MidiDeviceInfo]
listInputDevices = filter (.input) <$> listAllDevices

-- | All output-capable devices visible to PortMidi.
listOutputDevices :: IO [MidiDeviceInfo]
listOutputDevices = filter (.output) <$> listAllDevices

listAllDevices :: IO [MidiDeviceInfo]
listAllDevices = do
    initializeMidi
    n <- PM.countDevices
    mapM toMidiDev [0 .. n - 1]
  where
    toMidiDev i = do
        di <- PM.getDeviceInfo i
        pure
            MidiDeviceInfo
                { devId = i
                , name = PM.name di
                , interface = PM.interface di
                , input = PM.input di
                , output = PM.output di
                }

data DeviceSelector
    = ByIndex !Int
    | ByNameSubstring !String
    deriving (Eq, Show)

data MidiConfig = MidiConfig
    { deviceSelector :: !DeviceSelector
    , bufferSize :: !Int
    , pollIntervalUs :: !Int
    , outputLatencyMs :: !Int
    }
    deriving (Eq, Show)

{- | Pick the first matching device, 256-event input buffer, 1 ms poll, zero
output latency. 'ByNameSubstring ""' matches every device, so the first one
in each direction wins.
-}
defaultMidiConfig :: MidiConfig
defaultMidiConfig =
    MidiConfig
        { deviceSelector = ByNameSubstring ""
        , bufferSize = 256
        , pollIntervalUs = 1000
        , outputLatencyMs = 0
        }

data Direction = DirIn | DirOut deriving (Eq, Show)

{- | Opaque handle to an open PortMidi stream. Constructor intentionally
unexported.
-}
data MidiHandle = MidiHandle
    { stream :: !PM.PMStream
    , dir :: !Direction
    , devId :: !Int
    , name :: !String
    , config :: !MidiConfig
    }

{- | Open the first device matching the selector. Returns 'Left' with an
explanation if no device matches or PortMidi rejects the open.
-}
openInput :: MidiConfig -> IO (Either String MidiHandle)
openInput cfg = do
    initializeMidi
    devs <- listInputDevices
    case selectDevice cfg.deviceSelector devs of
        Nothing -> pure $ Left ("No input device matching " ++ show cfg.deviceSelector)
        Just dev -> do
            res <- PM.openInput dev.devId
            case res of
                Left err -> pure $ Left ("PortMidi openInput failed: " ++ show err)
                Right s ->
                    pure $
                        Right
                            MidiHandle
                                { stream = s
                                , dir = DirIn
                                , devId = dev.devId
                                , name = dev.name
                                , config = cfg
                                }

openOutput :: MidiConfig -> IO (Either String MidiHandle)
openOutput cfg = do
    initializeMidi
    devs <- listOutputDevices
    case selectDevice cfg.deviceSelector devs of
        Nothing -> pure $ Left ("No output device matching " ++ show cfg.deviceSelector)
        Just dev -> do
            res <- PM.openOutput dev.devId cfg.outputLatencyMs
            case res of
                Left err -> pure $ Left ("PortMidi openOutput failed: " ++ show err)
                Right s ->
                    pure $
                        Right
                            MidiHandle
                                { stream = s
                                , dir = DirOut
                                , devId = dev.devId
                                , name = dev.name
                                , config = cfg
                                }

closeMidi :: MidiHandle -> IO ()
closeMidi h = void $ PM.close h.stream

{- | Human-readable device name from a 'MidiHandle'. Useful for status output
("MIDI input: Launchpad Mini Mk3 LPMiniMK3 MIDI Out").
-}
midiHandleName :: MidiHandle -> String
midiHandleName h = h.name

selectDevice :: DeviceSelector -> [MidiDeviceInfo] -> Maybe MidiDeviceInfo
selectDevice sel devs = case sel of
    ByIndex i -> case filter ((== i) . (.devId)) devs of
        d : _ -> Just d
        [] -> Nothing
    ByNameSubstring needle -> case filter (matches needle . (.name)) devs of
        d : _ -> Just d
        [] -> Nothing
  where
    matches needle n = map toLower needle `isInfixOf` map toLower n

{- | Dispatches on the message kind: short messages go through 'PM.writeShort',
SysEx through 'PM.writeSysEx'. 'Unknown' is sent as raw bytes (best-effort).
-}
sendMessage :: MidiHandle -> MidiMessage -> IO ()
sendMessage h m = case m of
    SysEx bs -> sendSysEx h bs
    _ -> sendShort h (encodeMidiMessage m)

sendShort :: MidiHandle -> [Word8] -> IO ()
sendShort h bs = void $ PM.writeShort h.stream (PM.PMEvent (packShort bs) 0)

{- | Pack up to 3 bytes little-endian into a 32-bit message word (status in
byte 0, data1 in byte 1, data2 in byte 2).
-}
packShort :: [Word8] -> CLong
packShort bs = case bs of
    [s] -> w s
    [s, d1] -> w s .|. (w d1 `shiftL` 8)
    [s, d1, d2] -> w s .|. (w d1 `shiftL` 8) .|. (w d2 `shiftL` 16)
    _ -> 0
  where
    w :: Word8 -> CLong
    w = fromIntegral

{- | Send a SysEx payload. F0/F7 framing is added automatically; do not include
it in the input.
-}
sendSysEx :: MidiHandle -> [Word8] -> IO ()
sendSysEx h bs = void $ PM.writeSysEx h.stream 0 (toCString (0xF0 : bs ++ [0xF7]))
  where
    -- PortMidi's writeSysEx marshals a Haskell String through withCAString,
    -- treating each Char's low byte as a raw MIDI byte. SysEx data is 7-bit
    -- and F0/F7 are non-zero, so this conversion never produces a null byte
    -- that would truncate the C string.
    toCString = map (chr . fromIntegral)

{- | Rolling state used by 'stepRx' to reassemble SysEx frames across multiple
PortMidi events. Outside of an active SysEx run both fields are 'False' /
empty respectively.
-}
data RxState = RxState
    { inSysEx :: !Bool
    -- ^ True between an 0xF0 status byte and the terminating 0xF7.
    , buf :: ![Word8]
    -- ^ Accumulated payload bytes in REVERSE order. Empty unless mid-sysex.
    }
    deriving (Eq, Show)

initialRxState :: RxState
initialRxState = RxState False []

{- | Decode a batch of PortMidi events into 'MidiMessage's. Channel-voice
messages decode one-per-event; SysEx may span any number of events. Real-
time status bytes (0xF8..0xFF) arriving mid-sysex are emitted as separate
(Unknown) messages without breaking sysex reassembly.
-}
stepRx :: RxState -> [PM.PMEvent] -> ([MidiMessage], RxState)
stepRx st [] = ([], st)
stepRx st (e : rest) =
    let (here, st') = processEvent st e
        (later, stFinal) = stepRx st' rest
     in (here ++ later, stFinal)

processEvent :: RxState -> PM.PMEvent -> ([MidiMessage], RxState)
processEvent st e
    | st.inSysEx = case allEventBytes e of
        b : _ | b >= 0xF8 -> ([shortMessageFromPM e], st)
        bs -> sysexConsume st bs
    | otherwise = case allEventBytes e of
        0xF0 : rest -> sysexConsume (RxState True []) rest
        _ -> ([shortMessageFromPM e], st)

{- | Consume bytes while in sysex mode. On 0xF7, emit the accumulated payload
and drop any trailing bytes in the same event (usually zero padding).
-}
sysexConsume :: RxState -> [Word8] -> ([MidiMessage], RxState)
sysexConsume st [] = ([], st)
sysexConsume st (b : bs)
    | b == 0xF7 = ([SysEx (reverse st.buf)], RxState False [])
    | otherwise = sysexConsume st{buf = b : st.buf} bs

-- | Extract the 4 little-endian bytes from a PMEvent message word.
allEventBytes :: PM.PMEvent -> [Word8]
allEventBytes e =
    let w = PM.message e
     in [byteAt w i | i <- [0, 1, 2, 3]]
  where
    byteAt :: CLong -> Int -> Word8
    byteAt w i = fromIntegral ((w `shiftR` (8 * i)) .&. 0xFF)

{- | Decode a PortMidi short-message event using the status byte's natural
length: 1 byte for system real-time, 2 for ProgramChange / ChannelAftertouch,
3 for everything else. Trailing padding zeros are stripped before handoff
to 'parseMidiMessage'.
-}
shortMessageFromPM :: PM.PMEvent -> MidiMessage
shortMessageFromPM e =
    let msg = PM.decodeMsg (PM.message e)
        s = fromIntegral (PM.status msg) :: Word8
        d1 = fromIntegral (PM.data1 msg) :: Word8
        d2 = fromIntegral (PM.data2 msg) :: Word8
        bytes
            | s >= 0xF8 = [s]
            | s .&. 0xF0 == 0xC0 = [s, d1]
            | s .&. 0xF0 == 0xD0 = [s, d1]
            | otherwise = [s, d1, d2]
     in parseMidiMessage bytes

{- | Opaque handle to a running input poller. Carries the underlying
'MidiHandle' so 'stopInputThread' can close the stream after cancelling.
-}
data MidiInputThread = MidiInputThread
    { task :: !(Async ())
    , handle :: !MidiHandle
    }

{- | Spawn a background thread that polls the input handle and pushes parsed
'MidiMessage's onto the queue. The thread reassembles multi-event SysEx
frames transparently. Cancellation via 'stopInputThread' is the only way to
end the loop.
-}
startInputThread :: MidiHandle -> TQueue MidiMessage -> IO MidiInputThread
startInputThread h q = do
    let pollUs = h.config.pollIntervalUs
    a <- Async.async (loop pollUs initialRxState)
    pure MidiInputThread{task = a, handle = h}
  where
    loop pollUs st = do
        r <- PM.readEvents h.stream
        st' <- case r of
            -- Transient PortMidi error: skip this tick, the next will retry.
            -- A persistent fault keeps firing until 'stopInputThread' cancels.
            Left _err -> pure st
            Right [] -> pure st
            Right evs -> do
                let (msgs, st2) = stepRx st evs
                atomically (mapM_ (writeTQueue q) msgs)
                pure st2
        threadDelay pollUs
        loop pollUs st'

{- | Cancel the poller and close the underlying handle. Order matters:
'Async.cancel' must complete before 'PM.close' or PortMidi can read into
freed memory.
-}
stopInputThread :: MidiInputThread -> IO ()
stopInputThread mit = do
    Async.cancel mit.task
    closeMidi mit.handle

{- | Collapse a MIDI keyboard message into a 'SchedulerAction'. Channels are
ignored: the voice pool is mono-summed, so a note-on on any channel plays
the same note. CC, pitch bend, aftertouch, program change, and SysEx have
no audible effect in the current pipeline and return 'Nothing'.
-}
midiToSchedAction :: MidiMessage -> Maybe SchedulerAction
midiToSchedAction msg = case msg of
    NoteOn _ p v -> Just (SchedNoteOn p v)
    NoteOff _ p _ -> Just (SchedNoteOff p)
    _ -> Nothing

{- | Spawn a background thread that drains the message queue (populated by
'startInputThread') and feeds note-on/off events into the scheduler.
Blocks on 'readTQueue' between events; no polling.
-}
startMidiRouter :: TQueue MidiMessage -> TVar SchedulerState -> IO (Async ())
startMidiRouter q schedVar = Async.async $ forever $ atomically $ do
    msg <- readTQueue q
    case midiToSchedAction msg of
        Just act -> enqueueImmediate schedVar act
        Nothing -> pure ()

{-# NOINLINE midiInitGuard #-}

{- | PortMidi's 'PM.initialize' is not idempotent at the C level. This MVar
ensures it runs at most once per process. Held alive via the standard
'unsafePerformIO' + NOINLINE idiom (same pattern as 'Funktor.Live').
-}
midiInitGuard :: MVar Bool
midiInitGuard = unsafePerformIO (newMVar False)

{- | Initialise PortMidi if it hasn't been initialised yet. Idempotent. Called
automatically by 'openInput' / 'openOutput' / 'listInputDevices' etc., but
exposed for tests and power users.
-}
initializeMidi :: IO ()
initializeMidi = modifyMVar_ midiInitGuard $ \done ->
    if done
        then pure True
        else do
            r <- PM.initialize
            case r of
                Left err ->
                    ioError
                        ( userError
                            ("Funktor.Hardware.MIDI: PortMidi initialize failed: " ++ show err)
                        )
                Right _ -> pure True

{- | Tear down PortMidi. Calling this while any 'MidiHandle' is still open
crashes inside the PortMidi C library — always 'closeMidi' first.
-}
terminateMidi :: IO ()
terminateMidi = modifyMVar_ midiInitGuard $ \done ->
    if done
        then do
            _ <- PM.terminate
            pure False
        else pure False

{- | Bracket an action between 'initializeMidi' and 'terminateMidi'. Useful in
tests; not normally what you want in a long-running GHCi session.
-}
withMidi :: IO a -> IO a
withMidi = bracket_ initializeMidi terminateMidi
