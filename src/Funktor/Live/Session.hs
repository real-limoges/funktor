module Funktor.Live.Session (
    SessionAction (..),
    SessionEvent (..),
    Session (..),
    MaterializedNote (..),
    startRecording,
    stopRecording,
    recordEvent,
    replaySession,
    materializeSession,
    exportMidi,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad (when)
import Data.Bits ((.&.), (.|.))
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Word (Word32, Word8)
import GHC.Clock (getMonotonicTime)

import Funktor.Core.Stream (Stream)
import Funktor.Core.Types

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

{- | Open a new recording. Subsequent 'recordEvent' calls append while
'sessionActive' is True; 'stopRecording' flushes the buffer.
-}
startRecording :: IO Session
startRecording = do
    start <- getMonotonicTime
    evs <- newTVarIO []
    active <- newTVarIO True
    pure (Session evs start active)

-- | Stop recording and return the chronologically-ordered event list.
stopRecording :: Session -> IO [SessionEvent]
stopRecording sess = do
    atomically $ writeTVar sess.sessionActive False
    reversed <- readTVarIO sess.sessionEvents
    pure (reverse reversed)

-- | Append an event if the session is still active. Cheap no-op otherwise.
recordEvent :: Session -> SessionAction -> IO ()
recordEvent sess action = do
    active <- readTVarIO sess.sessionActive
    when active $ do
        now <- getMonotonicTime
        let ev =
                SessionEvent
                    { seTime = now - sess.sessionStart
                    , seBeat = Beat 0
                    , seAction = action
                    }
        atomically $ modifyTVar' sess.sessionEvents (ev :)

{- | Replay a session by printing its event timeline. Wiring back into a
running scheduler requires the caller to hold the relevant TVar handles;
that integration lives in 'Funktor.Live' and is out of scope here.
-}
replaySession :: [SessionEvent] -> IO ()
replaySession = mapM_ describe
  where
    describe e = putStrLn (show (seTime e) ++ "s: " ++ describeAction (seAction e))
    describeAction (StreamSwap label _) = "stream swap " ++ label
    describeAction (TempoChange (Tempo bpm)) = "tempo " ++ show bpm
    describeAction (PadPress n) = "press " ++ show n
    describeAction (PadRelease n) = "release " ++ show n

{- | Walk events pairing 'PadPress' with the next matching 'PadRelease' at
the same pad index to produce 'MaterializedNote's. Unpaired presses fall
back to a 1-beat default duration. Tempo + stream-swap events are dropped.
The pad number is reused as the MIDI pitch; velocity defaults to full.
-}
materializeSession :: [SessionEvent] -> [MaterializedNote]
materializeSession = go
  where
    go (SessionEvent t _ (PadPress n) : rest) =
        let (releaseT, leftover) = matchRelease n rest
            dur = max 1 (releaseT - t)
         in MaterializedNote
                { mnBeat = Beat (toRational t)
                , mnPitch = Pitch n
                , mnDuration = Duration (toRational dur)
                , mnVelocity = Velocity 1.0
                }
                : go leftover
    go (_ : rest) = go rest
    go [] = []

    matchRelease pad (SessionEvent t _ (PadRelease p) : rest)
        | p == pad = (t, rest)
    matchRelease pad (other : rest) =
        let (t, leftover) = matchRelease pad rest
         in (t, other : leftover)
    matchRelease _ [] = (1, [])

{- | Write a minimal Type-0 MIDI file with one track containing the supplied
notes. The output uses 480 ticks per quarter-note and treats one beat as
one quarter; durations in 'Duration' are converted to ticks directly.
-}
exportMidi :: FilePath -> [MaterializedNote] -> IO ()
exportMidi path notes = BL.writeFile path (BB.toLazyByteString builder)
  where
    builder = header <> track
    header =
        BB.string7 "MThd"
            <> BB.word32BE 6
            <> BB.word16BE 0 -- format 0
            <> BB.word16BE 1 -- one track
            <> BB.word16BE 480 -- ticks per quarter
    track =
        let body = BB.toLazyByteString (trackBody notes)
            bodyLen :: Word32
            bodyLen = fromIntegral (BL.length body)
         in BB.string7 "MTrk"
                <> BB.word32BE bodyLen
                <> BB.lazyByteString body

    trackBody ns =
        let pairs = concatMap toEvents ns
            sorted = sortOn fst pairs
         in deltaEncode 0 sorted <> endOfTrack

    toEvents n =
        [ (beatToTicks (mnBeat n), NoteOnE (mnPitch n) (mnVelocity n))
        ,
            ( beatToTicks (mnBeat n + Beat (unDuration (mnDuration n)))
            , NoteOffE (mnPitch n)
            )
        ]

    deltaEncode _ [] = mempty
    deltaEncode prev ((t, e) : rest) =
        let delta = max 0 (t - prev)
         in vlq delta <> midiEvent e <> deltaEncode t rest

    endOfTrack =
        BB.word8 0 -- delta 0
            <> BB.word8 0xFF
            <> BB.word8 0x2F
            <> BB.word8 0x00

beatToTicks :: Beat -> Integer
beatToTicks (Beat r) = round (r * 480)

data MidiEv = NoteOnE Pitch Velocity | NoteOffE Pitch

midiEvent :: MidiEv -> BB.Builder
midiEvent (NoteOnE (Pitch p) (Velocity v)) =
    BB.word8 0x90 <> BB.word8 (clamp7 p) <> BB.word8 (velocityByte v)
midiEvent (NoteOffE (Pitch p)) =
    BB.word8 0x80 <> BB.word8 (clamp7 p) <> BB.word8 0

clamp7 :: Int -> Word8
clamp7 x = fromIntegral (max 0 (min 127 x))

velocityByte :: Double -> Word8
velocityByte v = clamp7 (round (v * 127))

{- | MIDI variable-length quantity. Splits @n@ into 7-bit groups, big-endian
order; every byte except the last has its high bit set.
-}
vlq :: Integer -> BB.Builder
vlq n0 = mconcat (zipWith emit septets [0 :: Int ..])
  where
    septets = reverse (collect (max 0 n0))
    collect n
        | n < 128 = [fromIntegral n]
        | otherwise = let (q, r) = n `divMod` 128 in fromIntegral r : collect q
    lastIdx = length septets - 1
    emit b idx
        | idx == lastIdx = BB.word8 (b .&. 0x7F)
        | otherwise = BB.word8 ((b .&. 0x7F) .|. 0x80)
