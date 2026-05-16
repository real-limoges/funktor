{- | OSC client for SuperCollider's @scsynth@. Owns a UDP socket, allocates
node ids for synth instances, and tracks the currently-sounding node per
pitch so 'noteOff' can target the right synth.

The audio engine itself lives in @scsynth@; this module is the wire format.
A 'SchedNoteOn' from "Funktor.Audio.Scheduler" becomes an OSC @/s_new@
message; a 'SchedNoteOff' becomes @/n_set [id, "gate", 0]@ which lets the
SynthDef's 'EnvGen' run its release segment and free itself via
@doneAction: 2@.
-}
module Funktor.Audio.SC (
    SCConn,
    defaultPort,
    connect,
    disconnect,
    statusOk,
    noteOn,
    noteOff,
    releaseAll,
    freeAll,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, stateTVar)
import Control.Exception (SomeException, try)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Funktor.Audio.Timbre (Timbre (..))
import Funktor.Core.Types (Pitch, Velocity, midiToFreq, velocityToAmplitude)
import Sound.Osc.Fd (Datum (..), Message (..), ascii, recvMessage, sendMessage)
import Sound.Osc.Transport.Fd (close)
import Sound.Osc.Transport.Fd.Udp (Udp, openUdp)

data SCConn = SCConn
    { socket :: !Udp
    , nextNodeId :: !(TVar Int32)
    , active :: !(TVar (Map Pitch Int32))
    }

defaultPort :: Int
defaultPort = 57110

{- | Open a UDP socket to @scsynth@ on @127.0.0.1:port@. The socket itself
does not verify that scsynth is reachable; use 'statusOk' for that.
-}
connect :: Int -> IO SCConn
connect port = do
    sock <- openUdp "127.0.0.1" port
    nidVar <- newTVarIO 1000
    actVar <- newTVarIO Map.empty
    pure SCConn{socket = sock, nextNodeId = nidVar, active = actVar}

{- | Release any active voices and close the UDP socket. Idempotent on the
active map (next call sees an empty map and only re-sends @/g_freeAll@).
-}
disconnect :: SCConn -> IO ()
disconnect conn = do
    freeAll conn
    close conn.socket

{- | Send @/status@ and wait up to 500 ms for the reply. Returns 'False' on
network error or timeout; used by the @--check-sc@ executable flag and as
a one-time boot-time probe in 'Funktor.Live'.
-}
statusOk :: SCConn -> IO Bool
statusOk conn = do
    res <- try $ do
        sendMessage conn.socket (Message "/status" [])
        winner <- race (threadDelay 500000) (recvMessage conn.socket)
        case winner of
            Left _ -> pure False
            Right _ -> pure True
    case res of
        Left (_ :: SomeException) -> pure False
        Right b -> pure b

{- | Trigger a new note. Allocates a node id, records it in the per-pitch
active map (so 'noteOff' can find it), and sends @/s_new@. If the pitch
is already sounding, the previous node is freed immediately — voice steal
without a release tail, because the SynthDef's envelope can't cross-fade
between two instances of itself.
-}
noteOn :: SCConn -> Pitch -> Velocity -> Timbre -> IO ()
noteOn conn p v timbre = do
    mprev <- atomically $ Map.lookup p <$> readTVar conn.active
    mapM_ (freeNode conn) mprev
    nid <- allocNodeId conn
    atomically $ modifyTVar' conn.active (Map.insert p nid)
    let freq = realToFrac (midiToFreq p) :: Float
        amp = realToFrac (velocityToAmplitude v) :: Float
        header =
            [ AsciiString (ascii timbre.synthDef)
            , Int32 nid
            , Int32 0
            , Int32 0
            ]
        builtIns =
            [ AsciiString (ascii "freq")
            , Float freq
            , AsciiString (ascii "amp")
            , Float amp
            , AsciiString (ascii "gate")
            , Float 1.0
            ]
        extras = concatMap mkPair timbre.params
    sendMessage conn.socket (Message "/s_new" (header ++ builtIns ++ extras))
  where
    mkPair (k, x) = [AsciiString (ascii k), Float x]

{- | Release the most recent note-on for this pitch by setting its @gate@ arg
to 0. The SynthDef's 'EnvGen' (with @doneAction: 2@) runs its release segment
and frees the node — no Haskell-side cleanup needed.
-}
noteOff :: SCConn -> Pitch -> IO ()
noteOff conn p = do
    mnid <- atomically $ do
        m <- readTVar conn.active
        modifyTVar' conn.active (Map.delete p)
        pure (Map.lookup p m)
    case mnid of
        Just nid ->
            sendMessage
                conn.socket
                (Message "/n_set" [Int32 nid, AsciiString (ascii "gate"), Float 0])
        Nothing -> pure ()

{- | Send @gate=0@ to every active voice and forget them. Each voice still
runs its release tail in scsynth; this is the polite way to stop.
-}
releaseAll :: SCConn -> IO ()
releaseAll conn = do
    ids <- atomically $ do
        m <- readTVar conn.active
        modifyTVar' conn.active (const Map.empty)
        pure (Map.elems m)
    mapM_ release ids
  where
    release nid =
        sendMessage
            conn.socket
            (Message "/n_set" [Int32 nid, AsciiString (ascii "gate"), Float 0])

{- | Free every synth on the default group. The audible-cliff stop —
use 'releaseAll' to let envelopes finish.
-}
freeAll :: SCConn -> IO ()
freeAll conn = do
    atomically $ modifyTVar' conn.active (const Map.empty)
    sendMessage conn.socket (Message "/g_freeAll" [Int32 0])

allocNodeId :: SCConn -> IO Int32
allocNodeId conn = atomically $ stateTVar conn.nextNodeId $ \n -> (n, n + 1)

freeNode :: SCConn -> Int32 -> IO ()
freeNode conn nid = sendMessage conn.socket (Message "/n_free" [Int32 nid])
