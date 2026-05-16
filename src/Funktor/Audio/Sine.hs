module Funktor.Audio.Sine (sineCallback)
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO)
import Control.Monad (forM_)
import Data.Fixed (mod')
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Vector qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Funktor.Audio.Effects (lowPassCoeff)
import Funktor.Audio.Envelope (envelopeAmplitude)
import Funktor.Audio.Oscillator (oscillate)
import Funktor.Audio.State
import Funktor.Audio.Voice
import Funktor.Core.Types (velocityToAmplitude)
import SDL qualified

sineCallback :: TVar AudioState -> SDL.AudioFormat t -> VM.IOVector t -> IO ()
sineCallback stateVar SDL.FloatingLEAudio buf = do
    st <- readTVarIO stateVar
    let len = VM.length buf
        rate = sampleRate
        baseT = st.time
    -- Per-voice low-pass state, mutated as we walk the buffer so the filter
    -- carries across samples. Updates are flushed back into the pool at the end.
    lpRefs <-
        V.mapM
            (\mv -> newIORef (maybe 0 (.lowPassPrev) mv))
            st.pool.voices
    forM_ [0 .. len - 1] $ \i -> do
        let t = baseT + fromIntegral i / rate
        total <-
            V.ifoldM'
                ( \acc idx maybeV ->
                    case maybeV of
                        Nothing -> pure acc
                        Just v -> do
                            let env = envelopeAmplitude v.envelope v.noteOnAt v.noteOffAt t
                                amp = velocityToAmplitude v.velocity
                                dt = v.freq / rate
                                osc = oscillate v.waveform v.phase dt
                                coeff = lowPassCoeff v.cutoffHz
                            prev <- readIORef (lpRefs V.! idx)
                            let raw = amp * env * osc
                                filtered = prev + coeff * (raw - prev)
                            modifyIORef' (lpRefs V.! idx) (const filtered)
                            pure (acc + filtered)
                )
                (0.0 :: Double)
                st.pool.voices
        VM.write buf i (realToFrac (total / fromIntegral maxVoices))
    -- Snapshot the per-voice LP states back into the pool, advance phase + time.
    newLpValues <- V.mapM readIORef lpRefs
    let advance idx v =
            v
                { phase = (v.phase + (v.freq / rate) * fromIntegral len) `mod'` 1.0
                , lowPassPrev = newLpValues V.! idx
                }
        newVoices = V.imap (\i mv -> fmap (advance i) mv) st.pool.voices
    atomically $ modifyTVar' stateVar $ \s ->
        s
            { pool = s.pool{voices = newVoices}
            , time = s.time + fromIntegral len / rate
            }
    atomically $ modifyTVar' stateVar $ \s ->
        s{pool = cleanupVoices s.time s.pool}
sineCallback _ _ _ = pure ()
