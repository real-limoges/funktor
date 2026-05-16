module Funktor.Audio.Sine (sineCallback)
where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO)
import Control.Monad (forM_)
import Data.Fixed (mod')
import Data.Vector qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Funktor.Audio.Envelope (envelopeAmplitude)
import Funktor.Audio.State
import Funktor.Audio.Voice
import Funktor.Core.Types (velocityToAmplitude)
import SDL qualified

sineCallback :: TVar AudioState -> SDL.AudioFormat t -> VM.IOVector t -> IO ()
sineCallback stateVar SDL.FloatingLEAudio buf = do
    st <- readTVarIO stateVar
    let p = st.pool
        len = VM.length buf
        rate = sampleRate
        baseT = st.time
    forM_ [0 .. len - 1] $ \i -> do
        let t = baseT + fromIntegral i / rate
            total =
                V.foldl'
                    ( \acc maybeV ->
                        case maybeV of
                            Nothing -> acc
                            Just v ->
                                let env = envelopeAmplitude st.envelope v.noteOnAt v.noteOffAt t
                                    amp = velocityToAmplitude v.velocity
                                    sample = amp * env * sin (2 * pi * v.phase)
                                 in acc + sample
                    )
                    (0.0 :: Double)
                    p.voices
            out = realToFrac (total / fromIntegral maxVoices) :: Float
        VM.write buf i out
    let advance :: Voice -> Voice
        advance v = v{phase = (v.phase + (v.freq / rate) * fromIntegral len) `mod'` 1.0}
        newVoices = V.map (fmap advance) p.voices
    atomically $ modifyTVar' stateVar $ \s ->
        s
            { pool = s.pool{voices = newVoices}
            , time = s.time + fromIntegral len / rate
            }
    atomically $ modifyTVar' stateVar $ \s ->
        s{pool = cleanupVoices s.envelope s.time s.pool}
sineCallback _ _ _ = pure ()
