{-# LANGUAGE RecordWildCards #-}

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
    let pool = audioPool st
        len = VM.length buf
        rate = sampleRate
        baseT = audioTime st
    -- Fill buffer by summing all active voices per sample
    forM_ [0 .. len - 1] $ \i -> do
        let t = baseT + fromIntegral i / rate
            total =
                V.foldl'
                    ( \acc maybeV ->
                        case maybeV of
                            Nothing -> acc
                            Just v ->
                                let env = envelopeAmplitude (audioEnvelope st) (voiceNoteOnAt v) (voiceNoteOffAt v) t
                                    amp = velocityToAmplitude (voiceVelocity v)
                                    ph = voicePhase v
                                    sample = amp * env * sin (2 * pi * ph)
                                 in acc + sample
                    )
                    (0.0 :: Double)
                    (poolVoices pool)
            out = realToFrac (total / fromIntegral maxVoices) :: Float
        VM.write buf i out
    -- Advance phases for all voices
    let advance v = v{voicePhase = (voicePhase v + (voiceFreq v / rate) * fromIntegral len) `mod'` 1.0}
        newVoices = V.map (fmap advance) (poolVoices pool)
    atomically $ modifyTVar' stateVar $ \s ->
        s
            { audioPool = (audioPool s){poolVoices = newVoices}
            , audioTime = audioTime s + fromIntegral len / rate
            }
    -- Cleanup finished voices each buffer
    atomically $ modifyTVar' stateVar $ \s ->
        s{audioPool = cleanupVoices (audioEnvelope s) (audioTime s) (audioPool s)}
sineCallback _ _ _ = pure ()
