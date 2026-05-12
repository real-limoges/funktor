module Funktor.Audio (
    openDevice,
    closeDevice,
    noteOn,
    noteOff,
    sineCallback,

    -- * Types
    AudioState (..),
    OscState (..),
    createSineAudioState,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Vector.Storable.Mutable qualified as VM
import Funktor.Audio.State
import Funktor.Audio.Voice (poolNoteOff, poolNoteOn)
import Funktor.Core.Types (Pitch, Velocity)
import SDL qualified

sampleRateHz :: Double
sampleRateHz = 44100

openDevice :: IO (SDL.AudioDevice, TVar AudioState)
openDevice = do
    SDL.initialize [SDL.InitAudio]
    stateVar <- newTVarIO $ createSineAudioState 261.63 0.5
    (dev, _) <-
        SDL.openAudioDevice
            SDL.OpenDeviceSpec
                { SDL.openDeviceFreq = SDL.Mandate 44100
                , SDL.openDeviceFormat = SDL.Mandate SDL.FloatingLEAudio
                , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
                , SDL.openDeviceSamples = 512
                , SDL.openDeviceUsage = SDL.ForPlayback
                , SDL.openDeviceName = Nothing
                , SDL.openDeviceCallback = sineCallback stateVar
                }
    SDL.setAudioDevicePlaybackState dev SDL.Play
    pure (dev, stateVar)

sineCallback :: TVar AudioState -> SDL.AudioFormat t -> VM.IOVector t -> IO ()
sineCallback stateVar fmt buf = case fmt of
    SDL.FloatingLEAudio -> do
        st <- readTVarIO stateVar
        let osc = audioOsc st
            len = VM.length buf
            phaseInc = oscFreq osc / sampleRateHz
            sampleAt i = realToFrac $ oscAmplitude osc * sin (2 * pi * (oscPhase osc + fromIntegral i * phaseInc))
            finalPhase = wrapPhase (oscPhase osc + fromIntegral len * phaseInc)
            timeAdvance = fromIntegral len / sampleRateHz
        mapM_ (\i -> VM.write buf i (sampleAt i)) [0 .. len - 1]
        atomically $ modifyTVar' stateVar $ \s ->
            s
                { audioOsc = (audioOsc s){oscPhase = finalPhase}
                , audioTime = audioTime s + timeAdvance
                }
    _ -> pure ()

wrapPhase :: Double -> Double
wrapPhase p = p - fromIntegral (floor p :: Int)

closeDevice :: SDL.AudioDevice -> IO ()
closeDevice = SDL.closeAudioDevice

noteOn :: TVar AudioState -> Pitch -> Velocity -> IO ()
noteOn stateVar pitch vel = atomically $ modifyTVar' stateVar $ \s ->
    s{audioPool = poolNoteOn (audioTime s) pitch vel (audioPool s)}

noteOff :: TVar AudioState -> Pitch -> IO ()
noteOff stateVar pitch = atomically $ modifyTVar' stateVar $ \s ->
    s{audioPool = poolNoteOff (audioTime s) pitch (audioPool s)}
