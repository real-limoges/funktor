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
        let o = st.osc
            len = VM.length buf
            phaseInc = o.freq / sampleRateHz
            sampleAt i = realToFrac $ o.amplitude * sin (2 * pi * (o.phase + fromIntegral i * phaseInc))
            finalPhase = wrapPhase (o.phase + fromIntegral len * phaseInc)
            timeAdvance = fromIntegral len / sampleRateHz
        mapM_ (\i -> VM.write buf i (sampleAt i)) [0 .. len - 1]
        atomically $ modifyTVar' stateVar $ \s ->
            s
                { osc = s.osc{phase = finalPhase}
                , time = s.time + timeAdvance
                }
    _ -> pure ()

wrapPhase :: Double -> Double
wrapPhase p = p - fromIntegral (floor p :: Int)

closeDevice :: SDL.AudioDevice -> IO ()
closeDevice = SDL.closeAudioDevice

noteOn :: TVar AudioState -> Pitch -> Velocity -> IO ()
noteOn stateVar p vel = atomically $ modifyTVar' stateVar $ \s ->
    s{pool = poolNoteOn s.time p vel s.pool}

noteOff :: TVar AudioState -> Pitch -> IO ()
noteOff stateVar p = atomically $ modifyTVar' stateVar $ \s ->
    s{pool = poolNoteOff s.time p s.pool}
