module Funktor.Audio (
    openDevice,
    closeDevice,
    noteOn,
    noteOff,

    -- * Types
    AudioState (..),
    createAudioState,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Funktor.Audio.Sine (sineCallback)
import Funktor.Audio.State
import Funktor.Audio.Timbre (Timbre)
import Funktor.Audio.Voice (poolNoteOff, poolNoteOn)
import Funktor.Core.Types (Pitch, Velocity)
import SDL qualified

openDevice :: IO (SDL.AudioDevice, TVar AudioState)
openDevice = do
    SDL.initialize [SDL.InitAudio]
    stateVar <- newTVarIO createAudioState
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

closeDevice :: SDL.AudioDevice -> IO ()
closeDevice = SDL.closeAudioDevice

noteOn :: TVar AudioState -> Pitch -> Velocity -> Timbre -> IO ()
noteOn stateVar p vel t = atomically $ modifyTVar' stateVar $ \s ->
    s{pool = poolNoteOn s.time p vel t s.pool}

noteOff :: TVar AudioState -> Pitch -> IO ()
noteOff stateVar p = atomically $ modifyTVar' stateVar $ \s ->
    s{pool = poolNoteOff s.time p s.pool}
