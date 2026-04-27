{-# LANGUAGE RecordWildCards #-}
module Funktor.Audio
     ( openDevice
     , closeDevice
     , noteOn
     , noteOff
      -- * Types
      , AudioState(..)
      , OscState(..)
      , createSineAudioState
      ) where

import qualified SDL
import Control.Concurrent.STM (TVar     , atomically, modifyTVar', readTVarIO, newTVarIO)
import Funktor.Core.Types (Pitch(..), Velocity(..), midiToFreq, velocityToAmplitude)
import Funktor.Audio.State
import Funktor.Audio.Sine

openDevice :: IO (SDL.AudioDevice, TVar AudioState)
openDevice = do
    SDL.initialize [SDL.InitAudio]
    stateVar <- newTVarIO $ createSineAudioState 261.63 0.5
    (dev, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
        { SDL.openDeviceFreq       = SDL.Mandate 44100
        , SDL.openDeviceFormat     = SDL.Mandate SDL.FloatingLEAudio
        , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
        , SDL.openDeviceSamples    = 512
        , SDL.openDeviceUsage      = SDL.ForPlayback
        , SDL.openDeviceName       = Nothing
        , SDL.openDeviceCallback = sineCallback stateVar
        }
    SDL.setAudioDevicePlaybackState dev SDL.Play
    pure (dev, stateVar)

closeDevice :: SDL.AudioDevice -> IO ()
closeDevice = SDL.closeAudioDevice

noteOn :: TVar AudioState -> Pitch -> Velocity -> IO ()
noteOn stateVar pitch vel = atomically $ modifyTVar' stateVar $ \s ->
  s { audioPool = poolNoteOn (audioTime s) pitch vel (audioPool s) }

noteOff :: TVar AudioState -> Pitch -> IO ()
noteOff stateVar _pitch = atomically $ modifyTVar' stateVar $ \s ->
  s { audioNoteOffAt = Just (audioTime s)
    , audioPlaying = False
    }


noteOff :: TVar AudioState -> Pitch -> IO ()
noteOff stateVar _pitch = atomically $ modifyTVar' stateVar $ \s ->
  s { audioNoteOffAt = Just (audioTime s)
    , audioPlaying = False
    }

