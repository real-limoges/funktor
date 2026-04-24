{-# LANGUAGE RecordWildCards #-}
module Funktor.Audio.Sine where

import Funktor.Audio.State
import Control.Concurrent.STM (TVar, atomically, readTVarIO, modifyTVar')
import qualified SDL
import qualified Data.Vector.Storable.Mutable as VM

data OscState = OscState
    { oscFreq      :: !Double
    , oscPhase     :: !Double
    , oscAmplitude :: !Double
    } deriving (Show)

data AudioState = AudioState
    { audioOsc :: !OscState
    , audioPlaying :: !Bool
    } deriving (Show)

sineCallback :: TVar AudioState -> SDL.AudioFormat t -> VM.IOVector t -> IO ()
sineCallback stateVar SDL.FloatingLEAudio buf = do
    st <- readTVarIO stateVar
    let OscState{..} = audioOsc st
        len = VM.length buf
        phaseInc = oscFreq / sampleRate
        (finalPhase, _) =
            foldl
                ( \ (ph, i) _ ->
                    let ph'
                            | ph + phaseInc >= 1.0 = ph + phaseInc - 1.0
                            | otherwise = ph + phaseInc
                     in (ph', (i :: Int) + 1)
                )
                (oscPhase, (0 :: Int))
                [0 .. len - 1 :: Int]
    atomically $ modifyTVar' stateVar $ \s ->
        s { audioOsc = (audioOsc s) { oscPhase = finalPhase } }

sineCallback _ _ _ = pure ()

createAudioState :: Double -> Double -> AudioState
createAudioState freq amp =
    AudioState
        { audioOsc = OscState oscFreq oscPhase oscAmplitude
        , audioPlaying = False
        }
  where
    oscFreq = freq
    oscPhase = 0.0
    oscAmplitude = amp
