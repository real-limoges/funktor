module Funktor.Audio
    ( openDevice
    , closeDevice
    , noteOn
    , noteOff
    ) where

import qualified SDL
import qualified Data.Vector.Storable.Mutable as VM
import Control.Monad (forM_)
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16)
import Funktor.Core.Types (Pitch, Velocity)
import Funktor.Audio.State (bufferSize)

openDevice :: IO (SDL.AudioDevice, TVar ())
openDevice = do
    SDL.initialize [SDL.InitAudio]
    (dev, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
        { SDL.openDeviceFreq     = SDL.Mandate 44100
        , SDL.openDeviceFormat   = SDL.Mandate SDL.FloatingLEAudio
        , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
        , SDL.openDeviceSamples  = 512
        , SDL.openDeviceUsage    = SDL.ForPlayback
        , SDL.openDeviceName     = Nothing
        , SDL.openDeviceCallback = audioCallback
        }
    stateVar <- newTVarIO ()
    SDL.setAudioDevicePlaybackState dev SDL.Play
    pure (dev, stateVar)

closeDevice :: SDL.AudioDevice -> IO ()
closeDevice = SDL.closeAudioDevice

noteOn :: TVar () -> Pitch -> Velocity -> IO ()
noteOn _ _ _ = pure ()

noteOff :: TVar () -> Pitch -> IO ()
noteOff _ _ = pure ()

audioCallback :: SDL.AudioFormat t -> VM.IOVector t -> IO ()
audioCallback fmt buf = do
    let n = bufferSize
    case fmt of
        SDL.FloatingLEAudio        -> fillFloat buf n
        SDL.FloatingBEAudio        -> fillFloat buf n
        SDL.FloatingNativeAudio    -> fillFloat buf n
        SDL.Signed8BitAudio        -> fillInt8 buf n
        SDL.Unsigned8BitAudio      -> fillUint8 buf n
        SDL.Signed16BitLEAudio     -> fillInt16 buf n
        SDL.Signed16BitBEAudio     -> fillInt16 buf n
        SDL.Signed16BitNativeAudio -> fillInt16 buf n
        SDL.Unsigned16BitLEAudio   -> fillUint16 buf n
        SDL.Unsigned16BitBEAudio   -> fillUint16 buf n
        SDL.Unsigned16BitNativeAudio -> fillUint16 buf n
        SDL.Signed32BitLEAudio     -> fillInt32 buf n
        SDL.Signed32BitBEAudio     -> fillInt32 buf n
        SDL.Signed32BitNativeAudio -> fillInt32 buf n

fillFloat :: VM.IOVector Float -> Int -> IO ()
fillFloat buf n = forM_ [0 .. n-1] $ \i -> VM.write buf i 0.0

fillInt8 :: VM.IOVector Int8 -> Int -> IO ()
fillInt8 buf n = forM_ [0 .. n-1] $ \i -> VM.write buf i 0

fillUint8 :: VM.IOVector Word8 -> Int -> IO ()
fillUint8 buf n = forM_ [0 .. n-1] $ \i -> VM.write buf i 0

fillInt16 :: VM.IOVector Int16 -> Int -> IO ()
fillInt16 buf n = forM_ [0 .. n-1] $ \i -> VM.write buf i 0

fillUint16 :: VM.IOVector Word16 -> Int -> IO ()
fillUint16 buf n = forM_ [0 .. n-1] $ \i -> VM.write buf i 0

fillInt32 :: VM.IOVector Int32 -> Int -> IO ()
fillInt32 buf n = forM_ [0 .. n-1] $ \i -> VM.write buf i 0
