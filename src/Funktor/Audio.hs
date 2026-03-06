module Funktor.Audio
    ( openDevice
    ) where

import qualified SDL
import qualified Data.Vector.Storable.Mutable as VM

-- | Open the default audio device configured for 44100 Hz mono float playback.
-- SDL fills each audio buffer by calling 'silenceCallback'.
openDevice :: IO SDL.AudioDevice
openDevice = do
    SDL.initialize [SDL.InitAudio]
    (dev, _) <- SDL.openAudioDevice SDL.OpenDeviceSpec
        { SDL.openDeviceFreq     = SDL.Mandate 44100
        , SDL.openDeviceFormat   = SDL.Mandate SDL.FloatingLEAudio
        , SDL.openDeviceChannels = SDL.Mandate SDL.Mono
        , SDL.openDeviceSamples  = 512
        , SDL.openDeviceUsage    = SDL.ForPlayback
        , SDL.openDeviceName     = Nothing
        , SDL.openDeviceCallback = silenceCallback
        }
    SDL.setAudioDevicePlaybackState dev SDL.Play
    pure dev

-- | Fill an audio buffer with zeros regardless of sample format.
-- Pattern-matches on the GADT 'SDL.AudioFormat' to learn the concrete
-- sample type 't' for each branch, which lets 'VM.set buf 0' type-check.
silenceCallback :: SDL.AudioFormat t -> VM.IOVector t -> IO ()
silenceCallback SDL.FloatingLEAudio        buf = VM.set buf 0
silenceCallback SDL.FloatingBEAudio        buf = VM.set buf 0
silenceCallback SDL.FloatingNativeAudio    buf = VM.set buf 0
silenceCallback SDL.Signed8BitAudio        buf = VM.set buf 0
silenceCallback SDL.Unsigned8BitAudio      buf = VM.set buf 0
silenceCallback SDL.Signed16BitLEAudio     buf = VM.set buf 0
silenceCallback SDL.Signed16BitBEAudio     buf = VM.set buf 0
silenceCallback SDL.Signed16BitNativeAudio buf = VM.set buf 0
silenceCallback SDL.Unsigned16BitLEAudio   buf = VM.set buf 0
silenceCallback SDL.Unsigned16BitBEAudio   buf = VM.set buf 0
silenceCallback SDL.Unsigned16BitNativeAudio buf = VM.set buf 0
silenceCallback SDL.Signed32BitLEAudio     buf = VM.set buf 0
silenceCallback SDL.Signed32BitBEAudio     buf = VM.set buf 0
silenceCallback SDL.Signed32BitNativeAudio buf = VM.set buf 0