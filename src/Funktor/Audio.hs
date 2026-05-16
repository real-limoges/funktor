{- | Convenience facade over "Funktor.Audio.SC". Holds the old @openDevice@ /
@noteOn@ / @noteOff@ names so 'Funktor.Live' and ad-hoc REPL use don't have
to think about whether the audio engine is local or remote.
-}
module Funktor.Audio (
    openDevice,
    closeDevice,
    noteOn,
    noteOff,
    SCConn,
) where

import Funktor.Audio.SC (SCConn)
import Funktor.Audio.SC qualified as SC
import Funktor.Audio.Timbre (Timbre)
import Funktor.Core.Types (Pitch, Velocity)

-- | Connect to @scsynth@ on localhost:57110.
openDevice :: IO SCConn
openDevice = SC.connect SC.defaultPort

closeDevice :: SCConn -> IO ()
closeDevice = SC.disconnect

noteOn :: SCConn -> Pitch -> Velocity -> Timbre -> IO ()
noteOn = SC.noteOn

noteOff :: SCConn -> Pitch -> IO ()
noteOff = SC.noteOff
