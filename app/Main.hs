module Main where

import Funktor.Audio
import Funktor.Core.Types (Pitch(..), Velocity(..))
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    putStrLn "Funktor - Interactive Music Application"
    putStrLn "Opening audio device and playing a tone..."
    
    (device, stateVar) <- openDevice
    noteOn stateVar (Pitch 69) (Velocity 1.0)
    
    -- Play a tone for 2 seconds (2000000 microseconds)
    threadDelay 2000000
    noteOff stateVar (Pitch 69)
    
    putStrLn "Closing audio device..."
    closeDevice device
    putStrLn "Audio device closed. Exiting."
