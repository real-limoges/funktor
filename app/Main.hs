module Main where

import Control.Concurrent (threadDelay)
import Funktor.Audio
import Funktor.Core.Types (Pitch (..), Velocity (..))

main :: IO ()
main = do
    putStrLn "Funktor - Interactive Music Application"
    putStrLn "Use GHCi with ':m Funktor.Live Funktor' to control playback interactively"

    (device, stateVar) <- openDevice
    noteOn stateVar (Pitch 69) (Velocity 1.0)
    threadDelay 1000000
    noteOff stateVar (Pitch 69)
    closeDevice device

    putStrLn "Application ready. Start GHCi with 'cabal repl' to use live interface."
