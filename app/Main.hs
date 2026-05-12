module Main where

import Control.Concurrent (threadDelay)
import Funktor.Audio
import Funktor.Core.Types (Pitch (..), Velocity (..))

main :: IO ()
main = do
    putStrLn "Funktor - Interactive Music Application"
    putStrLn "This is now a live coding ready application"
    putStrLn "Use GHCi with ':m Funktor.Live Funktor' to control playback interactively"

    -- Demonstrate that we can still work with simple direct audio access
    (device, stateVar) <- openDevice

    -- Play a simple tone for a moment so we know it works
    noteOn stateVar (Pitch 69) (Velocity 1.0)
    threadDelay 1000000
    noteOff stateVar (Pitch 69)
    closeDevice device

    putStrLn "Application ready. Start GHCi with 'cabal repl' to use live interface."
