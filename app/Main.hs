module Main where

import Control.Concurrent (threadDelay)
import Funktor.Audio
import Funktor.Audio.SC qualified as SC
import Funktor.Audio.Timbre (defaultTimbre)
import Funktor.Core.Types (Pitch (..), Velocity (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--check-sc"] -> checkScsynth
        _ -> playDemo

checkScsynth :: IO ()
checkScsynth = do
    sc <- openDevice
    ok <- SC.statusOk sc
    closeDevice sc
    if ok
        then putStrLn "scsynth: OK (responded on 127.0.0.1:57110)"
        else do
            putStrLn $
                "scsynth: no response on 127.0.0.1:57110."
                    ++ " Boot SuperCollider and evaluate synthdefs/funktor.scd first."
            exitFailure

playDemo :: IO ()
playDemo = do
    putStrLn "Funktor - Interactive Music Application"
    putStrLn "Use GHCi with ':m Funktor.Live Funktor' to control playback interactively"

    sc <- openDevice
    noteOn sc (Pitch 69) (Velocity 1.0) defaultTimbre
    threadDelay 1000000
    noteOff sc (Pitch 69)
    -- give the release tail time to finish before tearing the connection down
    threadDelay 500000
    closeDevice sc

    putStrLn "Application ready. Start GHCi with 'cabal repl' to use live interface."
