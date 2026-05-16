module Live.SessionSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.ByteString.Lazy qualified as BL
import Funktor.Core.Types
import Funktor.Live.Session
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

withTempPath :: FilePath -> (FilePath -> IO a) -> IO a
withTempPath name k = do
    tmp <- getTemporaryDirectory
    let path = tmp </> name
    a <- k path
    exists <- doesFileExist path
    if exists then removeFile path else pure ()
    pure a

tests :: TestTree
tests =
    testGroup
        "Live.Session"
        [ testCase "startRecording produces an active session" $ do
            sess <- startRecording
            evs <- stopRecording sess
            length evs @?= 0
        , testCase "recordEvent appends while active" $ do
            sess <- startRecording
            recordEvent sess (PadPress 60)
            threadDelay 1000
            recordEvent sess (PadRelease 60)
            evs <- stopRecording sess
            length evs @?= 2
        , testCase "recordEvent ignored after stop" $ do
            sess <- startRecording
            recordEvent sess (PadPress 60)
            _ <- stopRecording sess
            recordEvent sess (PadPress 61)
            evs <- stopRecording sess
            length evs @?= 1
        , testCase "materializeSession pairs press/release on same pad" $ do
            let evs =
                    [ SessionEvent 0.0 (Beat 0) (PadPress 60)
                    , SessionEvent 1.0 (Beat 0) (PadRelease 60)
                    , SessionEvent 2.0 (Beat 0) (PadPress 62)
                    , SessionEvent 3.5 (Beat 0) (PadRelease 62)
                    ]
                ms = materializeSession evs
            length ms @?= 2
            map mnPitch ms @?= [Pitch 60, Pitch 62]
        , testCase "materializeSession ignores TempoChange / StreamSwap" $ do
            let evs =
                    [ SessionEvent 0.0 (Beat 0) (TempoChange (Tempo 140))
                    , SessionEvent 1.0 (Beat 0) (PadPress 60)
                    , SessionEvent 2.0 (Beat 0) (PadRelease 60)
                    ]
                ms = materializeSession evs
            length ms @?= 1
            mnPitch (head ms) @?= Pitch 60
        , testCase "exportMidi writes a non-empty file with MIDI header" $
            withTempPath "funktor-session.mid" $ \path -> do
                exportMidi
                    path
                    [ MaterializedNote (Beat 0) (Pitch 60) (Duration 1) (Velocity 1)
                    , MaterializedNote (Beat 1) (Pitch 62) (Duration 1) (Velocity 1)
                    ]
                bs <- BL.readFile path
                let header = BL.take 4 bs
                header @?= BL.pack [0x4D, 0x54, 0x68, 0x64] -- "MThd"
                assertBool "non-trivial size" (BL.length bs > 14)
        , testCase "exportMidi handles empty note list" $
            withTempPath "funktor-empty.mid" $ \path -> do
                exportMidi path []
                bs <- BL.readFile path
                BL.take 4 bs @?= BL.pack [0x4D, 0x54, 0x68, 0x64]
        , testCase "replaySession runs without error on empty input" $
            replaySession []
        , testCase "stopRecording is idempotent" $ do
            sess <- startRecording
            replicateM_ 3 (stopRecording sess >>= \_ -> pure ())
            evs <- stopRecording sess
            length evs @?= 0
        ]
