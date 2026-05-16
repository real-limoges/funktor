{- | Live-reload plumbing for a long-running GHCi session. Two responsibilities:

  * 'persistAt' — keep a value alive across a GHCi @:reload@ via the
    @foreign-store@ RTS-side cache. Used at exactly one site in
    "Funktor.Live" so the running audio session survives recompilation.
  * 'startWatcher' / 'stopWatcher' — a background @fsnotify@ thread that
    prints a notification when a Haskell source file is saved, prompting
    the user to issue @:reload@ themselves.

Auto re-eval via the GHC API is intentionally out of scope; this module is
the minimal sidekick to the existing 'Funktor.Audio.Scheduler.hotSwap'.
-}
module Funktor.Live.Reload (
    persistAt,
    startWatcher,
    stopWatcher,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Monad (forever)
import Data.List (isSuffixOf)
import Foreign.Store (Store (..), lookupStore, readStore, writeStore)
import System.FSNotify (Event, eventPath, watchTree, withManager)

{- | Cache an 'IO' action's result in a numbered RTS-side slot keyed by 'Int'.
First call evaluates the action and stores the result; subsequent calls
read the cached value, skipping re-evaluation.

The store survives @:reload@ in GHCi, so wrapping a top-level
'unsafePerformIO' definition in 'persistAt' keeps it alive across module
recompilation — the standard live-coding trick.
-}
persistAt :: Int -> IO a -> IO a
persistAt n action = do
    m <- lookupStore (fromIntegral n)
    case m of
        Just s -> readStore s
        Nothing -> do
            v <- action
            writeStore (Store (fromIntegral n)) v
            pure v

{- | Spawn a background thread that watches @path@ recursively for @.hs@
file events and prints a notification per event. The thread runs until
'stopWatcher' is called.
-}
startWatcher :: FilePath -> IO (Async ())
startWatcher path = async $ withManager $ \mgr -> do
    _ <- watchTree mgr path isHaskellSource handleEvent
    forever (threadDelay 1000000)
  where
    isHaskellSource :: Event -> Bool
    isHaskellSource ev = ".hs" `isSuffixOf` eventPath ev

    handleEvent :: Event -> IO ()
    handleEvent ev =
        putStrLn $
            "[Funktor.Reload] "
                ++ eventPath ev
                ++ " — :reload then re-issue play to pick up changes"

-- | Cancel a watcher started by 'startWatcher'.
stopWatcher :: Async () -> IO ()
stopWatcher = cancel
