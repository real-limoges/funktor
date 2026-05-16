module Live.ReloadSpec (tests) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Funktor.Live.Reload (persistAt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

{- | Slot ids reserved for these tests. Slot 0 belongs to Funktor.Live's
globalLive; we stay clear of it.
-}
slotA, slotB :: Int
slotA = 1001
slotB = 1002

tests :: TestTree
tests =
    testGroup
        "Live.Reload"
        [ testCase "second persistAt call returns the cached IORef" $ do
            r1 <- persistAt slotA (newIORef (0 :: Int))
            r2 <- persistAt slotA (newIORef (999 :: Int))
            -- Writes to r1 must be visible through r2 (same IORef).
            modifyIORef' r1 (+ 1)
            v <- readIORef r2
            v @?= 1
        , testCase "action body evaluates exactly once" $ do
            counter <- newIORef (0 :: Int)
            let action = do
                    modifyIORef' counter (+ 1)
                    newIORef ()
            _ <- persistAt slotB action
            _ <- persistAt slotB action
            n <- readIORef counter
            n @?= 1
        ]
