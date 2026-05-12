module LiveSpec (tests) where

import Funktor.Core.Pattern (singleton)
import Funktor.Core.Stream (fromPattern)
import Funktor.Core.Types (Duration (..), Note (..), Pitch (..), Velocity (..))
import Funktor.Live ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Live Tests"
        [ testCase "Pattern/Stream pipeline compiles" $ do
            let note = Note (Pitch 60) (Duration 1) (Velocity 100)
                _ = fromPattern (singleton (Duration 1) note)
            assertBool "Compiles" True
        ]
