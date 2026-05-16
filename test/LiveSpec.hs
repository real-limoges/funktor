module LiveSpec (tests) where

import Funktor.Core.Stream (singleton)
import Funktor.Core.Types (Duration (..), Note (..), Pitch (..), Velocity (..))
import Funktor.Live ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Live Tests"
        [ testCase "Stream pipeline compiles" $ do
            let _ = singleton (Duration 1) (Note (Pitch 60) (Velocity 100))
            assertBool "Compiles" True
        ]
