module HardwareSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

-- Placeholder: add pure helper tests for launchpad / midi mapping if any exist.

tests :: TestTree
tests =
    testGroup
        "Hardware"
        [ testCase "placeholder" $ assertBool "no pure hardware helpers to test" True
        ]
