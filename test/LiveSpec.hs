module LiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
-- Assuming Live module only contains pure helpers (add them here if they exist)

tests :: TestTree
tests = testGroup "Live"
  [ testCase "placeholder" $ assertBool "no pure functions to test yet" True
  ]
