module GridSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Funktor.Grid

tests :: TestTree
tests = testGroup "Grid"
  [ testCase "setPad out-of-bounds does not alter grid" $ do
        let g  = emptyGrid 4 4
            g' = setPad (-1) (-1) (Pad NoAction Red) g
        assertBool "grid unchanged" (g == g')

  , testCase "setPad stores pad correctly" $ do
        let g    = emptyGrid 4 4
            pad  = Pad NoAction Cyan
            g'   = setPad 1 2 pad g
        getPad 1 2 g' @?= Just pad

  , testCase "getPad out-of-bounds yields Nothing" $
        getPad 10 10 (emptyGrid 4 4) @?= Nothing
  ]
