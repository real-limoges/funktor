module GridSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Funktor.Grid
import qualified Data.Vector as V

tests :: TestTree
tests = testGroup "Grid"
  [ testCase "setPad out‑of‑bounds does not alter grid" $ do
        let g = emptyGrid
            g' = setPad g (Pad (-1) (-1)) (Color 255 0 0)
        assertBool "grid unchanged" (g == g')

  , testCase "setPad stores colour correctly" $ do
        let g = emptyGrid
            p = Pad 0 0
            c = Color 10 20 30
            g' = setPad g p c
            Just c' = getPad g' p
        assertBool "colour matches" (c == c')
  ]
