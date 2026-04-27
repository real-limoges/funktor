module Core.PatternSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty.QuickCheck (testProperty)

import Funktor.Core.Pattern
import Funktor.Core.Types
import Test.Utils.Arbitrary ()

tests :: TestTree
tests = testGroup "Core.Pattern"
  [ testProperty "append associative" $ \(p :: Pattern Int) (q :: Pattern Int) (r :: Pattern Int) ->
        append (append p q) r == append p (append q r)

  , testProperty "duration of append" $ \(p :: Pattern Int) (q :: Pattern Int) ->
        duration (append p q) == duration p + duration q

  , testProperty "scale composition" $ \s t (p :: Pattern Int) ->
        scale (s * t) p == (scale s . scale t) p

  , testProperty "shift inverse" $ \d (p :: Pattern Int) ->
        shift d (shift (-d) p) == p

  , testProperty "empty is empty" $\
        isEmpty (empty :: Pattern Int) @?= True

  , testProperty "singleton not empty" $ \x dur ->
        not (isEmpty (singleton dur x))

  , testProperty "pattern_ sorts events" $ \dur evs ->
        let pat = pattern_ dur evs
        in all (uncurry (<=)) $ zip (map eventBeat $ patternEvents pat) (tail $ map eventBeat $ patternEvents pat)

  , testProperty "repeat_ identity" $ \p ->
        repeat_ 1 p == p

  , testProperty "repeat_ zero yields empty" $ \p ->
        repeat_ 0 p == empty

  ]
