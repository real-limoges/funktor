module Test.Utils.Arbitrary where

import Test.QuickCheck (Arbitrary(..), Positive(..), NonNegative(..))
import Funktor.Core.Types
import Funktor.Core.Pattern
import Data.Maybe (fromJust)
import qualified Data.Vector as V

-- Beat is a newtype over Rational; keep it positive for most uses
instance Arbitrary Beat where
  arbitrary = Beat . getPositive <$> (arbitrary :: Gen (Positive Rational))

instance Arbitrary Duration where
  arbitrary = Duration . getPositive <$> (arbitrary :: Gen (Positive Rational))

instance Arbitrary Tempo where
  arbitrary = Tempo . getPositive <$> (arbitrary :: Gen (Positive Double))

instance Arbitrary Pitch where
  arbitrary = Pitch . getNonNegative <$> (arbitrary :: Gen (NonNegative Int))

instance Arbitrary Velocity where
  arbitrary = Velocity . getPositive <$> (arbitrary :: Gen (Positive Double))

instance (Arbitrary a) => Arbitrary (Event a) where
  arbitrary = Event <$> arbitrary <*> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Pattern a) where
  arbitrary = do
    evs <- arbitrary
    dur <- arbitrary
    pure $ pattern_ dur evs

-- Helper to extract a single voice from a pool with at least one entry
singleVoice :: VoicePool -> Voice
singleVoice pool = fromJust $ V.head $ poolVoices pool
