{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Utils.Arbitrary () where

import Test.QuickCheck (Arbitrary(..), Gen, Positive(..), NonNegative(..), choose, oneof, vectorOf, sized)
import Funktor.Core.Types
import Funktor.Core.Pattern (Pattern, pattern_)
import Funktor.Audio.Envelope (EnvelopeParams(..))
import Funktor.Audio.Oscillator (Waveform)

instance Arbitrary Beat where
  arbitrary = Beat . getPositive <$> (arbitrary :: Gen (Positive Rational))

instance Arbitrary Duration where
  arbitrary = Duration . getPositive <$> (arbitrary :: Gen (Positive Rational))

instance Arbitrary Tempo where
  arbitrary = Tempo . getPositive <$> (arbitrary :: Gen (Positive Double))

instance Arbitrary Pitch where
  arbitrary = do
    NonNegative n <- arbitrary :: Gen (NonNegative Int)
    pure (Pitch (n `mod` 128))

instance Arbitrary Velocity where
  arbitrary = do
    Positive d <- arbitrary :: Gen (Positive Double)
    pure (Velocity (min 1.0 d))

instance (Arbitrary a) => Arbitrary (Event a) where
  arbitrary = Event <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Pattern a) where
  arbitrary = sized $ \n -> do
    k   <- choose (0, n)
    evs <- vectorOf k arbitrary
    dur <- arbitrary
    pure (pattern_ dur evs)

instance Arbitrary EnvelopeParams where
  arbitrary = do
    Positive a <- arbitrary :: Gen (Positive Double)
    Positive d <- arbitrary :: Gen (Positive Double)
    s          <- choose (0.0, 1.0)
    Positive r <- arbitrary :: Gen (Positive Double)
    pure EnvelopeParams
      { envAttack  = a
      , envDecay   = d
      , envSustain = s
      , envRelease = r
      }

instance Arbitrary Waveform where
  arbitrary = oneof [pure minBound]
