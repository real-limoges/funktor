{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Utils.Arbitrary () where

import Funktor.Audio.Timbre (Waveform)
import Funktor.Core.Stream (Stream, fromList)
import Funktor.Core.Types
import Test.QuickCheck (Arbitrary (..), Gen, NonNegative (..), Positive (..), choose, elements, sized, vectorOf)

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

{- | Generates arcs with @end > start@ so 'arcLength' stays positive.
Useful for property tests that assume non-degenerate intervals.
-}
instance Arbitrary Arc where
    arbitrary = do
        s <- arbitrary
        Positive len <- arbitrary :: Gen (Positive Rational)
        pure (Arc s (s + Beat len))

instance (Arbitrary a) => Arbitrary (Event a) where
    arbitrary = do
        a <- arbitrary
        event a <$> arbitrary

instance Arbitrary Waveform where
    arbitrary = pure minBound

instance (Arbitrary a) => Arbitrary (Stream a) where
    arbitrary = sized $ \n -> do
        k <- choose (0, n)
        evs <- vectorOf k arbitrary
        pure (fromList evs)

instance Arbitrary ChordQuality where
    arbitrary = elements [minBound .. maxBound]
