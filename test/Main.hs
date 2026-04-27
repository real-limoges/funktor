{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Tasty (defaultMain, testGroup, TestTree)

-- Core
import qualified Core.TypesSpec as CoreTypes
import qualified Core.PatternSpec as CorePattern
import qualified Core.StreamSpec as CoreStream

-- Audio
import qualified Audio.EnvelopeSpec as AudioEnvelope
import qualified Audio.OscillatorSpec as AudioOscillator
import qualified Audio.StateSpec as AudioState
import qualified Audio.VoiceSpec as AudioVoice
import qualified Audio.SineSpec as AudioSine

-- Pure domains
import qualified GridSpec as Grid
import qualified HarmonySpec as Harmony
import qualified LiveSpec as Live
import qualified HardwareSpec as Hardware

main :: IO ()
main = defaultMain $ testGroup "Funktor test suite"
  [ CoreTypes.tests
  , CorePattern.tests
  , CoreStream.tests
  , AudioEnvelope.tests
  , AudioOscillator.tests
  , AudioState.tests
  , AudioVoice.tests
  , AudioSine.tests
  , Grid.tests
  , Harmony.tests
  , Live.tests
  , Hardware.tests
  ]
