{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

-- Core

import Core.PatternSpec qualified as CorePattern
import Core.StreamSpec qualified as CoreStream
import Core.TypesSpec qualified as CoreTypes

-- Audio
import Audio.AudioSpec qualified as AudioTop
import Audio.EffectsSpec qualified as AudioEffects
import Audio.EnvelopeSpec qualified as AudioEnvelope
import Audio.OscillatorSpec qualified as AudioOscillator
import Audio.SchedulerSpec qualified as AudioScheduler
import Audio.SineSpec qualified as AudioSine
import Audio.StateSpec qualified as AudioState
import Audio.VoiceSpec qualified as AudioVoice

-- Pure domains
import GridSpec qualified as Grid
import HarmonySpec qualified as Harmony
import LiveSpec qualified as Live

#ifdef MIDI_ENABLED
import HardwareSpec qualified as Hardware
#endif

hardwareTests :: [TestTree]
#ifdef MIDI_ENABLED
hardwareTests = [Hardware.tests]
#else
hardwareTests = []
#endif

main :: IO ()
main =
    defaultMain $
        testGroup "Funktor test suite" $
            [ CoreTypes.tests
            , CorePattern.tests
            , CoreStream.tests
            , AudioEnvelope.tests
            , AudioOscillator.tests
            , AudioState.tests
            , AudioVoice.tests
            , AudioSine.tests
            , AudioEffects.tests
            , AudioScheduler.tests
            , AudioTop.tests
            , Grid.tests
            , Harmony.tests
            , Live.tests
            ]
                <> hardwareTests
