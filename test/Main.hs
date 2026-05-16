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
import Generative.CellularAutomataSpec qualified as GenerativeCA
import Generative.EuclideanSpec qualified as GenerativeEuclidean
import Generative.MarkovSpec qualified as GenerativeMarkov
import GridBindingSpec qualified as GridBinding
import GridSpec qualified as Grid
import Harmony.AnalysisSpec qualified as HarmonyAnalysis
import Harmony.VoicingSpec qualified as HarmonyVoicing
import HarmonySpec qualified as Harmony
import Live.ReloadSpec qualified as LiveReload
import Live.SessionSpec qualified as LiveSession
import LiveSpec qualified as Live
import UISpec qualified as UI

#ifdef MIDI_ENABLED
import HardwareSpec qualified as Hardware
import LaunchpadSpec qualified as Launchpad
#endif

hardwareTests :: [TestTree]
#ifdef MIDI_ENABLED
hardwareTests = [Hardware.tests, Launchpad.tests]
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
            , GenerativeCA.tests
            , GenerativeEuclidean.tests
            , GenerativeMarkov.tests
            , Grid.tests
            , GridBinding.tests
            , Harmony.tests
            , HarmonyAnalysis.tests
            , HarmonyVoicing.tests
            , Live.tests
            , LiveReload.tests
            , LiveSession.tests
            , UI.tests
            ]
                <> hardwareTests
