{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.HUnit
import Data.Maybe (isJust)
import qualified Data.Vector as V
import Funktor.Core.Types (Pitch(Pitch), Velocity(Velocity), Duration(Duration), Beat(Beat), Event(Event), Note(Note))
import Funktor.Core.Stream (fromPattern, fromList, runStream)
import Funktor.Core.Pattern (pentatonic)
import Funktor.Audio.Envelope (EnvelopeParams (..), defaultEnvelope, envelopeAmplitude)
import Funktor.Audio.Oscillator (Waveform (..), oscillate, nextPhase)
import Funktor.Audio.Voice (Voice (..), VoicePool (..), maxVoices, emptyPool, isVoiceDone, poolNoteOn, poolNoteOff, findSlot, cleanupVoices)

main :: IO ()
main = runTestTT tests >> return ()

tests :: Test
tests = "audio modules" ~: test [
    "envelope" ~: testEnvelope
  , "oscillator" ~: testOscillator
  , "voice" ~: testVoice
  , "stream" ~: testStream
  ]

-- Envelope Tests
testEnvelope :: Test
testEnvelope = "envelope" ~: test [
    "amplitude at noteOn should be 0" ~: assertEqual "" 0 (envelopeAmplitude defaultEnvelope 0 Nothing 0)
  , "amplitude during attack should increase" ~: do
      let amp = envelopeAmplitude defaultEnvelope 0 Nothing 0.005
      assertBool "should be between 0 and 1" (amp >= 0 && amp <= 1)
  , "amplitude at end of attack should be ~1" ~: do
      let amp = envelopeAmplitude defaultEnvelope 0 Nothing 0.01
      assertBool "should be approximately 1" (abs (amp - 1.0) < 0.001)
  , "amplitude during sustain should equal sustain level" ~: do
      let amp = envelopeAmplitude defaultEnvelope 0 Nothing 1.0
      assertEqual "" 0.7 amp
  , "amplitude after release should be 0" ~: do
      let amp = envelopeAmplitude defaultEnvelope 0 (Just 1.0) 1.5
      assertEqual "" 0 amp
  , "amplitude during decay should decrease from 1" ~: do
      let amp = envelopeAmplitude defaultEnvelope 0 Nothing 0.05
      assertBool "should be between 1 and sustain level" (amp > 0.7)
  ]

-- Oscillator Tests
testOscillator :: Test
testOscillator = "oscillator" ~: test [
    "nextPhase should wrap at 1.0" ~: do
      let result = nextPhase 0.9 0.2
      assertBool "should wrap to ~0.1" (abs (result - 0.1) < 0.001)
  , "nextPhase should not wrap if < 1.0" ~: do
      let result = nextPhase 0.3 0.2
      assertBool "should be 0.5" (abs (result - 0.5) < 0.001)
  , "sine at phase 0 should be 0" ~: do
      let val = oscillate Sine 0 0
      assertBool "sin(0) = 0" (abs val < 0.001)
  , "sine at phase 0.25 should be ~1" ~: do
      let val = oscillate Sine 0.25 0
      assertBool "sin(pi/2) = 1" (abs (val - 1.0) < 0.001)
  , "sawtooth at phase 0 should be -1" ~: assertEqual "" (-1.0) (oscillate Sawtooth 0 0)
  , "sawtooth at phase 0.5 should be 0" ~: assertEqual "" 0.0 (oscillate Sawtooth 0.5 0)
  , "square at phase 0.25 should be 1" ~: assertEqual "" 1.0 (oscillate Square 0.25 0)
  , "square at phase 0.75 should be -1" ~: assertEqual "" (-1.0) (oscillate Square 0.75 0)
  , "triangle at phase 0 should be -1" ~: assertEqual "" (-1.0) (oscillate Triangle 0 0)
  , "triangle at phase 0.5 should be 1" ~: assertEqual "" 1.0 (oscillate Triangle 0.5 0)
  , "triangle at phase 1.0 should be -1" ~: assertEqual "" (-1.0) (oscillate Triangle 1.0 0)
  ]

-- Voice Tests
testVoice :: Test
testVoice = "voice" ~: test [
    "empty pool should have maxVoices slots" ~: assertEqual "" maxVoices (V.length (poolVoices emptyPool))
  , "findSlot on empty pool should return 0" ~: assertEqual "" 0 (findSlot emptyPool)
  , "poolNoteOn should add voice to first slot" ~: do
      let pool1 = emptyPool
          pool2 = poolNoteOn 0 (Pitch 60) (Velocity 0.5) pool1
      assertBool "first slot should have a voice" (isJust (V.head (poolVoices pool2)))
  , "poolNoteOff should mark voice for release" ~: do
      let noteOnTime = 0
          pool1 = poolNoteOn noteOnTime (Pitch 60) (Velocity 0.5) emptyPool
          pool2 = poolNoteOff (noteOnTime + 0.5) (Pitch 60) pool1
          voice = V.head (poolVoices pool2)
      case voice of
        Just v -> assertBool "voice should have noteOffAt set" (isJust (voiceNoteOffAt v))
        Nothing -> assertBool "voice should exist" False
  , "isVoiceDone should return True after release time" ~: do
      let noteOnTime = 0
          noteOffTime = 1.0
          params = defaultEnvelope
          pool1 = poolNoteOn noteOnTime (Pitch 60) (Velocity 0.5) emptyPool
          pool2 = poolNoteOff noteOffTime (Pitch 60) pool1
          voice = V.head (poolVoices pool2)
      case voice of
        Just v -> assertBool "voice should be done" (isVoiceDone params (noteOffTime + envRelease params + 0.1) v)
        Nothing -> assertBool "voice should exist" False
   , "cleanupVoices should remove done voices" ~: do
       let noteOnTime = 0
           noteOffTime = 1.0
           params = defaultEnvelope
           pool1 = poolNoteOn noteOnTime (Pitch 60) (Velocity 0.5) emptyPool
           pool2 = poolNoteOff noteOffTime (Pitch 60) pool1
           pool3 = cleanupVoices params (noteOffTime + envRelease params + 0.1) pool2
           voice = V.head (poolVoices pool3)
       case voice of
         Nothing -> assertBool "voice should be removed" True
         Just _ -> assertBool "voice should be removed" False
   ]

-- Stream Tests
testStream :: Test
testStream = "stream" ~: test [
    "fromPattern should create loopable stream" ~: do
      let stream = fromPattern (pentatonic 4)
          events = runStream stream 0 10
      assertBool "pentatonic stream should have events" (not (null events))
  , "all events within bounds" ~: do
      let stream = fromPattern (pentatonic 4)
          events = runStream stream 0 5
      assertBool "all events should be within bounds" (all (\(Event t _) -> t >= Beat 0 && t < Beat 5) events)
  , "fromList should create one-shot stream" ~: do
      let note = Note (Pitch 60) (Duration 1) (Velocity 0.5)
          stream = fromList [Event 0 note]
          events = runStream stream 0 2
      assertEqual "" 1 (length events)
  , "silence stream should have no events" ~: do
      let stream = fromList []
          events = runStream stream 0 10
      assertBool "empty stream should have no events" (null events)
  ]
