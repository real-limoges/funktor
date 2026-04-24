module Funktor.Audio.State where

sampleRate :: Double
sampleRate = 44100

bufferSize :: Int
bufferSize = 512

type Phase = Double

type Frequency = Double

type Gain = Double

type Time = Double

type FilterCoeff = Double

type DelayBuffer = ()

type MutableDelayBuffer = ()
