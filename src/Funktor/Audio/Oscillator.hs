module Funktor.Audio.Oscillator (
    Waveform (..),
    oscillate,
    polyBLEP,
    sawBLEP,
    squareBLEP,
    nextPhase,
) where

data Waveform = Sine | Sawtooth | Square | Triangle
    deriving (Eq, Show, Enum, Bounded)

nextPhase :: Double -> Double -> Double
nextPhase phase increment =
    let p = phase + increment
     in if p >= 1 then p - 1 else p

sawBLEP :: Double -> Double -> Double
sawBLEP phase dt =
    let naive = 2.0 * phase - 1.0
     in naive - polyBLEP phase dt

squareBLEP :: Double -> Double -> Double
squareBLEP phase dt =
    let naive = if phase < 0.5 then 1.0 else -1.0
        phase' = if phase >= 1.0 then phase - 1.0 else phase
     in naive + polyBLEP phase dt - polyBLEP ((phase' + 0.5) - if (phase' + 0.5) >= 1.0 then 1.0 else 0.0) dt

polyBLEP :: Double -> Double -> Double
polyBLEP phase dt
    | dt < 1e-10 = 0.0
    | phase < dt' = let t' = phase / dt' in t' + t' - t' * t' - 1.0
    | phase > 1 - dt' = let t' = (phase - 1.0) / dt' in t' * t' + t' + t' + 1.0
    | otherwise = 0.0
  where
    dt' = min dt 0.5

oscillate :: Waveform -> Double -> Double -> Double
oscillate Sine phase _ = sin (2 * pi * phase)
oscillate Sawtooth phase dt = sawBLEP phase dt
oscillate Square phase dt = squareBLEP phase dt
oscillate Triangle phase _ =
    if phase < 0.5
        then 4.0 * phase - 1.0
        else -4.0 * phase + 3.0
