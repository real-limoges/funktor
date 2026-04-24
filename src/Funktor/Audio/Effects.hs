module Funktor.Audio.Effects
    ( LowPassState (..)
    , DelayState (..)
    , ReverbState (..)
    , EffectsState (..)
    , lowPassCoeff
    , applyLowPass
    , newDelayState
    , applyDelay
    , newReverbState
    , applyEffects
    , processLowPass
    ) where

import qualified Data.Vector.Storable.Mutable as VM
import Control.Monad (foldM)

data LowPassState = LowPassState
    { lpPrevOutput :: !Double
    } deriving (Show)

data DelayState = DelayState
    { delayBuffer   :: !(VM.IOVector Double)
    , delayWritePos :: !Int
    }

data ReverbState = ReverbState
    { reverbCombs    :: ![DelayState]
    , reverbAllpasses :: ![DelayState]
    }

data EffectsState = EffectsState
    { fxLowPass :: !LowPassState
    , fxDelay   :: !DelayState
    , fxReverb  :: !ReverbState
    }

lowPassCoeff :: Double -> Double
lowPassCoeff cutoff = 1.0 - exp (-2.0 * pi * cutoff / 44100.0)

applyLowPass :: Double -> LowPassState -> (Double, LowPassState)
applyLowPass input state =
    let output = lpPrevOutput state + coeff * (input - lpPrevOutput state)
    in (output, LowPassState output)
  where
    coeff = lowPassCoeff 2000

processLowPass :: LowPassState -> VM.IOVector Float -> IO LowPassState
processLowPass st buf = foldM step st [0 .. VM.length buf - 1]
  where
    step st' i = do
        sample <- realToFrac <$> VM.read buf i
        let (out, st'') = applyLowPass sample st'
        VM.write buf i (realToFrac out)
        pure st''

newDelayState :: Int -> IO DelayState
newDelayState size = do
    buffer <- VM.new size
    VM.set buffer 0
    return DelayState
        { delayBuffer = buffer
        , delayWritePos = 0
        }

applyDelay :: Double -> Double -> DelayState -> Double -> IO (Double, DelayState)
applyDelay feedback mix st input = do
    let bufLen = VM.length (delayBuffer st)
        writePos = delayWritePos st
        readPos = (writePos - bufLen + 1) `mod` bufLen
    delayed <- VM.read (delayBuffer st) readPos
    let output = (1.0 - mix) * input + mix * delayed
        writeVal = input + feedback * delayed
    VM.write (delayBuffer st) writePos writeVal
    pure (output, st { delayWritePos = (writePos + 1) `mod` bufLen })

newReverbState :: IO ReverbState
newReverbState = do
    comb1 <- newDelayState 1557
    comb2 <- newDelayState 1617
    comb3 <- newDelayState 1491
    comb4 <- newDelayState 1422
    allpass1 <- newDelayState 225
    allpass2 <- newDelayState 556
    return ReverbState
        { reverbCombs = [comb1, comb2, comb3, comb4]
        , reverbAllpasses = [allpass1, allpass2]
        }

applyEffects :: EffectsState -> VM.IOVector Float -> IO EffectsState
applyEffects effects buf = do
    fx1 <- processLowPass (fxLowPass effects) buf
    pure effects { fxLowPass = fx1 }
