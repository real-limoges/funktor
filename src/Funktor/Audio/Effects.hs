{- | Audio effects.

Only the one-pole low-pass is wired into 'applyEffects' today. 'applyDelay',
'newReverbState', and the surrounding plumbing are kept exported and covered
by tests so the audio polish work in Tier 6 can pick them up directly. The
hardcoded sample rate and cutoff are placeholders pending that pass.
-}
module Funktor.Audio.Effects (
    LowPassState (..),
    DelayState (..),
    ReverbState (..),
    EffectsState (..),
    lowPassCoeff,
    applyLowPass,
    newDelayState,
    applyDelay,
    newReverbState,
    applyEffects,
    processLowPass,
) where

import Control.Monad (foldM)
import Data.Vector.Storable.Mutable qualified as VM

defaultCutoffHz :: Double
defaultCutoffHz = 2000

defaultSampleRate :: Double
defaultSampleRate = 44100

newtype LowPassState = LowPassState
    { prevOutput :: Double
    }
    deriving (Show)

data DelayState = DelayState
    { buffer :: !(VM.IOVector Double)
    , writePos :: !Int
    }

data ReverbState = ReverbState
    { combs :: ![DelayState]
    , allpasses :: ![DelayState]
    }

data EffectsState = EffectsState
    { lowPass :: !LowPassState
    , delay :: !DelayState
    , reverb :: !ReverbState
    }

lowPassCoeff :: Double -> Double
lowPassCoeff cutoff = 1.0 - exp (-(2.0 * pi * cutoff / defaultSampleRate))

applyLowPass :: Double -> LowPassState -> (Double, LowPassState)
applyLowPass input st =
    let output = st.prevOutput + coeff * (input - st.prevOutput)
     in (output, LowPassState output)
  where
    coeff = lowPassCoeff defaultCutoffHz

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
    buf <- VM.new size
    VM.set buf 0
    pure
        DelayState
            { buffer = buf
            , writePos = 0
            }

applyDelay :: Double -> Double -> DelayState -> Double -> IO (Double, DelayState)
applyDelay feedback mix st input = do
    let bufLen = VM.length st.buffer
        wp = st.writePos
        readPos = (wp - bufLen + 1) `mod` bufLen
    delayed <- VM.read st.buffer readPos
    let output = (1.0 - mix) * input + mix * delayed
        writeVal = input + feedback * delayed
    VM.write st.buffer wp writeVal
    pure (output, st{writePos = (wp + 1) `mod` bufLen})

newReverbState :: IO ReverbState
newReverbState = do
    comb1 <- newDelayState 1557
    comb2 <- newDelayState 1617
    comb3 <- newDelayState 1491
    comb4 <- newDelayState 1422
    allpass1 <- newDelayState 225
    allpass2 <- newDelayState 556
    pure
        ReverbState
            { combs = [comb1, comb2, comb3, comb4]
            , allpasses = [allpass1, allpass2]
            }

applyEffects :: EffectsState -> VM.IOVector Float -> IO EffectsState
applyEffects effects buf = do
    fx1 <- processLowPass effects.lowPass buf
    pure effects{lowPass = fx1}
