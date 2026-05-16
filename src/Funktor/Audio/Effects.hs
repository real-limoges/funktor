{- | Audio effects.

@applyEffects@ runs low-pass → delay → Schroeder-style reverb (four comb
filters in parallel, two allpasses in series) over a mutable float buffer.
The hardcoded sample rate, cutoff, feedback, and wet-mix values are
serviceable defaults; making them per-voice parameters is Tier 6c work.
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
    applyComb,
    applyAllpass,
    newReverbState,
    newEffectsState,
    applyEffects,
    processLowPass,
    processDelay,
    processReverb,
) where

import Control.Monad (foldM)
import Data.Vector.Storable.Mutable qualified as VM

defaultCutoffHz :: Double
defaultCutoffHz = 2000

defaultSampleRate :: Double
defaultSampleRate = 44100

-- Freeverb-ish constants. Tame enough that the wet path does not blow up.
combFeedback :: Double
combFeedback = 0.84

allpassFeedback :: Double
allpassFeedback = 0.5

reverbWet :: Double
reverbWet = 0.2

delayFeedback :: Double
delayFeedback = 0.4

delayMix :: Double
delayMix = 0.3

delayBufferSamples :: Int
delayBufferSamples = 8192

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

processDelay :: DelayState -> VM.IOVector Float -> IO DelayState
processDelay st buf = foldM step st [0 .. VM.length buf - 1]
  where
    step st' i = do
        sample <- realToFrac <$> VM.read buf i
        (out, st'') <- applyDelay delayFeedback delayMix st' sample
        VM.write buf i (realToFrac out)
        pure st''

-- | Schroeder comb: emits the delayed sample and writes back @input + fb * delayed@.
applyComb :: Double -> DelayState -> Double -> IO (Double, DelayState)
applyComb feedback st input = do
    let bufLen = VM.length st.buffer
        wp = st.writePos
        readPos = (wp + 1) `mod` bufLen
    delayed <- VM.read st.buffer readPos
    VM.write st.buffer wp (input + feedback * delayed)
    pure (delayed, st{writePos = (wp + 1) `mod` bufLen})

-- | Schroeder allpass section.
applyAllpass :: Double -> DelayState -> Double -> IO (Double, DelayState)
applyAllpass feedback st input = do
    let bufLen = VM.length st.buffer
        wp = st.writePos
        readPos = (wp + 1) `mod` bufLen
    delayed <- VM.read st.buffer readPos
    let writeVal = input + feedback * delayed
        output = delayed - feedback * writeVal
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

processReverb :: ReverbState -> VM.IOVector Float -> IO ReverbState
processReverb st buf = do
    (cs', aps') <- foldM step (st.combs, st.allpasses) [0 .. VM.length buf - 1]
    pure (ReverbState cs' aps')
  where
    step (cs, aps) i = do
        dry <- realToFrac <$> VM.read buf i :: IO Double
        (combOuts, cs') <- runCombs cs dry
        let combSum =
                if null combOuts
                    then 0
                    else sum combOuts / fromIntegral (length combOuts)
        (apOut, aps') <- runAllpasses aps combSum
        let mixed = (1 - reverbWet) * dry + reverbWet * apOut
        VM.write buf i (realToFrac mixed)
        pure (cs', aps')

    runCombs cs input = do
        results <- mapM (\c -> applyComb combFeedback c input) cs
        pure (map fst results, map snd results)

    runAllpasses [] x = pure (x, [])
    runAllpasses (a : as) x = do
        (y, a') <- applyAllpass allpassFeedback a x
        (out, as') <- runAllpasses as y
        pure (out, a' : as')

newEffectsState :: IO EffectsState
newEffectsState = do
    d <- newDelayState delayBufferSamples
    r <- newReverbState
    pure
        EffectsState
            { lowPass = LowPassState 0
            , delay = d
            , reverb = r
            }

applyEffects :: EffectsState -> VM.IOVector Float -> IO EffectsState
applyEffects effects buf = do
    fx1 <- processLowPass effects.lowPass buf
    fx2 <- processDelay effects.delay buf
    fx3 <- processReverb effects.reverb buf
    pure effects{lowPass = fx1, delay = fx2, reverb = fx3}
