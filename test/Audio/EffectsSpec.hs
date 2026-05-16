module Audio.EffectsSpec (tests) where

import Control.Monad (replicateM_)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VM
import Funktor.Audio.Effects
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

silentBuf :: Int -> IO (VM.IOVector Float)
silentBuf n = VM.replicate n 0

impulseBuf :: Int -> IO (VM.IOVector Float)
impulseBuf n = do
    buf <- VM.replicate n 0
    VM.write buf 0 1
    pure buf

tests :: TestTree
tests =
    testGroup
        "Audio.Effects"
        [ testCase "lowPassCoeff at 0 cutoff yields 0" $
            lowPassCoeff 0 @?= 0
        , testCase "lowPassCoeff is monotonically increasing in cutoff" $
            assertBool "monotone" (lowPassCoeff 1000 < lowPassCoeff 5000)
        , testCase "lowPassCoeff stays in (0,1) for audible cutoff" $
            let c = lowPassCoeff 2000
             in assertBool ("got " ++ show c) (c > 0 && c < 1)
        , testCase "applyLowPass smooths toward the input" $
            let (out, _) = applyLowPass 0.5 (LowPassState 0)
             in assertBool ("got " ++ show out) (out > 0 && out < 0.5)
        , testCase "applyLowPass updates state to the new output" $
            let (out, LowPassState prev) = applyLowPass 0.5 (LowPassState 0)
             in prev @?= out
        , testCase "newDelayState produces an initialised vector" $ do
            st <- newDelayState 256
            VM.length st.buffer @?= 256
            st.writePos @?= 0
        , testCase "applyDelay with feedback=0, mix=0 returns dry input" $ do
            st <- newDelayState 64
            (out, _) <- applyDelay 0 0 st 0.5
            assertBool ("got " ++ show out) (abs (out - 0.5) < 1e-9)
        , testCase "applyDelay advances write pointer" $ do
            st <- newDelayState 64
            (_, st') <- applyDelay 0 0 st 0.5
            st'.writePos @?= 1
        , testCase "applyComb returns the previously-stored delayed sample" $ do
            st <- newDelayState 4
            (out, _) <- applyComb 0.5 st 1.0
            -- Buffer starts zeroed, so first comb output is 0
            out @?= 0
        , testCase "applyAllpass with zeroed buffer returns -feedback*input" $ do
            st <- newDelayState 8
            (out, _) <- applyAllpass 0.5 st 1.0
            assertBool ("got " ++ show out) (abs (out + 0.5) < 1e-9)
        , testCase "applyEffects on silent buffer stays bounded near zero" $ do
            fx <- newEffectsState
            buf <- silentBuf 64
            _ <- applyEffects fx buf
            samples <- VS.freeze buf
            assertBool "silent buffer stays near zero" (VS.all (\x -> abs x < 1e-6) samples)
        , testCase "applyEffects on impulse stays bounded" $ do
            fx <- newEffectsState
            buf <- impulseBuf 256
            _ <- applyEffects fx buf
            samples <- VS.freeze buf
            assertBool "no overflow" (VS.all (\x -> abs x < 4) samples)
        , testCase "processReverb leaves an audible tail after an impulse" $ do
            r <- newReverbState
            buf <- impulseBuf 4096
            _ <- processReverb r buf
            samples <- VS.freeze buf
            -- after the impulse, energy should still be present further down the buffer
            let tail_ = VS.drop 256 samples
                energy = VS.sum (VS.map (\x -> x * x) tail_)
            assertBool ("expected nonzero tail energy, got " ++ show energy) (energy > 1e-9)
        , testCase "processDelay leaves a delayed echo of the impulse" $ do
            d <- newDelayState 512
            buf <- impulseBuf 2048
            _ <- processDelay d buf
            samples <- VS.freeze buf
            -- An echo of the original 1.0 impulse should appear ~512 samples later.
            let later = VS.drop 400 samples
            assertBool "delayed energy present" (VS.any (\x -> abs x > 0.05) later)
        , testCase "applyEffects is total across many buffers" $ do
            fx <- newEffectsState
            let go fx_ = do
                    buf <- impulseBuf 128
                    applyEffects fx_ buf
            -- Run 20 buffers; no exception is success.
            replicateM_ 20 (go fx)
        ]
