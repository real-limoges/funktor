module Audio.EffectsSpec (tests) where

import Data.Vector.Storable.Mutable qualified as VM
import Funktor.Audio.Effects
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

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
            -- Starting from prev=0, output should land between 0 and the input.
            let (out, _) = applyLowPass 0.5 (LowPassState 0)
             in assertBool ("got " ++ show out) (out > 0 && out < 0.5)
        , testCase "applyLowPass updates state to the new output" $
            let (out, LowPassState prev) = applyLowPass 0.5 (LowPassState 0)
             in prev @?= out
        , testCase "newDelayState produces an initialised vector" $ do
            st <- newDelayState 256
            VM.length (delayBuffer st) @?= 256
            delayWritePos st @?= 0
        , testCase "applyDelay with feedback=0, mix=0 returns dry input" $ do
            st <- newDelayState 64
            (out, _) <- applyDelay 0 0 st 0.5
            assertBool ("got " ++ show out) (abs (out - 0.5) < 1e-9)
        , testCase "applyDelay advances write pointer" $ do
            st <- newDelayState 64
            (_, st') <- applyDelay 0 0 st 0.5
            delayWritePos st' @?= 1
        ]
