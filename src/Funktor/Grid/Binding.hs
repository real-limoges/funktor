module Funktor.Grid.Binding
    ( AudioEngine (..)
    , pressPad
    , releasePad
    , defaultGrid
    ) where

import Control.Concurrent.STM (TVar)
import Funktor.Core.Types (Note)
import Funktor.Core.Stream (Stream)
import Funktor.Grid
import qualified Data.Vector as V

-- | Handle to the audio engine for grid dispatch.
data AudioEngine = AudioEngine
    { engineAudioVar  :: !(TVar ())   -- ^ placeholder for TVar AudioState
    , engineSchedVar  :: !(TVar ())   -- ^ placeholder for TVar SchedulerState
    , enginePatterns  :: !(V.Vector (Stream Note))
    }

pressPad :: Int -> Int -> Grid -> AudioEngine -> IO ()
pressPad = undefined

releasePad :: Int -> Int -> Grid -> AudioEngine -> IO ()
releasePad = undefined

defaultGrid :: Grid
defaultGrid = undefined
