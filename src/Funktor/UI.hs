module Funktor.UI
    ( UIState (..)
    , CustomEvent (..)
    ) where

import Funktor.Core.Types (Beat, Tempo, Duration)
import Funktor.Grid (Grid)

-- | Custom events pushed into the TUI from background threads.
data CustomEvent = Tick
    deriving (Show)

-- | The terminal UI state.
data UIState = UIState
    { uiGrid        :: !Grid
    , uiCursorRow   :: !Int
    , uiCursorCol   :: !Int
    , uiTempo       :: !Tempo
    , uiCurrentBeat :: !Beat
    , uiPatternLen  :: !Duration
    , uiPlaying     :: !Bool
    } deriving (Show)
