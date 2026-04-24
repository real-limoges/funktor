module Funktor.Live
    ( -- * Transport controls
      play
    , stop
    , setTempo
      -- * Re-exports for GHCi convenience
    , module Funktor.Core.Types
    , module Funktor.Core.Stream
    , module Funktor.Core.Pattern
    ) where

import Funktor.Core.Types
import Funktor.Core.Stream
import Funktor.Core.Pattern

play :: Stream Note -> IO ()
play = undefined

stop :: IO ()
stop = undefined

setTempo :: Tempo -> IO ()
setTempo = undefined
