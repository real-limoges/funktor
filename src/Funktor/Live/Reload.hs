module Funktor.Live.Reload
    ( persistAt
    , reload
    ) where

import Funktor.Core.Types (Note)
-- TODO: add foreign-store dependency for persistence across GHCi :reload
import Funktor.Core.Stream (Stream)

persistAt :: Int -> IO a -> IO a
persistAt = undefined

reload :: Stream Note -> IO ()
reload = undefined
