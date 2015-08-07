{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Source ( Source(..), runSource ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ( (.:), FromJSON(..), Value(..) )
import Data.Bits ( shiftR, (.&.) )
import Data.Ix ( range )

data Source = Source { seed :: Int
                     , size :: Int }

runSource :: Source -> [Int]
runSource (Source seed 0) = []
runSource (Source seed size) = num : runSource (Source next (size - 1))
  where num = shiftR seed 16 .&. 0x7fff
        next = (seed * multiplier + increment) .&. 0xffffffff
        multiplier = 1103515245
        increment = 12345
