module Main where

import Pos
import Control.Monad ( forM_ )
import System.Environment ( getArgs )

main :: IO ()
main = do args <- getArgs
          forM_ args $ \w -> print $ map decode w
