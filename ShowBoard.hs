module Main where

import Control.Monad ( forM_ )
import System.Environment ( getArgs )

import Board

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \f -> do
    b <- readBoard f
    putStrLn $ "File " ++ f
    print b
