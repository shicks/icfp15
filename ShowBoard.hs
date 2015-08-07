module Main where

import Control.Monad ( forM_ )
import System.Environment ( getArgs )

import Problem

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \f -> do
    b <- readProblem f
    putStrLn $ "=====" ++ (replicate (length f) '=')
    putStrLn $ "File " ++ f
    print b
