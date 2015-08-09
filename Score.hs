module Main where

import Control.Monad ( mapM, forM_ )
import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment ( getArgs )

import Debug
import Game
import Problem hiding ( tag )

data CLI = CLI { files :: [String]
               , power :: [String]
               , sol :: [String] }

main :: IO ()
main = do args <- getArgs
          cli <- parseArgs args
          problems <- mapM readProblem (files cli)
          sols <- concat `fmap` mapM readSolutions (sol cli)
          forM_ sols $ \s@(Output id seed _ _) ->
            putStrLn $ show id ++ " seed " ++ show seed ++ ": " ++
                       (let (reason, score) = runGame (power cli) problems s
                        in show score ++ " (" ++ reason ++ ")")

parseArgs :: [String] -> IO CLI
parseArgs [] = return $ CLI [] [] []
parseArgs ("-f":f:rest) = (\c -> c { files = f:files c }) `fmap` parseArgs rest
parseArgs ("-p":p:rest) = (\c -> c { power = p:power c }) `fmap` parseArgs rest
parseArgs ("-s":p:rest) = (\c -> c { sol = p:sol c }) `fmap` parseArgs rest
parseArgs ("--verbose":rest) = setVerbose True >> parseArgs rest
parseArgs args = fail $ "Bad args: " ++ show args
                 
