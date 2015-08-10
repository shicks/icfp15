module Main where

import Control.Monad ( forM_, mapM, when )
import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment ( getArgs )

import Debug
import Game
import Problem hiding ( tag )

data CLI = CLI { files :: [String]
               , time  :: Int
               , mem   :: Int
               , tag   :: String
               , power :: [String]
               , doScore :: Bool
               }

main :: IO ()
main = do args <- getArgs
          cli <- parseArgs args
          problems <- mapM readProblem $ files cli
          let outs = concat $ map (solve cli) problems
          B.putStrLn $ encode outs
          when (doScore cli) $ score cli problems outs
  where solve cli problem = playGame (tag cli) (power cli) problem
        score cli problems outs = forM_ outs $ \s@(Output id seed _ _) ->
          putStrLn $ show id ++ " seed " ++ show seed ++ ": " ++
                     (let (reason, score) = runGame (power cli) problems s
                      in show score ++ " (" ++ reason ++ ")")

parseArgs :: [String] -> IO CLI
parseArgs [] = return $ CLI [] 0 0 "" [] False
parseArgs ("-f":f:rest) = (\c -> c { files = f:files c }) `fmap` parseArgs rest
parseArgs ("-p":p:rest) = (\c -> c { power = p:power c }) `fmap` parseArgs rest
parseArgs ("-t":t:rest) = (\c -> c { time = read t }) `fmap` parseArgs rest
parseArgs ("-c":c:rest) = parseArgs rest -- ignore cpu param completely
parseArgs ("-m":m:rest) = (\c -> c { mem = read m }) `fmap` parseArgs rest
parseArgs ("--tag":t:rest) = (\c -> c { tag = t }) `fmap` parseArgs rest
parseArgs ("--score":rest) = (\c -> c { doScore = True }) `fmap` parseArgs rest
parseArgs ("--verbose":rest) = setVerbose True >> parseArgs rest
parseArgs args = fail $ "Bad args: " ++ show args
                 
