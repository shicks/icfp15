module Main where

import Control.Monad ( mapM )
import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment ( getArgs )

import Game
import Problem hiding ( tag )

data CLI = CLI { files :: [String]
               , time  :: Int
               , mem   :: Int
               , tag   :: String
               , power :: [String] }

main :: IO ()
main = do args <- getArgs
          cli <- parseArgs args
          outs <- mapM (solve cli) (files cli)
          B.putStrLn $ encode $ concat outs
  where solve cli file = playGame (tag cli) (power cli) `fmap` readProblem file

parseArgs :: [String] -> IO CLI
parseArgs [] = return $ CLI [] 0 0 "" []
parseArgs ("-f":f:rest) = (\c -> c { files = f:files c }) `fmap` parseArgs rest
parseArgs ("-p":p:rest) = (\c -> c { power = p:power c }) `fmap` parseArgs rest
parseArgs ("-t":t:rest) = (\c -> c { time = read t }) `fmap` parseArgs rest
parseArgs ("-m":m:rest) = (\c -> c { mem = read m }) `fmap` parseArgs rest
parseArgs ("--tag":t:rest) = (\c -> c { tag = t }) `fmap` parseArgs rest
parseArgs args = fail $ "Bad args: " ++ show args
