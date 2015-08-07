module Main where

import System.Environment ( getArgs )

import Board

data CLI = CLI { files :: [String]
               , time  :: Int
               , mem   :: Int
               , power :: [String] }

main :: IO ()
main = do
  args <- getArgs
  cli <- parseArgs args
  forM_ (files cli) $ \f -> do
    b <- readBoard f
    putStrLn $ "File " ++ f
    print b

parseArgs :: [String] -> IO CLI
parseArgs [] = return $ CLI [] 0 0 []
parseArgs ("-f":f:rest) = (\c -> c { files = f:files c }) `fmap` parseArgs rest
parseArgs ("-p":p:rest) = (\c -> c { power = p:power c }) `fmap` parseArgs rest
parseArgs ("-t":t:rest) = (\c -> c { time = read t }) `fmap` parseArgs rest
parseArgs ("-m":m:rest) = (\c -> c { mem = read m }) `fmap` parseArgs rest
parseArgs args = fail $ "Bad args: " ++ show args
