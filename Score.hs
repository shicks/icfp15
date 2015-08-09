module Main where

import Control.Monad ( mapM, forM_ )
import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Function ( on )
import Data.List ( groupBy, sort )
import System.Environment ( getArgs )

import Debug
import Game
import Problem hiding ( tag )

data CLI = CLI { files :: [String]
               , power :: [String]
               , sol :: [String]
               , splitProblems :: Bool
               }

type Result = (Int, Int, String, Int)

main :: IO ()
main = do args <- getArgs
          cli <- parseArgs args
          problems <- mapM readProblem (files cli)
          sols <- concat `fmap` mapM readSolutions (sol cli)
          let result :: [Result]
              result = map (\s -> let (reason, score) = runGame (power cli) problems s
                                  in (problemId s, outputSeed s, reason, score))
                       sols
          let grouped = if splitProblems cli
                        then result
                        else map avg $ groupBy ((==) `on` probId) $ sort result
                where avg :: [Result] -> Result
                      avg rs | any isRepeat rs = score 0 rs
                             | otherwise = score (sum (map getScore rs) `div` length rs) rs
                      score s ((i, _, _, _):_) = (i, -1, "", s)
                      score _ _ = undefined
                      probId :: Result -> Int
                      probId (i, _, _, _) = i
                      isRepeat :: Result -> Bool
                      isRepeat (_, _, r, _) = r == "repeat"
                      getScore :: Result -> Int
                      getScore (_, _, _, s) = s
          forM_ grouped $ \(prob, seed, reason, score) ->
            putStrLn $ show prob ++ (if seed >= 0 then " (" ++ show seed ++ ")" else "") ++
                       ": " ++ show score ++ (if null reason then "" else " (" ++ reason ++ ")")

parseArgs :: [String] -> IO CLI
parseArgs [] = return $ CLI [] [] [] False
parseArgs ("-f":f:rest) = (\c -> c { files = f:files c }) `fmap` parseArgs rest
parseArgs ("-p":p:rest) = (\c -> c { power = p:power c }) `fmap` parseArgs rest
parseArgs ("-s":p:rest) = (\c -> c { sol = p:sol c }) `fmap` parseArgs rest
parseArgs ("--split":rest) = (\c -> c { splitProblems = True }) `fmap` parseArgs rest
parseArgs ("--verbose":rest) = setVerbose True >> parseArgs rest
parseArgs args = fail $ "Bad args: " ++ show args
                 
