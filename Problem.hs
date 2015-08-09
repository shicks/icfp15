{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Problem where

import Board
import Pos hiding ( decode )
import Source
import Unit

import Control.Applicative ( (<$>), (<*>) )
import Data.List ( intercalate )
import Data.Aeson ( FromJSON(..), ToJSON(..), Value(..)
                  , (.:), (.=), decode, object)
import qualified Data.ByteString.Lazy as B

data Problem = Problem { id :: Int
                       , board :: Board
                       , units :: [Unit]
                       , sources :: [Source]
                       }

instance Show Problem where
  show (Problem id board units sources) = "ID " ++ show id ++ "\n" ++
                                          "Sources " ++ show sources ++ "\n" ++
                                          show board ++ "=====\n" ++
                                          intercalate "-----\n" (map show units)
                                          
                                         

instance FromJSON Problem where
  parseJSON o@(Object v) = Problem <$>
                           (v .: "id") <*>
                           parseJSON o <*>
                           (v .: "units") <*>
                           sources v
    where sources v = do
            len <- v .: "sourceLength"
            seeds <- v .: "sourceSeeds"
            return [Source seed len | seed <- seeds]
  parseJSON v = fail $ "Bad Problem: " ++ show v

data Output = Output { problemId :: Int
                     , outputSeed :: Int
                     , tag :: String
                     , solution :: String
                     }

instance ToJSON Output where
  toJSON (Output id seed tag solution) = object [ "problemId" .= id
                                                  , "seed" .= seed
                                                  , "tag" .= tag
                                                  , "solution" .= solution ]

instance FromJSON Output where
  parseJSON (Object v) = Output <$>
                         (v .: "problemId") <*>
                         (v .: "seed") <*>
                         (v .: "tag") <*>
                         (v .: "solution")

readProblem :: String -> IO Problem
readProblem f = do json <- B.readFile f
                   Just problem <- return $ decode json
                   return problem

readSolutions :: String -> IO [Output]
readSolutions f = do json <- B.readFile f
                     Just sol <- return $ decode json
                     return sol
