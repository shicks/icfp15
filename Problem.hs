{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Problem where

import Board
import Pos
import Unit

import Control.Applicative ( (<$>), (<*>) )
import Data.List ( intercalate )
import Data.Aeson ( (.:), decode, FromJSON(..), Value(..) )
import qualified Data.ByteString.Lazy as B

data Problem = Problem { board :: Board
                       , units :: [Unit] }
               -- TODO(sdh): source

instance Show Problem where
  show (Problem board units) = show board ++ "=====\n" ++ intercalate "-----\n" (map show units)

instance FromJSON Problem where
  parseJSON o@(Object v) = Problem <$> parseJSON o <*> (v .: "units")
  parseJSON v = fail $ "Bad Problem: " ++ show v

readProblem :: String -> IO Problem
readProblem f = do json <- B.readFile f
                   Just problem <- return $ decode json
                   return problem
