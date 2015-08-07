{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Unit ( Unit(..), unit ) where

import Pos

import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ( (.:), FromJSON(..), Value(..) )
import Data.Ix ( range )

data Unit = Unit { members :: [Pos]
                 , pivot :: Pos
                 , extent :: Pos }

unit :: [Pos] -> Pos -> Unit
unit ms p = Unit (map shift ms) (shift p) (shift $ Pos maxX maxY)
  where xs = map posX $ p:ms
        ys = map posY $ p:ms
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        shift :: Pos -> Pos
        shift (Pos x y) = Pos (x - minX) (y - minY)

instance Show Unit where
  show (Unit members pivot extent) = show' False $ range (Pos 0 0, extent)
    where width = 1 + posX extent
          show' _ [] = ""
          show' offset ps = (if offset then (' ':) else id) $ row ps $
                            show' (not offset) (drop width ps)
          row [] _ = ""
          row ps rest = '|':row' (take width ps) rest
          row' [] rest = '|':'\n':rest
          row' (p:ps) rest = s:space (row' ps rest)
            where s = if p == pivot
                      then if p `elem` members then '@' else '.'
                      else if p `elem` members then 'O' else ' '
                  space = if null ps then id else (' ':)

instance FromJSON Unit where
  parseJSON (Object v) = unit <$> (v .: "members") <*> (v .: "pivot")
  parseJSON v = fail $ "Bad Unit: " ++ show v
