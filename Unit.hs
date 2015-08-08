{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Unit ( Unit(..), unit ) where

import Pos

import Debug.Trace

import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ( (.:), FromJSON(..), Value(..) )
import Data.Ix ( range )
import qualified Data.Set as S

data Unit = Unit { members :: [Pos]
                 , pivot :: Pos
                 , extent :: Pos
                 , shapeBounds :: (Pos, Pos)
                 , order :: Int
                 }

unit :: [Pos] -> Pos -> Unit
unit ms p = Unit (map shift ms) (shift p) extent bounds (findOrder ms p)
  where xs = map posX $ ms
        ys = map posY $ ms
        px = posX p
        py = posY p
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        shift :: Pos -> Pos
        shift (Pos x y) = Pos (x - min minX px) (y - min minY py)
        extent = shift $ Pos (max maxX px) (max maxY py)
        bounds = (shift $ Pos minX minY, shift $ Pos maxX maxY)

instance Show Unit where
  show (Unit members pivot extent _ o) = "Order " ++ show o ++ "\n"
                                         ++ show' False (range (Pos 0 0, extent))
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

-- *Returns the order of rotational symmetry
findOrder :: [Pos] -> Pos -> Int
findOrder ps piv = order' (r s) 1
  where s :: S.Set Pos
        s = S.fromList $ map (%- piv) ps
        r :: S.Set Pos -> S.Set Pos
        r = S.map (rotate 1)
        order' :: S.Set Pos -> Int -> Int
        order' s' n | s' == s = n
                    | otherwise = order' (r s') (n + 1)

-- *Expands a unit to all the positions its members will occupy
realize :: Unit   -- ^Unit to realize
        -> Pos    -- ^Position of the pivot
        -> Int    -- ^Rotation (0..5)
        -> [Pos]
realize u pos r = map (offset . rot . center) $ members u
  where center = (%- (pivot u))
        offset = (%+ pos)
        rot = rotate r

