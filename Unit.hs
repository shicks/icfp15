{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Unit ( Unit(..), unit ) where

import Pos

import Debug.Trace

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( first )
import Data.Aeson ( (.:), FromJSON(..), Value(..) )
import Data.List ( sort )
import Data.Ix ( range )
import qualified Data.Set as S

data Unit = Unit { members :: [Pos]
                 , pivot :: Pos
                 , extent :: Pos
                 , shapeBounds :: (Pos, Pos)
                 , order :: Int
                 , neighbors :: [(Pos, Int)]  -- relative to the pivot, maps to penalty
                 , com :: Pos  -- center of mass relative to pivot
                 }

unit :: [Pos] -> Pos -> Unit
unit ms p = Unit (map shift ms) (shift p) extent bounds (findOrder ms p) ns com
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
        msSet = S.fromList ms
        ns :: [(Pos, Int)]
        ns = map (first (%-p)) $ combine ns'
        combine :: [(Pos, Int)] -> [(Pos, Int)]
        combine [] = []
        combine ((a,s):rest) | a `S.member` msSet = combine rest
        combine ((a,s):(a',s'):rest) | a==a' = combine $ (a,s+s'):rest
        combine (a:rest) = a:combine rest
        -- ns = map (%-p) $ S.elems $ (S.\\) ns' (S.fromList ms)
        ns' = sort $ concatMap around ms
        -- Note: this isn't technically center of mass, it's just the
        -- center of the bounds, but it's generally a good approximation
        com = Pos ((maxX + minX) `div` 2) ((maxY + minY) `div` 2) %- p
        

instance Show Unit where
  show (Unit members pivot extent _ o ns _ )
         = "Order " ++ show o ++ ", " ++ show (length ns) ++ " neighbors\n"
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
                      then if p `elem` members then '@' else 'x'
                      else if p `elem` members then 'O' else '·'
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
