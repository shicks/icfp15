{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Board where

import Pos

import Debug.Trace

import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.Array ( Array,
                    accum, array, assocs, bounds, elems, indices, ixmap, listArray, 
                    (!), (//) )
import qualified Data.ByteString.Lazy as B
import Data.Ix ( Ix(..) )
import Data.List ( intercalate, sort )

data Board = Board { cells :: Array Pos Bool }

empty :: Int -> Int -> Board
empty width height = Board $ array (start, end) elems
  where start = Pos 0 0
        end = Pos (width - 1) (height - 1)
        elems :: [(Pos, Bool)]
        elems = map (,False) $ range (start, end)

instance Show Board where
  show (Board c) = show' False $ map snd $ assocs c  -- for some reason, elems omits stuff?!?
    where width = 1 + (posX $ snd $ bounds c)
          show' _ [] = ""
          show' offset cs = (if offset then (' ':) else id) $ row cs $
                            show' (not offset) (drop width cs)
          row [] _ = ""
          row cs rest = '|':row' (take width cs) rest
          row' [] rest = '|':'\n':rest
          row' (c:cs) rest = s:space (row' cs rest)
            where s = if c then '*' else ' '
                  space = if null cs then id else (' ':)

type YMap = Int -> Maybe Int

fill :: [Pos] -> Board -> Board
fill ps (Board cs) = clearLines ys $
                     Board $ accum (const id) cs $ map (\p -> (p, True)) ps
  where ys = uniq $ sort $ map posY ps  -- why wasn't it sorted at the start?!?
        uniq (x:y:xs) | x == y = uniq (x:xs)
        uniq (x:xs) = x:uniq xs
        uniq [] = []

clearLines :: [Int] -> Board -> Board
clearLines ys b@(Board cs) = clear $ filter full ys
  where clear [] = b
        clear ls = trace ("Clearing " ++ show ls) $
                   Board $ listArray bs $
                   replicate (length ls * width) False ++ (clear' ls 0 $ elems cs)
        clear' [] _ cs = cs
        clear' (l:ls) y cs | y == l = clear' ls (y+1) rest
                           | otherwise = take width cs ++ clear' (l:ls) (y+1) rest
          where rest = drop width cs
        full :: Int -> Bool
        full y = all (cs!) $ map (\x -> Pos x y) [0..maxX]
        bs = bounds cs
        maxX = posX $ snd bs
        maxY = posY $ snd bs
        width = maxX + 1


instance FromJSON Board where
  parseJSON (Object v) = board <$> (v .: "width") <*> (v .: "height") <*> (v .: "filled")
    where board w h f = fill f $ empty w h
  parseJSON v = fail $ "Bad Board: " ++ show v
