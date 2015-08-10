{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Board where

import Debug
import Pos

import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.Array.Unboxed ( UArray,
                            accum, array, assocs, bounds, elems, indices, ixmap, listArray, 
                            (!), (//) )
import qualified Data.ByteString.Lazy as B
import Data.Ix ( Ix(..) )
import Data.List ( group, intercalate, sort )

data Board = Board { cells :: UArray Pos Bool
                   , rows :: UArray Int Int
                   }

empty :: Int -> Int -> Board
empty width height = Board (array (start, end) elems)
                           (array (0, maxY) [(y, 0) | y <- [0..maxY]])
  where start = Pos 0 0
        end = Pos (width - 1) maxY
        elems :: [(Pos, Bool)]
        elems = map (,False) $ range (start, end)
        maxY = height - 1

instance Show Board where
  show (Board c r) = show' 0 False $ map snd $ assocs c  -- for some reason, elems omits stuff?!?
    where width = 1 + (posX $ snd $ bounds c)
          show' _ _ [] = ""
          show' i offset cs = pad 3 (show (r!i)) ++ " " ++
                              ((if offset then (' ':) else id) $ row cs $
                               show' (i+1) (not offset) (drop width cs))
          row [] _ = ""
          row cs rest = '|':row' (take width cs) rest
          row' [] rest = '|':'\n':rest
          row' (c:cs) rest = s:space (row' cs rest)
            where s = if c then '✹' else '·'
                  space = if null cs then id else (' ':)
          pad n [] = replicate n ' '
          pad 0 x = x
          pad n (c:cs) = c:pad (n-1) cs

fill :: [Pos] -> Board -> Board
fill ps (Board cs rs) = clearLines ys $
                        Board (accum (const id) cs $ map (\p -> (p, True)) ps)
                              (accum (+) rs [(head y, length y) | y <- group ys'])
  where ys = uniq ys' 
        ys' = sort $ map posY ps -- why wasn't it sorted at the start?!?
        uniq (x:y:xs) | x == y = uniq (x:xs)
        uniq (x:xs) = x:uniq xs
        uniq [] = []
        
clearLines :: [Int] -> Board -> Board
clearLines ys b@(Board cs rs) = clear $ filter full ys
  where clear [] = b
        clear ls = trace ("Clearing " ++ show ls) $
                   Board (listArray bs $
                          replicate (length ls * width) False ++ (clear' width ls 0 $ elems cs))
                         (listArray (bounds rs) $
                          replicate (length ls) 0 ++ (clear' 1 ls 0 $ elems rs))
        clear' _ [] _ xs = xs
        clear' w (l:ls) y xs | y == l = clear' w ls (y+1) rest
                             | otherwise = take w xs ++ clear' w (l:ls) (y+1) rest
          where rest = drop w xs
        full :: Int -> Bool
        full y = rs ! y == width
        bs = bounds cs
        maxX = posX $ snd bs
        maxY = posY $ snd bs
        width = maxX + 1


instance FromJSON Board where
  parseJSON (Object v) = board <$> (v .: "width") <*> (v .: "height") <*> (v .: "filled")
    where board w h f = fill f $ empty w h
  parseJSON v = fail $ "Bad Board: " ++ show v
