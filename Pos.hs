{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Pos ( Pos(..), Dir(..), CDir(..), Command(..)
           , commands, commandChars
           , decode, encode
           , around, displacement, rotate
           , (%+), (%-)
           ) where

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( first )
import Data.Aeson ((.:), FromJSON(..), Value(..))
import Data.Ix ( Ix(..) )

data Dir = E | W | SE | SW
         deriving (Eq, Ord, Enum, Show)
data CDir = CW | CCW
          deriving (Eq, Ord, Enum, Show)

data Command = Move Dir | Rotate CDir
          deriving (Eq, Ord, Show)
commands = [Move E, Move W, Move SE, Move SW, Rotate CW, Rotate CCW]

commandChars :: [Char]
commandChars = map encode commands

encode :: Command -> Char
encode (Move W) = 'p'  -- also ', !, ., 0, 3
encode (Move E) = 'b'  -- also c, e, f, y, 2
encode (Move SW) = 'a'  -- also g, h, i, j, 4
encode (Move SE) = 'm'  -- also l, n, o, space, 5
encode (Rotate CW) = 'd'  -- also q, r, v, z, 1
encode (Rotate CCW) = 'k'  -- also s, t, u, w, x
-- TODO(sdh): \t, \n, and \r are NOOPs, should we bother supporting decode?

decode :: Char -> Maybe Command
decode c | c `elem` "p'!.03" = Just $ Move W
         | c `elem` "bcefy2" = Just $ Move E
         | c `elem` "aghij4" = Just $ Move SW
         | c `elem` "lmno 5" = Just $ Move SE
         | c `elem` "dqrvzl" = Just $ Rotate CW
         | c `elem` "kstuwx" = Just $ Rotate CCW
         | otherwise = Nothing

data Pos = Pos { posX :: Int, posY :: Int }
         deriving ( Eq )

-- Note: Pos does not behave well under addition, etc.  Use C instead.

type C = (Int, Int)

toC :: Pos -> C
toC (Pos x y) = (x - half' y, y)

fromC :: C -> Pos
fromC (a, b) = Pos (a + half' b) b

half' z | z >= 0 = z `div` 2
        | otherwise = -((1-z) `div` 2)

addC :: C -> C -> C
addC (x1, y1) (x2, y2) = (x1+x2, y1+y2)

subC :: C -> C -> C
subC (x1, y1) (x2, y2) = (x1-x2, y1-y2)

instance Ord Pos where
  compare (Pos x1 y1) (Pos x2 y2) = compare (y1, x1) (y2, x2)

instance Ix Pos where
  {-# INLINE range #-}
  range (Pos x1 y1, Pos x2 y2) = [Pos x y | y <- range (y1, y2), x <- range (x1, x2)]
  {-# INLINE index #-}
  index (Pos x1 y1, Pos x2 y2) (Pos x y) = (y - y1) * (x2 - x1 + 1) + (x - x1)
  {-# INLINE inRange #-}
  inRange (Pos x1 y1, Pos x2 y2) (Pos x y) = x1 <= x && x <= x2 && y1 <= y && y <= y2
  {-# INLINE rangeSize #-}
  rangeSize (Pos x1 y1, Pos x2 y2) = (x2-x1+1)*(y2-y1+1)

instance Show Pos where
  show (Pos x y) = show (x, y)

instance FromJSON Pos where
  parseJSON (Object v) = Pos <$> (v .: "x") <*> (v .: "y")
  parseJSON v = fail $ "Bad Pos: " ++ show v

around :: Pos -> [(Pos, Int)]
around p = map (first (%+p)) dirs
  where dirs :: [(Pos, Int)]
        dirs = [ (Pos 1 0, 2)
               , (Pos 0 1, 5)
               , (Pos (-1) 1, 5)
               , (Pos (-1) 0, 2)
               , (Pos (-1) (-1), 1)
               , (Pos 0 (-1), 1)
               ]

displacement :: Dir -> Pos
displacement E = Pos 1 0
displacement W = Pos (-1) 0
displacement SE = Pos 0 1
displacement SW = Pos (-1) 1

(%+) :: Pos -> Pos -> Pos
p %+ q = fromC $ toC p `addC` toC q

(%-) :: Pos -> Pos -> Pos
p %- q = fromC $ toC p `subC` toC q

-- For rotations, we use complex numbers: j = 1/2 - i * sqrt(3)/2
--   Then the point (x, y) maps to (x - y/2) + y * j
--   Clockwise rotations are multiplication by j, where j^2 = j-1.

-- Rotates clockwise |r| times about about 0 0
rotate :: Int -> Pos -> Pos
rotate 0 p = p
rotate 1 p = fromC $ rot' $ toC p
  where rot' (a, b) = (-b, a+b)
rotate r p | r > 1 && r < 6 = rotate (r - 1) (rotate 1 p)
           | otherwise = rotate (r `mod` 6) p
