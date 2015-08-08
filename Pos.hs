{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Pos ( Pos(..), Dir(..), move, rotate, (%+), (%-) ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ((.:), FromJSON(..), Value(..))
import Data.Ix ( Ix(..) )

data Dir = E | W | SE | SW


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
  index (Pos x1 y1, Pos x2 y2) (Pos x y) = (x - x1) * (y2 - y1) + (y - y1)
  {-# INLINE inRange #-}
  inRange (Pos x1 y1, Pos x2 y2) (Pos x y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

instance Show Pos where
  show (Pos x y) = show (x, y)

instance FromJSON Pos where
  parseJSON (Object v) = Pos <$> (v .: "x") <*> (v .: "y")
  parseJSON v = fail $ "Bad Pos: " ++ show v

move :: Pos -> Dir -> Pos
move (Pos x y) E = Pos (x + 1) y
move (Pos x y) W = Pos (x - 1) y
move (Pos x y) SE = Pos (x + (y `mod` 2)) (y + 1)
move (Pos x y) SW = Pos (x - ((y + 1) `mod` 2)) (y + 1)

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
