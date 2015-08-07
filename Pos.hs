{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Pos ( Pos(..), Dir(..), move ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ((.:), FromJSON(..), Value(..))
import Data.Ix ( Ix(..) )

data Dir = E | W | SE | SW

data Pos = Pos { posX :: Int, posY :: Int }
         deriving ( Eq )

move :: Pos -> Dir -> Pos
move (Pos x y) E = Pos (x + 1) y
move (Pos x y) W = Pos (x - 1) y
move (Pos x y) SE = Pos (x + (y `mod` 2)) (y + 1)
move (Pos x y) SW = Pos (x - ((y + 1) `mod` 2)) (y + 1)

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
