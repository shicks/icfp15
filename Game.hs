{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Game where

import Pos
import Board
import Unit
import Source

import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.Array ( Array,
                    accum, array, assocs, bounds, elems, indices, ixmap, listArray, 
                    (!), (//) )
import qualified Data.ByteString.Lazy as B
import Data.Ix ( Ix(..) )
import Data.List ( intercalate )


data Piece = Piece Unit Pos Int  -- unit, position, rotation

spawn :: Unit -> Board -> Piece
spawn u (Board b) = Piece u pos 0
  where unitWidth = posX (snd sb) - posX (fst sb) + 1
        boardWidth = (posX $ snd $ bounds b) + 1
        sb = shapeBounds u
        left = (boardWidth - unitWidth) `div` 2
        pos = Pos left 0 %+ pivot u %- fst sb

-- Does the piece collide?
collision :: Piece -> Board -> Bool
collision (Piece u p r) (Board b) = 
