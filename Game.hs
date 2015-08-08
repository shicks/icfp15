{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Game where

import Board
import Pos
import Unit
import Problem
import Source

import Debug.Trace

import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ((.:), FromJSON(..), Value(..))
import Data.Array ( Array,
                    accum, array, assocs, bounds, elems, indices, ixmap, listArray, 
                    (!), (//) )
import Data.Ix ( inRange, range )
import Data.List ( maximumBy )
import Data.Ord ( comparing )
import qualified Data.Set as S

data Piece = Piece Unit Pos Int  -- unit, position, rotation
           deriving ( Show )

spawn :: Unit -> Board -> Piece
spawn u (Board b) = Piece u pos 0
  where unitWidth = posX (snd sb) - posX (fst sb) + 1
        boardWidth = (posX $ snd $ bounds b) + 1
        sb = shapeBounds u
        left = (boardWidth - unitWidth) `div` 2
        pos = Pos left 0 %+ pivot u %- fst sb

-- Is the piece allowed to go here?
valid :: Piece -> Board -> Bool
valid p b = all (emptyPos b) (realize p)

emptyPos :: Board -> Pos -> Bool
emptyPos (Board b) p = (bounds b `inRange` p) && (not $ b ! p)

boardWidth :: Board -> Int
boardWidth (Board b) = 1 + (posX $ snd $ bounds b)

-- *Expands a unit to all the positions its members will occupy
realize :: Piece -> [Pos]
realize (Piece u pos r) = map (offset . rot . center) $ members u
  where center = (%- (pivot u))
        offset = (%+ pos)
        rot = rotate r

realizeNeighbors :: Piece -> [Pos]
realizeNeighbors (Piece u pos r) = map (offset . rot . center) $ neighbors u
  where center = (%- (pivot u))
        offset = (%+ pos)
        rot = rotate r

realizeHeight :: Piece -> Int
realizeHeight (Piece u pos r) = posY pos + posY (rotate r $ com u %- pivot u)

-- TODO(sdh): consider memoizing the row sizes to make bonus computation quicker
score :: Int -> Piece -> Board -> Int
score ls_old piece board = points - gap_penalty + height_bonus
  where points = line_points + line_bonus
        line_points = 100 * (1 + ls) * ls `div` 2
        cellsList = realize piece
        cellsSet = S.fromList cellsList
        ys = uniq $ map posY cellsList
        ls = length $ filter full ys
        full :: Int -> Bool
        full y = all (\p -> (cells board ! p) || (p `S.member` cellsSet)) $
                 map (\x -> Pos x y) [0..(boardWidth board - 1)]
        line_bonus = if ls_old > 1 then (ls_old - 1) * line_points `div` 10 else 0
        -- TODO(sdh): weight underside gaps more heavily?
        gap_penalty = gap_factor * length empty_neighbors
        empty_neighbors = filter (emptyPos board) $ realizeNeighbors piece
        height_bonus = height_factor * realizeHeight piece
        gap_factor = 10
        height_factor = 2

-- This is more efficient than nub if duplicates are always together
uniq :: Eq a => [a] -> [a]
uniq (x:y:xs) | x == y = uniq (x:xs)
uniq (x:xs) = x:uniq xs
uniq [] = []

apply :: Char -> Spot -> Spot
apply c (p, r, s) | Just cc <- decode c = apply' cc
                  | Nothing <- decode c = (p, r, s)
  where apply' (Move d) = (p %+ displacement d, r, c:s)
        apply' (Rotate CW) = (p, (r + 1) `mod` 6, c:s)
        apply' (Rotate CCW) = (p, (r - 1) `mod` 6, c:s)

undo :: Spot -> Spot
undo (p, r, (c:s)) | Just cc <- decode c = unapply cc
                   | Nothing <- decode c = undefined -- should never happen
  where unapply (Move d) = (p %- displacement d, r, s)
        unapply (Rotate CW) = (p, (r - 1) `mod` 6, s)
        unapply (Rotate CCW) = (p, (r + 1) `mod` 6, s)

type Spot = (Pos, Int, String)

-- Actually explore
findBest :: Board -> Piece -> Spot
findBest board (Piece unit p r) = search [(p, r, "")] S.empty S.empty
  where search :: [Spot] -> S.Set Spot -> S.Set Spot -> Spot
        search [] end _ = findMax end
        search (spot:queue) end seen
          | strip spot `S.member` seen = search queue end seen
          | not $ valid (piece spot) board = search queue (S.insert (undo spot) end) seen
          -- TODO(sdh): try power words FIRST
          | otherwise = search (followups ++ queue) end (S.insert (strip spot) seen)
            where followups = map (\c -> apply c spot) commandChars
        piece :: Spot -> Piece
        piece (p, r, _) = Piece unit p r
        strip :: Spot -> Spot  -- removes the string for set entry
        strip (p, r, _) = (p, r, "")
        findMax :: S.Set Spot -> Spot
        findMax = snd . maximumBy (comparing fst) .
                  map (\s@(_, _, sol) -> (score 0 (piece s) board, s)) .
                  S.toList

playGame :: Problem -> [Output]
playGame (Problem id initialBoard units sources) = play sources
  where play [] = []
        play (source:rest) = play1 source : play rest
        play1 :: Source -> Output
        play1 source = Output id (seed source) "" $
                       playSeed initialBoard $ map getUnit $ runSource source
        getUnit :: Int -> Unit
        getUnit i = units !! (i `mod` numUnits)
        numUnits = length units
        playSeed :: Board -> [Unit] -> String
        playSeed _ [] = trace "OUT OF UNITS" ""
        playSeed board (unit:rest) = trace ("playSeed\n" ++ show board ++ show unit) $
                                     playPiece (spawn unit board)
          where playPiece :: Piece -> String
                playPiece piece@(Piece unit _ _)
                  | not $ valid piece board = trace ("INVALID SPAWN: " ++ show piece) $
                                              ""
                  | otherwise = trace ("Locking: " ++ show pos) $
                                reverse sol ++ playSeed board' rest
                  where (pos, rot, sol) = findBest board piece
                        board' = trace ("Realized: " ++ show realized) $
                                 fill realized board
                        realized = realize $ Piece unit pos rot
