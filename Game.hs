{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Game where

import Board
import Pos
import Unit
import Problem
import Source

import Debug.Trace

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( first )
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

-- This doesn't work...
-- realizeNeighbors :: Piece -> [(Pos, Int)]
-- realizeNeighbors (Piece u pos r) = map (first $ offset . rot . center) $ neighbors u
--   where center = (%- (pivot u))
--         offset = (%+ pos)
--         rot = rotate r

realizeNeighbors :: S.Set Pos -> [(Pos, Int)]
realizeNeighbors ms = filter (not . (`S.member` ms) . fst) $ concatMap around $ S.toList ms

realizeHeight :: Piece -> Int
realizeHeight (Piece u pos r) = posY pos + posY (rotate r $ com u %- pivot u)

-- TODO(sdh): consider memoizing the row sizes to make bonus computation quicker
score :: Int -> Piece -> Board -> Int
score ls_old piece board = points - gap_penalty + height_bonus + same_row_bonus
  where points = line_points + line_bonus
        line_points = 100 * (1 + ls) * ls `div` 2
        cellsList = realize piece
        cellsSet = S.fromList cellsList
        ys = uniq $ map posY cellsList
        ls = length $ filter full ys
        same_row_bonus = (sum $ map same_row ys) * same_row_factor
        same_row y = length $ filter (\p -> (cells board ! p) || (p `S.member` cellsSet)) $
                     map (\x -> Pos x y) [0..maxX]
        full :: Int -> Bool
        full y = all (\p -> (cells board ! p) || (p `S.member` cellsSet)) $
                 map (\x -> Pos x y) [0..maxX]
        line_bonus = if ls_old > 1 then (ls_old - 1) * line_points `div` 10 else 0
        -- TODO(sdh): weight underside gaps more heavily?
        -- TODO(sdh): consider weighting by # filled on the same line...
        gap_penalty = gap_factor * sum empty_neighbors
        empty_neighbors = map snd $ filter (emptyPos board . fst) $ realizeNeighbors cellsSet
        height_bonus = height_factor * realizeHeight piece
        width = boardWidth board
        maxX = width - 1
        gap_factor = 2
        height_factor = 1
        same_row_factor = 3
        

-- This is more efficient than nub if duplicates are always together
uniq :: Eq a => [a] -> [a]
uniq (x:y:xs) | x == y = uniq (x:xs)
uniq (x:xs) = x:uniq xs
uniq [] = []

apply :: Unit -> Char -> Spot -> Spot
apply u c (p, r, s, w) | Just cc <- decode c = apply' cc
                       | Nothing <- decode c = (p, r, s, w)
  where apply' (Move d) = (p %+ displacement d, r, c:s, w)
        apply' (Rotate CW) = (p, (r + 1) `mod` order u, c:s, w)
        apply' (Rotate CCW) = (p, (r - 1) `mod` order u, c:s, w)

undo :: Unit -> Spot -> Spot
undo u (p, r, (c:s), w) | Just cc <- decode c = unapply cc
                        | Nothing <- decode c = undefined -- should never happen
  where unapply (Move d) = (p %- displacement d, r, s, w)
        unapply (Rotate CW) = (p, (r - 1) `mod` order u, s, w)
        unapply (Rotate CCW) = (p, (r + 1) `mod` order u, s, w)

type Power = ([String], String, String) -- used, current attempt, queue

applyPower :: Unit -> Spot -> Spot
applyPower unit (p, r, s, (u, w, c:[])) = apply unit c (p, r, s, (w:u, "", ""))
applyPower unit (p, r, s, (u, w, c:rest)) = apply unit c (p, r, s, (u, w, rest))
applyPower _ _ = undefined

addPower :: Unit -> String -> Spot -> Spot
addPower unit w (p, r, s, (u, "", "")) = applyPower unit (p, r, s, (u, w, w))
addPower _ _ _ = undefined

removePower :: Spot -> Spot
removePower (p, r, s, (u, _, _)) = (p, r, s, (u, "", ""))

hasPower :: Power -> Bool
hasPower (_, w, _) = not $ null w

type Spot = (Pos, Int, String, Power) -- pos, rot, sol, power

-- Actually explore
findBest :: [String] -> Board -> Piece -> Spot
findBest wordList board (Piece unit p r) = search [(p, r, "", ([], "", ""))] S.empty S.empty
  where search :: [Spot] -> S.Set Spot -> S.Set Spot -> Spot
        search [] end _ = findMax end
        search (spot@(_,_,_,w):queue) end seen
          | strip spot `S.member` seen = --trace ("ALREADY SEEN " ++ show spot) $
                                         search queue end seen
          | not $ valid (piece spot) board = --trace ("INVALID " ++ show spot) $
                                             search queue (S.insert (undo unit spot) end) seen
          | otherwise = --trace ("ADDING MORE TO spot=" ++ show spot ++ ", queue=" ++ show queue) $
                        search (power ++ followups ++ queue) end (S.insert (strip spot) seen)
            where followups = map (\c -> apply unit c $ removePower spot) commandChars
                  power = if hasPower w
                          then [applyPower unit spot]
                          else map (\w -> addPower unit w spot) $ rot (length queue) wordList
        piece :: Spot -> Piece
        piece (p, r, _, _) = Piece unit p r
        strip :: Spot -> Spot  -- removes the string for set entry
        strip (p, r, _, _) = (p, r, "", ([], "", ""))
        findMax :: S.Set Spot -> Spot
        findMax = snd . maximumBy (comparing fst) .
                  map (\s -> -- tr show $
                             (powerScore s + score 0 (piece s) board, s)) .
                  S.toList
        -- Note: doesn't count power_bonus, since it's not a marginal gain
        powerScore (_, _, _, (power, _, _)) = sum $ map (\p -> length p) power

tr f o = trace (f o) o

-- used to pseudo-randomize which words we try to use
rot :: Int -> [a] -> [a]
rot n [] = []
rot n as = drop nn as ++ take nn as
  where nn = n `mod` length as

playGame :: String -> [String] -> Problem -> [Output]
playGame tag words (Problem id initialBoard units sources) = play sources
  where play [] = []
        play (source:rest) = play1 source : play rest
        play1 :: Source -> Output
        play1 source = Output id (seed source) tag $
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
                  where (pos, rot, sol, _) = findBest words board piece
                        board' = trace ("Realized: " ++ show realized) $
                                 fill realized board
                        realized = realize $ Piece unit pos rot
