{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Game where

import Board
import Debug
import Pos
import Problem
import Unit
import Source

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( first )
import Data.Aeson ((.:), FromJSON(..), Value(..))
import Data.Array.Unboxed ( UArray,
                            accum, array, assocs, bounds, elems, indices, ixmap, listArray, 
                            (!), (//) )
import Data.Ix ( inRange, range )
import Data.List ( isPrefixOf, maximumBy, sort, sortBy )
import Data.Ord ( comparing )
import qualified Data.Set as S

data Piece = Piece Unit Pos Int  -- unit, position, rotation
           deriving ( Show )

spawn :: Unit -> Board -> Piece
spawn u (Board b _) = Piece u pos 0
  where unitWidth = posX (snd sb) - posX (fst sb) + 1
        boardWidth = (posX $ snd $ bounds b) + 1
        sb = shapeBounds u
        left = (boardWidth - unitWidth) `div` 2
        pos = Pos left 0 %+ pivot u %- fst sb

-- Is the piece allowed to go here?
valid :: Piece -> Board -> Bool
valid p b = all (emptyPos b) (realize p)

emptyPos :: Board -> Pos -> Bool
emptyPos (Board b _) p = (bounds b `inRange` p) && (not $ b ! p)

boardWidth :: Board -> Int
boardWidth (Board b _) = 1 + (posX $ snd $ bounds b)

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

around'' :: Board -> Pos -> [(Pos, Int)]
around'' b@(Board _ r) p = map fix $ around p
  where fix (p, 5) | (bounds r `inRange` posY p) && ((r ! posY p) > full) = (p, 25)
        fix z = z
        full = 4 * boardWidth b `div` 5

around' = const around

realizeNeighbors :: Board -> S.Set Pos -> [(Pos, Int)]
realizeNeighbors b ms =
  filter (not . (`S.member` ms) . fst) $ concatMap (around' b) $ S.toList ms

realizeBelow :: S.Set Pos -> [Pos]
realizeBelow ms = uniq $ sort $ [m %+ displacement d | m <- S.toList ms, d <- [SW, SE]]

realizeCoM :: Piece -> Pos
realizeCoM (Piece u pos r) = pos %+ (rotate r $ com u %- pivot u)

realizeHeight :: [Pos] -> Int
realizeHeight ps = minimum $ map posY ps

-- TODO(sdh): consider memoizing the row sizes to make bonus computation quicker
score :: Int -> Piece -> Board -> Int
score ls_old piece board = points            -- favor choices that get us points directly
                           - gap_penalty     -- minimize the empty spaces around our pieces
                           + height_bonus    -- favor locking on the bottom row
                           + same_row_bonus  -- favor locking on rows with many other pieces 
                           - cover_penalty   -- avoid empty spaces beneath the piece
                           -- - slant_penalty   -- favor propping pieces against the edge
                           -- - single_line_penalty   -- avoid eliminating one line at a time
  where points = line_points + line_bonus
        line_points = 100 * (1 + ls) * ls `div` 2
        cellsList = realize piece
        cellsSet = S.fromList cellsList
        ys' = sort $ map posY cellsList
        ys = uniq ys'
        fulls = filter full ys
        ls = length fulls
        same_row_bonus = (sum $ map same_row ys') * same_row_factor
        same_row y = length (filter (==y) ys') + (rows board ! y)
        full :: Int -> Bool
        full = (== width) . same_row
        line_bonus = if ls_old > 1 then (ls_old - 1) * line_points `div` 10 else 0
        -- TODO(sdh): weight underside gaps more heavily?
        -- TODO(sdh): consider weighting by # filled on the same line...
        gap_penalty = gap_factor * sum empty_neighbors
        empty_neighbors = map snd $ filter (emptyPos board . fst) $ realizeNeighbors board cellsSet
        height_bonus = height_factor * height
        height = posY com
        -- height = realizeHeight cellsList
        com = realizeCoM piece
        width = boardWidth board
        maxX = width - 1
        cover_penalty = cover_penalty_factor *
                        if covered_empty > 0 then covered_filled - covered_empty else 0
        covered_all = realizeBelow cellsSet
        covered_empty = length $ filter emptyPos' covered_all
        covered_filled = length covered_all - covered_empty
        emptyPos' p = emptyPos board p && not (p `S.member` cellsSet)
        xy_corr = avg (map (\(Pos x y) -> x * y) cellsList) -
                  avg (map posX cellsList) * avg (map posY cellsList)
        avg xs = sum xs `div` length xs
        slant_penalty = if xy_corr < 0 then 5 else 0
        gap_factor = 5
        height_factor = 2
        same_row_factor = 5
        cover_penalty_factor = 10
        -- attempt to make a clear path for multiple lines
        -- single_line_penalty | height > 5 && ls == 1 && not (cells board ! Pos 4 (head fulls)) = 2 * line_bonus
        --                     | height > 5 && ls == 0 && any ((==4) . posX) cellsList = 100
        --                     | otherwise = 0
        

-- This is more efficient than nub if duplicates are always together (why does this assumption break?!?)
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

-- Note: we *don't* remove the final command, since it was needed to lock it
undo :: Unit -> Spot -> Spot
undo u (p, r, s@(c:_), w) | Just cc <- decode c = unapply cc
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

emptyRow :: Board -> Int -> Bool
emptyRow (Board _ r) y = (bounds r `inRange` y) && (r!y) == 0

data OpenRows = OpenRows (S.Set Int) (S.Set Int)
openRows :: Unit -> Board -> OpenRows
openRows u b@(Board _ r) = OpenRows S.empty all
  where all = S.difference (S.fromList [0..maxY])
                           (S.fromList [y + h | y <- full, h <- [(-rad)..(rad-1)]])
        full = filter (\y -> (r!y) > 0) [0..maxY]
        rad = radius u
        maxY = snd $ bounds r
        
isSeen :: OpenRows -> Pos -> Bool
isSeen (OpenRows seen all) (Pos _ y) = y `S.member` seen

insertSeen :: Pos -> OpenRows -> OpenRows
insertSeen (Pos _ y) o@(OpenRows seen all) = if y `S.member` all
                                             then OpenRows (S.insert y seen) all
                                             else o

-- Actually explore
findBest :: Power -> [String] -> Board -> Piece -> Spot
findBest initialPower wordList board (Piece unit p r) = search [(p, r, "", initialPower)] S.empty S.empty initialOpenSeen
  where search :: [Spot] -> S.Set Spot -> S.Set Spot -> OpenRows -> Spot
        search [] end _ openSeen = findMax end
        search (spot@(p,_,_,w):queue) end seen openSeen
          | strip spot `S.member` seen = --trace ("ALREADY SEEN " ++ show spot) $
                                         search queue end seen openSeen
          | not $ valid (piece spot) board = --trace ("INVALID " ++ show spot) $
                                             search queue (S.insert (undo unit spot) end) seen openSeen
          -- | not (hasPower w) && isSeen openSeen p = search queue end seen openSeen
          | otherwise = --trace ("ADDING MORE TO spot=" ++ show spot ++ ", queue=" ++ show queue) $
                        search (power ++ followups ++ queue) end (S.insert (strip spot) seen) openSeen'
          where followups = map (\c -> apply unit c $ removePower spot) (commandChars seed)
                power = if hasPower w
                        then [applyPower unit spot]
                        else map (\w -> addPower unit w spot) $ rot'' seed wordList
                openSeen' = insertSeen p openSeen
                seed = length queue
        initialOpenSeen = openRows unit board
        piece :: Spot -> Piece
        piece (p, r, _, _) = Piece unit p r
        strip :: Spot -> Spot  -- removes the string for set entry
        strip (p, r, _, _) = (p, r, "", ([], "", ""))
        findMax :: S.Set Spot -> Spot
        findMax = snd . maximumBy (comparing fst) .
                  map (\s -> (powerScore s + score 0 (piece s) board, s)) .
                  S.toList
        -- Note: doesn't count power_bonus, since it's not a marginal gain
        powerScore (_, _, _, (power, _, _)) = sum $ map (\p -> length p) power
        rot'' = rot' (length wordList)


-- weighted rot
rot' :: Int -> Int -> [a] -> [a]
rot' tot seed list = rot index list
  where seed' = sqtot - (seed `mod` sqtot) - 1
        index = (tot - (floor $ sqrt $ fromIntegral seed') - 1) `mod` tot
        sqtot = tot * tot

-- used to pseudo-randomize which words we try to use
rot :: Int -> [a] -> [a]
rot n [] = []
rot n as = drop nn as ++ take nn as
  where nn = n `mod` length as

playGame :: String -> [String] -> Problem -> [Output]
playGame tag words' (Problem id initialBoard units sources) = play sources
  where words = sortBy (comparing length) words'
        play [] = []
        play (source:rest) = play1 source : play rest
        play1 :: Source -> Output
        play1 source = Output id (seed source) tag $
                       playSeed ([], "", "") initialBoard $ map getUnit $ runSource source
        getUnit :: Int -> Unit
        getUnit i = units !! (i `mod` numUnits)
        numUnits = length units
        playSeed :: Power -> Board -> [Unit] -> String
        playSeed _ _ [] = trace "OUT OF UNITS" ""
        playSeed power board (unit:rest) = trace ("playSeed\n" ++ show board ++ show unit) $
                                           playPiece (spawn unit board)
          where playPiece :: Piece -> String
                playPiece piece@(Piece unit _ _)
                  | not $ valid piece board = trace ("INVALID SPAWN: " ++ show piece) $
                                              ""
                  | otherwise = trace ("Locking: " ++ show pos ++ ": " ++ reverse sol) $
                                reverse sol ++ playSeed ([], w, q) board' rest
                  where (pos, rot, sol, (_, w, q)) = findBest power words board piece
                        board' = trace ("Realized: " ++ show realized) $
                                 fill realized board
                        realized = realize $ Piece unit pos rot


-- TODO(sdh): Result type (String, Int [String])

-- Runs an Output
runGame :: [String] -> [Problem] -> Output -> (String, Int)
runGame words ps (Output pid seed _ sol) = runProblem $ head $ filter (\(Problem i _ _ _) -> i == pid) ps
  where runProblem (Problem _ initialBoard units sources) =
          run initialBoard $ map getUnit $ runSource $ head $ filter (\(Source s _) -> s == seed) sources
            where getUnit :: Int -> Unit
                  getUnit i = units !! (i `mod` numUnits)
                  numUnits = length units
        run :: Board -> [Unit] -> (String, Int)
        run initialBoard (u:rest) = run' 0 0 S.empty sol "" initialBoard (spawn u initialBoard) rest S.empty
        run' :: Int -> Int -> S.Set String -> String -> String -> Board -> Piece -> [Unit] -> S.Set Spot -> (String, Int)
        run' score _ words_used "" _ _ _ _ _ = ("last command", score + 300 * S.size words_used)
        run' _ _ _ _ _ _ (Piece _ p r) _ seen | (p, r, "", none) `S.member` seen = ("repeat", 0)
        run' score ls_old words_used (c:cs) prev_cs board piece@(Piece u p r) next seen
          = case valid piece' board of
              True -> run' score' ls_old words_used' cs prev_cs' board piece' next seen'
              False -> if null next
                       then ("last unit", score'' + words_bonus)
                       else if valid piece'' board
                            then -- trace (show board') $
                                 run' score'' ls words_used' cs prev_cs' board' piece'' (tail next) S.empty
                            else ("board full: " ++ show (length cs) ++ " unused", score'' + words_bonus)
          where score' = (if new_words_score > 0 then trace ("scoring word " ++ show new_words_score) else \x->x) $
                         score + new_words_score
                score'' = trace ("scoring piece " ++ show points ++
                                 (if line_bonus > 0 then "\nline bonus " ++ show line_bonus else "")) $
                          score' + points + line_bonus
                points = length (members u) + line_score
                line_score = 100 * (1 + ls) * ls `div` 2
                line_bonus = if ls_old > 1
                             then points * (ls_old - 1) `div` 10
                             else 0
                seen' = S.insert (p, r, "", none) seen
                piece' = case decode c of
                  Nothing -> piece -- TODO(sdh): return 0, or at least an invalid piece...?
                  Just (Move d) -> Piece u (p %+ displacement d) r
                  Just (Rotate CW) -> Piece u p ((r + 1) `mod` order u)
                  Just (Rotate CCW) -> Piece u p ((r - 1) `mod` order u)
                piece'' = spawn (head next) board 
                board' = fill cellsList board
                words_used' = foldl (flip S.insert) words_used new_words
                prev_cs' = c:prev_cs
                new_words = filter (`isPrefixOf`prev_cs') reversed
                new_words_score = sum $ map ((2*) . length) $ new_words
                words_bonus = trace ("words bonus: " ++ show (300 * S.size words_used)) $
                              300 * S.size words_used
                -- the following are copied from other places
                cellsList = realize piece
                cellsSet = S.fromList cellsList
                ys = uniq $ sort $ map posY cellsList
                ls = length $ filter full ys
                maxX = posX $ snd $ bounds $ cells board
                full :: Int -> Bool
                full y = all (\p -> (cells board ! p) || (p `S.member` cellsSet)) $
                         map (\x -> Pos x y) [0..maxX]
        reversed = map reverse words
        none = ([], "", "")
