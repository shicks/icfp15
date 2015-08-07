{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}

module Board where

import Debug.Trace ( trace )
import Control.Applicative ( (<$>), (<*>) )
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Data.Array ( Array,
                    accum, array, assocs, bounds, elems, indices, ixmap, listArray, 
                    (!), (//) )
import qualified Data.ByteString.Lazy as B
import Data.Ix ( Ix(..) )
import Data.List ( intercalate )

data Pos = Pos { posX :: Int, posY :: Int }
         deriving ( Eq )

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

data Board = Board { cells :: Array Pos Bool
                   }

data Dir = E | W | SE | SW

empty :: Int -> Int -> Board
empty width height = Board $ array (start, end) elems
  where start = Pos 0 0
        end = Pos (width - 1) (height - 1)
        elems :: [(Pos, Bool)]
        elems = map (,False) $ range (start, end)

instance Show Board where
  show (Board c) = show' False $ map snd $ assocs c  -- for some reason, elems omits stuff
    where width = 1 + (posX $ snd $ bounds c)
          show' _ [] = ""
          show' offset cs = (if offset then (' ':) else id) $ row cs $
                            show' (not offset) (drop width cs)
          row [] _ = ""
          row cs rest = '|':row' (take width cs) rest
          row' [] rest = '|':'\n':rest
          row' (c:cs) rest = s:' ':row' cs rest
            where s = if c then '*' else ' '

fill :: [Pos] -> Board -> Board
fill ps (Board cs) = Board $ accum (const id) cs $ map (\p -> (p, True)) ps

instance FromJSON Board where
  parseJSON (Object v) = board <$> (v .: "width") <*> (v .: "height") <*> (v .: "filled")
    where board w h f = fill f $ empty w h
  parseJSON v = fail $ "Bad Board: " ++ show v

data CLI = CLI { files :: [String]
               , time  :: Int
               , mem   :: Int
               , power :: [String] }

readBoard :: String -> IO Board
readBoard f = do json <- B.readFile f
                 Just b <- return $ decode json
                 return b
