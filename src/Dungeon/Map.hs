{-|
Module      : Map
Description : Dungeon level map management
Copyright   : (c) Dmitry Sustretov, 2016
License     : GPL-3
Maintainer  : dmitri83@hcoop.net
Stability   : experimental
Portability : POSIX

This module contains functions for creating maps of @dungeon levels@.
-}
module Dungeon.Map (
    putRoomST
  , putRoom
  , newDungeon
  , DungeonLevel
  , renderLevel
) where
       
import Control.Monad.ST
import Control.Monad

import Graphics.Vty
import Graphics.Vty.Picture
import Data.Default (def)

import Data.Array
import Data.Array.ST

-- | Helper function will simply print out the dungeon
printOut :: Array (Int, Int) Char -> IO ()
printOut a =
  printL (elems a)
  where ((_,_),(h,w)) = bounds a
        printL [] = return ()
        printL l = let (line, rest) = splitAt w l
                   in do putStrLn line
                         printL rest

-- | An ST action that draws a room on the dungeon map.
putRoomST :: (Int, Int) -> (Int,Int) -> STArray s (Int, Int) Char -> ST s (STArray s (Int, Int) Char)
putRoomST (a,b) (c,d) ar = do
  forM_ [a..c] (\x -> writeArray ar (b,x) '#')
  forM_ [a..c] (\x -> writeArray ar (d,x) '#')
  forM_ [b..d] (\x -> writeArray ar (x,a) '#')
  forM_ [b..d] (\x -> writeArray ar (x,c) '#')
  forM_ [a+1..c-1] (\x -> forM_ [b+1..d-1] (\y -> writeArray ar (y,x) '.'))
  return ar

-- | Return a new dungeon map with new room drawn on it.
putRoom :: (Int, Int) -> (Int,Int) -> Array (Int, Int) Char -> Array (Int, Int) Char
putRoom (a,b) (c,d) ar = runSTArray $ do
    ar' <- thaw ar
    putRoomST (a,b) (c,d) ar'
    return ar'

-- | A dungeon level map
data DungeonLevel = Array (Int,Int) Char

-- | Create a blank dungeon level.
newDungeon :: Int -> Int -> Array (Int, Int) Char
newDungeon w h = listArray ((1,1),(w,h)) (take (w*h) [' ',' '..])


-- screen output

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs

-- | return a list of rows of a rectangle in an array
-- @x1 y1 x2 y2@ -- coordinates of the rectangle
-- @w@ - row length
-- @a@ - the array
subRect :: (Int,Int,Int,Int) -> Int -> [a] -> [[a]]
subRect (x1,y1,x2,y2)  w a =
  take (y2 - y1 + 1) . drop (y1-1) $ fmap (take (x2 - x1 + 1) . drop (x1-1)) aa
  where aa = chunk w a

-- | make a vty image of a rectangle in a dungeon on a given Vty
-- @level@ - the dungeon level (array of chars)
-- @rect$ - the coordinates of the rectangle to print
renderLevel
  :: (Num t, Ix t) =>
     Array (t, Int) Char -> (Int,Int,Int,Int) -> Image
renderLevel level rect =
    vertCat (fmap (string defAttr) (subRect rect w (elems level)))
    where w = bx2 - bx1 + 1
          h = by2 - by1 + 1          
          ((by1,bx1),(by2,bx2)) = bounds level

