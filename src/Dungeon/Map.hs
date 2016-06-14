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
) where
       
import Control.Monad.ST
import Control.Monad

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

