{-# LANGUAGE FlexibleContexts #-}
module Dungeon.Map (
    Dungeon,
    DungeonM,
    room,
    digX,
    digY,
    compose,
    buildDijkstra
) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Writer
import Data.Array
import Data.Array.ST
import Data.STRef
import Graphics.Vty
import Data.Default (def)

-- | Map of a dungeon 
type Dungeon = Array (Int,Int) Char

-------------------------------------------------------------------------------
-- DungeonM 
-------------------------------------------------------------------------------

data Command 
    = Room (Int, Int) (Int, Int) 
    | DigX (Int, Int) Int 
    | DigY (Int, Int) Int

-- | Monad to describe a dungeon
type DungeonM = Writer [Command]

room :: (Int, Int) -> (Int, Int) -> DungeonM ()
room p1 p2 = tell [Room p1 p2]

digX :: (Int, Int) -> Int -> DungeonM ()
digX p l = tell [DigX p l]

digY :: (Int, Int) -> Int -> DungeonM ()
digY p l = tell [DigY p l]

-- | Make a map from the description
compose :: DungeonM () -> Dungeon
compose dung = 
  let
    cmds = execWriter dung    
    -- Calculate bounds from the description
    (maxW, maxH) = foldl (
      \(mx, my) cmd -> case cmd of
        Room (x1,y1) (x2,y2) -> (maximum [mx, x1, x2], maximum [my, y1, y2])
        DigX (x,y) l         -> (max mx (x+l), max my y)
        DigY (x,y) l         -> (max mx x,     max my (y+l))
      ) (1, 1) cmds
    
    -- Create blank map
    bnds = ((1,1),(maxW + 1, maxH + 1))
    digTunnel :: (MArray a Char m ) => a (Int, Int) Char -> (Int, Int) -> Int -> ((Int, Int) -> (Int, Int)) -> m ()
    digTunnel a (p0, s0) len f = do
      forM_ [p0 .. p0 + len] $ \p -> do
        let crd = f (p, s0) 
        writeArray a crd ' '
        forM_ [s0 - 1, s0 + 1] $ \s -> do
          let sideCoord = f (p, s)
          when (inRange bnds sideCoord) $ do
            val <- readArray a sideCoord
            when (val == '.') $ writeArray a sideCoord '#'        
    blankMap = listArray bnds (repeat '.')
  in runSTArray $ do
    ar <- thaw $ blankMap :: ST s (STArray s (Int, Int) Char)
    forM_ cmds $ \cmd -> case cmd of
        Room (x1, y1) (x2, y2) -> do
            forM_ [x1..x2] (\x -> writeArray ar (x, y1) '#')
            forM_ [x1..x2] (\x -> writeArray ar (x, y2) '#')
            forM_ [y1..y2] (\y -> writeArray ar (x1, y) '#')
            forM_ [y1..y2] (\y -> writeArray ar (x2, y) '#')
            forM_ [y1+1..y2-1] (\y -> 
                forM_ [x1+1..x2-1] (\x -> writeArray ar (x, y) ' '))

        DigX (x0, y0) len -> digTunnel ar (x0,y0) len id

        DigY (x0, y0) len -> digTunnel ar (y0,x0) len  (\(x,y) -> (y,x))
        
    return ar
   

-------------------------------------------------------------------------------
-- build Dijkstra map for a given list of goals
-------------------------------------------------------------------------------

type DijkstraGrid = Array (Int, Int) Int

buildDijkstra :: [(Int, Int)] -> Dungeon -> DijkstraGrid
buildDijkstra goals level = runST $ do
    let bnds = bounds level
    dist <- newArray bnds 999 :: ST s (STArray s (Int, Int) Int)
    
    forM_ goals $ \pos -> writeArray dist pos 0
    
    let loop = do
            changed <- newSTRef False
            forM_ (indices level) $ \curr@(y, x) -> do
                val <- readArray dist curr
                -- Process floor cells only
                when (level ! curr == '.') $ do
                    let neighbors = [(y-1, x), (y+1, x), (y, x-1), (y, x+1)]
                    forM_ neighbors $ \nb -> do
                        when (inRange bnds nb && level ! nb == '.') $ do
                            nbVal <- readArray dist nb
                            when (val > nbVal + 1) $ do
                                writeArray dist curr (nbVal + 1)
                                writeSTRef changed True
            c <- readSTRef changed
            if c then loop else return ()
    loop
    freeze dist
