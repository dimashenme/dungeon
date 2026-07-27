{-# LANGUAGE FlexibleContexts #-}
module Dungeon.Map (
    Dungeon,
    DungeonM,
    room,
    digX,
    digY,
    compose,
    isWalkable,
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

-- | Whether a position can be occupied by a walking entity.
--
-- A carved floor is represented by a space.  Walls, undug terrain, and
-- positions outside the dungeon are not walkable.
isWalkable :: Dungeon -> (Int, Int) -> Bool
isWalkable dungeon position =
    inRange (bounds dungeon) position && dungeon ! position == ' '

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

-- | Dig an inclusive horizontal corridor.  If either endpoint meets a room
-- only at a corner, compose widens the endpoint into a traversable doorway.
digX :: (Int, Int) -> Int -> DungeonM ()
digX p l = tell [DigX p l]

-- | Dig an inclusive vertical corridor.  If either endpoint meets a room only
-- at a corner, compose widens the endpoint into a traversable doorway.
digY :: (Int, Int) -> Int -> DungeonM ()
digY p l = tell [DigY p l]

-- | Make a map from the description at the requested coordinate scale.
--
-- Every room and tunnel endpoint is rounded to the nearest integer after
-- scaling.  Tunnel lengths are derived from their rounded endpoints, keeping
-- doors aligned with the rooms they connect at non-integer scales.
compose :: Float -> DungeonM () -> Dungeon
compose scale dung =
  let
    cmds = map scaleCommand (execWriter dung)
    scaleCoordinate n = round (scale * fromIntegral n)
    scalePoint (x, y) = (scaleCoordinate x, scaleCoordinate y)
    scaleCommand command = case command of
      Room p1 p2 -> Room (scalePoint p1) (scalePoint p2)
      DigX (x, y) len ->
        let x' = scaleCoordinate x
            endX' = scaleCoordinate (x + len)
        in DigX (x', scaleCoordinate y) (endX' - x')
      DigY (x, y) len ->
        let y' = scaleCoordinate y
            endY' = scaleCoordinate (y + len)
        in DigY (scaleCoordinate x, y') (endY' - y')
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
      let cardinalNeighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
          surroundingCoordinates (x, y) =
            [ (x + dx, y + dy)
            | dx <- [-1 .. 1]
            , dy <- [-1 .. 1]
            , (dx, dy) /= (0, 0)
            ]
          wallSurroundings position =
            forM_ (surroundingCoordinates position) $ \neighbor -> do
              when (inRange bnds neighbor) $ do
                val <- readArray a neighbor
                when (val == '.') $ writeArray a neighbor '#'
          isFloor position =
            if inRange bnds position
              then (== ' ') <$> readArray a position
              else return False
          hasFloorNeighbor position corridorNeighbor =
            or <$> forM (filter (/= corridorNeighbor) (cardinalNeighbors position)) isFloor
          opensRoom position corridorNeighbor = do
            connected <- hasFloorNeighbor position corridorNeighbor
            unless connected $ do
              doorwayWalls <- filterM (isDoorway position) (cardinalNeighbors position)
              forM_ doorwayWalls $ \wall -> do
                writeArray a wall ' '
                wallSurroundings wall
          isDoorway endpoint wall =
            if not (inRange bnds wall)
              then return False
              else do
                tile <- readArray a wall
                if tile /= '#'
                  then return False
                  else hasFloorNeighbor wall endpoint
      forM_ [p0 .. p0 + len] $ \p -> do
        let crd = f (p, s0) 
        writeArray a crd ' '
        wallSurroundings crd
      when (len > 0) $ do
        opensRoom (f (p0, s0)) (f (p0 + 1, s0))
        opensRoom (f (p0 + len, s0)) (f (p0 + len - 1, s0))
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
