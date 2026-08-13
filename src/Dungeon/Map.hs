{-# LANGUAGE FlexibleContexts #-}

module Dungeon.Map
    ( Position
    , Dungeon
    , TerrainCommand
    , room
    , digX
    , digY
    , water
    , generateDungeon
    , isWalkable
    , isWater
    , buildDijkstra
    ) where

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Array
    ( Array
    , bounds
    , indices
    , inRange
    , listArray
    , (!)
    )
import Data.Array.ST
    ( MArray
    , STArray
    , freeze
    , newArray
    , readArray
    , runSTArray
    , thaw
    , writeArray
    )
import Data.STRef (newSTRef, readSTRef, writeSTRef)

type Position = (Int, Int)

type Dungeon = Array Position Char

data TerrainCommand
    = Room Position Position
    | Tunnel Position Position
    | Water Position

room :: Position -> Position -> TerrainCommand
room = Room

digX :: Position -> Int -> TerrainCommand
digX pos@(x, y) len = Tunnel pos (x + len, y)

digY :: Position -> Int -> TerrainCommand
digY pos@(x, y) len = Tunnel pos (x, y + len)

water :: Position -> TerrainCommand
water = Water

-- | Generate terrain at the requested coordinate scale.
--
-- Room and tunnel endpoints are rounded after scaling. Tunnel lengths are
-- derived from the rounded endpoints, keeping doors aligned with rooms at
-- non-integer scales.
generateDungeon :: Float -> [TerrainCommand] -> Dungeon
generateDungeon scale rawCommands = runSTArray $ do
    ar <- thaw blankMap
    forM_ commands $ \command ->
        case command of
            Room (x1, y1) (x2, y2) -> do
                forM_ [x1 .. x2] (\x -> writeArray ar (x, y1) '#')
                forM_ [x1 .. x2] (\x -> writeArray ar (x, y2) '#')
                forM_ [y1 .. y2] (\y -> writeArray ar (x1, y) '#')
                forM_ [y1 .. y2] (\y -> writeArray ar (x2, y) '#')
                forM_ [y1 + 1 .. y2 - 1] $ \y ->
                    forM_ [x1 + 1 .. x2 - 1] $ \x ->
                        writeArray ar (x, y) ' '
            Tunnel (x1, y1) (x2, y2)
                | y1 == y2 ->
                    digTunnel mapBounds ar (x1, y1) (x2 - x1) id
                | x1 == x2 ->
                    digTunnel
                        mapBounds
                        ar
                        (y1, x1)
                        (y2 - y1)
                        (\(x', y') -> (y', x'))
                | otherwise -> pure ()
            Water pos ->
                writeArray ar pos '~'
    pure ar
    where
        commands = map (scaleCommand scale) rawCommands
        (maxX, maxY) = commandBounds commands
        mapBounds = ((1, 1), (maxX + 1, maxY + 1))
        blankMap = listArray mapBounds (repeat '.')

scaleCommand :: Float -> TerrainCommand -> TerrainCommand
scaleCommand scale command =
    case command of
        Room pos1 pos2 -> Room (scalePosition pos1) (scalePosition pos2)
        Tunnel pos1 pos2 -> Tunnel (scalePosition pos1) (scalePosition pos2)
        Water pos -> Water (scalePosition pos)
    where
        scalePosition (x, y) = (scaleCoordinate x, scaleCoordinate y)
        scaleCoordinate n = round (scale * fromIntegral n)

commandBounds :: [TerrainCommand] -> Position
commandBounds =
    foldl include (1, 1)
    where
        include (maxX, maxY) command =
            case command of
                Room (x1, y1) (x2, y2) ->
                    (maximum [maxX, x1, x2], maximum [maxY, y1, y2])
                Tunnel (x1, y1) (x2, y2) ->
                    (maximum [maxX, x1, x2], maximum [maxY, y1, y2])
                Water _ -> (maxX, maxY)

digTunnel
    :: MArray array Char m
    => (Position, Position)
    -> array Position Char
    -> (Int, Int)
    -> Int
    -> ((Int, Int) -> Position)
    -> m ()
digTunnel mapBounds ar (start, fixed) len toPosition = do
    forM_ [start .. start + len] $ \offset -> do
        let pos = toPosition (offset, fixed)
        writeArray ar pos ' '
        wallAround pos
    when (len > 0) $ do
        openRoom
            (toPosition (start, fixed))
            (toPosition (start + 1, fixed))
        openRoom
            (toPosition (start + len, fixed))
            (toPosition (start + len - 1, fixed))
    where
        cardinal (x, y) =
            [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        around (x, y) =
            [ (x + dx, y + dy)
            | dx <- [-1 .. 1]
            , dy <- [-1 .. 1]
            , (dx, dy) /= (0, 0)
            ]
        wallAround pos =
            forM_ (around pos) $ \neighbor ->
                when (inRange mapBounds neighbor) $ do
                    tile <- readArray ar neighbor
                    when (tile == '.') $ writeArray ar neighbor '#'
        isFloor pos =
            if inRange mapBounds pos
                then (== ' ') <$> readArray ar pos
                else pure False
        hasFloorNeighbor pos corridorPosition =
            or
                <$> forM
                    (filter (/= corridorPosition) (cardinal pos))
                    isFloor
        openRoom pos corridorPosition = do
            connected <- hasFloorNeighbor pos corridorPosition
            unless connected $ do
                doorwayWalls <- filterM (isDoorway pos) (cardinal pos)
                forM_ doorwayWalls $ \wall -> do
                    writeArray ar wall ' '
                    wallAround wall
        isDoorway endpoint wall =
            if not (inRange mapBounds wall)
                then pure False
                else do
                    tile <- readArray ar wall
                    if tile /= '#'
                        then pure False
                        else hasFloorNeighbor wall endpoint

isWalkable :: Dungeon -> Position -> Bool
isWalkable dungeon pos =
    inRange (bounds dungeon) pos
        && dungeon ! pos `elem` [' ', '~']

isWater :: Dungeon -> Position -> Bool
isWater dungeon pos =
    inRange (bounds dungeon) pos && dungeon ! pos == '~'

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
