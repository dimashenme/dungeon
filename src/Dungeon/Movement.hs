{-# LANGUAGE Arrows #-}
{-|
Module      : Movement
Description : Movement in the dugneon
Copyright   : (c) Dmitry Sustretov, 2016
License     : GPL-3
Maintainer  : dmitri83@hcoop.net
Stability   : experimental
Portability : POSIX

This module contains autos that handle player movement in the dungeon. 
-}

module Dungeon.Movement (
  screenPos
, playerPos
, screenBounds
, Turn(..)
, ViewSettings(..)
) where

import Data.Array

import Control.Auto as A
import Control.Auto.Core
import Control.Auto.Interval
import Control.Auto.Generate
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Prelude hiding ((.))

import Dungeon.Map

-- | possible turns that a player can take
data Turn = North | South | West | East

data ViewSettings = ViewSettings {
    padX :: Int
  , padY :: Int
  , screenW :: Int
  , screenH :: Int
  ,  startX :: Int
  ,  startY :: Int
}  deriving Show


-- | check if given coordinates are within the dungeon
boundaryCheck :: (Monad m) => Level -> Auto m (Int,Int) Bool
boundaryCheck level  = arr $ \(x,y) ->
  let check
        | (x >= 1) && (x <= w) && (y >= 1) && (y <= h) = True
        | otherwise = False
      ((_,_),(w,h)) = bounds level
  in check
     
-- | an auto that tracks player position after a given turn has
-- been made
playerPos :: (MonadFix m) => Level -> ViewSettings -> Auto m Turn (Int,Int)
playerPos level s = proc turn -> do 
  rec
    (px,py) <- delay (startX s, startY s) -< (px',py')
    let (px'',py'') = case turn of
                        West -> (px-1,py)
                        East -> (px+1,py)
                        North -> (px,py-1)
                        South -> (px,py+1)
    validTurn <- boundaryCheck level -< (px'',py'')
    let (px',py') = if validTurn then (px'',py'') else (px,py)
  returnA -< (px',py')

-- | an auto that tracks the coordinates of the piece of map
-- that is currently drawn on the screen
screenBounds :: (MonadFix m) => Level -> ViewSettings -> Auto m Turn (Int,Int,Int,Int) 
screenBounds level s = proc turn -> do
  rec
    let ((_,_),(w, h)) = bounds level
    (x1,y1,x2,y2) <- delay (1,1, (screenW s), (screenH s)) -< (x1', y1', x2', y2')
    (px,py) <- playerPos level s -< turn
    let dx
          | ((px - x1) <= (padX s)) = (max (px - (padX s)) 1) - x1
          | ((x2 - px) <= (padX s)) = (min w ((padX s) + px)) - x2
          | otherwise = 0
        dy
          | ((py  - y1) <= (padY s)) =  (max (py - (padY s)) 1) - y1
          | ((y2 - py) <= (padY s)) =  (min h ((padY s) + py)) - y2
          | otherwise = 0
        (x1', y1', x2', y2') = (x1 + dx, y1 + dy, x2 + dx, y2 + dy)
  returnA -< (x1', y1', x2', y2')

-- | An auto that tracks the coordinates of the player as she
-- is drawn on the screen
screenPos :: (MonadFix m) => Level -> ViewSettings -> Auto m Turn (Int,Int)
screenPos level  s = proc turn -> do
  ((px,py),(x1,y1,x2,y2)) <- (playerPos level s &&& screenBounds level s) -< turn
  returnA -< (px - x1 + 1, py - y1 + 1)
