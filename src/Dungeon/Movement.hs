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
  playerPos
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
boundaryCheck ::  Int -> Int -> Level  -> Bool
boundaryCheck x y level = 
  let ((_, _), (w, h)) = bounds level
      check
        | (x >= 1) && (x <= w) && (y >= 1) && (y <= h) = True
        | otherwise = False
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
    let validTurn = boundaryCheck px'' py'' level
        (px',py') = if validTurn then (px'',py'') else (px,py)
  returnA -< (px',py')

-- | an auto that tracks the coordinates of the piece of map
-- that is currently drawn on the screen
screenBounds :: (MonadFix m) => Level -> ViewSettings -> Auto m Turn (Int,Int,Int,Int) 
screenBounds level s =
    let ((_,_),(w, h)) = bounds level
        shiftX (x1,y1,x2,y2) i = if (x1+i >= 1) && (x2 + i <= w)
                                 then (x1+i,y1,x2+i,y2) else (x1,y1,x2,y2)
        shiftY (x1,y1,x2,y2) i = if (y1+i >= 1) && (y2+i <= h) 
                                 then(x1,y1+i,x2,y2+i) else (x1,y1,x2,y2)
    in proc turn -> do
      rec
        bds <- delay (1,1, (screenW s), (screenH s)) -< bds'
        (px,py) <- playerPos level s -< turn
        let (x1,y1,x2,y2) = bds
        let bds' = case turn of
                     West -> if ((px - x1) <= (padX s))
                             then (shiftX bds (-1)) else bds
                     East -> if  ((x2 - px) <= (padX s))
                             then (shiftX bds 1) else bds
                     North -> if ((py - y1) <= (padY s)) 
                              then (shiftY bds (-1)) else bds
                     South ->  if ((y2 - py) <= (padY s))
                               then (shiftY bds 1) else bds                
      returnA -< bds'

