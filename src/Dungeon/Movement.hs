{-# LANGUAGE Arrows #-}

module Dungeon.Movement (
  screenPos
, playerPos
, dungeonW
, dungeonH
, screenW
, screenH
, startX
, startY
, screenBounds
, Turn(..)
) where

import Control.Auto as A
import Control.Auto.Core
import Control.Auto.Interval
import Control.Auto.Generate
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Prelude hiding ((.))

data Turn = North | South | West | East


dungeonW = 100 :: Int
dungeonH = 50 :: Int

screenW = 40
screenH = 20

padX = 5
padY = 5

startX = 2
startY = 10

boundaryCheck :: (Monad m) => Auto m (Int,Int) Bool
boundaryCheck = arr $ \(x,y) ->
  let check
        | (x >= 1) && (x <= dungeonW) && (y >= 1) && (y <= dungeonH) = True
        | otherwise = False
  in check
     
playerPos :: (MonadFix m) => Auto m Turn (Int,Int)
playerPos = proc turn -> do 
  rec
    (px,py) <- delay (startX, startY) -< (px',py')
    let (px'',py'') = case turn of
                        West -> (px-1,py)
                        East -> (px+1,py)
                        North -> (px,py-1)
                        South -> (px,py+1)
    validTurn <- boundaryCheck -< (px'',py'')
    let (px',py') = if validTurn then (px'',py'') else (px,py)
  returnA -< (px',py')


screenBounds :: (MonadFix m) => Auto m Turn (Int,Int,Int,Int)
screenBounds = proc turn -> do
  rec
    (x1,y1,x2,y2) <- delay (1,1,screenW,screenH) -< (x1', y1', x2', y2')
    (px,py) <- playerPos -< turn
    let dx
          | ((px - x1) <= padX) = (max (px - padX) 1) - x1
          | ((x2 - px) <= padX) = (min dungeonW (padX + px)) - x2
          | otherwise = 0
        dy
          | ((py  - y1) <= padY) =  (max (py - padY) 1) - y1
          | ((y2 - py) <= padY) =  (min dungeonH (padY + py)) - y2
          | otherwise = 0
        (x1', y1', x2', y2') = (x1 + dx, y1 + dy, x2 + dx, y2 + dy)
  returnA -< (x1', y1', x2', y2')

screenPos :: (MonadFix m) => Auto m Turn (Int,Int)
screenPos = proc turn -> do
  ((px,py),(x1,y1,x2,y2)) <- (playerPos &&& screenBounds) -< turn
  returnA -< (px - x1 + 1, py - y1 + 1)
