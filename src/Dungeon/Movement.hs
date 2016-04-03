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
, deltaScreen
, screenBounds
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


dungeonW = 100 :: Int
dungeonH = 50 :: Int

screenW = 40
screenH = 20

padX = 5
padY = 5

startX = 10
startY = 10

deltaPlayer :: (Monad m) => Auto m Char  (Int, Int)
deltaPlayer = arr $ \x -> case x of
   'h' -> (-1,0)
   'j' -> (0,1)
   'k' -> (0,-1)
   'l' -> (1,0)
   _   -> (0,0)

sumPair (a,b) (c,d) = (a+c,b+d)

playerPos' startx starty = (A.accum sumPair (startx,starty)) .  deltaPlayer

playerPos :: (Monad m) => Auto m Char (Int,Int)
playerPos = playerPos' startX startY

deltaScreen :: (Monad m) => Auto m (Char, Int, Int, Int, Int) (Int,Int)
deltaScreen = proc (inp, x1, y1, x2, y2) ->  do
  (playerx,playery) <- playerPos -< inp
  let deltax = if ((playerx  - x1) <= padX) 
               then playerx - x1 - padX 
               else if ((x2 - playerx) <= padX)
                    then padX + playerx - x2 
                    else 0
      deltay = if ((playery  - y1) <= padY)
               then playery - y1 - padY
               else if ((y2 - playery) <= padY)
                    then padY + playery - y2
                    else 0
    in returnA -< (deltax,deltay)

sumPQ (a,b,c,d) (x,y) = (a+x,b+y,c+x,d+y) 

screenBounds :: (MonadFix m) => Auto m Char (Int,Int,Int,Int)
screenBounds = proc inp -> do
  rec
    (x1,y1,x2,y2) <- lastVal (1,1,screenW,screenH) -< ret
    ret <- (A.accum sumPQ (1,1,screenW,screenH)) . deltaScreen -< (inp,x1,y1,x2,y2)
  returnA -< ret

screenPos :: (MonadFix m) => Auto m Char (Int,Int)
screenPos = proc inp -> do
  ((px,py),(x1,y1,x2,y2)) <- (playerPos &&& screenBounds) -< inp
  returnA -< (px - x1 + 1, py - y1 + 1)
