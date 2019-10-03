{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Arrows #-}
{-|
Module      : Iface
Description : 
Copyright   : (c) Dmitry Sustretov, 2016
License     : GPL-3
Maintainer  : dmitri83@hcoop.net
Stability   : experimental
Portability : POSIX

Rendering of the interface
-}
module Dungeon.Iface (
   ViewSettings(..)
 , outputAuto
) where

import Control.Arrow
import Control.Auto as A
import Control.Monad

import Data.Array

import Dungeon.Map

import Graphics.Vty
import Graphics.Vty.Picture
import Data.Default (def)




data ViewSettings = ViewSettings {
    screenW :: Int
  , screenH :: Int
  ,    padX :: Int -- | if the player is padX away from the viewPort boundary, the boundary shifts horizontally
  ,    padY :: Int -- | if the player is padY away from the viewPort boundary, the boundary shifts vertically
}  deriving Show 


-- | an auto that tracks the coordinates of the piece of map
-- that is currently drawn on the screen
-- @(lw,lh)@ - level dimensions
-- @vs@ - view settings
-- @vp@ - current viewport
-- @(px',py')@ - new player coordinates
viewport :: 
  (Int, Int) ->
  ViewSettings -> 
  (Int,Int,Int,Int) -> 
  (Int, Int) ->  
  (Int,Int,Int,Int)
viewport (lw,lh) vs vp (px',py') = 
    let shift (x1,y1,x2,y2) (dx,dy) =
          if (x1+dx >= 1) && (x2 + dx <= lw)
             && (y1+dy >=1) && (y2+dy <= lh)
          then (x1+dx,y1+dy,x2+dx,y2+dy) else (x1,y1,x2,y2)
        (x1,y1,x2,y2) = vp
        dx = (min (px' - x1 - (padX vs)) 0)
             + (max ((padX vs) - x2 + px') 0)
        dy = (min (py' - y1 - (padY vs)) 0)
             + (max ((padY vs) - y2 + py') 0)
    in if (dx /= 0) || (dy /= 0)               
       then (shift vp (dx,dy)) else vp

    
-- | Render the level and the player
-- @vty@ - output to this vty
-- @dims@ - dimensions of the level
-- @vs@ - viewport settings
outputAuto ::
  Vty ->
  (Int, Int) ->
  ViewSettings -> 
  Auto IO  (Level, (Int, Int)) ()
outputAuto vty dims vs =
  let vpAuto =  A.accum (viewport dims vs) (1, 1,(screenW vs), (screenH vs)) 
      o = arrM $ \(level, (px,py), vp) ->  do
        update vty (picForLayers
                    [(translate 10 5 (renderLevel level vp))
                    , string defAttr ("Player pos: " ++ (show (px,py)) ++ "\n" ++ "Viewport: " ++ (show $! vp))])
        let (x1,y1,_,_) = vp in 
          setCursorPos (outputIface vty) (px-x1-1+10) (py-y1-1+5)
        showCursor  (outputIface vty)
  in proc (level, player) -> do
    vp <- vpAuto  -< player
    nthng <- o -< (level, player, vp)
    returnA -< nthng
