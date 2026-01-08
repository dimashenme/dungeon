{-# LANGUAGE Arrows #-}
module Dungeon.Interface (
    Turn(..),
    ViewSettings(..),
    playerPos,
    inputVty,
    outputMSF
    
) where

import Control.Arrow
import Control.Monad.Trans.MSF
import Data.MonadicStreamFunction.Core
import Data.MonadicStreamFunction.Util
import Graphics.Vty
import Graphics.Vty.Picture
import Data.Default (def)
import Data.Array
import Dungeon.Map

data Direction = North | South | West | East deriving (Show, Eq)
data Turn = Move Direction | Quit deriving (Show, Eq)

data ViewSettings = ViewSettings { 
    screenW :: Int, screenH :: Int, padX :: Int, padY :: Int 
} deriving Show

inputVty :: Vty -> MStream IO (Maybe Turn)
inputVty vty = arrM $ \_ -> do
    evt <- nextEvent vty
    case evt of
        EvKey (KChar c)   [] -> return $ parseInput c
        _                    -> return Nothing 
  where
    parseInput 'h' = Just (Move West) 
    parseInput 'j' = Just (Move South)
    parseInput 'k' = Just (Move North)
    parseInput 'l' = Just (Move East)
    parseInput 'q' = Just Quit
    parseInput _   = Nothing           -- Returns Nothing for unknown keys

playerPos :: (Int, Int) -> Turn -> (Int, Int) -> (Int, Int)
playerPos (w, h) (Move dir) (px, py) = --
    let (px', py') = case dir of
            West  -> (px-1, py)
            East  -> (px+1, py)
            North -> (px, py-1)
            South -> (px, py+1)
        inBounds x y = x >= 1 && x <= w && y >= 1 && y <= h
    in if inBounds px' py' then (px', py') else (px, py)
playerPos _ Quit pos = pos --

-- | Render the level and the player
-- @vty@ - output to this vty
-- @dims@ - dimensions of the level
-- @vs@ - viewport settings
outputMSF :: Vty -> (Int, Int) -> ViewSettings -> MSF IO (Dungeon, (Int, Int)) ()
outputMSF vty dims@(lw, lh) vs = proc (level, (px, py)) -> do
    vp <- accumulateWith updateViewport initalVP -< (px, py)
    arrM render -< (level, (px, py), vp)
  where
    initalVP = (1, 1, screenW vs, screenH vs)
    updateViewport (px', py') vp@(x1, y1, x2, y2) =
      let dx = (min (px' - x1 - padX vs) 0) + (max (padX vs - x2 + px') 0)
          dy = (min (py' - y1 - padY vs) 0) + (max (padY vs - y2 + py') 0)            
          shift (sx1, sy1, sx2, sy2) (sdx, sdy) =
            if (sx1 + sdx >= 1) && (sx2 + sdx <= lw)
               && (sy1 + sdy >= 1) && (sy2 + sdy <= lh)
            then (sx1 + sdx, sy1 + sdy, sx2 + sdx, sy2 + sdy) 
            else (sx1, sy1, sx2, sy2)
      in if dx /= 0 || dy /= 0 then shift vp (dx, dy) else vp    

    render (lvl, (curPx, curPy), v@(x1, y1, x2, y2) ) = do
        update vty (picForLayers
                    [ translate 10 5 drawRect
                    , string defAttr ("Player pos: " ++ show (curPx, curPy))
                    ])
        setCursorPos (outputIface vty) (curPx - x1 + 10) (curPy - y1 + 5)
        showCursor (outputIface vty)
      where
        drawRect =
            vertCat [ string defAttr [ lvl ! (x, y) | x <- [x1 .. x2] ] 
                    | y <- [y1 .. y2] ]
