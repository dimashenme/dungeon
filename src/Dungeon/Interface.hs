{-# LANGUAGE Arrows #-}
module Dungeon.Interface (
    Turn(..),
    Direction(..),
    ViewSettings(..),
    inputVty,
    outputVty
) where

import Control.Arrow
import Data.MonadicStreamFunction
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

inputVty :: Vty -> MSF IO () (Maybe Turn)
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

-- | An MSF that renders the game state to the vty.
outputVty :: Vty -> (Int, Int) -> ViewSettings -> MSF IO (Dungeon, (Int, Int)) ()
outputVty vty dims vs = proc (dung, playerPos) -> do
    vp <- viewport dims vs -< playerPos
    arrM (\(d, p, v) -> render vty d p v) -< (dung, playerPos, vp)

-- | An MSF that calculates the viewport based on the player's position.
viewport :: (Int, Int) -> ViewSettings -> MSF IO (Int, Int) (Int, Int, Int, Int)
viewport dims@(lw, lh) vs = accumulateWith (updateViewport vs dims) initialVP
  where
    initialVP = (1, 1, screenW vs, screenH vs)

-- | Pure function to update the viewport.
updateViewport :: ViewSettings -> (Int, Int) -> (Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
updateViewport vs (lw, lh) (px', py') vp@(x1, y1, x2, y2) =
  let dx = min (px' - x1 - padX vs) 0 + max (padX vs - x2 + px') 0
      dy = min (py' - y1 - padY vs) 0 + max (padY vs - y2 + py') 0
      shift (sx1, sy1, sx2, sy2) (sdx, sdy) =
        if (sx1 + sdx >= 1) && (sx2 + sdx <= lw)
           && (sy1 + sdy >= 1) && (sy2 + sdy <= lh)
        then (sx1 + sdx, sy1 + sdy, sx2 + sdx, sy2 + sdy)
        else (sx1, sy1, sx2, sy2)
  in if dx /= 0 || dy /= 0 then shift vp (dx, dy) else vp

-- | Renders the game state to the Vty terminal.
render :: Vty -> Dungeon -> (Int, Int) -> (Int, Int, Int, Int) -> IO ()
render vty lvl (curPx, curPy) (x1, y1, x2, y2) = do
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
