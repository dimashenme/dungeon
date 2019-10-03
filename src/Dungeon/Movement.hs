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
, Turn(..)
) where

import Data.Array

import Control.Auto 
import Control.Auto.Core as A
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


     
-- | update player position
-- @(w,h)@ - level width and height
-- @(px,py)@ - player coordinates
-- @turn@ - turn to make

playerPos :: 
  (Int, Int) ->
  (Int, Int) ->
  Turn ->
  (Int,Int)
playerPos (w,h) (px,py) turn =
    let
      bdCheck x y 
        | (x >= 1) && (x <= w) && (y >= 1) && (y <= h) = True
        | otherwise = False
      (px'',py'') = case turn of
                        West -> (px-1,py)
                        East -> (px+1,py)
                        North -> (px,py-1)
                        South -> (px,py+1)    
      validTurn = bdCheck px'' py'' 
      (px',py') = if validTurn then (px'',py'') else (px,py)
    in  (px',py')

