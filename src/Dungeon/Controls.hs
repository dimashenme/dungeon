{-# LANGUAGE Arrows #-}
{-|
Module      : Controls
Description : Game controls
Copyright   : (c) Dmitry Sustretov, 2016
License     : GPL-3
Maintainer  : dmitri83@hcoop.net
Stability   : experimental
Portability : POSIX

Game controls
-}
module Dungeon.Controls (
    inputVty
  , inputList
  , parseInput
) where

import Control.Auto
import Control.Auto.Interval
import Control.Auto.Generate

import Dungeon.Movement as DM
import Graphics.Vty

-- | turn chars into turns
parseInput :: Char -> Maybe Turn
parseInput a 
  | a == 'h' = Just DM.West
  | a == 'j' = Just DM.South
  | a == 'k' = Just DM.North
  | a == 'l' = Just DM.East
  | otherwise = Nothing

-- | input auto that reads keypresses from a list
-- Nothing means quit.
inputList :: (Monad m) => [Char] -> Interval m a Turn
inputList lst =  (arr parseInput) `compI` unlessI (== 'q') `compI` fromList_ lst

-- | input auto that uses vty to get kepresses
-- Nothing means quit.
inputVty :: Vty -> Interval IO a Turn
inputVty vty = (arr parseInput) `compI` (inputVty' vty)
inputVty' vty = compI (unlessI (== 'q')) $ effect $ do
  evt <- nextEvent vty
  case evt of
    EvKey (KChar a) [] -> return $ Just a
    _ -> return Nothing
