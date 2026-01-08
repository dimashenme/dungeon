{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Arrows #-}

import Control.Arrow
import Control.Monad
import Prelude hiding ((.))

import Data.Array 
import Data.Array.ST

import Graphics.Vty
import Graphics.Vty.Picture
import Data.Default (def)

import Dungeon.Game

main :: IO ()
main = do
  Dungeon.Game.runGame
