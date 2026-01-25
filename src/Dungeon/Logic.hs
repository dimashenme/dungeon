module Dungeon.Logic where

import Data.MonadicStreamFunction
import Dungeon.Interface (Turn(..), Direction(..))

initialState :: (Int, Int)
initialState = (2,10)

updatePos :: (Int, Int) -> Maybe Turn -> (Int, Int) -> (Int, Int)
updatePos dims@(w, h) (Just (Move dir)) currentPos =
    let (px, py) = currentPos
        (px', py') = case dir of
            West  -> (px-1, py)
            East  -> (px+1, py)
            North -> (px, py-1)
            South -> (px, py+1)
        inBounds x y = x >= 1 && x <= w && y >= 1 && y <= h
    in if inBounds px' py' then (px', py') else currentPos
updatePos _ _ currentPos = currentPos

playerPos :: (Int, Int) -> MSF IO (Maybe Turn) (Int, Int)
playerPos dims = accumulateWith (updatePos dims) initialState
