{-# LANGUAGE Arrows #-}

import Control.Auto as A
import Control.Auto.Core
import Control.Auto.Interval
import Control.Auto.Generate
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Prelude hiding ((.))

import Data.Array 
import Data.Array.ST

import Graphics.Vty
import Graphics.Vty.Picture
import Data.Default (def)

import Dungeon.Map
import Dungeon.Movement as DM

import Dungeon.Controls


testDungeon :: Array (Int,Int) Char
testDungeon = runSTArray $ do
  a <- thaw $ newDungeon DM.dungeonH DM.dungeonW
  putRoomST (2,2) (10,6) a
  putRoomST (15,14) (20,30) a
  putRoomST (40,4) (45,30) a
  putRoomST (30,24) (40,40) a
  return a

-- game logic

initialState = ((DM.startX,DM.startY),(1,1,DM.screenW,DM.screenH))

logicAuto
  :: Interval IO Turn ((Int, Int), (Int, Int, Int, Int))
logicAuto = toOn . (screenPos &&& screenBounds)

-- | draw the dungeon and the player
output :: Vty -> (Int,Int) -> (Int, Int, Int, Int) -> IO ()
output vty (px,py) rect =  do
  update vty (picForImage (renderLevel testDungeon rect))
  setCursorPos (outputIface vty) (px-1) (py-1)
  showCursor  (outputIface vty)

outputAuto
  :: Vty -> Interval IO  ((Int, Int), (Int, Int, Int, Int)) ()
outputAuto vty = toOn . (arrM $ uncurry $ output vty)

--- wire everything together

-- | All constituent autos are intervals
-- and must be composed with compI
mainAuto :: Vty -> Auto IO a (Maybe ())
mainAuto vty =  (outputAuto vty) `compI` skipFirstInput where
  skipFirstInput = (onFor 1) . (pure $ Just initialState)
       -->  (logicAuto `compI` (inputVty vty))

--- main loop is simple

mainLoop :: Monad m => Auto m () (Maybe t) -> m ()
mainLoop a  = do
  (result, a') <- stepAuto a ()
  case result of
    Just action -> mainLoop a'
    Nothing -> return ()

test =  evalAuto playerPos DM.South :: IO (Int,Int)
  
test2 = streamAuto' ((toOn . screenBounds) `compI` (arr parseInput)) "llllllllll"

test3 = streamAuto' ((toOn . playerPos) `compI` (arr parseInput)) "hhhhhhhhhhhhllll"

main = do
  vty <- mkVty def
  showCursor (outputIface vty)
  mainLoop (mainAuto vty)
  shutdown vty
