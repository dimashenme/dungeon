{-# LANGUAGE Arrows #-}

import Control.Auto as A
import Control.Auto.Core
import Control.Auto.Interval
import Control.Arrow
import Control.Monad
import Prelude hiding ((.))

import Data.Array 
import Data.Array.ST

import Graphics.Vty
import Graphics.Vty.Picture
import Data.Default (def)

import Dungeon.Map
import Dungeon.Movement as DM

import Dungeon.Controls


testDungeon :: Level
testDungeon = runSTArray $ do
  a <- thaw $ newLevel 100 50 
  putRoomST (2,2) (10,6) a
  putRoomST (15,14) (20,30) a
  putRoomST (40,4) (45,30) a
  putRoomST (30,24) (40,40) a
  return a

-- view settings

testSettings = ViewSettings {
      DM.padX = 5
    , DM.padY = 5
    , DM.screenW = 40
    , DM.screenH = 20
    , DM.startX = 2
    , DM.startY = 10
  }

initialState = ((startX testSettings,startY testSettings),(1,1,screenW testSettings,screenH testSettings))

-- autos

-- | Game state variables

logic :: Auto IO Turn ((Int, Int), (Int, Int, Int, Int))
logic =
  (screenPos testDungeon testSettings &&& screenBounds testDungeon testSettings)

-- | Render the level and the player
output :: Vty -> Auto IO  ((Int, Int), (Int, Int, Int, Int)) ()
output vty = arrM $ \((px,py),rect) ->  do
  update vty (picForImage (translate 10 5 (renderLevel testDungeon rect)))
  setCursorPos (outputIface vty) (px-1) (py-1)
  showCursor  (outputIface vty)

--- wire everything together

-- | The main loop has rudiment of an mvc architure
-- @logic@ is responsible for updating the game state
-- based on user input
-- @output@ draws
-- @input@ handles keypresses and tells @logic@ what turns to
-- make
-- the main loop feeds @initialState@ to @output@ once
-- then connects the result of $inputVty$ to @logic@
-- unless it's a Nothing
mainAuto :: Vty -> Auto IO a (Maybe ())
mainAuto vty =  
  let doFirst = proc _ -> do
        always <- (output vty) . (pure initialState) -< ()
        onFor 1 -< Just always
        -- because we never quit at the first iteration       
      running = proc _ -> do
        inp <- inputVty vty -< ()
        case inp of
          Just i -> do
            ret <- output vty . logic -< i
            returnA -< Just ret
          Nothing -> do returnA -< Nothing
  in doFirst --> running

-- | The main loop just invokes the main auto
-- above until it return Nothing
mainLoop :: Monad m => Auto m () (Maybe t) -> m ()
mainLoop a  = do
  (result, a') <- stepAuto a ()
  case result of
    Just action -> mainLoop a'
    Nothing -> return ()

main :: IO ()
main = do
  vty <- mkVty def
  showCursor (outputIface vty)
  mainLoop (mainAuto vty)
  shutdown vty
