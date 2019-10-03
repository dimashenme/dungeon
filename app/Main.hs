{-# LANGUAGE BangPatterns #-}
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
import Dungeon.Iface



testSettings = ViewSettings {
      padX = 5
    , padY = 5
    , screenW = 40
    , screenH = 20
  }

testDungeon :: Level
testDungeon = runSTArray $ do
  a <- thaw $ newLevel 100 50 
  putRoomST (2,2) (10,6) a
  putRoomST (15,14) (20,30) a
  putRoomST (40,4) (45,30) a
  putRoomST (30,24) (40,40) a
  return a

-- | Game state 

initialState :: (Int, Int)
initialState = (2,10)


--- wire everything together

-- |  Rudimentary mvc architure
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
  let
    lvl = testDungeon
    dims = snd $ bounds lvl
    logic :: Auto IO Turn (Int, Int)
    logic = A.accum (DM.playerPos dims) initialState
    output = outputAuto vty dims testSettings 
    doFirst = proc _ -> do
        always <- output . (pure (lvl, initialState)) -< ()
        onFor 1 -< Just always
        -- because we never quit at the first iteration       
    running = proc _ -> do
        inp <- inputVty vty -< ()
        case inp of
          Just i -> do
            player <- logic -< i
            ret <- output -< (lvl, player) 
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
  cfg <- standardIOConfig
  vty <- mkVty cfg
  showCursor (outputIface vty)
  mainLoop (mainAuto vty)
  shutdown vty
