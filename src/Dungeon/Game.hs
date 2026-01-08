{-# LANGUAGE Arrows #-}
module Dungeon.Game where

import Data.Array
import Control.Arrow
import Control.Monad.Trans.MSF
import Control.Monad.Trans.MSF.Maybe
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore

import Graphics.Vty
import Graphics.Vty.CrossPlatform

import Dungeon.Map
import Dungeon.Interface as DI

testSettings = ViewSettings {
      padX = 5
    , padY = 5
    , screenW = 40
    , screenH = 20
}

testDungeon :: Dungeon
testDungeon = compose $ do
  room (2,2) (10,6)
  room (15,14) (20,30)
  room (40,4) (45,30)
  room (30,24) (40,40)
  digX (20, 20) 10

-- | Game state 

initialState :: (Int, Int)
initialState = (2,10)

-- | the MSF that ties it all together
-- stop stepping through it when it produces Nothing
mainMSF :: Vty -> MSF IO () (Maybe ())
mainMSF vty = 
  let
    dung = testDungeon
    dims = snd $ bounds dung
    output = outputMSF vty dims testSettings
    
    updatePos (Just (Move d)) pos = DI.playerPos dims (Move d) pos
    updatePos _               pos = pos

    trackPlayer = accumulateWith updatePos initialState

    -- The main loop logic
    running = proc _ -> do
        inp <- inputVty vty -< ()
        case inp of
            Just Quit -> returnA -< Nothing 
            _ -> do
                pos <- trackPlayer -< inp 
                output -< (dung, pos)
                returnA -< Just ()

    -- The "bootstrap" frame: draws once, then switches to 'running'
    doFirst = proc _ -> do
        output -< (dung, initialState)
        returnA -< (Just (), Just ()) -- The (output, event) for dSwitch

  in dSwitch doFirst (const running)

-- | Runs the MSF until it returns Nothing
runMainMSF :: MSF IO () (Maybe ()) -> IO ()
runMainMSF msf = do
    (result, nextMSF) <- unMSF msf ()
    case result of
        Just _  -> runMainMSF nextMSF
        Nothing -> return ()
 
runGame :: IO ()
runGame = do
  vty <- mkVty defaultConfig  
  runMainMSF (mainMSF vty)
  shutdown vty
