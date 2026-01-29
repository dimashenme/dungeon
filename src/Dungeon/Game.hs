{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}

module Dungeon.Game where

import Data.Array
import Control.Arrow
import Control.Monad (guard, mzero, void)
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Trans.MSF
import Control.Monad.Trans.MSF.Except
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.Util

import Graphics.Vty
import Graphics.Vty.CrossPlatform

import Dungeon.Map
import Dungeon.Interface as DI
import Dungeon.Logic
import Dungeon.Combinators

type GameFx = ReaderT (DI.Config, GameState) IO

instance HasVty (DI.Config, GameState) where
    getVty = cfgVty . fst
    getScreenDims = cfgScreenDims . fst
    getPadding = cfgPadding . fst

instance HasInitState (DI.Config, GameState) where
    initDungeon = stDungeon . snd
    initPlayerPos = stPlayerPos . snd

startPos :: (Int, Int)
startPos = (2,10)

testDungeon :: Dungeon
testDungeon = compose $ do
    room (2,2) (10,6)
    room (15,14) (20,30)
    room (40,4) (45,30)
    room (30,24) (40,40)
    digX (20, 20) 10

-- | An MSF that terminates the computation if the input is Just DI.Quit.
terminateOnQuit :: MSF (MaybeT GameFx) (Maybe DI.Turn) ()
terminateOnQuit = arrM (\u -> if u == Just DI.Quit then mzero else return ())

-- | The MSF that ties it all together, running in the `MaybeT GameFx` monad.
mainMSF :: GameState -> MSF (ExceptT () GameFx) () ()
mainMSF initState = runMSFExcept $ do
    try $ doOnce (DI.outputVty <<< arr (const initState))
    try $ proc () -> do
      rec
        gv <- iPre initState -< newGV
        mbTurn <-  DI.inputVty -< ()
        newGV <- case mbTurn of
                   Just DI.Quit -> throw () -< ()
                   Just turn -> gameView -< turn
                   Nothing -> returnA -< gv   
        DI.outputVty -< newGV
      returnA -< ()

-- | Runs the game by reactimating the main MSF until it terminates.
runGame :: IO ()
runGame = do
  vty <- mkVty defaultConfig
  let test = GameState { stPlayerPos = startPos, stDungeon = testDungeon }
  let defCfg = DI.Config { cfgVty = vty
                         , cfgScreenDims = (40, 20)
                         , cfgPadding = (5, 5)
                         }
  void $ runReaderT (reactimateExcept (try $ mainMSF test)) (defCfg, test)
  shutdown vty

