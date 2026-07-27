{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}

module Dungeon.Game where

import Data.Array
import Control.Arrow
import Control.Monad (forM_, guard, mzero, void)
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
startPos = (6,6)

testDungeonRooms :: [((Int, Int), (Int, Int))]
testDungeonRooms =
  [ ((2, 2), (11, 9)), ((18, 5), (29, 14)), ((37, 3), (47, 11))
  , ((56, 7), (68, 15)), ((78, 4), (91, 12)), ((5, 23), (16, 31))
  , ((24, 20), (34, 29)), ((43, 24), (55, 33)), ((64, 21), (75, 30))
  , ((84, 25), (96, 34)), ((2, 43), (14, 52)), ((21, 46), (33, 54))
  , ((40, 42), (51, 51)), ((60, 47), (72, 56)), ((80, 44), (93, 53))
  , ((6, 64), (18, 72)), ((27, 61), (38, 70)), ((45, 66), (58, 75))
  , ((67, 63), (78, 72)), ((87, 67), (99, 76)), ((3, 85), (15, 94))
  , ((22, 88), (35, 97)), ((43, 84), (54, 93)), ((63, 89), (76, 98))
  , ((83, 86), (95, 95)), ((7, 106), (19, 115)), ((28, 103), (39, 112))
  , ((47, 108), (59, 117)), ((68, 105), (80, 114)), ((88, 109), (101, 118))
  ]

testDungeon :: Dungeon
testDungeon = compose 2.0 $ do
    forM_ testDungeonRooms $ \(p1, p2) -> room p1 p2

    lTunnel (11, 5) (18, 8)
    zigZag (29, 6) (33, 8) (37, 10)
    lTunnel (47, 6) (56, 10)
    lTunnel (68, 9) (78, 10)

    digX (16, 26) 8
    zigZag (34, 24) (38, 26) (43, 28)
    digX (55, 27) 9
    lTunnel (75, 25) (84, 30)

    lTunnel (14, 47) (21, 49)
    digX (33, 48) 7
    zigZag (51, 45) (55, 48) (60, 51)
    digX (72, 50) 8

    digX (18, 68) 9
    zigZag (38, 65) (41, 67) (45, 70)
    digX (58, 70) 9
    lTunnel (78, 68) (87, 72)

    lTunnel (15, 89) (22, 92)
    zigZag (35, 90) (39, 91) (43, 92)
    lTunnel (54, 88) (63, 93)
    digX (76, 92) 7

    digX (19, 110) 9
    zigZag (39, 107) (43, 109) (47, 112)
    digX (59, 112) 9
    lTunnel (80, 110) (88, 114)

    digY (7, 9) 14
    digY (8, 31) 12
    digY (8, 52) 12
    digY (10, 72) 13
    digY (10, 94) 12
  where
    lTunnel (x1, y1) (x2, y2) = do
      digX (x1, y1) (x2 - x1)
      digY (x2, y1) (y2 - y1)

    zigZag (x1, y1) (xm, ym) (x2, y2) = do
      digX (x1, y1) (xm - x1)
      digY (xm, y1) (ym - y1)
      digX (xm, ym) (x2 - xm)
      digY (x2, ym) (y2 - ym)

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
                         , cfgScreenDims = (80, 30)
                         , cfgPadding = (5, 5)
                         }
  void $ runReaderT (reactimateExcept (try $ mainMSF test)) (defCfg, test)
  shutdown vty
