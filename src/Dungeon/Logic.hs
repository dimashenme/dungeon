{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Dungeon.Logic
  ( GameView(..)
  , GameState(..)
  , gameView
  , Turn(..)
  , Direction(..)
  , HasInitState(..)
  , playerPos
  , runScriptedGame
  ) where

import Control.Monad.Reader
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import Dungeon.Map
import Dungeon.Combinators

data Direction = North | South | West | East deriving (Show, Eq)
data Turn = Move Direction deriving (Show, Eq)

data GameState = GameState
  {
    stPlayerPos :: (Int, Int)
  , stDungeon   :: Dungeon
  }
class GameView a where
    getPlayerPos :: a -> (Int, Int)
    getDungeon   :: a -> Dungeon

instance GameView GameState where
    getPlayerPos = stPlayerPos
    getDungeon = stDungeon
  
-- the game logic MSFs below treat the GameState stored in the Reader
-- monad as the initial state of the current run of the game

class HasInitState r where
  initPlayerPos :: r -> (Int, Int)
  initDungeon   :: r -> Dungeon

instance HasInitState GameState where
    initPlayerPos = stPlayerPos
    initDungeon = stDungeon

playerPos :: (MonadReader r m, HasInitState r)
          => MSF m (Dungeon, Turn) (Int, Int)
playerPos = proc input -> do
    ip <- asksS initPlayerPos -< ()
    returnA <<< (accumulateS updatePosA) -< (input, ip)
      where
        updatePosA = arr $ uncurry $ uncurry updatePos
        updatePos :: Dungeon -> Turn -> (Int, Int) -> (Int, Int)
        updatePos dung (Move dir) currentPos@(px,py) =
          let candidate = case dir of
                             West  -> (px-1, py)
                             East  -> (px+1, py)
                             North -> (px, py-1)
                             South -> (px, py+1)
          in if isWalkable dung candidate then candidate else currentPos

dungeon :: (MonadReader r m, HasInitState r)
        => MSF m Turn Dungeon
dungeon = asksS initDungeon -- for now, may get dynamic dungeon

gameState :: (MonadReader r m, HasInitState r)
          => MSF m Turn GameState
gameState = proc turn -> do
    dung <- dungeon -< turn
    pos <- playerPos -< (dung, turn)

    returnA -< GameState{ stPlayerPos = pos
                        , stDungeon = dung}

gameView :: (MonadReader r m, HasInitState r)
         => MSF m Turn GameState
gameView = gameState

-- | Run the game-logic MSF with predetermined gameplay turns.
--
-- Each output is the view after its corresponding turn has been applied.
runScriptedGame :: GameState -> [Turn] -> [GameState]
runScriptedGame initialState turns =
  runReader (go gameView turns) initialState
  where
    go :: MSF (Reader GameState) Turn GameState
       -> [Turn]
       -> Reader GameState [GameState]
    go _ [] = pure []
    go msf (turn : rest) = do
      (view, nextMsf) <- unMSF msf turn
      (view :) <$> go nextMsf rest
