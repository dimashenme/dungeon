{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Dungeon.Interface (
    Turn(..),
    Direction(..),
    Config(..),
    HasVty(..),
    inputVty,
    outputVty,
    viewport
) where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.IO.Class

import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.Core
import Data.MonadicStreamFunction.Util
import Graphics.Vty
import Graphics.Vty.Picture
import Data.Default (def)
import Data.Array

import Dungeon.Combinators
import Dungeon.Map
import Dungeon.Logic
  ( GameView(..)
  , gameView
  , Turn(..)
  , Direction(..)
  ) -- and this is all we should know about game state here


-- | Configuration for the application
data Config = Config
    { cfgVty          :: Vty
    , cfgScreenDims   :: (Int, Int)
    , cfgPadding       :: (Int, Int)
    }


-- | Type class for accessing Vty and other common configuration data
class HasVty r where
    getVty :: r -> Vty
    getScreenDims :: r -> (Int, Int)
    getPadding :: r -> (Int, Int)

instance HasVty Config where
    getVty = cfgVty
    getScreenDims = cfgScreenDims
    getPadding = cfgPadding


-- | An MSF reading keys frm the terminal.
inputVty :: (MonadReader r m, HasVty r, MonadIO m) => MSF m () (Maybe Turn)
inputVty = arrM $ \_ -> do
    vty <- asks getVty
    evt <- liftIO $ nextEvent vty
    case evt of
        EvKey (KChar c)   [] -> return $ parseInput c
        _                    -> return Nothing
  where
    parseInput 'h' = Just (Move West)
    parseInput 'j' = Just (Move South)
    parseInput 'k' = Just (Move North)
    parseInput 'l' = Just (Move East)
    parseInput 'q' = Just Quit
    parseInput _   = Nothing           -- Returns Nothing for unknown keys



-- | An MSF that renders the game state to the terminal.
outputVty :: (MonadReader r m, HasVty r, MonadIO m, GameView v)
          => MSF m v ()
outputVty = proc gv -> do
    let newPlayerPos = getPlayerPos gv
    let dungeonDims = snd $ bounds $ getDungeon gv
    vp <- viewport -< (newPlayerPos, dungeonDims)
    render -< (gv, vp)

-- | An MSF that calculates the viewport based on the player's position.
viewport :: (MonadReader r m, HasVty r)
         => MSF m ( (Int, Int)
                  ,  (Int, Int))
            (Int, Int, Int, Int)
viewport = proc (newPlayerPos, dungeonDims) -> do
    (sw,sh) <- asksS getScreenDims -< ()

    (accumulateS updateViewportA) -< ((newPlayerPos,dungeonDims), (1, 1, sw, sh))
  where
    updateViewportA = arrM $ uncurry (uncurry updateViewport)
    updateViewport :: (MonadReader r m, HasVty r)
                   => (Int, Int)
                   -> (Int, Int)
                   -> (Int, Int, Int, Int)
                   -> m (Int, Int, Int, Int)
    updateViewport (px', py') (lw, lh) (x1, y1, x2, y2) =
      reader $
      (\c ->
          let (padX, padY) = getPadding c
              vpW = x2 - x1 + 1
              vpH = y2 - y1 + 1
              updateAxis position start end mapSize viewportSize padding
                | mapSize <= viewportSize = 1
                | position < start + padding =
                    max 1 (position - padding)
                | position > end - padding =
                    min (mapSize - viewportSize + 1)
                      (position + padding - viewportSize + 1)
                | otherwise = start
              x1' = updateAxis px' x1 x2 lw vpW padX
              y1' = updateAxis py' y1 y2 lh vpH padY
          in (x1', y1', x1' + vpW - 1, y1' + vpH - 1)
      )

-- | Renders the game state to the Vty terminal.
render :: (MonadReader r m, HasVty r, MonadIO m, GameView v)
       => MSF m (v, (Int, Int, Int, Int))  ()
render = arrM $ \(gv,  vp@(x1, y1, x2, y2)) -> do
    vty <- asks getVty
    let (px, py) = getPlayerPos gv
    let dung = getDungeon gv
    let viewportCell position
          | inRange (bounds dung) position = dung ! position
          | otherwise = ' '
    let drawRect = vertCat [ string defAttr [ viewportCell (x, y) | x <- [x1 .. x2] ]
                           | y <- [y1 .. y2] ]

    liftIO $ do
      update vty (picForLayers
                   [ translate 10 5 drawRect
                   , string defAttr ("Player pos: " ++ show (px, py))
                   ])
      setCursorPos (outputIface vty) (px - x1 + 10) (py - y1 + 5)
      showCursor (outputIface vty)
