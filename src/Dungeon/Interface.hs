{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Dungeon.Interface (
    Turn(..),
    Direction(..),
    Config(..),
    HasVty(..),
    inputVty,
    outputVty
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
viewport :: (MonadReader r m, HasVty r, MonadIO m)
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
    updateViewport (px', py') (lw, lh) vp@(x1, y1, x2, y2) =
      reader $
      (\c ->
          let (padX, padY) = getPadding c
              dx = min (px' - x1 - padX) 0 + max (padX - x2 + px') 0
              dy = min (py' - y1 - padY) 0 + max (padY - y2 + py') 0
              shift (sx1, sy1, sx2, sy2) (sdx, sdy) =
                if (sx1 + sdx >= 1) && (sx2 + sdx <= lw)
                && (sy1 + sdy >= 1) && (sy2 + sdy <= lh)
                then (sx1 + sdx, sy1 + sdy, sx2 + sdx, sy2 + sdy)
                else (sx1, sy1, sx2, sy2)
          in
            if dx /= 0 || dy /= 0 then shift vp (dx, dy) else vp
      )

-- | Renders the game state to the Vty terminal.
render :: (MonadReader r m, HasVty r, MonadIO m, GameView v)
       => MSF m (v, (Int, Int, Int, Int))  ()
render = arrM $ \(gv,  vp@(x1, y1, x2, y2)) -> do
    vty <- asks getVty
    let (px, py) = getPlayerPos gv
    let dung = getDungeon gv
    let drawRect = vertCat [ string defAttr [ dung ! (x, y) | x <- [x1 .. x2] ]
                           | y <- [y1 .. y2] ]

    liftIO $ do
      update vty (picForLayers
                   [ translate 10 5 drawRect
                   , string defAttr ("Player pos: " ++ show (px, py))
                   ])
      setCursorPos (outputIface vty) (px - x1 + 10) (py - y1 + 5)
      showCursor (outputIface vty)


