{-# LANGUAGE Arrows #-}
module Dungeon.Game where

import Data.Array
import Control.Arrow
import Control.Monad (guard, mzero, void)
import Control.Monad.Trans.MSF
import Control.Monad.Trans.MSF.Maybe
import Data.MonadicStreamFunction

import Graphics.Vty
import Graphics.Vty.CrossPlatform

import Dungeon.Map
import Dungeon.Interface as DI
import Dungeon.Logic

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

-- | The MSF that ties it all together, running in the `MaybeT IO` monad.
-- It terminates when `guard` fails.
mainMSF :: Vty -> MSF (MaybeT IO) () ()
mainMSF vty =
  let dung = testDungeon
      dims = snd $ bounds dung
      vs = testSettings
      -- Lift the effectful IO MSFs into the `MaybeT IO` monad
      input = liftTransS (inputVty vty)
      output = liftTransS (DI.outputVty vty dims vs)
      logic = liftTransS (playerPos dims)

      -- An MSF that terminates the computation if the input is Just DI.Quit.
      terminateOnQuit = arrM (\u -> if u == Just DI.Quit then mzero else return ())

  in proc () -> do
    userInput <- input -< ()

    -- Terminate the MSF if the user quits.
    _ <- terminateOnQuit -< userInput

    currentPosition <- iPre initialState <<< logic -< userInput
    output -< (dung, currentPosition)

-- | Runs the game by reactimating the main MSF until it terminates.
runGame :: IO ()
runGame = do
  vty <- mkVty defaultConfig
  void $ reactimateMaybe (mainMSF vty)
  shutdown vty
