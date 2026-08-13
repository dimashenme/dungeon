{-# LANGUAGE Arrows #-}

module Dungeon.Game
    ( gameStateWithMessages
    , gameView
    , messageLog
    , mainMSF
    , runGame
    , runScriptedGame
    , runScriptedGameWithMessages
    ) where

import Prelude hiding (init)
import Control.Arrow
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.MSF
import Control.Monad.Trans.MSF.RWS (runRWSS)
import Data.Functor.Identity (Identity, runIdentity)
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF(..))

import Graphics.Vty
import Graphics.Vty.CrossPlatform

import Dungeon.Interface as DI
import Dungeon.Logic
import Dungeon.Combinators
import Dungeon.TestLayout1

type GameFx = ReaderT DI.Config IO

gameStateWithMessages
    :: MonadFix m
    => GameState
    -> MSF m Turn (GameState, [String])
gameStateWithMessages init = proc turn -> do
    rec
        previousPosition <-
            iPre (plPos (stPlayer init)) -< currentPosition
        (state, msgs) <-
            runGameEffects
                defaultGameSettings
                GameRuntimeState
                    { rtRandomSeed = stRandomSeed init
                    , rtNextItemId = stNextItemId init
                    }
                (gameState init)
                -< turn
        let currentPosition = plPos (stPlayer state)
        let observationMsgs
                | currentPosition /= previousPosition || turn == Inspect =
                    DI.observe currentPosition (stFloorItems state)
                | otherwise = []
    returnA -< (state, msgs ++ observationMsgs)

runGameEffects
    :: MonadFix m
    => GameSettings
    -> GameRuntimeState
    -> MSF (GameT m) a b
    -> MSF m a (b, [String])
runGameEffects settings initRuntime msf =
    feedback initRuntime
        $ arr (\(input, runtime) -> (settings, runtime, input))
        >>> runRWSS msf
        >>> arr
            (\(messages, runtime, output) ->
                ((output, messages), runtime))

gameView
    :: MonadFix m
    => GameState
    -> MSF m Turn GameView
gameView init =
    gameStateWithMessages init
        >>> second messageLog
        >>> arr (uncurry (flip toGameView))

messageLog :: Monad m => MSF m [String] [String]
messageLog = accumulateWith (flip (++)) []

-- | The MSF that ties it all together, running in the application effect
-- stack while game logic receives its initial state explicitly.
mainMSF :: GameState -> MSF (ExceptT () GameFx) () ()
mainMSF initState = runMSFExcept $ do
    let initView = toGameView [] initState
    try $
        doOnce
            (DI.outputVty <<< arr (const (DI.DungeonScreen initView)))
    try $ proc () -> do
        rec
            prevView <- iPre initView -< nextView
            input <- DI.inputVty -< ()
            (nextView, screen) <- case input of
                Just DI.Quit -> throw () -< ()
                Just (DI.PlayTurn turn) ->
                    gameView initState
                        >>> arr (\view -> (view, DI.DungeonScreen view))
                        -< turn
                Just DI.ShowInventory ->
                    returnA -<
                        (prevView, DI.InventoryScreen prevView)
                Just DI.Redraw ->
                    returnA -< (prevView, DI.DungeonScreen prevView)
                Nothing ->
                    returnA -< (prevView, DI.DungeonScreen prevView)
            DI.outputVty -< screen
        returnA -< ()

-- | Runs the game by reactimating the main MSF until it terminates.
runGame :: IO ()
runGame = do
    vty <- mkVty defaultConfig
    let initState =
            initGameState
                (initPlayer testStartPos)
                testLayout
                (RandomSeed 1)
    let config = DI.Config
            { cfgVty = vty
            , cfgScreenDims = (80, 30)
            , cfgPadding = (5, 5)
            }
    void
        $ runReaderT
            (reactimateExcept (try $ mainMSF initState))
            config
    shutdown vty

runScriptedGame :: GameState -> [Turn] -> [GameState]
runScriptedGame init =
    map fst . runScriptedGameWithMessages init

runScriptedGameWithMessages
    :: GameState
    -> [Turn]
    -> [(GameState, [String])]
runScriptedGameWithMessages init =
    go (gameStateWithMessages init)
    where
        go
            :: MSF Identity Turn (GameState, [String])
            -> [Turn]
            -> [(GameState, [String])]
        go _ [] = []
        go msf (turn : rest) =
            let (out, nextMSF) = runIdentity (unMSF msf turn)
            in out : go nextMSF rest
