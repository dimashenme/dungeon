{-# LANGUAGE Arrows #-}

module Dungeon.Game
    ( gameStateWithMessages
    , gameView
    , gameViewInput
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

import Dungeon.GameData (observe)
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
                    observe currentPosition (stFloorItems state)
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
    arr (\turn -> (Just turn, []))
        >>> gameViewInput init

gameViewInput
    :: MonadFix m
    => GameState
    -> MSF m (Maybe Turn, [String]) GameView
gameViewInput init = proc (turn, frontendMsgs) -> do
    result <- mapMaybeS (gameStateWithMessages init) -< turn
    currentState <- sampleAndHold init (arr id) -< fst <$> result
    msgs <- messageLog -< maybe [] snd result ++ frontendMsgs
    returnA -< toGameView msgs currentState

messageLog :: Monad m => MSF m [String] [String]
messageLog = accumulateWith (flip (++)) []

-- | The MSF that ties it all together, running in the application effect
-- stack while game logic receives its initial state explicitly.
mainMSF :: GameState -> MSF (ExceptT () GameFx) () ()
mainMSF initState = runMSFExcept $ do
    let initView = toGameView [] initState
    try $
        doOnce
            (DI.outputVty <<< arr (const (initView, DI.DungeonScreen)))
    try $ proc () -> do
        rec
            prevView <- iPre initView -< nextView
            key <- DI.inputVty -< ()
            (modes, input) <- DI.uiState -< (key, prevView)
            let (quitRequested, nextGameInput) = routeUIInput input
            _ <- case quitRequested of
                True -> throw () -< ()
                _ -> returnA -< ()
            nextView <- gameViewInput initState -< nextGameInput
            let mode = case modes of
                    current : _ -> current
                    [] -> DI.DungeonScreen
            DI.outputVty -< (nextView, mode)
        returnA -< ()
    where
        routeUIInput
            :: Maybe DI.UIInput
            -> (Bool, (Maybe Turn, [String]))
        routeUIInput Nothing = (False, (Nothing, []))
        routeUIInput (Just (DI.PlayTurn turn)) =
            (False, (Just turn, []))
        routeUIInput (Just (DI.LogMessage msg)) =
            (False, (Nothing, [msg]))
        routeUIInput (Just DI.Quit) = (True, (Nothing, []))

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
            , cfgViewportDims = (48, 18)
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
