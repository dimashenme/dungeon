{-# LANGUAGE Arrows #-}

module Dungeon.Game
    ( gameStateWithMessages
    , gameView
    , gameViewInput
    , messageLog
    , mainMSF
    , runApplication
    , runConsole
    , runGame
    , runScriptedGame
    , runScriptedGameWithMessages
    ) where

import Prelude hiding (init)
import Control.Arrow
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.MSF
import Control.Monad.Trans.MSF.RWS (runRWSS)
import Data.Either (isRight)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Map.Strict as Map
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import System.Exit (exitFailure)
import System.IO
    ( Handle
    , hPutStrLn
    , hSetEncoding
    , stderr
    , stdin
    , stdout
    , utf8
    )
import System.IO.Error (tryIOError)
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , withCreateProcess
    )
import qualified System.Process as Process
import Text.Read (readMaybe)

import Graphics.Vty
import Graphics.Vty.CrossPlatform

import Dungeon.Console
import Dungeon.GameData (observe)
import Dungeon.Interface as DI
import Dungeon.Logic
import Dungeon.Combinators
import Dungeon.Npc (Npc, NpcDecision, NpcId(..))
import Dungeon.TestLayout1

type GameFx = ReaderT DI.Config IO

gameStateWithMessages
    :: MonadFix m
    => GameState
    -> MSF m Turn (GameState, [String])
gameStateWithMessages init =
    arr withoutNpcDecisions
        >>> gameStateWithNpcDecisionsAndMessages init

gameStateWithNpcDecisionsAndMessages
    :: MonadFix m
    => GameState
    -> MSF
         m
         (Turn, Map.Map NpcId NpcDecision)
         (GameState, [String])
gameStateWithNpcDecisionsAndMessages init = proc input@(turn, _) -> do
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
                (gameStateWithNpcDecisions init)
                -< input
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
gameViewInput init =
    arr (first $ fmap withoutNpcDecisions)
        >>> gameViewInputWithNpcDecisions init

gameViewInputWithNpcDecisions
    :: MonadFix m
    => GameState
    -> MSF
         m
         (Maybe (Turn, Map.Map NpcId NpcDecision), [String])
         GameView
gameViewInputWithNpcDecisions init = proc (input, frontendMsgs) -> do
    result <-
        mapMaybeS (gameStateWithNpcDecisionsAndMessages init) -< input
    currentState <- sampleAndHold init (arr id) -< fst <$> result
    msgs <- messageLog -< maybe [] snd result ++ frontendMsgs
    returnA -< toGameView msgs currentState

withoutNpcDecisions :: a -> (a, Map.Map NpcId NpcDecision)
withoutNpcDecisions input = (input, Map.empty)

messageLog :: Monad m => MSF m [String] [String]
messageLog = accumulateWith (flip (++)) []

-- | The MSF that ties it all together, running in the application effect
-- stack while game logic receives its initial state explicitly.
mainMSF :: GameState -> MSF (ExceptT () GameFx) () ()
mainMSF = mainMSFWithAgent Nothing

mainMSFWithAgent
    :: Maybe (NpcId, Handle, Handle)
    -> GameState
    -> MSF (ExceptT () GameFx) () ()
mainMSFWithAgent agent initState = runMSFExcept $ do
    let initView = toGameView [] initState
    try $
        doOnce
            (DI.outputVty <<< arr (const (initView, DI.DungeonScreen)))
    try $ proc () -> do
        rec
            prevView <- iPre initView -< nextView
            driverUsable <- iPre True -< driverUsable'
            key <- DI.inputVty -< ()
            (modes, input) <- DI.uiState -< (key, prevView)
            let (quitRequested, nextGameInput) = routeUIInput input
            let (nextTurn, frontendMsgs) = nextGameInput
            _ <- case quitRequested of
                True -> throw () -< ()
                _ -> returnA -< ()
            let decisionView
                    | driverUsable = prevView <$ nextTurn
                    | otherwise = Nothing
            external <- decisions -< decisionView
            nextView <-
                gameViewInputWithNpcDecisions initState
                    -< ( fmap
                            (\turn -> (turn, external))
                            nextTurn
                       , frontendMsgs
                       )
            let mode = case modes of
                    current : _ -> current
                    [] -> DI.DungeonScreen
            DI.outputVty -< (nextView, mode)
            let result
                    | Map.null external = Nothing
                    | otherwise = Just (prevView, nextView)
            acknowledged <- acknowledge -< result
            let driverUsable' = driverUsable && acknowledged
        returnA -< ()
    where
        decisions =
            case agent of
                Nothing -> arr $ const Map.empty
                Just (ident, input, output) ->
                    mapMaybeS (externalNpcDecisions ident input output)
                        >>> arr (maybe Map.empty id)

        acknowledge =
            case agent of
                Nothing -> arr $ const True
                Just (_, _, output) ->
                    mapMaybeS (acknowledgeTo output)
                        >>> arr (maybe True id)

        acknowledgeTo output = arrM $ \(previous, current) ->
            liftIO
                $ isRight
                    <$> tryIOError
                        (writeConsoleBlock
                            output
                            (turnResultLines previous current))

        routeUIInput
            :: Maybe DI.UIInput
            -> (Bool, (Maybe Turn, [String]))
        routeUIInput Nothing = (False, (Nothing, []))
        routeUIInput (Just (DI.PlayTurn turn)) =
            (False, (Just turn, []))
        routeUIInput (Just (DI.LogMessage msg)) =
            (False, (Nothing, [msg]))
        routeUIInput (Just DI.Quit) = (True, (Nothing, []))

consoleMSF
    :: GameState
    -> (NpcId, Npc)
    -> MSF (ExceptT () IO) () ()
consoleMSF initState (ident, npc) = runMSFExcept $ do
    try $ doOnce (outputConsole <<< arr (const $ readyLines npc))
    try $ proc () -> do
        command <- inputConsole -< ()
        rec
            previousView <- iPre initView -< nextView
            nextView <-
                gameViewInputWithNpcDecisions initState
                    -< (consoleGameInput ident previousView command, [])
        outputConsole -< responseLines ident command nextView
        _ <- case command of
            ConsoleQuit -> throw () -< ()
            _ -> returnA -< ()
        returnA -< ()
    where
        initView = toGameView [] initState

-- | Runs the game by reactimating the main MSF until it terminates.
runGame :: IO ()
runGame = runVtyGame Nothing

runVtyGame :: Maybe (NpcId, Handle, Handle) -> IO ()
runVtyGame agent =
    bracket (mkVty defaultConfig) shutdown $ \vty -> do
        let config = DI.Config
                { cfgVty = vty
                , cfgViewportDims = (48, 18)
                , cfgPadding = (5, 5)
                }
        void
            $ runReaderT
                (reactimateExcept
                    (try $ mainMSFWithAgent agent initialGameState))
                config

runConsole :: IO ()
runConsole = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    case consoleActor initialGameState of
        Nothing -> do
            hPutStrLn stderr "console: no NPC available"
            exitFailure
        Just actor ->
            void $ reactimateExcept (try $ consoleMSF initialGameState actor)

runApplication :: [String] -> IO ()
runApplication args =
    case args of
        [] -> runGame
        ["--console"] -> runConsole
        "--agent" : rawIdent : "--" : command : childArgs ->
            case readMaybe rawIdent of
                Just ident
                    | ident >= 0 ->
                        runAgentGame (NpcId ident) command childArgs
                _ -> usageFailure
        _ -> usageFailure

runAgentGame :: NpcId -> FilePath -> [String] -> IO ()
runAgentGame ident command args =
    case Map.lookup ident (stNpcs initialGameState) of
        Nothing -> do
            hPutStrLn stderr
                ("agent: NPC " ++ showNpcId ident ++ " is unavailable")
            exitFailure
        Just npc ->
            withCreateProcess child $ \toChild fromChild _ _ ->
                case (fromChild, toChild) of
                    (Just input, Just output) -> do
                        hSetEncoding input utf8
                        hSetEncoding output utf8
                        ready <- tryIOError
                            $ writeConsoleBlock output (readyLines npc)
                        case ready of
                            Right () ->
                                runVtyGame (Just (ident, input, output))
                            Left err -> do
                                hPutStrLn stderr
                                    ("agent: handshake failed; " ++ show err)
                                runVtyGame Nothing
                    _ -> do
                        hPutStrLn stderr "agent: failed to create pipes"
                        exitFailure
    where
        child =
            (Process.proc command args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                }

        showNpcId (NpcId value) = show value

usageFailure :: IO a
usageFailure = do
    hPutStrLn stderr
        "usage: dungeon-exe [--console | --agent NPC_ID -- PROGRAM [ARG ...]]"
    exitFailure

initialGameState :: GameState
initialGameState =
    initGameState
        (initPlayer testStartPos)
        testLayout
        (RandomSeed 1)

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
