{-# LANGUAGE Arrows #-}

module Dungeon.Console
    ( ConsoleCommand(..)
    , consoleActor
    , consoleGameInput
    , consoleViewRadius
    , externalNpcDecisions
    , inputConsole
    , inputConsoleFrom
    , outputConsole
    , outputConsoleTo
    , parseConsoleCommand
    , readyLines
    , readConsoleCommand
    , responseLines
    , senseLines
    , turnResultLines
    , writeConsoleBlock
    ) where

import Control.Arrow (arr, returnA, (>>>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Array (bounds, inRange, (!))
import Data.Char (isSpace)
import Data.Either (isRight)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import Data.MonadicStreamFunction (arrM, mapMaybeS)
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import System.IO
    ( Handle
    , hFlush
    , hGetLine
    , hIsEOF
    , hPutStr
    , stdin
    , stdout
    )
import System.IO.Error (tryIOError)

import Dungeon.GameData (npcGlyph, npcName)
import Dungeon.Logic
    ( GameState(..)
    , GameView(..)
    , Player(..)
    )
import Dungeon.Npc
    ( Npc(..)
    , NpcDecision
    , NpcId
    , npcAt
    )
import Dungeon.Types
    ( CharAttributes(..)
    , Direction(..)
    , Stats(..)
    , Turn(..)
    , Vitals(..)
    )

data ConsoleCommand
    = ConsoleMove Direction
    | ConsoleWait
    | ConsoleSense
    | ConsoleCharacter
    | ConsoleQuit
    | ConsoleInvalid
    deriving (Show, Eq)

data ChildStep
    = ContinueDialogue
    | SubmitDecision (Map.Map NpcId NpcDecision)
    | EndDialogue

parseConsoleCommand :: String -> ConsoleCommand
parseConsoleCommand input =
    case trim input of
        "h" -> ConsoleMove West
        "j" -> ConsoleMove South
        "k" -> ConsoleMove North
        "l" -> ConsoleMove East
        "." -> ConsoleWait
        "s" -> ConsoleSense
        "c" -> ConsoleCharacter
        "q" -> ConsoleQuit
        _ -> ConsoleInvalid
    where
        trim = dropWhileEnd isSpace . dropWhile isSpace

inputConsole :: MSF (ExceptT () IO) () ConsoleCommand
inputConsole = inputConsoleFrom stdin

inputConsoleFrom
    :: Handle
    -> MSF (ExceptT () IO) () ConsoleCommand
inputConsoleFrom handle =
    inputConsoleMaybeFrom handle
        >>> arrM (maybe (throwE ()) pure)

inputConsoleMaybeFrom
    :: MonadIO m
    => Handle
    -> MSF m () (Maybe ConsoleCommand)
inputConsoleMaybeFrom handle = arrM $ const $ liftIO $ do
    result <- tryIOError (readConsoleCommand handle)
    pure $ either (const Nothing) id result

outputConsole :: MonadIO m => MSF m [String] ()
outputConsole = outputConsoleTo stdout

outputConsoleTo :: MonadIO m => Handle -> MSF m [String] ()
outputConsoleTo handle = arrM $ liftIO . writeConsoleBlock handle

readConsoleCommand :: Handle -> IO (Maybe ConsoleCommand)
readConsoleCommand handle = do
    eof <- hIsEOF handle
    if eof
        then pure Nothing
        else Just . parseConsoleCommand <$> hGetLine handle

writeConsoleBlock :: Handle -> [String] -> IO ()
writeConsoleBlock handle block = do
    hPutStr handle (unlines $ block ++ [""])
    hFlush handle

consoleViewRadius :: Int
consoleViewRadius = 5

consoleActor :: GameState -> Maybe (NpcId, Npc)
consoleActor = Map.lookupMin . stNpcs

consoleGameInput
    :: NpcId
    -> GameView
    -> ConsoleCommand
    -> Maybe (Turn, Map.Map NpcId NpcDecision)
consoleGameInput ident view command =
    case command of
        ConsoleMove direction -> apply (directionDelta direction)
        ConsoleWait -> apply (0, 0)
        _ -> Nothing
    where
        apply npcDecision
            | Map.member ident (vNpcs view) =
                Just (Wait, Map.singleton ident npcDecision)
            | otherwise = Nothing

        directionDelta North = (0, -1)
        directionDelta South = (0, 1)
        directionDelta West = (-1, 0)
        directionDelta East = (1, 0)

externalNpcDecisions
    :: MonadIO m
    => NpcId
    -> Handle
    -> Handle
    -> MSF m GameView (Map.Map NpcId NpcDecision)
externalNpcDecisions ident input output =
    driveChildProtocol (childProtocol ident input output)

childProtocol
    :: MonadIO m
    => NpcId
    -> Handle
    -> Handle
    -> MSF m GameView ChildStep
childProtocol ident input output = proc view -> do
    command <- inputConsoleMaybeFrom input -< ()
    let (step, response) = childCommandStep ident view command
    written <- mapMaybeS (writeChildResponse output) -< response
    returnA -< case written of
        Just False -> EndDialogue
        _ -> step

childCommandStep
    :: NpcId
    -> GameView
    -> Maybe ConsoleCommand
    -> (ChildStep, Maybe [String])
childCommandStep ident view command =
    case command of
        Nothing -> (EndDialogue, Nothing)
        Just turn@(ConsoleMove _) -> submit turn
        Just ConsoleWait -> submit ConsoleWait
        Just ConsoleQuit -> (EndDialogue, Just ["bye"])
        Just other ->
            ( ContinueDialogue
            , Just (responseLines ident other view)
            )
    where
        submit turn =
            case consoleGameInput ident view turn of
                Just (_, decisions) ->
                    (SubmitDecision decisions, Nothing)
                Nothing ->
                    (EndDialogue, Just ["error actor-unavailable"])

writeChildResponse
    :: MonadIO m
    => Handle
    -> MSF m [String] Bool
writeChildResponse output = arrM $ \response ->
    liftIO
        $ isRight
            <$> tryIOError (writeConsoleBlock output response)

driveChildProtocol
    :: Monad m
    => MSF m GameView ChildStep
    -> MSF m GameView (Map.Map NpcId NpcDecision)
-- Protocol samples share one frozen view until the child submits a world turn.
driveChildProtocol protocol = MSF $ \view -> do
    (decision, protocol') <- awaitChildDecision view protocol
    pure $ case decision of
        Just decisions ->
            (decisions, driveChildProtocol protocol')
        Nothing ->
            (Map.empty, arr $ const Map.empty)

awaitChildDecision
    :: Monad m
    => GameView
    -> MSF m GameView ChildStep
    -> m
         ( Maybe (Map.Map NpcId NpcDecision)
         , MSF m GameView ChildStep
         )
awaitChildDecision view protocol = do
    (step, protocol') <- unMSF protocol view
    case step of
        ContinueDialogue -> awaitChildDecision view protocol'
        SubmitDecision decisions -> pure (Just decisions, protocol')
        EndDialogue -> pure (Nothing, protocol')

readyLines :: Npc -> [String]
readyLines npc =
    [ unwords
        [ "ready"
        , "version=1"
        , "kind=" ++ npcName (npcKind npc)
        , "radius=" ++ show consoleViewRadius
        ]
    ]

responseLines :: NpcId -> ConsoleCommand -> GameView -> [String]
responseLines ident command view =
    case command of
        ConsoleMove _ -> turnResponse
        ConsoleWait -> turnResponse
        ConsoleSense ->
            maybe unavailable id (senseLines ident view)
        ConsoleCharacter ->
            maybe unavailable id (characterLines ident view)
        ConsoleQuit -> ["bye"]
        ConsoleInvalid -> ["error unknown-command"]
    where
        unavailable = ["error actor-unavailable"]
        turnResponse
            | Map.member ident (vNpcs view) =
                ["ok turn=" ++ show (vTurnNumber view)]
            | otherwise = unavailable

senseLines :: NpcId -> GameView -> Maybe [String]
senseLines ident view = do
    npc <- Map.lookup ident (vNpcs view)
    let (x, y) = npcPosition npc
    pure $
        ["area pos=" ++ show x ++ "," ++ show y]
        ++ [ [ cellAt (x + dx, y + dy)
             | dx <- [-consoleViewRadius .. consoleViewRadius]
             ]
           | dy <- [-consoleViewRadius .. consoleViewRadius]
           ]
    where
        cellAt pos
            | pos == plPos (vPlayer view) = '@'
            | Just (other, npc) <- npcAt pos (vNpcs view)
            , other /= ident =
                npcGlyph (npcKind npc)
            | not (inRange (bounds dungeon) pos) = '_'
            | dungeon ! pos == ' ' = '_'
            | otherwise = dungeon ! pos
        dungeon = vDungeon view

characterLines :: NpcId -> GameView -> Maybe [String]
characterLines ident view = do
    npc <- Map.lookup ident (vNpcs view)
    let attributes = npcAttributes npc
        vitals = charVitals attributes
        stats = charStats attributes
    pure
        [ "character kind=" ++ npcName (npcKind npc)
        , unwords
            [ "vitals"
            , "hp=" ++ show (vitalHealth vitals)
            , "mp=" ++ show (vitalMana vitals)
            , "hunger=" ++ show (vitalHunger vitals)
            ]
        , unwords
            [ "stats"
            , "str=" ++ show (statStrength stats)
            , "int=" ++ show (statIntelligence stats)
            , "dex=" ++ show (statDexterity stats)
            , "con=" ++ show (statConstitution stats)
            ]
        ]

turnResultLines :: GameView -> GameView -> [String]
turnResultLines previous current =
    [ status ++ " turn=" ++ show (vTurnNumber current) ]
    where
        status
            | vTurnNumber current > vTurnNumber previous = "ok"
            | otherwise = "held"
