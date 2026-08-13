module Main (main) where

import Prelude hiding (init)
import Control.Arrow (arr, (&&&), (>>>))
import Control.Monad (unless)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State.Strict (State, get, modify, runState)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.MSF.Maybe (exceptToMaybeS)
import Control.Monad.Writer.Strict (runWriter)
import Data.Array (bounds, listArray, range, (!), (//))
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Map.Strict as Map
import Data.List (isSubsequenceOf, sort)
import Data.Maybe (listToMaybe)
import Data.MonadicStreamFunction
    ( accumulateWith
    , arrM
    , embed
    )
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import qualified Data.Set as Set
import Data.Word (Word64)
import Dungeon.Combinators
    ( countdownFrom
    , doOnce
    , oneTickThen
    , reconcileMSFs
    , restartOn
    , restartOnEvt
    , runMaybeStateS
    , sampleAndHold
    )
import Dungeon.Console
import Dungeon.DungeonLayout
import qualified Dungeon.Game as Game
import Dungeon.GameData
import Dungeon.Interface
    ( HasVty(..)
    , UIKey(..)
    , UIInput(..)
    , UIMode(..)
    , choicePageSize
    , gameViewCell
    , messageLogLines
    , pageChoices
    , parseInput
    , playerStatsLines
    , screenLines
    , uiStep
    , viewport
    )
import Dungeon.Item
import Dungeon.ItemState
import Dungeon.Logic
import Dungeon.Npc
import Dungeon.Random (drawBool, selectRandomSubset)
import Dungeon.TestLayout1 (testLayout, testStartPos)
import Dungeon.Types
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode(..), exitFailure)
import System.IO (hFlush, hGetLine, hIsEOF, stdin, stdout)
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , proc
    , readCreateProcessWithExitCode
    , withCreateProcess
    )
import System.Timeout (timeout)

type Test = [String]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--patrol-agent-fixture"] -> patrolAgentFixture
        ["--quitting-agent-fixture"] -> quittingAgentFixture
        _ -> runTests

runTests :: IO ()
runTests = do
    consoleFailures <- testConsoleBlackBox
    childAgentFailures <- testChildAgentDriver
    let failures =
            consoleFailures
            ++ childAgentFailures
            ++ checks
                [ testLayoutComposition
                , testLayoutReferences
                , testSampleLayout
                , testCharacterInitialization
                , testTerrain
                , testMovementAndHoldUp
                , testNpcTurns
                , testReconcileMSFs
                , testNpcPatrolBehaviour
                , testExternalNpcDecisions
                , testFightMode
                , testWetStatus
                , testWetStatusState
                , testWaitTurn
                , testItemTransfers
                , testItemStackInvariants
                , testItemStateReducers
                , testContainerTransfers
                , testNpcKillEventStreams
                , testPlayerAttack
                , testDeterministicLoot
                , testEncounterMessages
                , testGameView
                , testInput
                , testConsoleProtocol
                , testAttackConfirmation
                , testItemInteractionUI
                , testWielding
                , testItemPresentation
                , testRendering
                , testMessageLogLines
                , testViewport
                , testOneTickThenTiming
                , testDoOnceTiming
                , testRunMaybeStateS
                , testTurnNumber
                , testCountdownFrom
                , testRestartOn
                , testSampleAndHold
                ]
    unless (null failures) $ do
        putStrLn "Test failures:"
        mapM_ putStrLn failures
        exitFailure
    putStrLn "All tests passed."

testLayoutComposition :: Test
testLayoutComposition = checks
    [ expectEqual
        "compose scales dungeon bounds"
        ((1, 1), (15, 10))
        (boundsOf (layoutDungeon layout))
    , expectEqual
        "compose scales floor item positions"
        [redItem]
        (itemStackItems (itemsAt (4, 4) (layoutItems layout)))
    , expectEqual
        "compose turns chests into floor items with contents"
        [chestItem (testStack 2 [blueItem])]
        (itemStackItems (itemsAt (6, 4) (layoutItems layout)))
    , expectEqual
        "scaled tunnel remains joined to the room"
        True
        ( isWalkable (layoutDungeon layout) (9, 6)
        && isWalkable (layoutDungeon layout) (10, 6)
        )
    ]
    where
        layout = compose 1.5 $ do
            room (2, 2) (6, 6)
            digX (6, 4) 3
            placeItem (3, 3) redItem
            chest <- placeChest (4, 3)
            placeItem chest blueItem

testLayoutReferences :: Test
testLayoutReferences = checks
    [ expectEqual
        "same-tile chest references retain identity and placement order"
        [ chestItem
            (itemStackFromList
                [ (ItemId 2, redItem)
                , (ItemId 4, ringItem)
                ])
        , chestItem (testStack 3 [blueItem])
        , greenItem
        ]
        (itemStackItems (itemsAt (4, 4) (layoutItems layout)))
    , expectEqual
        "NPC references retain inventory contents"
        [[redItem, blueItem]]
        [ itemStackItems (npcInventory npc)
        | npc <- Map.elems (layoutNpcs layout)
        ]
    , expectEqual
        "layout IDs cover chests, contents, floor items, and NPC items"
        (map ItemId [0 .. 7])
        (sort (layoutItemIds layout))
    , expectEqual
        "layout identity allocation leaves the next unused ID"
        (ItemId 8)
        (layoutNextItemId layout)
    ]
    where
        layout = compose 2.0 $ do
            first <- placeChest (2, 2)
            second <- placeChest (2, 2)
            placeItem first redItem
            placeItem second blueItem
            placeItem first ringItem
            placeItem (2, 2) greenItem
            npc <- placeNpc (3, 2) Goblin Stationary
            placeItem npc redItem
            placeItem npc blueItem

testCharacterInitialization :: Test
testCharacterInitialization = checks
    [ expectEqual
        "player initialization uses authored attributes and empty item state"
        (initialPlayerAttributes, emptyItemStack, Nothing)
        (plAttributes player, plInventory player, plWielded player)
    , expectEqual
        "NPC initialization uses authored attributes and supplied inventory"
        (initialNpcAttributes, inventory)
        (npcAttributes npc, npcInventory npc)
    ]
    where
        player = initPlayer (2, 2)
        inventory = testStack 1 [redItem]
        npc = initNpc (2, 2) Goblin Stationary inventory

testSampleLayout :: Test
testSampleLayout = checks
    [ expectEqual
        "sample start is walkable"
        True
        (isWalkable (layoutDungeon testLayout) testStartPos)
    , expectEqual
        "sample layout contains every portable item kind"
        True
        (all (`elem` sampleItems) portableItems)
    , expectEqual
        "sample NPC identities, kinds, and scaled patrols are stable"
        [ ( NpcId 0
          , Adder
          , patrol Horizontal (8, 12)
          , PatrollingToward UpperBound
          )
        , ( NpcId 1
          , Goblin
          , patrol Horizontal (14, 18)
          , PatrollingToward UpperBound
          )
        , ( NpcId 2
          , Rat
          , patrol Vertical (10, 14)
          , PatrollingToward LowerBound
          )
        , (NpcId 3, Kobold, Stationary, StationaryState)
        ]
        [ ( ident
          , npcKind npc
          , npcBehaviour npc
          , npcBehaviourState npc
          )
        | (ident, npc) <- Map.toAscList (layoutNpcs testLayout)
        ]
    ]
    where
        sampleItems =
            concatMap itemStackItems (Map.elems (layoutItems testLayout))
        portableItems =
            [ ringItem
            , redItem
            , potionItem
            , bookItem
            , scrollItem
            , shortSwordItem
            , greatAxeItem
            , shortBowItem
            , lightArmourItem
            , heavyArmourItem
            ]

testTerrain :: Test
testTerrain = checks
    [ expectEqual "room boundary is a wall" '#' (dungeon ! (2, 3))
    , expectEqual "room interior is floor" '.' (dungeon ! (3, 3))
    , expectEqual "tunnel is floor" '.' (dungeon ! (8, 4))
    , expectEqual "space is vacuum outside rooms" ' ' (dungeon ! (1, 1))
    , expectEqual
        "vacuum is not walkable"
        False
        (isWalkable dungeon (1, 1))
    , expectEqual "water is walkable" True (isWalkable dungeon (4, 4))
    , expectEqual "water is recognized" True (isWater dungeon (4, 4))
    , expectEqual "outside the map is blocked" False (isWalkable dungeon (0, 0))
    ]
    where
        dungeon = layoutDungeon $ compose 1.0 $ do
            room (2, 2) (6, 6)
            digX (6, 4) 3
            water (4, 4)

testMovementAndHoldUp :: Test
testMovementAndHoldUp = checks
    [ expectEqual
        "cardinal movement uses the resulting position signal"
        [(2, 1), (2, 3), (1, 2), (3, 2)]
        [ plPos (stPlayer (last (runScriptedGame init [Move dir])))
        | dir <- [North, South, West, East]
        ]
    , expectEqual
        "a wall bump keeps position and logical time"
        ((1, 2), 0, ["*bump*"])
        ( plPos (stPlayer bumped)
        , stTurnNumber bumped
        , bumpMsgs
        )
    , expectEqual
        "Inspect holds up the clock"
        0
        (stTurnNumber inspected)
    , expectEqual
        "successful movement advances one logical turn per action"
        [1, 2]
        (map stTurnNumber (runScriptedGame init [Move East, Move South]))
    ]
    where
        init = emptyGameState (2, 2) testDungeon
        (bumped, bumpMsgs) =
            singleSample (runScriptedGameWithMessages
                (emptyGameState (1, 2) wallDungeon)
                [Move East])
        inspected = singleSample (runScriptedGame init [Inspect])

testNpcTurns :: Test
testNpcTurns = checks
    [ expectEqual
        "a held player action leaves NPC state unchanged"
        (stNpcs init)
        (stNpcs held)
    , expectEqual
        "NPC collision resolution follows stable identity order"
        [((1, 1), NpcId 2), ((2, 1), NpcId 1)]
        (sort
            [ (npcPosition npc, ident)
            | (ident, npc) <- Map.toList (stNpcs crowded)
            ])
    , expectEqual
        "a later NPC may enter a position vacated earlier in identity order"
        [((2, 1), NpcId 2), ((3, 1), NpcId 1)]
        (sort
            [ (npcPosition npc, ident)
            | (ident, npc) <- Map.toList (stNpcs following)
            ])
        ]
    where
        first =
            initNpc
                (1, 1)
                Rat
                (patrol Horizontal (1, 4))
                emptyItemStack
        init =
            setNpcs
                (Map.singleton (NpcId 1) first)
                (emptyGameState (3, 3) testDungeon)
        held = singleSample (runScriptedGame init [Inspect])
        crowdedFirst =
            initNpc
                (3, 1)
                Rat
                (patrol Horizontal (1, 3))
                emptyItemStack
        crowdedSecond =
            initNpc
                (1, 1)
                Adder
                (patrol Horizontal (1, 3))
                emptyItemStack
        crowdedInit =
            setNpcs
                (Map.fromList
                    [ (NpcId 2, crowdedSecond)
                    , (NpcId 1, crowdedFirst)
                    ])
                (emptyGameState (3, 3) testDungeon)
        crowded = singleSample (runScriptedGame crowdedInit [Wait])
        leadingNpc =
            initNpc
                (2, 1)
                Rat
                (patrol Horizontal (1, 4))
                emptyItemStack
        followingNpc =
            initNpc
                (1, 1)
                Adder
                (patrol Horizontal (1, 4))
                emptyItemStack
        followingInit =
            setNpcs
                (Map.fromList
                    [ (NpcId 2, followingNpc)
                    , (NpcId 1, leadingNpc)
                    ])
                (emptyGameState (5, 5) testDungeon)
        following = singleSample (runScriptedGame followingInit [Wait])

testReconcileMSFs :: Test
testReconcileMSFs =
    expectEqual
        "reconcileMSFs advances, adds, removes, and freshly re-adds keyed MSFs"
        [ Map.fromList [(1, 1), (2, 10)]
        , Map.fromList [(1, 2), (2, 20), (3, 100)]
        , Map.fromList [(2, 30), (3, 200)]
        , Map.fromList [(1, 1), (3, 300)]
        ]
        (runIdentity $ embed reconciled inputs)
    where
        reconciled
            :: MSF Identity (Map.Map Int Int) (Map.Map Int Int)
        reconciled = reconcileMSFs $ \_ _ -> accumulateWith (+) 0
        inputs :: [Map.Map Int Int]
        inputs =
            [ Map.fromList [(1, 1), (2, 10)]
            , Map.fromList [(1, 1), (2, 10), (3, 100)]
            , Map.fromList [(2, 10), (3, 100)]
            , Map.fromList [(1, 1), (3, 100)]
            ]

testNpcPatrolBehaviour :: Test
testNpcPatrolBehaviour = checks
    [ expectEqual
        "a patrol reverses at absolute bounds without an idle turn"
        [ ((2, 1), PatrollingToward UpperBound)
        , ((3, 1), PatrollingToward UpperBound)
        , ((2, 1), PatrollingToward LowerBound)
        , ((1, 1), PatrollingToward LowerBound)
        , ((2, 1), PatrollingToward UpperBound)
        ]
        (map patrolSnapshot oscillating)
    , expectEqual
        "a blocked patrol preserves its position and target"
        (replicate 2 ((2, 1), PatrollingToward UpperBound))
        (map patrolSnapshot blocked)
    , expectEqual
        "persisted patrol state reproduces the uninterrupted next turn"
        (singleSample resumed)
        (oscillating !! 2)
    ]
    where
        npc =
            initNpc
                (1, 1)
                Rat
                (patrol Horizontal (1, 3))
                emptyItemStack
        init =
            setNpcs
                (Map.singleton (NpcId 20) npc)
                (emptyGameState (5, 5) testDungeon)
        oscillating = runScriptedGame init (replicate 5 Wait)
        endpoint = oscillating !! 1
        resumed = runScriptedGame endpoint [Wait]
        blocked =
            runScriptedGame
                (setNpcs
                    (Map.singleton
                        (NpcId 20)
                        npc { npcPosition = (2, 1) })
                    (emptyGameState (5, 5) wallAtThreeDungeon))
                [Wait, Wait]

        patrolSnapshot state =
            case Map.toList (stNpcs state) of
                [(_, current)] ->
                    (npcPosition current, npcBehaviourState current)
                _ -> error "patrol test expected exactly one NPC"

testExternalNpcDecisions :: Test
testExternalNpcDecisions = checks
    [ expectEqual
        "external decisions replace and pause one NPC behaviour"
        [ ((1, 2), PatrollingToward UpperBound)
        , ((2, 2), PatrollingToward UpperBound)
        , ((2, 2), PatrollingToward UpperBound)
        ]
        (map (npcSnapshot controlledId) populations)
    , expectEqual
        "uncontrolled NPC behaviour continues beside external decisions"
        [(5, 4), (4, 4), (3, 4)]
        (map (npcPosition . (Map.! otherId)) populations)
    ]
    where
        controlledId = NpcId 1
        otherId = NpcId 2
        controlled =
            initNpc
                (1, 1)
                Adder
                (patrol Horizontal (1, 4))
                emptyItemStack
        other =
            initNpc
                (4, 4)
                Rat
                (patrol Horizontal (3, 5))
                emptyItemStack
        initial = Map.fromList
            [(controlledId, controlled), (otherId, other)]
        populations =
            runIdentity
                $ embed
                    (npcsStateWithDecisions testDungeon initial)
                    [ ((2, 3), Nothing, Map.singleton controlledId (0, 1))
                    , ((2, 3), Nothing, Map.empty)
                    , ((2, 3), Nothing, Map.singleton controlledId (0, 1))
                    ]

        npcSnapshot ident population =
            let npc = population Map.! ident
            in (npcPosition npc, npcBehaviourState npc)

testFightMode :: Test
testFightMode = checks
    [ expectEqual
        "fight membership starts nearby, persists through five tiles, then ends"
        (replicate 5 fighting ++ [Exploring])
        (map (plFightMode . stPlayer) states)
    , expectEqual
        "fight membership reads its enter and leave distances from Reader"
        [fighting, fighting, Exploring]
        configured
    , expectEqual
        "fight membership produces no join or leave messages"
        []
        (concatMap snd outputs)
    ]
    where
        ident = NpcId 7
        fighting = Fighting (Set.singleton ident)
        npc = initNpc (2, 2) Kobold Stationary emptyItemStack
        init =
            setNpcs
                (Map.singleton ident npc)
                (emptyGameState (3, 2) wideDungeon)
        outputs =
            runScriptedGameWithMessages
                init
                [ Wait
                , Move East
                , Move East
                , Move East
                , Move East
                , Move East
                ]
        states = map fst outputs
        configured =
            runReader
                (embed
                    (fightMode Exploring)
                    [ ((2, 2), configuredNpcs)
                    , ((1, 2), configuredNpcs)
                    , ((0, 2), configuredNpcs)
                    ])
                defaultGameSettings
                    { gsFightEnterDistance = 2
                    , gsFightLeaveDistance = 3
                    }
        configuredNpcs =
            Map.singleton ident npc { npcPosition = (4, 2) }

testWetStatus :: Test
testWetStatus = checks
    [ expectEqual
        "water, held samples, and dry turns produce the complete wetness trace"
        [ ((3, 2), 1, Wet, 5)
        , ((3, 2), 2, Wet, 5)
        , ((2, 2), 3, Wet, 4)
        , ((2, 2), 3, Wet, 4)
        , ((2, 2), 4, Wet, 3)
        , ((2, 2), 5, Wet, 2)
        , ((2, 2), 6, Wet, 1)
        , ((2, 2), 7, Dry, 0)
        ]
        [ ( plPos player
          , stTurnNumber state
          , plWetStatus player
          , plWetCountdown player
          )
        | state <- states
        , let player = stPlayer state
        ]
    , expectEqual
        "wet and dry edges notify once"
        ["you are dry", "you are wet"]
        (sort (concatMap snd outputs))
    ]
    where
        init = emptyGameState (2, 2) waterDungeon
        outputs =
            runScriptedGameWithMessages
                init
                [ Move East
                , Wait
                , Move West
                , Inspect
                , Wait
                , Wait
                , Wait
                , Wait
                ]
        states = map fst outputs

testWetStatusState :: Test
testWetStatusState = checks
    [ expectEqual
        "wetStatusState changes only on turn ticks"
        [ (Dry, 0)
        , (Wet, 5)
        , (Wet, 5)
        , (Wet, 4)
        , (Wet, 3)
        , (Wet, 2)
        , (Wet, 1)
        , (Dry, 0)
        ]
        states
    , expectEqual
        "wetStatusMessages reports committed wetness edges"
        ["you are wet", "you are dry"]
        messages
    , expectEqual
        "wetStatusState continues an existing partial countdown"
        [(Wet, 2), (Wet, 1), (Dry, 0)]
        resumedStates
    , expectEqual
        "wetStatusState reads its duration from Reader"
        [(Wet, 2), (Wet, 1), (Dry, 0)]
        shortStates
    ]
    where
        states =
            runReader
                (embed
                    (wetStatusState Dry 0)
                    [ (Wet, Nothing)
                    , (Wet, Just TurnTick)
                    , (Dry, Nothing)
                    , (Dry, Just TurnTick)
                    , (Dry, Just TurnTick)
                    , (Dry, Just TurnTick)
                    , (Dry, Just TurnTick)
                    , (Dry, Just TurnTick)
                    ])
                defaultGameSettings
        (_, messages) =
            runWriter
                $ embed
                    (wetStatusMessages Dry)
                    (map fst states)
        resumedStates =
            runReader
                (embed
                    (wetStatusState Wet 3)
                    [ (Dry, Just TurnTick)
                    , (Dry, Just TurnTick)
                    , (Dry, Just TurnTick)
                    ])
                defaultGameSettings
        shortStates =
            runReader
                (embed
                    (wetStatusState Dry 0)
                    [ (Wet, Just TurnTick)
                    , (Dry, Just TurnTick)
                    , (Dry, Just TurnTick)
                    ])
                defaultGameSettings { gsWetDurationTurns = 2 }

testWaitTurn :: Test
testWaitTurn =
    expectEqual
        "Wait preserves position and advances clocks and NPCs"
        ((3, 3), 1, [(2, 1)], 4)
        ( plPos (stPlayer waited)
        , stTurnNumber waited
        , map npcPosition (Map.elems $ stNpcs waited)
        , plWetCountdown (stPlayer waited)
        )
    where
        npc = initNpc
            (1, 1)
            Rat
            (patrol Horizontal (1, 3))
            emptyItemStack
        init =
            setWet Wet 5
            $ setNpcs (Map.singleton (NpcId 10) npc)
            $ emptyGameState (3, 3) testDungeon
        waited = singleSample (runScriptedGame init [Wait])

testItemTransfers :: Test
testItemTransfers = checks
    [ expectEqual
        "popItem rejects an empty stack"
        Nothing
        (popItem emptyItemStack)
    , expectEqual
        "popItem removes the front item"
        (Just (ItemId 1, redItem, [blueItem]))
        (fmap popResult (popItem (testStack 1 [redItem, blueItem])))
    , expectEqual
        "pushItem inserts at the front"
        [ringItem, redItem, blueItem]
        (itemStackItems
            (pushItem
                (ItemId 3)
                ringItem
                (testStack 1 [redItem, blueItem])))
    , expectEqual
        "pushItem moves an existing identity without duplicating it"
        [ItemId 2, ItemId 1]
        (itemStackIds
            (pushItem
                (ItemId 2)
                blueItem
                (testStack 1 [redItem, blueItem])))
    , expectEqual
        "item event projections preserve absent samples themselves"
        [ Nothing
        , Just (RemoveItemEvt pos Nothing (ItemId 1))
        ]
        (runIdentity
            $ embed
                floorEvtsFromPickup
                [ Nothing
                , Just
                    PickupEvt
                        { pickupPosition = pos
                        , pickupContainer = Nothing
                        , pickupItemId = ItemId 1
                        , pickupItem = redItem
                        }
                ])
    , expectEqual
        "Pick transfers the selected floor identity and value to inventory"
        ( [(ItemId 1, redItem)]
        , [(ItemId 2, redItem)]
        )
        ( itemStackToList (itemsAt pos (stFloorItems picked))
        , itemStackToList (inventoryOf picked)
        )
    , expectEqual
        "Drop transfers the selected inventory identity and value to the floor"
        ( [ (ItemId 3, ringItem)
          , (ItemId 1, redItem)
          , (ItemId 2, redItem)
          ]
        , []
        )
        ( itemStackToList (itemsAt pos (stFloorItems dropped))
        , itemStackToList (inventoryOf dropped)
        )
    , expectEqual
        "successful Pick advances and failed Pick holds up"
        [1, 2, 2]
        (map stTurnNumber gameStates)
    ]
    where
        popResult ((ident, item), stack) =
            (ident, item, itemStackItems stack)
        pos = (2, 2)
        floorItems = Map.singleton pos (testStack 1 [redItem, redItem])
        gameInit = setFloor floorItems (emptyGameState pos testDungeon)
        picked = singleSample (runScriptedGame gameInit [Pick (ItemId 2)])
        dropped =
            singleSample
                $ runScriptedGame
                    (setInventory (testStack 3 [ringItem]) gameInit)
                    [Drop (ItemId 3)]
        gameStates = runScriptedGame gameInit
            [ Pick (ItemId 1)
            , Pick (ItemId 2)
            , Pick (ItemId 99)
            ]

data StackOp
    = PushStack ItemId Item
    | RemoveStack ItemId
    deriving (Show, Eq)

testItemStackInvariants :: Test
testItemStackInvariants =
    expectEqual
        "bounded push/remove sequences preserve ItemStack invariants"
        Nothing
        firstInvalidStack
    where
        operations =
            [ PushStack (ItemId 1) redItem
            , PushStack (ItemId 1) ringItem
            , PushStack (ItemId 2) blueItem
            , PushStack (ItemId 3) greenItem
            , RemoveStack (ItemId 1)
            , RemoveStack (ItemId 2)
            , RemoveStack (ItemId 3)
            ]
        operationSequences =
            concatMap
                (\len -> sequence $ replicate len operations)
                [0 .. 5]
        firstInvalidStack =
            listToMaybe
                [ (operationSequence, stack)
                | operationSequence <- operationSequences
                , let stack =
                        foldl
                            (flip applyStackOp)
                            emptyItemStack
                            operationSequence
                , not (itemStackInvariant stack)
                ]

applyStackOp :: StackOp -> ItemStack -> ItemStack
applyStackOp operation stack =
    case operation of
        PushStack ident item -> pushItem ident item stack
        RemoveStack ident -> maybe stack snd (removeItem ident stack)

itemStackInvariant :: ItemStack -> Bool
itemStackInvariant stack =
    and
        [ itemStackFromList entries == stack
        , itemStackIds stack == map fst entries
        , itemStackItems stack == map snd entries
        , itemStackSize stack == length entries
        , itemStackNull stack == null entries
        , all
            (\(ident, item) -> lookupItem ident stack == Just item)
            entries
        ]
    where
        entries = itemStackToList stack

testItemStateReducers :: Test
testItemStateReducers = checks
    [ expectEqual
        "removing the final floor item deletes its position"
        Map.empty
        removedFinalItem
    , expectEqual
        "unknown floor and inventory identities leave their owners unchanged"
        (initialFloor, initialInventory)
        (unknownFloorRemoval, unknownInventoryRemoval)
    , expectEqual
        "floor placement always prepends the item"
        [blueItem, redItem]
        (itemStackItems (itemsAt pos floorPlacement))
    , expectEqual
        "Drop reports a floor destination"
        ["dropped a ring of protection at (2,2)"]
        dropMessages
    ]
    where
        pos = (2, 2)
        initialFloor = Map.singleton pos (testStack 1 [redItem])
        initialInventory = testStack 10 [ringItem]
        removedFinalItem =
            singleSample
                $ runIdentity
                $ embed
                    (floorItemsState initialFloor)
                    [Just (RemoveItemEvt pos Nothing (ItemId 1))]
        unknownFloorRemoval =
            singleSample
                $ runIdentity
                $ embed
                    (floorItemsState initialFloor)
                    [Just (RemoveItemEvt pos Nothing (ItemId 99))]
        unknownInventoryRemoval =
            singleSample
                $ runIdentity
                $ embed
                    (inventoryState initialInventory)
                    [Just (RemoveEvt (ItemId 99))]
        floorPlacement =
            singleSample
                $ runIdentity
                $ embed
                    (floorItemsState initialFloor)
                    [ Just
                        (PlaceItemEvt
                            pos
                            (ItemId 2)
                            blueItem)
                    ]
        dropEvent =
            DropEvt
                { dropPosition = pos
                , dropItemId = ItemId 10
                , dropItem = ringItem
                }
        dropMessages = snd $ runWriter $ embed floorEvtsFromDrop [Just dropEvent]

testContainerTransfers :: Test
testContainerTransfers = checks
    [ expectEqual
        "Pick transfers a selected container with its contents"
        ( [greenItem]
        , [chestItem (testStack 10 [redItem])]
        )
        ( itemStackItems (itemsAt pos $ stFloorItems pickedContainer)
        , inventoryItems pickedContainer
        )
    , expectEqual
        "Loot transfers only the selected item from the selected container"
        ( [blueItem]
        , [redItem]
        )
        ( itemStackItems (firstContainerItems pos looted)
        , inventoryItems looted
        )
    , expectEqual
        "Loot reports the selected source container"
        ["looted a red gem from the chest at (2,2)"]
        lootedMessages
    , expectEqual
        "corpses are pickable together with their contents"
        [corpseItem Goblin (testStack 30 [ringItem])]
        (inventoryItems pickedCorpse)
    , expectEqual
        "Loot validates the selected container identity"
        (0, [], floorBeforeInvalidLoot)
        ( stTurnNumber invalidLoot
        , inventoryItems invalidLoot
        , stFloorItems invalidLoot
        )
    , expectEqual
        "Drop remains on the floor beside containers"
        ( [ringItem, chestItem emptyItemStack]
        , []
        )
        ( itemStackItems (itemsAt pos $ stFloorItems dropped)
        , inventoryItems dropped
        )
    , expectEqual
        "container removal is scoped to the selected container"
        (Just
            [ chestItem (testStack 10 [redItem])
            , chestItem emptyItemStack
            ])
        (itemStackItems
            <$> removeFromContainer
                (ItemId 2)
                (ItemId 20)
                nestedRemovalStack)
    ]
    where
        pos = (2, 2)
        base =
            setFloor
                (Map.singleton pos
                    (testStack 1
                        [ greenItem
                        , chestItem (testStack 10 [redItem])
                        ]))
                (emptyGameState pos testDungeon)
        pickedContainer = singleSample
            $ runScriptedGame base [Pick (ItemId 2)]
        (looted, lootedMessages) = singleSample
            $ runScriptedGameWithMessages
                (setFloor
                    (Map.singleton pos
                        (testStack 1
                            [chestItem (testStack 10 [redItem, blueItem])]))
                    (emptyGameState pos testDungeon))
                [Loot (ItemId 1) (ItemId 10)]
        pickedCorpse = singleSample
            $ runScriptedGame
                (setFloor
                    (Map.singleton pos
                        (testStack 1
                            [corpseItem Goblin (testStack 30 [ringItem])]))
                    (emptyGameState pos testDungeon))
                [Pick (ItemId 1)]
        floorBeforeInvalidLoot =
            Map.singleton pos
                (testStack 1 [chestItem (testStack 10 [redItem])])
        invalidLoot = singleSample
            $ runScriptedGame
                (setFloor floorBeforeInvalidLoot
                    $ emptyGameState pos testDungeon)
                [Loot (ItemId 99) (ItemId 10)]
        dropped =
            singleSample
                $ runScriptedGame
                    (setInventory (testStack 20 [ringItem])
                        $ setFloor
                            (Map.singleton pos
                                (testStack 1 [chestItem emptyItemStack]))
                            (emptyGameState pos testDungeon))
                    [Drop (ItemId 20)]
        nestedRemovalStack =
            testStack 1
                [ chestItem (testStack 10 [redItem])
                , chestItem (testStack 20 [blueItem])
                ]

testNpcKillEventStreams :: Test
testNpcKillEventStreams = checks
    [ expectEqual
        "NPC kill lookup emits only for an occupied position"
        [ Nothing
        , Just killed
        , Nothing
        ]
        killedEvents
    , expectEqual
        "the NPC owner consumes the kill without retaining the killed NPC"
        [population, Map.singleton survivorId survivor]
        populations
    ]
    where
        targetPos = (2, 2)
        survivorPos = (4, 4)
        targetId = NpcId 1
        survivorId = NpcId 2
        target = initNpc targetPos Goblin Stationary emptyItemStack
        survivor = initNpc survivorPos Rat Stationary emptyItemStack
        population =
            Map.fromList
                [ (targetId, target)
                , (survivorId, survivor)
                ]
        killed =
            NpcKilledEvt
                { killedNpcId = targetId
                , killedNpc = target
                }
        killedEvents =
            runIdentity
                $ embed
                    npcKilledEvts
                    [ ((1, 1), population)
                    , (targetPos, population)
                    , ((3, 3), population)
                    ]
        populations =
            runIdentity
                $ embed
                    (npcsState testDungeon population)
                    [ ((1, 1), Nothing)
                    , ((1, 1), Just killed)
                    ]

testPlayerAttack :: Test
testPlayerAttack = checks
    [ expectEqual
        "attacking an occupied tile leaves the player in place and advances"
        ((1, 2), 1)
        ( plPos (stPlayer state)
        , stTurnNumber state
        )
    , expectEqual
        "the target is removed while surviving NPCs act"
        [(NpcId 2, (3, 2))]
        [ (ident, npcPosition npc)
        | (ident, npc) <- Map.toList (stNpcs state)
        ]
    , expectEqual
        "the killed NPC leaves a deterministic loot-bearing corpse"
        [corpseItem Goblin (testStack 101 [blueItem, ringItem])]
        (itemStackItems (itemsAt (2, 2) (stFloorItems state)))
    , expectEqual
        "corpse allocation uses one fresh identity"
        ([ItemId 10000], ItemId 10001)
        ( itemStackIds (itemsAt (2, 2) (stFloorItems state))
        , stNextItemId state
        )
    , expectEqual
        "the kill consumes randomness exactly once"
        (RandomSeed 11960119808228829710)
        (stRandomSeed state)
    , expectEqual
        "a killed participant is absent from fight mode"
        Exploring
        (plFightMode (stPlayer state))
    , expectEqual
        "the kill message is observational writer output"
        ["you kill the goblin at (2,2)"]
        msgs
    , expectEqual
        "a later non-spawning turn preserves the next item ID"
        (ItemId 10001)
        (stNextItemId afterWait)
    ]
    where
        target = initNpc
            (2, 2)
            Goblin
            Stationary
            (testStack 100 [redItem, blueItem, ringItem])
        survivor = initNpc
            (3, 1)
            Rat
            (patrol Vertical (1, 3))
            emptyItemStack
        init =
            setFight (Fighting (Set.singleton (NpcId 1)))
            $ setNpcs
                (Map.fromList
                    [ (NpcId 1, target)
                    , (NpcId 2, survivor)
                    ])
            $ emptyGameState (1, 2) testDungeon
        (state, msgs) =
            singleSample (runScriptedGameWithMessages init [Move East])
        afterWait =
            last (runScriptedGame init [Move East, Wait])

testDeterministicLoot :: Test
testDeterministicLoot = checks
    [ expectEqual
        "drawBool advances its retained random seed"
        (False, RandomSeed 7806831264735756412)
        (runState drawBool (RandomSeed 1))
    , expectEqual
        "loot selection remains stable for replay"
        ( [blueItem, ringItem]
        , RandomSeed 11960119808228829710
        )
        first
    , expectEqual
        "loot selection always preserves inventory order"
        Nothing
        (listToMaybe
            [ (seed, inventory, selected)
            | seed <- [0 .. 64]
            , inventory <- testInventories
            , let (selected, _) =
                    runState
                        (selectRandomSubset inventory)
                        (RandomSeed seed)
            , not (selected `isSubsequenceOf` inventory)
            ])
    ]
    where
        first = runState (selectRandomSubset items) (RandomSeed 1)
        items = [redItem, blueItem, ringItem]
        testInventories =
            [ []
            , [redItem]
            , [redItem, blueItem]
            , [ringItem, redItem, blueItem, greenItem]
            ]

testEncounterMessages :: Test
testEncounterMessages = checks
    [ expectEqual
        "movement observes loose items and containers"
        expected
        (sort movedMsgs)
    , expectEqual
        "Inspect repeats observations without advancing"
        (expected, 0)
        (sort inspectedMsgs, stTurnNumber inspected)
    ]
    where
        pos = (2, 2)
        floorItems =
            Map.singleton pos
                (testStack 1
                    [ redItem
                    , blueItem
                    , chestItem (testStack 10 [greenItem])
                    ])
        init = setFloor floorItems (emptyGameState (1, 2) testDungeon)
        (_, movedMsgs) =
            singleSample (runScriptedGameWithMessages init [Move East])
        (inspected, inspectedMsgs) =
            singleSample (runScriptedGameWithMessages
                (setFloor floorItems (emptyGameState pos testDungeon))
                [Inspect])
        expected = sort
            [ "you see 2 gems (red, blue) at (2,2)"
            , "you see a chest containing 1 item at (2,2)"
            ]

testGameView :: Test
testGameView =
    expectEqual
        "gameView accumulates writer output in chronological order"
        [ ["you see 1 gem (red) at (2,2)"]
        , [ "you see 1 gem (red) at (2,2)"
          , "picked up a red gem at (2,2)"
          ]
        ]
        (map vMessages views)
    where
        state =
            setFloor
                (Map.singleton (2, 2) (testStack 1 [redItem]))
                (emptyGameState (2, 2) testDungeon)
        views = runViews state [Inspect, Pick (ItemId 1)]

testInput :: Test
testInput =
    expectEqual
        "direct dungeon bindings exclude modal item commands"
        [ Just (PlayTurn (Move West))
        , Just (PlayTurn (Move South))
        , Just (PlayTurn (Move North))
        , Just (PlayTurn (Move East))
        , Just (PlayTurn Wait)
        , Nothing
        , Just (PlayTurn Inspect)
        , Just Quit
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        , Nothing
        ]
        (map parseInput "hjkl. \nqpdLiz")

testConsoleProtocol :: Test
testConsoleProtocol = checks
    [ expectEqual
        "console parsing trims lines and keeps commands case-sensitive"
        [ ConsoleMove West
        , ConsoleMove South
        , ConsoleMove North
        , ConsoleMove East
        , ConsoleWait
        , ConsoleSense
        , ConsoleCharacter
        , ConsoleQuit
        , ConsoleInvalid
        , ConsoleInvalid
        ]
        (map parseConsoleCommand
            [" h ", "j", "k", "l", ".", "s", "c", "q", "", "L"])
    , expectEqual
        "console turns become keyed NPC decisions paired with player Wait"
        [ Just (Wait, Map.singleton controlledId (1, 0))
        , Just (Wait, Map.singleton controlledId (0, 0))
        ]
        [ consoleGameInput controlledId view (ConsoleMove East)
        , consoleGameInput controlledId view ConsoleWait
        ]
    , expectEqual
        "sensing reports a fixed square with oriented occupancy and terrain"
        (Just (11, replicate 11 11, '.', '@', 'r', '#', '_'))
        sensorSnapshot
    , expectEqual
        "protocol blocks separate readiness, character data, errors, and quit"
        [ ["ready version=1 kind=adder radius=5"]
        , ["ok turn=7"]
        , ["ok turn=7"]
        , [ "character kind=adder"
          , "vitals hp=10 mp=0 hunger=0"
          , "stats str=10 int=10 dex=10 con=10"
          ]
        , ["error unknown-command"]
        , ["bye"]
        ]
        [ readyLines controlled
        , responseLines controlledId (ConsoleMove East) view
        , responseLines controlledId ConsoleWait view
        , responseLines controlledId ConsoleCharacter view
        , responseLines controlledId ConsoleInvalid view
        , responseLines controlledId ConsoleQuit view
        ]
    , expectEqual
        "an unavailable actor neither moves nor senses"
        (Nothing, Nothing, ["error actor-unavailable"])
        ( consoleGameInput missingId view (ConsoleMove East)
        , senseLines missingId view
        , responseLines missingId ConsoleSense view
        )
    , expectEqual
        "child turn results distinguish admitted and held samples"
        [["ok turn=8"], ["held turn=7"]]
        [ turnResultLines view (view { vTurnNumber = 8 })
        , turnResultLines view view
        ]
    ]
    where
        controlledId = NpcId 3
        missingId = NpcId 99
        controlled = initNpc (2, 2) Adder Stationary emptyItemStack
        neighbor = initNpc (3, 2) Rat Stationary emptyItemStack
        dungeon = testDungeon // [((2, 3), '#'), ((1, 2), ' ')]
        state =
            ( setFloor
                (Map.singleton (1, 2) (testStack 1 [ringItem]))
            $ setNpcs
                (Map.fromList
                    [ (controlledId, controlled)
                    , (NpcId 4, neighbor)
                    ])
            $ emptyGameState (2, 1) dungeon
            ) { stTurnNumber = 7 }
        view = toGameView [] state
        sensorSnapshot = do
            header : rows <- senseLines controlledId view
            if header /= "area pos=2,2"
                then Nothing
                else pure
                    ( length rows
                    , map length rows
                    , rows !! 5 !! 5
                    , rows !! 4 !! 5
                    , rows !! 5 !! 6
                    , rows !! 6 !! 5
                    , rows !! 5 !! 4
                    )

testAttackConfirmation :: Test
testAttackConfirmation = checks
    [ expectEqual
        "movement toward an NPC opens confirmation and logs its name"
        ( [AttackConfirmation East, DungeonScreen]
        , Just (LogMessage (attackConfirmationMessage Goblin))
        )
        prompted
    , expectEqual
        "lowercase y and Enter release the pending attack"
        [ ([DungeonScreen], Just (PlayTurn (Move East)))
        , ([DungeonScreen], Just (PlayTurn (Move East)))
        ]
        (map respond [CharKey 'y', CharKey '\n'])
    , expectEqual
        "every other key cancels the pending attack"
        (replicate 5 ([DungeonScreen], Nothing))
        (map respond
            [ CharKey 'n'
            , CharKey 'Y'
            , CharKey 'q'
            , OtherKey
            , EscapeKey
            ])
    , expectEqual
        "ordinary unoccupied movement bypasses confirmation"
        ([DungeonScreen], Just (PlayTurn (Move West)))
        (uiStep (CharKey 'h') view [DungeonScreen])
    ]
    where
        playerPos = (2, 2)
        view = toGameView []
            $ setNpcs
                (Map.singleton
                    (NpcId 1)
                    (initNpc (3, 2) Goblin Stationary emptyItemStack))
            $ emptyGameState playerPos testDungeon
        prompted = uiStep (CharKey 'l') view [DungeonScreen]
        respond key = uiStep key view (fst prompted)

testItemInteractionUI :: Test
testItemInteractionUI = checks
    [ expectEqual
        "choice pages contain fifteen entries and restart labels"
        (15, [('a', 16 :: Int)])
        (choicePageSize, pageChoices 1 [1 .. 16])
    , expectEqual
        "inventory paging, selection, and nested Escape form one pure trace"
        ( [InventoryScreen 0, DungeonScreen]
        , [InventoryScreen 1, DungeonScreen]
        , [InventoryScreen 0, DungeonScreen]
        , [ItemScreen (ItemId 16), InventoryScreen 1, DungeonScreen]
        , [InventoryScreen 1, DungeonScreen]
        , [DungeonScreen]
        )
        ( inventory0
        , inventory1
        , inventoryWrapped
        , detail
        , backToPage
        , backToDungeon
        )
    , expectEqual
        "the second page renders the same selector association"
        [ "Inventory (page 2/2)"
        , "a) red gem"
        , "b) short sword (one-handed melee weapon)"
        , "Space: next page  Esc: back"
        ]
        (screenLines view $ InventoryScreen 1)
    , expectEqual
        "modal choice flows emit exact item identities"
        [ Just (PlayTurn $ Drop (ItemId 1))
        , Just (PlayTurn $ Pick (ItemId 20))
        , Just (PlayTurn $ Loot (ItemId 21) (ItemId 30))
        ]
        [ commandFrom (DropScreen 0) 'a'
        , commandFrom (PickupScreen 0) 'a'
        , commandFrom (LootItemsScreen (ItemId 21) 0) 'a'
        ]
    , expectEqual
        "Pickup reports none, submits one, and selects among several"
        ( ([DungeonScreen], Just (LogMessage nothingToPickupMessage))
        , ([DungeonScreen], Just (PlayTurn (Pick (ItemId 20))))
        , [PickupScreen 0, DungeonScreen]
        )
        ( uiStep (CharKey 'p') emptyLootView root
        , uiStep (CharKey 'p') singleItemView root
        , fst (uiStep (CharKey 'p') view root)
        )
    , expectEqual
        "Loot skips selection for one container and Escape returns to the dungeon"
        ( [LootItemsScreen (ItemId 21) 0, DungeonScreen]
        , [DungeonScreen]
        )
        (lootItems, lootBack)
    , expectEqual
        "Loot reports an empty source without opening a screen"
        ( [DungeonScreen]
        , Just (LogMessage nothingToLootMessage)
        )
        (uiStep (CharKey 'L') emptyLootView root)
    , expectEqual
        "Loot retains container selection when there are several choices"
        [LootContainersScreen 0, DungeonScreen]
        (fst $ uiStep (CharKey 'L') multipleContainersView root)
    , expectEqual
        "frontend notices persist without advancing the held game state"
        ( [0, 0, 1, 1]
        , [ []
          , [nothingToLootMessage]
          , [nothingToLootMessage]
          , [nothingToLootMessage]
          ]
        )
        (map vTurnNumber heldViews, map vMessages heldViews)
    , expectEqual
        "item details offer only valid actions"
        ( Just (PlayTurn $ Drop (ItemId 1))
        , Nothing
        , Just (PlayTurn $ Wield (ItemId 40))
        )
        ( detailCommand (ItemId 1) 'd'
        , detailCommand (ItemId 1) 'w'
        , detailCommand (ItemId 40) 'w'
        )
    ]
    where
        pos = (2, 2)
        inventory = itemStackFromList
            $ [ (ItemId ident, redItem) | ident <- [1 .. 16] ]
            ++ [(ItemId 40, WeaponItem shortSword)]
        floorItems = Map.singleton pos
            $ itemStackFromList
                [ (ItemId 20, blueItem)
                , (ItemId 21, chestItem $ testStack 30 [redItem])
                ]
        view = toGameView []
            $ setInventory inventory
            $ setFloor floorItems
            $ emptyGameState pos testDungeon
        root = [DungeonScreen]
        inventory0 = fst $ uiStep (CharKey 'i') view root
        inventory1 = fst $ uiStep (CharKey ' ') view inventory0
        inventoryWrapped = fst $ uiStep (CharKey ' ') view inventory1
        detail = fst $ uiStep (CharKey 'a') view inventory1
        backToPage = fst $ uiStep EscapeKey view detail
        backToDungeon = fst $ uiStep EscapeKey view backToPage
        commandFrom mode key =
            snd $ uiStep (CharKey key) view [mode, DungeonScreen]
        detailCommand ident key = commandFrom (ItemScreen ident) key
        lootItems = fst $ uiStep (CharKey 'L') view root
        lootBack = fst $ uiStep EscapeKey view lootItems
        emptyLootView = toGameView [] $ emptyGameState pos testDungeon
        singleItemView = toGameView []
            $ setFloor
                (Map.singleton pos
                    $ itemStackFromList [(ItemId 20, blueItem)])
            $ emptyGameState pos testDungeon
        multipleContainersView = toGameView []
            $ setFloor
                (Map.singleton pos
                    $ itemStackFromList
                        [ (ItemId 21, chestItem emptyItemStack)
                        , (ItemId 22, corpseItem Goblin emptyItemStack)
                        ])
            $ emptyGameState pos testDungeon
        heldViews = runIdentity $ embed
            (Game.gameViewInput $ emptyGameState pos testDungeon)
            [ (Nothing, [])
            , (Nothing, [nothingToLootMessage])
            , (Just Wait, [])
            , (Nothing, [])
            ]

testWielding :: Test
testWielding = checks
    [ expectEqual
        "wieldedState owns weapon replacement and Drop recurrence"
        (Just [Just (ItemId 10), Just (ItemId 12), Nothing])
        directStates
    , expectEqual
        "Wield validates weapons, replaces the slot, and Drop clears it"
        ( [ Just (ItemId 10)
          , Just (ItemId 10)
          , Just (ItemId 12)
          , Nothing
          ]
        , [1, 1, 2, 3]
        )
        ( map (plWielded . stPlayer) states
        , map stTurnNumber states
        )
    , expectEqual
        "dropping the wielded weapon moves that exact identity to the floor"
        ([(ItemId 12, WeaponItem shortBow)], [ItemId 10, ItemId 11])
        ( itemStackToList (itemsAt pos $ stFloorItems final)
        , itemStackIds (inventoryOf final)
        )
    ]
    where
        pos = (2, 2)
        inventory = itemStackFromList
            [ (ItemId 10, WeaponItem shortSword)
            , (ItemId 11, ringItem)
            , (ItemId 12, WeaponItem shortBow)
            ]
        states = runScriptedGame
            (setInventory inventory $ emptyGameState pos testDungeon)
            [ Wield (ItemId 10)
            , Wield (ItemId 11)
            , Wield (ItemId 12)
            , Drop (ItemId 12)
            ]
        final = last states
        directStates = either (const Nothing) Just
            $ runIdentity
            $ runExceptT
            $ embed
                (wieldedState Nothing)
                [ (Wield (ItemId 10), inventory)
                , (Wield (ItemId 12), inventory)
                , (Drop (ItemId 12), inventory)
                ]

testItemPresentation :: Test
testItemPresentation = checks
    [ expectEqual
        "portable item descriptions remain stable"
        [ "red gem"
        , "ring of protection"
        , "short sword (one-handed melee weapon)"
        , "plate mail (heavy armour)"
        ]
        (map itemDescription
            [redItem, ringItem, shortSwordItem, heavyArmourItem])
    , expectEqual
        "container descriptions and glyphs distinguish chest and corpse"
        [("chest", 'C'), ("corpse of the goblin", ';')]
        [ (itemDescription item, itemGlyph item)
        | item <-
            [ chestItem emptyItemStack
            , corpseItem Goblin emptyItemStack
            ]
        ]
    ]

testRendering :: Test
testRendering = checks
    [ expectEqual
        "terrain renders floor as dot and vacuum as space"
        ('.', ' ')
        ( gameViewCell (toGameView [] base) (1, 1)
        , gameViewCell vacuumView (1, 1)
        )
    , expectEqual
        "NPCs render over floor stacks"
        'g'
        (gameViewCell npcView pos)
    , expectEqual
        "the first item in a floor stack is rendered"
        '%'
        (gameViewCell itemView pos)
    , expectEqual
        "the side panel shows vitals and character stats vertically"
        [ "Player"
        , "Position: (2,2)"
        , "HP: 10"
        , "MP: 10"
        , "Hunger: 0"
        , "Strength: 10"
        , "Intelligence: 10"
        , "Dexterity: 10"
        , "Constitution: 10"
        , "Items: 0"
        ]
        (playerStatsLines $ toGameView [] base)
    ]
    where
        pos = (2, 2)
        base = emptyGameState pos testDungeon
        vacuumView = toGameView []
            $ emptyGameState pos
            $ layoutDungeon
            $ compose 1.0
            $ room (2, 2) (6, 6)
        itemState = setFloor
            (Map.singleton pos
                (testStack 1 [redItem, chestItem emptyItemStack]))
            base
        itemView = toGameView [] itemState
        npcView =
            toGameView []
            $ setNpcs
                (Map.singleton (NpcId 3)
                    (initNpc
                        pos
                        Goblin
                        Stationary
                        emptyItemStack))
                itemState

testMessageLogLines :: Test
testMessageLogLines = checks
    [ expectEqual
        "short logs are top padded"
        ["", "", "", "", "a", "b"]
        (messageLogLines ["a", "b"])
    , expectEqual
        "only the latest six messages are rendered"
        ["2", "3", "4", "5", "6", "7"]
        (messageLogLines ["1", "2", "3", "4", "5", "6", "7"])
    ]

testViewport :: Test
testViewport = checks
    [ expectEqual
        "viewport clamps independently at map edges"
        [ (1, 1, 4, 3)
        , (7, 1, 10, 3)
        , (7, 8, 10, 10)
        , (1, 8, 4, 10)
        ]
        (runViewport
            (4, 3)
            (1, 1)
            (10, 10)
            [(2, 2), (9, 2), (9, 9), (2, 9)])
    , expectEqual
        "viewport scrolls at padding"
        [(1, 1, 6, 5), (2, 1, 7, 5), (1, 1, 6, 5)]
        (runViewport (6, 5) (2, 1) (20, 20) [(3, 3), (5, 3), (3, 3)])
    , expectEqual
        "viewport keeps screen dimensions for a small dungeon"
        [(1, 1, 6, 5), (1, 1, 6, 5)]
        (runViewport (6, 5) (2, 1) (3, 2) [(2, 1), (3, 2)])
    ]

testStack :: Word64 -> [Item] -> ItemStack
testStack firstId items =
    itemStackFromList
        (zip (map ItemId [firstId ..]) items)

layoutItemIds :: DungeonLayout -> [ItemId]
layoutItemIds layout =
    concatMap stackItemIds (Map.elems (layoutItems layout))
    ++ concatMap
        (stackItemIds . npcInventory)
        (Map.elems (layoutNpcs layout))
    where
        stackItemIds stack =
            concatMap entryIds (itemStackToList stack)
        entryIds (ident, item) =
            ident :
                case item of
                    ContainerItem container ->
                        stackItemIds (containerItems container)
                    _ -> []

redItem :: Item
redItem = GemItem redGem

greenItem :: Item
greenItem = GemItem greenGem

blueItem :: Item
blueItem = GemItem blueGem

ringItem :: Item
ringItem = RingItem ringOfProtection

potionItem :: Item
potionItem = PotionItem potionOfHealing

bookItem :: Item
bookItem = BookItem bookOfForgottenPaths

scrollItem :: Item
scrollItem = ScrollItem scrollOfMapping

shortSwordItem :: Item
shortSwordItem = WeaponItem shortSword

greatAxeItem :: Item
greatAxeItem = WeaponItem greatAxe

shortBowItem :: Item
shortBowItem = WeaponItem shortBow

lightArmourItem :: Item
lightArmourItem = ArmourItem leatherJerkin

heavyArmourItem :: Item
heavyArmourItem = ArmourItem plateMail

testDungeon :: Dungeon
testDungeon = listArray ((1, 1), (5, 5)) (repeat '.')

wideDungeon :: Dungeon
wideDungeon = layoutDungeon $ compose 1.0 $ room (1, 1) (10, 4)

wallDungeon :: Dungeon
wallDungeon =
    listArray
        ((1, 1), (3, 3))
        [ '.', '.', '.'
        , '.', '#', '.'
        , '.', '.', '.'
        ]

wallAtThreeDungeon :: Dungeon
wallAtThreeDungeon =
    listArray
        ((1, 1), (5, 5))
        [ if pos == (3, 1) then '#' else '.'
        | pos <- (range ((1, 1), (5, 5)) :: [Position])
        ]

waterDungeon :: Dungeon
waterDungeon = layoutDungeon $ compose 1.0 $ do
    room (1, 1) (10, 4)
    water (3, 2)

emptyGameState :: Position -> Dungeon -> GameState
emptyGameState pos dungeon =
    GameState
        { stPlayer = initPlayer pos
        , stDungeon = dungeon
        , stFloorItems = Map.empty
        , stNpcs = Map.empty
        , stTurnNumber = 0
        , stRandomSeed = RandomSeed 1
        , stNextItemId = ItemId 10000
        }

setFloor :: FloorItems -> GameState -> GameState
setFloor floorItems state =
    state { stFloorItems = floorItems }

setNpcs :: NpcPopulation -> GameState -> GameState
setNpcs npcs state =
    state { stNpcs = npcs }

setInventory :: Inventory -> GameState -> GameState
setInventory inventory state =
    state
        { stPlayer =
            (stPlayer state)
                { plInventory = inventory }
        }

setWet :: WetStatus -> Int -> GameState -> GameState
setWet status count state =
    state
        { stPlayer =
            (stPlayer state)
                { plWetStatus = status
                , plWetCountdown = count
                }
        }

setFight :: FightMode -> GameState -> GameState
setFight mode state =
    state
        { stPlayer =
            (stPlayer state) { plFightMode = mode }
        }

inventoryOf :: GameState -> Inventory
inventoryOf = plInventory . stPlayer

inventoryItems :: GameState -> [Item]
inventoryItems = itemStackItems . inventoryOf

containerStacks :: Position -> GameState -> [ItemStack]
containerStacks pos state =
    [ containerItems container
    | ContainerItem container <-
        itemStackItems (itemsAt pos (stFloorItems state))
    ]

firstContainerItems :: Position -> GameState -> ItemStack
firstContainerItems pos state =
    case containerStacks pos state of
        items : _ -> items
        [] -> emptyItemStack

runViews :: GameState -> [Turn] -> [GameView]
runViews init = go (Game.gameView init)
    where
        go _ [] = []
        go msf (turn : rest) =
            let (view, next) = runIdentityMSF msf turn
            in view : go next rest

runIdentityMSF :: MSF Identity a b -> a -> (b, MSF Identity a b)
runIdentityMSF msf input = runIdentity (unMSF msf input)

data ViewportConfig = ViewportConfig
    { viewportDims :: (Int, Int)
    , viewportPadding :: (Int, Int)
    }

instance HasVty ViewportConfig where
    getVty _ = error "viewport tests do not use Vty"
    getViewportDims = viewportDims
    getPadding = viewportPadding

runViewport
    :: (Int, Int)
    -> (Int, Int)
    -> (Int, Int)
    -> [(Int, Int)]
    -> [(Int, Int, Int, Int)]
runViewport viewportDims' padding dungeonDims positions =
    runReader (go viewport positions) (ViewportConfig viewportDims' padding)
    where
        go
            :: MSF
                 (Reader ViewportConfig)
                 ((Int, Int), (Int, Int))
                 (Int, Int, Int, Int)
            -> [(Int, Int)]
            -> Reader ViewportConfig [(Int, Int, Int, Int)]
        go _ [] = pure []
        go msf (pos : rest) = do
            (cur, next) <- unMSF msf (pos, dungeonDims)
            (cur :) <$> go next rest

testDoOnceTiming :: Test
testDoOnceTiming =
    case runExceptSample (doOnce (arr (+ (1 :: Int)))) 1 of
        Right (first, next) -> checks
            [ expectEqual
                "doOnce emits its wrapped first sample"
                2
                first
            , expectEqual
                "doOnce terminates on the following sample"
                (Left ())
                (fst <$> runExceptSample next 2)
            ]
        Left () ->
            ["doOnce terminated on its first sample"]

testOneTickThenTiming :: Test
testOneTickThenTiming =
    expectEqual
        "oneTickThen starts its second MSF on the following sample"
        [10, 1, 2]
        (runIdentity $ embed switched [(), (), ()])
    where
        switched = oneTickThen (arr (const 10)) counter
        counter :: MSF Identity () Int
        counter = arr (const 1) >>> accumulateWith (+) 0

testRunMaybeStateS :: Test
testRunMaybeStateS = checks
    [ expectEqual
        "runMaybeStateS commits only present MSF and State updates"
        [Just (1, 1), Nothing, Just (2, 2)]
        results
    , expectEqual
        "runMaybeStateS restores underlying State after Nothing"
        2
        finalState
    ]
    where
        (results, finalState) =
            runState
                (embed
                    (runMaybeStateS $ exceptToMaybeS attempted)
                    [False, True, False])
                0

        attempted :: MSF (ExceptT () (State Int)) Bool (Int, Int)
        attempted =
            ( (arr (const (1 :: Int)) >>> accumulateWith (+) 0)
                &&& arr id
            )
                >>> arrM
                    (\(count, reject) -> do
                        modify (+ 1)
                        current <- get
                        if reject
                            then throwE ()
                            else pure (count, current))

testTurnNumber :: Test
testTurnNumber =
    expectEqual
        "turnNumber accumulates only sampled turn ticks"
        [7, 8, 8, 9]
        (runIdentity $ embed (turnNumber 7) ticks)
    where
        ticks = [Nothing, Just TurnTick, Nothing, Just TurnTick]

testCountdownFrom :: Test
testCountdownFrom =
    expectEqual
        "countdownFrom counts samples and holds at zero"
        [3, 2, 1, 0, 0]
        (runIdentity
            $ embed
                (countdownFrom 3)
                [(), (), (), (), ()])

testRestartOn :: Test
testRestartOn = checks
    [ expectEqual
        "restartOn advances its initial MSF and restarts the replacement"
        [5, 3, 2, 3, 2]
        (runIdentity $ embed restarted boolInputs)
    , expectEqual
        "restartOnEvt can replace its initial MSF on the first sample"
        [3, 2]
        (runIdentity
            $ embed
                restartedEvt
                [((), Just ()), ((), Nothing)])
    , expectEqual
        "restartOn steps an effectful MSF once on a reset sample"
        ([0, 1, 2, 3, 4], 4)
        (runState
            (embed (restartOn (arr $ const 0) effectful) boolInputs)
            0)
    ]
    where
        restarted = restartOn (countdownFrom 5) (countdownFrom 3)
        restartedEvt = restartOnEvt (countdownFrom 5) (countdownFrom 3)
        effectful :: MSF (State Int) () Int
        effectful = arrM $ \() -> do
            modify (+ 1)
            get
        boolInputs =
            [ ((), False)
            , ((), True)
            , ((), False)
            , ((), True)
            , ((), False)
            ]

testSampleAndHold :: Test
testSampleAndHold =
    expectEqual
        "sampleAndHold pauses state and holds the latest output"
        [0, 1, 1, 2, 2]
        (runIdentity $ embed sampledCounter inputs)
    where
        sampledCounter = sampleAndHold 0 counter
        counter = arr (const (1 :: Int)) >>> accumulateWith (+) 0
        inputs = [Nothing, Just (), Nothing, Just (), Nothing]

runExceptSample
    :: MSF (ExceptT () Identity) a b
    -> a
    -> Either () (b, MSF (ExceptT () Identity) a b)
runExceptSample msf input =
    runIdentity (runExceptT (unMSF msf input))

runScriptedGame :: GameState -> [Turn] -> [GameState]
runScriptedGame = Game.runScriptedGame

runScriptedGameWithMessages
    :: GameState
    -> [Turn]
    -> [(GameState, [String])]
runScriptedGameWithMessages =
    Game.runScriptedGameWithMessages

boundsOf :: Dungeon -> (Position, Position)
boundsOf = bounds

testConsoleBlackBox :: IO Test
testConsoleBlackBox = do
    (status, output, errors) <-
        readCreateProcessWithExitCode
            (proc "dungeon-exe" ["--console"])
            "s\nc\nk\nk\nh\nh\n.\ns\nx\nq\n"
    (eofStatus, eofOutput, eofErrors) <-
        readCreateProcessWithExitCode
            (proc "dungeon-exe" ["--console"])
            ""
    (usageStatus, usageOutput, usageErrors) <-
        readCreateProcessWithExitCode
            (proc "dungeon-exe" ["--unknown"])
            ""
    (agentStatus, agentOutput, agentErrors) <-
        readCreateProcessWithExitCode
            (proc "dungeon-exe"
                [ "--agent", "99", "--"
                , "unused-child"
                ])
            ""
    let blocks = responseBlocks output
    pure $ checks
        [ expectEqual
            "the piped console transcript has deterministic blocks and timing"
            ( Just
                ( [ "ready version=1 kind=adder radius=5"
                  , "area pos=8,8"
                  , "character kind=adder"
                  , "ok turn=1"
                  , "ok turn=2"
                  , "ok turn=3"
                  , "ok turn=4"
                  , "ok turn=5"
                  , "area pos=7,6"
                  , "error unknown-command"
                  , "bye"
                  ]
                , [1, 12, 3, 1, 1, 1, 1, 1, 12, 1, 1]
                , "_#.@......."
                , "__#.@......"
                )
            )
            (transcriptSnapshot blocks)
        , expectEqual
            "console mode exits successfully without stderr or ANSI output"
            (ExitSuccess, "", False)
            (status, errors, '\ESC' `elem` output)
        , expectEqual
            "EOF after readiness exits cleanly without a fabricated response"
            ( ExitSuccess
            , "ready version=1 kind=adder radius=5\n\n"
            , ""
            )
            (eofStatus, eofOutput, eofErrors)
        , expectEqual
            "unknown command-line arguments fail with usage on stderr"
            ( True
            , ""
            , "usage: dungeon-exe [--console | --agent NPC_ID -- PROGRAM [ARG ...]]\n"
            )
            (isFailure usageStatus, usageOutput, usageErrors)
        , expectEqual
            "agent mode rejects an unavailable identity before opening Vty"
            (True, "", "agent: NPC 99 is unavailable\n")
            (isFailure agentStatus, agentOutput, agentErrors)
        ]
    where
        isFailure (ExitFailure _) = True
        isFailure ExitSuccess = False

        transcriptSnapshot blocks = do
            initialArea <- blocksAt 1 blocks
            finalArea <- blocksAt 8 blocks
            initialPlayerRow <- blocksAt 4 initialArea
            finalPlayerRow <- blocksAt 6 finalArea
            pure
                ( [first | first : _ <- blocks]
                , map length blocks
                , initialPlayerRow
                , finalPlayerRow
                )

        blocksAt index values =
            case drop index values of
                value : _ -> Just value
                [] -> Nothing

responseBlocks :: String -> [[String]]
responseBlocks = go . lines
    where
        go input =
            case dropWhile null input of
                [] -> []
                nonEmpty ->
                    let (block, rest) = break null nonEmpty
                    in block : go rest

testChildAgentDriver :: IO Test
testChildAgentDriver = do
    executable <- getExecutablePath
    patrolResult <- timeout 5000000
        $ withCreateProcess (patrolChild executable)
        $ \toChild fromChild _ _ ->
            case (toChild, fromChild) of
                (Just input, Just output) -> runPatrol input output
                _ -> pure ["patrol agent pipes were not created"]
    fallbackResult <- timeout 5000000
        $ withCreateProcess (quittingChild executable)
        $ \toChild fromChild _ _ ->
            case (toChild, fromChild) of
                (Just input, Just output) -> runFallback input output
                _ -> pure ["quitting agent pipes were not created"]
    pure
        $ maybe ["patrol agent dialogue timed out"] id patrolResult
        ++ maybe ["quitting agent dialogue timed out"] id fallbackResult
    where
        patrolChild executable =
            (proc executable ["--patrol-agent-fixture"])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                }
        quittingChild executable =
            (proc executable ["--quitting-agent-fixture"])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                }
        ident = NpcId 3
        npc = initNpc (1, 1) Adder Stationary emptyItemStack

        viewAt x turn =
            toGameView []
                $ ( setNpcs
                        (Map.singleton ident npc { npcPosition = (x, 1) })
                    $ emptyGameState (5, 5) testDungeon
                  ) { stTurnNumber = turn }

        runPatrol input output = do
            let view1 = viewAt 1 0
                view2 = viewAt 2 1
                view3 = viewAt 3 2
            writeConsoleBlock input (readyLines npc)
            (first, driver1) <-
                unMSF (externalNpcDecisions ident output input) view1
            writeConsoleBlock input (turnResultLines view1 view2)
            (second, driver2) <- unMSF driver1 view2
            writeConsoleBlock input (turnResultLines view2 view3)
            (third, driver3) <- unMSF driver2 view3
            writeConsoleBlock input (turnResultLines view3 view3)
            (retried, _) <- unMSF driver3 view3
            pure
                $ expectEqual
                    "the child fixture reverses at bounds and retries held turns"
                    [ Map.singleton ident (1, 0)
                    , Map.singleton ident (1, 0)
                    , Map.singleton ident (-1, 0)
                    , Map.singleton ident (-1, 0)
                    ]
                    [first, second, third, retried]

        runFallback input output = do
            let view = viewAt 1 0
            (onQuit, fallback) <-
                unMSF (externalNpcDecisions ident output input) view
            (afterQuit, _) <- unMSF fallback view
            pure
                $ expectEqual
                    "a quitting child permanently releases its NPC override"
                    [Map.empty, Map.empty]
                    [onQuit, afterQuit]

patrolAgentFixture :: IO ()
patrolAgentFixture = do
    expectFixtureBlock "ready"
    sendFixtureCommand "c"
    expectFixtureBlock "character"
    mapM_ takeTurn ["l", "l", "h", "h"]
    where
        takeTurn direction = do
            sendFixtureCommand "s"
            expectFixtureBlock "area"
            sendFixtureCommand direction
            expectFixtureBlock "turn result"

quittingAgentFixture :: IO ()
quittingAgentFixture = do
    sendFixtureCommand "q"
    expectFixtureBlock "bye"

sendFixtureCommand :: String -> IO ()
sendFixtureCommand command = do
    putStrLn command
    hFlush stdout

expectFixtureBlock :: String -> IO ()
expectFixtureBlock description = do
    block <- readFixtureBlock
    case block of
        Just (_ : _) -> pure ()
        _ -> error ("agent fixture expected " ++ description)

readFixtureBlock :: IO (Maybe [String])
readFixtureBlock = go []
    where
        go linesSoFar = do
            eof <- hIsEOF stdin
            if eof
                then pure Nothing
                else do
                    line <- hGetLine stdin
                    if null line
                        then pure (Just $ reverse linesSoFar)
                        else go (line : linesSoFar)

checks :: [Test] -> Test
checks = concat

singleSample :: [a] -> a
singleSample samples =
    case samples of
        [sample] -> sample
        _ -> error "test expected exactly one signal sample"

expectEqual :: (Eq a, Show a) => String -> a -> a -> Test
expectEqual name expected actual =
    [ name ++ ": expected " ++ show expected ++ ", got " ++ show actual
    | expected /= actual
    ]
