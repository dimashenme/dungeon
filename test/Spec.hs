module Main (main) where

import Prelude hiding (init)
import Control.Arrow (arr, (&&&), (>>>))
import Control.Monad (unless)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State.Strict (State, get, modify, runState)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.MSF.Maybe (exceptToMaybeS)
import Control.Monad.Writer.Strict (runWriter)
import Data.Array (bounds, listArray, range, (!))
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
import Dungeon.DungeonLayout
import qualified Dungeon.Game as Game
import Dungeon.GameData
import Dungeon.Interface
    ( HasVty(..)
    , UIInput(..)
    , gameViewCell
    , inventoryLines
    , messageLogLines
    , parseInput
    , viewport
    )
import Dungeon.Item
import Dungeon.ItemState
import Dungeon.Logic
import Dungeon.Npc
import Dungeon.Random (drawBool, selectRandomSubset)
import Dungeon.TestLayout1 (testLayout, testStartPos)
import Dungeon.Types
import System.Exit (exitFailure)

type Test = [String]

main :: IO ()
main = do
    let failures =
            checks
                [ testLayoutComposition
                , testLayoutReferences
                , testSampleLayout
                , testCharacterInitialization
                , testTerrain
                , testMovementAndHoldUp
                , testNpcTurns
                , testReconcileMSFs
                , testNpcPatrolBehaviour
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
                , testItemPresentation
                , testRendering
                , testInventoryLines
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
        "player initialization uses authored attributes and an empty inventory"
        (initialPlayerAttributes, emptyItemStack)
        (plAttributes player, plInventory player)
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
    , expectEqual "room interior is floor" ' ' (dungeon ! (3, 3))
    , expectEqual "tunnel is floor" ' ' (dungeon ! (8, 4))
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
        , Just (RemoveItemEvt pos (ItemId 1))
        ]
        (runIdentity
            $ embed
                floorEvtsFromPickup
                [ Nothing
                , Just
                    PickupEvt
                        { pickupPosition = pos
                        , pickupItemId = ItemId 1
                        , pickupItem = redItem
                        }
                ])
    , expectEqual
        "Pick transfers the top floor identity and value to inventory"
        ( [(ItemId 2, blueItem)]
        , [(ItemId 1, redItem)]
        )
        ( itemStackToList (itemsAt pos (stFloorItems picked))
        , itemStackToList (inventoryOf picked)
        )
    , expectEqual
        "Drop transfers the inventory head identity and value to the floor"
        ( [ (ItemId 3, ringItem)
          , (ItemId 1, redItem)
          , (ItemId 2, blueItem)
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
        floorItems = Map.singleton pos (testStack 1 [redItem, blueItem])
        gameInit = setFloor floorItems (emptyGameState pos testDungeon)
        picked = singleSample (runScriptedGame gameInit [Pick])
        dropped =
            singleSample
                $ runScriptedGame
                    (setInventory (testStack 3 [ringItem]) gameInit)
                    [Drop]
        gameStates = runScriptedGame gameInit [Pick, Pick, Pick]

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
        "an invalid container target falls back to loose floor placement"
        [blueItem, redItem]
        (itemStackItems (itemsAt pos invalidTargetPlacement))
    , expectEqual
        "Drop messages distinguish the floor, a chest, and a corpse"
        [ "dropped a ring of protection at (2,2)"
        , "dropped a ring of protection into a chest at (2,2)"
        , "dropped a ring of protection into the corpse of the goblin at (2,2)"
        ]
        [ messageForDrop Map.empty
        , messageForDrop chestFloor
        , messageForDrop corpseFloor
        ]
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
                    [Just (RemoveItemEvt pos (ItemId 1))]
        unknownFloorRemoval =
            singleSample
                $ runIdentity
                $ embed
                    (floorItemsState initialFloor)
                    [Just (RemoveItemEvt pos (ItemId 99))]
        unknownInventoryRemoval =
            singleSample
                $ runIdentity
                $ embed
                    (inventoryState initialInventory)
                    [Just (RemoveEvt (ItemId 99))]
        invalidTargetPlacement =
            singleSample
                $ runIdentity
                $ embed
                    (floorItemsState initialFloor)
                    [ Just
                        (PlaceItemEvt
                            pos
                            (Just (ItemId 99))
                            (ItemId 2)
                            blueItem)
                    ]
        dropEvent =
            DropEvt
                { dropPosition = pos
                , dropItemId = ItemId 10
                , dropItem = ringItem
                }
        messageForDrop floorItems =
            case snd
                    $ runWriter
                    $ embed
                        floorEvtsFromDrop
                        [Just (dropEvent, floorItems)] of
                [message] -> message
                _ -> error "expected exactly one Drop message"
        chestFloor =
            Map.singleton pos (testStack 1 [chestItem emptyItemStack])
        corpseFloor =
            Map.singleton
                pos
                (testStack 1 [corpseItem Goblin emptyItemStack])

testContainerTransfers :: Test
testContainerTransfers = checks
    [ expectEqual
        "Pick uses the first container even below a loose item"
        [redItem]
        (inventoryItems afterContents)
    , expectEqual
        "an empty portable container can itself be picked up"
        [chestItem emptyItemStack, redItem]
        (inventoryItems afterContainer)
    , expectEqual
        "an empty non-portable corpse blocks access to lower items"
        (0, [corpseItem Goblin emptyItemStack, blueItem], [])
        ( stTurnNumber blockedResult
        , itemStackItems (itemsAt pos (stFloorItems blockedResult))
        , inventoryItems blockedResult
        )
    , expectEqual
        "Drop inserts into the first container and leaves later ones unchanged"
        [[ringItem], [blueItem]]
        (map itemStackItems (containerStacks pos dropped))
    , expectEqual
        "container transfers preserve item identities"
        ([ItemId 10], [ItemId 2, ItemId 10])
        ( itemStackIds (firstContainerItems pos dropped)
        , itemStackIds (inventoryOf afterContainer)
        )
    , expectEqual
        "nested removal searches later containers for the requested identity"
        (Just
            [ chestItem (testStack 10 [redItem])
            , chestItem emptyItemStack
            ])
        (itemStackItems
            <$> removeFromFloorStack
                (ItemId 20)
                nestedRemovalStack)
    ]
    where
        pos = (2, 2)
        init =
            setFloor
                (Map.singleton pos
                    (testStack 1
                        [ greenItem
                        , chestItem (testStack 10 [redItem])
                        , corpseItem Goblin emptyItemStack
                        ]))
                (emptyGameState pos testDungeon)
        (afterContents, afterContainer) =
            case runScriptedGame init [Pick, Pick] of
                [first, second] -> (first, second)
                _ -> error "expected two item-transfer samples"
        blockedResult =
            singleSample
                $ runScriptedGame
                    (setFloor
                        (Map.singleton pos
                            (testStack 1
                                [ corpseItem Goblin emptyItemStack
                                , blueItem
                                ]))
                        (emptyGameState pos testDungeon))
                    [Pick]
        dropped =
            singleSample
                $ runScriptedGame
                    (setInventory (testStack 10 [ringItem])
                        $ setFloor
                            (Map.singleton pos
                                (testStack 1
                                    [ greenItem
                                    , chestItem emptyItemStack
                                    , corpseItem Goblin
                                        (testStack 20 [blueItem])
                                    ]))
                            (emptyGameState pos testDungeon))
                    [Drop]
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
        views = runViews state [Inspect, Pick]

testInput :: Test
testInput =
    expectEqual
        "input bindings and an unbound key"
        [ Just (PlayTurn (Move West))
        , Just (PlayTurn (Move South))
        , Just (PlayTurn (Move North))
        , Just (PlayTurn (Move East))
        , Just (PlayTurn Pick)
        , Just (PlayTurn Drop)
        , Just (PlayTurn Wait)
        , Just Redraw
        , Just ShowInventory
        , Just (PlayTurn Inspect)
        , Just Quit
        , Nothing
        ]
        (map parseInput "hjklpd. i\nqz")

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
        "NPCs render over floor stacks"
        'g'
        (gameViewCell npcView pos)
    , expectEqual
        "the first item in a floor stack is rendered"
        '%'
        (gameViewCell itemView pos)
    ]
    where
        pos = (2, 2)
        base = emptyGameState pos testDungeon
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

testInventoryLines :: Test
testInventoryLines = checks
    [ expectEqual
        "inventory renders numbered item descriptions"
        ["Inventory", "1. red gem", "2. ring of protection"]
        (inventoryLines 4 (testStack 1 [redItem, ringItem]))
    , expectEqual
        "empty inventory has an explicit line"
        ["Inventory", "(empty)"]
        (inventoryLines 3 emptyItemStack)
    , expectEqual
        "inventory is clipped to viewport height"
        ["Inventory", "1. red gem"]
        (inventoryLines 2 (testStack 1 [redItem, ringItem]))
    ]

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
testDungeon = listArray ((1, 1), (5, 5)) (repeat ' ')

wideDungeon :: Dungeon
wideDungeon = layoutDungeon $ compose 1.0 $ room (1, 1) (10, 4)

wallDungeon :: Dungeon
wallDungeon =
    listArray
        ((1, 1), (3, 3))
        [ ' ', ' ', ' '
        , ' ', '#', ' '
        , ' ', ' ', ' '
        ]

wallAtThreeDungeon :: Dungeon
wallAtThreeDungeon =
    listArray
        ((1, 1), (5, 5))
        [ if pos == (3, 1) then '#' else ' '
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
    { viewportScreenDims :: (Int, Int)
    , viewportPadding :: (Int, Int)
    }

instance HasVty ViewportConfig where
    getVty _ = error "viewport tests do not use Vty"
    getScreenDims = viewportScreenDims
    getPadding = viewportPadding

runViewport
    :: (Int, Int)
    -> (Int, Int)
    -> (Int, Int)
    -> [(Int, Int)]
    -> [(Int, Int, Int, Int)]
runViewport screenDims padding dungeonDims positions =
    runReader (go viewport positions) (ViewportConfig screenDims padding)
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
