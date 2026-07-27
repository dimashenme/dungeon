module Main (main) where

import Control.Monad (forM_, unless)
import Control.Monad.Reader (Reader, runReader)
import Data.Array ((!), bounds, listArray)
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import Dungeon.Interface (HasVty(..), viewport)
import Dungeon.Logic (Direction(..), GameState(..), Turn(..), playerPos)
import Dungeon.Map (Dungeon, compose, digX, digY, isWalkable, room)
import System.Exit (exitFailure)

type Test = IO [String]

main :: IO ()
main = do
  failures <- concat <$> sequence
    [ testComposedDungeonBounds
    , testNonIntegerDungeonScale
    , testRoomTerrain
    , testTunnelTerrain
    , testWalkability
    , testLargeDungeonStartWalkable
    , testLargeDungeonRooms
    , testLargeDungeonConnections
    , testLargeDungeonConnected
    , testTunnelWallConnection
    , testTunnelCornerConnection
    , testVerticalTunnelCornerConnection
    , testTunnelEndpointWalls
    , testTunnelJunctionWalls
    , testCornerDoorwayWalls
    , testMovementDirections
    , testMovementAtMapEdge
    , testRepeatedMovement
    , testMovementBlockedByWall
    , testViewportAtMapEdges
    , testViewportScrollsAtPadding
    , testViewportPadsSmallDungeon
    ]
  unless (null failures) $ do
    putStrLn "Test failures:"
    mapM_ putStrLn failures
    exitFailure
  putStrLn "All tests passed."

-- | A carved room joined to a horizontal tunnel for map semantics tests.
testDungeon1 :: Dungeon
testDungeon1 = compose 1.0 $ do
  room (2, 2) (6, 6)
  digX (6, 4) 3

-- | A room and tunnel composed with a non-integer coordinate scale.
testDungeon8 :: Dungeon
testDungeon8 = compose 1.5 $ do
  room (2, 2) (6, 6)
  digX (6, 4) 3

testComposedDungeonBounds :: Test
testComposedDungeonBounds =
  expectEqual "composed dungeon bounds" ((1, 1), (10, 7)) (bounds testDungeon1)

testNonIntegerDungeonScale :: Test
testNonIntegerDungeonScale = concat <$> sequence
  [ expectEqual
      "non-integer scaling rounds dungeon bounds"
      ((1, 1), (15, 10))
      (bounds testDungeon8)
  , expectEqual
      "scaled tunnel remains connected to its room"
      True
      (isWalkable testDungeon8 (9, 6) && isWalkable testDungeon8 (10, 6))
  ]

testRoomTerrain :: Test
testRoomTerrain = concat <$> sequence
  [ expectEqual "room boundary is a wall" '#' (testDungeon1 ! (2, 3))
  , expectEqual "room interior is floor" ' ' (testDungeon1 ! (3, 3))
  , expectEqual "undug terrain remains solid" '.' (testDungeon1 ! (1, 1))
  ]

testTunnelTerrain :: Test
testTunnelTerrain = concat <$> sequence
  [ expectEqual "tunnel is floor" ' ' (testDungeon1 ! (8, 4))
  , expectEqual "tunnel upper edge becomes a wall" '#' (testDungeon1 ! (8, 3))
  , expectEqual "tunnel lower edge becomes a wall" '#' (testDungeon1 ! (8, 5))
  ]

testWalkability :: Test
testWalkability = concat <$> sequence
  [ expectEqual "room floor is walkable" True (isWalkable testDungeon1 (3, 3))
  , expectEqual "tunnel floor is walkable" True (isWalkable testDungeon1 (8, 4))
  , expectEqual "room wall is not walkable" False (isWalkable testDungeon1 (2, 3))
  , expectEqual "undug terrain is not walkable" False (isWalkable testDungeon1 (1, 1))
  , expectEqual "out-of-range position is not walkable" False (isWalkable testDungeon1 (0, 1))
  ]

testLargeDungeonStartWalkable :: Test
testLargeDungeonStartWalkable =
  expectEqual
    "large dungeon start is walkable"
    True
    (isWalkable testDungeon7 testDungeon7StartPos)

testLargeDungeonRooms :: Test
testLargeDungeonRooms = concat <$> sequence
  [ expectEqual
      ("room interior at " ++ show (x, y) ++ " is walkable")
      True
      (isWalkable testDungeon7 (x + 1, y + 1))
  | ((x, y), _) <- testDungeon7Rooms
  ]

testLargeDungeonConnections :: Test
testLargeDungeonConnections = concat <$> sequence
  [ expectEqual "first L-tunnel exit is walkable" True (isWalkable testDungeon7 (11, 5))
  , expectEqual "first L-tunnel entrance is walkable" True (isWalkable testDungeon7 (18, 8))
  , expectEqual "first zig-zag turn is walkable" True (isWalkable testDungeon7 (33, 8))
  , expectEqual "first vertical room exit is walkable" True (isWalkable testDungeon7 (7, 9))
  , expectEqual "first vertical room entrance is walkable" True (isWalkable testDungeon7 (7, 23))
  , expectEqual "last room interior is walkable" True (isWalkable testDungeon7 (89, 110))
  ]

testLargeDungeonConnected :: Test
testLargeDungeonConnected =
  expectEqual
    "every room in the large dungeon is reachable from its start"
    True
    (all (`elem` walkableRegion testDungeon7 testDungeon7StartPos) roomInteriors)
  where
    roomInteriors = [(x + 1, y + 1) | ((x, y), _) <- testDungeon7Rooms]

testTunnelWallConnection :: Test
testTunnelWallConnection =
  expectEqual
    "a tunnel through a room wall creates a passage"
    [(6, 4), (7, 4)]
    (runPlayerMoves (5, 4) testDungeon1 [Move East, Move East])

testTunnelCornerConnection :: Test
testTunnelCornerConnection =
  expectEqual
    "a tunnel through room corners connects both room interiors"
    [ (5, 2), (6, 2), (7, 2), (8, 2)
    , (9, 2), (10, 2), (11, 2), (11, 3)
    ]
    (runPlayerMoves (5, 3) testDungeon4
      [ Move North, Move East, Move East, Move East
      , Move East, Move East, Move East, Move South
      ])

testVerticalTunnelCornerConnection :: Test
testVerticalTunnelCornerConnection =
  expectEqual
    "a vertical tunnel through room corners connects both room interiors"
    [ (3, 6), (2, 6), (2, 7), (2, 8)
    , (2, 9), (2, 10), (3, 10)
    ]
    (runPlayerMoves (3, 5) testDungeon5
      [ Move South, Move West, Move South, Move South
      , Move South, Move South, Move East
      ])

testTunnelEndpointWalls :: Test
testTunnelEndpointWalls = concat <$> sequence
  [ expectEqual "tunnel endpoint is capped by a wall" '#' (testDungeon1 ! (10, 4))
  , expectEqual "upper endpoint diagonal is a wall" '#' (testDungeon1 ! (10, 3))
  , expectEqual "lower endpoint diagonal is a wall" '#' (testDungeon1 ! (10, 5))
  ]

testTunnelJunctionWalls :: Test
testTunnelJunctionWalls = concat <$> sequence
  [ expectEqual "left junction diagonal is a wall" '#' (testDungeon6 ! (5, 4))
  , expectEqual "right junction diagonal is a wall" '#' (testDungeon6 ! (7, 4))
  ]

testCornerDoorwayWalls :: Test
testCornerDoorwayWalls = concat <$> sequence
  [ expectEqual "first corner doorway diagonal is a wall" '#' (testDungeon4 ! (4, 1))
  , expectEqual "second corner doorway diagonal is a wall" '#' (testDungeon4 ! (12, 1))
  ]

testMovementDirections :: Test
testMovementDirections =
  expectEqual
    "player movement handles all directions"
    [(2, 1), (3, 1), (3, 2), (2, 2)]
    (runPlayerMoves (2, 2) testDungeon2
      [Move North, Move East, Move South, Move West])

testMovementAtMapEdge :: Test
testMovementAtMapEdge =
  expectEqual
    "player movement remains within map bounds"
    [(1, 1), (1, 1), (2, 1), (2, 1), (2, 2)]
    (runPlayerMoves (1, 1) testDungeon2
      [Move West, Move North, Move East, Move North, Move South])

testRepeatedMovement :: Test
testRepeatedMovement =
  expectEqual
    "repeated movement retains local player position"
    [(2, 2), (3, 2), (3, 2)]
    (runPlayerMoves (1, 2) testDungeon2 [Move East, Move East, Move East])

testMovementBlockedByWall :: Test
testMovementBlockedByWall =
  expectEqual
    "player movement does not enter walls"
    [(1, 2), (1, 2), (1, 3)]
    (runPlayerMoves (1, 2) testDungeon3
      [Move East, Move East, Move South])

data ViewportConfig = ViewportConfig
  { viewportScreenDims :: (Int, Int)
  , viewportPadding :: (Int, Int)
  }

instance HasVty ViewportConfig where
  getVty _ = error "viewport tests do not use Vty"
  getScreenDims = viewportScreenDims
  getPadding = viewportPadding

testViewportAtMapEdges :: Test
testViewportAtMapEdges =
  expectEqual
    "viewport clamps independently at every map edge"
    [ (1, 1, 4, 3)
    , (7, 1, 10, 3)
    , (7, 8, 10, 10)
    , (1, 8, 4, 10)
    ]
    (runViewport (4, 3) (1, 1) (10, 10)
      [(2, 2), (9, 2), (9, 9), (2, 9)])

testViewportScrollsAtPadding :: Test
testViewportScrollsAtPadding =
  expectEqual
    "viewport scrolls only when the player crosses its padding"
    [ (1, 1, 6, 5)
    , (2, 1, 7, 5)
    , (1, 1, 6, 5)
    ]
    (runViewport (6, 5) (2, 1) (20, 20)
      [(3, 3), (5, 3), (3, 3)])

testViewportPadsSmallDungeon :: Test
testViewportPadsSmallDungeon =
  expectEqual
    "viewport keeps screen dimensions when the dungeon is smaller"
    [(1, 1, 6, 5), (1, 1, 6, 5)]
    (runViewport (6, 5) (2, 1) (3, 2) [(2, 1), (3, 2)])

-- | An all-floor map for movement and edge-boundary tests.
testDungeon2 :: Dungeon
testDungeon2 =
  let dungeonBounds = ((1, 1), (3, 3))
  in listArray dungeonBounds (repeat ' ')

-- | A floor map with a central wall for collision tests.
testDungeon3 :: Dungeon
testDungeon3 =
  listArray ((1, 1), (3, 3))
    [ ' ', ' ', ' '
    , ' ', '#', ' '
    , ' ', ' ', ' '
    ]

-- | Two rooms joined by a corridor that meets a corner of each room.
testDungeon4 :: Dungeon
testDungeon4 = compose 1.0 $ do
  room (2, 2) (6, 6)
  room (10, 2) (14, 6)
  digX (6, 2) 4

-- | Two stacked rooms joined by a corridor that meets a corner of each room.
testDungeon5 :: Dungeon
testDungeon5 = compose 1.0 $ do
  room (2, 2) (6, 6)
  room (2, 10) (6, 14)
  digY (2, 6) 4

-- | Two tunnels meeting at a right angle.
testDungeon6 :: Dungeon
testDungeon6 = compose 1.0 $ do
  digX (3, 5) 3
  digY (6, 5) 3

-- | A large irregular dungeon used to characterize the room and tunnel DSL.
testDungeon7 :: Dungeon
testDungeon7 = compose 1.0 $ do
  forM_ testDungeon7Rooms $ \(p1, p2) -> room p1 p2

  lTunnel (11, 5) (18, 8)
  zigZag (29, 6) (33, 8) (37, 10)
  lTunnel (47, 6) (56, 10)
  lTunnel (68, 9) (78, 10)

  digX (16, 26) 8
  zigZag (34, 24) (38, 26) (43, 28)
  digX (55, 27) 9
  lTunnel (75, 25) (84, 30)

  lTunnel (14, 47) (21, 49)
  digX (33, 48) 7
  zigZag (51, 45) (55, 48) (60, 51)
  digX (72, 50) 8

  digX (18, 68) 9
  zigZag (38, 65) (41, 67) (45, 70)
  digX (58, 70) 9
  lTunnel (78, 68) (87, 72)

  lTunnel (15, 89) (22, 92)
  zigZag (35, 90) (39, 91) (43, 92)
  lTunnel (54, 88) (63, 93)
  digX (76, 92) 7

  digX (19, 110) 9
  zigZag (39, 107) (43, 109) (47, 112)
  digX (59, 112) 9
  lTunnel (80, 110) (88, 114)

  digY (7, 9) 14
  digY (8, 31) 12
  digY (8, 52) 12
  digY (10, 72) 13
  digY (10, 94) 12
  where
    lTunnel (x1, y1) (x2, y2) = do
      digX (x1, y1) (x2 - x1)
      digY (x2, y1) (y2 - y1)

    zigZag (x1, y1) (xm, ym) (x2, y2) = do
      digX (x1, y1) (xm - x1)
      digY (xm, y1) (ym - y1)
      digX (xm, ym) (x2 - xm)
      digY (x2, ym) (y2 - ym)

testDungeon7Rooms :: [((Int, Int), (Int, Int))]
testDungeon7Rooms =
  [ ((2, 2), (11, 9)), ((18, 5), (29, 14)), ((37, 3), (47, 11))
  , ((56, 7), (68, 15)), ((78, 4), (91, 12)), ((5, 23), (16, 31))
  , ((24, 20), (34, 29)), ((43, 24), (55, 33)), ((64, 21), (75, 30))
  , ((84, 25), (96, 34)), ((2, 43), (14, 52)), ((21, 46), (33, 54))
  , ((40, 42), (51, 51)), ((60, 47), (72, 56)), ((80, 44), (93, 53))
  , ((6, 64), (18, 72)), ((27, 61), (38, 70)), ((45, 66), (58, 75))
  , ((67, 63), (78, 72)), ((87, 67), (99, 76)), ((3, 85), (15, 94))
  , ((22, 88), (35, 97)), ((43, 84), (54, 93)), ((63, 89), (76, 98))
  , ((83, 86), (95, 95)), ((7, 106), (19, 115)), ((28, 103), (39, 112))
  , ((47, 108), (59, 117)), ((68, 105), (80, 114)), ((88, 109), (101, 118))
  ]

testDungeon7StartPos :: (Int, Int)
testDungeon7StartPos = (3, 3)

runPlayerMoves :: (Int, Int) -> Dungeon -> [Turn] -> [(Int, Int)]
runPlayerMoves initialPosition dungeon turns =
  runReader (go playerPos (map (\turn -> (dungeon, turn)) turns)) initial
  where
    initial = GameState initialPosition dungeon

    go :: MSF (Reader GameState) (Dungeon, Turn) (Int, Int)
       -> [(Dungeon, Turn)]
       -> Reader GameState [(Int, Int)]
    go _ [] = pure []
    go msf (input : rest) = do
      (position, nextMsf) <- unMSF msf input
      (position :) <$> go nextMsf rest

runViewport :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
            -> [(Int, Int, Int, Int)]
runViewport screenDims padding dungeonDims positions =
  runReader (go viewport positions) (ViewportConfig screenDims padding)
  where
    go :: MSF (Reader ViewportConfig) ((Int, Int), (Int, Int)) (Int, Int, Int, Int)
       -> [(Int, Int)]
       -> Reader ViewportConfig [(Int, Int, Int, Int)]
    go _ [] = pure []
    go msf (position : rest) = do
      (currentViewport, nextMsf) <- unMSF msf (position, dungeonDims)
      (currentViewport :) <$> go nextMsf rest

walkableRegion :: Dungeon -> (Int, Int) -> [(Int, Int)]
walkableRegion dungeon start = go [start] []
  where
    go [] visited = visited
    go (position : pending) visited
      | position `elem` visited = go pending visited
      | not (isWalkable dungeon position) = go pending visited
      | otherwise =
          go (neighbors position ++ pending) (position : visited)

    neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

expectEqual :: (Eq a, Show a) => String -> a -> a -> Test
expectEqual name expected actual =
  pure
    [ name ++ ": expected " ++ show expected ++ ", got " ++ show actual
    | expected /= actual
    ]
