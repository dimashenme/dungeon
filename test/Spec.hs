module Main (main) where

import Control.Monad (unless)
import Control.Monad.Reader (Reader, runReader)
import Data.Array ((!), bounds, listArray)
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import Dungeon.Logic (Direction(..), GameState(..), Turn(..), playerPos)
import Dungeon.Map (Dungeon, compose, digX, isWalkable, room)
import System.Exit (exitFailure)

type Test = IO [String]

main :: IO ()
main = do
  failures <- concat <$> sequence
    [ testComposedDungeonBounds
    , testRoomTerrain
    , testTunnelTerrain
    , testWalkability
    , testMovementDirections
    , testMovementAtMapEdge
    , testRepeatedMovement
    , testMovementBlockedByWall
    ]
  unless (null failures) $ do
    putStrLn "Test failures:"
    mapM_ putStrLn failures
    exitFailure
  putStrLn "All tests passed."

-- | A carved room joined to a horizontal tunnel for map semantics tests.
testDungeon1 :: Dungeon
testDungeon1 = compose $ do
  room (2, 2) (6, 6)
  digX (6, 4) 3

testComposedDungeonBounds :: Test
testComposedDungeonBounds =
  expectEqual "composed dungeon bounds" ((1, 1), (10, 7)) (bounds testDungeon1)

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

expectEqual :: (Eq a, Show a) => String -> a -> a -> Test
expectEqual name expected actual =
  pure
    [ name ++ ": expected " ++ show expected ++ ", got " ++ show actual
    | expected /= actual
    ]
