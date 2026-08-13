module Dungeon.TestLayout1
    ( testStartPos
    , testLayout
    ) where

import Control.Monad (forM_)
import Dungeon.DungeonLayout
import Dungeon.GameData
import Dungeon.Item
import Dungeon.Npc
    ( PatrolAxis(..)
    , NpcBehaviour(..)
    , patrol
    )
import Dungeon.Types (NpcKind(..))

testStartPos :: Position
testStartPos = (6, 6)

testLayout :: DungeonLayout
testLayout = compose 2.0 $ do
    forM_ testDungeonRooms $ \(p1, p2) -> room p1 p2

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

    water (9, 6)
    water (10, 6)
    water (9, 7)
    water (10, 7)

    placeItem (3, 3) (GemItem redGem)
    placeItem (4, 3) (GemItem greenGem)
    placeItem (5, 3) (GemItem blueGem)
    placeItem (6, 3) (GemItem yellowGem)

    _ <- placeChest (5, 3)
    greenChest <- placeChest (6, 3)
    placeItem greenChest (GemItem greenGem)
    gemChest <- placeChest (7, 3)
    placeItem gemChest (GemItem redGem)
    placeItem gemChest (GemItem blueGem)
    _ <- placeChest (8, 3)

    placeItem (9, 3) (RingItem ringOfProtection)
    placeItem (10, 3) (PotionItem potionOfHealing)
    placeItem (19, 6) (BookItem bookOfForgottenPaths)
    placeItem (20, 6) (ScrollItem scrollOfMapping)
    placeItem (21, 6) (WeaponItem shortSword)
    placeItem (22, 6) (WeaponItem greatAxe)
    placeItem (23, 6) (WeaponItem shortBow)
    placeItem (24, 6) (ArmourItem leatherJerkin)
    placeItem (25, 6) (ArmourItem plateMail)

    adder <- placeNpc
        (4, 4)
        Adder
        (patrol Horizontal (4, 6))
    placeItem adder (GemItem redGem)
    goblin <- placeNpc
        (7, 4)
        Goblin
        (patrol Horizontal (7, 9))
    placeItem goblin (RingItem ringOfProtection)
    placeItem goblin (PotionItem potionOfHealing)
    rat <- placeNpc
        (4, 7)
        Rat
        (patrol Vertical (5, 7))
    placeItem rat (BookItem bookOfForgottenPaths)
    placeNpc (8, 7) Kobold Stationary
    where
        lTunnel (x1, y1) (x2, y2) = do
            digX (x1, y1) (x2 - x1)
            digY (x2, y1) (y2 - y1)

        zigZag (x1, y1) (xm, ym) (x2, y2) = do
            digX (x1, y1) (xm - x1)
            digY (xm, y1) (ym - y1)
            digX (xm, ym) (x2 - xm)
            digY (x2, ym) (y2 - ym)

testDungeonRooms :: [(Position, Position)]
testDungeonRooms =
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
