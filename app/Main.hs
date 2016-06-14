{-# LANGUAGE Arrows #-}

import Control.Auto as A
import Control.Auto.Core
import Control.Auto.Interval
import Control.Auto.Generate
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Applicative
import Prelude hiding ((.))

import Data.Array 
import Data.Array.ST

import Graphics.Vty
import Graphics.Vty.Picture
import Data.Default (def)

import Dungeon.Map
import Dungeon.Movement as DM

-- dungeon


zeroArr :: Array (Int,Int) Char
zeroArr =  newDungeon DM.dungeonH DM.dungeonW

level :: Array (Int,Int) Char
level = runSTArray $ do
  a <- thaw zeroArr
  putRoomST (2,2) (10,6) a
  putRoomST (15,14) (20,30) a
  putRoomST (40,4) (45,30) a
  putRoomST (30,24) (40,40) a
  return a

-- input

parseInput :: Char -> Maybe Turn
parseInput a 
  | a == 'h' = Just DM.West
  | a == 'j' = Just DM.South
  | a == 'k' = Just DM.North
  | a == 'l' = Just DM.East
  | otherwise = Nothing

inputList :: (Monad m) => [Char] -> Interval m a Turn
inputList lst =  (arr parseInput) `compI` unlessI (== 'q') `compI` fromList_ lst

inputVty :: Vty -> Interval IO a Turn
inputVty vty = (arr parseInput) `compI` (inputVty' vty)
inputVty' vty = compI (unlessI (== 'q')) $ effect $ do
  evt <- nextEvent vty
  case evt of
    EvKey (KChar a) [] -> return $ Just a
    _ -> return Nothing


-- game logic

initialState = ((DM.startX,DM.startY),(1,1,DM.screenW,DM.screenH))


logicAuto
  :: Interval IO Turn ((Int, Int), (Int, Int, Int, Int))
logicAuto = toOn . (screenPos &&& screenBounds)

-- screen output

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs

subRect :: Int -> Int -> Int -> Int -> Int -> [a] -> [[a]]
subRect x1 y1 x2 y2 w  l =
  take (y2 - y1 + 1) . drop (y1-1) $ fmap (take (x2 - x1 + 1) . drop (x1-1)) ll
  where ll = chunk w l 

renderLevel
  :: (Num t, Ix t) =>
     Array (t, Int) Char -> Int -> Int -> Int -> Int -> Image
renderLevel level x1 y1 x2 y2 =
    vertCat (fmap (string defAttr) (subRect x1 y1 x2 y2 w (elems level)))
    where w = bx2 - bx1 + 1
          h = by2 - by1 + 1          
          ((by1,bx1),(by2,bx2)) = bounds level

output :: Vty -> (Int,Int) -> (Int, Int, Int, Int) -> IO ()
output vty (px,py) (x1, y1, x2, y2)=  do
  update vty (picForImage (renderLevel level x1 y1 x2 y2))
  setCursorPos (outputIface vty) (px-1) (py-1)
  showCursor  (outputIface vty)

outputAuto
  :: Vty -> Interval IO  ((Int, Int), (Int, Int, Int, Int)) ()
outputAuto vty = toOn . (arrM $ uncurry $ output vty)

--- wire everything together

-- | All constituent autos are intervals
-- and must be composted with compI
mainAuto :: Vty -> Auto IO a (Maybe ())
mainAuto vty =  (outputAuto vty) `compI` skipFirstInput where
  skipFirstInput = (onFor 1) . (pure $ Just initialState)
       -->  (logicAuto `compI` (inputVty vty))

--- main loop is simple

mainLoop :: Monad m => Auto m () (Maybe t) -> m ()
mainLoop a  = do
  (result, a') <- stepAuto a ()
  case result of
    Just action -> mainLoop a'
    Nothing -> return ()

test =  evalAuto playerPos DM.South :: IO (Int,Int)
  
test2 = streamAuto' ((toOn . screenBounds) `compI` (arr parseInput)) "llllllllll"

test3 = streamAuto' ((toOn . playerPos) `compI` (arr parseInput)) "hhhhhhhhhhhhllll"

main = do
  vty <- mkVty def
  showCursor (outputIface vty)
  mainLoop (mainAuto vty)
  shutdown vty
