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
import Dungeon.Movement as M

-- dungeon


zeroArr :: Array (Int,Int) Char
zeroArr =  newDungeon M.dungeonH M.dungeonW

level :: Array (Int,Int) Char
level = runSTArray $ do
  a <- thaw zeroArr
  putRoomST (2,2) (10,6) a
  putRoomST (15,14) (20,30) a
  putRoomST (40,4) (45,30) a
  putRoomST (30,24) (20,40) a
  return a

-- input

inputList :: (Monad m) => [Char] -> Auto m a (Maybe Char)
inputList lst = unlessI (== 'q') `compI` fromList_ lst

inputVty :: Vty -> Auto IO a (Maybe Char)
inputVty vty = compI (unlessI (== 'q')) $ effect $ do
  evt <- nextEvent vty
  case evt of
    EvKey (KChar a) [] -> return $ Just a
    _ -> return Nothing

-- movement Auto

mainAuto = screenPos &&& screenBounds

-- screen output

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs

subRect x1 y1 x2 y2 w  l =
  take (y2 - y1 + 1) . drop (y1-1) $ fmap (take (x2 - x1 + 1) . drop (x1-1)) ll
  where ll = chunk w l 

getLevelImage level x1 y1 x2 y2 =
    vertCat (fmap (string defAttr) (subRect x1 y1 x2 y2 w (elems level)))
    where w = bx2 - bx1 + 1
          h = by2 - by1 + 1          
          ((by1,bx1),(by2,bx2)) = bounds level

output :: Vty -> (Int,Int) -> (Int, Int, Int, Int) -> IO ()
output vty (px,py) (x1, y1, x2, y2)=  do
  update vty (picForImage (getLevelImage level x1 y1 x2 y2))
  setCursorPos (outputIface vty) px py
  showCursor  (outputIface vty)

--- main loop

-- mainLoop ::  Vty -> Auto IO () (Maybe Char) -> Auto IO Char b -> IO ()
mainLoop vty inAuto outAuto  = do
  (input, inAuto') <- stepAuto inAuto ()
  case input of
    Just key -> do
      (((px,py),(x1,y1,x2,y2)), outAuto') <- stepAuto outAuto key
      output vty (px,py) (x1,y1,x2,y2)    
      mainLoop vty inAuto' outAuto'
    Nothing -> return ()

test =  evalAuto deltaScreen ('k', 1,1,M.screenW,M.screenH) :: IO (Int,Int)
  
test2 = streamAuto screenPos "lllhhhhjjjkkk" :: IO [(Int,Int)]
  
main = do
  vty <- mkVty def
  showCursor (outputIface vty)
  output vty (M.startX,M.startY) (1,1,M.screenW,M.screenH)
  mainLoop vty (inputVty vty) mainAuto
  shutdown vty
