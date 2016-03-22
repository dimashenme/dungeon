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

import Dungeon.Map

import Data.Array 
import Data.Array.ST

import Graphics.Vty
import Graphics.Vty.Picture
import Data.Default (def)

-- dungeon

dungeonW = 100
dungeonH = 50

screenW = 40
screenH = 20

padX = 5
padY = 5

startX = 10
startY = 10

zeroArr :: Array (Int,Int) Char
zeroArr =  newDungeon dungeonH dungeonW

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

-- movement

deltaPlayer :: (Monad m) => Auto m Char  (Int, Int)
deltaPlayer = arr $ \x -> case x of
   'h' -> (-1,0)
   'j' -> (0,1)
   'k' -> (0,-1)
   'l' -> (1,0)
   _   -> (0,0)

sumPair (a,b) (c,d) = (a+c,b+d)

playerPos' startx starty = (A.accum sumPair (startx,starty)) .  deltaPlayer

playerPos :: (Monad m) => Auto m Char (Int,Int)
playerPos = playerPos' startX startY

deltaScreen :: (Monad m) => Auto m (Char, Int, Int, Int, Int) (Int,Int)
deltaScreen = proc (inp, x1, y1, x2, y2) ->  do
  (playerx,playery) <- playerPos -< inp
  let deltax = if ((playerx  - x1) <= padX) 
               then playerx - x1 - padX 
               else if ((x2 - playerx) <= padX)
                    then padX + playerx - x2 
                    else 0
      deltay = if ((playery  - y1) <= padY)
               then playery - y1 - padY
               else if ((y2 - playery) <= padY)
                    then padY + playery - y2
                    else 0
    in returnA -< (deltax,deltay)

sumPQ (a,b,c,d) (x,y) = (a+x,b+y,c+x,d+y) 

screenBounds :: (MonadFix m) => Auto m Char (Int,Int,Int,Int)
screenBounds = proc inp -> do
  rec
    (x1,y1,x2,y2) <- lastVal (1,1,screenW,screenH) -< ret
    ret <- (A.accum sumPQ (1,1,screenW,screenH)) . deltaScreen -< (inp,x1,y1,x2,y2)
  returnA -< ret

screenPos :: (MonadFix m) => Auto m Char (Int,Int)
screenPos = proc inp -> do
  ((px,py),(x1,y1,x2,y2)) <- (playerPos &&& screenBounds) -< inp
  returnA -< (px - x1 + 1, py - y1 + 1)

mainArrow = screenPos &&& screenBounds

-- screen output

printOut :: Array (Int, Int) Char -> IO ()
printOut a =
  printL (elems a)
  where ((_,_),(h,w)) = bounds a
        printL [] = return ()
        printL l = let (line, rest) = splitAt w l
                   in do putStrLn line
                         printL rest

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

test =  evalAuto deltaScreen ('k', 1,1,screenW,screenH) :: IO (Int,Int)
  
test2 = streamAuto screenPos "lllhhhhjjjkkk" :: IO [(Int,Int)]
  
main = do
  vty <- mkVty def
  showCursor (outputIface vty)
  output vty (startX,startY) (1,1,screenW,screenH)
  mainLoop vty (inputVty vty) mainArrow
  shutdown vty
