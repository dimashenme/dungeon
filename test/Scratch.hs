{-# LANGUAGE Arrows #-}

import Dungeon.Combinators
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore
import Control.Arrow
import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.Class (lift)

-- to test accumulateS

addA :: Monad m => MSF m (Int, Int) Int
addA = arr (uncurry (+))  

foldedA :: Monad m => MSF m (Int, Int) Int
foldedA = accumulateS addA

printA :: MSF IO Int ()
printA = arrM $ putStrLn . show 

withInit :: Monad m => MSF m Int Int
withInit = (returnA &&& arr (const 0)) >>> foldedA 


action1 = constM $ putStrLn "One"
action2 = constM $ putStrLn "Two"

waitFor = proc () -> do
  n <- count -< ()
 
  if (n < 10)
    then arrM $ lift . putStrLn . show -< n
    else throw () -< ()
