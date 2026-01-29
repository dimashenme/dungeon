{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Dungeon.Combinators where

import Control.Arrow
import Control.Monad.Reader
import Data.MonadicStreamFunction
import Control.Monad.Trans.MSF.Except

-- | feedback but with initial monadic value
feedbackM :: Monad m
          => m s                   -- initial monadic value
          -> MSF m (a, s) (b, s)   -- stateful MSF
          -> MSF m a b             -- s is fed back next step
feedbackM ms msf =
    performOnFirstSample $ do
       s0 <- ms
       return $ feedback s0 msf

-- | accumulateWith but with initial monadic value
accumulateWithM :: Monad m
                => (a -> s -> s)   -- transform funciton
                -> m s             -- initial monadic value
                -> MSF m a s       -- s is fed back next step
accumulateWithM f ms =
  feedbackM ms $ arr $ uncurry f >>> (\s -> (s,s))

-- | Take two signals, output the first one for one tick, then always
-- second.  It's a little bit like iPre, but works with signals
-- instead of pure values
oneTickThen :: Monad m
            => MSF m a b
            -> MSF m a b
            -> MSF m a b
oneTickThen msf1 msf2 =
  dSwitch
    (msf1 >>> arr (\x -> (x, Just ())))
    (const msf2)

doOnce :: Monad m
       => MSF (ExceptT () m) a b
       -> MSF (ExceptT () m) a b
doOnce msf = 
  dSwitch
    (msf >>> arr (\x -> (x, Just ())))
    (\_ -> arrM ( \_ -> throwE ()))

oneTickThen_ :: Monad m
            => MSF m a b
            -> MSF m a b
            -> MSF m a b
oneTickThen_ msf1 msf2 = safely $ do
    try $ doOnce (liftTransS msf1)
    safe msf2

-- | Like feedback but samples inital state from a signal
feedbackS :: Monad m
          => MSF m (a,s) (b,s)
          -> MSF m (a,s) b
feedbackS f = feedback Nothing $ proc ((x, s0), mbS) -> do
    let s_ = maybe s0 id mbS     -- pick s0 on first Nothing
    (y, s) <- f -< (x, s_)
    returnA -< (y, Just s)       -- ..then feedback on Just-s

-- | Like accumulateWith, but samples initial state from a signal
accumulateS :: Monad m
            => MSF m (a,b) b
            -> MSF m (a,b) b
accumulateS f =
  feedbackS (f >>> arr (\x -> (x,x)))

-- | Shortcut to lift a value from ReaderT monad to the signal level
asksS :: (MonadReader r m)
      => (r -> b)
      -> MSF m a b
asksS f = constM $ asks f
