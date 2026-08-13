{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Dungeon.Combinators
    ( feedbackM
    , accumulateWithM
    , oneTickThen
    , doOnce
    , oneTickThen_
    , runMaybeStateS
    , feedbackS
    , accumulateS
    , accumulateMaybe
    , countdownFrom
    , restartOn
    , restartOnEvt
    , reconcileMSFs
    , sampleAndHold
    , asksS
    ) where

import Prelude hiding (init)
import Control.Arrow
import Control.Monad (guard)
import Control.Monad.Reader
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF.Maybe (MaybeT, runMaybeS)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.MonadicStreamFunction hiding (next)
import Data.MonadicStreamFunction.InternalCore (MSF(..))

-- | feedback but with initial monadic value
feedbackM
    :: Monad m
    => m s
    -> MSF m (a, s) (b, s)
    -> MSF m a b
feedbackM ms msf =
    performOnFirstSample $ do
        s0 <- ms
        return $ feedback s0 msf

-- | accumulateWith but with initial monadic value
accumulateWithM
    :: Monad m
    => (a -> s -> s)
    -> m s
    -> MSF m a s
accumulateWithM f ms =
    feedbackM ms $ arr $ uncurry f >>> (\s -> (s, s))

-- | Take two signals, output the first one for one tick, then always
-- second.  It's a little bit like iPre, but works with signals
-- instead of pure values
oneTickThen
    :: Monad m
    => MSF m a b
    -> MSF m a b
    -> MSF m a b
oneTickThen msf1 msf2 = MSF $ \input -> do
    (output, _) <- unMSF msf1 input
    pure (output, msf2)

doOnce
    :: Monad m
    => MSF m a b
    -> MSF (ExceptT () m) a b
doOnce msf =
    dSwitch
        (liftTransS msf >>> arr (\x -> (x, Just ())))
        (\_ -> arrM (const (throwE ())))

oneTickThen_
    :: Monad m
    => MSF m a b
    -> MSF m a b
    -> MSF m a b
oneTickThen_ msf1 msf2 = safely $ do
    try $ doOnce msf1
    safe msf2

-- | Run a MaybeT MSF while restoring underlying State after 'Nothing'.
-- Other underlying effects, such as Writer observations, are preserved.
runMaybeStateS
    :: MonadState s m
    => MSF (MaybeT m) a b
    -> MSF m a (Maybe b)
runMaybeStateS msf = proc input -> do
    state <- constM get -< ()
    result <- runMaybeS msf -< input
    _ <- arrM (uncurry restore) -< (state, result)
    returnA -< result
    where
        restore state = maybe (put state) (const $ pure ())

-- | Like feedback but samples inital state from a signal
feedbackS
    :: Monad m
    => MSF m (a, s) (b, s)
    -> MSF m (a, s) b
feedbackS f = feedback Nothing $ proc ((x, s0), mbS) -> do
    let cur = fromMaybe s0 mbS
    (y, next) <- f -< (x, cur)
    returnA -< (y, Just next)

-- | Like accumulateWith, but samples initial state from a signal
accumulateS
    :: Monad m
    => MSF m (a, b) b
    -> MSF m (a, b) b
accumulateS f =
    feedbackS (f >>> arr (\x -> (x, x)))

-- | Accumulate event occurrences and hold the state between them.
accumulateMaybe
    :: Monad m
    => (a -> s -> s)
    -> s
    -> MSF m (Maybe a) s
accumulateMaybe f init =
    sampleAndHold init (accumulateWith f init)

-- | Count down once per sample, holding at zero after the duration expires.
countdownFrom :: Monad m => Int -> MSF m a Int
countdownFrom duration =
    count
        >>> arr (\tick -> max 0 (duration - tick + 1))

-- | Restart when the Boolean input is true.
restartOn
    :: Monad m
    => MSF m a b
    -> MSF m a b
    -> MSF m (a, Bool) b
restartOn init f =
    arr (second guard)
        >>> restartOnEvt init f

-- | Step the initial MSF until an event replaces it with a fresh instance of
-- the restart MSF. Each later event restarts that original MSF again.
restartOnEvt
    :: Monad m
    => MSF m a b
    -> MSF m a b
    -> MSF m (a, Maybe event) b
restartOnEvt init f =
    feedback init
        $ arrM
        $ \((input, reset), current) ->
            unMSF (maybe current (const f) reset) input

-- | Reconcile a changing keyed collection with independently advancing MSFs.
-- Present keys retain their continuations, new keys are started and stepped
-- immediately, and missing keys are discarded.
reconcileMSFs
    :: (Monad m, Ord key)
    => (key -> input -> MSF m input output)
    -> MSF m (Map key input) (Map key output)
reconcileMSFs start =
    feedback Map.empty
        $ arrM
        $ \(inputs, running) -> do
            stepped <-
                Map.traverseWithKey
                    (\key input ->
                        unMSF
                            (Map.findWithDefault
                                (start key input)
                                key
                                running)
                            input)
                    inputs
            pure (fst <$> stepped, snd <$> stepped)

-- | Step an MSF on 'Just' inputs and hold its latest output on 'Nothing'.
-- The initial value is output until the first 'Just' input arrives.
sampleAndHold
    :: Monad m
    => b
    -> MSF m a b
    -> MSF m (Maybe a) b
sampleAndHold init msf =
    mapMaybeS msf
        >>> accumulateWith (flip fromMaybe) init

-- | Shortcut to lift a value from ReaderT monad to the signal level
asksS
    :: MonadReader r m
    => (r -> b)
    -> MSF m a b
asksS = constM . asks
