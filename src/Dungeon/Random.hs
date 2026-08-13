{-# LANGUAGE FlexibleContexts #-}

module Dungeon.Random
    ( RandomSeed(..)
    , drawBool
    , selectRandomSubset
    ) where

import Control.Monad (filterM)
import Control.Monad.State.Class (MonadState, state)
import Data.Bits (testBit)
import Data.Word (Word64)

newtype RandomSeed = RandomSeed Word64
    deriving (Show, Eq)

drawBool :: MonadState RandomSeed m => m Bool
drawBool = state $ \(RandomSeed seed) ->
    let next =
            seed * 6364136223846793005
            + 1442695040888963407
    in (testBit next 63, RandomSeed next)

selectRandomSubset :: MonadState RandomSeed m => [a] -> m [a]
selectRandomSubset = filterM (const drawBool)
