module Control.Monad.Extra where

import Data.Bifunctor (first, second)
import Data.Bool (bool)

-- | Execute an action only if the execution of the condition returns 'False'.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond act = cond >>= bool act (pure ())
{-# INLINE unlessM #-}

-- | Execute an action only if the execution of the condition returns 'True'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM = unlessM . fmap not
{-# INLINE whenM #-}

-- | A version of 'partition' that works with a monadic predicate.
--
-- > partitionM (Just . even) [1,2,3] == Just ([2], [1,3])
-- > partitionM (const Nothing) [1,2,3] == Nothing
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = go ([], [])
  where
    go acc [] = pure acc
    go acc (x : xs) = do
      cond <- f x
      go ((if cond then first else second) (x :) acc) xs
