module Control.Monad.Extra where

import Data.Bool (bool)

-- | Execute an action only if the execution of the condition returns 'False'.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond act = cond >>= bool act (pure ())
{-# INLINE unlessM #-}

-- | Execute an action only if the execution of the condition returns 'True'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM = unlessM . fmap not
{-# INLINE whenM #-}
