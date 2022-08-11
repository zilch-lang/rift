{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rift.Commands.Executor where

import Rift.Environment (Environment)

-- | The class of executable commands in an IO-like monad.
class (Monad m) => CommandExecutor c m where
  -- | Executes a command of type @c@ given an 'Environment', in a monad @m@ which is at least a @'MonadIO' m@.
  executeCommand :: (?logLevel :: Int) => c -> Environment -> m ()
