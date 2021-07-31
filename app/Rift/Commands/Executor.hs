{-# LANGUAGE MultiParamTypeClasses #-}

module Rift.Commands.Executor where

import Control.Monad.IO.Class (MonadIO)

import Rift.Environment (Environment)

class (MonadIO m) => CommandExecutor c m where
  executeCommand :: c -> Environment -> m ()
