{-# LANGUAGE ConstraintKinds #-}

module Command where

import Control.Monad.IO.Class (MonadIO)

import Environment


type CommandExecutor m = (MonadIO m)


data Command
  = Update


executeCommand :: CommandExecutor m => Command -> Environment -> m ()
executeCommand Update _ = do
  pure ()
executeCommand _ _ = pure ()
