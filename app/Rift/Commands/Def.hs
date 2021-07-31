{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wno-overlapping-patterns #-}

module Rift.Commands.Def where

import Control.Monad.IO.Class (MonadIO)

import Rift.Commands.Executor
import Rift.Commands.Impl.UpdatePackageSet (updatePackageSetCommand)
import Rift.Environment (Environment(..))

data Command
  = Package PkgCommand
  | Project ProjCommand

data PkgCommand
  = UpdatePackageSet

data ProjCommand


instance (MonadIO m) => CommandExecutor Command m where
  executeCommand (Package c) e = executeCommand c e
  executeCommand (Project c) e = executeCommand c e

instance (MonadIO m) => CommandExecutor PkgCommand m where
  executeCommand UpdatePackageSet e = updatePackageSetCommand e
  executeCommand _ e = error "not yet implemented"

instance (MonadIO m) => CommandExecutor ProjCommand m where
  executeCommand _ e = error "not yet implemented"
