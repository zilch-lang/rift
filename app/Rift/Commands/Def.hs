{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wno-overlapping-patterns #-}
{-# OPTIONS -Wno-unused-matches #-}

module Rift.Commands.Def where

import Control.Monad.IO.Class (MonadIO)

import Data.Text (Text)

import Rift.Commands.Executor
import Rift.Commands.Impl.SearchPackage (searchPackageCommand)
import Rift.Commands.Impl.UpdatePackageSet (updatePackageSetCommand)

-- | A basic command is either a command on packages or a command on projects.
data Command
  = Package PkgCommand
  | Project ProjCommand

-- | A command acting on the package set.
data PkgCommand
  = -- | Updates the package set to the latest version available on the github repository.
    UpdatePackageSet
  | -- | Search a package across all LTSs, reporting valid and broken versions.
    SearchPackage
      Text            -- ^  The name of the apckage to search for.

-- | A command acting on the current project.
data ProjCommand


instance (MonadIO m) => CommandExecutor Command m where
  executeCommand (Package c) e = executeCommand c e
  executeCommand (Project c) e = executeCommand c e

instance (MonadIO m) => CommandExecutor PkgCommand m where
  executeCommand UpdatePackageSet e  = updatePackageSetCommand e
  executeCommand (SearchPackage p) e = searchPackageCommand p e
  executeCommand _ e = error "not yet implemented"

instance (MonadIO m) => CommandExecutor ProjCommand m where
  executeCommand _ e = error "not yet implemented"
