{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wno-overlapping-patterns #-}
{-# OPTIONS -Wno-unused-matches #-}

module Rift.Commands.Def where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Network.HTTP.Req (MonadHttp)
import Rift.Commands.Executor
import Rift.Commands.Impl.BuildProject (buildProjectCommand)
import Rift.Commands.Impl.FetchPackage (fetchPackageCommand)
import Rift.Commands.Impl.NewProject (newProjectCommand)
import Rift.Commands.Impl.PrintPaths (printPaths)
import Rift.Commands.Impl.QueryPaths (queryAndPrintPaths)
import Rift.Commands.Impl.SearchPackage (searchPackageCommand)
import Rift.Commands.Impl.UpdatePackageSet (updatePackageSetCommand)

-- | A basic command is either a command on packages or a command on projects.
data Command
  = Package PkgCommand
  | Project ProjCommand
  | Path PathCommand

-- | A command acting on the package set.
data PkgCommand
  = -- | Updates the package set to the latest version available on the github repository.
    UpdatePackageSet
  | -- | Search a package across all LTSs, reporting valid and broken versions.
    SearchPackage
      Text
      -- ^  The name of the package to search for.
  | -- | Fetch a package either from the default LTS (given in the config) or from the specified LTS.
    FetchPackage
      Text
      -- ^ The name of the package to fetch.
      (Maybe Text)
      -- ^ An optional version constraint to satisfy.
      (Maybe Text)
      -- ^ The LTS where to get the package from (defaults to the latest if not specified).
      Bool
      -- ^ Whether to force redownloading/unpacking the package.

-- | A command acting on the current project.
data ProjCommand
  = -- | Initializes a new project in the given directory.
    NewProject
      FilePath
      -- ^ Where to initialize the project
      (Maybe Text)
      -- ^ The name of the project
      (Maybe Text)
      -- ^ The template used for project generation
      Bool
      -- ^ Should we force project creation?
  | BuildProject
      Bool
      -- ^ Should we simply perform a dry run?
      Integer
      -- ^ The maximum number of cores allowed to be used to build package concurrently.
      --   Setting this to 0 makes it use the maximum number of cores available on the system.
      Bool
      -- ^ Should we dirty all the files in the project?
      --   This is useful when a change has not been detected.
      [Text]
      -- ^ The names of the components to build (lib:XXX or exe:XXX).
      --   If none are specified, build all the components found in the project.

data PathCommand
  = -- | Print all known/used paths.
    Print
  | -- | Query a single path key.
    Query
      Text

instance (MonadIO m, MonadHttp m, MonadMask m) => CommandExecutor Command m where
  executeCommand (Package c) e = executeCommand c e
  executeCommand (Project c) e = executeCommand c e
  executeCommand (Path c) e = executeCommand c e

instance (MonadIO m, MonadHttp m, MonadMask m) => CommandExecutor PkgCommand m where
  executeCommand UpdatePackageSet e = updatePackageSetCommand e
  executeCommand (SearchPackage p) e = searchPackageCommand p e
  executeCommand (FetchPackage n c l f) e = fetchPackageCommand n c l f e
  executeCommand _ e = error "not yet implemented"

instance (MonadIO m, MonadHttp m, MonadMask m) => CommandExecutor ProjCommand m where
  executeCommand (NewProject p n t f) e = newProjectCommand p n t f e
  executeCommand (BuildProject dry cores dirty comps) e = buildProjectCommand dry cores dirty comps e
  executeCommand _ e = error "not yet implemented"

instance (MonadIO m) => CommandExecutor PathCommand m where
  executeCommand Print e = printPaths e
  executeCommand (Query r) e = queryAndPrintPaths r e
