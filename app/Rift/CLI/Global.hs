module Rift.CLI.Global (globalCLI) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Foldable (fold)

import Options.Applicative

import Rift.CLI.Package (packageCLI)
import Rift.CLI.Project (projectCLI)
import Rift.Commands (Command(..))


globalCLI :: MonadIO m => m Command
globalCLI = liftIO $ customExecParser preferences opts
  where
    opts = info (internalCLI <**> helper) (fullDesc <> progDesc "Rift, the manager of Zilch projects")
    preferences = prefs showHelpOnError

    internalCLI = hsubparser $ fold $ mconcat [ packageCLI, projectCLI ]
