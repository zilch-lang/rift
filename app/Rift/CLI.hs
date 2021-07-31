module Rift.CLI (parseCLI) where

import Control.Monad.IO.Class (MonadIO)

import Rift.CLI.Global (globalCLI)
import Rift.Commands (Command)

-- | Convenient alias for 'globalCLI'.
parseCLI :: (MonadIO m) => m Command
parseCLI = globalCLI
