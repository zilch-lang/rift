module Rift.CLI.Project (projectCLI) where

import Options.Applicative

import Rift.Commands (Command)

projectCLI :: [Mod CommandFields Command]
projectCLI = mempty
