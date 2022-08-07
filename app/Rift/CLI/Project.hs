module Rift.CLI.Project (projectCLI) where

import Data.Foldable (fold)
import Options.Applicative
import Rift.CLI.Project.Build (projectBuildCLI)
import Rift.CLI.Project.New (projectNewCLI)
import Rift.Commands (Command (..))

projectCLI :: [Mod CommandFields Command]
projectCLI =
  let cmd name meta = command name $ info (hsubparser $ fold packageSubCommands) $ fullDesc <> progDesc meta
   in [cmd "project" "Project management related commands."]
  where
    packageSubCommands =
      let cmd name parser = command name $ Project <$> parser
       in [ cmd "new" projectNewCLI,
            cmd "build" projectBuildCLI
          ]
