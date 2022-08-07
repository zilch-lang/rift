module Rift.CLI.Project.Build where

import Data.Foldable (fold)
import Options.Applicative
import Rift.Commands (ProjCommand (..))

projectBuildCLI :: ParserInfo ProjCommand
projectBuildCLI = info parseProjectCommand (fullDesc <> progDesc "Create a new project in an empty directory.")

parseProjectCommand :: Parser ProjCommand
parseProjectCommand =
  BuildProject
    <$> switch (fold [long "dry-run", help "Don't build anything, only print commands executed"])
    <*> option auto (fold [long "cores", short 'j', metavar "INTEGER", value 1, help "How many cores to use to build"])
    <*> switch (fold [long "dirty", help "Dirty all files in the project before building"])
    <*> many (strArgument (fold [metavar "COMPONENT", help "Components to build, or everything if none is specified"]))
