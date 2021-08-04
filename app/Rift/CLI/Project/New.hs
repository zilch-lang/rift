module Rift.CLI.Project.New where

import Data.Foldable (fold)

import Options.Applicative

import Rift.Commands (ProjCommand(..))

projectNewCLI :: ParserInfo ProjCommand
projectNewCLI = info parseProjectCommand (fullDesc <> progDesc "Create a new project in an empty directory.")

parseProjectCommand :: Parser ProjCommand
parseProjectCommand =
  NewProject <$> strArgument (fold [ metavar "WHERE", help "Where to create the new project", showDefault, value "." ])
             <*> optional (strOption (fold [ long "name", metavar "NAME", help "The name of the project" ]))
             <*> optional (strOption (fold [ long "template", short 't', metavar "TEMPLATE", help "A template to apply for project generation" ]))
             <*> switch (fold [ long "force", short 'f', help "Force creation of the project in non-empty directories" ])
