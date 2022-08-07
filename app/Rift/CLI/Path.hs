module Rift.CLI.Path where

import Data.Foldable (fold)
import Options.Applicative
import Rift.CLI.Path.Print (pathPrintCLI)
import Rift.CLI.Path.Query (pathQueryCLI)
import Rift.Commands (Command (Path))

pathCLI :: [Mod CommandFields Command]
pathCLI =
  [command "path" $ info (hsubparser $ fold pathCommands) $ fullDesc <> progDesc "Query or print path information"]
  where
    pathCommands =
      [ command "query" $ Path <$> pathQueryCLI,
        command "print" $ Path <$> pathPrintCLI
      ]
