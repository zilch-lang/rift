module Rift.CLI.Path.Query (pathQueryCLI) where

import Data.Foldable (fold)
import Options.Applicative
import Rift.Commands (PathCommand (..))

pathQueryCLI :: ParserInfo PathCommand
pathQueryCLI = info parseQueryCommand (fullDesc <> progDesc "Query a single path, and print it to stdout.")

parseQueryCommand :: Parser PathCommand
parseQueryCommand =
  Query
    <$> strArgument (fold [metavar "KEY"])
