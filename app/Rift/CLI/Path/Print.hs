module Rift.CLI.Path.Print (pathPrintCLI) where

import Options.Applicative
import Rift.Commands (PathCommand (..))

pathPrintCLI :: ParserInfo PathCommand
pathPrintCLI = info parsePrintCommand (fullDesc <> progDesc "Print all known paths to stdout.")

parsePrintCommand :: Parser PathCommand
parsePrintCommand = pure Print
