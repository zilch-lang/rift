module Rift.CLI.Package.Search (packageSearchCLI) where

import Data.Foldable (fold)

import Options.Applicative

import Rift.Commands (PkgCommand(..))


packageSearchCLI :: ParserInfo PkgCommand
packageSearchCLI = info parsePackageCommand (fullDesc <> progDesc "Search for a package, and report all versions in all LTSs.")

parsePackageCommand :: Parser PkgCommand
parsePackageCommand =
  SearchPackage <$> strArgument (fold [ metavar "PACKAGE" ])
