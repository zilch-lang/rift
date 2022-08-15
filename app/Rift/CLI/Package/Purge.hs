module Rift.CLI.Package.Purge (packagePurgeCLI) where

import Data.Foldable (fold)
import Options.Applicative
import Rift.Commands (PkgCommand (..))

packagePurgeCLI :: ParserInfo PkgCommand
packagePurgeCLI = info parsePackageCommand (fullDesc <> progDesc "Purge the given package.")

parsePackageCommand :: Parser PkgCommand
parsePackageCommand =
  PurgePackage
    <$> strArgument (fold [metavar "PACKAGE", help "The package to purge from the cache"])
    <*> optional (strOption (fold [long "lts", metavar "LTS", help "In which LTS do we want to remove the package?"]))
    <*> optional (strOption (fold [long "version", metavar "CONSTRAINT", help "A filter to remove only certain packages"]))
    <*> switch (fold [long "transitive", help "Do we also remove dependencies?"])
