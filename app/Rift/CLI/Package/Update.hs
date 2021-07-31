module Rift.CLI.Package.Update (packageUpdateCLI) where

import Options.Applicative

import Rift.Commands (PkgCommand(..))

packageUpdateCLI :: ParserInfo PkgCommand
packageUpdateCLI = info parsePackageCommand (fullDesc <> progDesc "Update the package set to the latest version available on <https://github.com/zilch-lang/pkgs>.")

parsePackageCommand :: Parser PkgCommand
parsePackageCommand = pure UpdatePackageSet
