module Rift.CLI.Package.PurgeLTS (packagePurgeLTSCLI) where

import Options.Applicative
import Rift.Commands (PkgCommand (..))

packagePurgeLTSCLI :: ParserInfo PkgCommand
packagePurgeLTSCLI = info parsePackageCommand (fullDesc <> progDesc "Purge old generations of all LTSes ever fetched.")

parsePackageCommand :: Parser PkgCommand
parsePackageCommand = pure PurgeLTS
