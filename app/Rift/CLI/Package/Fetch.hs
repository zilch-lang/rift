module Rift.CLI.Package.Fetch (packageFetchCLI) where

import Data.Foldable (fold)
import Options.Applicative
import Rift.Commands (PkgCommand (..))

packageFetchCLI :: ParserInfo PkgCommand
packageFetchCLI = info parsePackageCommand (fullDesc <> progDesc "Fetch a single package (and all its dependencies) inside the local cache.")

parsePackageCommand :: Parser PkgCommand
parsePackageCommand =
  FetchPackage
    <$> strArgument (fold [metavar "PACKAGE"])
    <*> optional (strOption $ fold [long "version", metavar "RANGE", help "A version constraint to satisfy."])
    <*> optional (strOption $ fold [long "lts", metavar "LTS", help "The LTS where to get the package from."])
    <*> switch (fold [long "force", short 'f', help "Whether to force fetching the package even if it is already cached."])
