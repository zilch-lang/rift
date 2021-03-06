module Rift.CLI.Package (packageCLI) where

import Data.Foldable (fold)

import Options.Applicative

import Rift.CLI.Package.Update (packageUpdateCLI)
import Rift.CLI.Package.Search (packageSearchCLI)
import Rift.Commands (Command(..))


-- | The package CLI contains:
--
--   * Subcommands:
--
--     * @update@: Update the current copy of the package set to the latest version available
--
--   * Flags & options:
--
--     * @-h@/@--help@: Show the help menu of the subcommand
packageCLI :: [Mod CommandFields Command]
packageCLI =
  let cmd name meta = command name $ info (hsubparser $ fold packageSubCommands) $ fullDesc <> progDesc meta
  in [ cmd "package" "Package management related commands."
     , cmd "pkg" "A synonym for 'package'." ]
  where
    packageSubCommands =
      let cmd name parser = command name $ Package <$> parser
      in [ cmd "update" packageUpdateCLI
         , cmd "search" packageSearchCLI
         ]
