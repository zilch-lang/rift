module Flags where


import Command

import Data.Foldable (fold)

import Options.Applicative



parseCLI :: IO Command
parseCLI = customExecParser preferences opts
  where
    opts = info (cli <**> helper) (fullDesc <> progDesc "Rift, the manager of Zilch projects")
    preferences = prefs showHelpOnError

cli :: Parser Command
cli = hsubparser $
  fold [ command "update-set" $ info updateSetCommand $ fullDesc <> progDesc "Updates the package set to the latest version referenced in your `project.dhall`" ]

------------------------------------


updateSetCommand :: Parser Command
updateSetCommand = pure Update
