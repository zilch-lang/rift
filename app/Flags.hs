module Flags where


import Data.Foldable (fold)

import Options.Applicative


data Flags
  = UpdateSet


parseCLI :: IO Flags
parseCLI = customExecParser preferences opts
  where
    opts = info (cli <**> helper) (fullDesc <> progDesc "Rift, the manager of Zilch projects")
    preferences = prefs showHelpOnError

cli :: Parser Flags
cli = hsubparser $
  fold [ command "update-set" $ info updateSetCommand $ fullDesc <> progDesc "Updates the package set to the latest version referenced in your `project.dhall`" ]

------------------------------------


updateSetCommand :: Parser Flags
updateSetCommand = pure UpdateSet
