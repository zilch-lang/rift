{-# LANGUAGE BlockArguments #-}

module Main where

import Rift.CLI (parseCLI)
import Rift.Commands (executeCommand, Command(Package), PkgCommand(UpdatePackageSet))
import Rift.Environment (setupEnv)

main :: IO ()
main = do
  cmd <- parseCLI

  env <- setupEnv case cmd of
    Package UpdatePackageSet -> False
    _                        -> True

  executeCommand cmd env

  pure ()
