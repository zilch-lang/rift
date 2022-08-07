{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Exception (throwIO)
import Network.HTTP.Req (MonadHttp (..))
import Rift.CLI (parseCLI)
import Rift.Commands (Command (Package), PkgCommand (UpdatePackageSet), executeCommand)
import Rift.Environment (setupEnv)

main :: IO ()
main = do
  cmd <- parseCLI

  env <- setupEnv case cmd of
    Package UpdatePackageSet -> False
    _ -> True

  executeCommand cmd env

  pure ()

instance MonadHttp IO where
  handleHttpException = throwIO
