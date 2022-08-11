{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Exception (SomeException, handle, throwIO)
import qualified Data.Text as Text
import Network.HTTP.Req (MonadHttp (..))
import Rift.CLI (parseCLI)
import Rift.Commands (Command (Package), PkgCommand (UpdatePackageSet), executeCommand)
import Rift.Environment (setupEnv)
import qualified Rift.Logger as Logger
import System.Exit (exitFailure)

main :: IO ()
main = do
  cmd <- parseCLI

  env <- setupEnv case cmd of
    Package UpdatePackageSet -> False
    _ -> True

  handle mkError (executeCommand cmd env)

  pure ()
  where
    mkError (err :: SomeException) = do
      Logger.error (Text.pack $ show err)
      exitFailure

instance MonadHttp IO where
  handleHttpException = throwIO
