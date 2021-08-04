{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS -Wno-name-shadowing #-}

module Rift.Environment.Setup (setupEnv) where

import Data.Maybe (fromMaybe)

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as Text

import Rift.Environment.Def (Environment(..))
import Rift.Environment.TH (rf)
import qualified Rift.Logger as Logger

import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory(XdgData), doesPathExist, findExecutable)
import System.Envy ((.=))
import qualified System.Envy as E
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)


type Setup m = (MonadIO m)

-- | Setups the environment needed for the project manager to correctly work:
--
--   * Fetches the @RIFT_HOME@ environment variable which defaults to @$XDG_DATA_HOME/rift@
--
--   * Sets the @RIFT_CFG@ environment variable to @$RIFT_HOME/config.dhall@ to allow using @env:RIFT_CFG@ in the project configuration file
--
--   * Writes the config template (which contains useful types and functions for the definition of a project) to @$RIFT_CFG$ if the file does not already exist
--
--   * Checks whether the @dhall-to-json@ executable is in the PATH or not
--
--   * Warns the user if the package set has not been initialized already (unless @warnAboutPkgsSetNotInit@ is set to @False@)
setupEnv :: Setup m => Bool -> m Environment
setupEnv warnAboutPkgsSetNotInit = liftIO do
  E.runEnv (E.envMaybe @FilePath "RIFT_HOME") >>= \ case
    Left err       -> do
      Logger.error $ Text.pack err
      exitFailure
    Right riftHome -> do
      riftHome <- ($ riftHome) <$> (fromMaybe <$> getXdgDirectory XdgData "rift")
      let configPath = riftHome </> "config.dhall"

      E.runEnv (E.envMaybe @FilePath "RIFT_CFG") >>= \ case
        Left _  -> do
          E.setEnvironment (E.makeEnv [ "RIFT_CFG" .= configPath ]) >>= \ case
            Left err -> do
              Logger.error $ Text.pack err
              exitFailure
            Right _  -> writeDhallConfigToRiftCfg configPath
        Right riftCfg -> do
          writeDhallConfigToRiftCfg $ fromMaybe configPath riftCfg

      dhallJsonExe <- findExecutable "dhall-to-json" >>= \ case
        Nothing -> do
          Logger.error "Executable 'dhall-to-json' not found in the PATH.\nSee <https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-json#readme> for instructions on how to install it."
          exitFailure
        Just p  -> pure p

      when warnAboutPkgsSetNotInit do
        let packageSetPath = riftHome </> "pkgs"
        exists <- doesPathExist packageSetPath
        unless exists do
          Logger.warn $ "Package set not initialized.\nPlease run 'rift package update'."

      git <- findExecutable "git" >>= \ case
        Nothing -> do
          Logger.error "Executable 'git' not found in the PATH."
          exitFailure
        Just p  -> pure p

      pure $ Env { riftHome, pkgsHome = riftHome </> "pkgs", dhallToJson = dhallJsonExe, git }

-- | Writes the default template to the given configuration path.
writeDhallConfigToRiftCfg :: Setup m => FilePath -> m ()
writeDhallConfigToRiftCfg cfgPath = liftIO do
  let defaultDhallConfig = [rf|default-config.dhall|]

  alreadyExists <- doesPathExist cfgPath
  unless alreadyExists do
    Logger.info $ "Creating default configuration file at path '" <> Text.pack cfgPath <> "'.\nThis file can be referenced using 'env:RIFT_CFG' in your project configuration."

    createDirectoryIfMissing True (takeDirectory cfgPath)
    writeFile cfgPath defaultDhallConfig
