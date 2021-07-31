{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ConstraintKinds #-}

module Environment (setupEnv) where

import Data.Maybe (fromMaybe)

import Control.Monad (unless)

import Environment.TH (rf)

import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory(XdgData), doesPathExist)
import System.Envy ((.=))
import qualified System.Envy as E
import System.FilePath ((</>), takeDirectory)
import System.IO (hPrint, stderr)
import Control.Monad.IO.Class (MonadIO(..))

type Setup m = (MonadIO m)

setupEnv :: Setup m => m ()
setupEnv = liftIO do
  E.runEnv (E.envMaybe @FilePath "RIFT_HOME") >>= \ case
    Left err       -> hPrint stderr err
    Right riftHome -> do
      riftHome <- ($ riftHome) <$> (fromMaybe <$> getXdgDirectory XdgData "rift")
      let configPath = riftHome </> "config.dhall"

      E.runEnv (E.envMaybe @FilePath "RIFT_CFG") >>= \ case
        Left _  -> do
          E.setEnvironment (E.makeEnv [ "RIFT_CFG" .= configPath ]) >>= \ case
            Left err -> hPrint stderr err
            Right _  -> writeDhallConfigToRiftCfg configPath
        Right riftCfg -> do
          writeDhallConfigToRiftCfg $ fromMaybe configPath riftCfg


writeDhallConfigToRiftCfg :: Setup m => FilePath -> m ()
writeDhallConfigToRiftCfg cfgPath = liftIO do
  let defaultDhallConfig = [rf|defaultConfig.dhall|]

  alreadyExists <- doesPathExist cfgPath
  unless alreadyExists do
    createDirectoryIfMissing True (takeDirectory cfgPath)
    writeFile cfgPath defaultDhallConfig
