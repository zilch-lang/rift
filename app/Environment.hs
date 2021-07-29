{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Environment (setupEnv) where

import Data.Maybe (fromMaybe)

import Control.Monad (unless)

import Environment.TH (rf)

import System.Directory
import System.Envy ((.=))
import qualified System.Envy as E
import System.FilePath ((</>), takeDirectory)
import System.IO (hPrint, stderr)

setupEnv :: IO ()
setupEnv = do
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


writeDhallConfigToRiftCfg :: FilePath -> IO ()
writeDhallConfigToRiftCfg cfgPath = do
  let defaultDhallConfig = [rf|defaultConfig.dhall|]

  alreadyExists <- doesPathExist cfgPath
  unless alreadyExists do
    createDirectory (takeDirectory cfgPath)
    writeFile cfgPath defaultDhallConfig
