{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Rift.Commands.Impl.Utils.Config where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import Dhall (auto, input, inputFile)
import Rift.Commands.Impl.Utils.Paths (projectDhall, riftDhall)
import Rift.Config.Configuration (Configuration (..))
import qualified Rift.Config.Configuration as Configuration
import Rift.Config.Project (ProjectType)
import Rift.Environment (Environment (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))

readPackageDhall :: (MonadIO m) => FilePath -> m ProjectType
readPackageDhall path = liftIO $ input auto $ "toMap " <> Text.pack (path </> projectDhall)

readRiftDhall :: (MonadIO m) => FilePath -> Environment -> m Configuration
readRiftDhall path env = do
  localConfig <- liftIO do
    let path' = path </> riftDhall
    fileExists <- doesFileExist path'
    if fileExists then Just <$> inputFile auto path' else pure Nothing
  globalConfig <- liftIO do
    let path' = riftConfig env </> riftDhall
    fileExists <- doesFileExist path'
    if fileExists then Just <$> inputFile auto path' else pure Nothing
  pure $ Configuration.merge localConfig globalConfig
