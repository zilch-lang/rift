{-# LANGUAGE OverloadedStrings #-}

module Rift.Commands.Impl.Utils.Config where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import Dhall (auto, input)
import Rift.Commands.Impl.Utils.Paths (projectDhall)
import Rift.Config.Project (ProjectType)
import System.FilePath ((</>))

readPackageDhall :: (MonadIO m) => FilePath -> m ProjectType
readPackageDhall path = liftIO $ input auto $ "toMap " <> Text.pack (path </> projectDhall)
