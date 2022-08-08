{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.FetchPackage where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Dhall (auto, inputFile)
import Network.HTTP.Req (MonadHttp)
import Rift.Commands.Impl.Utils.Download (fetchPackageTo)
import Rift.Commands.Impl.Utils.Paths (ltsPath)
import Rift.Config.PackageSet (LTSVersion (..), Package (..), Snapshot (..), readLTSVersion)
import Rift.Environment (Environment (..))
import qualified Rift.Logger as Logger
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>))

fetchPackageCommand :: (MonadIO m, MonadHttp m, MonadMask m) => Text -> Maybe Text -> Maybe Text -> Bool -> Environment -> m ()
fetchPackageCommand name versionConstraint ltsName force env = do
  let lts = fromMaybe Unstable (ltsName >>= readLTSVersion)
  ltsDir <- ltsPath (riftCache env) lts

  case ltsDir of
    Nothing -> do
      Logger.error $ "LTS '" <> Text.pack (show lts) <> "' not found in global cache.\nPerhaps you want to run 'rift package update'?"
      liftIO exitFailure
    Just ltsDir -> do
      Snapshot _ _ packageSet <- liftIO (inputFile auto (ltsDir </> "lts" </> "packages" </> "set" <.> "dhall") :: IO Snapshot)
      let packages = filter (\(Pkg n _ _ _ _ _) -> n == name) packageSet

      case packages of
        [] -> do
          Logger.error $ "Package '" <> name <> "' not found in LTS '" <> Text.pack (show lts) <> "'"
          liftIO exitFailure
        p : _ -> do
          -- TODO: take @versionConstraint@ in account
          fetchPackageTo ltsDir (riftCache env </> "extra-deps") force env p

      pure ()
