{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.FetchPackage where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Dhall (auto, inputFile)
import Network.HTTP.Req (MonadHttp)
import Rift.Commands.Impl.Utils.Download (downloadAndExtract)
import Rift.Config.PackageSet (LTSVersion (..), Package (..), Snapshot (..), readLTSVersion)
import Rift.Config.Project (packageSourceToDependency)
import Rift.Environment (Environment (..))
import qualified Rift.Logger as Logger
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>))

fetchPackageCommand :: (MonadIO m, MonadHttp m, MonadMask m) => Text -> Maybe Text -> Maybe Text -> Environment -> m ()
fetchPackageCommand name versionConstraint ltsName env = do
  let lts = fromMaybe Unstable (ltsName >>= readLTSVersion)
      ltsTag = show lts
      ltsHashFile = riftCache env </> "hashes" </> ltsTag <.> "hash"

  unlessM (liftIO $ doesFileExist ltsHashFile) do
    Logger.error $ "LTS '" <> Text.pack ltsTag <> "' not found in global cache.\nPerhaps you want to run 'rift package update'?"
    liftIO exitFailure

  ltsHash <- liftIO $ Text.readFile ltsHashFile
  let ltsDir = riftCache env </> (ltsTag <> "." <> Text.unpack ltsHash)

  Snapshot _ _ packageSet <- liftIO (inputFile auto (ltsDir </> "lts" </> "packages" </> "set" <.> "dhall") :: IO Snapshot)
  let packages = filter (\(Pkg n _ _ _ _ _) -> n == name) packageSet

  case sortBy (flip compare `on` version) packages of
    [] -> do
      Logger.error $ "Package '" <> name <> "' not found in LTS '" <> Text.pack ltsTag <> "'"
      liftIO exitFailure
    p : ps -> do
      -- TODO: take @versionConstraint@ in account
      let dir = ltsDir </> "sources" </> Text.unpack name <.> Text.unpack (version p)

      -- TODO: do transitively: also download all dependencies after
      (_, project) <- downloadAndExtract (\_ _ _ -> dir) (packageSourceToDependency $ src p) env

      pure ()
