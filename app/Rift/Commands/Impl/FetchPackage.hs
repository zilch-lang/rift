{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.FetchPackage where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Network.HTTP.Req (MonadHttp)
import Rift.Commands.Impl.Utils.Download (fetchPackageTo', resolvePackage)
import Rift.Commands.Impl.Utils.Paths (ltsPath)
import Rift.Config.PackageSet (LTSVersion (..), readLTSVersion)
import Rift.Config.Version (parseVersionConstraint, trueConstraint)
import Rift.Environment (Environment (..))
import System.FilePath ((</>))

fetchPackageCommand :: (MonadIO m, MonadHttp m, MonadMask m) => Text -> Maybe Text -> Maybe Text -> Bool -> Environment -> m ()
fetchPackageCommand name versionConstraint ltsName force env = do
  let lts = fromMaybe Unstable (ltsName >>= readLTSVersion)
  constr <- fromMaybe trueConstraint <$> traverse parseVersionConstraint versionConstraint
  pkg <- resolvePackage name lts constr force [] env

  ltsDir <- fromJust <$> ltsPath (riftCache env) lts

  void $ fetchPackageTo' lts (ltsDir </> "sources") (riftCache env </> "extra-deps") force True env pkg
