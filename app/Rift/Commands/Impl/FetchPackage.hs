{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.FetchPackage where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Req (MonadHttp)
import Rift.Commands.Impl.Utils.Download (resolvePackage)
import Rift.Config.PackageSet (LTSVersion (..), readLTSVersion)
import Rift.Config.Version (parseVersionConstraint, trueConstraint)
import Rift.Environment (Environment (..))

fetchPackageCommand :: (MonadIO m, MonadHttp m, MonadMask m) => Text -> Maybe Text -> Maybe Text -> Bool -> Environment -> m ()
fetchPackageCommand name versionConstraint ltsName force env = do
  let lts = fromMaybe Unstable (ltsName >>= readLTSVersion)
  constr <- fromMaybe trueConstraint <$> traverse parseVersionConstraint versionConstraint
  void $ resolvePackage name lts constr force [] env
