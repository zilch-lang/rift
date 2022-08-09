{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rift.Commands.Impl.Utils.Paths where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Text
import Rift.Config.Package (Package (..))
import Rift.Config.PackageSet (LTSVersion)
import Rift.Config.Source (Location (..), Source (..))
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

-- | The file containing all components inside the project.
projectDhall :: FilePath
projectDhall = "project" <.> "dhall"

-- | The file containing information such as the LTS used to compile the project, extra dependencies not present in the official package set, etc.
riftDhall :: FilePath
riftDhall = "rift" <.> "dhall"

-- | A simple file to setup the environment easily.
envDhall :: FilePath
envDhall = "env" <.> "dhall"

-- | Compute the effective path of a package.
packagePath :: Package -> FilePath -> FilePath
packagePath Pkg {..} relativeTo = sourcePath relativeTo src

-- | Compute the effective path of a single source.
sourcePath :: FilePath -> Source -> FilePath
sourcePath relativeTo src = relativeTo </> hash' src
  where
    hash' (Git (Remote url) rev) = "git-" <> show (hash $ encodeUtf8 (url <> "/" <> rev) :: Digest SHA256)
    hash' (Git url _) = error $ "Unsupported location type " <> show url <> " for git source"
    hash' (Tar _ sha256) = "tar-" <> Text.unpack sha256
    hash' (TarGz _ sha256) = "targz-" <> Text.unpack sha256
    hash' (Zip _ sha256) = "zip-" <> Text.unpack sha256

-- | Compute the effective path of a LTS.
ltsPath :: (MonadIO m) => FilePath -> LTSVersion -> m (Maybe FilePath)
ltsPath relativeTo lts = do
  let ltsTag = show lts
      ltsHashFile = relativeTo </> "hashes" </> ltsTag <.> "hash"

  isLtsCached <- liftIO $ doesFileExist ltsHashFile
  if isLtsCached
    then do
      ltsHash <- liftIO $ Text.readFile ltsHashFile
      pure . Just $ relativeTo </> ltsTag <.> Text.unpack ltsHash
    else pure Nothing
