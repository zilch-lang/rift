{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Rift.Commands.Impl.Utils.LTS where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Rift.Config.PackageSet (LTSVersion, readLTSVersion)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, isExtensionOf, (<.>), (</>))

-- | Returns all unbroken LTS versions found in the local cache.
discoverLTSes :: (MonadIO m) => FilePath -> m [(FilePath, LTSVersion)]
discoverLTSes dir = do
  let hashesPath = dir </> "hashes"
  hashFiles <- filter (isExtensionOf "hash") <$> liftIO (listDirectory hashesPath)
  let ltsNames = dropExtension <$> hashFiles
  ltsHashes <- liftIO $ forM ltsNames \lts -> (lts,) <$> Text.readFile (hashesPath </> lts <.> "hash")

  pure $ filterBrokenLTSes ltsHashes
  where
    filterBrokenLTSes = filterMap (\(lts, hash) -> readLTSVersion (Text.pack lts) <&> \lts' -> (dir </> lts <.> Text.unpack hash, lts'))

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f l = catMaybes $ map f l
