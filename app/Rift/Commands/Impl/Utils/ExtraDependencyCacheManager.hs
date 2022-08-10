{-# LANGUAGE BlockArguments #-}

module Rift.Commands.Impl.Utils.ExtraDependencyCacheManager where

import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Dhall (auto, inputFile)
import Dhall.Core (pretty)
import Dhall.Marshal.Encode (Encoder (embed), inject)
import Rift.Config.ExtraDependencyCache (ExtraCache (..))
import Rift.Config.Source (Source)
import Rift.Config.Version (SemVer)
import Rift.Environment (Environment (riftCache))
import Rift.Internal.LockFile (withLockFile)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

insertExtraDependency :: (MonadIO m) => Text -> SemVer -> FilePath -> Source -> Environment -> m ()
insertExtraDependency name version path src env = do
  let cachePath = riftCache env </> "extra-deps" </> "hashes" <.> "cache"

  liftIO $ withLockFile (riftCache env </> "extra-deps" </> "hashes" <.> "lock") do
    ExtraCache versions paths <- readExtraCache cachePath
    let versions' = MultiMap.insert name version versions
        paths' = Map.insert (name, version) (path, src) paths
    writeExtraCache cachePath (ExtraCache versions' paths')

  pure ()

-- | Read the cache into a Haskell structure.
readExtraCache :: (MonadIO m) => FilePath -> m ExtraCache
readExtraCache path = do
  fileExists <- liftIO $ doesFileExist path
  if fileExists
    then liftIO $ inputFile auto path
    else pure $ ExtraCache MultiMap.empty mempty

-- | Write the cache back to a file.
writeExtraCache :: (MonadIO m) => FilePath -> ExtraCache -> m ()
writeExtraCache path cache = liftIO $ Text.writeFile path (pretty $ embed inject cache)
