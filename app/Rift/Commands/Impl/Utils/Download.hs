{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.Utils.Download where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Read (decimal)
import Dhall (auto, inputFile)
import Network.HTTP.Req (GET (GET), MonadHttp, NoReqBody (..), lbsResponse, req, responseBody, responseHeader, useURI)
import Rift.Commands.Impl.Utils.Directory (copyDirectoryRecursive)
import Rift.Commands.Impl.Utils.Paths (projectDhall, riftDhall)
import Rift.Config.Configuration (Configuration)
import Rift.Config.Project (ProjectType (..))
import Rift.Config.Source (Location (..), Source (..))
import Rift.Environment (Environment (..))
import qualified Rift.Logger as Logger
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Text.URI as URI
import Turtle (empty, procStrictWithErr)

downloadAndExtract :: (MonadIO m, MonadHttp m, MonadMask m) => (Text -> Text -> Bool -> FilePath) -> Source -> Environment -> m (FilePath, ProjectType, Configuration)
downloadAndExtract dir dep env =
  case dep of
    Tar (Remote url) sha256 -> unpackArchive url sha256 \path dir tar -> do
      liftIO . Tar.unpack dir $ Tar.read tar
      liftIO $ copyArchive url dir path "Tarball" =<< listDirectory dir
    TarGz (Remote url) sha256 -> unpackArchive url sha256 \path dir tar -> do
      liftIO . Tar.unpack dir . Tar.read $ GZip.decompress tar
      liftIO $ copyArchive url dir path "GZipped tarball" =<< listDirectory dir
    Zip (Remote url) sha256 -> unpackArchive url sha256 \path dir zip -> do
      liftIO . Zip.extractFilesFromArchive [Zip.OptDestination dir] $ Zip.toArchive zip
      liftIO $ copyArchive url dir path "Zipped" =<< listDirectory dir
    Git (Remote url) rev -> do
      let path = riftCache env </> dir url rev True
      unlessM (liftIO $ doesDirectoryExist path) do
        Logger.info $ "Checking git repository '" <> url <> "' at revision '" <> rev <> "'..."

        liftIO $ withSystemTempDirectory "rift" \dir -> do
          let gitexe = Text.pack $ git env
          (exit, out, err) <- procStrictWithErr (Text.pack $ git env) ["-C", Text.pack dir, "clone", url, "."] empty
          unless (exit == ExitSuccess) do
            Logger.error $
              "Could not fetch git repository at '" <> url <> "'.\n* Standard output:\n"
                <> Text.unlines (mappend "> " <$> Text.lines out)
                <> "\n* Standard error:\n"
                <> Text.unlines (mappend "> " <$> Text.lines err)
            exitFailure
          (exit, out, err) <- procStrictWithErr gitexe ["-C", Text.pack dir, "checkout", rev] empty
          unless (exit == ExitSuccess) do
            Logger.error $
              "Could not fetch git repository at '" <> url <> "'.\n* Standard output:\n"
                <> Text.unlines (mappend "> " <$> Text.lines out)
                <> "\n* Standard error:\n"
                <> Text.unlines (mappend "> " <$> Text.lines err)
            exitFailure
          (exit, out, err) <- procStrictWithErr gitexe ["-C", Text.pack dir, "reset", "--hard"] mempty
          unless (exit == ExitSuccess) do
            Logger.error $
              "Could not fetch git repository at '" <> url <> "'.\n* Standard output:\n"
                <> Text.unlines (mappend "> " <$> Text.lines out)
                <> "\n* Standard error:\n"
                <> Text.unlines (mappend "> " <$> Text.lines err)
            exitFailure

          removeDirectoryRecursive (dir </> ".git")

          unlessM (liftIO . doesFileExist $ dir </> projectDhall) do
            Logger.error $ "Zipped file '" <> url <> "' does not contain a Rift project (file '" <> Text.pack projectDhall <> "' not present)"
            liftIO exitFailure

          liftIO $ copyDirectoryRecursive dir path (const True)

      project <- liftIO $ inputFile auto (path </> projectDhall)
      configuration <- liftIO $ inputFile auto (path </> riftDhall)
      pure (path, configuration, project)
  where
    unpackArchive url sha256 unpack = do
      let path = riftCache env </> dir url sha256 False
      unlessM (liftIO $ doesDirectoryExist path) do
        Logger.info $ "Downloading file '" <> url <> "'..."

        !(resp, _ :: Integer) <- do
          uri <- URI.mkURI url
          response <- case useURI uri of
            Nothing -> do
              Logger.error $ "URI '" <> url <> "' does not seem to be either HTTP or HTTPS"
              liftIO exitFailure
            Just (Left (url', options)) -> req GET url' NoReqBody lbsResponse options
            Just (Right (url', options)) -> req GET url' NoReqBody lbsResponse options
          pure (responseBody response, either (const 0) fst . decimal . decodeUtf8 . fromMaybe "0" $ responseHeader response "Content-Length")

        let hashed = Text.pack $ show (hash (LBS.toStrict resp) :: Digest SHA256)

        when (hashed /= sha256) do
          Logger.error $ "Cannot validate extra dependency '" <> url <> "':\n* Expected SHA256: " <> sha256 <> "\n* Got SHA256: " <> hashed
          liftIO exitFailure

        withSystemTempDirectory "rift" \dir -> unpack path dir resp

      project <- liftIO $ inputFile auto (path </> projectDhall)
      configuration <- liftIO $ inputFile auto (path </> riftDhall)

      pure (path, configuration, project)

    copyArchive url _ _ kind [] = do
      Logger.error $ kind <> " file '" <> url <> "' is empty."
      liftIO exitFailure
    copyArchive url dir path kind [dir2] = do
      let dir' = dir </> dir2
      unlessM (liftIO . doesFileExist $ dir' </> projectDhall) do
        Logger.error $ kind <> " file '" <> url <> "' does not contain a Rift project (file '" <> Text.pack projectDhall <> "' not present)"
        liftIO exitFailure
      liftIO $ copyDirectoryRecursive dir' path (const True)
    copyArchive url dir path kind _ = do
      unlessM (liftIO . doesFileExist $ dir </> projectDhall) do
        Logger.error $ kind <> " file '" <> url <> "' does not contain a Rift project (file '" <> Text.pack projectDhall <> "' not present)"
        liftIO exitFailure
      liftIO $ copyDirectoryRecursive dir path (const True)
