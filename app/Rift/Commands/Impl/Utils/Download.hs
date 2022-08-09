{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.Utils.Download where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Lazy as LBS
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Dhall (auto, inputFile)
import Network.HTTP.Req (GET (GET), MonadHttp, NoReqBody (..), lbsResponse, req, responseBody, responseHeader, useURI)
import Rift.Commands.Impl.Utils.Directory (copyDirectoryRecursive)
import Rift.Commands.Impl.Utils.ExtraDependencyCacheManager (insertExtraDependency)
import Rift.Commands.Impl.Utils.Paths (ltsPath, packagePath, projectDhall, riftDhall, sourcePath)
import Rift.Config.Configuration (Configuration (..))
import Rift.Config.Package (ExtraPackage (..), Package (..))
import Rift.Config.PackageSet (LTSVersion, Snapshot (..))
import Rift.Config.Project (ComponentType (..), ProjectType)
import Rift.Config.Source (Location (..), Source (..))
import Rift.Config.Version (ConstraintExpr, PackageDependency (..), VersionConstraint, trueConstraint, trueConstraintExpr)
import Rift.Environment.Def (Environment (..))
import qualified Rift.Logger as Logger
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Text.URI as URI
import Turtle (empty, procStrictWithErr)

resolvePackage :: (MonadIO m, MonadHttp m, MonadMask m) => Text -> LTSVersion -> (ConstraintExpr, VersionConstraint) -> Bool -> Environment -> m Package
resolvePackage name lts (constraintExpr, versionConstraint) force env = do
  ltsDir <- ltsPath (riftCache env) lts

  case ltsDir of
    Nothing -> do
      Logger.error $ "LTS '" <> Text.pack (show lts) <> "' not found in global cache.\nPerhaps you want to run 'rift package update'?"
      liftIO exitFailure
    Just ltsDir -> do
      Snapshot _ _ packageSet <- liftIO (inputFile auto (ltsDir </> "lts" </> "packages" </> "set" <.> "dhall") :: IO Snapshot)
      let packages = filter (\(Pkg n _ _ _ _ _ _) -> n == name) packageSet

      case sortBy (flip compare `on` version) packages of
        [] -> do
          -- extraCache <- readExtraDepsCache (riftCache env </> "extra-deps")

          Logger.error $ "Package '" <> name <> "' not found in LTS '" <> Text.pack (show lts) <> "'"
          liftIO exitFailure
        ps -> case filter (versionConstraint . version) ps of
          [] -> do
            let v1 = version $ head ps
            Logger.error $
              "Chosen LTS does not have a version of the package '"
                <> name
                <> "' satisfying the given constraint.\nLatest version found: "
                <> Text.pack (show v1)
                <> "\n\nWhile checking that the constraint '"
                <> Text.pack (show constraintExpr)
                <> "' holds."
            liftIO exitFailure
          p : _ -> do
            -- TODO: take @versionConstraint@ in account
            fetchPackageTo lts (ltsDir </> "sources") (riftCache env </> "extra-deps") force env p
            pure p

fetchPackageTo :: (MonadIO m, MonadHttp m, MonadMask m) => LTSVersion -> FilePath -> FilePath -> Bool -> Environment -> Package -> m ()
fetchPackageTo lts ltsDir extraDepDir force env pkg@Pkg {..} = do
  let pkgDir = packagePath pkg ltsDir
  (_, components, configuration) <- do
    pathExists <- liftIO $ doesDirectoryExist pkgDir

    if not force && pathExists
      then do
        Logger.info $ "Package '" <> name <> "' is already cached.\nUse --force to redownload it."

        project <- liftIO $ inputFile auto (pkgDir </> projectDhall)
        configuration <- liftIO $ inputFile auto (pkgDir </> riftDhall)
        pure (pkgDir, project, configuration)
      else do
        (pkgDir, project, configuration@(Configuration _ extraDeps)) <- downloadAndExtract pkgDir src False env
        let deps = project >>= \(ComponentType _ _ d _ _ _) -> d

        -- - fetch extra dependencies
        forM_ extraDeps \(ExtraPkg name version source _) -> do
          let srcPath = sourcePath extraDepDir source
          pathExists <- liftIO $ doesDirectoryExist srcPath

          when (force || not pathExists) do
            void $ downloadAndExtract srcPath source True env
            insertExtraDependency name version srcPath source env

        -- - fetch dependencies
        forM_ deps \(Version name constraint) -> do
          pkg <- resolvePackage name lts (undefined, constraint) force env
          fetchPackageTo lts ltsDir extraDepDir force env pkg

          pure ()

        pure (pkgDir, project, configuration)

  pure ()

downloadAndExtract :: (MonadIO m, MonadHttp m, MonadMask m) => FilePath -> Source -> Bool -> Environment -> m (FilePath, ProjectType, Configuration)
downloadAndExtract dir dep isExtra env =
  case dep of
    Tar (Remote url) sha256 -> unpackArchive url sha256 \path dir tar -> do
      liftIO . Tar.unpack dir $ Tar.read tar
      liftIO $ copyArchive url dir path "Tarball" =<< listDirectory dir
    Tar (Local uri) sha256 -> error "TODO"
    Tar (Environment _) _ -> do
      Logger.error "Cannot fetch .tar archive from environment"
      liftIO exitFailure
    Tar Missing _ -> do
      Logger.error "Cannot fetch .tar archive"
      liftIO exitFailure
    TarGz (Remote url) sha256 -> unpackArchive url sha256 \path dir tar -> do
      liftIO . Tar.unpack dir . Tar.read $ GZip.decompress tar
      liftIO $ copyArchive url dir path "GZipped tarball" =<< listDirectory dir
    TarGz (Local uri) sha256 -> error "TODO"
    TarGz (Environment _) _ -> do
      Logger.error "Cannot fetch .tar.gz archive from environment"
      liftIO exitFailure
    TarGz Missing _ -> do
      Logger.error "Cannot fetch .tar.gz archive"
      liftIO exitFailure
    Zip (Remote url) sha256 -> unpackArchive url sha256 \path dir zip -> do
      liftIO . Zip.extractFilesFromArchive [Zip.OptDestination dir] $ Zip.toArchive zip
      liftIO $ copyArchive url dir path "Zipped" =<< listDirectory dir
    Zip (Local uri) sha256 -> error "TODO"
    Zip (Environment _) _ -> do
      Logger.error "Cannot fetch .zip archive from environment"
      liftIO exitFailure
    Zip Missing _ -> do
      Logger.error "Cannot fetch .zip archive"
      liftIO exitFailure
    Git (Remote url) rev -> do
      let path = dir
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
      pure (path, project, configuration)
    Git _ _ -> do
      Logger.error "Cannot fetch a git repository from any other location than a link"
      liftIO exitFailure
  where
    unpackArchive url sha256 unpack = do
      let path = dir
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

      pure (path, project, configuration)

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
