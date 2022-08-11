{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.Utils.Download where

import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
import qualified Algebra.Graph.AdjacencyMap as Cyclic
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Control.Exception (throwIO)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (foldl', foldrM)
import Data.Function (on)
import Data.List (find, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Dhall (auto, inputFile)
import Network.HTTP.Req (GET (GET), MonadHttp, NoReqBody (..), lbsResponse, req, responseBody, responseHeader, useURI)
import Rift.Commands.Impl.Utils.Config (readPackageDhall)
import Rift.Commands.Impl.Utils.Directory (copyDirectoryRecursive)
import Rift.Commands.Impl.Utils.ExtraDependencyCacheManager (insertExtraDependency)
import Rift.Commands.Impl.Utils.Paths (ltsPath, packagePath, projectDhall, riftDhall, sourcePath)
import Rift.Config.Configuration (Configuration (..))
import Rift.Config.Package (ExtraPackage (..), Package (..))
import Rift.Config.PackageSet (LTSVersion, Snapshot (..))
import Rift.Config.Project (ComponentType (..), ProjectType)
import Rift.Config.Source (Location (..), Source (..), prettySource)
import Rift.Config.Version (ConstraintExpr, PackageDependency (..), SemVer, VersionConstraint, trueConstraint)
import Rift.Environment.Def (Environment (..))
import Rift.Internal.Exceptions (RiftException (..))
import qualified Rift.Logger as Logger
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Text.URI as URI
import Turtle (empty, procStrictWithErr)

resolvePackage :: (MonadIO m, MonadHttp m, MonadMask m) => Text -> LTSVersion -> VersionConstraint -> Bool -> [Package] -> Environment -> m Package
resolvePackage name lts (constraintExpr, versionConstraint) force extra env = do
  ltsDir <- ltsPath (riftCache env) lts

  case ltsDir of
    Nothing -> do
      Logger.error $ "LTS '" <> Text.pack (show lts) <> "' not found in global cache.\nPerhaps you want to run 'rift package update'?"
      liftIO exitFailure
    Just ltsDir -> do
      Snapshot _ _ packageSet <- liftIO (inputFile auto (ltsDir </> "lts" </> "packages" </> "set" <.> "dhall") :: IO Snapshot)
      let packages = filter (\(Pkg n _ _ _ _ _) -> n == name) (extra <> packageSet)

      case sortBy (flip compare `on` version) packages of
        [] -> do
          liftIO $ throwIO $ NoSuchPackage name lts
        ps -> case filter (versionConstraint . version) ps of
          [] -> do
            let v1 = version $ head ps
            liftIO $ throwIO $ BrokenVersionConstraint name constraintExpr
          ps@(p : _) -> do
            let candidates = filter ((==) (version p) . version) ps

            let candidates2 = filter (not . broken) candidates
            when (null candidates2) do
              liftIO $ throwIO $ PackageIsBroken name (version p)

            when (length candidates2 /= length candidates) do
              Logger.warn $ "Ignoring broken sources for package '" <> name <> "'"

            let candidates3 = filter (not . deprecated) candidates
            if null candidates3
              then do
                Logger.warn $ "Cannot find a source for the package '" <> name <> " at version " <> Text.pack (show (version p)) <> " which is not deprecated"

                when (length candidates2 > 1) do
                  liftIO $ throwIO $ AmbiguousPackageSources name (version p) candidates2

                pure (head candidates2)
              else do
                when (length candidates3 /= length candidates2) do
                  Logger.warn $ "Ignoring deprecated sources for the package '" <> name <> "'"

                when (length candidates3 > 1) do
                  liftIO $ throwIO $ AmbiguousPackageSources name (version p) candidates3

                pure (head candidates3)

fetchPackageTo' :: (MonadIO m, MonadHttp m, MonadMask m) => LTSVersion -> FilePath -> FilePath -> Bool -> Bool -> Environment -> Package -> m (Acyclic.AdjacencyMap Package)
fetchPackageTo' lts ltsDir extraDepDir force infoCached env pkg =
  Acyclic.toAcyclic <$> fetchPackageTo Cyclic.empty lts ltsDir extraDepDir force infoCached env pkg >>= \case
    Just graph -> pure graph
    Nothing -> undefined

fetchPackageTo :: (MonadIO m, MonadHttp m, MonadMask m) => Cyclic.AdjacencyMap Package -> LTSVersion -> FilePath -> FilePath -> Bool -> Bool -> Environment -> Package -> m (Cyclic.AdjacencyMap Package)
fetchPackageTo depGraph lts ltsDir extraDepDir force infoCached env pkg@Pkg {..} = do
  checkCycles pkg depGraph

  let pkgDir = packagePath pkg ltsDir
  (graph', _, components, configuration) <- do
    pathExists <- liftIO $ doesDirectoryExist pkgDir

    if not force && pathExists
      then do
        case src of
          Directory _ -> pure ()
          _ | infoCached -> Logger.info $ "Package '" <> name <> "' is already cached.\nUse '--force' to redownload it."
          _ -> pure ()

        project <- liftIO $ readPackageDhall pkgDir
        configuration <- liftIO $ inputFile auto (pkgDir </> riftDhall)
        pure (depGraph, pkgDir, project, configuration)
      else do
        (pkgDir, project, configuration@(Configuration _ extraDeps)) <- downloadAndExtract pkgDir src env
        let deps = Map.elems project >>= \(ComponentType _ d _ _ _) -> d

        -- - fetch extra dependencies
        forM_ extraDeps \(ExtraPkg name version source) -> do
          let srcPath = sourcePath extraDepDir source
          pathExists <- liftIO $ doesDirectoryExist srcPath

          when (force || not pathExists) do
            (_, components, _) <- downloadAndExtract srcPath source env
            checkConsistency name version components
            insertExtraDependency name version srcPath source env

        -- - fetch dependencies
        graph' <- foldrM (resolveDep pkg lts ltsDir extraDepDir force env) depGraph deps

        pure (graph', pkgDir, project, configuration)

  checkConsistency name version components

  pure graph'
  where
    resolveDep :: (MonadIO m, MonadHttp m, MonadMask m) => Package -> LTSVersion -> FilePath -> FilePath -> Bool -> Environment -> PackageDependency -> Cyclic.AdjacencyMap Package -> m (Cyclic.AdjacencyMap Package)
    resolveDep thisPkg lts ltsDir extraDepDir force env (Version name constraint) depGraph = do
      pkg <- resolvePackage name lts constraint force [] env
      fetchPackageTo (depGraph `Cyclic.overlay` Cyclic.edge thisPkg pkg) lts ltsDir extraDepDir force infoCached env pkg
    {-# INLINE resolveDep #-}

    checkCycles pkg depGraph = dfs [pkg] pkg depGraph

    dfs stack root graph =
      forM_ (Cyclic.postSet root graph) \pkg -> do
        when (pkg `elem` stack) do
          liftIO $ throwIO $ DependencyCycle (reverse $ pkg : stack)
        dfs (pkg : stack) pkg graph

checkConsistency :: (MonadIO m) => Text -> SemVer -> Map Text ComponentType -> m ()
checkConsistency name ver cs = case Map.lookup name cs of
  Nothing -> liftIO $ throwIO $ NoSuchComponent name
  Just (ComponentType ver2 _ _ _ _) -> do
    when (ver /= ver2) do
      liftIO $ throwIO $ InconsistentComponentVersions ver ver2

downloadAndExtract :: (MonadIO m, MonadHttp m, MonadMask m) => FilePath -> Source -> Environment -> m (FilePath, ProjectType, Configuration)
downloadAndExtract dir dep env =
  case dep of
    Tar (Remote url) sha256 -> unpackArchive url sha256 \path dir tar -> do
      liftIO . Tar.unpack dir $ Tar.read tar
      liftIO $ copyArchive url dir path "Tarball" =<< listDirectory dir
    Tar (Local uri) sha256 -> error "TODO"
    Tar (Environment _) _ -> liftIO $ throwIO $ InvalidDependencyLocation dep
    Tar Missing _ -> liftIO $ throwIO $ InvalidDependencyLocation dep
    TarGz (Remote url) sha256 -> unpackArchive url sha256 \path dir tar -> do
      liftIO . Tar.unpack dir . Tar.read $ GZip.decompress tar
      liftIO $ copyArchive url dir path "GZipped tarball" =<< listDirectory dir
    TarGz (Local uri) sha256 -> error "TODO"
    TarGz (Environment _) _ -> liftIO $ throwIO $ InvalidDependencyLocation dep
    TarGz Missing _ -> liftIO $ throwIO $ InvalidDependencyLocation dep
    Zip (Remote url) sha256 -> unpackArchive url sha256 \path dir zip -> do
      liftIO . Zip.extractFilesFromArchive [Zip.OptDestination dir] $ Zip.toArchive zip
      liftIO $ copyArchive url dir path "Zipped" =<< listDirectory dir
    Zip (Local uri) sha256 -> error "TODO"
    Zip (Environment _) _ -> liftIO $ throwIO $ InvalidDependencyLocation dep
    Zip Missing _ -> liftIO $ throwIO $ InvalidDependencyLocation dep
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

      project <- readPackageDhall path
      configuration <- liftIO $ inputFile auto (path </> riftDhall)
      pure (path, project, configuration)
    Git _ _ -> liftIO $ throwIO $ InvalidDependencyLocation dep
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
        liftIO $ throwIO $ Sha256ValidationError url sha256 hashed

      withSystemTempDirectory "rift" \dir -> unpack path dir resp

      project <- readPackageDhall path
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
