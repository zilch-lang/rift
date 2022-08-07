{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.BuildProject where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as Text
import Data.Text.Read (decimal)
import Dhall (auto, inputFile)
import Network.HTTP.Req (GET (..), MonadHttp, NoReqBody (..), lbsResponse, req, responseBody, responseHeader, useURI)
import Rift.Commands.Impl.Utils.Directory (copyDirectoryRecursive)
import Rift.Config.PackageSet (Snapshot (..), snapshotFromDhallFile)
import Rift.Config.Project (ComponentType (..), Dependency (..), ProjectType (..), nameOf)
import Rift.Environment (Environment, git, riftCache)
import qualified Rift.Logger as Logger
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Text.URI as URI
import Turtle (empty, procStrictWithErr)

-- | Building the project happens in multiple steps:
--
--   1. Parse the @package.dhall@ file to retrieve all components and (extra-)dependencies
--
--   2. Fetch every dependency (with its version) from the package set inside the global cache in @env:RIFT_HOME@
--
--      We may want to put them inside @~\/.cache\/rift@ instead, where each entry is organized as:
--
--      > ~/.cache/rift/
--      > └── <lts name>.<hash of 'lts/packages/set.dhall'>/
--      >     ├── lts
--      >     │   ├── lib
--      >     │   ├── packages
--      >     │   │   └── set.dhall
--      >     │   └── utils
--      >     └── sources
--      >         └── <component name>.<component version>/
--      >             ├── project.dhall
--      >             └── ...
--
--   3. Create a dependency tree, taking in account both and extra- and intra-dependencies
--
--   4. Topsort the dependency tree to prevent any cycle in dependency resolution
--
--   5. Try building every dependency starting from the beginning of the topsort
--      /Note:/ Our topsort may contain concurrent branches: build those packages concurrently (as long as there are processors remaining)
--
--   5.1. If a dependency is a library, simply execute the command @gzc FLAGS MODULES -I DIRS -ddump-dir=$PWD/.rift/build --keep-zco@ inside its cache directory where:
--
--          * @MODULES@ is the list of modules found in the source directories (the compiler will order them as wanted with an additional topsort)
--          * @DIRS@ is a comma-separated list containing the source directories of the component, as well as the directories containing code in every dependency
--
--   5.2. If a dependency is an executable, the behavior is not yet fixed (we may want to throw an error here)
--
--   6. I don't know
buildProjectCommand :: (MonadIO m, MonadHttp m, MonadMask m) => Bool -> Integer -> Bool -> [Text] -> Environment -> m ()
buildProjectCommand dryRun nbCores dirtyFiles componentsToBuild env = do
  !project@(ProjectType components lts extraDeps) <- liftIO $ inputFile auto "project.dhall"

  let ltsTag = show lts
      ltsHashFile = riftCache env </> "hashes" </> ltsTag <.> "hash"
  ltsHash <- liftIO $ Text.readFile ltsHashFile
  let ltsDir = riftCache env </> (ltsTag <> "." <> Text.unpack ltsHash)

  componentsToBuild <- case componentsToBuild of
    [] -> do
      Logger.debug "No component specified. Building all local components."
      pure components
    cs -> getComponentsByName components (nub cs)

  snapshot <- liftIO $ snapshotFromDhallFile (ltsDir </> "lts" </> "packages" </> "set.dhall") env
  extraDeps' <- fetchExtraDependencies env (nub extraDeps)
  (resolvedDeps1, unresolvedDeps) <- gatherDependencies snapshot componentsToBuild
  (resolvedDeps2, unresolvedDeps) <- checkUnresolvedDependencies extraDeps' (nameOf <$> components) unresolvedDeps

  pure ()

getComponentsByName :: (MonadIO m) => [ComponentType] -> [Text] -> m [ComponentType]
getComponentsByName _ [] = pure []
getComponentsByName components (c : cs) = case findElem (\(ComponentType name _ _ _ _ _) -> name == c) components of
  Nothing -> do
    Logger.error $ "Component named '" <> c <> "' not found in local project."
    liftIO exitFailure
  Just (c1, others) -> (c1 :) <$> getComponentsByName others cs

gatherDependencies :: (MonadIO m) => Snapshot -> [ComponentType] -> m ([Text], [Text])
gatherDependencies _ [] = pure ([], [])
gatherDependencies snapshot (ComponentType _ _ deps _ _ _ : cs) = pure ([], [])

fetchExtraDependencies :: (MonadIO m, MonadHttp m, MonadMask m) => Environment -> [Dependency] -> m (Map FilePath ProjectType)
fetchExtraDependencies _ [] = pure mempty
fetchExtraDependencies env (dep : deps) = do
  done <- fetchExtraDependencies env deps
  (path, project) <- case dep of
    TarDep url sha256 -> unpackArchive url sha256 \path dir tar -> do
      liftIO . Tar.unpack dir $ Tar.read tar
      liftIO $ copyArchive url dir path "Tarball" =<< listDirectory dir
    TarGzDep url sha256 -> unpackArchive url sha256 \path dir tar -> do
      liftIO . Tar.unpack dir . Tar.read $ GZip.decompress tar
      liftIO $ copyArchive url dir path "GZipped tarball" =<< listDirectory dir
    ZipDep url sha256 -> unpackArchive url sha256 \path dir zip -> do
      liftIO . Zip.extractFilesFromArchive [Zip.OptDestination dir] $ Zip.toArchive zip
      liftIO $ copyArchive url dir path "Zipped" =<< listDirectory dir
    GitDep url rev -> do
      let sha256 = Text.pack $ show (hash $ encodeUtf8 (url <> "/" <> rev) :: Digest SHA256)
      let path = riftCache env </> "extra-deps" </> ("g-" <> Text.unpack sha256)

      unlessM (liftIO $ (not (Text.null sha256) &&) <$> doesDirectoryExist path) do
        Logger.info $ "Checking git repository '" <> url <> "' at revision '" <> rev <> "'..."

        liftIO $ withSystemTempDirectory "rift" \dir -> do
          let gitexe = Text.pack $ git env
          (exit, out, err) <- procStrictWithErr gitexe ["-C", Text.pack dir, "clone", url, "."] empty
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

          unlessM (liftIO . doesFileExist $ dir </> "project.dhall") do
            Logger.error $ "Zipped file '" <> url <> "' does not contain a Rift project (file 'project.dhall' not present)"
            liftIO exitFailure

          liftIO $ copyDirectoryRecursive dir path (const True)

      project <- liftIO $ inputFile auto (path </> "project.dhall")
      pure (path, project)
  pure (Map.insert path project done)
  where
    unpackArchive url sha256 unpack = do
      let path = riftCache env </> "extra-deps" </> ("a-" <> Text.unpack sha256)

      unlessM (liftIO $ (not (Text.null sha256) &&) <$> doesDirectoryExist path) do
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

      project <- liftIO $ inputFile auto (path </> "project.dhall")

      pure (path, project)

    copyArchive url dir path kind [] = do
      Logger.error $ kind <> " file '" <> url <> "' is empty."
      liftIO exitFailure
    copyArchive url dir path kind [dir2] = do
      let dir' = dir </> dir2
      unlessM (liftIO . doesFileExist $ dir' </> "project.dhall") do
        Logger.error $ kind <> " file '" <> url <> "' does not contain a Rift project (file 'project.dhall' not present)"
        liftIO exitFailure
      liftIO $ copyDirectoryRecursive dir' path (const True)
    copyArchive url dir path kind _ = do
      unlessM (liftIO . doesFileExist $ dir </> "project.dhall") do
        Logger.error $ kind <> " file '" <> url <> "' does not contain a Rift project (file 'project.dhall' not present)"
        liftIO exitFailure
      liftIO $ copyDirectoryRecursive dir path (const True)

checkUnresolvedDependencies :: (MonadIO m) => Map FilePath ProjectType -> [Text] -> [Text] -> m ([Text], [Text])
checkUnresolvedDependencies _ _ [] = pure ([], [])

------------------------------

findElem :: (a -> Bool) -> [a] -> Maybe (a, [a])
findElem p = find' id
  where
    find' _ [] = Nothing
    find' prefix (x : xs)
      | p x = Just (x, prefix xs)
      | otherwise = find' (prefix . (x :)) xs
