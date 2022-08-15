{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.PurgePackage where

import Control.Monad (forM, forM_, when)
import Control.Monad.Extra (partitionM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Rift.Commands.Impl.Utils.Config (readPackageDhall)
import Rift.Commands.Impl.Utils.LTS (discoverLTSes)
import Rift.Commands.Impl.Utils.Paths (ltsPath, packagePath, setDhallPath)
import Rift.Config.Package (Package (..))
import Rift.Config.PackageSet (LTSVersion, Snapshot (Snapshot), readLTSVersion, snapshotFromDhallFile)
import Rift.Config.Project (ComponentType (..))
import Rift.Config.Version (PackageDependency (..), SemVer, VersionConstraint, trueConstraint)
import Rift.Config.Version.Parser (parseVersionConstraint)
import Rift.Environment (Environment, riftCache)
import qualified System.Console.ANSI as ANSI
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (stdout)

purgePackage :: (?logLevel :: Int, MonadIO m) => Text -> Maybe Text -> Maybe Text -> Bool -> Environment -> m ()
purgePackage name ltsVersion versionConstraint transitivePurge env = do
  let lts = ltsVersion >>= readLTSVersion
  verPredicate <- fromMaybe trueConstraint <$> traverse parseVersionConstraint versionConstraint
  purgePackage' name lts verPredicate transitivePurge env

purgePackage' :: (?logLevel :: Int, MonadIO m) => Text -> Maybe LTSVersion -> VersionConstraint -> Bool -> Environment -> m ()
purgePackage' name lts verPredicate transitivePurge env = do
  packagesToRemove <- findPackages name lts verPredicate env

  when (not $ null packagesToRemove) do
    liftIO do
      Text.hPutStrLn stdout "Trying to remove the following packages from the cache:"
      forM_ packagesToRemove \(lts, _, Pkg name ver _ _ _ _) -> do
        Text.hPutStr stdout "- "
        ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
        Text.hPutStr stdout name
        ANSI.hSetSGR stdout [ANSI.Reset]
        Text.hPutStr stdout " version "
        ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
        Text.hPutStr stdout $ Text.pack $ show ver
        ANSI.hSetSGR stdout [ANSI.Reset]
        Text.hPutStr stdout " in LTS "
        ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
        Text.hPutStr stdout $ Text.pack $ show lts
        ANSI.hSetSGR stdout [ANSI.Reset]
        Text.hPutStrLn stdout $ (if transitivePurge then " (and its dependencies)" else "") <> "\n"

    forM_ packagesToRemove \(lts, path, pkg@(Pkg name ver _ _ _ _)) -> do
      allUpwardsDeps <- dependsOn lts pkg env
      case allUpwardsDeps of
        Just deps -> liftIO do
          Text.hPutStr stdout "Cannot remove version "
          ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
          Text.hPutStr stdout $ Text.pack $ show ver
          ANSI.hSetSGR stdout [ANSI.Reset]
          Text.hPutStr stdout " of package "
          ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
          Text.hPutStr stdout name
          ANSI.hSetSGR stdout [ANSI.Reset]
          Text.hPutStr stdout " (found in LTS "
          ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
          Text.hPutStr stdout $ Text.pack $ show lts
          ANSI.hSetSGR stdout [ANSI.Reset]
          Text.hPutStrLn stdout ") because some other package(s) depend on it:"
          forM_ deps \(name, ver) -> do
            Text.hPutStr stdout "- "
            ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
            Text.hPutStr stdout name
            ANSI.hSetSGR stdout [ANSI.Reset]
            Text.hPutStr stdout " version "
            ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
            Text.hPutStr stdout $ Text.pack $ show ver
            ANSI.hSetSGR stdout [ANSI.Reset]
            Text.hPutStrLn stdout " (found in the same LTS)"
        Nothing -> do
          when transitivePurge do
            ComponentType _ deps _ _ _ <- (Map.! name) <$> readPackageDhall path
            forM_ deps \(Version n vc) -> purgePackage' n (Just lts) vc transitivePurge env

          liftIO do
            removeDirectoryRecursive path
            Text.hPutStr stdout "Package "
            ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
            Text.hPutStr stdout name
            ANSI.hSetSGR stdout [ANSI.Reset]
            Text.hPutStr stdout " (version "
            ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
            Text.hPutStr stdout $ Text.pack $ show ver
            ANSI.hSetSGR stdout [ANSI.Reset]
            Text.hPutStr stdout ", LTS "
            ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
            Text.hPutStr stdout $ Text.pack $ show lts
            ANSI.hSetSGR stdout [ANSI.Reset]
            Text.hPutStrLn stdout ") successfully purged!"

findPackages :: (MonadIO m, ?logLevel :: Int) => Text -> Maybe LTSVersion -> VersionConstraint -> Environment -> m [(LTSVersion, FilePath, Package)]
findPackages name Nothing constraint env = do
  ltses <- discoverLTSes (riftCache env)

  mconcat <$> forM ltses \(_, lts) -> findPackages name (Just lts) constraint env
findPackages name (Just lts) (_, constraint) env = do
  path <- ltsPath (riftCache env) lts
  case path of
    Nothing -> pure []
    Just path -> do
      Snapshot _ _ packageSet <- liftIO $ snapshotFromDhallFile (setDhallPath $ path </> "lts") env

      case find (\(Pkg n _ _ _ _ _) -> n == name) packageSet of
        Nothing ->
          -- skip: package either not existent or not cached in this LTS
          pure []
        Just p@(Pkg _ ver _ _ _ _) | constraint ver -> do
          let pkgPath = packagePath p (path </> "sources")
          pathExists <- liftIO $ doesDirectoryExist pkgPath
          -- if package is not cached, do not consider it for removal
          pure if pathExists then [(lts, pkgPath, p)] else []
        _ -> pure []

dependsOn :: (MonadIO m, ?logLevel :: Int) => LTSVersion -> Package -> Environment -> m (Maybe [(Text, SemVer)])
dependsOn lts (Pkg name ver _ _ _ _) env = do
  lts' <- fromJust <$> ltsPath (riftCache env) lts
  let ltsDir = lts' </> "lts"
  Snapshot _ _ packageSet <- liftIO $ snapshotFromDhallFile (setDhallPath ltsDir) env
  (deps, _) <- partitionM (checkIfDepends ltsDir name ver) packageSet
  pure if null deps then Nothing else Just $ (\(Pkg name ver _ _ _ _) -> (name, ver)) <$> deps
  where
    checkIfDepends ltsDir name ver pkg@(Pkg n _ _ _ _ _) = do
      let path = packagePath pkg (ltsDir </> "sources")
      exists <- liftIO $ doesDirectoryExist path
      if not exists
        then pure False
        else do
          ComponentType _ deps _ _ _ <- (Map.! n) <$> readPackageDhall path

          let depOn = flip any deps \(Version n (_, vc)) -> n == name && vc ver
          pure depOn
