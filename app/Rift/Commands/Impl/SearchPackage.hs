{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS -Wno-name-shadowing #-}

module Rift.Commands.Impl.SearchPackage (searchPackageCommand) where

import Control.Exception (finally)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap, first)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Dhall (auto, inputFile)
import Rift.Commands.Impl.Utils.GitTags (fetchAllTags)
import Rift.Commands.Impl.Utils.Paths (ltsPath, packagePath, projectDhall)
import Rift.Config.Package (Package (..))
import Rift.Config.PackageSet
import Rift.Config.Project (ComponentType (..), ProjectType)
import Rift.Environment (Environment (..))
import Rift.Internal.LockFile (withLockFile)
import qualified Rift.Logger as Logger
import qualified System.Console.ANSI as ANSI
import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>))
import System.IO (stdout)
import Turtle (ExitCode (..), empty, procStrictWithErr)

searchPackageCommand :: MonadIO m => Text -> Environment -> m ()
searchPackageCommand pkgName env@Env {..} = do
  let pkgsHome = riftCache </> "pkgs"

  liftIO $ withLockFile (riftHome </> "package-set" <.> "lock") do
    allTags <- fetchAllTags git pkgsHome

    let restoreToUnstable = do
          (exit, out, err) <- procStrictWithErr (Text.pack git) ["-C", Text.pack pkgsHome, "checkout", "unstable"] empty
          unless (exit == ExitSuccess) do
            Logger.error $
              "Failed to restore the package set to a correct state.\nPlease delete the directory '" <> Text.pack pkgsHome <> "' and run the command 'rift package update'."
                <> "\n* Standard output:\n"
                <> Text.unlines (mappend "> " <$> Text.lines out)
                <> "\n* Standard error:\n"
                <> Text.unlines (mappend "> " <$> Text.lines err)
            exitFailure

    allVersionsInAllLTSs <- (HashMap.toList <$> queryAllTagsForPackage env (allTags <> ["unstable"])) `finally` restoreToUnstable
    let sortedPackagesOnLTS = (first readLTSVersion <$> allVersionsInAllLTSs) & mapMaybe (\(m, x) -> (,x) <$> m) & List.sort

    case sortedPackagesOnLTS of
      [] -> do
        Text.hPutStr stdout "Package "
        ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
        Text.hPutStr stdout pkgName
        ANSI.hSetSGR stdout [ANSI.Reset]
        Text.hPutStrLn stdout " not found in the current package set."
        Text.hPutStrLn stdout "Maybe you want to update it with 'rift package update'?"
      _ -> do
        let tmpPackages = HashMap.fromList $ sortedPackagesOnLTS >>= \(lts, vs) -> vs <&> \(version, broken, deprecated) -> (version, (lts, broken, deprecated))
            keysInOrder = reverse $ fst <$> sortedPackagesOnLTS
            packages = HashMap.foldlWithKey' (\m version (lts, broken, deprecated) -> HashMap.insertWith (<>) lts [(version, broken, deprecated)] m) mempty tmpPackages

        Text.hPutStr stdout "Found package "
        ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
        Text.hPutStr stdout pkgName
        ANSI.hSetSGR stdout [ANSI.Reset]
        Text.hPutStrLn stdout ":"

        forM_ keysInOrder \k -> do
          let versions = packages HashMap.! k

          if null versions
            then pure ()
            else do
              outputLTS stdout k
              outputVersions stdout versions
  where
    queryAllTagsForPackage env tags = do
      (uncached, versionsFound) <-
        bimap mconcat (HashMap.unionWith (<>) mempty . mconcat) . unzip <$> forM tags \t -> do
          ltsDir <- ltsPath riftCache (fromMaybe Unstable $ readLTSVersion t)
          case ltsDir of
            Nothing -> do
              Logger.warn $ "LTS '" <> t <> "' is not cached. It will be ignored when searching for packages."
              pure ([t], mempty)
            Just ltsDir -> do
              Snapshot _ _ packageSet <- liftIO (snapshotFromDhallFile (ltsDir </> "lts" </> "packages" </> "set" <.> "dhall") env)

              let versionsOfPackageInLTS = filter (\Pkg {..} -> pkgName == name) packageSet
              versions <- forM versionsOfPackageInLTS \pkg@Pkg {..} -> do
                let pkgPath = packagePath pkg (ltsDir </> "sources")
                pathExists <- liftIO $ doesDirectoryExist pkgPath
                if not pathExists
                  then pure (t, [(Nothing, broken, deprecated)])
                  else do
                    cs <- liftIO (inputFile auto (pkgPath </> projectDhall) :: IO ProjectType)
                    let retained = filter (\(ComponentType name _ _ _ _ _) -> isNothing component || Just name == component) cs
                    case retained of
                      [] -> pure (t, [])
                      _ : _ : _ -> pure (t, [])
                      [ComponentType _ version _ _ _ _] -> pure (t, [(Just version, broken, deprecated)])
              pure ([], HashMap.fromListWith (<>) versions)

      when (not $ null uncached) do
        Logger.warn "Some LTSes were not present in the global cache.\nYou may want to run 'rift package update' to fix this."
      when (any (any (\(v, _, _) -> isNothing v)) $ HashMap.elems versionsFound) do
        Logger.warn $ "Some versions were not cached. Run 'rift package fetch " <> pkgName <> "' to cache them."

      pure versionsFound

    outputVersion handle version isBroken isDeprecated = do
      Text.hPutStr handle "  - version "
      let version' = case version of
            Nothing -> "(not cached)"
            Just v -> Text.pack $ show v
      if
          | isBroken -> do
            ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]
            Text.hPutStr handle version'
            Text.hPutStr handle " (broken)"
          | isDeprecated -> do
            ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow]
            Text.hPutStr handle version'
            Text.hPutStr handle " (deprecated)"
          | otherwise -> do
            ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
            Text.hPutStr handle version'
      ANSI.hSetSGR handle [ANSI.Reset]

    outputLTS handle name = do
      Text.hPutStr handle "- in LTS "
      ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
      Text.hPutStr handle $ Text.pack (show name)
      ANSI.hSetSGR handle [ANSI.Reset]
      Text.hPutStrLn handle ":"

    outputVersions _ [] = pure ()
    outputVersions handle ((version, isBroken, isDeprecated) : vs) = do
      outputVersion handle version isBroken isDeprecated
      Text.hPutStrLn handle ""
      outputVersions handle vs
