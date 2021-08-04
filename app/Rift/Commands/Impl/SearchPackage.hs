{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS -Wno-name-shadowing #-}

module Rift.Commands.Impl.SearchPackage (searchPackageCommand) where

import Control.Exception (finally, catch)
import Control.Monad (unless, forM, when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Rift.Config.PackageSet
import Rift.Environment (Environment(..))
import Rift.Internal.LockFile (withLockFile)
import qualified Rift.Logger as Logger

import qualified System.Console.ANSI as ANSI
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stdout)

import Turtle (procStrictWithErr, ExitCode(..), empty)


searchPackageCommand :: MonadIO m => Text -> Environment -> m ()
searchPackageCommand pkgName env@Env{..} = do
  liftIO $ withLockFile (riftHome </> "package-set.lock") do
    (exit, out, err) <- procStrictWithErr (Text.pack git) [ "-C", Text.pack pkgsHome, "tag", "-l", "-n", "1", "--color=never" ] empty
    unless (exit == ExitSuccess) do
      Logger.error $ "Failed to fetch all the versions in your package set.\n* Standard output:\n"
                                                                          <> Text.unlines (mappend "> " <$> Text.lines out) <> "\n* Standard error:\n"
                                                                          <> Text.unlines (mappend "> " <$> Text.lines err)
      exitFailure

    let allTags = Text.lines out

    let restoreToUnstable = do
          (exit, out, err) <- procStrictWithErr (Text.pack git) [ "-C", Text.pack pkgsHome, "checkout", "unstable" ] empty
          unless (exit == ExitSuccess) do
            Logger.error $ "Failed to restore the package set to a correct state.\nPlease delete the directory '" <> Text.pack pkgsHome <> "' and run the command 'rift package update'."
                                                                          <> "\n* Standard output:\n" <> Text.unlines (mappend "> " <$> Text.lines out)
                                                                          <> "\n* Standard error:\n" <> Text.unlines (mappend "> " <$> Text.lines err)
            exitFailure

    allVersionsInAllLTSs <- (HashMap.toList <$> queryAllTagsForPackage git (allTags <> ["unstable"])) `finally` restoreToUnstable
    let sortedPackagesOnLTS = (first readLTSVersion <$> allVersionsInAllLTSs) & filter (isJust . fst) & List.sort

    case sortedPackagesOnLTS of
      [] -> do
        Text.hPutStr stdout "Package "
        ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
        Text.hPutStr stdout pkgName
        ANSI.hSetSGR stdout [ANSI.Reset]
        Text.hPutStrLn stdout " not found in the current package set."
        Text.hPutStrLn stdout "Maybe you want to update it with 'rift package update'?"
      _ -> do
        let tmpPackages = HashMap.fromList $ sortedPackagesOnLTS >>= \ (lts, vs) -> vs <&> \ (version, broken) -> (version, (lts, broken))
            keysInOrder = reverse $ fst <$> sortedPackagesOnLTS
            packages = HashMap.foldlWithKey' (\ m version (lts, broken) -> HashMap.insertWith (<>) lts [(version, broken)] m) mempty tmpPackages

        Text.hPutStr stdout "Found package "
        ANSI.hSetSGR stdout [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
        Text.hPutStr stdout pkgName
        ANSI.hSetSGR stdout [ANSI.Reset]
        Text.hPutStrLn stdout ":"

        forM_ keysInOrder \ k -> do
          let versions = packages HashMap.! k

          case versions of
            []              -> pure ()
            [(ver, broken)] -> do
              Text.hPutStr stdout "- version "
              outputVersion stdout ver broken
            vs              -> do
              Text.hPutStr stdout "- versions "
              outputVersions stdout vs
          outputLTS stdout k
  where
    queryAllTagsForPackage gitExe tags = do
      foldl' (HashMap.unionWith (<>)) mempty <$> forM tags \ t -> do
        (exit, _, _) <- procStrictWithErr (Text.pack gitExe) [ "-C", Text.pack pkgsHome, "checkout", t, "--force", "--detach" ] empty

        if exit /= ExitSuccess
        then mempty <$ Logger.warn ("Failed to checkout tag '" <> t <> "'.\nIgnoring any package in this LTS version.")
        else do
          ((Just <$> snapshotFromDhallFile (pkgsHome </> "packages" </> "set.dhall") env) `catch` \ (_ :: ExitCode) -> pure Nothing) >>= \ case
            Nothing           -> pure mempty
            Just Snapshot{..} -> do
              let versionsOfPackageInLTS = filter (\ Pkg{..} -> pkgName == name) packageSet
              pure $ HashMap.fromListWith (<>) $ versionsOfPackageInLTS <&> \ Pkg{..} -> (t, [(version, broken)])

    _2 ~(_, x, _) = x

    outputVersion handle version isBroken = do
      ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull $ if isBroken then ANSI.Red else ANSI.Green]
      Text.hPutStr handle version
      when isBroken do
        Text.hPutStr handle " (broken)"
      ANSI.hSetSGR handle [ANSI.Reset]

    outputLTS handle name = do
      Text.hPutStr handle " in "
      ANSI.hSetSGR handle [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
      Text.hPutStr handle $ Text.pack (show name)
      ANSI.hSetSGR handle [ANSI.Reset]
      Text.hPutStrLn handle ""

    outputVersions handle []                       = pure ()
    outputVersions handle [(version, isBroken)]    = outputVersion handle version isBroken
    outputVersions handle ((version, isBroken):vs) = do
      outputVersion handle version isBroken
      Text.hPutStr handle ", "
      outputVersions handle vs
