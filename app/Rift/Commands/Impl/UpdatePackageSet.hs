{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rift.Commands.Impl.UpdatePackageSet (updatePackageSetCommand) where

import Control.Monad (forM_, unless)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Dhall (auto, inputFile)
import Rift.Commands.Impl.Utils.DhallHash (dhallHash)
import Rift.Commands.Impl.Utils.Directory (copyDirectoryRecursive)
import Rift.Commands.Impl.Utils.GitTags (fetchAllTags)
import Rift.Config.PackageSet (Snapshot, ltsOf)
import Rift.Environment (Environment (..))
import Rift.Internal.LockFile (withLockFile)
import qualified Rift.Logger as Logger
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, removePathForcibly)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (<.>), (</>))
import Turtle (ExitCode (..), empty, procStrictWithErr)

-- | Updates the package set by following this simple algorithm:
--
--   * Fetch the path to the @git@ command, error out if not found in the PATH
--
--   * Check if @$RIFT_CACHE/pkgs@ exists
--
--   * If yes:
--
--     * Check whether @$RIFT_CACHE/pkgs/.git@ is an existing directory
--
--     * If yes:
--
--       * Pull & rebase the package set repository and report any error
--
--     * If no:
--
--       * Error out: not a git repository
--
--   * If no:
--
--     * Clone the repository to @$RIFT_HOME/pkgs@ and report any error
updatePackageSetCommand :: (MonadMask m, MonadIO m) => Environment -> m ()
updatePackageSetCommand Env {..} = do
  let pkgsHome = riftCache </> "pkgs"
      hashes = riftCache </> "hashes"

  liftIO $ createDirectoryIfMissing True hashes

  pkgsHomeExists <- liftIO $ doesPathExist pkgsHome
  if pkgsHomeExists
    then do
      unlessM (liftIO $ doesDirectoryExist (pkgsHome </> ".git")) do
        Logger.error $ "'" <> Text.pack pkgsHome <> "' does not contain a git repository.\nPlease move it to another destination and retry."
        liftIO exitFailure

      liftIO $ withLockFile (riftHome </> "package-set.lock") do
        Logger.info "Updating the package set."

        (exit, out, err) <- procStrictWithErr (Text.pack git) ["-C", Text.pack pkgsHome, "pull", "--rebase", "--all", "--tags"] empty
        unless (exit == ExitSuccess) do
          Logger.error $
            "Could not update the package set due to a git error.\n* Standard output:\n"
              <> Text.unlines (mappend "> " <$> Text.lines out)
              <> "\n* Standard error:\n"
              <> Text.unlines (mappend "> " <$> Text.lines err)
          exitFailure
        Logger.info "Successfully updated the package set!"
    else do
      liftIO $ withLockFile (riftHome </> "package-set.lock") do
        Logger.info $ "Initializing package set at path '" <> Text.pack pkgsHome <> "'."
        createDirectoryIfMissing True pkgsHome

        (exit, out, err) <- procStrictWithErr (Text.pack git) ["clone", "https://github.com/zilch-lang/pkgs", Text.pack pkgsHome] empty
        unless (exit == ExitSuccess) do
          Logger.error $
            "Failed to clone package set to destination '" <> Text.pack pkgsHome <> "'.\n* Standard output:\n"
              <> Text.unlines (mappend "> " <$> Text.lines out)
              <> "\n* Standard error:\n"
              <> Text.unlines (mappend "> " <$> Text.lines err)
          exitFailure

        Logger.info "Package set initialized!"

  tags <- liftIO $ ("unstable" :) <$> fetchAllTags git pkgsHome
  -- add the branch 'unstable' to the list of tags, so that we don't miss it

  flip finally (backToUnstable git pkgsHome) $ forM_ tags \tag -> do
    liftIO $ withLockFile (riftHome </> "package-set.lock") do
      Logger.debug $ "Updating hash for tag '" <> tag <> "'..."

      (exit, out, err) <- procStrictWithErr (Text.pack git) ["-C", Text.pack pkgsHome, "checkout", tag, "--force", "--detach"] empty
      unless (exit == ExitSuccess) do
        Logger.error $
          "Could not update the package set due to a git error.\n* Standard output:\n"
            <> Text.unlines (mappend "> " <$> Text.lines out)
            <> "\n* Standard error:\n"
            <> Text.unlines (mappend "> " <$> Text.lines err)
        exitFailure

      let snapshotPath = pkgsHome </> "packages" </> "set" <.> "dhall"
      setDhallHash <- fromJust . Text.stripPrefix "sha256:" <$> dhallHash snapshotPath
      snapshot <- inputFile auto snapshotPath :: IO Snapshot
      let ltsName = show (ltsOf snapshot)

      let ltsDir = riftCache </> ltsName <.> Text.unpack setDhallHash

      Text.writeFile (hashes </> ltsName <.> "hash") setDhallHash

      liftIO $ removePathForcibly ltsDir
      liftIO $ createDirectoryIfMissing True (ltsDir </> "lts")

      copyDirectoryRecursive pkgsHome (ltsDir </> "lts") \path -> not (isPrefixOf "." $ takeFileName path)

backToUnstable :: (MonadIO m) => FilePath -> FilePath -> m ()
backToUnstable git pkgsHome = do
  (exit, out, err) <- liftIO $ procStrictWithErr (Text.pack git) ["-C", Text.pack pkgsHome, "checkout", "unstable", "--force"] empty
  unless (exit == ExitSuccess) do
    Logger.error $
      "Could not update the package set due to a git error.\n* Standard output:\n"
        <> Text.unlines (mappend "> " <$> Text.lines out)
        <> "\n* Standard error:\n"
        <> Text.unlines (mappend "> " <$> Text.lines err)
    liftIO exitFailure
