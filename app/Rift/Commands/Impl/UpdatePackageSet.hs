{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.UpdatePackageSet (updatePackageSetCommand) where

import Control.Exception (throwIO)
import Control.Monad (forM_, unless)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Dhall (Encoder (embed), auto, inject, inputFile)
import qualified Dhall.Core as Dhall (pretty)
import Rift.Commands.Impl.Utils.DhallHash (dhallHash)
import Rift.Commands.Impl.Utils.Directory (copyDirectoryRecursive)
import Rift.Commands.Impl.Utils.GitTags (fetchAllTags)
import Rift.Commands.Impl.Utils.Paths (setDhallPath)
import Rift.Config.PackageSet (Snapshot, ltsOf)
import Rift.Environment (Environment (..))
import Rift.Internal.Exceptions (RiftException (..))
import Rift.Internal.LockFile (withLockFile)
import qualified Rift.Logger as Logger
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesPathExist, removePathForcibly)
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
updatePackageSetCommand :: (?logLevel :: Int, MonadMask m, MonadIO m) => Environment -> m ()
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
          liftIO $ throwIO $ ExternalCommandError "Could not update the package set due to a git error." out err
        Logger.info "Successfully updated the package set!"
    else do
      liftIO $ withLockFile (riftHome </> "package-set.lock") do
        Logger.info $ "Initializing package set at path '" <> Text.pack pkgsHome <> "'."
        createDirectoryIfMissing True pkgsHome

        (exit, out, err) <- procStrictWithErr (Text.pack git) ["clone", "https://github.com/zilch-lang/pkgs", Text.pack pkgsHome] empty
        unless (exit == ExitSuccess) do
          liftIO $ throwIO $ ExternalCommandError ("Failed to clone package set to destination '" <> Text.pack pkgsHome <> "'.") out err

        Logger.info "Package set initialized!"

  tags <- liftIO $ ("unstable" :) <$> fetchAllTags git pkgsHome
  -- add the branch 'unstable' to the list of tags, so that we don't miss it

  flip finally (backToUnstable git pkgsHome) $ forM_ tags \tag -> do
    liftIO $ withLockFile (riftHome </> "package-set.lock") do
      Logger.info $ "Updating and precompiling snapshot of LTS " <> tag <> "..."

      (exit, out, err) <- procStrictWithErr (Text.pack git) ["-C", Text.pack pkgsHome, "checkout", tag, "--force", "--detach"] empty
      unless (exit == ExitSuccess) do
        liftIO $ throwIO $ ExternalCommandError "Could not update the package set due to a git error." out err

      let snapshotPath = setDhallPath pkgsHome
      setDhallHash <- fromJust . Text.stripPrefix "sha256:" <$> dhallHash snapshotPath
      snapshot <- inputFile auto snapshotPath :: IO Snapshot
      let ltsName = show (ltsOf snapshot)

      let ltsDir = riftCache </> ltsName <.> Text.unpack setDhallHash

      Text.writeFile (hashes </> ltsName <.> "hash") setDhallHash

      liftIO $ removePathForcibly ltsDir
      liftIO $ createDirectoryIfMissing True (ltsDir </> "lts")

      copyDirectoryRecursive pkgsHome (ltsDir </> "lts") \path -> not (isPrefixOf "." $ takeFileName path)

      -- precompile the LTS package set in order to resolve all links only once instead of each time parsing it
      -- this will speed up future lookups
      let setDhall = setDhallPath (ltsDir </> "lts")
      Text.writeFile setDhall (Dhall.pretty $ Dhall.embed Dhall.inject snapshot)

      pure ()

backToUnstable :: (MonadIO m) => FilePath -> FilePath -> m ()
backToUnstable git pkgsHome = do
  (exit, out, err) <- liftIO $ procStrictWithErr (Text.pack git) ["-C", Text.pack pkgsHome, "checkout", "unstable", "--force"] empty
  unless (exit == ExitSuccess) do
    liftIO $ throwIO $ ExternalCommandError "Could not update the package set due to a git error." out err
