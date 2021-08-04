{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Rift.Commands.Impl.UpdatePackageSet (updatePackageSetCommand) where

import Control.Monad (unless)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text as Text

import Rift.Environment (Environment(..))
import Rift.Internal.LockFile (withLockFile)
import qualified Rift.Logger as Logger

import System.Directory (doesPathExist, doesDirectoryExist, createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Turtle (procStrictWithErr, ExitCode(..), empty)


-- | Updates the package set by following this simple algorithm:
--
--   * Fetch the path to the @git@ command, error out if not found in the PATH
--
--   * Check if @$RIFT_HOME/pkgs@ exists
--
--   * If yes:
--
--     * Check whether @$RIFT_HOME/pkgs/.git@ is an existing directory
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
updatePackageSetCommand :: MonadIO m => Environment -> m ()
updatePackageSetCommand Env{..} = do
  pkgsHomeExists <- liftIO $ doesPathExist pkgsHome
  if pkgsHomeExists
  then do
    unlessM (liftIO $ doesDirectoryExist (pkgsHome </> ".git")) do
      Logger.error $ "'" <> Text.pack pkgsHome <> "' does not contain a git repository.\nPlease move it to another destination and retry."
      liftIO exitFailure

    liftIO $ withLockFile (riftHome </> "package-set.lock") do
      Logger.info "Updating the package set."

      (exit, out, err) <- procStrictWithErr (Text.pack git) [ "-C", Text.pack pkgsHome, "pull", "--rebase", "--all", "--tags" ] empty
      unless (exit == ExitSuccess) do
        Logger.error $ "Could not update the package set due to a git error.\n* Standard output:\n"
                                                                    <> Text.unlines (mappend "> " <$> Text.lines out) <> "\n* Standard error:\n"
                                                                    <> Text.unlines (mappend "> " <$> Text.lines err)
        exitFailure
      Logger.info "Successfully updated the package set!"
  else do
    liftIO $ withLockFile (riftHome </> "package-set.lock") do
      Logger.info $ "Initializing package set at path '" <> Text.pack pkgsHome <> "'."
      createDirectoryIfMissing True pkgsHome

      (exit, out, err) <- procStrictWithErr (Text.pack git) [ "clone", "https://github.com/zilch-lang/pkgs", Text.pack pkgsHome ] empty
      unless (exit == ExitSuccess) do
        Logger.error $ "Failed to clone package set to destination '" <> Text.pack pkgsHome <> "'.\n* Standard output:\n"
                                                                      <> Text.unlines (mappend "> " <$> Text.lines out) <> "\n* Standard error:\n"
                                                                      <> Text.unlines (mappend "> " <$> Text.lines err)
        exitFailure

      Logger.info "Package set initialized!"
