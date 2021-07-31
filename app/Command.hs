{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Command where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text as Text

import Environment

import qualified Logger

import System.Directory (findExecutable, createDirectoryIfMissing, doesPathExist, doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Turtle (procStrictWithErr, ExitCode(..), empty)

type CommandExecutor m = (MonadIO m)


data Command
  = Update


executeCommand :: CommandExecutor m => Command -> Environment -> m ()
executeCommand Update Env{..} = liftIO do
  findExecutable "git" >>= \ case
    Nothing     -> do
      Logger.error "Executable 'git' not found in PATH."
      exitFailure
    Just gitExe -> do
      pkgsHomeExists <- doesPathExist pkgsHome
      if pkgsHomeExists
      then do
        pkgsHomeDotGitExists <- doesDirectoryExist (pkgsHome </> ".git")
        unless pkgsHomeDotGitExists do
          Logger.error $ "'" <> Text.pack pkgsHome <> "' does not contain a git repository.\nPlease move it to another destination and retry."
          exitFailure
        (exit, out, err) <- procStrictWithErr (Text.pack gitExe) [ "-C", Text.pack pkgsHome, "pull", "--rebase" ] empty
        unless (exit == ExitSuccess) do
          Logger.error $ "Could not update the package set due to a git error.\n* Standard output:\n"
                                                                        <> Text.unlines (mappend "> " <$> Text.lines out) <> "\n* Standard error:\n"
                                                                        <> Text.unlines (mappend "> " <$> Text.lines err)
          exitFailure
        Logger.info "Successfully updated the package set!"
      else do
        Logger.info $ "Initializing package set at path '" <> Text.pack pkgsHome <> "'."
        createDirectoryIfMissing True pkgsHome

        (exit, out, err) <- procStrictWithErr (Text.pack gitExe) [ "clone", "https://github.com/zilch-lang/pkgs", Text.pack pkgsHome ] empty
        unless (exit == ExitSuccess) do
          Logger.error $ "Failed to clone package set to destination '" <> Text.pack pkgsHome <> "'.\n* Standard output:\n"
                                                                        <> Text.unlines (mappend "> " <$> Text.lines out) <> "\n* Standard error:\n"
                                                                        <> Text.unlines (mappend "> " <$> Text.lines err)
          exitFailure
      pure ()
executeCommand _ _ = pure ()
