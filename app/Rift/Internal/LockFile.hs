{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Rift.Internal.LockFile where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket_)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text as Text

import qualified Rift.Logger as Logger

import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeDirectory)
import System.FSNotify (withManager, Event(..), watchDir)


acquire :: MonadIO m => FilePath -> m ()
acquire path = do
  lockFileExists <- liftIO $ doesFileExist path

  when lockFileExists do
    liftIO $ withManager \ mgr -> do
      Logger.warn $ "Acquiring lock on file '" <> Text.pack path <> "'..."

      let isRemovedEventOnLockFile (Removed p _ _) = p == path
          isRemovedEventOnLockFile _               = False

      lock <- newEmptyMVar @()
      cancel <- watchDir mgr (takeDirectory path) isRemovedEventOnLockFile (const $ putMVar lock ())
      takeMVar lock
      cancel

  liftIO $ writeFile path "damn"

release :: MonadIO m => FilePath -> m ()
release path = do
  lockFileExists <- liftIO $ doesFileExist path

  if lockFileExists
  then liftIO $ removeFile path
  else Logger.warn $ "Lock file '" <> Text.pack path <> "' does not exist on disk anymore!"

withLockFile :: FilePath -> IO a -> IO a
withLockFile path = bracket_ (acquire path) (release path)
