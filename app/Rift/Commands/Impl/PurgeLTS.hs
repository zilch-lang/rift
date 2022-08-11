{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Rift.Commands.Impl.PurgeLTS where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List ((\\))
import qualified Data.Text as Text
import Rift.Environment (Environment, riftCache)
import qualified Rift.Logger as Logger
import System.Directory (listDirectory, removeDirectoryRecursive)
import System.FilePath (dropExtensions, (<.>), (</>))

purgeAllOldLTS :: (?logLevel :: Int, MonadIO m) => Environment -> m ()
purgeAllOldLTS env = do
  let dir = riftCache env

  let hashesDir = dir </> "hashes"
  allHashes <- liftIO $ listDirectory hashesDir

  aliveLTSes <- liftIO $ forM allHashes \hashFile -> do
    let ltsName = dropExtensions hashFile
    (ltsName <.>) <$> readFile (hashesDir </> hashFile)
  allClonedLTSes <- liftIO (listDirectory dir)

  let deadLTSes = removeStaticDirs $ allClonedLTSes \\ aliveLTSes
  liftIO $ forM_ deadLTSes \ltsDir -> do
    let dir' = dir </> ltsDir
    Logger.info $ "Removing dead LTS at path " <> Text.pack dir' <> "..."
    removeDirectoryRecursive dir'

  pure ()
  where
    removeStaticDirs = (\\ ["hashes", "pkgs", "extra-deps"])
