{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.Utils.Directory where

import Control.Monad (forM)
import Control.Monad.Extra (whenM)
import Data.Foldable (fold)
import qualified Data.Text as Text
import qualified Rift.Logger as Logger
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory)
import System.FilePath ((</>))

-- | Recursively copy the input directory to the output directory.
copyDirectoryRecursive :: (?logLevel :: Int) => FilePath -> FilePath -> (FilePath -> Bool) -> IO ()
copyDirectoryRecursive from to filterChildren = do
  createDirectoryIfMissing True to
  children <- filter filterChildren <$> listDirectory from
  copyRecursive' from children to
  where
    copyRecursive' _ [] _ = pure ()
    copyRecursive' from (c : cs) to = do
      whenM (doesPathExist $ from </> c) do
        doesDirectoryExist (from </> c) >>= \case
          True -> do
            Logger.debug $ "Copying directory '" <> Text.pack (from </> c) <> "' to '" <> Text.pack (to </> c) <> "'"
            copyDirectoryRecursive (from </> c) (to </> c) filterChildren
          False -> do
            Logger.debug $ "Copying file '" <> Text.pack (from </> c) <> "' to '" <> Text.pack (to </> c) <> "'"
            copyFile (from </> c) (to </> c)
      copyRecursive' from cs to

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
  isFile <- doesFileExist path

  if isFile
    then pure []
    else do
      children <- listDirectory path
      let children' = (path </>) <$> children
      (children' <>) . fold <$> forM children (listDirectoryRecursive . (path </>))
