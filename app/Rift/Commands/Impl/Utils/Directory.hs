{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Commands.Impl.Utils.Directory where

import Control.Monad.Extra (whenM)
import qualified Data.Text as Text
import qualified Rift.Logger as Logger
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesPathExist, listDirectory)
import System.FilePath ((</>))

-- | Recursively copy the input directory to the output directory.
copyDirectoryRecursive :: FilePath -> FilePath -> (FilePath -> Bool) -> IO ()
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
