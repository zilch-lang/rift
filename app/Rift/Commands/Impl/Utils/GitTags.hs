{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Rift.Commands.Impl.Utils.GitTags where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Rift.Logger as Logger
import System.Exit (ExitCode (..), exitFailure)
import Turtle (empty, procStrictWithErr)

-- | Retrieves all of the tags inside a package set git repository.
fetchAllTags :: FilePath -> FilePath -> IO [Text]
fetchAllTags git dir = do
  (exit, out, err) <- procStrictWithErr (Text.pack git) ["-C", Text.pack dir, "tag", "-l", "-n", "1", "--color=never"] empty
  unless (exit == ExitSuccess) do
    Logger.error $
      "Failed to fetch all the versions in your package set.\n* Standard output:\n"
        <> Text.unlines (mappend "> " <$> Text.lines out)
        <> "\n* Standard error:\n"
        <> Text.unlines (mappend "> " <$> Text.lines err)
    exitFailure

  pure $ Text.lines out
