{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Rift.Commands.Impl.Utils.GitTags where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as Text
import Rift.Internal.Exceptions (RiftException (..))
import System.Exit (ExitCode (..))
import Turtle (empty, procStrictWithErr)

-- | Retrieves all of the tags inside a package set git repository.
fetchAllTags :: FilePath -> FilePath -> IO [Text]
fetchAllTags git dir = do
  (exit, out, err) <- procStrictWithErr (Text.pack git) ["-C", Text.pack dir, "tag", "-l", "-n", "1", "--color=never"] empty
  unless (exit == ExitSuccess) do
    throwIO $ ExternalCommandError "Failed to fetch all the versions in your package set." out err

  pure $ Text.lines out
