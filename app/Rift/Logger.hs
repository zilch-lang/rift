{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Rift.Logger (info, warn, Rift.Logger.error) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Bifunctor (second)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import System.Console.ANSI as ANSI
import System.IO (stderr)

type Logger m = MonadIO m

-- | Log a colored, prefixed message to the standard error.
info, warn, error :: Logger m => Text -> m ()
info  = Rift.Logger.log ANSI.Green  "INFO "
warn  = Rift.Logger.log ANSI.Yellow "WARN "
error = Rift.Logger.log ANSI.Red    "ERROR"

log :: Logger m => ANSI.Color -> Text -> Text -> m ()
log color prefix msg = liftIO do
  ANSI.hSetSGR stderr [ANSI.SetColor ANSI.Foreground ANSI.Dull color]
  Text.hPutStr stderr $ "[" <> prefix <> "] " <> prefixAllLinesButFirstWith "|" (Text.length prefix + 1) msg
  ANSI.hSetSGR stderr [ANSI.Reset]
  where
    prefixAllLinesButFirstWith linePrefix leftMargin =
      maybe "" (Text.unlines
                . uncurry (:)
                . second (fmap $ mappend (Text.replicate leftMargin " " <> linePrefix <> " ")))
      . List.uncons
      . Text.lines
