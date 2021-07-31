{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger (info, warn, Logger.error) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Console.ANSI as ANSI
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Bifunctor (second)
import System.IO (stderr)

type Logger m = MonadIO m

info, warn, error :: Logger m => Text -> m ()
info  = Logger.log ANSI.Green  "INFO "
warn  = Logger.log ANSI.Yellow "WARN "
error = Logger.log ANSI.Red    "ERROR"

log :: Logger m => ANSI.Color -> Text -> Text -> m ()
log color prefix msg = liftIO do
  ANSI.hSetSGR stderr [ANSI.SetColor ANSI.Foreground ANSI.Dull color]
  Text.hPutStrLn stderr $ "[" <> prefix <> "] " <> prefixAllLinesButFirstWith "|" (Text.length prefix + 1) msg
  ANSI.hSetSGR stderr [ANSI.Reset]
  where
    prefixAllLinesButFirstWith linePrefix leftMargin =
      maybe "" (Text.unlines
                . uncurry (:)
                . second (fmap $ mappend (Text.replicate leftMargin " " <> linePrefix <> " ")))
      . List.uncons
      . Text.lines
