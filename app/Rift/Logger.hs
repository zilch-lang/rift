{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Rift.Logger (info, warn, Rift.Logger.error, debug) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (second)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.ANSI as ANSI
import System.IO (stderr)

type Logger m = (MonadIO m, ?logLevel :: Int)

-- | Log a colored, prefixed message to the standard error.
info, warn, error, debug :: Logger m => Text -> m ()
info = Rift.Logger.log ANSI.Green True "INFO "
warn = Rift.Logger.log ANSI.Yellow True "WARN "
error = Rift.Logger.log ANSI.Red True "ERROR"
debug = Rift.Logger.log ANSI.Blue (?logLevel > 1) "DEBUG"

log :: Logger m => ANSI.Color -> Bool -> Text -> Text -> m ()
log color doOutput prefix msg = liftIO do
  when doOutput do
    ANSI.hSetSGR stderr [ANSI.SetColor ANSI.Foreground ANSI.Dull color]
    Text.hPutStr stderr $ "[" <> prefix <> "] " <> prefixAllLinesButFirstWith "|" (Text.length prefix + 1) msg
    ANSI.hSetSGR stderr [ANSI.Reset]
  where
    prefixAllLinesButFirstWith linePrefix leftMargin =
      maybe
        ""
        ( Text.unlines
            . uncurry (:)
            . second (fmap $ mappend (Text.replicate leftMargin " " <> linePrefix <> " "))
        )
        . List.uncons
        . Text.lines
