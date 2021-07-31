{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger (info) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Console.ANSI as ANSI
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Function ((&))
import Data.Bifunctor (second)

type Logger m = MonadIO m

info :: Logger m => Text -> m ()
info = Logger.log ANSI.Blue "INFO "

log :: Logger m => ANSI.Color -> Text -> Text -> m ()
log color prefix msg = liftIO do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull color]
  Text.putStrLn $ "[" <> prefix <> "] " <> prefixAllLinesButFirstWith "|" (Text.length prefix + 1) msg
  ANSI.setSGR [ANSI.Reset]
  where
    prefixAllLinesButFirstWith linePrefix leftMargin =
      maybe "" (Text.unlines
                . uncurry (:)
                . second (fmap $ mappend (Text.replicate leftMargin " " <> linePrefix <> " ")))
      . List.uncons
      . Text.lines
