{-# LANGUAGE OverloadedStrings #-}

module Rift.Config.Template where

import Data.Text (Text)
import qualified Data.Text as Text


data Template
  = Executable
  | Library
  | Empty
  deriving (Eq)

resolveTemplate :: Text -> Maybe Template
resolveTemplate temp = case Text.toLower temp of
  "executable" -> Just Executable
  "exe"        -> Just Executable
  "library"    -> Just Library
  "lib"        -> Just Library
  "empty"      -> Just Empty
  _            -> Nothing
