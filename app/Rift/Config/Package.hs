{-# LANGUAGE OverloadedStrings #-}

module Rift.Config.Package where

import Data.Text (Text)
import Dhall.Marshal.Decode (FromDhall (..), auto, field, record)
import Rift.Config.Source (Source)
import Rift.Config.Version (SemVer)

data Package = Pkg
  { name :: Text,
    version :: SemVer,
    src :: Source,
    component :: Maybe Text,
    maintainers :: [Text],
    broken :: Bool,
    deprecated :: Bool
  }
  deriving (Show)

data ExtraPackage = ExtraPkg
  { extraName :: Text,
    extraVersion :: SemVer,
    extraSrc :: Source,
    extraComponent :: Maybe Text
  }
  deriving (Show)

instance FromDhall Package where
  autoWith _ =
    record $
      Pkg
        <$> field "name" auto
        <*> field "version" auto
        <*> field "src" auto
        <*> field "component" auto
        <*> field "maintainers" auto
        <*> field "broken" auto
        <*> field "deprecated" auto

instance FromDhall ExtraPackage where
  autoWith _ =
    record $
      ExtraPkg
        <$> field "name" auto
        <*> field "version" auto
        <*> field "src" auto
        <*> field "component" auto
