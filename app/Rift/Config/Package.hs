{-# LANGUAGE OverloadedStrings #-}

module Rift.Config.Package where

import Data.Function (on)
import Data.Text (Text)
import Dhall.Marshal.Decode (FromDhall (..), auto, field, record)
import Rift.Config.Source (Source)
import Rift.Config.Version (SemVer)

data Package = Pkg
  { name :: Text,
    version :: SemVer,
    src :: Source,
    maintainers :: [Text],
    broken :: Bool,
    deprecated :: Bool
  }
  deriving (Show)

instance Eq Package where
  (==) = (==) `on` name

instance Ord Package where
  (<=) = (<=) `on` name

data ExtraPackage = ExtraPkg
  { extraName :: Text,
    extraVersion :: SemVer,
    extraSrc :: Source
  }
  deriving (Show)

instance FromDhall Package where
  autoWith _ =
    record $
      Pkg
        <$> field "name" auto
        <*> field "version" auto
        <*> field "src" auto
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
