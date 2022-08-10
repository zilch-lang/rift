{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Config.Project where

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Text (Text)
import Dhall.Marshal.Decode (FromDhall (..), auto, constructor, field, record, union, unit)
import Rift.Config.Version (PackageDependency, SemVer)

data ComponentKind = Executable | Library
  deriving (Show)

data ComponentType
  = ComponentType
      SemVer
      -- ^ The version of the component.
      [PackageDependency]
      -- ^ The list of dependencies of the component.
      [Text]
      -- ^ A list of source directories containing the soure files.
      ComponentKind
      -- ^ The kind of component to refer to.
      [Text]
      -- ^ Some additional flags to pass for all the modules inside this component.

type ProjectType = Map Text ComponentType

instance FromDhall ComponentKind where
  autoWith _ =
    union $
      fold
        [ Executable <$ constructor "Executable" unit,
          Library <$ constructor "Library" unit
        ]

instance FromDhall ComponentType where
  autoWith _ =
    record $
      ComponentType
        <$> field "version" auto
        <*> field "dependencies" auto
        <*> field "source-dirs" auto
        <*> field "kind" auto
        <*> field "gzc-flags" auto
