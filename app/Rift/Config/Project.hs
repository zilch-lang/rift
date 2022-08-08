{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Config.Project where

import Data.Foldable (fold)
import Data.Text (Text)
import Dhall.Marshal.Decode (FromDhall (..), auto, constructor, field, record, union, unit)

data VersionRange
  = Version
      Text
      -- ^ The name of the package.
      Text
      -- ^ The version range, in the form of, for example, @>= version@.
  deriving (Show)

data ComponentKind = Executable | Library
  deriving (Show)

data ComponentType
  = ComponentType
      Text
      -- ^ The name of the component.
      Text
      -- ^ The version of the component.
      [VersionRange]
      -- ^ The list of dependencies of the component.
      [Text]
      -- ^ A list of source directories containing the soure files.
      ComponentKind
      -- ^ The kind of component to refer to.
      [Text]
      -- ^ Some additional flags to pass for all the modules inside this component.
  deriving (Show)

-- | Retrieves the name of the component.
nameOf :: ComponentType -> Text
nameOf (ComponentType name _ _ _ _ _) = name

type ProjectType = [ComponentType]

instance FromDhall VersionRange where
  autoWith _ =
    record $
      Version
        <$> field "package" auto
        <*> field "range" auto

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
        <$> field "name" auto
        <*> field "version" auto
        <*> field "dependencies" auto
        <*> field "source-dirs" auto
        <*> field "kind" auto
        <*> field "gzc-flags" auto
