{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Rift.Config.Project where

import Data.Foldable (fold)
import Data.Text (Text)
import Dhall.Marshal.Decode (FromDhall (..), auto, constructor, field, record, union, unit)
import Rift.Config.PackageSet (LTSVersion, PackageSource (..))

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

data Dependency
  = GitDep
      Text
      -- ^ The URL to the @git@ repository.
      Text
      -- ^ The revision to fetch
  | TarDep
      Text
      -- ^ The URL to the @.tar@ file.
      Text
      -- ^ The hash of the file, in SHA-256.
  | ZipDep
      Text
      -- ^ The URL to the @.zip@ file.
      Text
      -- ^ The hash of the file, in SHA-256.
  | TarGzDep
      Text
      -- ^ The URL to the @.tar.gz@ file.
      Text
      -- ^ The hash of the file, in SHA-256.
  deriving (Show, Eq)

-- | Transforms a 'PackageSource' into a 'Dependency'.
packageSourceToDependency :: PackageSource -> Dependency
packageSourceToDependency (Git url rev) = GitDep url rev
packageSourceToDependency (Tar url sha256) = TarDep url sha256
packageSourceToDependency (TarGz url sha256) = TarGzDep url sha256
packageSourceToDependency (Zip url sha256) = ZipDep url sha256

data ProjectType
  = ProjectType
      [ComponentType]
      -- ^ All the components present in this project.
      LTSVersion
      -- ^ The version of the LTS used.
      [Dependency]
      -- ^ Extra dependencies which are not in the package set.
  deriving (Show)

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
        <*> field "flags" auto

instance FromDhall Dependency where
  autoWith _ =
    union $
      fold
        [ constructor "Git" git,
          constructor "Tar" tar,
          constructor "TarGz" targz,
          constructor "Zip" zip
        ]
    where
      git =
        record $
          GitDep
            <$> field "url" auto
            <*> field "rev" auto
      tar =
        record $
          TarDep
            <$> field "url" auto
            <*> field "sha256" auto
      targz =
        record $
          TarGzDep
            <$> field "url" auto
            <*> field "sha256" auto
      zip =
        record $
          ZipDep
            <$> field "url" auto
            <*> field "sha256" auto

instance FromDhall ProjectType where
  autoWith _ =
    record $
      ProjectType
        <$> field "components" auto
        <*> field "lts" auto
        <*> field "extra-deps" auto
